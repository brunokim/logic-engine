package wam

import (
	"fmt"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
)

// NewMachine creates a new abstract machine.
func NewMachine() *Machine {
	m := new(Machine)
	m.Code = make(map[Functor]*Clause)
	for _, builtin := range builtins {
		m.AddClause(builtin)
	}
	m.interrupt = make(chan struct{})
	return m
}

// Reset creates a new machine with clean state, copying only the compiled clauses.
func (m *Machine) Reset() *Machine {
	cloned := new(Machine)
	cloned.Code = m.Code
	cloned.Reg = make([]Cell, len(m.Reg))
	cloned.DebugFilename = m.DebugFilename
	cloned.interrupt = make(chan struct{})
	return cloned
}

// AddClause adds a compiled clause to the machine.
// It overwrites any present clause with the same functor.
func (m *Machine) AddClause(clause *Clause) {
	m.Code[clause.Functor] = clause
	// Grow machine registers to accomodate the clause requirements.
	for i := len(m.Reg); i < clause.NumRegisters; i++ {
		m.Reg = append(m.Reg, nil)
	}
}

// Compute the permanent vars of a logic clause.
//
// A permanent var is a local var in a call that is referenced in more
// than one body term. They must be stored in the environment stack, or
// otherwise they may be overwritten if stored in a register, since a
// body term may use them in any ways.
//
// TODO: ignore cuts (and other builtin calls) that we know to preserve
// registers. For example, in "f(X) :- !, a(X)." X should not be a
// permanent variable.
func permanentVars(clause *logic.Clause) map[logic.Var]struct{} {
	if len(clause.Body) < 2 {
		return map[logic.Var]struct{}{}
	}
	seen := make(map[logic.Var]struct{})
	perm := make(map[logic.Var]struct{})
	// Vars in head are considered to be part of the first body term.
	for _, x := range logic.Vars(clause.Head) {
		seen[x] = struct{}{}
	}
	for _, x := range logic.Vars(clause.Body[0]) {
		seen[x] = struct{}{}
	}
	// Walk through other clause terms; vars that appear in more than
	// one term are permanent.
	for _, c := range clause.Body[1:] {
		for _, x := range logic.Vars(c) {
			if _, ok := seen[x]; ok {
				perm[x] = struct{}{}
			} else {
				seen[x] = struct{}{}
			}
		}
	}
	delete(perm, logic.AnonymousVar)
	return perm
}

func numArgs(clause *logic.Clause) int {
	max := len(clause.Head.(*logic.Comp).Args)
	for _, term := range clause.Body {
		n := len(term.(*logic.Comp).Args)
		if n > max {
			max = n
		}
	}
	return max
}

func hasDeepCut(clause *logic.Clause) bool {
	for i, term := range clause.Body {
		if term.(*logic.Comp).Functor == "!" && i > 0 {
			return true
		}
	}
	return false
}

func hasNonLastCall(clause *logic.Clause) bool {
	for i, term := range clause.Body {
		if term.(*logic.Comp).Functor == "call" && i < len(clause.Body)-1 {
			return true
		}
	}
	return false
}

type compound struct {
	t    logic.Term
	addr RegAddr
}

// compileCtx wraps all the state necessary to compile a single clause.
type compileCtx struct {
	topReg  RegAddr
	seen    map[logic.Var]struct{}
	varAddr map[logic.Var]Addr
	delayed []compound
	instrs  []Instruction
}

func newCompileCtx(numArgs int) *compileCtx {
	return &compileCtx{
		topReg:  RegAddr(numArgs),
		seen:    make(map[logic.Var]struct{}),
		varAddr: make(map[logic.Var]Addr),
	}
}

func (ctx *compileCtx) getVar(x logic.Var, regAddr RegAddr) Instruction {
	if x == logic.AnonymousVar {
		return nil
	}
	if _, ok := ctx.seen[x]; ok {
		return GetValue{ctx.varAddr[x], regAddr}
	}
	ctx.seen[x] = struct{}{}
	return GetVariable{ctx.varAddr[x], regAddr}
}

func (ctx *compileCtx) unifyVar(x logic.Var) Instruction {
	if x == logic.AnonymousVar {
		return UnifyVoid{}
	}
	if _, ok := ctx.seen[x]; ok {
		return UnifyValue{ctx.varAddr[x]}
	}
	ctx.seen[x] = struct{}{}
	return UnifyVariable{ctx.varAddr[x]}
}

func (ctx *compileCtx) putVar(x logic.Var, regAddr RegAddr) Instruction {
	if x == logic.AnonymousVar {
		addr := ctx.topReg
		ctx.topReg++
		return PutVariable{addr, regAddr}
	}
	if _, ok := ctx.seen[x]; ok {
		return PutValue{ctx.varAddr[x], regAddr}
	}
	ctx.seen[x] = struct{}{}
	return PutVariable{ctx.varAddr[x], regAddr}
}

func (ctx *compileCtx) setVar(x logic.Var) Instruction {
	return ctx.unifyVar(x)
}

func (ctx *compileCtx) getTerm(term logic.Term, addr RegAddr) []Instruction {
	switch t := term.(type) {
	case logic.Atom:
		return []Instruction{GetConstant{toConstant(t), addr}}
	case logic.Int:
		return []Instruction{GetConstant{toConstant(t), addr}}
	case logic.Var:
		return []Instruction{ctx.getVar(t, addr)}
	case *logic.Comp:
		instrs := make([]Instruction, len(t.Args)+1)
		instrs[0] = GetStruct{toFunctor(t.Indicator()), addr}
		for i, arg := range t.Args {
			instrs[i+1] = ctx.unifyArg(arg)
		}
		return instrs
	case *logic.List:
		return []Instruction{
			GetPair{ListPair, addr},
			ctx.unifyArg(t.Terms[0]),
			ctx.unifyArg(t.Slice(1)),
		}
	case *logic.Assoc:
		return []Instruction{
			GetPair{AssocPair, addr},
			ctx.unifyArg(t.Key),
			ctx.unifyArg(t.Val),
		}
	case *logic.Dict:
		return []Instruction{
			GetPair{DictPair, addr},
			ctx.unifyArg(t.Assocs[0]),
			ctx.unifyArg(t.Tail()),
		}
	default:
		panic(fmt.Sprintf("wam.getTerm: unhandled type %T (%v)", term, term))
	}
}

func (ctx *compileCtx) delayComplexArg(arg logic.Term) Instruction {
	addr := RegAddr(ctx.topReg)
	ctx.topReg++
	ctx.delayed = append(ctx.delayed, compound{t: arg, addr: addr})
	return UnifyVariable{addr}
}

func (ctx *compileCtx) unifyArg(arg logic.Term) Instruction {
	switch a := arg.(type) {
	case logic.Atom:
		return UnifyConstant{toConstant(a)}
	case logic.Int:
		return UnifyConstant{toConstant(a)}
	case logic.Var:
		return ctx.unifyVar(a)
	case *logic.Comp:
		return ctx.delayComplexArg(a)
	case *logic.List:
		return ctx.delayComplexArg(a)
	case *logic.Assoc:
		return ctx.delayComplexArg(a)
	case *logic.Dict:
		return ctx.delayComplexArg(a)
	default:
		panic(fmt.Sprintf("wam.unifyArg: unhandled type %T (%v)", arg, arg))
	}
}

func (ctx *compileCtx) putTerm(term logic.Term, addr RegAddr) []Instruction {
	switch t := term.(type) {
	case logic.Atom:
		return []Instruction{PutConstant{toConstant(t), addr}}
	case logic.Int:
		return []Instruction{PutConstant{toConstant(t), addr}}
	case logic.Var:
		return []Instruction{ctx.putVar(t, addr)}
	case *logic.Comp:
		instrs := make([]Instruction, len(t.Args)+1)
		instrs[0] = PutStruct{toFunctor(t.Indicator()), addr}
		ctx.setArgs(t.Args, instrs)
		return instrs
	case *logic.List:
		instrs := []Instruction{PutPair{ListPair, addr}, nil, nil}
		head, tail := t.Terms[0], t.Slice(1)
		ctx.setArgs([]logic.Term{head, tail}, instrs)
		return instrs
	case *logic.Assoc:
		instrs := []Instruction{PutPair{AssocPair, addr}, nil, nil}
		ctx.setArgs([]logic.Term{t.Key, t.Val}, instrs)
		return instrs
	case *logic.Dict:
		instrs := []Instruction{PutPair{DictPair, addr}, nil, nil}
		head, tail := t.Assocs[0], t.Tail()
		ctx.setArgs([]logic.Term{head, tail}, instrs)
		return instrs
	default:
		panic(fmt.Sprintf("wam.putTerm: unhandled type %T (%v)", term, term))
	}
}

// Set arguments by first handling complex terms (comp, list) and later
// vars. This is necessary because complex terms may have vars within them, and
// these must be set first (e.g. with set_variable) before the top-level reference.
//
// Example
//   f(A, g(A))
//   --> put_struct g/1, X3
//       unify_variable X2     % A's reference within g/1 comes first
//       put_struct f/2, X0
//       unify_value X2        % A's reference within f/2 comes later
//       unify_value X3
func (ctx *compileCtx) setArgs(args []logic.Term, instrs []Instruction) {
	var varIdxs []int
	for i, arg := range args {
		if _, ok := arg.(logic.Var); ok {
			varIdxs = append(varIdxs, i)
		} else {
			instrs[i+1] = ctx.setArg(arg)
		}
	}
	for _, idx := range varIdxs {
		instrs[idx+1] = ctx.setArg(args[idx])
	}
}

func (ctx *compileCtx) setArg(arg logic.Term) Instruction {
	switch a := arg.(type) {
	case logic.Atom:
		return UnifyConstant{toConstant(a)}
	case logic.Int:
		return UnifyConstant{toConstant(a)}
	case logic.Var:
		return ctx.setVar(a)
	case *logic.Comp:
		return ctx.setComplexArg(arg)
	case *logic.List:
		return ctx.setComplexArg(arg)
	case *logic.Assoc:
		return ctx.setComplexArg(arg)
	case *logic.Dict:
		return ctx.setComplexArg(arg)
	default:
		panic(fmt.Sprintf("wam.setArg: unhandled type %T (%v)", arg, arg))
	}
}

func (ctx *compileCtx) setComplexArg(arg logic.Term) Instruction {
	addr := ctx.topReg
	ctx.topReg++
	ctx.instrs = append(ctx.instrs, ctx.putTerm(arg, addr)...)
	return UnifyValue{addr}
}

// Compile compiles a single logic clause.
func Compile(clause *logic.Clause) *Clause {
	clause2, err := clause.Normalize()
	if err != nil {
		panic(err)
	}
	c := compile(clause2, permanentVars(clause2))
	c.Code = optimizeLastCall(optimizeInstructions(c.Code))
	return c
}

func compileQuery(query []logic.Term) (*Clause, error) {
	dummy, err := logic.NewClause(dsl.Atom("dummy"), query...).Normalize()
	if err != nil {
		return nil, err
	}
	// All vars in a query are made permanent, so we can retrieve them at the end
	// for display.
	permVars := make(map[logic.Var]struct{})
	for _, x := range dummy.Vars() {
		permVars[x] = struct{}{}
	}
	c := compile(dummy, permVars)
	c.Code = optimizeInstructions(c.Code)
	c.Functor = Functor{}
	// Remove deallocate, so we can retrieve the vars at the end of execution.
	n := len(c.Code)
	if _, ok := c.Code[n-1].(Deallocate); ok {
		c.Code = c.Code[:n-1]
	}
	// Add halt instruction
	c.Code = append(c.Code, Halt{})
	return c, nil
}

func isBuiltin(term *logic.Comp) bool {
	ind := term.Indicator()
	return ind == dsl.Indicator("!", 0) || ind == dsl.Indicator("fail", 0)
}

func (ctx *compileCtx) compileBodyTerm(pos int, term *logic.Comp) []Instruction {
	// Special cases.
	switch term.Indicator() {
	case dsl.Indicator("!", 0):
		if pos == 0 {
			return []Instruction{NeckCut{}}
		}
		return []Instruction{Cut{}}
	case dsl.Indicator("fail", 0):
		return []Instruction{Fail{}}
	}
	// Regular goal: put term args into registers X0-Xn and issue a call to f/n.
	ctx.instrs = nil
	for i, arg := range term.Args {
		ctx.instrs = append(ctx.instrs, ctx.putTerm(arg, RegAddr(i))...)
	}
	ctx.instrs = append(ctx.instrs, Call{toFunctor(term.Indicator())})
	return ctx.instrs
}

func compile(clause *logic.Clause, permVars map[logic.Var]struct{}) *Clause {
	functor := toFunctor(clause.Head.(*logic.Comp).Indicator())
	c := &Clause{Functor: functor}
	ctx := newCompileCtx(numArgs(clause))
	// Designate address for each var (either in a register or on the stack)
	currStack := 0
	for _, x := range clause.Vars() {
		if _, ok := permVars[x]; ok {
			ctx.varAddr[x] = StackAddr(currStack)
			currStack++
		} else if x != logic.AnonymousVar {
			ctx.varAddr[x] = ctx.topReg
			ctx.topReg++
		}
	}
	// If call requires an environment, add an allocate-deallocate pair to the clause.
	var header, footer []Instruction
	if currStack > 0 || hasDeepCut(clause) || hasNonLastCall(clause) {
		header = []Instruction{Allocate{currStack}}
		footer = []Instruction{Deallocate{}}
	}
	// Compile clause head
	for i, term := range clause.Head.(*logic.Comp).Args {
		c.Code = append(c.Code, ctx.getTerm(term, RegAddr(i))...)
	}
	for len(ctx.delayed) > 0 {
		buf := ctx.delayed
		ctx.delayed = nil
		for _, compound := range buf {
			c.Code = append(c.Code, ctx.getTerm(compound.t, compound.addr)...)
		}
	}
	// Compile clause body
	for i, term := range clause.Body {
		c.Code = append(c.Code, ctx.compileBodyTerm(i, term.(*logic.Comp))...)
	}
	// Add "proceed" instruction for facts, and when a body ends with a builtin call,
	// e.g., '!'
	n := len(clause.Body)
	if n == 0 || isBuiltin(clause.Body[n-1].(*logic.Comp)) {
		c.Code = append(c.Code, Proceed{})
	}
	c.Code = append(header, c.Code...)
	c.Code = append(c.Code, footer...)
	c.NumRegisters = int(ctx.topReg)
	return c
}

func optimizeInstructions(code []Instruction) []Instruction {
	var buf []Instruction
	for _, instr := range code {
		// Remove nils
		if instr == nil {
			continue
		}
		// Simply append if there's no instruction yet.
		n := len(buf)
		if n < 1 {
			buf = append(buf, instr)
			continue
		}
		// Inline CallMeta instruction
		if instr, ok := instr.(Call); ok && instr.Functor.Name == "call" {
			callMeta := CallMeta{
				Addr:   RegAddr(0),
				Params: make([]Addr, instr.Functor.Arity-1),
			}
			for i := 0; i < instr.Functor.Arity-1; i++ {
				callMeta.Params[i] = RegAddr(i + 1)
			}
			buf = append(buf, callMeta)
			continue
		}
		// Otherwise, simply append instruction to buffer.
		buf = append(buf, instr)
	}
	return buf
}

func optimizeLastCall(code []Instruction) []Instruction {
	// If code ends with Deallocate, swap it with the previous instruction,
	// that may be either Call or CallMeta.
	{
		n := len(code)
		dealloc, isDeallocate := code[n-1].(Deallocate)
		if isDeallocate {
			code[n-2], code[n-1] = dealloc, code[n-2]
		}
	}
	// If code ends with Call, change it for Execute
	{
		n := len(code)
		call, isCall := code[n-1].(Call)
		if isCall {
			code[n-1] = Execute{call.Functor}
		}
	}
	// If code ends with CallMeta, change it for ExecuteMeta
	{
		n := len(code)
		call, isCallMeta := code[n-1].(CallMeta)
		if isCallMeta {
			code[n-1] = ExecuteMeta{call.Addr, call.Params}
		}
	}
	return code
}

// Create level-1 index of clauses, based on their first arg.
func compileClausesWithSameFunctor(clauses []*logic.Clause) *Clause {
	if len(clauses) == 1 {
		return Compile(clauses[0])
	}
	if len(clauses[0].Head.(*logic.Comp).Args) == 0 {
		// No first arg, impossible to index.
		return compileSequence(clauses)
	}
	subSeqs, anyNonVar := splitSubsequences(clauses)
	if !anyNonVar {
		// All first arg are vars, impossible to index.
		return compileSequence(clauses)
	}
	var numReg int
	codes := make([]*Clause, len(subSeqs))
	for i, subSeq := range subSeqs {
		codes[i] = compileSubSequence(subSeq)
		if codes[i].NumRegisters > numReg {
			numReg = codes[i].NumRegisters
		}
	}
	addChoiceLinks(codes)
	return &Clause{
		Functor:      toFunctor(clauses[0].Head.(*logic.Comp).Indicator()),
		NumRegisters: numReg,
		Code:         codes[0].Code,
	}
}

// Compile a subsequence of clauses with non-var first argument.
//
// Clauses are indexed on whether their first argument is a constant,
// functor or list, accelerating matching during unification.
//
// Clauses with same first arg are also placed in a linked-list of
// try-retry-trust instructions.
func compileSubSequence(clauses []*logic.Clause) *Clause {
	var numReg int
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		codes[i] = Compile(clause)
		if codes[i].NumRegisters > numReg {
			numReg = codes[i].NumRegisters
		}
	}
	addChoiceLinks(codes)
	if len(codes) == 1 {
		// TODO: maybe index this one, if it's a nonvar?
		return codes[0]
	}
	// Group clauses by type and index key.
	constIndex := make(map[Constant][]InstrAddr)
	structIndex := make(map[Functor][]InstrAddr)
	var listIndex []InstrAddr
	for i, clause := range clauses {
		arg := clause.Head.(*logic.Comp).Args[0]
		switch a := arg.(type) {
		case logic.Atom:
			constIndex[toConstant(a)] = append(constIndex[toConstant(a)], InstrAddr{codes[i], 1})
		case logic.Int:
			constIndex[toConstant(a)] = append(constIndex[toConstant(a)], InstrAddr{codes[i], 1})
		case *logic.Comp:
			f := toFunctor(a.Indicator())
			structIndex[f] = append(structIndex[f], InstrAddr{codes[i], 1})
		case *logic.List:
			listIndex = append(listIndex, InstrAddr{codes[i], 1})
		default:
			panic(fmt.Sprintf("compileSubSequences: unexpected term type %T (%v)", arg, arg))
		}
	}
	// Create first indexing instruction.
	switchOnTerm := SwitchOnTerm{
		IfVar:      InstrAddr{codes[0], 0},
		IfConstant: InstrAddr{fail, 0},
		IfPair:     InstrAddr{fail, 0},
		IfStruct:   InstrAddr{fail, 0},
	}
	indexClause := &Clause{
		Functor:      toFunctor(clauses[0].Head.(*logic.Comp).Indicator()),
		NumRegisters: numReg,
		Code:         []Instruction{nil},
	}
	putCode := func(instrs ...Instruction) InstrAddr {
		pos := len(indexClause.Code)
		indexClause.Code = append(indexClause.Code, instrs...)
		return InstrAddr{indexClause, pos}
	}
	// Index constants.
	if len(constIndex) > 0 {
		switchOnConst := SwitchOnConstant{Continuation: make(map[Constant]InstrAddr)}
		switchOnTerm.IfConstant = putCode(switchOnConst)
		for name, addrs := range constIndex {
			if len(addrs) == 1 {
				switchOnConst.Continuation[name] = addrs[0]
				continue
			}
			switchOnConst.Continuation[name] = putCode(addrsToInstrs(addrs)...)
		}
	}
	// Index structures.
	if len(structIndex) > 0 {
		switchOnStruct := SwitchOnStruct{Continuation: make(map[Functor]InstrAddr)}
		switchOnTerm.IfStruct = putCode(switchOnStruct)
		for functor, addrs := range structIndex {
			if len(addrs) == 1 {
				switchOnStruct.Continuation[functor] = addrs[0]
				continue
			}
			switchOnStruct.Continuation[functor] = putCode(addrsToInstrs(addrs)...)
		}
	}
	// Index lists.
	if len(listIndex) > 0 {
		if len(listIndex) == 1 {
			switchOnTerm.IfPair = listIndex[0]
		} else {
			switchOnTerm.IfPair = putCode(addrsToInstrs(listIndex)...)
		}
	}
	indexClause.Code[0] = switchOnTerm
	return indexClause
}

func addrsToInstrs(addrs []InstrAddr) []Instruction {
	if len(addrs) == 1 {
		return nil
	}
	instrs := make([]Instruction, len(addrs))
	for i, addr := range addrs {
		if i == 0 {
			instrs[i] = Try{addr}
		} else if i < len(addrs)-1 {
			instrs[i] = Retry{addr}
		} else {
			instrs[i] = Trust{addr}
		}
	}
	return instrs
}

// Split sequence into subsequences, where clauses whose first arg is
// a nonvar are grouped together, while clauses whose first arg is a
// var are isolated.
//
// Example:
//    f(g(A, b)).  |
//    f(t).        |-- first subsequence
//    f([X|T]).    |
//    f(X, _).    |--- second subsequence
//    f(a, b, c).  |-- third subsequence
//    f(g(u, v)).  |
//    f(U, V).    |--- fourth subsequence
//    f(X, p(X)).  |-- fifth subsequence
//
func splitSubsequences(clauses []*logic.Clause) ([][]*logic.Clause, bool) {
	var buf []*logic.Clause
	var subSeqs [][]*logic.Clause
	var anyNonVar bool
	for _, clause := range clauses {
		arg := clause.Head.(*logic.Comp).Args[0]
		if _, ok := arg.(logic.Var); ok {
			if buf != nil {
				subSeqs = append(subSeqs, buf)
				buf = nil
			}
			subSeqs = append(subSeqs, []*logic.Clause{clause})
		} else {
			anyNonVar = true
			buf = append(buf, clause)
		}
	}
	if buf != nil {
		subSeqs = append(subSeqs, buf)
	}
	return subSeqs, anyNonVar
}

func compileSequence(clauses []*logic.Clause) *Clause {
	// Compile each clause in isolation.
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		codes[i] = Compile(clause)
	}
	addChoiceLinks(codes)
	return codes[0]
}

func addChoiceLinks(clauses []*Clause) {
	if len(clauses) < 2 {
		return
	}
	for i, clause := range clauses {
		var instr Instruction
		if i == 0 {
			instr = TryMeElse{InstrAddr{clauses[i+1], 0}}
		} else if i < len(clauses)-1 {
			instr = RetryMeElse{InstrAddr{clauses[i+1], 0}}
		} else {
			instr = TrustMe{}
		}
		clause.Code = append([]Instruction{instr}, clause.Code...)
	}
}

// CompileClauses returns a list of compiled clauses. Each corresponds with
// a functor f/n, and all sub-clauses that implement the same functor.
func CompileClauses(clauses []*logic.Clause) ([]*Clause, error) {
	m := make(map[logic.Indicator][]*logic.Clause)
	var order []logic.Indicator
	for _, clause := range clauses {
		c, err := clause.Normalize()
		if err != nil {
			return nil, fmt.Errorf("compiling clause %v: %v", clause, err)
		}
		ind := c.Head.(*logic.Comp).Indicator()
		if _, ok := m[ind]; !ok {
			order = append(order, ind)
		}
		m[ind] = append(m[ind], c)
	}
	var cs []*Clause
	for _, ind := range order {
		cs2 := m[ind]
		cs = append(cs, compileClausesWithSameFunctor(cs2))
	}
	return cs, nil
}
