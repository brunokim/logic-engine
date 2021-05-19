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
	m.attributes = make(map[int]map[string]Cell)
	m.interrupt = make(chan struct{})
	return m
}

// Reset creates a new machine with clean state, copying only the compiled clauses.
func (m *Machine) Reset() *Machine {
	cloned := new(Machine)
	cloned.Code = m.Code
	cloned.Reg = make([]Cell, len(m.Reg))
	cloned.DebugFilename = m.DebugFilename
	cloned.IterLimit = m.IterLimit
	cloned.attributes = make(map[int]map[string]Cell)
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

// ---- conversion

type flatClause []*logic.Comp

func flatten(clause *logic.Clause) flatClause {
	goals := make(flatClause, len(clause.Body)+1)
	switch head := clause.Head.(type) {
	case logic.Atom:
		goals[0] = logic.NewComp(head.Name)
	case *logic.Comp:
		goals[0] = head
	default:
		panic(fmt.Sprintf("Unhandled clause head term type: %T (%v)", clause.Head, clause.Head))
	}
	copy(goals[1:], toGoals(clause.Body))
	return goals
}

func toGoal(term logic.Term) *logic.Comp {
	switch t := term.(type) {
	case *logic.Comp:
		return t
	case logic.Atom:
		return logic.NewComp(t.Name)
	case logic.Var:
		return logic.NewComp("call", t)
	default:
		panic(fmt.Sprintf("Unhandled goal type %T (%v)", term, term))
	}
}

func toGoals(terms []logic.Term) flatClause {
	goals := make(flatClause, len(terms))
	for i, term := range terms {
		goals[i] = toGoal(term)
	}
	return goals
}

func goalVars(goals flatClause) []logic.Var {
	var xs []logic.Var
	m := make(map[logic.Var]struct{})
	for _, goal := range goals {
		for _, x := range logic.Vars(goal) {
			if _, ok := m[x]; ok {
				continue
			}
			m[x] = struct{}{}
			xs = append(xs, x)
		}
	}
	return xs
}

// ----

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
func permanentVars(clause flatClause) map[logic.Var]struct{} {
	if len(clause) < 2 {
		return map[logic.Var]struct{}{}
	}
	seen := make(map[logic.Var]struct{})
	perm := make(map[logic.Var]struct{})
	// Vars in head are considered to be part of the first body term.
	for _, x := range logic.Vars(clause[0]) {
		seen[x] = struct{}{}
	}
	for _, x := range logic.Vars(clause[1]) {
		seen[x] = struct{}{}
	}
	// Walk through other clause terms; vars that appear in more than
	// one term are permanent.
	for _, c := range clause[2:] {
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

func numArgs(clause flatClause) int {
	max := 0
	for _, term := range clause {
		n := len(term.Args)
		if n > max {
			max = n
		}
	}
	return max
}

type compound struct {
	t    logic.Term
	addr RegAddr
}

// compileCtx wraps all the state necessary to compile a single clause.
type compileCtx struct {
	clause  *Clause
	topReg  RegAddr
	seen    map[logic.Var]struct{}
	varAddr map[logic.Var]Addr
	delayed []compound
	instrs  []Instruction
}

func newCompileCtx(clause *Clause, numArgs int) *compileCtx {
	return &compileCtx{
		clause:  clause,
		topReg:  RegAddr(numArgs),
		seen:    make(map[logic.Var]struct{}),
		varAddr: make(map[logic.Var]Addr),
	}
}

// ---- get/unify/put variables

func (ctx *compileCtx) getVar(x logic.Var, regAddr RegAddr) Instruction {
	if x == logic.AnonymousVar {
		return nil
	}
	if _, ok := ctx.seen[x]; ok {
		return getValue{ctx.varAddr[x], regAddr}
	}
	ctx.seen[x] = struct{}{}
	return getVariable{ctx.varAddr[x], regAddr}
}

func (ctx *compileCtx) unifyVar(x logic.Var) Instruction {
	if x == logic.AnonymousVar {
		return unifyVoid{}
	}
	if _, ok := ctx.seen[x]; ok {
		return unifyValue{ctx.varAddr[x]}
	}
	ctx.seen[x] = struct{}{}
	return unifyVariable{ctx.varAddr[x]}
}

func (ctx *compileCtx) putVar(x logic.Var, regAddr RegAddr) Instruction {
	if x == logic.AnonymousVar {
		addr := ctx.topReg
		ctx.topReg++
		return putVariable{addr, regAddr}
	}
	if _, ok := ctx.seen[x]; ok {
		return putValue{ctx.varAddr[x], regAddr}
	}
	ctx.seen[x] = struct{}{}
	return putVariable{ctx.varAddr[x], regAddr}
}

// ---- get terms

func (ctx *compileCtx) getTerm(term logic.Term, addr RegAddr) []Instruction {
	switch t := term.(type) {
	case logic.Atom:
		return []Instruction{getConstant{toConstant(t), addr}}
	case logic.Int:
		return []Instruction{getConstant{toConstant(t), addr}}
	case logic.Var:
		return []Instruction{ctx.getVar(t, addr)}
	case *logic.Comp:
		instrs := make([]Instruction, len(t.Args)+1)
		instrs[0] = getStruct{toFunctor(t.Indicator()), addr}
		for i, arg := range t.Args {
			instrs[i+1] = ctx.unifyArg(arg)
		}
		return instrs
	case *logic.List:
		return ctx.getPair(ListPair, addr, t.Terms[0], t.Slice(1))
	case *logic.Assoc:
		return ctx.getPair(AssocPair, addr, t.Key, t.Val)
	case *logic.Dict:
		return ctx.getPair(DictPair, addr, t.Assocs[0], t.Tail())
	default:
		panic(fmt.Sprintf("wam.getTerm: unhandled type %T (%v)", term, term))
	}
}

func (ctx *compileCtx) getPair(tag PairTag, addr RegAddr, head, tail logic.Term) []Instruction {
	return []Instruction{getPair{tag, addr}, ctx.unifyArg(head), ctx.unifyArg(tail)}
}

// ---- unify terms

func (ctx *compileCtx) delayComplexArg(arg logic.Term) Instruction {
	addr := RegAddr(ctx.topReg)
	ctx.topReg++
	ctx.delayed = append(ctx.delayed, compound{t: arg, addr: addr})
	return unifyVariable{addr}
}

func (ctx *compileCtx) unifyArg(arg logic.Term) Instruction {
	switch a := arg.(type) {
	case logic.Atom:
		return unifyConstant{toConstant(a)}
	case logic.Int:
		return unifyConstant{toConstant(a)}
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

// ---- put terms

func (ctx *compileCtx) putTerm(term logic.Term, addr RegAddr) []Instruction {
	switch t := term.(type) {
	case logic.Atom:
		return []Instruction{putConstant{toConstant(t), addr}}
	case logic.Int:
		return []Instruction{putConstant{toConstant(t), addr}}
	case logic.Var:
		return []Instruction{ctx.putVar(t, addr)}
	case *logic.Comp:
		instrs := make([]Instruction, len(t.Args)+1)
		instrs[0] = putStruct{toFunctor(t.Indicator()), addr}
		ctx.setArgs(t.Args, instrs)
		return instrs
	case *logic.List:
		return ctx.putPair(ListPair, addr, t.Terms[0], t.Slice(1))
	case *logic.Assoc:
		return ctx.putPair(AssocPair, addr, t.Key, t.Val)
	case *logic.Dict:
		return ctx.putPair(DictPair, addr, t.Assocs[0], t.Tail())
	default:
		panic(fmt.Sprintf("wam.putTerm: unhandled type %T (%v)", term, term))
	}
}

func (ctx *compileCtx) putPair(tag PairTag, addr RegAddr, head, tail logic.Term) []Instruction {
	instrs := []Instruction{putPair{tag, addr}, nil, nil}
	ctx.setArgs([]logic.Term{head, tail}, instrs)
	return instrs
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
		return unifyConstant{toConstant(a)}
	case logic.Int:
		return unifyConstant{toConstant(a)}
	case logic.Var:
		return ctx.unifyVar(a)
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
	return unifyValue{addr}
}

// ---- term addr

func (ctx *compileCtx) ensureVar(x logic.Var) {
	if x == logic.AnonymousVar {
		return
	}
	if _, ok := ctx.seen[x]; ok {
		return
	}
	ctx.seen[x] = struct{}{}
	addr := ctx.topReg
	ctx.topReg++
	ctx.instrs = append(ctx.instrs, putVariable{ctx.varAddr[x], addr})
}

func (ctx *compileCtx) termAddr(term logic.Term) Addr {
	switch t := term.(type) {
	case logic.Atom:
		return ConstantAddr{toConstant(t)}
	case logic.Int:
		return ConstantAddr{toConstant(t)}
	case logic.Ptr:
		return ConstantAddr{toConstant(t)}
	case logic.Var:
		ctx.ensureVar(t)
		return ctx.varAddr[t]
	}
	addr := ctx.topReg
	ctx.topReg++
	ctx.instrs = append(ctx.instrs, ctx.putTerm(term, addr)...)
	return addr
}

// ---- compiling terms

func Compile(clause *logic.Clause) *Clause {
	return compile(flatten(clause))
}

func compile(clause flatClause) *Clause {
	c := compile0(clause, permanentVars(clause))
	c.Code = optimizeLastCall(optimizeInstructions(c.Code))
	return c
}

func compileQuery(query []logic.Term) *Clause {
	dummy := make(flatClause, len(query)+1)
	dummy[0] = logic.NewComp("dummy")
	for i, term := range query {
		dummy[i+1] = toGoal(term)
	}
	// All vars in a query are made permanent, so we can retrieve them at the end
	// for display.
	permVars := make(map[logic.Var]struct{})
	for _, x := range goalVars(dummy) {
		permVars[x] = struct{}{}
	}
	c := compile0(dummy, permVars)
	c.Code = optimizeInstructions(c.Code)
	c.Functor = Functor{}
	// Remove deallocate, so we can retrieve the vars at the end of execution.
	n := len(c.Code)
	if _, ok := c.Code[n-1].(deallocate); ok {
		c.Code = c.Code[:n-1]
		n--
	}
	// Remove proceed, if the clause ends with an inline predicate.
	if _, ok := c.Code[n-1].(proceed); ok {
		c.Code = c.Code[:n-1]
		n--
	}
	// Add halt instruction
	c.Code = append(c.Code, halt{})
	return c
}

func (ctx *compileCtx) flattenControl(terms0 flatClause) flatClause {
	terms := make(flatClause, len(terms0))
	copy(terms, terms0)
	var body flatClause
	labelID := 1
	for len(terms) > 0 {
		goal := terms[0]
		terms = terms[1:]
		if goal.Functor == "and" {
			terms = append(toGoals(goal.Args), terms...)
			continue
		}
		switch goal.Indicator() {
		default:
			body = append(body, goal)
		case dsl.Indicator("\\+", 1):
			target := goal.Args[0]
			targetID, endID := labelID, labelID+1
			labelID += 2
			// Ensure that variables referenced within Target are initialized.
			ctx.instrs = nil
			for _, x := range logic.Vars(goal) {
				ctx.ensureVar(x)
			}
			ctx.clause.Code = append(ctx.clause.Code, ctx.instrs...)
			// Inline \+(Target) :- ->(Target, false, true).
			goals := flatClause{
				comp("asm", comp("try", comp("instr", ptr(ctx.clause), int_(-targetID)))),
				comp("asm", comp("trust", comp("instr", ptr(ctx.clause), int_(-endID)))),
				comp("asm", comp("label", int_(targetID))),
				toGoal(target),
				comp("asm", atom("cut")),
				comp("asm", atom("fail")),
				comp("asm", comp("label", int_(endID))),
			}
			terms = append(goals, terms...)
		case dsl.Indicator("->", 3):
			cond, then_, else_ := goal.Args[0], goal.Args[1], goal.Args[2]
			thenID, elseID, endID := labelID, labelID+1, labelID+2
			labelID += 3
			// Ensure that variables referenced in any branch are initialized.
			ctx.instrs = nil
			for _, x := range logic.Vars(goal) {
				ctx.ensureVar(x)
			}
			ctx.clause.Code = append(ctx.clause.Code, ctx.instrs...)
			// Inline cond, then and else branches, adding control instructions to move around.
			goals := flatClause{
				comp("asm", comp("try", comp("instr", ptr(ctx.clause), int_(-thenID)))),
				comp("asm", comp("trust", comp("instr", ptr(ctx.clause), int_(-elseID)))),
				comp("asm", comp("label", int_(thenID))),
				toGoal(cond),
				comp("asm", atom("cut")),
				toGoal(then_),
				comp("asm", comp("jump", comp("instr", ptr(ctx.clause), int_(-endID)))),
				comp("asm", comp("label", int_(elseID))),
				toGoal(else_),
				comp("asm", comp("label", int_(endID))),
			}
			terms = append(goals, terms...)
		}
	}
	return body
}

func (ctx *compileCtx) compileBodyTerm(pos int, term *logic.Comp) []Instruction {
	ctx.instrs = nil
	if term.Functor == "call" {
		callee := ctx.termAddr(term.Args[0])
		params := make([]Addr, len(term.Args)-1)
		for i, param := range term.Args[1:] {
			params[i] = ctx.termAddr(param)
		}
		ctx.instrs = append(ctx.instrs, callMeta{Addr: callee, Params: params})
		return ctx.instrs
	}
	switch term.Indicator() {
	case dsl.Indicator("!", 0):
		if pos == 0 {
			return []Instruction{neckCut{}}
		}
		return []Instruction{cut{}}
	case dsl.Indicator("true", 0):
		return []Instruction{}
	case dsl.Indicator("fail", 0), dsl.Indicator("false", 0):
		return []Instruction{fail{}}
	case dsl.Indicator("asm", 1):
		return []Instruction{DecodeInstruction(term.Args[0])}
	case dsl.Indicator("=", 2):
		x := ctx.termAddr(term.Args[0])
		y := ctx.termAddr(term.Args[1])
		ctx.instrs = append(ctx.instrs, inlineUnify{x, y})
		return ctx.instrs
	case dsl.Indicator("@<", 2),
		dsl.Indicator("@=<", 2),
		dsl.Indicator("@>=", 2),
		dsl.Indicator("@>", 2),
		dsl.Indicator("==", 2),
		dsl.Indicator("\\==", 2):
		x := ctx.termAddr(term.Args[0])
		y := ctx.termAddr(term.Args[1])
		pred := comparisonPredicates[term.Functor]
		ctx.instrs = append(ctx.instrs, builtinComparisonInstruction(pred, x, y))
		return ctx.instrs
	case dsl.Indicator("atom", 1),
		dsl.Indicator("int", 1),
		dsl.Indicator("ptr", 1),
		dsl.Indicator("var", 1),
		dsl.Indicator("list", 1),
		dsl.Indicator("assoc", 1),
		dsl.Indicator("dict", 1):
		x := ctx.termAddr(term.Args[0])
		pred := typeCheckPredicates[term.Functor]
		ctx.instrs = append(ctx.instrs, builtinTypeCheckInstruction(pred, x))
		return ctx.instrs
	case dsl.Indicator("get_attr", 2):
		x := ctx.termAddr(term.Args[0])
		attr := ctx.termAddr(term.Args[1])
		ctx.instrs = append(ctx.instrs, getAttr{x, attr})
		return ctx.instrs
	case dsl.Indicator("put_attr", 2):
		x := ctx.termAddr(term.Args[0])
		attr := ctx.termAddr(term.Args[1])
		ctx.instrs = append(ctx.instrs, putAttr{x, attr})
		return ctx.instrs
	case dsl.Indicator("del_attr", 2):
		x := ctx.termAddr(term.Args[0])
		attr := ctx.termAddr(term.Args[1])
		ctx.instrs = append(ctx.instrs, delAttr{x, attr})
		return ctx.instrs
	default:
		// Regular goal: put term args into registers X0-Xn and issue a call to f/n.
		for i, arg := range term.Args {
			ctx.instrs = append(ctx.instrs, ctx.putTerm(arg, RegAddr(i))...)
		}
		ctx.instrs = append(ctx.instrs, call{toFunctor(term.Indicator())})
		return ctx.instrs
	}
}

func compile0(clause flatClause, permVars map[logic.Var]struct{}) *Clause {
	functor := toFunctor(clause[0].Indicator())
	c := &Clause{Functor: functor}
	ctx := newCompileCtx(c, numArgs(clause))
	// Designate address for each var (either in a register or on the stack)
	currStack := 0
	for _, x := range goalVars(clause) {
		if _, ok := permVars[x]; ok {
			ctx.varAddr[x] = StackAddr(currStack)
			currStack++
		} else if x != logic.AnonymousVar {
			ctx.varAddr[x] = ctx.topReg
			ctx.topReg++
		}
	}
	// Compile clause head
	for i, term := range clause[0].Args {
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
	body := ctx.flattenControl(clause[1:])
	for i, term := range body {
		c.Code = append(c.Code, ctx.compileBodyTerm(i, term)...)
	}
	// Add "proceed" instruction for facts and when a body doesn't end with a call.
	if requiresProceed(c.Code) {
		c.Code = append(c.Code, proceed{Run})
	}
	// If call requires an environment, add an allocate-deallocate pair to the clause.
	if currStack > 0 || requiresEnv(c.Code) {
		c.Code = append([]Instruction{allocate{currStack}}, c.Code...)
		c.Code = append(c.Code, deallocate{})
	}
	c.NumRegisters = int(ctx.topReg)
	return c
}

// If instructions don't end with call, adds a proceed instruction.
func requiresProceed(code []Instruction) bool {
	n := len(code)
	if n == 0 {
		return true
	}
	_, isLastCall := code[n-1].(call)
	_, isLastCallMeta := code[n-1].(callMeta)
	_, isLastProceed := code[n-1].(proceed)
	return !(isLastCall || isLastCallMeta || isLastProceed)
}

func requiresEnv(code []Instruction) bool {
	for i, instr := range code {
		switch instr.(type) {
		case cut:
			return true
		case call:
			if i < len(code)-1 {
				return true
			}
		}
	}
	return false
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
		// Otherwise, simply append instruction to buffer.
		buf = append(buf, instr)
	}
	return buf
}

func optimizeLastCall(code []Instruction) []Instruction {
	// If code ends with deallocate, swap it with the previous instruction,
	// that may be either call or callMeta.
	{
		n := len(code)
		dealloc, isdeallocate := code[n-1].(deallocate)
		if isdeallocate {
			code[n-2], code[n-1] = dealloc, code[n-2]
		}
	}
	// If code ends with call, change it for execute
	{
		n := len(code)
		call, isCall := code[n-1].(call)
		if isCall {
			code[n-1] = execute{call.Functor}
		}
	}
	// If code ends with callMeta, change it for executeMeta
	{
		n := len(code)
		call, isCallMeta := code[n-1].(callMeta)
		if isCallMeta {
			code[n-1] = executeMeta{call.Addr, call.Params}
		}
	}
	return code
}

// ASSUMPTION: labels are only used for jumping around within a clause.
// ASSUMPTION: labels are only used with try/retry/trust/jump/switch instructions.
func optimizeLabels(code []Instruction) []Instruction {
	var buf []Instruction
	labelPos := make(map[int]int)
	pending := make(map[int]struct{})
	// Remove labels from code and store their positions.
	// Also save the location of instructions that reference labels.
	for _, instr := range code {
		if l, ok := instr.(label); ok {
			labelPos[l.ID] = len(buf)
			continue
		}
		for _, instrAddr := range instructionPointers(instr) {
			if instrAddr.Pos >= 0 {
				continue
			}
			pending[len(buf)] = struct{}{}
		}
		buf = append(buf, instr)
	}
	// Replace relative references with absolute references.
	for i, instr := range buf {
		buf[i] = replaceInstructionPointer(instr, func(instrAddr InstrAddr) InstrAddr {
			if instrAddr.Pos >= 0 {
				return instrAddr
			}
			labelID := -instrAddr.Pos
			pos, ok := labelPos[labelID]
			if !ok {
				panic(fmt.Sprintf("Unknown label %d", labelID))
			}
			return InstrAddr{instrAddr.Clause, pos}
		})
	}
	return buf
}

// ---- indexing

// Create level-1 index of clauses, based on their first arg.
func compileClauseGroup(ind logic.Indicator, clauses []flatClause) *Clause {
	if len(clauses) == 1 {
		return compile(clauses[0])
	}
	if ind.Arity == 0 {
		// No first arg, impossible to index.
		return compileSequenceNoIndex(clauses)
	}
	seqs, anyNonVar := splitOnVarFirstArg(clauses)
	if !anyNonVar {
		// All first arg are vars, impossible to index.
		return compileSequenceNoIndex(clauses)
	}
	var numReg int
	compiledClauses := make([]*Clause, len(seqs))
	for i, seq := range seqs {
		compiledClauses[i] = compileSequence(ind, seq)
		if compiledClauses[i].NumRegisters > numReg {
			numReg = compiledClauses[i].NumRegisters
		}
	}
	addChoiceLinks(compiledClauses)
	return &Clause{
		Functor:      toFunctor(ind),
		NumRegisters: numReg,
		Code:         compiledClauses[0].Code,
	}
}

// Compile a subsequence of clauses with non-var first argument.
//
// Clauses are indexed on whether their first argument is a constant,
// functor or list, accelerating matching during unification.
//
// Clauses with same first arg are also placed in a linked-list of
// try-retry-trust instructions.
func compileSequence(ind logic.Indicator, clauses []flatClause) *Clause {
	var numReg int
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		codes[i] = compile(clause)
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
	var listIndex, assocIndex, dictIndex []InstrAddr
	for i, clause := range clauses {
		arg := clause[0].Args[0]
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
		case *logic.Assoc:
			assocIndex = append(assocIndex, InstrAddr{codes[i], 1})
		case *logic.Dict:
			dictIndex = append(dictIndex, InstrAddr{codes[i], 1})
		default:
			panic(fmt.Sprintf("compileSubSequences: unexpected term type %T (%v)", arg, arg))
		}
	}
	// Create first indexing instruction.
	switchOnTerm := switchOnTerm{
		IfVar:      InstrAddr{codes[0], 0},
		IfConstant: InstrAddr{failClause, 0},
		IfStruct:   InstrAddr{failClause, 0},
		IfList:     InstrAddr{failClause, 0},
		IfAssoc:    InstrAddr{failClause, 0},
		IfDict:     InstrAddr{failClause, 0},
	}
	indexClause := &Clause{
		Functor:      toFunctor(ind),
		NumRegisters: numReg,
		Code:         []Instruction{nil}, // First instruction reserved for switch_on_term
	}
	labelID := 1
	putWithMark := func(instrs ...Instruction) InstrAddr {
		indexClause.Code = append(indexClause.Code, label{labelID})
		indexClause.Code = append(indexClause.Code, instrs...)
		instrAddr := InstrAddr{indexClause, -labelID}
		labelID++
		return instrAddr
	}
	putAddrs := func(addrs []InstrAddr) InstrAddr {
		if len(addrs) == 1 {
			return addrs[0]
		}
		instrs := make([]Instruction, len(addrs))
		for i, addr := range addrs {
			if i == 0 {
				instrs[i] = try{addr}
			} else if i < len(addrs)-1 {
				instrs[i] = retry{addr}
			} else {
				instrs[i] = trust{addr}
			}
		}
		return putWithMark(instrs...)
	}
	// Index constants.
	if len(constIndex) > 0 {
		switchOnConst := switchOnConstant{Continuation: make(map[Constant]InstrAddr)}
		switchOnTerm.IfConstant = putWithMark(switchOnConst)
		for name, addrs := range constIndex {
			switchOnConst.Continuation[name] = putAddrs(addrs)
		}
	}
	// Index structures.
	if len(structIndex) > 0 {
		switchOnStruct := switchOnStruct{Continuation: make(map[Functor]InstrAddr)}
		switchOnTerm.IfStruct = putWithMark(switchOnStruct)
		for functor, addrs := range structIndex {
			switchOnStruct.Continuation[functor] = putAddrs(addrs)
		}
	}
	// Index lists.
	if len(listIndex) > 0 {
		switchOnTerm.IfList = putAddrs(listIndex)
	}
	// Index assocs.
	if len(assocIndex) > 0 {
		switchOnTerm.IfAssoc = putAddrs(assocIndex)
	}
	// Index dicts.
	if len(dictIndex) > 0 {
		switchOnTerm.IfDict = putAddrs(dictIndex)
	}
	indexClause.Code[0] = switchOnTerm
	return indexClause
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
func splitOnVarFirstArg(clauses []flatClause) ([][]flatClause, bool) {
	var buf []flatClause
	var subSeqs [][]flatClause
	var anyNonVar bool
	for _, clause := range clauses {
		arg := clause[0].Args[0]
		if _, ok := arg.(logic.Var); ok {
			if buf != nil {
				subSeqs = append(subSeqs, buf)
				buf = nil
			}
			subSeqs = append(subSeqs, []flatClause{clause})
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

func compileSequenceNoIndex(clauses []flatClause) *Clause {
	// Compile each clause in isolation.
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		codes[i] = compile(clause)
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
			instr = tryMeElse{InstrAddr{clauses[i+1], 0}}
		} else if i < len(clauses)-1 {
			instr = retryMeElse{InstrAddr{clauses[i+1], 0}}
		} else {
			instr = trustMe{}
		}
		clause.Code = append([]Instruction{instr}, clause.Code...)
	}
}

func reachableClauses(clauses []*Clause) []*Clause {
	// Copy items in reverse order to stack
	stack := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		stack[len(clauses)-i-1] = clause
	}
	seen := make(map[*Clause]struct{})
	var order []*Clause
	for len(stack) > 0 {
		n := len(stack)
		clause := stack[n-1]
		stack = stack[:n-1]
		if _, ok := seen[clause]; ok || clause == nil {
			continue
		}
		order = append(order, clause)
		seen[clause] = struct{}{}
		for _, instr := range clause.Code {
			for _, instrAddr := range instructionPointers(instr) {
				clause := instrAddr.Clause
				if _, ok := seen[clause]; ok {
					continue
				}
				stack = append(stack, clause)
			}
		}
	}
	return order
}

// Option to control compilation.
type CompileOption interface {
	isCompileOption()
}

// Keep labels in control instructions, instead of replacing them for their actual
// position in code.
type KeepLabels struct{}

func (KeepLabels) isCompileOption() {}

// CompileClauses returns a list of compiled clauses. Each corresponds with
// a functor f/n, and all sub-clauses that implement the same functor.
func CompileClauses(clauses []*logic.Clause, options ...CompileOption) []*Clause {
	opts := make(map[CompileOption]struct{})
	for _, opt := range options {
		opts[opt] = struct{}{}
	}
	// Group clauses by functor and compile each group.
	m := make(map[logic.Indicator][]flatClause)
	var order []logic.Indicator
	for _, clause := range clauses {
		goals := flatten(clause)
		ind := goals[0].Indicator()
		if _, ok := m[ind]; !ok {
			order = append(order, ind)
		}
		m[ind] = append(m[ind], goals)
	}
	var cs []*Clause
	for _, ind := range order {
		cs2 := m[ind]
		cs = append(cs, compileClauseGroup(ind, cs2))
	}
	// Remove labels from clause bodies.
	if _, ok := opts[KeepLabels{}]; !ok {
		for _, clause := range reachableClauses(cs) {
			clause.Code = optimizeLabels(clause.Code)
		}
	}
	return cs
}
