package wam

import (
	"fmt"
	"sort"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/errors"
	"github.com/brunokim/logic-engine/logic"
)

// ---- special goals

var empty = struct{}{}

var controlFunctors = map[string]struct{}{
	"and": empty,
}

var controlIndicators = map[logic.Indicator]struct{}{
	dsl.Indicator("->", 3):     empty,
	dsl.Indicator("\\+", 1):    empty,
	dsl.Indicator("phrase", 2): empty,
	dsl.Indicator("phrase", 3): empty,
}

var inlinedFunctors = map[string]struct{}{
	"call": empty,
}

var inlinedIndicators = map[logic.Indicator]struct{}{
	dsl.Indicator("!", 0):        empty,
	dsl.Indicator("true", 0):     empty,
	dsl.Indicator("fail", 0):     empty,
	dsl.Indicator("false", 0):    empty,
	dsl.Indicator("asm", 1):      empty,
	dsl.Indicator("import", 1):   empty,
	dsl.Indicator("=", 2):        empty,
	dsl.Indicator("@<", 2):       empty,
	dsl.Indicator("@=<", 2):      empty,
	dsl.Indicator("@>=", 2):      empty,
	dsl.Indicator("@>", 2):       empty,
	dsl.Indicator("==", 2):       empty,
	dsl.Indicator("\\==", 2):     empty,
	dsl.Indicator("atom", 1):     empty,
	dsl.Indicator("int", 1):      empty,
	dsl.Indicator("ptr", 1):      empty,
	dsl.Indicator("var", 1):      empty,
	dsl.Indicator("list", 1):     empty,
	dsl.Indicator("assoc", 1):    empty,
	dsl.Indicator("dict", 1):     empty,
	dsl.Indicator("get_attr", 3): empty,
	dsl.Indicator("put_attr", 3): empty,
	dsl.Indicator("del_attr", 2): empty,
}

// ---- var sets

type varset map[logic.Var]struct{}

func varOccurs(goal *logic.Comp) varset {
	set := make(varset)
	for _, x := range logic.Vars(goal) {
		set[x] = empty
	}
	return set
}

// ---- conversion

type goal struct {
	pkg  string
	comp *logic.Comp
}
type flatClause []goal

func flatten(clause *logic.Clause) (flatClause, error) {
	var headGoal goal
	switch head := clause.Head.(type) {
	case logic.Atom:
		headGoal = goal{"", logic.NewComp(head.Name)}
	case *logic.Comp:
		headGoal = goal{"", head}
	default:
		return nil, errors.New("invalid clause head term type: %T (%v)", clause.Head, clause.Head)
	}
	goals, err := toGoals(clause.Body)
	if err != nil {
		return nil, errors.New("invalid clause body: %v", err)
	}
	return append([]goal{headGoal}, goals...), nil
}

func toGoal(term logic.Term) (goal, error) {
	switch t := term.(type) {
	case *logic.Comp:
		return goal{"", t}, nil
	case logic.Atom:
		return goal{"", logic.NewComp(t.Name)}, nil
	case logic.Var:
		return goal{"", logic.NewComp("call", t)}, nil
	case *logic.Assoc:
		pkg, ok := t.Key.(logic.Atom)
		if !ok {
			break
		}
		g, err := toGoal(t.Val)
		if err != nil {
			return goal{}, err
		}
		g.pkg = pkg.Name
		return g, nil
	}
	return goal{}, errors.New("invalid goal type %T (%v)", term, term)
}

func toGoals(terms []logic.Term) (flatClause, error) {
	goals := make(flatClause, len(terms))
	for i, term := range terms {
		var err error
		goals[i], err = toGoal(term)
		if err != nil {
			return nil, errors.New("goal #%d: %v", i+1, err)
		}
	}
	return goals, nil
}

func goalVars(goals flatClause) []logic.Var {
	var xs []logic.Var
	m := make(varset)
	for _, goal := range goals {
		for _, x := range logic.Vars(goal.comp) {
			if _, ok := m[x]; ok {
				continue
			}
			m[x] = empty
			xs = append(xs, x)
		}
	}
	return xs
}

// ---- DCG utilities

func phrase(dcg logic.Term, list, rest logic.Term) (flatClause, error) {
	dcgGoal, err := toGoal(dcg)
	if err != nil {
		return nil, err
	}
	comp := logic.DCGExpandComp(dcgGoal.comp, list, rest)
	return flatClause{goal{dcgGoal.pkg, comp}}, nil
}

// ---- permanent vars

// Compute the permanent vars of a logic clause.
//
// A permanent var is a local var in a call that is referenced in more
// than one chunk. They must be stored in the environment stack, or
// otherwise they may be overwritten if stored in a register, since a
// body term may use them in any ways.
func permanentVars(clause flatClause) (varset, []flatClause, error) {
	// Facts don't have permanent vars.
	if len(clause) < 2 {
		return varset{}, []flatClause{clause}, nil
	}
	// Split terms in chunks
	chunks, err := getChunks(clause)
	if err != nil {
		return nil, nil, err
	}
	// Add vars in first chunk.
	seen := chunkVars(chunks[0])
	perm := make(varset)
	// Walk through other chunks; vars that appear in more than
	// one chunk are permanent.
	for _, chunk := range chunks[1:] {
		for x := range chunkVars(chunk) {
			if _, ok := seen[x]; ok {
				perm[x] = empty
			} else {
				seen[x] = empty
			}
		}
	}
	delete(perm, logic.AnonymousVar)
	return perm, chunks, nil
}

func chunkVars(chunk flatClause) varset {
	vars := make(varset)
	for _, goal := range chunk {
		for _, x := range logic.Vars(goal.comp) {
			vars[x] = empty
		}
	}
	return vars
}

// Group terms into chunks, consisting of a sequence of inline terms ended by a predicate call.
func getChunks(clause flatClause) ([]flatClause, error) {
	var chunks []flatClause
	chunk := flatClause{clause[0]}
	goals := make(flatClause, len(clause)-1)
	copy(goals, clause[1:])
	for len(goals) > 0 {
		goal := goals[0]
		goals = goals[1:]
		// A nil goal.comp may be introduced by control goals to force a chunk to be split.
		if goal.pkg == "" && goal.comp != nil {
			term := goal.comp
			// Control predicates are expanded, add sub-terms to goals.
			if isControl(term) {
				subGoals, err := controlGoals(term)
				if err != nil {
					return nil, err
				}
				goals = append(subGoals, goals...)
				continue
			}
			// Inlined predicates don't manipulate registers, add them to the chunk.
			if isInlined(term) && term.Functor != "call" {
				chunk = append(chunk, goal)
				continue
			}
		}
		// Default case: predicate call should end current chunk.
		if goal.comp != nil {
			chunk = append(chunk, goal)
		}
		if len(chunk) > 0 {
			chunks = append(chunks, chunk)
		}
		chunk = flatClause{}
	}
	if len(chunk) > 0 {
		chunks = append(chunks, chunk)
	}
	return chunks, nil
}

func controlGoals(term *logic.Comp) (flatClause, error) {
	switch term.Functor {
	case "and":
		return toGoals(term.Args)
	}
	switch term.Indicator() {
	case dsl.Indicator("\\+", 1):
		return toGoals(term.Args)
	case dsl.Indicator("->", 3):
		goals, err := toGoals(term.Args)
		if err != nil {
			return nil, err
		}
		cond, then_, else_ := goals[0], goals[1], goals[2]
		// Force a chunk to be split after then and else, since we can't guarantee that
		// either of them will be executed contiguously to the remaining of the body.
		return flatClause{cond, then_, goal{}, else_, goal{}}, nil
	case dsl.Indicator("phrase", 2):
		return phrase(term.Args[0], term.Args[1], dsl.List())
	case dsl.Indicator("phrase", 3):
		return phrase(term.Args[0], term.Args[1], term.Args[2])
	}
	panic(fmt.Sprintf("Unimplemented control goals for %v", term))
}

func isControl(term *logic.Comp) bool {
	_, isControlFunctor := controlFunctors[term.Functor]
	_, isControlIndicator := controlIndicators[term.Indicator()]
	return isControlFunctor || isControlIndicator
}

func isInlined(term *logic.Comp) bool {
	_, isInlinedFunctor := inlinedFunctors[term.Functor]
	_, isInlinedIndicator := inlinedIndicators[term.Indicator()]
	return isInlinedFunctor || isInlinedIndicator
}

func isSpecial(term *logic.Comp) bool {
	return isControl(term) || isInlined(term)
}

// ---- regset

// regset is a set of register addresses, implemented as a sorted array.
type regset []RegAddr

func (r regset) index(reg RegAddr) (int, bool) {
	i := sort.Search(len(r), func(i int) bool { return r[i] >= reg })
	return i, i < len(r) && r[i] == reg
}

func (r regset) has(reg RegAddr) bool {
	_, ok := r.index(reg)
	return ok
}

func (r regset) add(reg RegAddr) regset {
	i, ok := r.index(reg)
	if ok {
		return r
	}
	r = append(r, -1)
	copy(r[i+1:], r[i:])
	r[i] = reg
	return r
}

func (r regset) remove(reg RegAddr) regset {
	i, ok := r.index(reg)
	if !ok {
		return r
	}
	copy(r[i:], r[i+1:])
	r = r[:len(r)-1]
	return r
}

// ---- register allocation

type registerAllocation struct {
	use      regset
	noUse    regset
	conflict regset
}

func chunkAllocationSets(pos int, chunk flatClause, temps varset) map[logic.Var]*registerAllocation {
	sets := make(map[logic.Var]*registerAllocation)
	for x := range temps {
		sets[x] = &registerAllocation{}
	}
	// 0. Annotate which register is used by which temp.
	usedRegsPerTemp := func(goal *logic.Comp) map[logic.Var]regset {
		used := make(map[logic.Var]regset)
		for i, arg := range goal.Args {
			x, ok := arg.(logic.Var)
			if !ok {
				continue
			}
			if _, ok = temps[x]; !ok {
				continue
			}
			used[x] = used[x].add(RegAddr(i))
		}
		return used
	}
	var numLastGoalRegs int
	var headUsed, lastGoalUsed map[logic.Var]regset
	// Analyze head, if this is the first chunk.
	if pos == 0 {
		head := chunk[0].comp
		headUsed = usedRegsPerTemp(head)
	}
	// Analyze last goal, if it's not an inlined goal.
	n := len(chunk)
	lastGoal := chunk[n-1].comp
	if n > 1 && !isSpecial(lastGoal) {
		numLastGoalRegs = len(lastGoal.Args)
		lastGoalUsed = usedRegsPerTemp(lastGoal)
	}
	// 1. Compute USE set
	for x := range temps {
		for _, reg := range headUsed[x] {
			sets[x].use = sets[x].use.add(reg)
		}
		for _, reg := range lastGoalUsed[x] {
			sets[x].use = sets[x].use.add(reg)
		}
	}
	// 2. Compute NOUSE set
	var allUsedRegs regset
	for _, regs := range lastGoalUsed {
		for _, reg := range regs {
			allUsedRegs.add(reg)
		}
	}
	for x := range temps {
		for _, reg := range allUsedRegs {
			if !sets[x].use.has(reg) {
				sets[x].noUse = sets[x].noUse.add(reg)
			}
		}
	}
	// 3. Compute CONFLICT set
	lastGoalVars := varOccurs(lastGoal)
	for x := range temps {
		if _, ok := lastGoalVars[x]; !ok {
			continue
		}
		for i := 0; i < numLastGoalRegs; i++ {
			reg := RegAddr(i)
			if !lastGoalUsed[x].has(reg) {
				sets[x].conflict = sets[x].conflict.add(reg)
			}
		}
	}
	return sets
}

// Compute USE, NOUSE and CONFLICT register sets for each temporary variable.
func tempAllocationSets(chunks []flatClause, permVars varset) map[logic.Var]*registerAllocation {
	tempSets := make(map[logic.Var]*registerAllocation)
	for i, chunk := range chunks {
		// Get chunk temporary variables.
		xs := chunkVars(chunk)
		temps := make(varset)
		for x := range xs {
			if _, ok := permVars[x]; !ok {
				temps[x] = empty
			}
		}
		for x, sets := range chunkAllocationSets(i, chunk, temps) {
			tempSets[x] = sets
		}
	}
	return tempSets
}

// ---- clause compiler

func numArgs(clause flatClause) int {
	max := 0
	for _, term := range clause {
		n := len(term.comp.Args)
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

// clauseCompiler wraps all the state necessary to compile a single clause.
type clauseCompiler struct {
	pc       *packageCompiler
	clause   *Clause
	freeRegs regset
	topReg   RegAddr
	topStack StackAddr
	register map[logic.Var]Addr
	content  map[Addr]logic.Var
	permVars varset
	tempSets map[logic.Var]*registerAllocation
	delayed  []compound
	instrs   []Instruction
	labelID  int
}

func newClauseCompiler(pc *packageCompiler, clause *Clause, numArgs int, permVars varset, tempSets map[logic.Var]*registerAllocation) *clauseCompiler {
	freeRegs := make(regset, numArgs)
	for i := 0; i < numArgs; i++ {
		freeRegs[i] = RegAddr(i)
	}
	return &clauseCompiler{
		pc:       pc,
		clause:   clause,
		freeRegs: freeRegs,
		topReg:   RegAddr(numArgs),
		register: make(map[logic.Var]Addr),
		content:  make(map[Addr]logic.Var),
		permVars: permVars,
		tempSets: tempSets,
	}
}

func (cc *clauseCompiler) nextReg() RegAddr {
	addr := cc.topReg
	cc.topReg++
	return addr
}

func (cc *clauseCompiler) nextStack() StackAddr {
	addr := cc.topStack
	cc.topStack++
	return addr
}

func (cc *clauseCompiler) allocReg(x logic.Var) RegAddr {
	alloc, ok := cc.tempSets[x]
	if !ok {
		panic(fmt.Sprintf("Temp variable %v doesn't have a allocation sets", x))
	}
	var addr RegAddr = -1
	for _, reg := range cc.freeRegs {
		// 1. Reserve a free register from the USE set.
		if alloc.use.has(reg) {
			addr = reg
			break
		}
		// 2. Reserve a register if it doesn't add a conflict with another var (NOUSE set)
		// or parameter (CONFLICT set).
		if alloc.noUse.has(reg) {
			continue
		}
		if alloc.conflict.has(reg) {
			continue
		}
		addr = reg
		break
	}
	if addr >= 0 {
		cc.freeRegs = cc.freeRegs.remove(addr)
		return addr
	}
	return cc.nextReg()
}

func (cc *clauseCompiler) nextAddr(x logic.Var) Addr {
	if x == logic.AnonymousVar {
		panic(fmt.Sprintf("Trying to allocate register for anonymous var"))
	}
	var addr Addr
	if _, ok := cc.permVars[x]; ok {
		addr = cc.nextStack()
	} else {
		if _, ok := cc.pc.opts[UseConflictAvoidanceAllocationStrategy{}]; ok {
			addr = cc.allocReg(x)
		} else {
			addr = cc.nextReg()
		}
	}
	cc.register[x] = addr
	cc.content[addr] = x
	return addr
}

// ---- get/unify/put variables

func (cc *clauseCompiler) getVar(x logic.Var, regAddr RegAddr) Instruction {
	if addr, ok := cc.register[x]; ok {
		return getValue{addr, regAddr}
	}
	addr := cc.nextAddr(x)
	if addr == regAddr {
		return nil
	}
	return getVariable{addr, regAddr}
}

func (cc *clauseCompiler) unifyVar(x logic.Var) Instruction {
	if x == logic.AnonymousVar {
		return unifyVoid{}
	}
	if addr, ok := cc.register[x]; ok {
		return unifyValue{addr}
	}
	return unifyVariable{cc.nextAddr(x)}
}

func (cc *clauseCompiler) putVar(x logic.Var, regAddr RegAddr) Instruction {
	if x == logic.AnonymousVar {
		return putVariable{cc.nextReg(), regAddr}
	}
	addr, ok := cc.register[x]
	if !ok {
		return putVariable{cc.nextAddr(x), regAddr}
	}
	if addr == regAddr {
		return nil
	}
	return putValue{addr, regAddr}
}

// ---- get terms

func (cc *clauseCompiler) getTerm(term logic.Term, addr RegAddr) []Instruction {
	switch t := term.(type) {
	case logic.Atom:
		return []Instruction{getConstant{toConstant(t), addr}}
	case logic.Int:
		return []Instruction{getConstant{toConstant(t), addr}}
	case logic.Var:
		if t == logic.AnonymousVar {
			return []Instruction{}
		}
		instr := cc.getVar(t, addr)
		if instr == nil {
			return []Instruction{}
		}
		return []Instruction{instr}
	case *logic.Comp:
		instrs := make([]Instruction, len(t.Args)+1)
		instrs[0] = getStruct{toFunctor(t.Indicator()), addr}
		for i, arg := range t.Args {
			instrs[i+1] = cc.unifyArg(arg)
		}
		return instrs
	case *logic.List:
		return cc.getPair(ListPair, addr, t.Terms[0], t.Slice(1))
	case *logic.Assoc:
		return cc.getPair(AssocPair, addr, t.Key, t.Val)
	case *logic.Dict:
		return cc.getPair(DictPair, addr, t.Assocs[0], t.Tail())
	default:
		panic(fmt.Sprintf("wam.getTerm: unhandled type %T (%v)", term, term))
	}
}

func (cc *clauseCompiler) getPair(tag PairTag, addr RegAddr, head, tail logic.Term) []Instruction {
	return []Instruction{getPair{tag, addr}, cc.unifyArg(head), cc.unifyArg(tail)}
}

// ---- unify terms

func (cc *clauseCompiler) delayComplexArg(arg logic.Term) Instruction {
	addr := cc.nextReg()
	cc.delayed = append(cc.delayed, compound{t: arg, addr: addr})
	return unifyVariable{addr}
}

func (cc *clauseCompiler) unifyArg(arg logic.Term) Instruction {
	switch a := arg.(type) {
	case logic.Atom:
		return unifyConstant{toConstant(a)}
	case logic.Int:
		return unifyConstant{toConstant(a)}
	case logic.Var:
		return cc.unifyVar(a)
	case *logic.Comp:
		return cc.delayComplexArg(a)
	case *logic.List:
		return cc.delayComplexArg(a)
	case *logic.Assoc:
		return cc.delayComplexArg(a)
	case *logic.Dict:
		return cc.delayComplexArg(a)
	default:
		panic(fmt.Sprintf("wam.unifyArg: unhandled type %T (%v)", arg, arg))
	}
}

// ---- put terms

func (cc *clauseCompiler) putTerm(term logic.Term, addr RegAddr) []Instruction {
	switch t := term.(type) {
	case logic.Atom:
		return []Instruction{putConstant{toConstant(t), addr}}
	case logic.Int:
		return []Instruction{putConstant{toConstant(t), addr}}
	case logic.Var:
		instr := cc.putVar(t, addr)
		if instr == nil {
			return []Instruction{}
		}
		return []Instruction{instr}
	case *logic.Comp:
		instrs := make([]Instruction, len(t.Args)+1)
		instrs[0] = putStruct{toFunctor(t.Indicator()), addr}
		cc.setArgs(t.Args, instrs)
		return instrs
	case *logic.List:
		return cc.putPair(ListPair, addr, t.Terms[0], t.Slice(1))
	case *logic.Assoc:
		return cc.putPair(AssocPair, addr, t.Key, t.Val)
	case *logic.Dict:
		return cc.putPair(DictPair, addr, t.Assocs[0], t.Tail())
	default:
		panic(fmt.Sprintf("wam.putTerm: unhandled type %T (%v)", term, term))
	}
}

func (cc *clauseCompiler) putPair(tag PairTag, addr RegAddr, head, tail logic.Term) []Instruction {
	instrs := []Instruction{putPair{tag, addr}, nil, nil}
	cc.setArgs([]logic.Term{head, tail}, instrs)
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
func (cc *clauseCompiler) setArgs(args []logic.Term, instrs []Instruction) {
	var varIdxs []int
	for i, arg := range args {
		if _, ok := arg.(logic.Var); ok {
			varIdxs = append(varIdxs, i)
		} else {
			instrs[i+1] = cc.setArg(arg)
		}
	}
	for _, idx := range varIdxs {
		instrs[idx+1] = cc.setArg(args[idx])
	}
}

func (cc *clauseCompiler) setArg(arg logic.Term) Instruction {
	switch a := arg.(type) {
	case logic.Atom:
		return unifyConstant{toConstant(a)}
	case logic.Int:
		return unifyConstant{toConstant(a)}
	case logic.Var:
		return cc.unifyVar(a)
	case *logic.Comp:
		return cc.setComplexArg(arg)
	case *logic.List:
		return cc.setComplexArg(arg)
	case *logic.Assoc:
		return cc.setComplexArg(arg)
	case *logic.Dict:
		return cc.setComplexArg(arg)
	default:
		panic(fmt.Sprintf("wam.setArg: unhandled type %T (%v)", arg, arg))
	}
}

func (cc *clauseCompiler) setComplexArg(arg logic.Term) Instruction {
	addr := cc.nextReg()
	cc.instrs = append(cc.instrs, cc.putTerm(arg, addr)...)
	return unifyValue{addr}
}

// ---- term addr

func (cc *clauseCompiler) ensureVar(x logic.Var) {
	if x == logic.AnonymousVar {
		return
	}
	if _, ok := cc.register[x]; ok {
		return
	}
	cc.instrs = append(cc.instrs, putVariable{cc.nextAddr(x), cc.nextReg()})
}

func (cc *clauseCompiler) termAddr(term logic.Term) Addr {
	switch t := term.(type) {
	case logic.Atom:
		return ConstantAddr{toConstant(t)}
	case logic.Int:
		return ConstantAddr{toConstant(t)}
	case logic.Ptr:
		return ConstantAddr{toConstant(t)}
	case logic.Var:
		cc.ensureVar(t)
		return cc.register[t]
	}
	addr := cc.nextReg()
	cc.instrs = append(cc.instrs, cc.putTerm(term, addr)...)
	return addr
}

// ---- compiling terms

// Compile compiles a single clause in isolation. Useful for testing; it's necessary
// to use CompilePackage to make use of clause indexing for clauses with same functor.
func Compile(clause *logic.Clause, options ...CompileOption) (*Clause, error) {
	goals, err := flatten(clause)
	if err != nil {
		return nil, err
	}
	opts := make(map[CompileOption]struct{})
	for _, opt := range options {
		opts[opt] = empty
	}
	return newPackageCompiler(opts).compile(goals)
}

func compileQuery(query []logic.Term) (*Clause, error) {
	dummy := make(flatClause, len(query)+1)
	dummy[0] = goal{"", logic.NewComp("dummy")}
	for i, term := range query {
		var err error
		dummy[i+1], err = toGoal(term)
		if err != nil {
			return nil, errors.New("term #%d: %v", i+1, err)
		}
	}
	// All vars in a query are made permanent, so we can retrieve them at the end
	// for display.
	permVars := make(varset)
	for _, x := range goalVars(dummy) {
		permVars[x] = empty
	}
	c, err := newPackageCompiler(nil).compile0(dummy, nil, permVars)
	if err != nil {
		return nil, err
	}
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
	return c, nil
}

func asmGoal(t logic.Term) goal {
	return goal{"", comp("asm", t)}
}

func (cc *clauseCompiler) flattenControl(terms0 flatClause) (flatClause, error) {
	terms := make(flatClause, len(terms0))
	copy(terms, terms0)
	var body flatClause
	cc.labelID = 1
	for len(terms) > 0 {
		g := terms[0]
		terms = terms[1:]
		if g.pkg != "" {
			body = append(body, g)
			continue
		}
		term := g.comp
		if _, ok := controlFunctors[term.Functor]; ok {
			goals, err := cc.controlFunctor(term)
			if err != nil {
				return nil, err
			}
			terms = append(goals, terms...)
			continue
		}
		if _, ok := controlIndicators[term.Indicator()]; ok {
			goals, err := cc.controlIndicator(term)
			if err != nil {
				return nil, err
			}
			terms = append(goals, terms...)
			continue
		}
		// Default: simply append goal to body.
		body = append(body, g)
	}
	return body, nil
}

func (cc *clauseCompiler) controlFunctor(term *logic.Comp) (flatClause, error) {
	switch term.Functor {
	case "and":
		return toGoals(term.Args)
	default:
		panic(fmt.Sprintf("Unimplemented control functor %s (%v)", term.Functor, term))
	}
}

func (cc *clauseCompiler) controlIndicator(term *logic.Comp) (flatClause, error) {
	switch term.Indicator() {
	case dsl.Indicator("\\+", 1):
		target := term.Args[0]
		targetID, endID := cc.labelID, cc.labelID+1
		cc.labelID += 2
		//
		targetGoal, err := toGoal(target)
		if err != nil {
			return nil, err
		}
		// Ensure that variables referenced within Target are initialized.
		cc.instrs = nil
		for _, x := range logic.Vars(term) {
			cc.ensureVar(x)
		}
		cc.clause.Code = append(cc.clause.Code, cc.instrs...)
		// Inline \+(Target) :- ->(Target, false, true).
		return flatClause{
			asmGoal(comp("try", comp("instr", ptr(cc.clause), int_(-targetID)))),
			asmGoal(comp("trust", comp("instr", ptr(cc.clause), int_(-endID)))),
			asmGoal(comp("label", int_(targetID))),
			targetGoal,
			asmGoal(atom("cut")),
			asmGoal(atom("fail")),
			asmGoal(comp("label", int_(endID))),
		}, nil
	case dsl.Indicator("->", 3):
		cond, then_, else_ := term.Args[0], term.Args[1], term.Args[2]
		thenID, elseID, endID := cc.labelID, cc.labelID+1, cc.labelID+2
		cc.labelID += 3
		//
		condGoal, err1 := toGoal(cond)
		thenGoal, err2 := toGoal(then_)
		elseGoal, err3 := toGoal(else_)
		if !(err1 == nil && err2 == nil && err3 == nil) {
			return nil, errors.New("cond err=%v, then err=%v, else err=%v", err1, err2, err3)
		}
		// Ensure that variables referenced in any branch are initialized.
		cc.instrs = nil
		for _, x := range logic.Vars(term) {
			cc.ensureVar(x)
		}
		cc.clause.Code = append(cc.clause.Code, cc.instrs...)
		// Inline cond, then and else branches, adding control instructions to move around.
		return flatClause{
			asmGoal(comp("try", comp("instr", ptr(cc.clause), int_(-thenID)))),
			asmGoal(comp("trust", comp("instr", ptr(cc.clause), int_(-elseID)))),
			asmGoal(comp("label", int_(thenID))),
			condGoal,
			asmGoal(atom("cut")),
			thenGoal,
			asmGoal(comp("jump", comp("instr", ptr(cc.clause), int_(-endID)))),
			asmGoal(comp("label", int_(elseID))),
			elseGoal,
			asmGoal(comp("label", int_(endID))),
		}, nil
	case dsl.Indicator("phrase", 2):
		return phrase(term.Args[0], term.Args[1], dsl.List())
	case dsl.Indicator("phrase", 3):
		return phrase(term.Args[0], term.Args[1], term.Args[2])
	default:
		panic(fmt.Sprintf("Unimplemented control indicator %v (%v)", term.Indicator(), term))
	}
}

func (cc *clauseCompiler) compileBodyTerm(pos int, g goal) ([]Instruction, error) {
	cc.instrs = nil
	if g.pkg != "" {
		return cc.compileDefaultTerm(g)
	}
	term := g.comp
	if _, ok := inlinedFunctors[term.Functor]; ok {
		return cc.inlinedFunctor(pos, term)
	}
	if _, ok := inlinedIndicators[term.Indicator()]; ok {
		return cc.inlinedIndicator(pos, term)
	}
	return cc.compileDefaultTerm(g)
}

func (cc *clauseCompiler) inlinedFunctor(pos int, term *logic.Comp) ([]Instruction, error) {
	switch term.Functor {
	case "call":
		callee := cc.termAddr(term.Args[0])
		params := make([]Addr, len(term.Args)-1)
		for i, param := range term.Args[1:] {
			params[i] = cc.termAddr(param)
		}
		cc.instrs = append(cc.instrs, callMeta{Addr: callee, Params: params})
		return cc.instrs, nil
	default:
		panic(fmt.Sprintf("unimplemented inlined functor %v", term.Functor))
	}
}

func (cc *clauseCompiler) inlinedIndicator(pos int, term *logic.Comp) ([]Instruction, error) {
	switch term.Indicator() {
	case dsl.Indicator("!", 0):
		if pos == 0 {
			return []Instruction{neckCut{}}, nil
		}
		return []Instruction{cut{}}, nil
	case dsl.Indicator("true", 0):
		return []Instruction{}, nil
	case dsl.Indicator("fail", 0), dsl.Indicator("false", 0):
		return []Instruction{fail{}}, nil
	case dsl.Indicator("asm", 1):
		return []Instruction{DecodeInstruction(term.Args[0])}, nil
	case dsl.Indicator("import", 1):
		pkg, ok := term.Args[0].(logic.Atom)
		if !ok {
			return nil, errors.New("expected atom for import/1, got %v", term.Args[0])
		}
		return []Instruction{importPkg{pkg.Name}}, nil
	case dsl.Indicator("=", 2):
		x := cc.termAddr(term.Args[0])
		y := cc.termAddr(term.Args[1])
		cc.instrs = append(cc.instrs, inlineUnify{x, y})
		return cc.instrs, nil
	case dsl.Indicator("@<", 2),
		dsl.Indicator("@=<", 2),
		dsl.Indicator("@>=", 2),
		dsl.Indicator("@>", 2),
		dsl.Indicator("==", 2),
		dsl.Indicator("\\==", 2):
		x := cc.termAddr(term.Args[0])
		y := cc.termAddr(term.Args[1])
		pred := comparisonPredicates[term.Functor]
		cc.instrs = append(cc.instrs, builtinComparisonInstruction(pred, x, y))
		return cc.instrs, nil
	case dsl.Indicator("atom", 1),
		dsl.Indicator("int", 1),
		dsl.Indicator("ptr", 1),
		dsl.Indicator("var", 1),
		dsl.Indicator("list", 1),
		dsl.Indicator("assoc", 1),
		dsl.Indicator("dict", 1):
		x := cc.termAddr(term.Args[0])
		pred := typeCheckPredicates[term.Functor]
		cc.instrs = append(cc.instrs, builtinTypeCheckInstruction(pred, x))
		return cc.instrs, nil
	case dsl.Indicator("get_attr", 3):
		pkg, ok := term.Args[0].(logic.Atom)
		if !ok {
			return nil, errors.New("expected atom for get_attr/3, got %v", term.Args[0])
		}
		x := cc.termAddr(term.Args[1])
		attr := cc.termAddr(term.Args[2])
		cc.instrs = append(cc.instrs, getAttr{pkg.Name, x, attr})
		return cc.instrs, nil
	case dsl.Indicator("put_attr", 3):
		pkg, ok := term.Args[0].(logic.Atom)
		if !ok {
			return nil, errors.New("expected atom for put_attr/3, got %v", term.Args[0])
		}
		x := cc.termAddr(term.Args[1])
		attr := cc.termAddr(term.Args[2])
		cc.instrs = append(cc.instrs, putAttr{pkg.Name, x, attr})
		return cc.instrs, nil
	case dsl.Indicator("del_attr", 2):
		pkg, ok := term.Args[0].(logic.Atom)
		if !ok {
			return nil, errors.New("expected atom for del_attr/2, got %v", term.Args[0])
		}
		x := cc.termAddr(term.Args[1])
		cc.instrs = append(cc.instrs, delAttr{pkg.Name, x})
		return cc.instrs, nil
	default:
		panic(fmt.Sprintf("unimplemented inlined indicator %v", term.Indicator()))
	}
}

func (cc *clauseCompiler) compileDefaultTerm(g goal) ([]Instruction, error) {
	// Put term args into registers X0-Xn and issue a call to f/n.
	for i, arg := range g.comp.Args {
		cc.instrs = append(cc.instrs, cc.putTerm(arg, RegAddr(i))...)
	}
	var pkg Addr
	if g.pkg != "" {
		pkg = ConstantAddr{WAtom(g.pkg)}
	}
	cc.instrs = append(cc.instrs, call{
		Pkg:     pkg,
		Functor: toFunctor(g.comp.Indicator()),
	})
	return cc.instrs, nil
}

// ---- indexing

// Create level-1 index of clauses, based on their first arg.
func (pc *packageCompiler) compileClauseGroup(ind logic.Indicator, clauses []flatClause) (*Clause, error) {
	if len(clauses) == 1 {
		return pc.compile(clauses[0])
	}
	if ind.Arity == 0 {
		// No first arg, impossible to index.
		return pc.compileSequenceNoIndex(clauses)
	}
	seqs, anyNonVar := splitOnVarFirstArg(clauses)
	if !anyNonVar {
		// All first arg are vars, impossible to index.
		return pc.compileSequenceNoIndex(clauses)
	}
	var numReg int
	compiledClauses := make([]*Clause, len(seqs))
	for i, seq := range seqs {
		var err error
		compiledClauses[i], err = pc.compileSequence(ind, seq)
		if err != nil {
			return nil, err
		}
		if compiledClauses[i].NumRegisters > numReg {
			numReg = compiledClauses[i].NumRegisters
		}
	}
	addChoiceLinks(compiledClauses)
	return &Clause{
		Functor:      toFunctor(ind),
		NumRegisters: numReg,
		Code:         compiledClauses[0].Code,
	}, nil
}

// Compile a subsequence of clauses with non-var first argument.
//
// Clauses are indexed on whether their first argument is a constant,
// functor or list, accelerating matching during unification.
//
// Clauses with same first arg are also placed in a linked-list of
// try-retry-trust instructions.
func (pc *packageCompiler) compileSequence(ind logic.Indicator, clauses []flatClause) (*Clause, error) {
	var numReg int
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		var err error
		codes[i], err = pc.compile(clause)
		if err != nil {
			return nil, errors.New("%v#%d: %v", ind, i+1, err)
		}
		if codes[i].NumRegisters > numReg {
			numReg = codes[i].NumRegisters
		}
	}
	addChoiceLinks(codes)
	if len(codes) == 1 {
		// TODO: maybe index this one, if it's a nonvar?
		return codes[0], nil
	}
	// Group clauses by type and index key.
	constIndex := make(map[Constant][]InstrAddr)
	structIndex := make(map[Functor][]InstrAddr)
	var listIndex, assocIndex, dictIndex []InstrAddr
	for i, clause := range clauses {
		arg := clause[0].comp.Args[0]
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
	return indexClause, nil
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
		arg := clause[0].comp.Args[0]
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

func (pc *packageCompiler) compileSequenceNoIndex(clauses []flatClause) (*Clause, error) {
	// Compile each clause in isolation.
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		var err error
		codes[i], err = pc.compile(clause)
		if err != nil {
			return nil, err
		}
	}
	addChoiceLinks(codes)
	return codes[0], nil
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

func (pc *packageCompiler) compile(clause flatClause) (*Clause, error) {
	permVars, chunks, err := permanentVars(clause)
	if err != nil {
		return nil, err
	}
	c, err := pc.compile0(clause, chunks, permVars)
	if err != nil {
		return nil, err
	}
	c.Code = optimizeLastCall(c.Code)
	return c, nil
}

func (pc *packageCompiler) compile0(clause flatClause, chunks []flatClause, permVars varset) (*Clause, error) {
	functor := toFunctor(clause[0].comp.Indicator())
	c := &Clause{Functor: functor}
	cc := newClauseCompiler(pc, c, numArgs(clause), permVars, tempAllocationSets(chunks, permVars))
	// Compile clause head
	for i, term := range clause[0].comp.Args {
		c.Code = append(c.Code, cc.getTerm(term, RegAddr(i))...)
	}
	for len(cc.delayed) > 0 {
		buf := cc.delayed
		cc.delayed = nil
		for _, compound := range buf {
			c.Code = append(c.Code, cc.getTerm(compound.t, compound.addr)...)
		}
	}
	// Compile clause body
	body, err := cc.flattenControl(clause[1:])
	if err != nil {
		return nil, err
	}
	for i, term := range body {
		instrs, err := cc.compileBodyTerm(i, term)
		if err != nil {
			return nil, err
		}
		c.Code = append(c.Code, instrs...)
	}
	// Add "proceed" instruction for facts and when a body doesn't end with a call.
	if requiresProceed(c.Code) {
		c.Code = append(c.Code, proceed{Run})
	}
	// If call requires an environment, add an allocate-deallocate pair to the clause.
	if cc.topStack > 0 || requiresEnv(c.Code) {
		c.Code = append([]Instruction{allocate{int(cc.topStack)}}, c.Code...)
		c.Code = append(c.Code, deallocate{})
	}
	c.NumRegisters = int(cc.topReg)
	c.Vars = cc.content
	return c, nil
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
			code[n-1] = execute{Pkg: call.Pkg, Functor: call.Functor}
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
			pending[len(buf)] = empty
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

// ----

func reachableClauses(clauses ...*Clause) []*Clause {
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
		seen[clause] = empty
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

// ----

// Option to control compilation.
type CompileOption interface {
	isCompileOption()
}

// Compile option to keep labels in control instructions. Without it, replace label
// references to a concrete position in code.
type KeepLabels struct{}

// Compile option to use conflict-avoidance allocation strategy for allocating registers
// in clauses. Without this, we use a very conservative approach where registers are
// reserved for parameter passing, and even temporary variables are allocated to registers
// outside those used for parameters.
type UseConflictAvoidanceAllocationStrategy struct{}

func (KeepLabels) isCompileOption()                             {}
func (UseConflictAvoidanceAllocationStrategy) isCompileOption() {}

type packageCompiler struct {
	opts map[CompileOption]struct{}
}

func newPackageCompiler(opts map[CompileOption]struct{}) *packageCompiler {
	pc := new(packageCompiler)
	pc.opts = opts
	return pc
}

// compileClauses returns a list of compiled clauses. Each corresponds with
// a functor f/n, and all sub-clauses that implement the same functor.
func compileClauses(clauses []*logic.Clause, options ...CompileOption) ([]*Clause, error) {
	opts := make(map[CompileOption]struct{})
	for _, opt := range options {
		opts[opt] = empty
	}
	// Group clauses by functor and compile each group.
	m := make(map[logic.Indicator][]flatClause)
	var order []logic.Indicator
	for i, clause := range clauses {
		goals, err := flatten(clause)
		if err != nil {
			return nil, errors.New("clause #%d: %v", i+1, err)
		}
		ind := goals[0].comp.Indicator()
		if _, ok := m[ind]; !ok {
			order = append(order, ind)
		}
		m[ind] = append(m[ind], goals)
	}
	pc := newPackageCompiler(opts)
	var cs []*Clause
	for _, ind := range order {
		clause, err := pc.compileClauseGroup(ind, m[ind])
		if err != nil {
			return nil, err
		}
		cs = append(cs, clause)
	}
	// Remove labels from clause bodies.
	if _, ok := opts[KeepLabels{}]; !ok {
		for _, clause := range reachableClauses(cs...) {
			clause.Code = optimizeLabels(clause.Code)
		}
	}
	return cs, nil
}

// CompilePackage compiles a list of rules into a single package.
//
// If the first clause is a fact like `package(pkg_name, [pkg1, pkg2], ['f/2', 'g/1']).`,
// the package returned defines a namespace, with specified exported functions and imported
// package names.
//
// Otherwise, the package is nameless, and all clauses will be added to the global namespace.
func CompilePackage(rules []logic.Rule, options ...CompileOption) (*Package, error) {
	if len(rules) == 0 {
		return NewPackage(""), nil
	}
	clauses := make([]*logic.Clause, len(rules))
	for i, rule := range rules {
		clauses[i] = rule.ToClause()
	}
	pkgClause := clauses[0]
	head, ok := pkgClause.Head.(*logic.Comp)
	if !(ok && head.Indicator() == dsl.Indicator("package", 3) && len(pkgClause.Body) == 0) {
		// Global package
		pkg := NewPackage("")
		compiled, err := compileClauses(clauses, options...)
		if err != nil {
			return nil, err
		}
		for _, clause := range compiled {
			pkg.AddExported(clause)
		}
		return pkg, nil
	}
	pkgName, ok := head.Args[0].(logic.Atom)
	if !ok {
		return nil, errors.New("package arg #1 is not atom")
	}
	var imported []logic.Term
	if head.Args[1] != logic.EmptyList {
		list, ok := head.Args[1].(*logic.List)
		if !ok {
			return nil, errors.New("package arg #2 is not list")
		}
		imported = list.Terms
	}
	var exported []logic.Term
	if head.Args[2] != logic.EmptyList {
		list, ok := head.Args[2].(*logic.List)
		if !ok {
			return nil, errors.New("package arg #3 is not list")
		}
		exported = list.Terms
	}
	pkg := NewPackage(pkgName.Name)
	for i, importedPkg := range imported {
		pkgName, ok := importedPkg.(logic.Atom)
		if !ok {
			return nil, errors.New("imported #%d: expecting atom package name, got %v", i+1, importedPkg)
		}
		pkg.AddImported(pkgName.Name)
	}
	exportedFunctors := make(map[Functor]struct{})
	for i, exportedFn := range exported {
		fnAtom, ok := exportedFn.(logic.Atom)
		if !ok {
			return nil, errors.New("exported #%d: expecting atom functor name, got %v", i+1, exportedFn)
		}
		fn, err := ParseFunctor(fnAtom.Name)
		if err != nil {
			return nil, errors.New("exported #%d: %v", i+1, err)
		}
		exportedFunctors[fn] = empty
	}
	compiled, err := compileClauses(clauses[1:], options...)
	if err != nil {
		return nil, err
	}
	for _, clause := range compiled {
		if _, ok := exportedFunctors[clause.Functor]; ok {
			if err := pkg.AddExported(clause); err != nil {
				return nil, errors.New("exported: %v", err)
			}
			delete(exportedFunctors, clause.Functor)
		} else {
			if err := pkg.AddInternal(clause); err != nil {
				return nil, errors.New("internal: %v", err)
			}
		}
	}
	if len(exportedFunctors) > 0 {
		return nil, errors.New("exported functors are not implemented: %v", exportedFunctors)
	}
	return pkg, nil
}
