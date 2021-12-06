package wam

import (
	"fmt"
	"sort"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/errors"
	"github.com/brunokim/logic-engine/logic"
)

// ---- Package compiler

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
type goalList []goal

func flatten(clause *logic.Clause) (goalList, error) {
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

func toGoals(terms []logic.Term) (goalList, error) {
	goals := make(goalList, len(terms))
	for i, term := range terms {
		var err error
		goals[i], err = toGoal(term)
		if err != nil {
			return nil, errors.New("goal #%d: %v", i+1, err)
		}
	}
	return goals, nil
}

func goalVars(goals goalList) []logic.Var {
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

func phrase(dcg logic.Term, list, rest logic.Term) (goalList, error) {
	dcgGoal, err := toGoal(dcg)
	if err != nil {
		return nil, err
	}
	comp := logic.DCGExpandComp(dcgGoal.comp, list, rest)
	return goalList{goal{dcgGoal.pkg, comp}}, nil
}

// ---- permanent vars

// Compute the permanent vars of a logic clause.
//
// A permanent var is a local var in a call that is referenced in more
// than one chunk. They must be stored in the environment stack, or
// otherwise they may be overwritten if stored in a register, since a
// body term may use them in any ways.
func permanentVars(clause goalList) (varset, []goalList, error) {
	// Facts don't have permanent vars.
	if len(clause) < 2 {
		return varset{}, []goalList{clause}, nil
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

func chunkVars(chunk goalList) varset {
	vars := make(varset)
	for _, goal := range chunk {
		for _, x := range logic.Vars(goal.comp) {
			vars[x] = empty
		}
	}
	return vars
}

// Group terms into chunks, consisting of a sequence of inline terms ended by a predicate call.
func getChunks(clause goalList) ([]goalList, error) {
	var chunks []goalList
	chunk := goalList{clause[0]}
	goals := make(goalList, len(clause)-1)
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
		chunk = goalList{}
	}
	if len(chunk) > 0 {
		chunks = append(chunks, chunk)
	}
	return chunks, nil
}

func controlGoals(term *logic.Comp) (goalList, error) {
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
		return goalList{cond, then_, goal{}, else_, goal{}}, nil
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

func chunkAllocationSets(pos int, chunk goalList, temps varset) map[logic.Var]*registerAllocation {
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
func tempAllocationSets(chunks []goalList, permVars varset) map[logic.Var]*registerAllocation {
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
	return newPackageCompiler(opts).compileProgram(goals)
}

func compileQuery(query []logic.Term) (*Clause, error) {
	dummy := make(goalList, len(query)+1)
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
	c, err := newPackageCompiler(nil).compile(dummy, nil, permVars)
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

// ---- indexing

// Create level-1 index of clauses, based on their first arg.
func (pc *packageCompiler) compilePredicate(ind logic.Indicator, clauses []goalList) (*Clause, error) {
	if len(clauses) == 1 {
		return pc.compileProgram(clauses[0])
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
func (pc *packageCompiler) compileSequence(ind logic.Indicator, clauses []goalList) (*Clause, error) {
	var numReg int
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		var err error
		codes[i], err = pc.compileProgram(clause)
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
func splitOnVarFirstArg(clauses []goalList) ([][]goalList, bool) {
	var buf []goalList
	var subSeqs [][]goalList
	var anyNonVar bool
	for _, clause := range clauses {
		arg := clause[0].comp.Args[0]
		if _, ok := arg.(logic.Var); ok {
			if buf != nil {
				subSeqs = append(subSeqs, buf)
				buf = nil
			}
			subSeqs = append(subSeqs, []goalList{clause})
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

func (pc *packageCompiler) compileSequenceNoIndex(clauses []goalList) (*Clause, error) {
	// Compile each clause in isolation.
	codes := make([]*Clause, len(clauses))
	for i, clause := range clauses {
		var err error
		codes[i], err = pc.compileProgram(clause)
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

func (pc *packageCompiler) compileProgram(clause goalList) (*Clause, error) {
	permVars, chunks, err := permanentVars(clause)
	if err != nil {
		return nil, err
	}
	c, err := pc.compile(clause, chunks, permVars)
	if err != nil {
		return nil, err
	}
	c.Code = optimizeLastCall(c.Code)
	return c, nil
}

func (pc *packageCompiler) compile(clause goalList, chunks []goalList, permVars varset) (*Clause, error) {
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
		instrs, err := cc.compileGoal(i, term)
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

// compileClauses returns a list of compiled clauses. Each corresponds with
// a functor f/n, and all sub-clauses that implement the same functor.
func compileClauses(clauses []*logic.Clause, options ...CompileOption) ([]*Clause, error) {
	opts := make(map[CompileOption]struct{})
	for _, opt := range options {
		opts[opt] = empty
	}
	// Group clauses by functor to form predicates.
	m := make(map[logic.Indicator][]goalList)
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
		clause, err := pc.compilePredicate(ind, m[ind])
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
