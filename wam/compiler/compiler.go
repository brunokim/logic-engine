package compiler

import (
	"fmt"
	"sort"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

// regset is a set of register addresses, implemented as a sorted array.
type regset []wam.RegAddr

func (r regset) index(reg wam.RegAddr) (int, bool) {
	i := sort.Search(len(r), func(i int) bool { return r[i] >= reg })
	return i, i < len(r) && r[i] == reg
}

func (r regset) has(reg wam.RegAddr) bool {
	_, ok := r.index(reg)
	return ok
}

func (r regset) add(reg wam.RegAddr) regset {
	i, ok := r.index(reg)
	if ok {
		return r
	}
	r = append(r, -1)
	copy(r[i+1:], r[i:])
	r[i] = reg
	return r
}

func (r regset) remove(reg wam.RegAddr) regset {
	i, ok := r.index(reg)
	if !ok {
		return r
	}
	copy(r[i:], r[i+1:])
	r = r[:len(r)-1]
	return r
}

func (r regset) union(s regset) regset {
	if len(r) < len(s) {
		// Let r be the largest of the sets
		r, s = s, r
	}
	t := make(regset, len(r), len(r)+len(s))
	copy(t, r)
	for _, x := range s {
		t = t.add(x)
	}
	return t
}

func (r regset) difference(s regset) regset {
	if len(r) < len(s) {
		// Let r be the largest of the sets
		r, s = s, r
	}
	t := make(regset, len(r))
	copy(t, r)
	for _, x := range s {
		t = t.remove(x)
	}
	return t
}

// ----

var builtins = map[logic.Indicator]struct{}{
	logic.Indicator{"=", 2}: struct{}{},
}

// ----

type Code struct {
	Functor      wam.Functor
	Instructions []wam.Instruction
	NumRegs      int
}

func newCode(functor wam.Functor, instructions []wam.Instruction) *Code {
	return nil
}

type Index struct {
	IsVar    bool
	ByVar    []*Code
	ByAtom   map[logic.Atom][]*Code
	ByStruct map[logic.Indicator][]*Code
}

func compileCode(functor wam.Functor, clause *logic.Clause) map[wam.Functor][]*Index {
	return nil
}

func indexClauses(clauses []*logic.Clause) map[wam.Functor][]*Index {
	return nil
}

type PackageCompiler struct {
	clauses []*logic.Clause
}

func NewPackageCompiler(clauses []*logic.Clause) *PackageCompiler {
	return &PackageCompiler{clauses}
}

func (pc *PackageCompiler) Compile() map[wam.Functor][]*Index {
	return nil
}

type goal struct {
	pkg  string
	comp *logic.Comp
}

type chunk struct {
	goals []goal
}

func (chunk *chunk) vars() []logic.Var {
	return nil
}

func chunks(clause *logic.Clause) []*chunk {
	return nil
}

type chunksInfo struct {
	temps      []logic.Var
	permanents []logic.Var
	chunks     []*chunk
}

func newChunksInfo(clause *logic.Clause) *chunksInfo {
	return nil
}

func countNestedComplexTerms(chunk *chunk) int {
	return 0
}

// Address allocation result.
type addrAlloc int

const (
	existingTerm addrAlloc = iota
	newVariable
	newComplexTerm
)

// Analysis of temporary variables' locations to compute best registers
// to allocate.
type allocSets struct {
	maxRegs  int
	use      map[logic.Var]regset
	noUse    map[logic.Var]regset
	conflict map[logic.Var]regset
}

func newAllocSets(chunk *chunk, temps []logic.Var, isHead bool) *allocSets {
	return nil
}

// Compiler for a single predicate clause.
type clauseCompiler struct {
	clause         *logic.Clause
	chunksInfo     *chunksInfo
	permanents     map[logic.Var]struct{}
	permanentAddrs map[logic.Var]wam.StackAddr
	tempAddrs      map[logic.Term]wam.RegAddr
}

func newClauseCompiler(clause *logic.Clause) *clauseCompiler {
	info := newChunksInfo(clause)
	perms := make(map[logic.Var]struct{})
	for _, x := range info.permanents {
		perms[x] = struct{}{}
	}
	return &clauseCompiler{
		clause:     clause,
		permanents: perms,
		chunksInfo: info,
	}
}

func (cc *clauseCompiler) compile() []wam.Instruction {
	cc.permanentAddrs = make(map[logic.Var]wam.StackAddr)
	cc.tempAddrs = make(map[logic.Term]wam.RegAddr)

	var instrs []wam.Instruction
	for i, chunk := range cc.chunksInfo.chunks {
		chunkCompiler := newChunkCompiler(chunk, i == 0, cc)
		instrs = append(instrs, chunkCompiler.compile()...)
		for x, addr := range chunkCompiler.tempAddrs {
			cc.tempAddrs[x] = addr
		}
	}
	return instrs
}

func (cc *clauseCompiler) isPermanent(x logic.Var) bool {
	_, ok := cc.permanents[x]
	return ok
}

func (cc *clauseCompiler) permanentAddr(x logic.Var) (wam.StackAddr, addrAlloc) {
	if !cc.isPermanent(x) {
		panic(fmt.Sprintf("%v is not a permament variable (%v)", x, cc.chunksInfo.permanents))
	}
	if addr, ok := cc.permanentAddrs[x]; ok {
		return addr, existingTerm
	}
	index := len(cc.permanentAddrs)
	addr := wam.StackAddr(index)
	cc.permanentAddrs[x] = addr
	return addr, newVariable
}

// Tuple relating a complex term to its allocated register.
type delayedComplexTerm struct {
	term logic.Term
	reg  wam.RegAddr
}

// Compiler for a clause chunk.
type chunkCompiler struct {
	chunk  *chunk
	isHead bool
	parent *clauseCompiler

	allocSets *allocSets

	delayedComplexTerms []delayedComplexTerm

	freeRegs   regset
	tempAddrs  map[logic.Term]wam.RegAddr
	regContent map[wam.RegAddr]logic.Term
}

func newChunkCompiler(chunk *chunk, isHead bool, clauseCompiler *clauseCompiler) *chunkCompiler {
	return &chunkCompiler{
		chunk:     chunk,
		isHead:    isHead,
		parent:    clauseCompiler,
		allocSets: newAllocSets(chunk, clauseCompiler.chunksInfo.temps, isHead),
	}
}

func (cc *chunkCompiler) setReg(reg wam.RegAddr, term logic.Term) {
	cc.tempAddrs[term] = reg
	cc.regContent[reg] = term
}

func (cc *chunkCompiler) unsetReg(reg wam.RegAddr, term logic.Term) {
	delete(cc.tempAddrs, term)
	delete(cc.regContent, reg)
}

func (cc *chunkCompiler) compile() []wam.Instruction {
	cc.freeRegs = make(regset, cc.allocSets.maxRegs)
	for i := 0; i < cc.allocSets.maxRegs; i++ {
		cc.freeRegs[i] = wam.RegAddr(i)
	}
	cc.tempAddrs = make(map[logic.Term]wam.RegAddr)
	cc.regContent = make(map[wam.RegAddr]logic.Term)
	goals := cc.chunk.goals

	var instrs []wam.Instruction
	// If this is the clause head, compile first chunk to issue get instructions.
	if cc.isHead {
		var firstGoal goal
		firstGoal, goals = goals[0], goals[1:]
		for i := 0; i < len(firstGoal.comp.Args); i++ {
			cc.freeRegs = cc.freeRegs.remove(wam.RegAddr(i))
		}
		instrs = append(instrs, cc.compileHead(firstGoal.comp)...)
	}
	// Early return if clause is a fact.
	if len(goals) == 0 {
		return instrs
	}
	// Compile intermediate, builtin goals.
	// TODO: free registers from temp variables that are last referenced
	// in builtins before the last goal.
	// E.g., in "p :- q(X, Y), X > 0, f(Y)", the register storing X
	// should be free after "X > 0".
	lastGoal := goals[len(goals)-1]
	_, isLastBuiltin := builtins[lastGoal.comp.Indicator()]
	builtinGoals := goals
	if !isLastBuiltin {
		builtinGoals = goals[:len(goals)-1]
	}
	for _, goal := range builtinGoals {
		name := goal.comp.Functor
		var addrs []wam.Addr
		for _, arg := range goal.comp.Args {
			addr, alloc := cc.termAddr(arg)
			if alloc == newComplexTerm {
				instrs = append(instrs, cc.putTerm(arg, addr.(wam.RegAddr), false /*isTopLevel*/)...)
			}
			addrs = append(addrs, addr)
		}
		instrs = append(instrs /*builtin{name, addrs}*/)
		_ = name
	}
	// Issue put instructions and predicate call for non-builtin goal.
	if !isLastBuiltin {
		for i, arg := range lastGoal.comp.Args {
			instrs = append(instrs, cc.putTerm(arg, wam.RegAddr(i), true /*isTopLevel*/)...)
			cc.freeRegs.remove(wam.RegAddr(i))
		}
		instrs = append(instrs /*call{lastGoal.pkg, toFunctor(lastGoal.comp.Indicator())}*/)
	}
	return instrs
}

// Compile head of clause.
//
// Return get instruction for args, delaying complex terms after atoms and vars.
// If there is a nested complex term, it will be added to the delayed list as well.
func (cc *chunkCompiler) compileHead(head *logic.Comp) []wam.Instruction {
	var instrs []wam.Instruction
	cc.delayedComplexTerms = nil
	for i, arg := range head.Args {
		instrs = append(instrs, cc.getTerm(arg, wam.RegAddr(i))...)
	}
	for len(cc.delayedComplexTerms) > 0 {
		delayed := cc.delayedComplexTerms
		cc.delayedComplexTerms = nil
		for _, t := range delayed {
			instrs = append(instrs, cc.getTerm(t.term, t.reg)...)
		}
	}
	return instrs
}

// Issue get instruction for term.
func (cc *chunkCompiler) getTerm(term logic.Term, reg wam.RegAddr) []wam.Instruction {
	cat := category(term)
	switch cat {
	case atomic:
		cc.freeRegs = cc.freeRegs.add(reg)
		return []wam.Instruction{ /*getConstant{toConstant(term), reg}*/ }
	case variable:
		x := term.(logic.Var)
		if _, ok := cc.tempAddrs[x]; !ok {
			cc.setReg(reg, x)
		}
		addr, alloc := cc.varAddr(x, false /*isHead*/)
		if addr == reg {
			// Filter no-op get instructions that wouldn't move values around.
			return nil
		}
		cc.freeRegs = cc.freeRegs.add(reg)
		if alloc == existingTerm {
			return []wam.Instruction{ /*getValue{addr, reg}*/ }
		}
		return []wam.Instruction{ /*getVariable{addr, reg}*/ }
	case complexTerm:
		var instrs []wam.Instruction
		if t, ok := term.(*logic.Comp); ok {
			_ = t
			instrs = append(instrs /*getStruct{t.functor(), reg}*/)
		} else {
			pairType := pairTag(term)
			_ = pairType
			instrs = append(instrs /*getPair{pairType, reg}*/)
		}
		cc.freeRegs = cc.freeRegs.add(reg)
		for _, arg := range complexArgs(term) {
			instrs = append(instrs, cc.unifyArg(arg)...)
		}
		return instrs
	default:
		panic(fmt.Sprintf("unifyArg: unexpected term category %T (%v)", cat, term))
	}
}

// Issue unify instruction for struct arg.
func (cc *chunkCompiler) unifyArg(term logic.Term) []wam.Instruction {
	cat := category(term)
	switch cat {
	case atomic:
		return []wam.Instruction{ /*unifyConstant{toConstant(term)}*/ }
	case variable:
		addr, alloc := cc.varAddr(term.(logic.Var), false /*isHead*/)
		_ = addr
		if alloc == existingTerm {
			return []wam.Instruction{ /*unifyValue{addr}*/ }
		}
		return []wam.Instruction{ /*unifyVariable{addr}*/ }
	case complexTerm:
		addr, _ := cc.tempAddr(term, false /*isHead*/)
		cc.delayedComplexTerms = append(cc.delayedComplexTerms, delayedComplexTerm{term, addr})
		return []wam.Instruction{ /*unifyVariable{addr}*/ }
	default:
		panic(fmt.Sprintf("unifyArg: unexpected term category %T (%v)", cat, term))
	}
}

// Data structure to convert putTerm into an iterative implementation.
// As complex terms are discovered, their components are enqueued to
// be put ahead of its parent in the first round. The parent itself is
// enqueued as well, and will be put in the second round.
type putStep struct {
	term       logic.Term
	isTopLevel bool
	reg        wam.RegAddr
	hasPutArgs bool
}

// Conflict resolution: move content out of register if in
// conflict with an argument.
func (cc *chunkCompiler) conflictResolution(step putStep) []wam.Instruction {
	// Conflicts may only arise when putting args for calls, in
	// the top level, not when we're putting nested args.
	if !step.isTopLevel {
		return nil
	}
	value, ok := cc.regContent[step.reg]
	// Register is empty or term is already present.
	if !ok || value == step.term {
		return nil
	}
	// Atomic terms may be overwritten.
	if category(step.term) == atomic {
		return nil
	}
	// Permanent variables may be overwritten.
	if x, ok := step.term.(logic.Var); ok && cc.parent.isPermanent(x) {
		return nil
	}
	cc.unsetReg(step.reg, value)
	addr, _ := cc.tempAddr(value, false /*isHead*/)
	if addr != step.reg {
		return []wam.Instruction{ /*getVariable{addr, step.reg}*/ }
	}
	return nil
}

// Issue put instruction for term in register.
//
// Debray's allocation method lets variable as long as possible in
// the register.
// If we were to put a term in the same register, we need to move the
// existing variable elsewhere.
func (cc *chunkCompiler) putTerm(term logic.Term, reg wam.RegAddr, isTopLevel bool) []wam.Instruction {
	var instrs []wam.Instruction
	addInstrs := func(ins ...wam.Instruction) {
		instrs = append(instrs, ins...)
	}
	steps := []putStep{{term, isTopLevel, reg, false}}
	for len(steps) > 0 {
		var step putStep
		step, steps = steps[0], steps[1:]
		addInstrs(cc.conflictResolution(step)...)
		cat := category(step.term)
		switch cat {
		case atomic:
			addInstrs( /*putConstant{toConstant(step.term), step.reg}*/ )
		case variable:
			addr, alloc := cc.varAddr(step.term.(logic.Var), false)
			if alloc == existingTerm {
				if addr == step.reg {
					// Filter no-op putValue instruction that wouldn't
					// move value around.
					continue
				}
				addInstrs( /*putValue{addr, step.reg}*/ )
			} else {
				addInstrs( /*putVariable{addr, step.reg}*/ )
			}
			if regAddr, ok := addr.(wam.RegAddr); ok {
				cc.freeRegs = cc.freeRegs.add(regAddr)
			}
		case complexTerm:
			if !step.hasPutArgs {
				// Reserve reg for put_struct instruction in the second round
				cc.freeRegs = cc.freeRegs.remove(step.reg)
				var nextSteps []putStep
				for _, arg := range complexArgs(step.term) {
					if category(arg) != complexTerm {
						continue
					}
					addr, alloc := cc.tempAddr(arg, false /*isHead*/)
					if alloc == newComplexTerm {
						nextSteps = append(nextSteps, putStep{arg, false, addr, false})
					}
				}
				nextSteps = append(nextSteps, putStep{step.term, step.isTopLevel, step.reg, true /*hasPutArgs*/})
				steps = append(nextSteps, steps...)
			} else {
				if t, ok := step.term.(*logic.Comp); ok {
					addInstrs( /*putStruct{t.functor(), step.reg}*/ )
					_ = t
				} else {
					pairType := pairTag(step.term)
					addInstrs( /*putPair{pairType, step.reg}*/ )
					_ = pairType
				}
				for _, arg := range complexArgs(step.term) {
					if category(arg) == complexTerm {
						addInstrs( /*unifyValue{cc.tempAddrs[arg]}*/ )
					} else {
						addInstrs(cc.unifyArg(arg)...)
					}
				}
			}
		default:
			panic(fmt.Sprintf("putTerm: unexpected term category %T (%v)", cat, step.term))
		}
	}
	return instrs
}

// Return address for a term.
func (cc *chunkCompiler) termAddr(term logic.Term) (wam.Addr, addrAlloc) {
	isHead := false
	cat := category(term)
	switch cat {
	case atomic:
		return wam.ConstantAddr{toConstant(term)}, existingTerm
	case variable:
		return cc.varAddr(term.(logic.Var), isHead)
	case complexTerm:
		return cc.tempAddr(term, isHead)
	default:
		panic(fmt.Sprintf("termAddr: unexpected term category %T (%v)", cat, term))
	}
}

// Return address for a variable.
func (cc *chunkCompiler) varAddr(x logic.Var, isHead bool) (wam.Addr, addrAlloc) {
	if cc.parent.isPermanent(x) {
		return cc.parent.permanentAddr(x)
	}
	return cc.tempAddr(x, isHead)
}

// Return register for a variable or complex term.
//
// Debray's allocation algorithm seeks to put term in registers used
// by the variable (either in first or last goal of chunk),
// and avoid registers used by other variables (NOUSE set) or any
// term in the last goal (CONFLICT set).
func (cc *chunkCompiler) tempAddr(term logic.Term, isHead bool) (wam.RegAddr, addrAlloc) {
	if reg, ok := cc.tempAddrs[term]; ok {
		return reg, existingTerm
	}
	var use, noUse regset
	var alloc addrAlloc
	if x, ok := term.(logic.Var); !ok {
		alloc = newComplexTerm
	} else {
		alloc = newVariable
		use, noUse = cc.allocSets.use[x], cc.allocSets.noUse[x]
		if !isHead {
			// Conflict avoidance: do not use registers that are
			// necessary for last goal args.
			noUse = noUse.union(cc.allocSets.conflict[x])
		}
	}
	addr := cc.allocReg(use, noUse)
	cc.setReg(addr, term)
	return addr, alloc
}

// Allocate a register for a variable or complex term.
func (cc *chunkCompiler) allocReg(use regset, noUse regset) wam.RegAddr {
	free := cc.freeRegs.union(use)
	if len(free) == 0 {
		free = cc.freeRegs.difference(noUse)
	}
	reg := free[0] // Get the min of free registers
	cc.freeRegs = cc.freeRegs.remove(reg)
	return reg
}
