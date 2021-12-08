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

type chunk struct {
	terms []logic.Term
}

func (chunk *chunk) vars() []logic.Var {
	return nil
}

func chunks(clause *logic.Clause) []*chunk {
	return nil
}

type clauseChunks struct {
	temps      []logic.Var
	permanents []logic.Var
	chunks     []*chunk
}

func newClauseChunks(clause *logic.Clause) *clauseChunks {
	return nil
}

func countNestedStructs(chunk *chunk) int {
	return 0
}

type addrAlloc int

const (
	existingTerm addrAlloc = iota
	newVariable
	newComplexTerm
)

type chunkSets struct {
	maxRegs  int
	use      map[logic.Var]regset
	noUse    map[logic.Var]regset
	conflict map[logic.Var]regset
}

func newChunkSets(chunk *chunk, temps []logic.Var, isHead bool) *chunkSets {
	return nil
}

type clauseCompiler struct {
	clause         *logic.Clause
	temps          []logic.Var
	permanents     []logic.Var
	chunks         []*chunk
	permanentAddrs map[logic.Var]wam.StackAddr
	tempAddrs      map[logic.Term]wam.RegAddr
}

func newClauseCompiler(clause *logic.Clause) *clauseCompiler {
	return nil
}

func (cc *clauseCompiler) compile() []wam.Instruction {
	return nil
}

func (cc *clauseCompiler) isPermanent(x logic.Var) bool {
	return false
}

func (cc *clauseCompiler) permanentAddr(x logic.Var) (wam.StackAddr, addrAlloc) {
	return wam.StackAddr(0), existingTerm
}

type delayedComplexTerm struct {
	term     logic.Term
	register wam.RegAddr
}

type chunkCompiler struct {
	chunk  *chunk
	isHead bool
	parent *clauseCompiler

	maxRegs  int
	use      map[logic.Var]regset
	noUse    map[logic.Var]regset
	conflict map[logic.Var]regset

	delayedComplexTerms []delayedComplexTerm

	freeRegs   regset
	tempAddrs  map[logic.Term]wam.RegAddr
	regContent map[wam.RegAddr]logic.Term
}

func newChunkCompiler(chunk *chunk, isHead bool, clauseCompiler *clauseCompiler) *chunkCompiler {
	return nil
}

func (cc *chunkCompiler) setReg(reg wam.RegAddr, term logic.Term) {
}

func (cc *chunkCompiler) unsetReg(reg wam.RegAddr, term logic.Term) {
}

func (cc *chunkCompiler) compile() []wam.Instruction {
	return nil
}

func (cc *chunkCompiler) compileHead(head logic.Term) []wam.Instruction {
	return nil
}

func (cc *chunkCompiler) getTerm(term logic.Term, reg wam.RegAddr) []wam.Instruction {
	return nil
}

func (cc *chunkCompiler) unifyArg(term logic.Term) []wam.Instruction {
	return nil
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
				cc.freeRegs.add(regAddr)
			}
		case complexTerm:
			if !step.hasPutArgs {
				var nextSteps []putStep
				cc.freeRegs.remove(step.reg) // Reserve reg for put_struct instruction in the second round
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
		use, noUse = cc.use[x], cc.noUse[x]
		if !isHead {
			// Conflict avoidance: do not use registers that are
			// necessary for last goal args.
			noUse = noUse.union(cc.conflict[x])
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
	cc.freeRegs.remove(reg)
	return reg
}
