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

func (cc *chunkCompiler) putTerm(term logic.Term, reg wam.RegAddr, isTopLevel bool) []wam.Instruction {
	return nil
}

// Return address for a term.
func (cc *chunkCompiler) termAddr(term logic.Term) (wam.Addr, addrAlloc) {
	isHead := false
	cat := logic.Category(term)
	switch cat {
	case logic.Atomic:
		return wam.ConstantAddr{toConstant(term)}, existingTerm
	case logic.Variable:
		return cc.varAddr(term.(logic.Var), isHead)
	case logic.Complex:
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
