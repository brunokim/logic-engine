package compiler

import (
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

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

type regSet []wam.RegAddr

type chunkSets struct {
	maxRegs  int
	use      map[logic.Var]regSet
	noUse    map[logic.Var]regSet
	conflict map[logic.Var]regSet
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
	use      map[logic.Var]regSet
	noUse    map[logic.Var]regSet
	conflict map[logic.Var]regSet

	delayedComplexTerms []delayedComplexTerm

	freeRegs   regSet
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

func (cc *chunkCompiler) termAddr(term logic.Term) (wam.Addr, addrAlloc) {
	return wam.StackAddr(0), existingTerm
}

func (cc *chunkCompiler) varAddr(x logic.Var, isHead bool) (wam.Addr, addrAlloc) {
	return wam.StackAddr(0), existingTerm
}

func (cc *chunkCompiler) tempAddr(x logic.Term, isHead bool) (wam.RegAddr, addrAlloc) {
	return wam.RegAddr(0), existingTerm
}

func (cc *chunkCompiler) allocReg(x logic.Term, use regSet, noUse regSet) wam.RegAddr {
	return wam.RegAddr(0)
}
