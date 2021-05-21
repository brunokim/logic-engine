package wam

import (
	"fmt"
	"regexp"
	"strconv"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/errors"
	"github.com/brunokim/logic-engine/logic"
)

var functorRE = regexp.MustCompile(`^(.+)/(\d+)$`)

func decodeFunctor(t logic.Term) Functor {
	a := t.(logic.Atom)
	fn, err := ParseFunctor(a.Name)
	if err != nil {
		panic(err)
	}
	return fn
}

// ParseFunctor returns a Functor from a string like 'fn/3'.
func ParseFunctor(s string) (Functor, error) {
	matches := functorRE.FindStringSubmatch(s)
	if len(matches) != 3 {
		return Functor{}, errors.New("%q doesn't match a functor pattern", s)
	}
	name, arityStr := matches[1], matches[2]
	arity, err := strconv.Atoi(arityStr)
	if err != nil {
		return Functor{}, errors.New("invalid arity for functor %q: %v", s, err)
	}
	return Functor{name, arity}, nil
}

func decodeAddr(t logic.Term) Addr {
	switch x := t.(type) {
	case logic.Atom:
		return ConstantAddr{toConstant(x)}
	case logic.Int:
		return ConstantAddr{toConstant(x)}
	case logic.Var:
		switch x.Name[0] {
		case 'X', 'A':
			return RegAddr(varAddr(x))
		case 'Y':
			return StackAddr(varAddr(x))
		default:
			panic(fmt.Sprintf("invalid letter for addr %v", x))
		}
	default:
		panic(fmt.Sprintf("invalid addr %v", t))
	}
}

func decodeRegAddr(t logic.Term) RegAddr {
	x := t.(logic.Var)
	s := x.Name
	if s[0] != 'X' && s[0] != 'A' {
		panic(fmt.Sprintf("invalid letter for register addr in %v", x))
	}
	return RegAddr(varAddr(x))
}

func varAddr(x logic.Var) int {
	addr, err := strconv.Atoi(x.Name[1:])
	if err != nil {
		panic(fmt.Sprintf("invalid address for %v: %v", x, err))
	}
	return addr
}

func decodePairTag(t logic.Term) PairTag {
	switch t {
	case dsl.Atom("list"):
		return ListPair
	case dsl.Atom("assoc"):
		return AssocPair
	case dsl.Atom("dict"):
		return DictPair
	default:
		panic(fmt.Sprintf("invalid pair tag: %v", t))
	}
}

func decodeExecutionMode(t logic.Term) ExecutionMode {
	switch t {
	case dsl.Atom("run"):
		return Run
	case dsl.Atom("verify_attributes"):
		return VerifyAttributes
	default:
		panic(fmt.Sprintf("invalid execution mode: %v", t))
	}
}

func decodeAddrList(t logic.Term) []Addr {
	if t == logic.EmptyList {
		return nil
	}
	list := t.(*logic.List)
	return decodeAddrs(list.Terms)
}

func decodeAddrs(terms []logic.Term) []Addr {
	addrs := make([]Addr, len(terms))
	for i, term := range terms {
		addrs[i] = decodeAddr(term)
	}
	return addrs
}

func decodeString(t logic.Term) string {
	a := t.(logic.Atom)
	return a.Name
}

func decodeInt(t logic.Term) int {
	i := t.(logic.Int)
	return i.Value
}

func decodeInstr(t logic.Term) InstrAddr {
	c := t.(*logic.Comp)
	if c.Indicator() != dsl.Indicator("instr", 2) {
		panic(fmt.Sprintf("invalid instruction address %v", t))
	}
	ptr := c.Args[0].(logic.Ptr)
	var clause *Clause
	if ptr.Value != nil {
		clause = ptr.Value.(*Clause)
	}
	pos := decodeInt(c.Args[1])
	return InstrAddr{Clause: clause, Pos: pos}
}

func decodeConstantInstrMap(t logic.Term) map[Constant]InstrAddr {
	d := t.(*logic.Dict)
	m := make(map[Constant]InstrAddr)
	for _, assoc := range d.Assocs {
		key := toConstant(assoc.Key)
		val := decodeInstr(assoc.Val)
		m[key] = val
	}
	return m
}

func decodeFunctorInstrMap(t logic.Term) map[Functor]InstrAddr {
	d := t.(*logic.Dict)
	m := make(map[Functor]InstrAddr)
	for _, assoc := range d.Assocs {
		key := decodeFunctor(assoc.Key)
		val := decodeInstr(assoc.Val)
		m[key] = val
	}
	return m
}

func decodeMachineFunc(t logic.Term) func(*Machine, []Addr) (InstrAddr, error) {
	ptr := t.(logic.Ptr)
	return ptr.Value.(func(*Machine, []Addr) (InstrAddr, error))
}

// DecodeInstruction builds an instruction from its representation as a logic term.
func DecodeInstruction(term logic.Term) Instruction {
	var c *logic.Comp
	switch t := term.(type) {
	case logic.Atom:
		c = dsl.Comp(t.Name)
	case *logic.Comp:
		c = t
	default:
		panic(fmt.Sprintf("invalid instruction representation %v", term))
	}
	if c.Functor == "builtin" && len(c.Args) >= 2 {
		return builtin{
			Name: decodeString(c.Args[0]),
			Func: decodeMachineFunc(c.Args[1]),
			Args: decodeAddrs(c.Args[2:]),
		}
	}
	switch c.Indicator() {
	case dsl.Indicator("put_struct", 2):
		return putStruct{decodeFunctor(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("put_variable", 2):
		return putVariable{decodeAddr(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("put_value", 2):
		return putValue{decodeAddr(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("put_constant", 2):
		return putConstant{toConstant(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("put_pair", 2):
		return putPair{decodePairTag(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("get_struct", 2):
		return getStruct{decodeFunctor(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("get_variable", 2):
		return getVariable{decodeAddr(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("get_value", 2):
		return getValue{decodeAddr(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("get_constant", 2):
		return getConstant{toConstant(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("get_pair", 2):
		return getPair{decodePairTag(c.Args[0]), decodeRegAddr(c.Args[1])}
	case dsl.Indicator("unify_variable", 1):
		return unifyVariable{decodeAddr(c.Args[0])}
	case dsl.Indicator("unify_value", 1):
		return unifyValue{decodeAddr(c.Args[0])}
	case dsl.Indicator("unify_constant", 1):
		return unifyConstant{toConstant(c.Args[0])}
	case dsl.Indicator("unify_void", 0):
		return unifyVoid{}
	case dsl.Indicator("call", 1):
		return call{Functor: decodeFunctor(c.Args[0])}
	case dsl.Indicator("call", 2):
		return call{Pkg: decodeAddr(c.Args[0]), Functor: decodeFunctor(c.Args[1])}
	case dsl.Indicator("call_meta", 2):
		return callMeta{decodeAddr(c.Args[0]), decodeAddrList(c.Args[1])}
	case dsl.Indicator("execute", 1):
		return execute{Functor: decodeFunctor(c.Args[0])}
	case dsl.Indicator("execute", 2):
		return execute{Pkg: decodeAddr(c.Args[0]), Functor: decodeFunctor(c.Args[1])}
	case dsl.Indicator("execute_meta", 2):
		return executeMeta{decodeAddr(c.Args[0]), decodeAddrList(c.Args[1])}
	case dsl.Indicator("proceed", 1):
		return proceed{decodeExecutionMode(c.Args[0])}
	case dsl.Indicator("halt", 0):
		return halt{}
	case dsl.Indicator("allocate", 1):
		return allocate{decodeInt(c.Args[0])}
	case dsl.Indicator("deallocate", 0):
		return deallocate{}
	case dsl.Indicator("try_me_else", 1):
		return tryMeElse{decodeInstr(c.Args[0])}
	case dsl.Indicator("retry_me_else", 1):
		return retryMeElse{decodeInstr(c.Args[0])}
	case dsl.Indicator("trust_me", 0):
		return trustMe{}
	case dsl.Indicator("try", 1):
		return try{decodeInstr(c.Args[0])}
	case dsl.Indicator("retry", 1):
		return retry{decodeInstr(c.Args[0])}
	case dsl.Indicator("trust", 1):
		return trust{decodeInstr(c.Args[0])}
	case dsl.Indicator("label", 1):
		return label{decodeInt(c.Args[0])}
	case dsl.Indicator("jump", 1):
		return jump{decodeInstr(c.Args[0])}
	case dsl.Indicator("switch_on_term", 6):
		return switchOnTerm{
			decodeInstr(c.Args[0]),
			decodeInstr(c.Args[1]),
			decodeInstr(c.Args[2]),
			decodeInstr(c.Args[3]),
			decodeInstr(c.Args[4]),
			decodeInstr(c.Args[5]),
		}
	case dsl.Indicator("switch_on_constant", 1):
		return switchOnConstant{decodeConstantInstrMap(c.Args[0])}
	case dsl.Indicator("switch_on_struct", 1):
		return switchOnStruct{decodeFunctorInstrMap(c.Args[0])}
	case dsl.Indicator("neck_cut", 0):
		return neckCut{}
	case dsl.Indicator("cut", 0):
		return cut{}
	case dsl.Indicator("fail", 0):
		return fail{}
	case dsl.Indicator("import", 1):
		return importPkg{decodeString(c.Args[0])}
	case dsl.Indicator("put_attr", 3):
		return putAttr{decodeString(c.Args[0]), decodeAddr(c.Args[1]), decodeAddr(c.Args[2])}
	case dsl.Indicator("get_attr", 3):
		return getAttr{decodeString(c.Args[0]), decodeAddr(c.Args[1]), decodeAddr(c.Args[2])}
	case dsl.Indicator("del_attr", 2):
		return delAttr{decodeString(c.Args[0]), decodeAddr(c.Args[1])}
	case dsl.Indicator("=", 2):
		return inlineUnify{decodeAddr(c.Args[0]), decodeAddr(c.Args[1])}
	default:
		panic(fmt.Sprintf("decode: unhandled instruction %v", c))
	}
}

func toReg(addr Addr) RegAddr {
	if r, ok := addr.(RegAddr); ok {
		return r
	}
	return 0
}

func registers(instr Instruction) (RegAddr, RegAddr) {
	switch i := instr.(type) {
	case putStruct:
		return 0, i.ArgAddr
	case putVariable:
		return toReg(i.Addr), i.ArgAddr
	case putValue:
		return toReg(i.Addr), i.ArgAddr
	case putConstant:
		return 0, i.ArgAddr
	case putPair:
		return 0, i.ArgAddr
	case getStruct:
		return 0, i.ArgAddr
	case getVariable:
		return toReg(i.Addr), i.ArgAddr
	case getValue:
		return toReg(i.Addr), i.ArgAddr
	case getConstant:
		return 0, i.ArgAddr
	case getPair:
		return 0, i.ArgAddr
	case unifyVariable:
		return toReg(i.Addr), 0
	case unifyValue:
		return toReg(i.Addr), 0
	}
	return 0, 0
}

func subClausesInClause(clause *Clause) []*Clause {
	if clause == nil {
		return nil
	}
	var clauses []*Clause
	for _, instr := range clause.Code {
		clauses = append(clauses, subClauses(instr)...)
	}
	return clauses
}

func tryMeChain(instr tryMeElse) []*Clause {
	var clauses []*Clause
	clause := instr.Alternative.Clause
	for clause != nil {
		clauses = append(clauses, clause)
		switch ins := clause.Code[0].(type) {
		case retryMeElse:
			clause = ins.Alternative.Clause
		case trustMe:
			clause = nil
		default:
			panic(fmt.Sprintf("unexpected instruction in try_me_else chain: %v", ins))
		}
	}
	return clauses
}

func subClauses(instr Instruction) []*Clause {
	switch i := instr.(type) {
	case tryMeElse:
		return tryMeChain(i)
	case try:
		subSub := subClausesInClause(i.Continuation.Clause)
		return append(subSub, i.Continuation.Clause)
	case retry:
		subSub := subClausesInClause(i.Continuation.Clause)
		return append(subSub, i.Continuation.Clause)
	case trust:
		subSub := subClausesInClause(i.Continuation.Clause)
		return append(subSub, i.Continuation.Clause)
	case switchOnTerm:
		return []*Clause{
			i.IfVar.Clause,
			i.IfConstant.Clause,
			i.IfStruct.Clause,
			i.IfList.Clause,
			i.IfAssoc.Clause,
			i.IfDict.Clause,
		}
	case switchOnConstant:
		var clauses []*Clause
		for _, instrAddr := range i.Continuation {
			clauses = append(clauses, instrAddr.Clause)
		}
		return clauses
	case switchOnStruct:
		var clauses []*Clause
		for _, instrAddr := range i.Continuation {
			clauses = append(clauses, instrAddr.Clause)
		}
		return clauses
	}
	return nil
}

func maxReg(code []Instruction) RegAddr {
	var max RegAddr
	for _, instr := range code {
		// Check registers directly
		reg1, reg2 := registers(instr)
		if reg1 > max {
			max = reg1
		}
		if reg2 > max {
			max = reg2
		}
		// Check number of registers in sub-clauses
		for _, clause := range subClauses(instr) {
			if clause == nil {
				continue
			}
			reg := RegAddr(clause.NumRegisters) - 1
			if reg > max {
				max = reg
			}
		}
	}
	return max
}

// DecodeClause builds a WAM clause from its representation as a logic term.
func DecodeClause(functor logic.Indicator, instructions ...logic.Term) *Clause {
	instrs := make([]Instruction, len(instructions))
	for i, instruction := range instructions {
		instrs[i] = DecodeInstruction(instruction)
	}
	max := maxReg(instrs)
	return &Clause{
		Functor:      toFunctor(functor),
		NumRegisters: int(max) + 1,
		Code:         instrs,
	}
}
