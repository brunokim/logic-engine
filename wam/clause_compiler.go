package wam

import (
	"fmt"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/errors"
	"github.com/brunokim/logic-engine/logic"
)

func numArgs(clause goalList) int {
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
		panic(fmt.Sprintf("Temp variable %v doesn't have allocation sets", x))
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

func (cc *clauseCompiler) compileGoal(pos int, g goal) ([]Instruction, error) {
	cc.instrs = nil
	if g.pkg != "" {
		return cc.compileCall(g)
	}
	term := g.comp
	if _, ok := inlinedFunctors[term.Functor]; ok {
		return cc.inlinedFunctor(pos, term)
	}
	if _, ok := inlinedIndicators[term.Indicator()]; ok {
		return cc.inlinedIndicator(pos, term)
	}
	return cc.compileCall(g)
}

func (cc *clauseCompiler) compileCall(g goal) ([]Instruction, error) {
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

// ---- Handle control goals and inlined cases

func asmGoal(t logic.Term) goal {
	return goal{"", comp("asm", t)}
}

func (cc *clauseCompiler) flattenControl(terms0 goalList) (goalList, error) {
	terms := make(goalList, len(terms0))
	copy(terms, terms0)
	var body goalList
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

func (cc *clauseCompiler) controlFunctor(term *logic.Comp) (goalList, error) {
	switch term.Functor {
	case "and":
		return toGoals(term.Args)
	default:
		panic(fmt.Sprintf("Unimplemented control functor %s (%v)", term.Functor, term))
	}
}

func (cc *clauseCompiler) controlIndicator(term *logic.Comp) (goalList, error) {
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
		return goalList{
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
		return goalList{
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
