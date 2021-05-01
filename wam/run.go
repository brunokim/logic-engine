package wam

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"

	"github.com/brunokim/logic-engine/logic"
)

// RunQuery executes the given logic query, returning all bindings that satisfy the query
// and compiled clauses simultaneously.
func (m *Machine) RunQuery(query ...logic.Term) (map[logic.Var]logic.Term, error) {
	seen := make(map[logic.Var]struct{})
	m.xs = nil
	for _, c := range query {
		for _, x := range logic.Vars(c) {
			if _, ok := seen[x]; ok {
				continue
			}
			seen[x] = struct{}{}
			m.xs = append(m.xs, x)
		}
	}
	c, err := compileQuery(query)
	if err != nil {
		return nil, err
	}
	m.AddClause(c)
	return m.runOnce()
}

func (m *Machine) runOnce() (map[logic.Var]logic.Term, error) {
	if err := m.Run(); err != nil {
		return nil, err
	}
	if len(m.xs) == 0 {
		return map[logic.Var]logic.Term{}, nil
	}
	if m.Env == nil {
		return nil, fmt.Errorf("nil env at the end of execution")
	}
	return fromCells(m.xs, m.Env.PermanentVars), nil
}

var debugFilenameRE = regexp.MustCompile(`(.*?)(-(\d+))?.jsonl`)

func incrementDebugFilename(filename string) string {
	parts := debugFilenameRE.FindStringSubmatch(filename)
	if len(parts) == 0 {
		return filename
	}
	name, iter := parts[1], parts[3]
	i := 1
	if iter != "" {
		i, _ = strconv.Atoi(iter)
		i++
	}
	return fmt.Sprintf("%s-%03d.jsonl", name, i)
}

// NextSolution backtracks on the next choice point and execute that
// alternate path, returning all bindings that satisfy the original query.
func (m *Machine) NextSolution() (map[logic.Var]logic.Term, error) {
	instr, err := m.backtrack(fmt.Errorf("no more solutions"))
	if err != nil {
		return nil, err
	}
	m.CodePtr = instr
	if m.DebugFilename != "" {
		m.DebugFilename = incrementDebugFilename(m.DebugFilename)
	}
	return m.runOnce()
}

// Interrupt signals the machine to shutdown, and blocks until it has received
// the signal.
func (m *Machine) Interrupt() {
	m.interrupt <- struct{}{}
}

// Run executes the instructions currently present in the machine.
//
// If CodePtr was not set, it will look for a clause with empty functor (/0) to treat as query,
// and will start execution from its first instruction.
func (m *Machine) Run() error {
	if m.IterLimit == 0 {
		m.IterLimit = math.MaxInt32
	}
	if !m.CodePtr.isValid() {
		query, ok := m.Code[Functor{}]
		if !ok {
			return fmt.Errorf("query clause with empty functor (/0) not found")
		}
		m.CodePtr = InstrAddr{Clause: query, Pos: 0}
	}
	var i int
	f := m.debugInit()
	defer m.debugClose(f)
	m.debugWrite(f, 0)
	for ; i < m.IterLimit; i++ {
		instr := m.CodePtr.instr()
		if instr == nil {
			return fmt.Errorf("invalid instruction @ clock %d (did you miss a proceed or deallocate at the end of a clause?)", i)
		}
		if _, ok := instr.(Halt); ok {
			// Return normally when reaching Halt instruction.
			break
		}
		select {
		case <-m.interrupt:
			return fmt.Errorf("interrupted @ clock %d", i)
		default:
		}
		m.hasBacktracked = false
		nextInstr, err := m.execute(instr)
		if err != nil {
			return err
		}
		m.CodePtr = nextInstr
		m.debugWrite(f, i)
	}
	if i >= m.IterLimit {
		return fmt.Errorf("maximum iteration limit reached: %d", i)
	}
	return nil
}

// ---- debugging

func (m *Machine) debugInit() io.WriteCloser {
	if m.DebugFilename == "" {
		return nil
	}
	f, err := os.Create(m.DebugFilename)
	if err != nil {
		log.Printf("Failed to open debug file: %v", err)
		return nil
	}
	m.shouldEncodeClauses = true
	data, err := json.Marshal(m)
	if err != nil {
		log.Printf("Failed to marshal data: %v", err)
		return nil
	}
	m.shouldEncodeClauses = false
	f.Write(data)
	f.Write([]byte{'\n'})
	return f
}

func (m *Machine) debugClose(f io.WriteCloser) {
	if f == nil {
		return
	}
	if err := f.Close(); err != nil {
		log.Printf("Failed closing debug file: %v", err)
	}
}

func (m *Machine) debugWrite(f io.WriteCloser, counter int) {
	if f == nil {
		return
	}
	data, err := json.Marshal(m)
	if err != nil {
		log.Printf("Failed to marshal data: %v", err)
		return
	}
	f.Write(data)
	f.Write([]byte{'\n'})
}

// ---- memory access

func (m *Machine) set(addr Addr, cell Cell) {
	switch a := addr.(type) {
	case RegAddr:
		m.Reg[a] = cell
	case StackAddr:
		m.Env.PermanentVars[a] = cell
	default:
		panic(fmt.Sprintf("wam.Machine.set: unhandled type %T (%v)", addr, addr))
	}
}

func (m *Machine) get(addr Addr) Cell {
	switch a := addr.(type) {
	case RegAddr:
		return m.Reg[a]
	case StackAddr:
		return m.Env.PermanentVars[a]
	default:
		panic(fmt.Sprintf("wam.Machine.get: unhandled type %T (%v)", addr, addr))
	}
}

// ---- reading/writing complex terms

func (m *Machine) writeArg(instr Instruction) Cell {
	switch instr := instr.(type) {
	case UnifyVariable:
		// Place new unbound ref during query building.
		x := m.newRef()
		m.set(instr.Addr, x)
		return x
	case UnifyValue:
		// Copy already-seen cell from register to the heap during query building.
		return m.get(instr.Addr)
	case UnifyConstant:
		// Push a constant to the current struct arg.
		return instr.Constant
	case UnifyVoid:
		// Push an unbound variable to the current struct arg.
		return m.newRef()
	}
	panic(fmt.Sprintf("writeArg: unhandled instr type: %T (%v)", instr, instr))
}

func (m *Machine) writeStructArgs(c *Struct, instrs []Instruction) {
	for i, instr := range instrs {
		c.Args[i] = m.writeArg(instr)
	}
}

func (m *Machine) writePairArgs(c *Pair, instrs []Instruction) {
	c.Head = m.writeArg(instrs[0])
	c.Tail = m.writeArg(instrs[1])
}

func (m *Machine) readArg(instr Instruction, arg Cell) error {
	switch instr := instr.(type) {
	case UnifyVariable:
		// Unify newly-seen cell, placing current arg in register.
		m.set(instr.Addr, arg)
	case UnifyValue:
		// Unify already-seen cell, unifying the address with the arg.
		cell := m.get(instr.Addr)
		return m.unify(cell, arg)
	case UnifyConstant:
		// Unify already-seen constant, unifying the address with the arg.
		if err := m.readConstant(instr.Constant, arg); err != nil {
			return err
		}
	case UnifyVoid:
		// Do nothing
	default:
		panic(fmt.Sprintf("readArg: unhandled instruction type %T (%v)", instr, instr))
	}
	return nil
}

func (m *Machine) readConstant(constant Constant, arg Cell) error {
	cell := deref(arg)
	switch c := cell.(type) {
	case Constant:
		if c != constant {
			return &unifyError{c, constant}
		}
	case *Ref:
		c.Cell = constant
		m.trail(c)
	default:
		return &unifyError{cell, constant}
	}
	return nil
}

func (m *Machine) readStructArgs(c *Struct, instrs []Instruction) error {
	for i, instr := range instrs {
		if err := m.readArg(instr, c.Args[i]); err != nil {
			return err
		}
	}
	return nil
}

func (m *Machine) readPairArgs(c *Pair, instrs []Instruction) error {
	if err := m.readArg(instrs[0], c.Head); err != nil {
		return err
	}
	if err := m.readArg(instrs[1], c.Tail); err != nil {
		return err
	}
	return nil
}

// ---- build objects

func (m *Machine) newRef() *Ref {
	m.LastRefID++
	x := &Ref{nil, m.LastRefID}
	return x
}

func makeStructFrom(f Functor) *Struct {
	return &Struct{f.Name, make([]Cell, f.Arity)}
}

func (m *Machine) newChoicePoint(alternative InstrAddr) *ChoicePoint {
	numArgs := m.CodePtr.Clause.Functor.Arity
	choicePoint := &ChoicePoint{
		Prev:            m.ChoicePoint,
		Continuation:    m.Continuation,
		NextAlternative: alternative,
		AttrTrail:       make(map[*Ref]map[string]Cell),
		Args:            make([]Cell, numArgs),
		LastRefID:       m.LastRefID,
		Env:             m.Env,
		CutChoice:       m.CutChoice,
	}
	copy(choicePoint.Args, m.Reg)
	return choicePoint
}

// ---- attributes

func (m *Machine) setAttribute(ref *Ref, name string, attr Cell) {
	m.trailAttribute(ref, name)
	attrs, ok := m.attributes[ref.id]
	if !ok {
		attrs = make(map[string]Cell)
		m.attributes[ref.id] = attrs
	}
	attrs[name] = attr
}

func (m *Machine) getAttribute(ref *Ref, name string) Cell {
	attrs, ok := m.attributes[ref.id]
	if !ok {
		return nil
	}
	return attrs[name]
}

// ---- control

func (m *Machine) restoreFromChoicePoint() {
	copy(m.Reg, m.ChoicePoint.Args)
	m.Env = m.ChoicePoint.Env
	m.Continuation = m.ChoicePoint.Continuation
	m.unwindTrail()
}

func (m *Machine) call(functor Functor) (InstrAddr, error) {
	clause, ok := m.Code[functor]
	if !ok {
		return m.backtrack(fmt.Errorf("clause not found: %v", functor))
	}
	return InstrAddr{Clause: clause, Pos: 0}, nil
}

func (m *Machine) putMeta(addr Addr, params []Addr) (Functor, error) {
	cell := deref(m.get(addr))
	var name string
	var args []Cell
	switch c := cell.(type) {
	case *Struct:
		name, args = c.Name, c.Args
	case WAtom:
		name, args = string(c), nil
	default:
		return Functor{}, fmt.Errorf("not an atom or struct: %v", cell)
	}
	n1, n2 := len(args), len(params)
	copy(m.Reg, args)
	for i, param := range params {
		m.Reg[n1+i] = deref(m.get(param))
	}
	return Functor{Name: name, Arity: n1 + n2}, nil
}

func (m *Machine) execute(instr Instruction) (InstrAddr, error) {
	switch instr := instr.(type) {
	case PutStruct:
		// Place flattened struct (in post order) during query building.
		f := makeStructFrom(instr.Functor)
		n := instr.Functor.Arity
		m.Reg[instr.ArgAddr] = f
		m.writeStructArgs(f, m.CodePtr.next(n))
		return m.CodePtr.jump(n + 1), nil
	case PutVariable:
		// Place newly-seen query argument as an unbound ref during query building.
		x := m.newRef()
		m.Reg[instr.ArgAddr] = x
		m.set(instr.Addr, x)
	case PutValue:
		// Move already-seen query argument from register to arg register.
		m.Reg[instr.ArgAddr] = m.get(instr.Addr)
	case PutConstant:
		// Put constant as argument in register.
		m.Reg[instr.ArgAddr] = instr.Constant
	case PutPair:
		// Put pair as argument in register.
		l := &Pair{Tag: instr.Tag}
		m.Reg[instr.ArgAddr] = l
		m.writePairArgs(l, m.CodePtr.next(2))
		return m.CodePtr.jump(3), nil
	case GetStruct:
		// Get flattened struct (in pre order) from register.
		// If already a literal struct, will read another struct from the heap during unification.
		// If a ref, will build the struct on the heap instead. In this case, it's necessary to
		// bind the register address with the newly created heap address.
		cell := deref(m.get(instr.ArgAddr))
		n := instr.Functor.Arity
		switch c := cell.(type) {
		case *Struct:
			if f := c.Functor(); f != instr.Functor {
				return m.backtrack(&unifyError{f, instr.Functor})
			}
			if err := m.readStructArgs(c, m.CodePtr.next(n)); err != nil {
				return m.backtrack(err)
			}
			return m.CodePtr.jump(n + 1), nil
		case *Ref:
			f := makeStructFrom(instr.Functor)
			m.bind(c, f)
			m.writeStructArgs(f, m.CodePtr.next(n))
			return m.CodePtr.jump(n + 1), nil
		default:
			return m.backtrack(&unifyError{cell, instr.Functor})
		}
	case GetVariable:
		// Move newly-seen clause param from arg register to register.
		m.set(instr.Addr, m.Reg[instr.ArgAddr])
	case GetValue:
		// Unify already-seen clause param with register value.
		if err := m.unify(m.get(instr.Addr), m.get(instr.ArgAddr)); err != nil {
			return m.backtrack(err)
		}
	case GetConstant:
		// Expect a constant from register.
		if err := m.readConstant(instr.Constant, m.get(instr.ArgAddr)); err != nil {
			return m.backtrack(err)
		}
	case GetPair:
		// Expect a pair from register.
		cell := deref(m.get(instr.ArgAddr))
		switch c := cell.(type) {
		case *Pair:
			if c.Tag != instr.Tag {
				return m.backtrack(&unifyError{cell, &Pair{Tag: instr.Tag}})
			}
			if err := m.readPairArgs(c, m.CodePtr.next(2)); err != nil {
				return m.backtrack(err)
			}
			return m.CodePtr.jump(3), nil
		case *Ref:
			l := &Pair{Tag: instr.Tag}
			m.bind(c, l)
			m.writePairArgs(l, m.CodePtr.next(2))
			return m.CodePtr.jump(3), nil
		default:
			return m.backtrack(&unifyError{cell, &Pair{Tag: instr.Tag}})
		}
	case Call:
		// Save instruction pointer, and set it to clause location.
		m.Continuation = m.CodePtr.inc()
		m.CutChoice = m.ChoicePoint
		return m.call(instr.Functor)
	case CallMeta:
		// Call clause pointed by a ref or struct.
		functor, err := m.putMeta(instr.Addr, instr.Params)
		if err != nil {
			return m.backtrack(fmt.Errorf("call_meta: %v", err))
		}
		m.Continuation = m.CodePtr.inc()
		m.CutChoice = m.ChoicePoint
		return m.call(functor)
	case Execute:
		// Trampoline into other clause, without changing the continuation.
		m.CutChoice = m.ChoicePoint
		return m.call(instr.Functor)
	case ExecuteMeta:
		// Trampoline into other dynamic clause, without changing the continuation.
		functor, err := m.putMeta(instr.Addr, instr.Params)
		if err != nil {
			return m.backtrack(fmt.Errorf("execute_meta: %v", err))
		}
		m.CutChoice = m.ChoicePoint
		return m.call(functor)
	case Proceed:
		// Jump to the continuation.
		nextInstr := m.Continuation
		m.Continuation.Clause = nil
		return nextInstr, nil
	case Allocate:
		// Allocate a new stack frame.
		env := &Env{
			Prev:          m.Env,
			Continuation:  m.Continuation,
			PermanentVars: make([]Cell, instr.NumVars),
			CutChoice:     m.CutChoice,
		}
		m.Continuation.Clause = nil
		m.Env = env
	case Deallocate:
		// Pop the current environment. It may still be in memory if a choice point references it.
		m.Continuation = m.Env.Continuation
		m.Env = m.Env.Prev
	case TryMeElse:
		// Create a choice point, saving current machine state and pointing to next possible clause.
		m.ChoicePoint = m.newChoicePoint(instr.Alternative)
	case RetryMeElse:
		// Reset the machine state to latest choice point, and point to the next possible clause.
		m.restoreFromChoicePoint()
		m.ChoicePoint.NextAlternative = instr.Alternative
	case TrustMe:
		// Reset the machine state to latest choice point, and "deallocate" current choice point.
		m.restoreFromChoicePoint()
		m.ChoicePoint = m.ChoicePoint.Prev
	case Try:
		// Create a choice point, saving current machine state and pointing to next instruction. Jump execution to the instruction continuation.
		m.ChoicePoint = m.newChoicePoint(m.CodePtr.inc())
		return instr.Continuation, nil
	case Retry:
		// Reset the machine state to latest choice point, and point to the next instruction. Jump execution to the instruction continuation.
		m.restoreFromChoicePoint()
		m.ChoicePoint.NextAlternative = m.CodePtr.inc()
		return instr.Continuation, nil
	case Trust:
		// Reset the machine state to latest choice point, and "deallocate" current choice point. Jump execution to the instruction continuation.
		m.restoreFromChoicePoint()
		m.ChoicePoint = m.ChoicePoint.Prev
		return instr.Continuation, nil
	case SwitchOnTerm:
		// Jump to instructions matching the first arg type.
		cell := deref(m.Reg[0])
		switch c := cell.(type) {
		case *Ref:
			return instr.IfVar, nil
		case Constant:
			return instr.IfConstant, nil
		case *Struct:
			return instr.IfStruct, nil
		case *Pair:
			switch c.Tag {
			case AssocPair:
				return instr.IfAssoc, nil
			case ListPair:
				return instr.IfList, nil
			case DictPair:
				return instr.IfDict, nil
				panic(fmt.Sprintf("switch_on_term: pair: unhandled pair type %T (%v)", c.Tag, c))
			}
		default:
			panic(fmt.Sprintf("switch_on_term: unhandled type %T (%v)", cell, cell))
		}
	case SwitchOnConstant:
		// Jump to instructions matching the first arg constant.
		cell := deref(m.Reg[0]).(Constant)
		cont, ok := instr.Continuation[cell]
		if !ok {
			return m.backtrack(fmt.Errorf("constant index not found: %v", cell))
		}
		return cont, nil
	case SwitchOnStruct:
		// Jump to instructions matching the first arg functor.
		cell := deref(m.Reg[0]).(*Struct)
		cont, ok := instr.Continuation[cell.Functor()]
		if !ok {
			return m.backtrack(fmt.Errorf("functor index not found: %v", cell.Functor()))
		}
		return cont, nil
	case NeckCut:
		// Remove any choicepoint created since the function call, removing choicepoints
		// due to indexing.
		if m.ChoicePoint == m.CutChoice {
			break
		}
		m.ChoicePoint = m.CutChoice
		m.tidyTrail()
	case Cut:
		// Remove any choicepoint created since the function initial execution, keeping
		// choicepoints due to indexing.
		if m.Env.CutChoice == m.ChoicePoint {
			break
		}
		m.ChoicePoint = m.Env.CutChoice
		m.tidyTrail()
	case Fail:
		// Fail unconditionally.
		return m.backtrack(fmt.Errorf("fail instruction"))
	case Builtin:
		// Calls builtin function.
		if err := instr.Func(m); err != nil {
			return m.backtrack(err)
		}
	case PutAttr:
		// Associates attribute to a ref.
		ref, ok := deref(m.get(instr.Addr)).(*Ref)
		if !ok {
			return m.backtrack(fmt.Errorf("put_attr: variable is bound"))
		}
		cell := deref(m.get(instr.Attribute))
		switch c := cell.(type) {
		case *Struct:
			m.setAttribute(ref, c.Name, c)
		default:
			return m.backtrack(fmt.Errorf("put_attr: invalid attribute"))
		}
	case GetAttr:
		// Retrieves attribute associated to ref.
		ref, ok := deref(m.get(instr.Addr)).(*Ref)
		if !ok {
			return m.backtrack(fmt.Errorf("get_attr: variable is bound"))
		}
		cell := deref(m.get(instr.Attribute))
		var attr Cell
		switch c := cell.(type) {
		case *Struct:
			attr = m.getAttribute(ref, c.Name)
		default:
			return m.backtrack(fmt.Errorf("put_attr: invalid attribute"))
		}
		if err := m.unify(attr, cell); err != nil {
			return m.backtrack(err)
		}
	default:
		panic(fmt.Sprintf("execute: unhandled instruction type %T (%v)", instr, instr))
	}
	return m.CodePtr.inc(), nil
}

func (m *Machine) backtrack(err error) (InstrAddr, error) {
	m.hasBacktracked = true
	if m.ChoicePoint == nil {
		return InstrAddr{}, err
	}
	m.CutChoice = m.ChoicePoint.CutChoice
	return m.ChoicePoint.NextAlternative, nil
}

// bind must be called with at least one unbound ref.
func (m *Machine) bind(c1, c2 Cell) error {
	ref1, isRef1 := c1.(*Ref)
	ref2, isRef2 := c2.(*Ref)
	if isRef1 && isRef2 {
		return m.bindRefs(ref1, ref2)
	}
	if isRef1 && ref1.Cell == nil {
		if err := m.checkAttrs(ref1, c2); err != nil {
			return err
		}
		ref1.Cell = c2
		m.trail(ref1)
		return nil
	}
	if isRef2 && ref2.Cell == nil {
		if err := m.checkAttrs(ref2, c1); err != nil {
			return err
		}
		ref2.Cell = c1
		m.trail(ref2)
		return nil
	}
	panic(fmt.Sprintf("bind(%v, %v): no unbound refs", c1, c2))
}

func (m *Machine) bindRefs(ref1, ref2 *Ref) error {
	if !(ref1.Cell == nil && ref2.Cell == nil) {
		panic(fmt.Sprintf("bind(%v, %v): bound ref in bind", ref1, ref2))
	}
	// Safety measure: always bind newer variables (larger id) to older
	// variables (smaller id).
	// This is "WAM Binding Rule 1", but shouldn't be necessary in our
	// impl, because we don't care if a stack variable references a "heap"
	// one, as there's no heap to manage.
	// Still, establishing an order may prevent reference loops in Refs,
	// and is a very cheap check.
	if ref1.id < ref2.id {
		ref1, ref2 = ref2, ref1
	}
	attrs1, hasAttrs1 := m.attributes[ref1.id]
	attrs2, hasAttrs2 := m.attributes[ref2.id]
	if hasAttrs1 || hasAttrs2 {
		if !hasAttrs1 {
			attrs1 = map[string]Cell{}
		}
		if !hasAttrs2 {
			attrs2 = map[string]Cell{}
		}
		for name, attr := range attrs1 {
			if _, ok := attrs2[name]; ok {
				// Join attribute present in both vars.
				m.Reg[0] = WAtom(name)
				m.Reg[1] = ref1
				m.Reg[2] = ref2
				// TODO: somehow, call join_attribute(name, X1, X2).
			} else {
				// Copy ref1's exclusive attribute to ref2.
				m.setAttribute(ref2, name, attr)
			}
		}
	}
	ref1.Cell = ref2
	m.trail(ref1)
	return nil
}

func (m *Machine) checkAttrs(ref *Ref, value Cell) error {
	attrs, ok := m.attributes[ref.id]
	if !ok {
		return nil
	}
	for name, attr := range attrs {
		newAttr := m.newRef()
		m.Reg[0] = attr
		m.Reg[1] = value
		m.Reg[2] = newAttr
		// TODO: somehow, call check_attribute(Attr, Value, NewAttr).
		m.setAttribute(ref, name, deref(newAttr))
	}
	return nil
}

// ---- unification

type unifyError struct {
	c1, c2 interface{}
}

func (err *unifyError) Error() string {
	return fmt.Sprintf("%v != %v", err.c1, err.c2)
}

// unify executes a depth-first traversal of cells, binding unbound refs to the other
// cell, or comparing them for equality.
func (m *Machine) unify(a1, a2 Cell) error {
	stack := []Cell{a1, a2}
	for len(stack) > 0 {
		// Pop address pair from stack.
		n := len(stack)
		a1, a2 := stack[n-2], stack[n-1]
		stack = stack[:n-2]
		// Deref cells and compare them.
		c1, c2 := deref(a1), deref(a2)
		if c1 == c2 {
			// They are the same, nothing to do.
			continue
		}
		_, isRef1 := c1.(*Ref)
		_, isRef2 := c2.(*Ref)
		if isRef1 || isRef2 {
			// Some of them is a ref. Bind them.
			m.bind(c1, c2)
			continue
		}
		// Special case: '{}' unifies with any dict.
		if c1 == WAtom("{}") || c2 == WAtom("{}") {
			p1, isPair1 := c1.(*Pair)
			p2, isPair2 := c2.(*Pair)
			if c1 == WAtom("{}") && isPair2 && p2.Tag == DictPair {
				continue
			}
			if c2 == WAtom("{}") && isPair1 && p1.Tag == DictPair {
				continue
			}
		}
		switch t1 := c1.(type) {
		case Constant:
			// If they are both constants, check that they are equal.
			t2, ok := c2.(Constant)
			if !(ok && t1 == t2) {
				return &unifyError{c1, c2}
			}
		case *Struct:
			// Check if they are both struct cells.
			t2, ok := c2.(*Struct)
			if !ok {
				return &unifyError{c1, c2}
			}
			// Get the functors being pointed by the structs.
			f1, f2 := t1.Functor(), t2.Functor()
			if f1 != f2 {
				return &unifyError{f1, f2}
			}
			// Push addresses of args pair-wise onto stack.
			for i := f1.Arity - 1; i >= 0; i-- {
				stack = append(stack, t1.Args[i], t2.Args[i])
			}
		case *Pair:
			// Check if they are the same type of pair cells.
			t2, ok := c2.(*Pair)
			if !(ok && t1.Tag == t2.Tag) {
				return &unifyError{c1, c2}
			}
			if t1.Tag == DictPair {
				pairs, err := m.dictMatchingPairs(t1, t2)
				if err != nil {
					return err
				}
				stack = append(stack, pairs...)
			} else {
				// Assocs, Lists: compare heads and tails.
				stack = append(stack, t1.Tail, t2.Tail, t1.Head, t2.Head)
			}
		default:
			panic(fmt.Sprintf("wam.Machine.unify: unhandled type %T (%v)", c1, c1))
		}
	}
	return nil
}

// Walks the list of (sorted) assocs from each dict, trying to match their keys.
// Assocs whose key are not present in the other are matched with the other dict's parent.
func (m *Machine) dictMatchingPairs(d1, d2 *Pair) ([]Cell, error) {
	assocs1, parent1, err1 := unrollDict(d1)
	assocs2, parent2, err2 := unrollDict(d2)
	if err1 != nil {
		return nil, err1
	}
	if err2 != nil {
		return nil, err2
	}
	matching, rest1, rest2 := assocsDifference(assocs1, assocs2)
	hasRest1 := len(rest1) > 0
	hasRest2 := len(rest2) > 0
	isComplete1 := parent1 == WAtom("{}")
	isComplete2 := parent2 == WAtom("{}")

	var cells []Cell
	// {a:1|P1} = {a:1|P2} => P1=P2
	if !hasRest1 && !hasRest2 && !isComplete1 && !isComplete2 {
		cells = append(cells, parent1, parent2)
	}
	// {a:1|P1}      = {a:1, b:2}    => P1={b:2}
	// {a:1|P1}      = {a:1, b:2|P2} => P1={b:2|P2}
	// {a:1, c:3|P1} = {a:1, b:2}    => P1={b:2}
	if !isComplete1 && hasRest2 && (!hasRest1 || isComplete2) {
		cells = append(cells, parent1, rollDict(rest2, parent2))
	}
	// {a:1, b:2}    = {a:1|P2}      => {b:2}=P2
	// {a:1, b:2|P1} = {a:1|P2}      => {b:2|P1}=P2
	// {a:1, b:2}    = {a:1, c:3|P2} => {b:2}=P2
	if !isComplete2 && hasRest1 && (!hasRest2 || isComplete1) {
		cells = append(cells, rollDict(rest1, parent1), parent2)
	}
	// {a:1, b:2|P1} = {a:2, c:3|P2} => P1={c:3|P}, {b:2|P}=P2
	if hasRest1 && hasRest2 && !isComplete1 && !isComplete2 {
		parent := m.newRef()
		cells = append(cells,
			parent1, rollDict(rest2, parent),
			rollDict(rest1, parent), parent2)
	}
	for i := len(matching) - 1; i >= 0; i-- {
		match := matching[i]
		cells = append(cells, match.left, match.right)
	}
	return cells, nil
}

// ---- choicepoints

// If a ref was created before the current choice point, than it's a conditional
// ref that must be kept in the trail.
func (m *Machine) isConditional(ref *Ref) bool {
	return m.ChoicePoint.LastRefID >= ref.id
}

// Append bound address that need to be undone when backtracking.
// Those are the addresses that already existed before the current choice
// point, since the ones created after will be unreachable after resetting.
func (m *Machine) trail(ref *Ref) {
	if m.ChoicePoint == nil {
		return
	}
	if !m.isConditional(ref) {
		return
	}
	m.ChoicePoint.Trail = append(m.ChoicePoint.Trail, ref)
}

// Record the current value of the ref's attribute in the choicepoint's
// attribute trail.
func (m *Machine) trailAttribute(ref *Ref, name string) {
	if m.ChoicePoint == nil {
		return
	}
	trail, ok := m.ChoicePoint.AttrTrail[ref]
	if !ok {
		trail = make(map[string]Cell)
		m.ChoicePoint.AttrTrail[ref] = trail
	}
	if _, ok := trail[name]; ok {
		// Don't overwrite attribute already recorded.
		return
	}
	var attr Cell
	if attrs, ok := m.attributes[ref.id]; ok {
		attr = attrs[name]
	}
	trail[name] = attr
}

// Restore all conditional bindings and attributes since the current
// choice point back to nil, and reset trail.
func (m *Machine) unwindTrail() {
	if m.ChoicePoint == nil {
		return
	}
	cpt := m.ChoicePoint
	for _, ref := range cpt.Trail {
		ref.Cell = nil
	}
	for ref, attrs := range cpt.AttrTrail {
		for name, attr := range attrs {
			if attr != nil {
				m.attributes[ref.id][name] = attr
			} else {
				delete(m.attributes[ref.id], name)
				if len(m.attributes[ref.id]) == 0 {
					delete(m.attributes, ref.id)
				}
			}
		}
	}
	cpt.Trail = nil
	m.LastRefID = cpt.LastRefID
}

// Keep all refs that are still conditional after a cut.
func (m *Machine) tidyTrail() {
	if m.ChoicePoint == nil {
		return
	}
	var trail []*Ref
	for _, ref := range m.ChoicePoint.Trail {
		if m.isConditional(ref) {
			trail = append(trail, ref)
		}
	}
	m.ChoicePoint.Trail = trail
}
