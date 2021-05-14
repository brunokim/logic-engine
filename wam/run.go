package wam

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"regexp"
	"sort"
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
	c := compileQuery(query)
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
		var exit bool
		var err error
		switch m.Mode {
		case Run:
			exit, err = m.runCode(i)
		case Unify:
			m.verifyAttributes()
		}
		m.debugWrite(f, i)
		if err != nil {
			return err
		}
		if exit {
			break
		}
	}
	if i >= m.IterLimit {
		return fmt.Errorf("maximum iteration limit reached: %d", i)
	}
	return nil
}

func (m *Machine) runCode(i int) (bool, error) {
	instr := m.CodePtr.instr()
	if instr == nil {
		return false, fmt.Errorf("invalid instruction @ clock %d (did you miss a proceed or deallocate at the end of a clause?)", i)
	}
	if _, ok := instr.(halt); ok {
		// Return normally when reaching halt instruction.
		return true, nil
	}
	select {
	case <-m.interrupt:
		return false, fmt.Errorf("interrupted @ clock %d", i)
	default:
	}
	m.hasBacktracked = false
	nextInstr, err := m.execute(instr)
	if err != nil {
		return false, err
	}
	m.CodePtr = nextInstr
	return false, nil
}

// ---- debugging

func (m *Machine) debugInit() io.WriteCloser {
	if m.DebugFilename == "" {
		return nil
	}
	f, err := os.Create(m.DebugFilename)
	if err != nil {
		log.Printf("failed to open debug file: %v", err)
		return nil
	}
	m.shouldEncodeClauses = true
	data, err := json.Marshal(m)
	if err != nil {
		log.Printf("failed to marshal data: %v", err)
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
		log.Printf("failed closing debug file: %v", err)
	}
}

func (m *Machine) debugWrite(f io.WriteCloser, counter int) {
	if f == nil {
		return
	}
	data, err := json.Marshal(m)
	if err != nil {
		log.Printf("failed to marshal data: %v", err)
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
	case ConstantAddr:
		panic(fmt.Sprintf("wam.Machine.set: can't write %v to read-only constant %v", cell, a.Constant))
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
	case ConstantAddr:
		return a.Constant
	default:
		panic(fmt.Sprintf("wam.Machine.get: unhandled type %T (%v)", addr, addr))
	}
}

// ---- reading/writing complex terms

func (m *Machine) setMode(mode UnificationMode, cell Cell) {
	m.ComplexArg.Mode = mode
	m.ComplexArg.Cell = cell
	m.ComplexArg.Index = 0
}

func (m *Machine) resetMode() {
	m.ComplexArg.Mode = NoUnificationMode
	m.ComplexArg.Cell = nil
	m.ComplexArg.Index = 0
}

func (m *Machine) writeArg(instr Instruction) Cell {
	switch instr := instr.(type) {
	case unifyVariable:
		// Place new unbound ref during query building.
		x := m.newRef()
		m.set(instr.Addr, x)
		return x
	case unifyValue:
		// Copy already-seen cell from register to the heap during query building.
		return m.get(instr.Addr)
	case unifyConstant:
		// Push a constant to the current struct arg.
		return instr.Constant
	case unifyVoid:
		// Push an unbound variable to the current struct arg.
		return m.newRef()
	}
	panic(fmt.Sprintf("writeArg: unhandled instr type: %T (%v)", instr, instr))
}

func (m *Machine) readArg(instr Instruction, arg Cell) (InstrAddr, error) {
	switch instr := instr.(type) {
	case unifyVariable:
		// Unify newly-seen cell, placing current arg in register.
		m.set(instr.Addr, arg)
	case unifyValue:
		// Unify already-seen cell, unifying the address with the arg.
		cell := m.get(instr.Addr)
		return m.preUnify(cell, arg)
	case unifyConstant:
		// Unify already-seen constant, unifying the address with the arg.
		return m.readConstant(instr.Constant, arg)
	case unifyVoid:
		// Do nothing
	default:
		panic(fmt.Sprintf("readArg: unhandled instruction type %T (%v)", instr, instr))
	}
	return m.forward()
}

func maxIndex(c Cell) int {
	switch c := c.(type) {
	case *Struct:
		return len(c.Args)
	case *Pair:
		return 2
	default:
		panic(fmt.Sprintf("unhandled complex type %T (%v)", c, c))

	}
}

func (m *Machine) unifyArg(instr Instruction) (InstrAddr, error) {
	defer func() {
		m.ComplexArg.Index++
		if m.ComplexArg.Index >= maxIndex(m.ComplexArg.Cell) {
			m.resetMode()
		}
	}()
	switch m.ComplexArg.Mode {
	case Write:
		arg := m.writeArg(instr)
		switch c := m.ComplexArg.Cell.(type) {
		case *Struct:
			c.Args[m.ComplexArg.Index] = arg
		case *Pair:
			switch m.ComplexArg.Index {
			case 0:
				c.Head = arg
			case 1:
				c.Tail = arg
			}
		}
		return m.forward()
	case Read:
		var arg Cell
		switch c := m.ComplexArg.Cell.(type) {
		case *Struct:
			arg = c.Args[m.ComplexArg.Index]
		case *Pair:
			switch m.ComplexArg.Index {
			case 0:
				arg = c.Head
			case 1:
				arg = c.Tail
			}
		}
		return m.readArg(instr, arg)
	}
	panic(fmt.Sprintf("unifyArg: invalid machine state: %v", m.ComplexArg))
}

func (m *Machine) readConstant(constant Constant, arg Cell) (InstrAddr, error) {
	cell := deref(arg)
	switch c := cell.(type) {
	case Constant:
		if c != constant {
			return m.backtrack(&unifyError{c, constant})
		}
	case *Ref:
		if _, ok := m.attributes[c.id]; ok {
			return m.setupVerifyAttributes(map[*Ref]Cell{c: constant})
		}
		m.bindRef(c, constant)
	default:
		return m.backtrack(&unifyError{cell, constant})
	}
	return m.forward()
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

func (m *Machine) deleteAttributes(ref *Ref) {
	delete(m.attributes, ref.id)
}

// ---- control

func (m *Machine) restoreFromChoicePoint() {
	copy(m.Reg, m.ChoicePoint.Args)
	m.resetMode()
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
	case putStruct:
		// Place flattened struct (in post order) during query building.
		f := makeStructFrom(instr.Functor)
		m.Reg[instr.ArgAddr] = f
		m.setMode(Write, f)
	case putVariable:
		// Place newly-seen query argument as an unbound ref during query building.
		x := m.newRef()
		m.Reg[instr.ArgAddr] = x
		m.set(instr.Addr, x)
	case putValue:
		// Move already-seen query argument from register to arg register.
		m.Reg[instr.ArgAddr] = m.get(instr.Addr)
	case putConstant:
		// Put constant as argument in register.
		m.Reg[instr.ArgAddr] = instr.Constant
	case putPair:
		// Put pair as argument in register.
		l := &Pair{Tag: instr.Tag}
		m.Reg[instr.ArgAddr] = l
		m.setMode(Write, l)
	case getStruct:
		// Get flattened struct (in pre order) from register.
		// If already a literal struct, will read another struct from the heap during unification.
		// If a ref, will build the struct on the heap instead. In this case, it's necessary to
		// bind the register address with the newly created heap address.
		cell := deref(m.get(instr.ArgAddr))
		switch c := cell.(type) {
		case *Struct:
			if f := c.Functor(); f != instr.Functor {
				return m.backtrack(&unifyError{f, instr.Functor})
			}
			m.setMode(Read, c)
		case *Ref:
			f := makeStructFrom(instr.Functor)
			m.bind(c, f)
			m.setMode(Write, f)
		default:
			return m.backtrack(&unifyError{cell, instr.Functor})
		}
	case getVariable:
		// Move newly-seen clause param from arg register to register.
		m.set(instr.Addr, m.Reg[instr.ArgAddr])
	case getValue:
		// Unify already-seen clause param with register value.
		return m.preUnify(m.get(instr.Addr), m.get(instr.ArgAddr))
	case getConstant:
		// Expect a constant from register.
		return m.readConstant(instr.Constant, m.get(instr.ArgAddr))
	case getPair:
		// Expect a pair from register.
		cell := deref(m.get(instr.ArgAddr))
		switch c := cell.(type) {
		case *Pair:
			if c.Tag != instr.Tag {
				return m.backtrack(&unifyError{cell, &Pair{Tag: instr.Tag}})
			}
			m.setMode(Read, c)
		case *Ref:
			l := &Pair{Tag: instr.Tag}
			m.bind(c, l)
			m.setMode(Write, l)
		default:
			return m.backtrack(&unifyError{cell, &Pair{Tag: instr.Tag}})
		}
	case unifyVariable:
		return m.unifyArg(instr)
	case unifyValue:
		return m.unifyArg(instr)
	case unifyConstant:
		return m.unifyArg(instr)
	case unifyVoid:
		return m.unifyArg(instr)
	case call:
		// Save instruction pointer, and set it to clause location.
		m.Continuation = m.CodePtr.inc()
		m.CutChoice = m.ChoicePoint
		return m.call(instr.Functor)
	case callMeta:
		// call clause pointed by a ref or struct.
		functor, err := m.putMeta(instr.Addr, instr.Params)
		if err != nil {
			return m.backtrack(fmt.Errorf("call_meta: %v", err))
		}
		m.Continuation = m.CodePtr.inc()
		m.CutChoice = m.ChoicePoint
		return m.call(functor)
	case execute:
		// Trampoline into other clause, without changing the continuation.
		m.CutChoice = m.ChoicePoint
		return m.call(instr.Functor)
	case executeMeta:
		// Trampoline into other dynamic clause, without changing the continuation.
		functor, err := m.putMeta(instr.Addr, instr.Params)
		if err != nil {
			return m.backtrack(fmt.Errorf("execute_meta: %v", err))
		}
		m.CutChoice = m.ChoicePoint
		return m.call(functor)
	case proceed:
		// Jump to the continuation.
		m.Mode = instr.Mode
		nextInstr := m.Continuation
		m.Continuation.Clause = nil
		return nextInstr, nil
	case allocate:
		// allocate a new stack frame.
		env := &Env{
			Prev:          m.Env,
			Continuation:  m.Continuation,
			PermanentVars: make([]Cell, instr.NumVars),
			CutChoice:     m.CutChoice,
		}
		m.Continuation.Clause = nil
		m.Env = env
	case deallocate:
		// Pop the current environment. It may still be in memory if a choice point references it.
		m.Continuation = m.Env.Continuation
		m.Env = m.Env.Prev
	case tryMeElse:
		// Create a choice point, saving current machine state and pointing to next possible clause.
		m.ChoicePoint = m.newChoicePoint(instr.Alternative)
	case retryMeElse:
		// Reset the machine state to latest choice point, and point to the next possible clause.
		m.restoreFromChoicePoint()
		m.ChoicePoint.NextAlternative = instr.Alternative
	case trustMe:
		// Reset the machine state to latest choice point, and "deallocate" current choice point.
		m.restoreFromChoicePoint()
		m.ChoicePoint = m.ChoicePoint.Prev
	case try:
		// Create a choice point, saving current machine state and pointing to next instruction.
		// Jump execution to the instruction continuation.
		m.ChoicePoint = m.newChoicePoint(m.CodePtr.inc())
		return instr.Continuation, nil
	case retry:
		// Reset the machine state to latest choice point, and point to the next instruction.
		// Jump execution to the instruction continuation.
		m.restoreFromChoicePoint()
		m.ChoicePoint.NextAlternative = m.CodePtr.inc()
		return instr.Continuation, nil
	case trust:
		// Reset the machine state to latest choice point, and "deallocate" current choice point.
		// Jump execution to the instruction continuation.
		m.restoreFromChoicePoint()
		m.ChoicePoint = m.ChoicePoint.Prev
		return instr.Continuation, nil
	case label:
		// Do nothing, simply forwards to next instruction.
	case jump:
		// Jump unconditionally to instruction.
		return instr.Continuation, nil
	case switchOnTerm:
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
			default:
				panic(fmt.Sprintf("switch_on_term: pair: unhandled pair type %T (%v)", c.Tag, c))
			}
		default:
			panic(fmt.Sprintf("switch_on_term: unhandled type %T (%v)", cell, cell))
		}
	case switchOnConstant:
		// Jump to instructions matching the first arg constant.
		cell := deref(m.Reg[0]).(Constant)
		cont, ok := instr.Continuation[cell]
		if !ok {
			return m.backtrack(fmt.Errorf("constant index not found: %v", cell))
		}
		return cont, nil
	case switchOnStruct:
		// Jump to instructions matching the first arg functor.
		cell := deref(m.Reg[0]).(*Struct)
		cont, ok := instr.Continuation[cell.Functor()]
		if !ok {
			return m.backtrack(fmt.Errorf("functor index not found: %v", cell.Functor()))
		}
		return cont, nil
	case neckCut:
		// Remove any choicepoint created since the function call, removing choicepoints
		// due to indexing.
		if m.ChoicePoint == m.CutChoice {
			break
		}
		m.ChoicePoint = m.CutChoice
		m.tidyTrail()
	case cut:
		// Remove any choicepoint created since the function initial execution, keeping
		// choicepoints due to indexing.
		if m.Env.CutChoice == m.ChoicePoint {
			break
		}
		m.ChoicePoint = m.Env.CutChoice
		m.tidyTrail()
	case fail:
		// fail unconditionally.
		return m.backtrack(fmt.Errorf("fail instruction"))
	case builtin:
		// calls builtin function.
		if err := instr.Func(m, instr.Args); err != nil {
			return m.backtrack(err)
		}
	case putAttr:
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
	case getAttr:
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
			if attr == nil {
				return m.backtrack(fmt.Errorf("get_attr: attribute not present"))
			}
		default:
			return m.backtrack(fmt.Errorf("get_attr: invalid attribute"))
		}
		return m.preUnify(attr, cell)
	case inlineUnify:
		// Unify two arbitrary addresses.
		return m.preUnify(m.get(instr.Addr1), m.get(instr.Addr2))
	default:
		panic(fmt.Sprintf("execute: unhandled instruction type %T (%v)", instr, instr))
	}
	return m.forward()
}

func (m *Machine) forward() (InstrAddr, error) {
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
func (m *Machine) bind(c1, c2 Cell) {
	m.bindRef(bindOrder(c1, c2))
}

func (m *Machine) bindRef(x *Ref, cell Cell) {
	x.Cell = cell
	m.trail(x)
}

func bindOrder(c1, c2 Cell) (*Ref, Cell) {
	ref1, isRef1 := c1.(*Ref)
	ref2, isRef2 := c2.(*Ref)
	// Safety measure: always bind newer variables (larger id) to older
	// variables (smaller id).
	// This is "WAM Binding Rule 1", but shouldn't be necessary in our
	// impl, because we don't care if a stack variable references a "heap"
	// one, as there's no heap to manage.
	// Still, establishing an order may prevent reference loops in Refs,
	// and is a very cheap check.
	if isRef1 && ref1.Cell == nil && (!isRef2 || ref2.id < ref1.id) {
		return ref1, c2
	}
	if isRef2 && ref2.Cell == nil {
		return ref2, c1
	}
	panic(fmt.Sprintf("bind(%v, %v): no unbound refs", c1, c2))
}

// ---- unification

func bindingsToSlice(bs map[*Ref]Cell) []Binding {
	bindings := make([]Binding, len(bs))
	i := 0
	for x, value := range bs {
		bindings[i] = Binding{x, value}
		i++
	}
	// Sort bindings by var age (older to newer).
	sort.Slice(bindings, func(i, j int) bool {
		return compareCells(bindings[i].Ref, bindings[j].Ref) == less
	})
	return bindings
}

func (m *Machine) preUnify(a1, a2 Cell) (InstrAddr, error) {
	bindings, hasAttr, err := m.unifyBindings(a1, a2)
	if err != nil {
		return m.backtrack(err)
	}
	// Early exit if there's no attributes to check.
	if !hasAttr {
		return m.forward()
	}
	// Undo bindings.
	for x := range bindings {
		x.Cell = nil
	}
	return m.setupVerifyAttributes(bindings)
}

func (m *Machine) setupVerifyAttributes(bindings map[*Ref]Cell) (InstrAddr, error) {
	m.Mode = Unify
	m.UnificationFrame = &UnificationFrame{
		Prev:         m.UnificationFrame,
		Continuation: m.Continuation,
		CutChoice:    m.CutChoice,
		Bindings:     bindingsToSlice(bindings),
		FirstRun:     true,
	}
	return m.forward()
}

type unifyError struct {
	c1, c2 interface{}
}

func (err *unifyError) Error() string {
	return fmt.Sprintf("%v != %v", err.c1, err.c2)
}

type unifyCtx struct {
	stack    []Cell
	bindings map[*Ref]Cell
	anyAttr  bool
}

// unify executes a depth-first traversal of cells, binding unbound refs to the other
// cell, or comparing them for equality.
func (m *Machine) unifyBindings(a1, a2 Cell) (map[*Ref]Cell, bool, error) {
	ctx := &unifyCtx{
		stack:    []Cell{a1, a2},
		bindings: make(map[*Ref]Cell),
		anyAttr:  false,
	}
	// Collect bindings.
	for len(ctx.stack) > 0 {
		// Pop address pair from stack.
		n := len(ctx.stack)
		a1, a2 := ctx.stack[n-2], ctx.stack[n-1]
		ctx.stack = ctx.stack[:n-2]
		if err := m.unifyStep(ctx, a1, a2); err != nil {
			return nil, false, err
		}
	}
	return ctx.bindings, ctx.anyAttr, nil
}

func (m *Machine) unifyStep(ctx *unifyCtx, a1, a2 Cell) error {
	// Deref cells and compare them.
	c1, c2 := deref(a1), deref(a2)
	if c1 == c2 {
		// They are the same, nothing to do.
		return nil
	}
	_, isRef1 := c1.(*Ref)
	_, isRef2 := c2.(*Ref)
	if isRef1 || isRef2 {
		// Some of them is a ref. Bind them.
		x, cell := bindOrder(c1, c2)
		m.bindRef(x, cell)
		ctx.bindings[x] = cell
		if _, ok := m.attributes[x.id]; ok {
			ctx.anyAttr = true
		}
		return nil
	}
	// Special case: '{}' unifies with any dict.
	if c1 == WAtom("{}") || c2 == WAtom("{}") {
		p1, isPair1 := c1.(*Pair)
		p2, isPair2 := c2.(*Pair)
		if c1 == WAtom("{}") && isPair2 && p2.Tag == DictPair {
			return nil
		}
		if c2 == WAtom("{}") && isPair1 && p1.Tag == DictPair {
			return nil
		}
	}
	switch t1 := c1.(type) {
	case Constant:
		// If they are both constants, check that they are equal.
		t2, ok := c2.(Constant)
		if !(ok && t1 == t2) {
			return &unifyError{c1, c2}
		}
		return nil
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
			ctx.stack = append(ctx.stack, t1.Args[i], t2.Args[i])
		}
		return nil
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
			ctx.stack = append(ctx.stack, pairs...)
		} else {
			// Assocs, Lists: compare heads and tails.
			ctx.stack = append(ctx.stack, t1.Tail, t2.Tail, t1.Head, t2.Head)
		}
		return nil
	default:
		panic(fmt.Sprintf("wam.Machine.unify: unhandled type %T (%v)", c1, c1))
	}
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

func attrName(attr Cell) string {
	switch c := attr.(type) {
	case *Struct:
		return c.Name
	default:
		panic(fmt.Sprintf("unhandled attribute type %T (%v)", attr, attr))
	}
}

func attrsToSlice(as map[string]Cell) []Cell {
	attrs := make([]Cell, len(as))
	i := 0
	for _, attr := range as {
		attrs[i] = attr
		i++
	}
	sort.Slice(attrs, func(i, j int) bool {
		return attrName(attrs[i]) < attrName(attrs[j])
	})
	return attrs
}

func (m *Machine) verifyAttributes() {
	frame := m.UnificationFrame
	if len(frame.Attributes) == 0 {
		if !frame.FirstRun {
			frame.Index++
		}
		// Look for attributed ref in remaining bindings.
		for ; frame.Index < len(frame.Bindings); frame.Index++ {
			binding := frame.Bindings[frame.Index]
			if attrs, ok := m.attributes[binding.Ref.id]; ok {
				frame.Attributes = attrsToSlice(attrs)
				break
			}
		}
		if frame.Index >= len(frame.Bindings) {
			// Bindings has been exhausted: restore bindings and return to run code.
			for _, binding := range frame.Bindings {
				ref, cell := binding.Ref, binding.Value
				m.deleteAttributes(ref)
				m.bindRef(ref, cell)
			}
			m.Mode = Run
			m.Continuation = frame.Continuation
			m.CutChoice = frame.CutChoice
			m.UnificationFrame = frame.Prev
			return
		}
	}
	// Pop first attribute.
	attr := frame.Attributes[0]
	frame.Attributes = frame.Attributes[1:]
	frame.FirstRun = false
	// Prepare machine
	binding := frame.Bindings[frame.Index]
	ref, value := binding.Ref, binding.Value
	var functor Functor
	if otherRef, ok := value.(*Ref); ok {
		// If value is a var, call join_attribute(AttrName, X, Y)
		m.Reg[0] = WAtom(attrName(attr))
		m.Reg[1] = ref
		m.Reg[2] = otherRef
		functor = Functor{"$join_attribute", 3}
	} else {
		// If value is not a var, call check_attribute(Attr, Value)
		m.Reg[0] = attr
		m.Reg[1] = value
		functor = Functor{"$check_attribute", 2}
	}
	m.Continuation = m.CodePtr
	m.CutChoice = m.ChoicePoint
	instrAddr, err := m.call(functor)
	if err != nil {
		// Should never happen.
		panic(err)
	}
	m.CodePtr = instrAddr
	m.Mode = Run
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
