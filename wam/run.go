package wam

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math"
	"os"

	"github.com/brunokim/logic-engine/logic"
)

func (m *Machine) RunQuery(query ...logic.Term) (map[logic.Var]logic.Term, error) {
	pos := make(map[logic.Var]int)
	var xs []logic.Var
	for _, c := range query {
		for _, x := range logic.Vars(c) {
			if _, ok := pos[x]; ok {
				continue
			}
			pos[x] = len(xs)
			xs = append(xs, x)
		}
	}
	c, err := compileQuery(query)
	if err != nil {
		return nil, err
	}
	m.AddClause(c)
	if err := m.Run(); err != nil {
		return nil, err
	}
	if m.Env == nil {
		return nil, fmt.Errorf("RunQuery: nil env at the end of execution")
	}
	bindings := make(map[logic.Var]logic.Term)
	for i, term := range fromCells(m.Env.PermanentVars) {
		bindings[xs[i]] = term
	}
	return bindings, nil
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

func (m *Machine) getCompoundArg() Cell {
	switch c := m.Compound.(type) {
	case *Struct:
		return c.Args[m.ArgIndex]
	case *List:
		switch m.ArgIndex {
		case 0:
			return c.Head
		case 1:
			return c.Tail
		default:
			panic(fmt.Sprintf("invalid ArgIndex for List: %d", m.ArgIndex))
		}
	default:
		panic(fmt.Sprintf("unhandled compound type: %T (%v)", m.Compound, m.Compound))
	}
}

func (m *Machine) setCompoundArg(cell Cell) {
	switch c := m.Compound.(type) {
	case *Struct:
		c.Args[m.ArgIndex] = cell
	case *List:
		switch m.ArgIndex {
		case 0:
			c.Head = cell
		case 1:
			c.Tail = cell
		default:
			panic(fmt.Sprintf("invalid ArgIndex for List: %d", m.ArgIndex))
		}
	default:
		panic(fmt.Sprintf("unhandled compound type: %T (%v)", m.Compound, m.Compound))
	}
}

func (m *Machine) newRef() *Ref {
	x := &Ref{nil, m.LastRefID}
	m.LastRefID++
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
		Args:            make([]Cell, numArgs),
		TrailSize:       len(m.Trail),
		LastRefID:       m.LastRefID,
		Env:             m.Env,
		CutChoice:       m.CutChoice,
	}
	copy(choicePoint.Args, m.Reg)
	return choicePoint
}

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
	case *Constant:
		name, args = c.Value, nil
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
		m.Reg[instr.ArgAddr] = f
		m.Compound = f
		m.ArgIndex = 0
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
	case PutList:
		// Put list as argument in register.
		l := &List{}
		m.Reg[instr.ArgAddr] = l
		m.Compound = l
		m.ArgIndex = 0
	case GetStruct:
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
			m.Compound = c
			m.ArgIndex = 0
			m.Mode = Read
		case *Ref:
			f := makeStructFrom(instr.Functor)
			m.Compound = f
			m.ArgIndex = 0
			m.bind(c, f)
			m.Mode = Write
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
		cell := deref(m.get(instr.ArgAddr))
		switch c := cell.(type) {
		case *Constant:
			if c.Value != instr.Constant.Value {
				return m.backtrack(&unifyError{c, instr.Constant})
			}
		case *Ref:
			c.Cell = instr.Constant
			m.trail(c)
		default:
			return m.backtrack(&unifyError{cell, instr.Constant})
		}
	case GetList:
		// Expect a list from register.
		cell := deref(m.get(instr.ArgAddr))
		switch c := cell.(type) {
		case *List:
			m.Compound = c
			m.ArgIndex = 0
			m.Mode = Read
		case *Ref:
			l := &List{}
			m.Compound = l
			m.ArgIndex = 0
			m.bind(c, l)
			m.Mode = Write
		default:
			return m.backtrack(&unifyError{cell, &List{}})
		}
	case SetVariable:
		// Place new unbound ref during query building.
		x := m.newRef()
		m.setCompoundArg(x)
		m.set(instr.Addr, x)
		m.ArgIndex++
	case SetValue:
		// Copy already-seen cell from register to the heap during query building.
		m.setCompoundArg(m.get(instr.Addr))
		m.ArgIndex++
	case SetConstant:
		// Push a constant to the current struct arg.
		m.setCompoundArg(instr.Constant)
		m.ArgIndex++
	case SetVoid:
		// Push N unbound variables to the current struct arg.
		for i := 0; i < instr.NumVars; i++ {
			m.setCompoundArg(m.newRef())
			m.ArgIndex++
		}
	case UnifyVariable:
		// Unify newly-seen struct cell.
		// In read mode, place current struct ptr cell into register.
		// In write mode, place unbound ref cell in the heap.
		switch m.Mode {
		case Read:
			m.set(instr.Addr, m.getCompoundArg())
		case Write:
			x := m.newRef()
			m.setCompoundArg(x)
			m.set(instr.Addr, x)
		}
		m.ArgIndex++
	case UnifyValue:
		// Unify already-seen struct cell.
		// In read mode, unify this address with struct ptr.
		// In write mode, move the cell from the register to the struct ptr.
		cell := m.get(instr.Addr)
		switch m.Mode {
		case Read:
			if err := m.unify(cell, m.getCompoundArg()); err != nil {
				return m.backtrack(err)
			}
		case Write:
			m.setCompoundArg(cell)
		}
		m.ArgIndex++
	case UnifyConstant:
		// Unify already-seen constant.
		// In read mode, unify this address with struct ptr.
		// In write mode, copy the constant to the struct ptr.
		switch m.Mode {
		case Read:
			cell := deref(m.getCompoundArg())
			switch c := cell.(type) {
			case *Constant:
				if c.Value != instr.Constant.Value {
					return m.backtrack(&unifyError{c, instr.Constant})
				}
			case *Ref:
				c.Cell = instr.Constant
				m.trail(c)
			default:
				return m.backtrack(&unifyError{cell, instr.Constant})
			}
		case Write:
			m.setCompoundArg(instr.Constant)
		}
		m.ArgIndex++
	case UnifyVoid:
		// Unify arg with unreferenced vars...
		switch m.Mode {
		case Read:
			m.ArgIndex += instr.NumVars
		case Write:
			for i := 0; i < instr.NumVars; i++ {
				m.setCompoundArg(m.newRef())
				m.ArgIndex++
			}
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
		cell := deref(m.Reg[0])
		switch cell.(type) {
		case *Ref:
			return instr.IfVar, nil
		case *Constant:
			return instr.IfConstant, nil
		case *List:
			return instr.IfList, nil
		case *Struct:
			return instr.IfStruct, nil
		default:
			panic(fmt.Sprintf("switch_on_term: unhandled type %T (%v)", cell, cell))
		}
	case SwitchOnConstant:
		cell := deref(m.Reg[0]).(*Constant)
		cont, ok := instr.Continuation[cell.Value]
		if !ok {
			return m.backtrack(fmt.Errorf("constant index not found: %v", cell))
		}
		return cont, nil
	case SwitchOnStruct:
		cell := deref(m.Reg[0]).(*Struct)
		cont, ok := instr.Continuation[cell.Functor()]
		if !ok {
			return m.backtrack(fmt.Errorf("functor index not found: %v", cell.Functor()))
		}
		return cont, nil
	case NeckCut:
		if m.ChoicePoint == m.CutChoice {
			break
		}
		m.ChoicePoint = m.CutChoice
		m.tidyTrail()
	case Cut:
		if m.Env.CutChoice == m.ChoicePoint {
			break
		}
		m.ChoicePoint = m.Env.CutChoice
		m.tidyTrail()
	case Fail:
		return m.backtrack(fmt.Errorf("fail instruction"))
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

// deref walks the reference chain until if finds a non-ref cell, or an unbound ref.
func deref(cell Cell) Cell {
	ref, ok := cell.(*Ref)
	for ok && ref.Cell != nil {
		cell = ref.Cell
		ref, ok = cell.(*Ref)
	}
	return cell
}

// bind must be called with at least one unbound ref.
func (m *Machine) bind(c1, c2 Cell) {
	if ref1, ok := c1.(*Ref); ok && ref1.Cell == nil {
		ref1.Cell = c2
		m.trail(ref1)
		return
	}
	if ref2, ok := c2.(*Ref); ok && ref2.Cell == nil {
		ref2.Cell = c1
		m.trail(ref2)
		return
	}
	panic(fmt.Sprintf("bind(%v, %v): no unbound refs", c1, c2))
}

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
			// 1. They are the same, nothing to do.
			continue
		}
		_, isRef1 := c1.(*Ref)
		_, isRef2 := c2.(*Ref)
		if isRef1 || isRef2 {
			// 2. Some of them is a ref. Bind them.
			m.bind(c1, c2)
			continue
		}
		switch t1 := c1.(type) {
		case *Constant:
			// 3. If they are both constants, check that they are equal.
			t2, ok := c2.(*Constant)
			if !(ok && t1.Value == t2.Value) {
				return &unifyError{c1, c2}
			}
		case *Struct:
			// 4. Check if they are both struct cells.
			t2, ok := c2.(*Struct)
			if !ok {
				return &unifyError{c1, c2}
			}
			// 5. Get the functors being pointed by the structs.
			f1, f2 := t1.Functor(), t2.Functor()
			if f1 != f2 {
				return &unifyError{f1, f2}
			}
			// 6. Push addresses of args pair-wise onto stack.
			for i := 0; i < f1.Arity; i++ {
				stack = append(stack, t1.Args[i], t2.Args[i])
			}
		case *List:
			t2, ok := c2.(*List)
			if !ok {
				return &unifyError{c1, c2}
			}
			stack = append(stack, t1.Head, t2.Head, t1.Tail, t2.Tail)
		default:
			panic(fmt.Sprintf("wam.Machine.unify: unhandled type %T (%v)", c1, c1))
		}
	}
	return nil
}

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
	m.Trail = append(m.Trail, ref)
}

// Restore all conditional bindings since current choice point back to
// nil, and reset trail.
func (m *Machine) unwindTrail() {
	if m.ChoicePoint == nil {
		return
	}
	n := m.ChoicePoint.TrailSize
	for _, ref := range m.Trail[n:] {
		ref.Cell = nil
	}
	m.Trail = m.Trail[:n]
	m.LastRefID = m.ChoicePoint.LastRefID
}

// Remove all refs that are no longer conditional after a cut.
func (m *Machine) tidyTrail() {
	if m.ChoicePoint == nil {
		m.Trail = nil
		return
	}
	i := m.ChoicePoint.TrailSize
	for i < len(m.Trail) {
		ref := m.Trail[i]
		// Still a conditional ref, keep it in the trail.
		if m.isConditional(ref) {
			i++
			continue
		}
		// Pop the last ref and overwrite the i-th position with it.
		n := len(m.Trail)
		m.Trail[i], m.Trail = m.Trail[n-1], m.Trail[:n-1]
	}
}
