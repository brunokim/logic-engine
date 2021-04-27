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
	if m.Env == nil {
		return nil, fmt.Errorf("RunQuery: nil env at the end of execution")
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
	case *Pair:
		switch m.ArgIndex {
		case 0:
			return c.Head
		case 1:
			return c.Tail
		default:
			panic(fmt.Sprintf("invalid ArgIndex for Pair: %d", m.ArgIndex))
		}
	default:
		panic(fmt.Sprintf("unhandled compound type: %T (%v)", m.Compound, m.Compound))
	}
}

func (m *Machine) setCompoundArg(cell Cell) {
	switch c := m.Compound.(type) {
	case *Struct:
		c.Args[m.ArgIndex] = cell
	case *Pair:
		switch m.ArgIndex {
		case 0:
			c.Head = cell
		case 1:
			c.Tail = cell
		default:
			panic(fmt.Sprintf("invalid ArgIndex for Pair: %d", m.ArgIndex))
		}
	default:
		panic(fmt.Sprintf("unhandled compound type: %T (%v)", m.Compound, m.Compound))
	}
}

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
	case PutPair:
		// Put pair as argument in register.
		l := &Pair{Tag: instr.Tag}
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
		case Constant:
			if c != instr.Constant {
				return m.backtrack(&unifyError{c, instr.Constant})
			}
		case *Ref:
			c.Cell = instr.Constant
			m.trail(c)
		default:
			return m.backtrack(&unifyError{cell, instr.Constant})
		}
	case GetPair:
		// Expect a pair from register.
		cell := deref(m.get(instr.ArgAddr))
		switch c := cell.(type) {
		case *Pair:
			if c.Tag != instr.Tag {
				return m.backtrack(&unifyError{cell, &Pair{Tag: instr.Tag}})
			}
			m.Compound = c
			m.ArgIndex = 0
			m.Mode = Read
		case *Ref:
			l := &Pair{Tag: instr.Tag}
			m.Compound = l
			m.ArgIndex = 0
			m.bind(c, l)
			m.Mode = Write
		default:
			return m.backtrack(&unifyError{cell, &Pair{Tag: instr.Tag}})
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
			case Constant:
				if c != instr.Constant {
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
		case Constant:
			return instr.IfConstant, nil
		case *Pair:
			return instr.IfPair, nil
		case *Struct:
			return instr.IfStruct, nil
		default:
			panic(fmt.Sprintf("switch_on_term: unhandled type %T (%v)", cell, cell))
		}
	case SwitchOnConstant:
		cell := deref(m.Reg[0]).(Constant)
		cont, ok := instr.Continuation[cell]
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

// bind must be called with at least one unbound ref.
func (m *Machine) bind(c1, c2 Cell) {
	ref1, isRef1 := c1.(*Ref)
	ref2, isRef2 := c2.(*Ref)
	// Safety measure: always bind newer variables to older variables.
	// This is "WAM Binding Rule 1", but shouldn't be necessary in our
	// impl, because we don't care if a stack variable references a "heap"
	// one, as there's no heap to manage.
	// Still, establishing an order may prevent reference loops in Refs,
	// and is a very cheap check.
	if isRef1 && ref1.Cell == nil && (!isRef2 || ref2.id < ref1.id) {
		ref1.Cell = c2
		m.trail(ref1)
		return
	}
	if isRef2 && ref2.Cell == nil {
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
		case Constant:
			// 3. If they are both constants, check that they are equal.
			t2, ok := c2.(Constant)
			if !(ok && t1 == t2) {
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
		case *Pair:
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
				stack = append(stack, t1.Head, t2.Head, t1.Tail, t2.Tail)
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
	var cells []Cell
	var diff1, diff2 []*Pair
	var i, j int
	for i < len(assocs1) && j < len(assocs2) {
		assoc1, assoc2 := assocs1[i], assocs2[j]
		switch compareCells(assoc1.Head, assoc2.Head) {
		case equal:
			cells = append(cells, assoc1.Tail, assoc2.Tail)
			i++
			j++
		case less:
			diff1 = append(diff1, assoc1)
			i++
		case more:
			diff2 = append(diff2, assoc2)
			j++
		}
	}
	diff1 = append(diff1, assocs1[i:]...)
	diff2 = append(diff2, assocs2[j:]...)
	// {a:A, b:B|D1} = {a:X|D2}  =>  A=X, D2={b:B|D1}
	if len(diff1) > 0 && len(diff2) == 0 {
		cells = append(cells, rollDict(diff1, parent1), parent2)
	}
	// {a:A|D1} = {a:X, z:Z|D2}  =>  A=X, D1={z:Z|D2}
	if len(diff2) > 0 && len(diff1) == 0 {
		cells = append(cells, rollDict(diff2, parent2), parent1)
	}
	// {a:A, b:B|D1} = {a:X, z:Z|D2}  =>  A=X, D1={z:Z|D}, D2={b:B|D}
	if len(diff1) > 0 && len(diff2) > 0 {
		d := m.newRef()
		cells = append(cells,
			rollDict(diff1, d), parent2,
			rollDict(diff2, d), parent1)
	}
	return cells, nil
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
