package wam

import (
	"fmt"
	"sort"

	"github.com/brunokim/logic-engine/errors"
)

func listPair(head, tail Cell) *Pair {
	return &Pair{Tag: ListPair, Head: head, Tail: tail}
}

func assocPair(key, val Cell) *Pair {
	return &Pair{Tag: AssocPair, Head: key, Tail: val}
}

func dictPair(key, val, parent Cell) *Pair {
	return &Pair{Tag: DictPair, Head: assocPair(key, val), Tail: parent}
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

// isGround returns whether cell and its component cells are ground, that is, there's
// no unbound reference within.
func isGround(cell Cell) bool {
	stack := []Cell{cell}
	seen := make(map[Cell]struct{})
	for len(stack) > 0 {
		n := len(stack)
		cell := deref(stack[n-1])
		stack = stack[:n-1]
		// Check for reference loop.
		if _, ok := seen[cell]; ok {
			continue
		}
		seen[cell] = struct{}{}
		// If cell is complex, append its components to the stack.
		switch c := cell.(type) {
		case Constant:
			continue
		case *Ref:
			return false
		case *Struct:
			stack = append(stack, c.Args...)
		case *Pair:
			stack = append(stack, c.Head, c.Tail)
		default:
			panic(fmt.Sprintf("isGround: unhandled type %T (%v)", cell, cell))
		}
	}
	return true
}

// unroll returns all cells that compose a linked-list object, and its tail.
func unroll(p *Pair) ([]Cell, Cell) {
	if p.Tag == AssocPair {
		return nil, p
	}
	var elems []Cell
	var head, tail Cell
	isPair := true
	seen := make(map[Cell]struct{})
	tag := p.Tag
	for isPair && p.Tag == tag {
		if _, ok := seen[p]; ok {
			break
		}
		seen[p] = struct{}{}
		head, tail = deref(p.Head), deref(p.Tail)
		elems = append(elems, head)
		p, isPair = tail.(*Pair)
	}
	return elems, tail
}

func searchAssoc(assocs []*Pair, key Cell) (int, bool) {
	i := sort.Search(len(assocs), func(i int) bool {
		return compareCells(assocs[i].Head, key) != less
	})
	return i, i < len(assocs) && compareCells(assocs[i].Head, key) == equal
}

func insertAssoc(assocs []*Pair, assoc *Pair) []*Pair {
	i, ok := searchAssoc(assocs, assoc.Head)
	if ok {
		// Do not overwrite already-present value.
		return assocs
	}
	as := make([]*Pair, len(assocs)+1)
	copy(as, assocs[:i])
	as[i] = assoc
	copy(as[i+1:], assocs[i:])
	return as
}

type match struct {
	key, left, right Cell
}

// Returns the keys that are shared among both assocs, as well as the assocs
// in each one whose key is not present in the other.
func assocsDifference(assocs1, assocs2 []*Pair) ([]match, []*Pair, []*Pair) {
	var matching []match
	var diff1, diff2 []*Pair
	var i, j int
	for i < len(assocs1) && j < len(assocs2) {
		assoc1, assoc2 := assocs1[i], assocs2[j]
		switch compareCells(assoc1.Head, assoc2.Head) {
		case equal:
			matching = append(matching, match{assoc1.Head, assoc1.Tail, assoc2.Tail})
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
	return matching, diff1, diff2
}

// unrollDict returns all assoc Pairs that compose a dict, and its parent.
func unrollDict(d *Pair) ([]*Pair, Cell, error) {
	if d.Tag != DictPair {
		return nil, nil, errors.New("unrollDict: not a dict: %v", d)
	}
	elems, parent := unroll(d)
	var assocs []*Pair
	for _, elem := range elems {
		pair, ok := elem.(*Pair)
		if !(ok && pair.Tag == AssocPair) {
			return nil, nil, errors.New("non-assoc content in dict: %v", elem)
		}
		if !isGround(pair.Head) {
			return nil, nil, errors.New("non-ground key in dict: %v", pair.Head)
		}
		assocs = insertAssoc(assocs, pair)
	}
	return assocs, parent, nil
}

// rollDict creates a new dict Pair from a (sorted) list of assocs.
func rollDict(assocs []*Pair, parent Cell) Cell {
	d := parent
	for i := len(assocs) - 1; i >= 0; i-- {
		d = &Pair{Tag: DictPair, Head: assocs[i], Tail: d}
	}
	return d
}

func instructionPointers(instr Instruction) []InstrAddr {
	switch instr := instr.(type) {
	case tryMeElse:
		return []InstrAddr{instr.Alternative}
	case retryMeElse:
		return []InstrAddr{instr.Alternative}
	case try:
		return []InstrAddr{instr.Continuation}
	case retry:
		return []InstrAddr{instr.Continuation}
	case trust:
		return []InstrAddr{instr.Continuation}
	case jump:
		return []InstrAddr{instr.Continuation}
	case switchOnTerm:
		return []InstrAddr{
			instr.IfVar,
			instr.IfConstant,
			instr.IfStruct,
			instr.IfList,
			instr.IfAssoc,
			instr.IfDict,
		}
	case switchOnConstant:
		addrs := make([]InstrAddr, len(instr.Continuation))
		i := 0
		for _, instrAddr := range instr.Continuation {
			addrs[i] = instrAddr
			i++
		}
		return addrs
	case switchOnStruct:
		addrs := make([]InstrAddr, len(instr.Continuation))
		i := 0
		for _, instrAddr := range instr.Continuation {
			addrs[i] = instrAddr
			i++
		}
		return addrs
	}
	return nil
}

func replaceInstructionPointer(instr Instruction, f func(InstrAddr) InstrAddr) Instruction {
	switch instr := instr.(type) {
	case tryMeElse:
		return tryMeElse{f(instr.Alternative)}
	case retryMeElse:
		return retryMeElse{f(instr.Alternative)}
	case try:
		return try{f(instr.Continuation)}
	case retry:
		return retry{f(instr.Continuation)}
	case trust:
		return trust{f(instr.Continuation)}
	case jump:
		return jump{f(instr.Continuation)}
	case switchOnTerm:
		return switchOnTerm{
			IfVar:      f(instr.IfVar),
			IfConstant: f(instr.IfConstant),
			IfStruct:   f(instr.IfStruct),
			IfList:     f(instr.IfList),
			IfAssoc:    f(instr.IfAssoc),
			IfDict:     f(instr.IfDict),
		}
	case switchOnConstant:
		m := make(map[Constant]InstrAddr)
		for c, instrAddr := range instr.Continuation {
			m[c] = f(instrAddr)
		}
		return switchOnConstant{m}
	case switchOnStruct:
		m := make(map[Functor]InstrAddr)
		for functor, instrAddr := range instr.Continuation {
			m[functor] = f(instrAddr)
		}
		return switchOnStruct{m}
	}
	return instr
}
