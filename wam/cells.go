package wam

import (
	"fmt"
)

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

// unrollDict returns all assoc Pairs that compose a dict, and its parent if present.
func unrollDict(d *Pair) ([]*Pair, Cell, error) {
	if d.Tag != DictPair {
		return nil, nil, fmt.Errorf("unrollDict: not a dict: %v", d)
	}
	var assocs []*Pair
	var head, tail Cell
	isPair := true
	for isPair && d.Tag == DictPair {
		head, tail = deref(d.Head), deref(d.Tail)
		pair, ok := head.(*Pair)
		if !(ok && pair.Tag == AssocPair) {
			return nil, nil, fmt.Errorf("non-assoc content in dict: %v", head)
		}
		if !isGround(pair.Head) {
			return nil, nil, fmt.Errorf("non-ground key in dict: %v", pair.Head)
		}
		assocs = append(assocs, pair)
		d, isPair = tail.(*Pair)
	}
	return assocs, tail, nil
}

// rollDict creates a new dict Pair from the provided assoc list.
func rollDict(assocs []*Pair, parent Cell) Cell {
	d := parent
	for i := len(assocs) - 1; i >= 0; i-- {
		d = &Pair{Tag: DictPair, Head: assocs[i], Tail: d}
	}
	return d
}

// Walks the list of (sorted) assocs from each dict, trying to match their keys.
// Assocs whose key are not present in the other are matched with the other dict's parent.
//
//   {a:1, b:2} = {a:X, b:Y}  => X=1, Y=2
//   {a:1|D1} = {a:X, b:2}    => X=1, D1={b:2}
//   {a:1|D1} = {a:X, b:2|D2} => X=1, D1={b:2|D2}
//   {a:1|D1} = {b:2|D2}      => D1={b:2|D2}, D2={a:1|D1}
func dictMatchingPairs(d1, d2 *Pair) ([]Cell, error) {
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
	if len(diff1) > 0 {
		cells = append(cells, rollDict(diff1, parent1), parent2)
	}
	if len(diff2) > 0 {
		cells = append(cells, rollDict(diff2, parent2), parent1)
	}
	return cells, nil
}
