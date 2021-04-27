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

// unrollDict returns all assoc Pairs that compose a dict, and its parent.
func unrollDict(d *Pair) ([]*Pair, Cell, error) {
	if d.Tag != DictPair {
		return nil, nil, fmt.Errorf("unrollDict: not a dict: %v", d)
	}
	elems, parent := unroll(d)
	assocs := make([]*Pair, len(elems))
	for i, elem := range elems {
		pair, ok := elem.(*Pair)
		if !(ok && pair.Tag == AssocPair) {
			return nil, nil, fmt.Errorf("non-assoc content in dict: %v", elem)
		}
		if !isGround(pair.Head) {
			return nil, nil, fmt.Errorf("non-ground key in dict: %v", pair.Head)
		}
		assocs[i] = pair
	}
	return assocs, parent, nil
}

// rollDict creates a new dict Pair from the provided assoc list.
func rollDict(assocs []*Pair, parent Cell) Cell {
	d := parent
	for i := len(assocs) - 1; i >= 0; i-- {
		d = &Pair{Tag: DictPair, Head: assocs[i], Tail: d}
	}
	return d
}
