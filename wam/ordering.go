package wam

import (
	"fmt"
)

type ordering int

const (
	equal ordering = iota
	less
	more
)

func compareInts(i1, i2 int) ordering {
	if i1 < i2 {
		return less
	}
	if i1 > i2 {
		return more
	}
	return equal
}

func compareStrings(s1, s2 string) ordering {
	if s1 < s2 {
		return less
	}
	if s1 > s2 {
		return more
	}
	return equal
}

func cellOrder(c Cell) int {
	switch c := c.(type) {
	case *Ref:
		return 1
	case WInt:
		return 2
	case WAtom:
		return 3
	case WPtr:
		return 4
	case *Struct:
		return 10
	case *Pair:
		switch c.Tag {
		case ListPair:
			return 20
		case AssocPair:
			return 30
		case DictPair:
			return 40
		}
	}
	panic(fmt.Sprintf("cellOrder: unhandled type %T (%v)", c, c))
}

func compareCells(c1, c2 Cell) ordering {
	queue := []Cell{c1, c2}
	for len(queue) > 0 {
		c1, c2 := deref(queue[0]), deref(queue[1])
		queue = queue[2:]
		if o := compareInts(cellOrder(c1), cellOrder(c2)); o != equal {
			return o
		}
		switch c1 := c1.(type) {
		case WAtom:
			c2 := c2.(WAtom)
			if o := compareStrings(string(c1), string(c2)); o != equal {
				return o
			}
		case WInt:
			c2 := c2.(WInt)
			if o := compareInts(int(c1), int(c2)); o != equal {
				return o
			}
		case WPtr:
			panic("compareCells: WPtr: not implemented")
		case *Ref:
			c2 := c2.(*Ref)
			if o := compareInts(c1.id, c2.id); o != equal {
				return o
			}
		case *Struct:
			c2 := c2.(*Struct)
			if o := compareInts(len(c1.Args), len(c2.Args)); o != equal {
				return o
			}
			if o := compareStrings(c1.Name, c2.Name); o != equal {
				return o
			}
			for i := 0; i < len(c1.Args); i++ {
				queue = append(queue, c1.Args[i], c2.Args[i])
			}
		case *Pair:
			c2 := c2.(*Pair)
			queue = append(queue, c1.Head, c2.Head, c1.Tail, c2.Tail)
		default:
			panic(fmt.Sprintf("compareCells: unhandled type %T (%v)", c1, c1))
		}
	}
	return equal
}
