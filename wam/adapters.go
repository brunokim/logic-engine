package wam

import (
	"fmt"

	"github.com/brunokim/logic-engine/logic"
)

func toFunctor(indicator logic.Indicator) Functor {
	return Functor{Name: indicator.Name, Arity: indicator.Arity}
}

func toConstant(atom logic.Atom) *Constant {
	return &Constant{Value: atom.Name}
}

func fromCell(c Cell) logic.Term {
	c = deref(c)
	switch c := c.(type) {
	case *Ref:
		return fromRef(c)
	case *Struct:
		return fromStruct(c)
	case *Constant:
		return fromConstant(c)
	case *List:
		return fromList(c)
	}
	panic(fmt.Sprintf("fromCell: unexpected cell type %T (%v)", c, c))
}

func fromCells(cs []Cell) []logic.Term {
	args := make([]logic.Term, len(cs))
	for i, arg := range cs {
		args[i] = fromCell(arg)
	}
	return args
}

func fromRef(ref *Ref) logic.Term {
	if ref.Cell != nil {
		return fromCell(ref.Cell)
	}
	return logic.Var{Name: "_X"}.WithSuffix(ref.id)
}

func fromStruct(s *Struct) *logic.Comp {
	return logic.NewComp(s.Name, fromCells(s.Args)...)
}

func fromConstant(c *Constant) logic.Atom {
	return logic.Atom{Name: c.Value}
}

func fromList(l *List) logic.Term {
	terms := []Cell{l.Head}
	tail := deref(l.Tail)
	t, ok := tail.(*List)
	for ok {
		terms = append(terms, t.Head)
		tail = deref(t.Tail)
		t, ok = tail.(*List)
	}
	return logic.NewIncompleteList(fromCells(terms), fromCell(tail))
}
