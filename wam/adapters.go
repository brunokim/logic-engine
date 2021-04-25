package wam

import (
	"fmt"

	"github.com/brunokim/logic-engine/logic"
)

func toFunctor(indicator logic.Indicator) Functor {
	return Functor{Name: indicator.Name, Arity: indicator.Arity}
}

func toConstant(term logic.Term) Constant {
	switch t := term.(type) {
	case logic.Atom:
		return WAtom(t.Name)
	case logic.Int:
		return WInt(t.Value)
	default:
		panic(fmt.Sprintf("toConstant: unhandled type %T (%v)", term, term))
	}
}

func fromCell(c Cell) logic.Term {
	c = deref(c)
	switch c := c.(type) {
	case *Ref:
		return fromRef(c)
	case *Struct:
		return fromStruct(c)
	case Constant:
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

func fromConstant(c Constant) logic.Term {
	switch c := c.(type) {
	case WAtom:
		return logic.Atom{Name: string(c)}
	case WInt:
		return logic.Int{Value: int(c)}
	default:
		panic(fmt.Sprintf("fromConstant: unhandled type %T (%v)", c, c))
	}
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
