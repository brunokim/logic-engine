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
	case *Pair:
		return fromPair(c)
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

func fromAssocPairs(cs []Cell) []*logic.Assoc {
	args := make([]*logic.Assoc, len(cs))
	for i, arg := range cs {
		args[i] = fromCell(arg).(*logic.Assoc)
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

func fromPair(p *Pair) logic.Term {
	if p.Tag == AssocPair {
		return logic.NewAssoc(fromCell(p.Head), fromCell(p.Tail))
	}
	tag := p.Tag
	terms := []Cell{p.Head}
	tail := deref(p.Tail)
	t, ok := tail.(*Pair)
	for ok && t.Tag == tag {
		terms = append(terms, t.Head)
		tail = deref(t.Tail)
		t, ok = tail.(*Pair)
	}
	switch tag {
	case ListPair:
		return logic.NewIncompleteList(fromCells(terms), fromCell(tail))
	case DictPair:
		return logic.NewIncompleteDict(fromAssocPairs(terms), fromCell(tail))
	default:
		panic(fmt.Sprintf("fromPair: unhandled pair type %v", tag))
	}
}
