package compiler

import (
	"fmt"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

func toConstant(term logic.Term) wam.Constant {
	switch t := term.(type) {
	case logic.Atom:
		return wam.WAtom(t.Name)
	case logic.Int:
		return wam.WInt(t.Value)
	case logic.Ptr:
		return wam.WPtr{t.Value}
	default:
		panic(fmt.Sprintf("toConstant: unhandled type %T (%v)", term, term))
	}
}

type termCategory int

const (
	atomic termCategory = iota
	variable
	complexTerm
)

func category(term logic.Term) termCategory {
	switch term.(type) {
	case logic.Int:
		return atomic
	case logic.Atom:
		return atomic
	case logic.Ptr:
		return atomic
	case logic.Var:
		return variable
	case *logic.Comp:
		return complexTerm
	case *logic.List:
		return complexTerm
	case *logic.Assoc:
		return complexTerm
	case *logic.Dict:
		return complexTerm
	default:
		panic(fmt.Sprintf("compiler.category: unhandled term type %T (%v)", term, term))
	}
}

func complexArgs(term logic.Term) []logic.Term {
	switch t := term.(type) {
	case *logic.Comp:
		return t.Args
	case *logic.List:
		return []logic.Term{t.Terms[0], t.Slice(1)}
	case *logic.Assoc:
		return []logic.Term{t.Key, t.Val}
	case *logic.Dict:
		return []logic.Term{t.Assocs[0], t.Tail()}
	default:
		panic(fmt.Sprintf("compiler.complexArgs: unhandled term type %T (%v)", term, term))
	}
}

func pairTag(term logic.Term) wam.PairTag {
	switch term.(type) {
	case *logic.List:
		return wam.ListPair
	case *logic.Assoc:
		return wam.AssocPair
	case *logic.Dict:
		return wam.DictPair
	default:
		panic(fmt.Sprintf("compiler.pairTag: unhandled term type %T (%v)", term, term))
	}
}
