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
