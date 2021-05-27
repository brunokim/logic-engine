package logic

import (
	"fmt"
)

// DCGExpandComp expands a DCG goal to be called with the provided initial and final states.
func DCGExpandComp(c *Comp, l0, l1 Term) *Comp {
	n := len(c.Args)
	terms := make([]Term, n+2)
	copy(terms, c.Args)
	terms[n] = l0
	terms[n+1] = l1
	return NewComp(c.Functor, terms...)
}

// a => a(L0, L1)
func expandAtom(a Atom, l0, l1 Var) *Comp {
	if a == EmptyList {
		return NewComp("=", l0, l1)
	}
	return NewComp(a.Name, l0, l1)
}

func expandVar(x Var, l0, l1 Var) *Comp {
	return NewComp("phrase", x, l0, l1)
}

// f(a, X) => f(a, X, L0, L1)
func expandComp(c *Comp, l0, l1 Var) *Comp {
	return DCGExpandComp(c, l0, l1)
}

// [a, b]  =>  L0 = [a, b|L1].
// [a, b|X]  =>  $append([a, b|X], L1, L0).
func expandList(l *List, l0, l1 Var) *Comp {
	if l.Tail == EmptyList {
		return NewComp("=", l0, NewIncompleteList(l.Terms, l1))
	}
	return NewComp("$append", l, l1, l0)
}

// ToClause converts a DCG into a regular clause.
func (dcg *DCG) ToClause() *Clause {
	cnt := 0
	currVar := func() Var {
		// TODO: avoid a possible var name collision by checking used var names.
		return NewVar(fmt.Sprintf("_L%d", cnt))
	}
	nextVar := func() Var {
		cnt++
		return currVar()
	}
	first := currVar()
	var body []Term
	for _, term := range dcg.Body {
		switch t := term.(type) {
		case Atom:
			body = append(body, expandAtom(t, currVar(), nextVar()))
		case Var:
			body = append(body, expandVar(t, currVar(), nextVar()))
		case *Comp:
			body = append(body, expandComp(t, currVar(), nextVar()))
		case *List:
			body = append(body, expandList(t, currVar(), nextVar()))
		case DCGGoals:
			body = append(body, t...)
		default:
			panic(fmt.Sprintf("Unhandled DCG body term type %T (%v)", term, term))
		}
	}
	last := currVar()
	var head Term
	switch t := dcg.Head.(type) {
	case Atom:
		head = expandAtom(t, first, last)
	case *Comp:
		head = expandComp(t, first, last)
	default:
		panic(fmt.Sprintf("Unhandled DCG head term type %T (%v)", dcg.Head, dcg.Head))
	}
	return NewClause(head, body...)
}
