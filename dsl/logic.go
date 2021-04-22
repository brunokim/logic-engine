package dsl

import (
	"github.com/brunokim/logic-engine/logic"
)

func Terms(terms ...logic.Term) []logic.Term {
	return terms
}

func Atom(name string) logic.Atom {
	return logic.Atom{Name: name}
}

func Int(i int) logic.Int {
	return logic.Int{Value: i}
}

func Var(name string) logic.Var {
	return logic.NewVar(name)
}

func SVar(name string, suffix int) logic.Var {
	return logic.NewVar(name).WithSuffix(suffix)
}

func Comp(functor string, args ...logic.Term) *logic.Comp {
	return logic.NewComp(functor, args...)
}

func Indicator(name string, arity int) logic.Indicator{
    return logic.Indicator{Name: name, Arity: arity}
}

func Query(comps ...*logic.Comp) []*logic.Comp {
	return comps
}

func Clause(head logic.Term, body ...logic.Term) *logic.Clause {
	return logic.NewClause(head, body...)
}

func Clauses(cs ...*logic.Clause) []*logic.Clause {
	return cs
}

// ----

func List(terms ...logic.Term) logic.Term {
	return logic.NewList(terms...)
}

func IList(terms ...logic.Term) logic.Term {
	n := len(terms)
	butlast, last := terms[:n-1], terms[n-1]
	return logic.NewIncompleteList(butlast, last)
}

// ----

func Assoc(key, val logic.Term) *logic.Assoc {
	return logic.NewAssoc(key, val)
}

func Dict(kvs ...logic.Term) logic.Term {
	n := len(kvs)
	if n%2 == 1 {
		panic("Expected even number of key-value entries")
	}
	tmp := make([]logic.Term, n+1)
	copy(tmp, kvs)
	tmp[n] = logic.EmptyDict
	return IDict(tmp...)
}

func IDict(kvs ...logic.Term) logic.Term {
	n := len(kvs)
	if n%2 == 0 {
		panic("Expected even number of key-value entries + parent")
	}
	assocs := make([]*logic.Assoc, n/2)
	for i := 0; i < n/2; i++ {
		key := kvs[2*i]
		val := kvs[2*i+1]
		assocs[i] = Assoc(key, val)
	}
	parent := kvs[n-1]
	return logic.NewIncompleteDict(assocs, parent)
}
