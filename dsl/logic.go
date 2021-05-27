// Package dsl contains constructors for several types as functions.
//
// This package may be used to simplify building complex types with a
// uniform interface. It's specially useful for testing, but may be
// used anywhere as a thin wrapper over the actual constructors.
//
// It's more ergonomic to alias function names instead of dot-importing
// this package, e.g.
//
//     var (
//         atom = dsl.Atom
//         var_ = dsl.Var
//         comp = dsl.Comp
//         list = dsl.List
//
//         // The list [a, X, f(Y)]
//         aList = list(atom("a"), var_("X"), comp("f", var_("Y")))
//     )
package dsl

import (
	"fmt"

	"github.com/brunokim/logic-engine/logic"
)

// Terms builds a list of logic terms.
func Terms(terms ...logic.Term) []logic.Term {
	return terms
}

// Atom builds a logic atom.
func Atom(name string) logic.Atom {
	return logic.Atom{Name: name}
}

// Int builds a logic int.
func Int(i int) logic.Int {
	return logic.Int{Value: i}
}

// Ptr builds a logic pointer.
func Ptr(v interface{}) logic.Ptr {
	return logic.Ptr{Value: v}
}

// Var builds a logic variable.
func Var(name string) logic.Var {
	return logic.NewVar(name)
}

// SVar builds a logic variable with a suffix.
//
// This should only be useful for tests that *want* to check that the suffix
// of a generated var is correct.
func SVar(name string, suffix int) logic.Var {
	return logic.NewVar(name).WithSuffix(suffix)
}

// Comp builds a logic compound term.
func Comp(functor string, args ...logic.Term) *logic.Comp {
	return logic.NewComp(functor, args...)
}

// Indicator builds a logic functor indicator.
func Indicator(name string, arity int) logic.Indicator {
	return logic.Indicator{Name: name, Arity: arity}
}

// Clause builds a logic clause.
func Clause(head logic.Term, body ...logic.Term) *logic.Clause {
	return logic.NewClause(head, body...)
}

// DCG builds a logic DCG.
func DCG(head logic.Term, body ...logic.DCGTerm) *logic.DCG {
	return logic.NewDCG(head, body...)
}

// DCGList builds a list for a DCG body.
func DCGList(terms ...logic.Term) logic.DCGTerm {
	if len(terms) == 0 {
		return logic.EmptyList
	}
	return List(terms...).(*logic.List)
}

// DCGIList builds an incomplete list for a DCG body.
func DCGIList(terms ...logic.Term) logic.DCGTerm {
	return IList(terms...).(*logic.List)
}

// DCGGoals builds a braced list of terms for a DCG body.
func DCGGoals(terms ...logic.Term) logic.DCGTerm {
	return logic.DCGGoals(terms)
}

// Rules builds a list of logic rules.
func Rules(rules ...logic.Rule) []logic.Rule {
	return rules
}

// ----

// List builds a logic list.
//
//     List()                                          // []
//     List(Atom("a"), Var("X"), Comp("f", Var("Y")))  // [a, X, f(Y)]
func List(terms ...logic.Term) logic.Term {
	return logic.NewList(terms...)
}

// IList builds an incomplete list, where the last element is the list's tail.
// It panics if less than 2 terms are provided.
//
//     IList(Atom("a"), Var("X"), Var("T")))  // [a, X|T]
func IList(terms ...logic.Term) logic.Term {
	n := len(terms)
	if n < 2 {
		panic(fmt.Sprintf("IList called with <2 arguments: %v", terms))
	}
	butlast, last := terms[:n-1], terms[n-1]
	return logic.NewIncompleteList(butlast, last)
}

// ----

// Assoc builds a key:val association logic term.
func Assoc(key, val logic.Term) *logic.Assoc {
	return logic.NewAssoc(key, val)
}

// Dict builds a logic dictionary.
// It expects an even list of terms, corresponding to alternating key/value terms.
//
//     Dict()                                              // {}
//     Dict(Atom("a"), Var("X"))                           // {a: X}
//     Dict(Atom("a"), Var("X"), List(Atom("p")), Int(1))  // {a: X, [p]: 1}
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

// IDict builds an incomplete dictionary.
// It expects an odd list of terms, corresponding to alternating key/value terms,
// with the last one being the dict's parent.
//
//     Dict(Atom("a"), Var("X"), Var("Ds"))  // {a: X|Ds}
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
