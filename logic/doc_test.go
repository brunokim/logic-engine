package logic_test

import (
	"fmt"

	. "github.com/brunokim/logic-engine/logic"
)

func ExampleAtom() {
	fmt.Println(Atom{"a123"}, Atom{"space-> <-"}, Atom{"Upper"}, Atom{"123"})
	// Output: a123 'space-> <-' 'Upper' '123'
}

func ExampleClause_Normalize() {
	clause1 := NewClause(Atom{"p"}, NewComp("f", NewVar("X")), Atom{"q"}, NewVar("Y"))
	clause2, _ := clause1.Normalize()
	fmt.Println(clause1)
	fmt.Println(clause2)
	// Output: p :-
	//   f(X),
	//   q,
	//   Y.
	// p() :-
	//   f(X),
	//   q(),
	//   call(Y).
}
