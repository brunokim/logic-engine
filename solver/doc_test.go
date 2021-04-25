package solver_test

import (
	"fmt"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/solver"
)

func Example() {
	succ := func(t logic.Term) logic.Term { return comp("s", t) }
	s, _ := solver.NewSolver(clauses(
		// nat(0).
		// nat(s(X)).
		clause(comp("nat", atom("0"))),
		clause(comp("nat", succ(var_("X"))),
			comp("nat", var_("X"))),
		// add(0, Sum, Sum).
		// add(s(A), B, s(Sum)) :-
		//   add(A, B, Sum).
		clause(comp("add", atom("0"), var_("Sum"), var_("Sum"))),
		clause(comp("add", succ(var_("A")), var_("B"), succ(var_("Sum"))),
			comp("add", var_("A"), var_("B"), var_("Sum"))),
		// mul(0, _, 0).
		// mul(s(A), B, Product) :-
		//   mul(A, B, Partial),
		//   add(B, Partial, Product).
		clause(comp("mul", atom("0"), var_("_"), atom("0"))),
		clause(comp("mul", succ(var_("A")), var_("B"), var_("Product")),
			comp("mul", var_("A"), var_("B"), var_("Partial")),
			comp("add", var_("B"), var_("Partial"), var_("Product"))),
	))

	// ?- mul(s(s(0)), s(s(s(0))), Y), add(s(0), X, Y).
	solutions, _ := s.Query(
		comp("mul", succ(succ(atom("0"))), succ(succ(succ(atom("0")))), var_("Y")),
		comp("add", succ(atom("0")), var_("X"), var_("Y")))
	for solution := range solutions {
		fmt.Println(solution)
	}
	// Output: X = s(s(s(s(s("0"))))), Y = s(s(s(s(s(s("0"))))))
}
