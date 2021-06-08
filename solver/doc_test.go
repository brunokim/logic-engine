package solver_test

import (
	"fmt"

	"github.com/brunokim/logic-engine/solver"
)

func Example() {
	s := solver.New()
	s.Consult(`
	% Natural number definition in terms of successor s(X).
	nat(0).
	nat(s(X)) :- nat(X).

	% Adding A+B=Sum
	add(0, Sum, Sum).
	add(s(A), B, s(Sum)) :-
		add(A, B, Sum).

	% Multiplying A*B=Product
	mul(0, _, 0).
	mul(s(A), B, Product) :-
		mul(A, B, Partial),
		add(B, Partial, Product).
`)

	solutions, _ := s.Query(`
	mul(s(s(0)), s(s(s(0))), Y), % 2*3=Y
	add(s(0), X, Y),             % 1+X=Y
`)
	for solution := range solutions {
		fmt.Println(solution)
	}

	// You can reuse the same solver object for a different query.
	solutions, _ = s.Query(`
	add(A, B, s(s(s(0))))  % A+B=3
`)
	for solution := range solutions {
		fmt.Println(solution)
	}
	// Output: X = s(s(s(s(s(0))))), Y = s(s(s(s(s(s(0))))))
	// A = 0, B = s(s(s(0)))
	// A = s(0), B = s(s(0))
	// A = s(s(0)), B = s(0)
	// A = s(s(s(0))), B = 0
}
