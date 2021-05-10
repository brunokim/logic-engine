package wam_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/brunokim/logic-engine/solver"
)

func firstSolution(solutions <-chan solver.Solution, cancel func()) (solver.Solution, error) {
	select {
	case solution := <-solutions:
		return solution, nil
	case <-time.After(50 * time.Millisecond):
		cancel()
		return nil, fmt.Errorf("timeout exceeded")
	}
}

// ----

func TestCheckAttribute(t *testing.T) {
	s, err := solver.New(`
        in_range(X, Min, Max) :-
            put_attr(X, range(Min, Max)).

        % Join ranges if Y also has range attribute, otherwise simply
        % move it to Y.
        join_attribute(range, X, Y) :-
            get_attr(X, range(Min, Max)),
            if(get_attr(Y, range(A, B)),
                join_range(X, Y),
                in_range(Y, Min, Max)).

        % Compute the intersection of ranges, and associate it to Y.
        join_range(X, Y) :-
            get_attr(X, range(Min1, Max1)),
            get_attr(Y, range(Min2, Max2)),
            if(@<(Min1, Min2), =(Min, Min2), =(Min, Min1)),
            if(@>(Max1, Max2), =(Max, Max2), =(Max, Max1)),
            @<(Min, Max),
            in_range(Y, Min, Max).

        % Check that the value is compatible with the attribute.
        check_attribute(range(Min, Max), Value) :-
            @=<(Min, Value),
            @>(Max, Value).
    `)
	if err != nil {
		t.Fatal(err)
	}
	s.SetDebug("debugtest/check-attribute.jsonl")
	s.SetIterLimit(150)

	_, err = firstSolution(s.Query(`
        in_range(X, 1, 5),
        in_range(Y, 3, 9),
        =(X, Y),
        =(X, 4),
    `))
	if err != nil {
		t.Fatal(err)
	}
}
