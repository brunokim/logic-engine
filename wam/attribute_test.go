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
	case <-time.After(10 * time.Millisecond):
		cancel()
		return nil, fmt.Errorf("timeout exceeded")
	}
}

// ----

func TestCheckAttribute(t *testing.T) {
	s, err := solver.New(`
        in_range(X, Min, Max) :-
            put_attr(X, range(Min, Max)).

        % The variable range is reduced to the intersection of both
        % ranges.
        check_attribute(range(Min1, Max1), Value, range(Min, Max)) :-
            get_attr(Value, range(Min2, Max2)),
            if(@<(Min1, Min2), =(Min, Min2), =(Min, Min1)),
            if(@>(Max1, Max2), =(Max, Max2), =(Max, Max1)),
            in_range(Value, Min, Max).
    `)
	if err != nil {
		t.Fatal(err)
	}
	s.SetDebug("debugtest/check-attribute.jsonl")
	s.SetIterLimit(150)

	solution, err := firstSolution(s.Query(`
        in_range(X, 1, 5),
        in_range(Y, 3, 9),
        =(X, Y),
        get_attr(Y, range(Min, Max))
    `))
	if err != nil {
		t.Fatal(err)
	}
	min, max := solution[var_("Min")], solution[var_("Max")]
	minWant, maxWant := int_(3), int_(5)
	if min != minWant {
		t.Errorf("Min = %v != %s", min, minWant)
	}
	if max != maxWant {
		t.Errorf("Max = %v != %s", max, maxWant)
	}
}
