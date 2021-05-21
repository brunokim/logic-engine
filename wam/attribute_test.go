package wam_test

import (
	"errors"
	"testing"
	"time"

	"github.com/brunokim/logic-engine/solver"
	"github.com/brunokim/logic-engine/test_helpers"

	"github.com/google/go-cmp/cmp"
)

func firstSolution(solutions <-chan solver.Solution, cancel func()) (solver.Solution, error) {
	select {
	case solution := <-solutions:
		return solution, nil
	case <-time.After(50 * time.Millisecond):
		cancel()
		return nil, errors.New("timeout exceeded")
	}
}

// ----

func TestCheckAttribute(t *testing.T) {
	s, err := solver.New(`
        package(range, [], ['in_range/3']).

        in_range(X, Min, Max) :-
            put_attr(range, X, range(Min, Max)).

        % Join ranges if Y also has range attribute, otherwise simply
        % move it to Y.
        join_attribute(X, Y) :-
            get_attr(range, X, range(Min1, Max1)),
            ->(get_attr(range, Y, range(Min2, Max2)),
                % Compute the intersection of ranges
                and(
                    ->(@<(Min1, Min2), =(Min, Min2), =(Min, Min1)),
                    ->(@>(Max1, Max2), =(Max, Max2), =(Max, Max1)),
                    @<(Min, Max),
                ),
                and(=(Min, Min1), =(Max, Max1))),
            in_range(Y, Min, Max),
            asm(proceed(verify_attributes)).

        % Check that the value is compatible with the attribute.
        check_attribute(range(Min, Max), Value) :-
            @=<(Min, Value),
            @>(Max, Value),
            asm(proceed(verify_attributes)).
    `)
	if err != nil {
		t.Fatal(err)
	}
	s.SetDebug("debugtest/check-attribute.jsonl")
	s.SetIterLimit(150)

	_, err = firstSolution(s.Query(`
        import(range),
        in_range(X, 1, 5),
        in_range(Y, 3, 9),
        =(X, Y),
        =(X, 4),
    `))
	if err != nil {
		t.Fatal(err)
	}
}

func TestAttributeBacktrack(t *testing.T) {
	s, err := solver.New(`
        package(range, [], ['test/1']).

        test_value(1).
        test_value(2).
        test_value(3).
        test_value(4).
        test(X) :-
            test_value(Value),
            in_range(Y, 1, 2), 
            =(X, Y),           % Check that X didn't keep attribute from previous choicepoint.
            in_range(X, 3, 5), % Overwrite attribute.
            =(X, Value).       % Will backtrack for 1 and 2, and succeed for 3 and 4.

        in_range(X, Min, Max) :-
            put_attr(range, X, range(Min, Max)).

        % Join ranges if Y also has range attribute, otherwise simply
        % move it to Y.
        join_attribute(X, Y) :-
            get_attr(range, X, range(Min1, Max1)),
            ->(get_attr(range, Y, range(Min2, Max2)),
                % Compute the intersection of ranges
                and(->(@<(Min1, Min2), =(Min, Min2), =(Min, Min1)),
                    ->(@>(Max1, Max2), =(Max, Max2), =(Max, Max1)),
                    @<(Min, Max)),
                and(=(Min, Min1), =(Max, Max1))),
            in_range(Y, Min, Max),
            asm(proceed(verify_attributes)).

        % Check that the value is compatible with the attribute.
        check_attribute(range(Min, Max), Value) :-
            @=<(Min, Value),
            @>(Max, Value),
            asm(proceed(verify_attributes)).
    `)
	if err != nil {
		t.Fatal(err)
	}
	s.SetDebug("debugtest/attribute-backtrack.jsonl")
	s.SetIterLimit(400)

	solutions, _ := s.Query("import(range), test(X)")
	var got []solver.Solution
	for solution := range solutions {
		got = append(got, solution)
	}
	want := []solver.Solution{
		solver.Solution{var_("X"): int_(3)},
		solver.Solution{var_("X"): int_(4)},
	}
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf("(-want,+got):\n%s", diff)
		t.Log(s.Err)
	}
}

func TestDeleteAttribute(t *testing.T) {
	s, err := solver.New(`
        package(only, [], []).

        join_attribute(X, Y) :-
            get_attr(only, X, V1),
            ->(get_attr(only, Y, V2),
                =(V1, V2),
                true),
            put_attr(only, Y, V1),
            asm(proceed(verify_attributes)).

        check_attribute(V1, V2) :-
            =(V1, V2),
            asm(proceed(verify_attributes)).
    `)
	if err != nil {
		t.Fatal(err)
	}
	s.SetDebug("debugtest/attribute-delete.jsonl")
	s.SetIterLimit(100)

	got, err := firstSolution(s.Query(`
        put_attr(only, X, 1),
        put_attr(only, Y, 2),
        \=(X, Y),
        del_attr(only, Y),
        =(X, Y),
        \=(X, 2),
        =(X, 1),
    `))
	if err != nil {
		t.Fatal(err)
	}
	want := solver.Solution{var_("X"): int_(1), var_("Y"): var_("X")}
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want,+got):\n%s", diff)
		t.Log(s.Err)
	}
}
