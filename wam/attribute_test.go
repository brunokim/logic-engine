package wam_test

import (
	"testing"

	"github.com/brunokim/logic-engine/parser"
	"github.com/brunokim/logic-engine/wam"
)

func TestCheckAttribute(t *testing.T) {
	m := wam.NewMachine()

	logicClauses, err := parser.ParseClauses(`
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
	clauses, err := wam.CompileClauses(logicClauses)
	if err != nil {
		t.Fatal(err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	m.IterLimit = 150
	m.DebugFilename = "debugtest/check-attribute.jsonl"

	terms, err := parser.ParseQuery(`
        in_range(X, 1, 5),
        in_range(Y, 3, 9),
        =(X, Y),
        get_attr(Y, range(Min, Max))
    `)
	if err != nil {
		t.Fatal(err)
	}
	solution, err := m.RunQuery(terms...)
	if err != nil {
		t.Fatalf("expected nil, got err: %v", err)
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
