package wam_test

import (
	"testing"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

func TestCheckAttribute(t *testing.T) {
	m := wam.NewMachine()

	// check_attribute(domain(Min1, Max1), Value, domain(Min, Max)) :-
	//   get_attr(Value, range(Min2, Max2)),
	//   if(@<(Min1, Min2), Min = Min2, Min = Min1),
	//   if(@>(Max1, Max2), Max = Max2, Max = Max1),
	//   put_attr(Value, range(Min, Max)).
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(
			comp("check_attribute",
				comp("range", var_("Min1"), var_("Max1")),
				var_("Value"),
				comp("range", var_("Min"), var_("Max"))),
			comp("get_attr", var_("Value"), comp("range", var_("Min2"), var_("Max2"))),
			comp("if", comp("@<", var_("Min1"), var_("Min2")),
				comp("=", var_("Min"), var_("Min2")),
				comp("=", var_("Min"), var_("Min1"))),
			comp("if", comp("@>", var_("Max1"), var_("Max2")),
				comp("=", var_("Max"), var_("Max2")),
				comp("=", var_("Max"), var_("Max1"))),
			comp("put_attr", var_("Value"), comp("range", var_("Min"), var_("Max")))),
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	m.IterLimit = 150
	m.DebugFilename = "debugtest/check-attribute.jsonl"

	// ?-
	//   put_attr(X, range(1, 5)),
	//   put_attr(Y, range(3, 9)),
	//   X = Y,
	//   get_attr(Y, range(Min, Max)).
	solution, err := m.RunQuery(
		comp("put_attr", var_("X"), comp("range", int_(1), int_(5))),
		comp("put_attr", var_("Y"), comp("range", int_(3), int_(9))),
		comp("=", var_("X"), var_("Y")),
		comp("get_attr", var_("Y"), comp("range", var_("Min"), var_("Max"))))
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
