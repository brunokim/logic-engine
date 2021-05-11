package wam_test

import (
	"testing"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/test_helpers"
	"github.com/brunokim/logic-engine/wam"

	"github.com/google/go-cmp/cmp"
)

func TestInlineUnify(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 40
	m.DebugFilename = "debugtest/inline-unify.jsonl"
	got, err := m.RunQuery(
		comp("=", var_("X"), var_("Y")),
		comp("=", var_("X"), int_(10)),
		comp("=", var_("Y"), int_(10)))
	if err != nil {
		t.Fatal(err)
	}
	want := map[logic.Var]logic.Term{
		var_("X"): int_(10),
		var_("Y"): var_("X"),
	}
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want, +got)\n%s", diff)
	}
}

// Disabled test: "soft-cut" if is not implemented.
func _TestBacktrackingIf(t *testing.T) {
	m := wam.NewMachine()
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(comp("test", int_(1))),
		dsl.Clause(comp("test", int_(2))),
		dsl.Clause(comp("test", int_(3))),
		dsl.Clause(comp("test", int_(4))),
	})
	m.IterLimit = 150
	m.DebugFilename = "debugtest/backtracking-if.jsonl"
	if err != nil {
		t.Fatal(err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	solution, err := m.RunQuery(comp("->",
		comp("test", var_("X")),
		comp("@>", var_("X"), int_(4)),
		comp("=", var_("X"), int_(5))))
	if err != nil {
		t.Fatal(err)
	}
	want := int_(5)
	if got := solution[var_("X")]; got != want {
		t.Errorf("X = %v != %v", got, want)
	}
}

func TestUnicode(t *testing.T) {
	m := wam.NewMachine()
	solution, err := m.RunQuery(comp("unicode_digit", var_("X")))
	var digits string
	for err == nil {
		x, ok := solution[var_("X")]
		if !ok {
			t.Fatalf("X not found in solution: %v", solution)
		}
		a, ok := x.(logic.Atom)
		if !ok {
			t.Fatalf("X is not an atom: %v", a)
		}
		digits += a.Name
		solution, err = m.NextSolution()
	}
	t.Logf("%v", err)
	t.Logf("%q", digits)
}

func TestComparison(t *testing.T) {
	m := wam.NewMachine()
	m.DebugFilename = "debugtest/builtin-comparison.jsonl"
	solution, err := m.RunQuery(
		comp("@<", int_(1), int_(2)),
		comp("@=<", int_(1), int_(2)),
		comp("@=<", int_(2), int_(2)),
		comp("@>", int_(2), int_(1)),
		comp("@>=", int_(2), int_(1)),
		comp("@>=", int_(2), int_(2)),
		comp("==", int_(2), int_(2)),
		comp("\\==", int_(2), int_(3)))
	if err != nil {
		t.Fatal(err)
	}
	t.Log(solution)
}

func TestUnifiable(t *testing.T) {
	m := wam.NewMachine()
	m.DebugFilename = "debugtest/builtin-unifiable.jsonl"
	solution, err := m.RunQuery(
		comp("unifiable",
			comp("f", var_("X"), comp("g", atom("b")), comp("h", var_("Y"))),
			comp("f", comp("h", comp("g", var_("Z"))), var_("Y"), var_("X")),
			var_("Unifier")))
	if err != nil {
		t.Fatal(err)
	}
	got := solution[var_("Unifier")]
	want := list(
		assoc(var_("Y"), comp("g", atom("b"))),
		assoc(var_("X"), comp("h", comp("g", var_("Z")))),
		assoc(var_("Z"), atom("b")))
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want,+got)\n%s", diff)
	}
}
