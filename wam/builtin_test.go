package wam_test

import (
	"fmt"
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
	pkg, err := wam.CompilePackage([]logic.Rule{
		dsl.Clause(comp("test", int_(1))),
		dsl.Clause(comp("test", int_(2))),
		dsl.Clause(comp("test", int_(3))),
		dsl.Clause(comp("test", int_(4))),
	}, wam.UseConflictAvoidanceAllocationStrategy{})
	if err != nil {
		t.Fatal(err)
	}
	m := wam.NewMachine()
	m.AddPackage(pkg)
	m.IterLimit = 150
	m.DebugFilename = "debugtest/backtracking-if.jsonl"
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
			t.Fatalf("X is not an atom: %v", x)
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
	tests := []struct {
		query   []logic.Term
		want    logic.Term
		wantErr bool
	}{
		{
			[]logic.Term{
				comp("unifiable",
					comp("f", var_("X"), comp("g", atom("b")), comp("h", var_("Y"))),
					comp("f", comp("h", comp("g", var_("Z"))), var_("Y"), var_("X")),
					var_("Unifier")),
			},
			list(
				assoc(var_("Y"), comp("g", atom("b"))),
				assoc(var_("X"), comp("h", comp("g", var_("Z")))),
				assoc(var_("Z"), atom("b"))),
			false,
		},
		{
			[]logic.Term{comp("unifiable", var_("X"), var_("Y"), var_("Unifier"))},
			list(assoc(var_("Y"), var_("X"))),
			false,
		},
		{
			[]logic.Term{comp("unifiable", atom("a"), atom("b"), var_("Unifier"))},
			nil,
			true,
		},
		{
			[]logic.Term{
				comp("=", var_("X"), var_("Y")),
				comp("unifiable", comp("f", var_("X")), comp("f", var_("Y")), var_("Unifier")),
			},
			list(),
			false,
		},
	}
	for i, test := range tests {
		m := wam.NewMachine()
		m.DebugFilename = fmt.Sprintf("debugtest/builtin-unifiable-%d.jsonl", i)
		solution, err := m.RunQuery(test.query...)
		if err != nil && !test.wantErr {
			t.Fatal(err)
		}
		if test.wantErr {
			if len(solution) > 0 {
				t.Errorf("%v: want err, got solution: %v", test.query, solution)
			}
			continue
		}
		got := solution[var_("Unifier")]
		if diff := cmp.Diff(test.want, got, test_helpers.IgnoreUnexported); diff != "" {
			t.Errorf("%v: (-want,+got)\n%s", test.query, diff)
		}
	}
}

func TestTypeCheck(t *testing.T) {
	m := wam.NewMachine()
	m.DebugFilename = "debugtest/builtin-type-check.jsonl"
	solution, err := m.RunQuery(
		//
		comp("=", var_("Atom"), atom("a")),
		comp("atom", atom("a")),
		comp("atom", var_("Atom")),
		//
		comp("=", var_("Int"), int_(10)),
		comp("int", int_(20)),
		comp("int", var_("Int")),
		//
		comp("=", var_("Ptr"), ptr(m)),
		comp("ptr", ptr(&struct{}{})),
		comp("ptr", var_("Ptr")),
		//
		comp("=", var_("List"), list(int_(42))),
		comp("list", ilist(var_("H"), var_("T"))),
		comp("list", var_("List")),
		//
		comp("=", var_("Assoc"), assoc(atom("a"), int_(1))),
		comp("assoc", assoc(int_(1), atom("a"))),
		comp("assoc", var_("Assoc")),
		//
		comp("=", var_("Dict"), dict(atom("x"), var_("X"))),
		comp("dict", dict(atom("y"), var_("Y"))),
		comp("dict", var_("Dict")),
	)
	if err != nil {
		t.Fatal(err)
	}
	t.Log(solution)
}
