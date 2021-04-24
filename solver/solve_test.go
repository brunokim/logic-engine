package solver_test

import (
	"strings"
	"testing"
	"time"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/solver"
	"github.com/brunokim/logic-engine/test_helpers"

	"github.com/google/go-cmp/cmp"
)

var (
	clauses = dsl.Clauses
	clause  = dsl.Clause
	atom    = dsl.Atom
	var_    = dsl.Var
	comp    = dsl.Comp
)

func TestSolve(t *testing.T) {
	// nat(0).
	// nat(s(X)) :- nat(X).
	s, err := solver.NewSolver(clauses(
		clause(comp("nat", atom("0"))),
		clause(comp("nat", comp("s", var_("X"))),
			comp("nat", var_("X"))),
	))
	s.Debug("debugtest/test-solve.jsonl")
	if err != nil {
		t.Fatalf("NewSolver: got err: %v", err)
	}
	// ?- nat(X).
	solutions, cancel := s.Query(comp("nat", var_("X")))
	var got [5]solver.Solution
	for i := 0; i < 5; i++ {
		result := <-solutions
		if result.Err != nil {
			t.Fatalf("#%d: got err: %v", i, result.Err)
		}
		got[i] = result.Solution
	}
	cancel()
	want := [5]solver.Solution{
		solver.Solution{var_("X"): atom("0")},
		solver.Solution{var_("X"): comp("s", atom("0"))},
		solver.Solution{var_("X"): comp("s", comp("s", atom("0")))},
		solver.Solution{var_("X"): comp("s", comp("s", comp("s", atom("0"))))},
		solver.Solution{var_("X"): comp("s", comp("s", comp("s", comp("s", atom("0")))))},
	}
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want, +got)%s", diff)
	}
}

func TestSolve_All(t *testing.T) {
	succ := func(t logic.Term) logic.Term { return comp("s", t) }

	// add(0, S, S).
	// add(s(A), B, s(S)) :- add(A, B, S).
	s, err := solver.NewSolver(clauses(
		clause(comp("add", atom("0"), var_("S"), var_("S"))),
		clause(comp("add", succ(var_("A")), var_("B"), succ(var_("S"))),
			comp("add", var_("A"), var_("B"), var_("S"))),
	))
	s.Debug("debugtest/test-solve-all.jsonl")
	if err != nil {
		t.Fatalf("NewSolver: got err: %v", err)
	}
	// ?- add(X, Y, s(s(s(0)))).
	solutions, _ := s.Query(comp("add", var_("X"), var_("Y"), succ(succ(succ(atom("0"))))))
	var got []solver.Solution
	var i int
	for result := range solutions {
		if result.Err != nil {
			break
		}
		i++
		got = append(got, result.Solution)
	}
	want := []solver.Solution{
		solver.Solution{var_("X"): atom("0"), var_("Y"): succ(succ(succ(atom("0"))))},
		solver.Solution{var_("X"): succ(atom("0")), var_("Y"): succ(succ(atom("0")))},
		solver.Solution{var_("X"): succ(succ(atom("0"))), var_("Y"): succ(atom("0"))},
		solver.Solution{var_("X"): succ(succ(succ(atom("0")))), var_("Y"): atom("0")},
	}
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want, +got)%s", diff)
	}
}

func TestSolve_Cancel(t *testing.T) {
	// loop :- loop.
	s, err := solver.NewSolver(clauses(
		clause(atom("loop"), atom("loop")),
	))
	if err != nil {
		t.Fatalf("NewSolver, got err: %v", err)
	}
	// ?- loop.
	solutions, cancel := s.Query(atom("loop"))
	<-time.After(10 * time.Millisecond)
	cancel()
	result, ok := <-solutions
	if !ok {
		t.Fatalf("s.Query() channel was closed prematurely")
	}
	if !strings.Contains(result.Err.Error(), "interrupted @ clock") {
		t.Errorf("expected interrupted error, got: %v", result.Err)
	} else {
		t.Log(result.Err)
	}
}
