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
	int_    = dsl.Int
	var_    = dsl.Var
	comp    = dsl.Comp
)

func TestSolve(t *testing.T) {
	s, err := solver.NewSolver(`
        nat(0).
        nat(s(X)) :- nat(X).
    `)
	s.Debug("debugtest/test-solve.jsonl")
	if err != nil {
		t.Fatalf("NewSolver: got err: %v", err)
	}
	solutions, cancel := s.Query("nat(X)")
	var got [5]solver.Solution
	for i := 0; i < 5; i++ {
		result, ok := <-solutions
		if !ok {
			t.Fatalf("#%d: stream closed prematurely (err: %v)", i, s.Err)
		}
		got[i] = result
	}
	cancel()
	want := [5]solver.Solution{
		solver.Solution{var_("X"): int_(0)},
		solver.Solution{var_("X"): comp("s", int_(0))},
		solver.Solution{var_("X"): comp("s", comp("s", int_(0)))},
		solver.Solution{var_("X"): comp("s", comp("s", comp("s", int_(0))))},
		solver.Solution{var_("X"): comp("s", comp("s", comp("s", comp("s", int_(0)))))},
	}
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want, +got)%s", diff)
	}
}

func TestSolve_All(t *testing.T) {
	succ := func(t logic.Term) logic.Term { return comp("s", t) }
	s, err := solver.NewSolver(`
        add(0, S, S).
        add(s(A), B, s(S)) :-
            add(A, B, S).
    `)
	s.Debug("debugtest/test-solve-all.jsonl")
	if err != nil {
		t.Fatalf("NewSolver: got err: %v", err)
	}
	solutions, _ := s.Query("add(X, Y, s(s(s(0))))")
	var got []solver.Solution
	for result := range solutions {
		got = append(got, result)
	}
	want := []solver.Solution{
		solver.Solution{var_("X"): int_(0), var_("Y"): succ(succ(succ(int_(0))))},
		solver.Solution{var_("X"): succ(int_(0)), var_("Y"): succ(succ(int_(0)))},
		solver.Solution{var_("X"): succ(succ(int_(0))), var_("Y"): succ(int_(0))},
		solver.Solution{var_("X"): succ(succ(succ(int_(0)))), var_("Y"): int_(0)},
	}
	if diff := cmp.Diff(want, got, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("(-want, +got)%s", diff)
	}
}

func TestSolve_Cancel(t *testing.T) {
	s, err := solver.NewSolver("loop() :- loop().")
	if err != nil {
		t.Fatalf("NewSolver, got err: %v", err)
	}
	solutions, cancel := s.Query("loop()")
	<-time.After(10 * time.Millisecond)
	cancel()
	result, ok := <-solutions
	if result != nil {
		t.Fatalf("result is not nil: %v", result)
	}
	if ok {
		t.Fatalf("channel is open after canceling")
	}
	if !strings.Contains(s.Err.Error(), "interrupted @ clock") {
		t.Errorf("expected interrupted error, got: %v", s.Err)
	} else {
		t.Log(s.Err)
	}
}
