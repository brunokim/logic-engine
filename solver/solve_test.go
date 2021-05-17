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
	"github.com/google/go-cmp/cmp/cmpopts"
)

var (
	clauses = dsl.Clauses
	clause  = dsl.Clause
	atom    = dsl.Atom
	int_    = dsl.Int
	var_    = dsl.Var
	comp    = dsl.Comp
	list    = dsl.List
	ilist   = dsl.IList
)

func TestSolve(t *testing.T) {
	s, err := solver.New(`
        nat(0).
        nat(s(X)) :- nat(X).
    `)
	if err != nil {
		t.Fatalf("New: got err: %v", err)
	}
	s.SetDebug("debugtest/test-solve.jsonl")
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
	s, err := solver.New(`
        add(0, S, S).
        add(s(A), B, s(S)) :-
            add(A, B, S).
    `)
	if err != nil {
		t.Fatalf("New: got err: %v", err)
	}
	s.SetDebug("debugtest/test-solve-all.jsonl")
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
	s, err := solver.New("loop :- loop.")
	if err != nil {
		t.Fatalf("New, got err: %v", err)
	}
	solutions, cancel := s.Query("loop")
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

func TestSolve_Lib(t *testing.T) {
	tests := []struct {
		query string
		want  []solver.Solution
	}{
		{"append([], [1, 2], L)", []solver.Solution{{var_("L"): list(int_(1), int_(2))}}},
		{"append([1], [2|T], L)", []solver.Solution{{var_("L"): ilist(int_(1), int_(2), var_("T"))}}},
		{
			"append(X, Y, [1, 2, 3])",
			[]solver.Solution{
				{var_("X"): list(), var_("Y"): list(int_(1), int_(2), int_(3))},
				{var_("X"): list(int_(1)), var_("Y"): list(int_(2), int_(3))},
				{var_("X"): list(int_(1), int_(2)), var_("Y"): list(int_(3))},
				{var_("X"): list(int_(1), int_(2), int_(3)), var_("Y"): list()},
			},
		},
		{"member(1, [3, 2, 1])", []solver.Solution{{}}},
		{"member(2, [3, 2, 2])", []solver.Solution{{}, {}}},
		{"member(4, [3, 2, 1])", []solver.Solution{}},
		{
			"member(f(X), [f(a), g(b), h(c), f(d)])",
			[]solver.Solution{
				{var_("X"): atom("a")},
				{var_("X"): atom("d")},
			},
		},
	}
	for _, test := range tests {
		s, err := solver.New("")
		if err != nil {
			t.Fatal(err)
		}
		var got []solver.Solution
		solutions, _ := s.Query(test.query)
		for solution := range solutions {
			got = append(got, solution)
		}
		if diff := cmp.Diff(test.want, got, cmpopts.EquateEmpty(), test_helpers.IgnoreUnexported); diff != "" {
			t.Errorf("%q: (-want, +got)\n%s", test.query, diff)
		}
	}
}
