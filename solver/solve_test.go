package solver_test

import (
	"strings"
	"testing"
	"time"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/solver"
)

var (
	clauses = dsl.Clauses
	clause  = dsl.Clause
	atom    = dsl.Atom
	var_    = dsl.Var
	comp    = dsl.Comp
)

func TestSolve(t *testing.T) {
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
