// Package solver contains methods to execute logic programs and list their
// solutions.
package solver

import (
	"fmt"
	"sort"
	"strings"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

// Solver provides an asynchronous interface for enumerating the solutions
// of a logic query.
type Solver struct {
	// Err stores the error that terminated a query.
	Err error

	m *wam.Machine
}

// Solution (or bindings) is a substitution from vars to terms that produce
// a valid predicate.
type Solution map[logic.Var]logic.Term

func (s Solution) String() string {
	vars := make([]logic.Var, len(s))
	i := 0
	for x := range s {
		vars[i] = x
		i++
	}
	sort.Slice(vars, func(i, j int) bool { return vars[i].Less(vars[j]) })
	var b strings.Builder
	for i, x := range vars {
		fmt.Fprintf(&b, "%v = %v", x, s[x])
		if i < len(vars)-1 {
			b.WriteString(", ")
		}
	}
	return b.String()
}

// NewSolver compiles the provided clauses and returns a Solver object.
func NewSolver(clauses []*logic.Clause) (*Solver, error) {
	compiled, err := wam.CompileClauses(clauses)
	if err != nil {
		return nil, err
	}
	solver := new(Solver)
	solver.m = wam.NewMachine()
	for _, clause := range compiled {
		solver.m.AddClause(clause)
	}
	return solver, nil
}

// Debug sets a file to output execution information for the internal machine.
func (solver *Solver) Debug(filename string) {
	solver.m.DebugFilename = filename
}

// Query returns an (unbuffered) channel of solutions for the provided query,
// and a cancel function to interrupt the execution.
//
// The last error found is stored in solver.Err.
func (solver *Solver) Query(terms ...logic.Term) (<-chan Solution, func()) {
	solver.Err = nil
	solver.m.Reset()
	stream := make(chan Solution)
	go func() {
		bindings, err := solver.m.RunQuery(terms...)
		for err == nil {
			stream <- bindings
			bindings, err = solver.m.NextSolution()
		}
		solver.Err = err
		close(stream)
	}()
	return stream, func() { solver.m.Interrupt() }
}
