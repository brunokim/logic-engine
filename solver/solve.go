package solver

import (
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

type Solver struct {
	m *wam.Machine
}

type Solution map[logic.Var]logic.Term

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

type Result struct {
	Solution Solution
	Err      error
}

func (solver *Solver) Query(terms ...logic.Term) (<-chan Result, func()) {
	solver.m.Reset()
	stream := make(chan Result)
	go func() {
		bindings, err := solver.m.RunQuery(terms...)
		stream <- Result{bindings, err}
		for err != nil {
			bindings, err = solver.m.NextSolution()
			stream <- Result{bindings, err}
		}
		close(stream)
	}()
	return stream, func() { solver.m.Interrupt() }
}
