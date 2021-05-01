package fuzz

import (
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/parser"
	"github.com/brunokim/logic-engine/solver"
)

func Fuzz(data []byte) int {
	clauses, err := parser.ParseClauses(string(data))
	if err != nil {
		return 0
	}
	_, err = solver.NewSolverFromClauses(clauses)
	if err != nil {
		if _, ok := err.(*logic.ClauseNormalizeError); ok {
			return 0
		}
		panic(err)
	}
	return 1
}
