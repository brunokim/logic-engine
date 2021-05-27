package fuzz

import (
	"github.com/brunokim/logic-engine/parser"
	"github.com/brunokim/logic-engine/solver"
)

func Fuzz(data []byte) int {
	rules, err := parser.ParseRules(string(data))
	if err != nil {
		return 0
	}
	_, err = solver.NewSolverFromRules(rules)
	if err != nil {
		panic(err)
	}
	return 1
}
