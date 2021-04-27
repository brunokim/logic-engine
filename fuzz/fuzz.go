package fuzz

import (
	"github.com/brunokim/logic-engine/parser"
)

func Fuzz(data []byte) int {
	_, err := parser.ParseClauses(string(data))
	if err != nil {
		return 0
	}
	return 1
}
