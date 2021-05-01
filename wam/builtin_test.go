package wam_test

import (
	"testing"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

func TestUnicode(t *testing.T) {
	m := wam.NewMachine()
	m.DebugFilename = "debugtest/test-unicode.jsonl"
	solution, err := m.RunQuery(comp("unicode_digit", var_("X")))
	var digits string
	for err == nil {
		x, ok := solution[var_("X")]
		if !ok {
			t.Fatalf("X not found in solution: %v", solution)
		}
		a, ok := x.(logic.Atom)
		if !ok {
			t.Fatalf("X is not an atom: %v", a)
		}
		digits += a.Name
		solution, err = m.NextSolution()
	}
	t.Logf("%v", err)
	t.Logf("%q", digits)
}
