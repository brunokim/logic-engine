package wam_test

import (
	"fmt"

	. "github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

func Example() {
	m := wam.NewMachine()
	logicClauses := []*logic.Clause{
		Clause(Comp("parent", Atom("elizabeth"), Atom("charles"))),
		Clause(Comp("parent", Atom("philip"), Atom("charles"))),
		Clause(Comp("parent", Atom("charles"), Atom("william"))),
		Clause(Comp("parent", Atom("diana"), Atom("william"))),
		Clause(Comp("parent", Atom("charles"), Atom("harry"))),
		Clause(Comp("parent", Atom("diana"), Atom("harry"))),

		Clause(Comp("grandparent", Var("G"), Var("C")),
			Comp("parent", Var("G"), Var("P")),
			Comp("parent", Var("P"), Var("C"))),
		Clause(Comp("partner", Var("P1"), Var("P2")),
			Comp("parent", Var("P1"), Var("C")),
			Comp("parent", Var("P2"), Var("C")),
			Comp("\\=", Var("P1"), Var("P2"))),
	}
	wamClauses, _ := wam.CompileClauses(logicClauses)
	for _, clause := range wamClauses {
		m.AddClause(clause)
	}
	bindings, _ := m.RunQuery(
		Comp("grandparent", Var("G1"), Atom("harry")),
		Comp("partner", Var("G1"), Var("G2")),
	)
	for x, term := range bindings {
		fmt.Println(x, "=", term)
	}
	// Unordered output: G1 = elizabeth
	// G2 = philip
}
