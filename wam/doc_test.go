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

func ExampleDecodeInstruction() {
	instrs := []wam.Instruction{
		wam.DecodeInstruction(Comp("allocate", Int(2))),
		wam.DecodeInstruction(Comp("get_struct", Atom("f/2"), Var("X1"))),
		wam.DecodeInstruction(Comp("switch_on_constant", Dict(
			Atom("a"), Comp("instr", Ptr(nil), Int(1)),
			Atom("b"), Comp("instr", Ptr(nil), Int(1))))),
		wam.DecodeInstruction(Atom("deallocate")),
		wam.DecodeInstruction(Comp("proceed", Atom("run"))),
	}
	for _, instr := range instrs {
		fmt.Println(instr)
	}
	// Output: allocate 2
	// get_struct f/2, A1
	// switch_on_constant
	//	a: <nil>[1]
	//	b: <nil>[1]
	// deallocate
	// proceed run
}
