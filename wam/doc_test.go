package wam_test

import (
    "fmt"

    "github.com/brunokim/logic-engine/logic"
    "github.com/brunokim/logic-engine/dsl"
    "github.com/brunokim/logic-engine/wam"
)

func Example() {
    m := wam.NewMachine()
    clauses := []*logic.Clause{
        dsl.Clause(dsl.Comp("parent", dsl.Atom("elizabeth"), dsl.Atom("charles"))),
        dsl.Clause(dsl.Comp("parent", dsl.Atom("philip"), dsl.Atom("charles"))),
        dsl.Clause(dsl.Comp("parent", dsl.Atom("charles"), dsl.Atom("william"))),
        dsl.Clause(dsl.Comp("parent", dsl.Atom("diana"), dsl.Atom("william"))),
        dsl.Clause(dsl.Comp("parent", dsl.Atom("charles"), dsl.Atom("harry"))),
        dsl.Clause(dsl.Comp("parent", dsl.Atom("diana"), dsl.Atom("harry"))),

        dsl.Clause(dsl.Comp("grandparent", dsl.Var("G"), dsl.Var("C")),
            dsl.Comp("parent", dsl.Var("G"), dsl.Var("P")),
            dsl.Comp("parent", dsl.Var("P"), dsl.Var("C"))),
        dsl.Clause(dsl.Comp("partner", dsl.Var("P1"), dsl.Var("P2")),
            dsl.Comp("parent", dsl.Var("P1"), dsl.Var("C")),
            dsl.Comp("parent", dsl.Var("P2"), dsl.Var("C")),
            dsl.Comp("\\=", dsl.Var("P1"), dsl.Var("P2"))),
    }
    compiledClauses, _ := wam.CompileClauses(clauses)
    for _, clause := range compiledClauses {
        m.AddClause(clause)
    }
    bindings, _ := m.RunQuery(
        dsl.Comp("grandparent", dsl.Var("G1"), dsl.Atom("harry")),
        dsl.Comp("partner", dsl.Var("G1"), dsl.Var("G2")),
    )
    for x, term := range bindings {
        fmt.Println(x, "=", term)
    }
    // Unordered output: G1 = elizabeth
    // G2 = philip
}
