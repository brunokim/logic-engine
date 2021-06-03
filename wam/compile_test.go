package wam_test

import (
	"testing"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/test_helpers"
	"github.com/brunokim/logic-engine/wam"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

func emptyBuiltinFunc(m *wam.Machine, addrs []wam.Addr) (wam.InstrAddr, error) {
	return wam.InstrAddr{}, nil
}

func TestCompile(t *testing.T) {
	tests := []struct {
		clause *logic.Clause
		want   *wam.Clause
	}{
		{
			dsl.Clause(comp("=", var_("X"), var_("X"))),
			wam.DecodeClause(indicator("=", 2),
				comp("get_variable", var_("X2"), var_("X0")),
				comp("get_value", var_("X2"), var_("X1")),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp("nat", comp("s", var_("X"))),
				comp("nat", var_("X"))),
			wam.DecodeClause(indicator("nat", 1),
				comp("get_struct", atom("s/1"), var_("X0")),
				comp("unify_variable", var_("X1")),
				comp("put_value", var_("X1"), var_("X0")),
				comp("execute", atom("nat/1")),
			),
		},
		{
			dsl.Clause(comp("take-1", var_("H"), list(var_("H")), ilist(var_("H"), var_("_")))),
			wam.DecodeClause(indicator("take-1", 3),
				comp("get_variable", var_("X3"), var_("X0")),
				comp("get_pair", atom("list"), var_("X1")),
				comp("unify_value", var_("X3")),
				comp("unify_constant", atom("[]")),
				comp("get_pair", atom("list"), var_("X2")),
				comp("unify_value", var_("X3")),
				comp("unify_void"),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp("f", var_("X"), var_("Y")),
				comp("g", var_("X"), var_("Z")),
				comp("h", var_("Y"), var_("Z"))),
			wam.DecodeClause(indicator("f", 2),
				comp("allocate", int_(2)),
				// head
				comp("get_variable", var_("X2"), var_("X0")),
				comp("get_variable", var_("Y0"), var_("X1")),
				// body 1
				comp("put_value", var_("X2"), var_("X0")),
				comp("put_variable", var_("Y1"), var_("X1")),
				comp("call", atom("g/2")),
				// body 2
				comp("put_value", var_("Y0"), var_("X0")),
				comp("put_value", var_("Y1"), var_("X1")),
				comp("deallocate"),
				comp("execute", atom("h/2"))),
		},
		{
			dsl.Clause(comp("mul", int_(0), var_("_"), int_(0))),
			wam.DecodeClause(indicator("mul", 3),
				comp("get_constant", int_(0), var_("X0")),
				comp("get_constant", int_(0), var_("X2")),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp("non-empty", ilist(var_("_"), var_("_")))),
			wam.DecodeClause(indicator("non-empty", 1),
				comp("get_pair", atom("list"), var_("X0")),
				comp("unify_void"),
				comp("unify_void"),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp(">=3", comp("s", comp("s", comp("s", var_("_")))))),
			wam.DecodeClause(indicator(">=3", 1),
				comp("get_struct", atom("s/1"), var_("X0")),
				comp("unify_variable", var_("X1")),
				comp("get_struct", atom("s/1"), var_("X1")),
				comp("unify_variable", var_("X2")),
				comp("get_struct", atom("s/1"), var_("X2")),
				comp("unify_void"),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp("f", list(comp("g", atom("a")), comp("h", atom("b"))))),
			wam.DecodeClause(indicator("f", 1),
				comp("get_pair", atom("list"), var_("X0")),
				comp("unify_variable", var_("X1")),
				comp("unify_variable", var_("X2")),
				comp("get_struct", atom("g/1"), var_("X1")),
				comp("unify_constant", atom("a")),
				comp("get_pair", atom("list"), var_("X2")),
				comp("unify_variable", var_("X3")),
				comp("unify_constant", atom("[]")),
				comp("get_struct", atom("h/1"), var_("X3")),
				comp("unify_constant", atom("b")),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp("query"),
				comp("make", comp("p", var_("Z"), comp("h", var_("Z"), var_("W")), comp("f", var_("W"))))),
			wam.DecodeClause(indicator("query", 0),
				comp("put_struct", atom("h/2"), var_("X1")),
				comp("unify_variable", var_("X2")),
				comp("unify_variable", var_("X3")),
				comp("put_struct", atom("f/1"), var_("X4")),
				comp("unify_value", var_("X3")),
				comp("put_struct", atom("p/3"), var_("X0")),
				comp("unify_value", var_("X2")),
				comp("unify_value", var_("X1")),
				comp("unify_value", var_("X4")),
				comp("execute", atom("make/1"))),
		},
		{
			dsl.Clause(comp("mul", var_("A"), comp("s", var_("B")), comp("s", var_("P"))),
				comp("mul", var_("A"), var_("B"), var_("P1")),
				comp("add", var_("B"), var_("P1"), var_("P"))),
			wam.DecodeClause(indicator("mul", 3),
				comp("allocate", int_(3)),
				// head
				comp("get_variable", var_("X3"), var_("X0")),
				comp("get_struct", atom("s/1"), var_("X1")),
				comp("unify_variable", var_("Y0")),
				comp("get_struct", atom("s/1"), var_("X2")),
				comp("unify_variable", var_("Y1")),
				// body 1
				comp("put_value", var_("X3"), var_("X0")),
				comp("put_value", var_("Y0"), var_("X1")),
				comp("put_variable", var_("Y2"), var_("X2")),
				comp("call", atom("mul/3")),
				// body 2
				comp("put_value", var_("Y0"), var_("X0")),
				comp("put_value", var_("Y2"), var_("X1")),
				comp("put_value", var_("Y1"), var_("X2")),
				comp("deallocate"),
				comp("execute", atom("add/3"))),
		},
		{
			dsl.Clause(comp("if*", var_("Cond"), var_("Then"), var_("Else")),
				var_("Cond"),
				atom("!"),
				var_("Then")),
			wam.DecodeClause(indicator("if*", 3),
				comp("allocate", int_(1)),
				// head
				comp("get_variable", var_("X3"), var_("X0")),
				comp("get_variable", var_("Y0"), var_("X1")),
				comp("get_variable", var_("X4"), var_("X2")),
				// body
				comp("call_meta", var_("X3"), list()),
				comp("cut"),
				comp("deallocate"),
				comp("execute_meta", var_("Y0"), list())),
		},
		{
			dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
				comp("atom", var_("Term"), var_("L1"), var_("L2")),
				atom("!")),
			wam.DecodeClause(indicator("term", 3),
				comp("allocate", int_(0)),
				// head
				comp("get_variable", var_("X3"), var_("X0")),
				comp("get_variable", var_("X4"), var_("X1")),
				comp("get_variable", var_("X5"), var_("X2")),
				// atom(...)
				comp("put_value", var_("X3"), var_("X0")),
				comp("put_value", var_("X4"), var_("X1")),
				comp("put_value", var_("X5"), var_("X2")),
				comp("call", atom("atom/3")),
				// cut
				comp("cut"),
				comp("deallocate"),
				comp("proceed", atom("run"))),
		},
		// query :- =(
		//   p({a:1, b:2|P1}, P1, P2),
		//   p({a:1, c:3|P2}, _, _)).
		{
			dsl.Clause(atom("query"),
				comp("=",
					comp("p",
						idict(atom("a"), int_(1), atom("b"), int_(2), var_("P1")),
						var_("P1"),
						var_("P2")),
					comp("p",
						idict(atom("a"), int_(1), atom("c"), int_(3), var_("P2")),
						var_("_"),
						var_("_")))),
			wam.DecodeClause(indicator("query", 0),
				// a:1
				comp("put_pair", atom("assoc"), var_("X4")),
				comp("unify_constant", atom("a")),
				comp("unify_constant", int_(1)),
				// b:2
				comp("put_pair", atom("assoc"), var_("X6")),
				comp("unify_constant", atom("b")),
				comp("unify_constant", int_(2)),
				// {b:2|P1}
				comp("put_pair", atom("dict"), var_("X5")),
				comp("unify_value", var_("X6")),
				comp("unify_variable", var_("X7")), // P1=X7
				// {a:1, b:2|P1}
				comp("put_pair", atom("dict"), var_("X3")),
				comp("unify_value", var_("X4")),
				comp("unify_value", var_("X5")),
				// p({...}, P1, P2)
				comp("put_struct", atom("p/3"), var_("X2")),
				comp("unify_value", var_("X3")),
				comp("unify_value", var_("X7")),
				comp("unify_variable", var_("X8")), // P2=X8
				// a:1
				comp("put_pair", atom("assoc"), var_("X11")),
				comp("unify_constant", atom("a")),
				comp("unify_constant", int_(1)),
				// c:3
				comp("put_pair", atom("assoc"), var_("X13")),
				comp("unify_constant", atom("c")),
				comp("unify_constant", int_(3)),
				// {c:3|P2}
				comp("put_pair", atom("dict"), var_("X12")),
				comp("unify_value", var_("X13")),
				comp("unify_value", var_("X8")),
				// {a:1, c:3|P2}
				comp("put_pair", atom("dict"), var_("X10")),
				comp("unify_value", var_("X11")),
				comp("unify_value", var_("X12")),
				// p({...}, _, _)
				comp("put_struct", atom("p/3"), var_("X9")),
				comp("unify_value", var_("X10")),
				comp("unify_void"),
				comp("unify_void"),
				// =(..., ...)
				comp("=", var_("X2"), var_("X9")),
				comp("proceed", atom("run"))),
		},
		// query :- f(g(h(W), W, Z), g(h(Z))).
		{
			dsl.Clause(atom("query"),
				comp("f",
					comp("g", comp("h", var_("W")), var_("W"), var_("Z")),
					comp("g", comp("h", var_("Z"))))),
			wam.DecodeClause(indicator("query", 0),
				// h(W)
				comp("put_struct", atom("h/1"), var_("X2")),
				comp("unify_variable", var_("X3")), // W=X3
				// g(., W, Z)
				comp("put_struct", atom("g/3"), var_("X0")),
				comp("unify_value", var_("X2")),
				comp("unify_value", var_("X3")),
				comp("unify_variable", var_("X4")), // Z=X4
				// h(Z)
				comp("put_struct", atom("h/1"), var_("X5")),
				comp("unify_value", var_("X4")),
				// g(.)
				comp("put_struct", atom("g/1"), var_("X1")),
				comp("unify_value", var_("X5")),
				// :- f(., .).
				comp("execute", atom("f/2"))),
		},
		{
			dsl.Clause(comp("add", var_("Set"), idict(var_("X"), var_("X"), var_("Set")), var_("X"))),
			wam.DecodeClause(indicator("add", 3),
				comp("get_variable", var_("X3"), var_("X0")), // Set = X3
				comp("get_pair", atom("dict"), var_("X1")),   // {X:X|Set)
				comp("unify_variable", var_("X4")),
				comp("unify_value", var_("X3")),
				comp("get_variable", var_("X5"), var_("X2")), // X = X5
				// X:X
				comp("get_pair", atom("assoc"), var_("X4")),
				comp("unify_value", var_("X5")),
				comp("unify_value", var_("X5")),
				comp("proceed", atom("run"))),
		},
		{
			dsl.Clause(comp("f"),
				assoc(atom("list"), comp("length", var_("_")))),
			wam.DecodeClause(indicator("f", 0),
				comp("put_variable", var_("X1"), var_("X0")),
				comp("execute", atom("list"), atom("length/1"))),
		},
		{
			// X is temporary, because the whole clause is a single chunk.
			dsl.Clause(comp("f", var_("X")),
				comp("!"),
				comp("g", var_("X"))),
			wam.DecodeClause(indicator("f", 1),
				comp("get_variable", var_("X1"), var_("X0")),
				atom("neck_cut"),
				comp("put_value", var_("X1"), var_("X0")),
				comp("execute", atom("g/1"))),
		},
		{
			// X and Y are temporary.
			dsl.Clause(comp("f", var_("X")),
				comp("!"),
				comp("=", comp("s", var_("Y")), var_("X")),
				comp("g", var_("X"))),
			wam.DecodeClause(indicator("f", 1),
				comp("get_variable", var_("X2"), var_("X0")),
				atom("neck_cut"),
				comp("put_struct", atom("s/1"), var_("X3")),
				comp("unify_variable", var_("X4")),
				comp("=", var_("X3"), var_("X2")),
				comp("put_value", var_("X2"), var_("X0")),
				comp("execute", atom("g/1"))),
		},
		{
			// X and Z are temporary, because they are only present in a single chunk each.
			// Y is present in both chunks.
			dsl.Clause(comp("f", var_("X")),
				comp("!"),
				comp("=", comp("s", var_("Y")), var_("X")),
				comp("g", var_("X")),
				comp("=", var_("Z"), int_(1)),
				comp("h", var_("Y"), var_("Z"))),
			wam.DecodeClause(indicator("f", 1),
				comp("allocate", int_(1)),
				comp("get_variable", var_("X2"), var_("X0")),
				atom("neck_cut"),
				comp("put_struct", atom("s/1"), var_("X3")),
				comp("unify_variable", var_("Y0")),
				comp("=", var_("X3"), var_("X2")),
				comp("put_value", var_("X2"), var_("X0")),
				comp("call", atom("g/1")),
				comp("put_variable", var_("X4"), var_("X5")),
				comp("=", var_("X4"), int_(1)),
				comp("put_value", var_("Y0"), var_("X0")),
				comp("put_value", var_("X4"), var_("X1")),
				atom("deallocate"),
				comp("execute", atom("h/2"))),
		},
		{
			// Even though the whole if body is inline, Y is present in different branches,
			// which are different chunks, and thus is permanent. X is temporary.
			dsl.Clause(comp("f_aux", var_("X")),
				comp("->", comp("\\==", var_("X"), atom("[]")),
					comp("=", var_("Y"), var_("X")),
					comp("=", var_("Y"), list(int_(42)))),
				comp("f", var_("Y"))),
			wam.DecodeClause(indicator("f_aux", 1),
				comp("allocate", int_(1)),
				comp("get_variable", var_("X3"), var_("X0")),
				comp("put_variable", var_("Y0"), var_("X4")),
				comp("try", comp("instr", ptr(nil), int_(-1))),
				comp("trust", comp("instr", ptr(nil), int_(-2))),
				comp("label", int_(1)),
				comp("builtin", atom("\\=="), ptr(emptyBuiltinFunc), var_("X3"), atom("[]")),
				atom("cut"),
				comp("=", var_("Y0"), var_("X3")),
				comp("jump", comp("instr", ptr(nil), int_(-3))),
				comp("label", int_(2)),
				comp("put_pair", atom("list"), var_("X5")),
				comp("unify_constant", int_(42)),
				comp("unify_constant", atom("[]")),
				comp("=", var_("Y0"), var_("X5")),
				comp("label", int_(3)),
				comp("put_value", var_("Y0"), var_("X0")),
				atom("deallocate"),
				comp("execute", atom("f/1"))),
		},
	}
	for _, test := range tests {
		got, err := wam.Compile(test.clause)
		if err != nil {
			t.Fatal(err)
		}
		opts := cmp.Options{
			cmpopts.EquateEmpty(),
			cmpopts.IgnoreFields(instr{}, "Clause"),
			cmpopts.IgnoreFields(wam.Clause{}, "Vars"),
			cmpopts.IgnoreFields(wam.DecodeInstruction(comp("builtin", atom("b"), ptr(emptyBuiltinFunc))), "Func"),
		}
		if diff := cmp.Diff(test.want, got, opts); diff != "" {
			t.Errorf("%v: (-want, +got)%s", test.clause, diff)
		}
	}
}

var (
	// vowel(a).
	// vowel(e).
	// vowel(i).
	// vowel(o).
	// vowel(u).
	vowelA = wam.DecodeClause(indicator("vowel", 1),
		comp("try_me_else", comp("instr", ptr(vowelE), int_(0))),
		comp("get_constant", atom("a"), var_("X0")),
		comp("proceed", atom("run")))
	vowelE = wam.DecodeClause(indicator("vowel", 1),
		comp("retry_me_else", comp("instr", ptr(vowelI), int_(0))),
		comp("get_constant", atom("e"), var_("X0")),
		comp("proceed", atom("run")))
	vowelI = wam.DecodeClause(indicator("vowel", 1),
		comp("retry_me_else", comp("instr", ptr(vowelO), int_(0))),
		comp("get_constant", atom("i"), var_("X0")),
		comp("proceed", atom("run")))
	vowelO = wam.DecodeClause(indicator("vowel", 1),
		comp("retry_me_else", comp("instr", ptr(vowelU), int_(0))),
		comp("get_constant", atom("o"), var_("X0")),
		comp("proceed", atom("run")))
	vowelU = wam.DecodeClause(indicator("vowel", 1),
		comp("trust_me"),
		comp("get_constant", atom("u"), var_("X0")),
		comp("proceed", atom("run")))
	vowelIndex = wam.DecodeClause(indicator("vowel", 1),
		comp("switch_on_term",
			comp("instr", ptr(vowelA), int_(0)),
			comp("instr", ptr(nil), int_(-1)),
			comp("instr", ptr(nil), int_(0)),
			comp("instr", ptr(nil), int_(0)),
			comp("instr", ptr(nil), int_(0)),
			comp("instr", ptr(nil), int_(0))),
		comp("label", int_(1)),
		comp("switch_on_constant", dict(
			atom("a"), comp("instr", ptr(vowelA), int_(1)),
			atom("e"), comp("instr", ptr(vowelE), int_(1)),
			atom("i"), comp("instr", ptr(vowelI), int_(1)),
			atom("o"), comp("instr", ptr(vowelO), int_(1)),
			atom("u"), comp("instr", ptr(vowelU), int_(1)))))

	// f(a) :- a().
	// f(a) :- b().
	// f(b).
	// f([]).
	// f(g(0)).
	// f([H|T]).
	// f(g(1)).
	// f([x]).
	// f([y]).
	fAtomA1 = wam.DecodeClause(indicator("f", 1),
		comp("try_me_else", comp("instr", ptr(fAtomA2), int_(0))),
		comp("get_constant", atom("a"), var_("X0")),
		comp("execute", atom("a/0")))
	fAtomA2 = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fAtomB), int_(0))),
		comp("get_constant", atom("a"), var_("X0")),
		comp("execute", atom("b/0")))
	fAtomB = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fAtomNil), int_(0))),
		comp("get_constant", atom("b"), var_("X0")),
		comp("proceed", atom("run")))
	fAtomNil = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fStructG1), int_(0))),
		comp("get_constant", atom("[]"), var_("X0")),
		comp("proceed", atom("run")))
	fStructG1 = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fList1), int_(0))),
		comp("get_struct", atom("g/1"), var_("X0")),
		comp("unify_constant", int_(0)),
		comp("proceed", atom("run")))
	fList1 = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fStructG2), int_(0))),
		comp("get_pair", atom("list"), var_("X0")),
		comp("unify_variable", var_("X1")),
		comp("unify_variable", var_("X2")),
		comp("proceed", atom("run")))
	fStructG2 = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fList2), int_(0))),
		comp("get_struct", atom("g/1"), var_("X0")),
		comp("unify_constant", atom("1")),
		comp("proceed", atom("run")))
	fList2 = wam.DecodeClause(indicator("f", 1),
		comp("retry_me_else", comp("instr", ptr(fList3), int_(0))),
		comp("get_pair", atom("list"), var_("X0")),
		comp("unify_constant", atom("x")),
		comp("unify_constant", atom("[]")),
		comp("proceed", atom("run")))
	fList3 = wam.DecodeClause(indicator("f", 1),
		comp("trust_me"),
		comp("get_pair", atom("list"), var_("X0")),
		comp("unify_constant", atom("y")),
		comp("unify_constant", atom("[]")),
		comp("proceed", atom("run")))
	fIndex = wam.DecodeClause(indicator("f", 1),
		comp("switch_on_term",
			comp("instr", ptr(fAtomA1), int_(0)),
			comp("instr", ptr(nil), int_(-1)),
			comp("instr", ptr(nil), int_(-3)),
			comp("instr", ptr(nil), int_(-5)),
			comp("instr", ptr(nil), int_(0)),
			comp("instr", ptr(nil), int_(0))),
		// Constants
		comp("label", int_(1)),
		comp("switch_on_constant", dict(
			atom("a"), comp("instr", ptr(nil), int_(-2)),
			atom("b"), comp("instr", ptr(fAtomB), int_(1)),
			atom("[]"), comp("instr", ptr(fAtomNil), int_(1)))),
		comp("label", int_(2)),
		comp("try", comp("instr", ptr(fAtomA1), int_(1))),
		comp("trust", comp("instr", ptr(fAtomA2), int_(1))),
		// Structs
		comp("label", int_(3)),
		comp("switch_on_struct", dict(
			atom("g/1"), comp("instr", ptr(nil), int_(-4)))),
		comp("label", int_(4)),
		comp("try", comp("instr", ptr(fStructG1), int_(1))),
		comp("trust", comp("instr", ptr(fStructG2), int_(1))),
		// Lists
		comp("label", int_(5)),
		comp("try", comp("instr", ptr(fList1), int_(1))),
		comp("retry", comp("instr", ptr(fList2), int_(1))),
		comp("trust", comp("instr", ptr(fList3), int_(1))))
)

func TestCompileClauses(t *testing.T) {
	tests := []struct {
		rules []logic.Rule
		want  map[wam.Functor]*wam.Clause
	}{
		{
			dsl.Rules(
				dsl.Clause(comp("vowel", atom("a"))),
				dsl.Clause(comp("vowel", atom("e"))),
				dsl.Clause(comp("vowel", atom("i"))),
				dsl.Clause(comp("vowel", atom("o"))),
				dsl.Clause(comp("vowel", atom("u")))),
			map[wam.Functor]*wam.Clause{wam.Functor{"vowel", 1}: vowelIndex},
		},
		{

			dsl.Rules(
				dsl.Clause(comp("f", atom("a")), comp("a")),
				dsl.Clause(comp("f", atom("a")), comp("b")),
				dsl.Clause(comp("f", atom("b"))),
				dsl.Clause(comp("f", atom("[]"))),
				dsl.Clause(comp("f", comp("g", int_(0)))),
				dsl.Clause(comp("f", ilist(var_("H"), var_("T")))),
				dsl.Clause(comp("f", comp("g", atom("1")))),
				dsl.Clause(comp("f", list(atom("x")))),
				dsl.Clause(comp("f", list(atom("y"))))),
			map[wam.Functor]*wam.Clause{wam.Functor{"f", 1}: fIndex},
		},
		{
			dsl.Rules(
				dsl.Clause(comp("vowel", atom("a"))),
				dsl.Clause(comp("vowel", atom("e"))),
				dsl.Clause(comp("vowel", atom("i"))),
				dsl.Clause(comp("vowel", atom("o"))),
				dsl.Clause(comp("vowel", atom("u"))),
				dsl.Clause(comp("f", atom("a")), comp("a")),
				dsl.Clause(comp("f", atom("a")), comp("b")),
				dsl.Clause(comp("f", atom("b"))),
				dsl.Clause(comp("f", atom("[]"))),
				dsl.Clause(comp("f", comp("g", int_(0)))),
				dsl.Clause(comp("f", ilist(var_("H"), var_("T")))),
				dsl.Clause(comp("f", comp("g", atom("1")))),
				dsl.Clause(comp("f", list(atom("x")))),
				dsl.Clause(comp("f", list(atom("y"))))),
			map[wam.Functor]*wam.Clause{
				wam.Functor{"vowel", 1}: vowelIndex,
				wam.Functor{"f", 1}:     fIndex,
			},
		},
	}
	// Need to ignore self-referential fields because go-cmp can't handle them.
	for _, test := range tests {
		pkg, err := wam.CompilePackage(test.rules, wam.KeepLabels{})
		if err != nil {
			t.Fatal(err)
		}
		got := pkg.Exported
		opts := cmp.Options{
			cmpopts.IgnoreFields(wam.Clause{}, "Pkg"),
			cmpopts.IgnoreFields(instr{}, "Clause"),
		}
		if diff := cmp.Diff(test.want, got, opts); diff != "" {
			t.Errorf("%v: (-want, +got)%s", test.rules, diff)
		}
	}
}

func TestCompilePackage(t *testing.T) {
	pkg1 := wam.NewPackage("pkg1")
	f1 := wam.Functor{"f", 1}
	f2 := wam.Functor{"f", 2}
	pkg1.Exported[f1] = &wam.Clause{Pkg: pkg1, Functor: f1}
	pkg1.Internal[f2] = &wam.Clause{Pkg: pkg1, Functor: f2}

	pkg2 := wam.NewPackage("pkg2")
	g1 := wam.Functor{"g", 1}
	pkg2.ImportedPkgs = []string{"pkg1"}
	pkg2.Exported[g1] = &wam.Clause{Pkg: pkg2, Functor: g1}

	pkg3, err := wam.CompilePackage([]logic.Rule{
		dsl.Clause(comp("package", atom("pkg3"),
			list(atom("pkg1"), atom("pkg2")),
			list(atom("f/1"), atom("g/2")))),
		dsl.Clause(comp("f", atom("a"))),
		dsl.Clause(comp("g", var_("A"), var_("B")),
			comp("g", var_("A"), var_("B"), int_(3))),
		dsl.Clause(comp("g", int_(1), int_(2), var_("_"))),
	})
	if err != nil {
		t.Fatal(err)
	}
	g2 := wam.Functor{"g", 2}
	g3 := wam.Functor{"g", 3}
	want := &wam.Package{
		Name:         "pkg3",
		ImportedPkgs: []string{"pkg1", "pkg2"},
		Exported: map[wam.Functor]*wam.Clause{
			f1: &wam.Clause{Pkg: pkg3, Functor: f1},
			g2: &wam.Clause{Pkg: pkg3, Functor: g2},
		},
		Internal: map[wam.Functor]*wam.Clause{
			g3: &wam.Clause{Pkg: pkg3, Functor: g3},
		},
	}
	opts := cmp.Options{
		test_helpers.IgnoreUnexported,
		// TODO: somehow test that Pkg field is as expected. cmp does not support
		// recursive references.
		cmpopts.IgnoreFields(wam.Clause{}, "Pkg", "NumRegisters", "Vars", "Code"),
	}
	if diff := cmp.Diff(want, pkg3, opts...); diff != "" {
		t.Errorf("(-want, +got):\n%s", diff)
	}
}
