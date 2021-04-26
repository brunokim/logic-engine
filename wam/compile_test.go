package wam_test

import (
	"testing"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
)

func TestCompile(t *testing.T) {
	tests := []struct {
		clause *logic.Clause
		want   *wam.Clause
	}{
		{
			dsl.Clause(comp("=", var_("X"), var_("X"))),
			clause(functor{"=", 2},
				get_variable{reg(2), reg(0)},
				get_value{reg(2), reg(1)},
				proceed{}),
		},
		{
			dsl.Clause(comp("nat", comp("s", var_("X"))),
				comp("nat", var_("X"))),
			clause(functor{"nat", 1},
				get_struct{functor{"s", 1}, reg(0)},
				unify_variable{reg(1)},
				put_value{reg(1), reg(0)},
				execute{functor{"nat", 1}},
			),
		},
		{
			dsl.Clause(comp("take-1", var_("H"), list(var_("H")), ilist(var_("H"), var_("_")))),
			clause(functor{"take-1", 3},
				get_variable{reg(3), reg(0)},
				get_pair{list_pair, reg(1)},
				unify_value{reg(3)},
				unify_constant{watom("[]")},
				get_pair{list_pair, reg(2)},
				unify_value{reg(3)},
				unify_void{1},
				proceed{}),
		},
		{
			dsl.Clause(comp("f", var_("X"), var_("Y")),
				comp("g", var_("X"), var_("Z")),
				comp("h", var_("Y"), var_("Z"))),
			clause(functor{"f", 2},
				allocate{2},
				// head
				get_variable{reg(2), reg(0)},
				get_variable{stack(0), reg(1)},
				// body 1
				put_value{reg(2), reg(0)},
				put_variable{stack(1), reg(1)},
				call{functor{"g", 2}},
				// body 2
				put_value{stack(0), reg(0)},
				put_value{stack(1), reg(1)},
				deallocate{},
				execute{functor{"h", 2}}),
		},
		{
			dsl.Clause(comp("mul", int_(0), var_("_"), int_(0))),
			clause(functor{"mul", 3},
				get_constant{wint(0), reg(0)},
				get_constant{wint(0), reg(2)},
				proceed{}),
		},
		{
			dsl.Clause(comp("non-empty", ilist(var_("_"), var_("_")))),
			clause(functor{"non-empty", 1},
				get_pair{list_pair, reg(0)},
				unify_void{2},
				proceed{}),
		},
		{
			dsl.Clause(comp(">=3", comp("s", comp("s", comp("s", var_("_")))))),
			clause(functor{">=3", 1},
				get_struct{functor{"s", 1}, reg(0)},
				unify_variable{reg(1)},
				get_struct{functor{"s", 1}, reg(1)},
				unify_variable{reg(2)},
				get_struct{functor{"s", 1}, reg(2)},
				unify_void{1},
				proceed{}),
		},
		{
			dsl.Clause(comp("f", list(comp("g", atom("a")), comp("h", atom("b"))))),
			clause(functor{"f", 1},
				get_pair{list_pair, reg(0)},
				unify_variable{reg(1)},
				unify_variable{reg(2)},
				get_struct{functor{"g", 1}, reg(1)},
				unify_constant{watom("a")},
				get_pair{list_pair, reg(2)},
				unify_variable{reg(3)},
				unify_constant{watom("[]")},
				get_struct{functor{"h", 1}, reg(3)},
				unify_constant{watom("b")},
				proceed{}),
		},
		{
			dsl.Clause(comp("query"),
				comp("make", comp("p", var_("Z"), comp("h", var_("Z"), var_("W")), comp("f", var_("W"))))),
			clause(functor{"query", 0},
				put_struct{functor{"h", 2}, reg(3)},
				set_variable{reg(1)},
				set_variable{reg(2)},
				put_struct{functor{"f", 1}, reg(4)},
				set_value{reg(2)},
				put_struct{functor{"p", 3}, reg(0)},
				set_value{reg(1)},
				set_value{reg(3)},
				set_value{reg(4)},
				execute{functor{"make", 1}}),
		},
		{
			dsl.Clause(comp("mul", var_("A"), comp("s", var_("B")), comp("s", var_("P"))),
				comp("mul", var_("A"), var_("B"), var_("P1")),
				comp("add", var_("B"), var_("P1"), var_("P"))),
			clause(functor{"mul", 3},
				allocate{3},
				// head
				get_variable{reg(3), reg(0)},
				get_struct{functor{"s", 1}, reg(1)},
				unify_variable{stack(0)},
				get_struct{functor{"s", 1}, reg(2)},
				unify_variable{stack(1)},
				// body 1
				put_value{reg(3), reg(0)},
				put_value{stack(0), reg(1)},
				put_variable{stack(2), reg(2)},
				call{functor{"mul", 3}},
				// body 2
				put_value{stack(0), reg(0)},
				put_value{stack(2), reg(1)},
				put_value{stack(1), reg(2)},
				deallocate{},
				execute{functor{"add", 3}}),
		},
		{
			dsl.Clause(comp("if*", var_("Cond"), var_("Then"), var_("Else")),
				var_("Cond"),
				atom("!"),
				var_("Then")),
			clause(functor{"if*", 3},
				allocate{1},
				// head
				get_variable{reg(3), reg(0)},
				get_variable{stack(0), reg(1)},
				get_variable{reg(4), reg(2)},
				// body
				put_value{reg(3), reg(0)},
				call_meta{reg(0), nil},
				cut{},
				put_value{stack(0), reg(0)},
				deallocate{},
				execute_meta{reg(0), nil}),
		},
	}
	for _, test := range tests {
		got := wam.Compile(test.clause)
		if diff := cmp.Diff(test.want, got, cmpopts.EquateEmpty()); diff != "" {
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
	vowelA = clause(functor{"vowel", 1},
		try_me_else{instr{vowelE, 0}},
		get_constant{watom("a"), reg(0)},
		proceed{})
	vowelE = clause(functor{"vowel", 1},
		retry_me_else{instr{vowelI, 0}},
		get_constant{watom("e"), reg(0)},
		proceed{})
	vowelI = clause(functor{"vowel", 1},
		retry_me_else{instr{vowelO, 0}},
		get_constant{watom("i"), reg(0)},
		proceed{})
	vowelO = clause(functor{"vowel", 1},
		retry_me_else{instr{vowelU, 0}},
		get_constant{watom("o"), reg(0)},
		proceed{})
	vowelU = clause(functor{"vowel", 1},
		trust_me{},
		get_constant{watom("u"), reg(0)},
		proceed{})
	vowelIndex = &wam.Clause{
		Functor:      functor{"vowel", 1},
		NumRegisters: 1,
		Code: []wam.Instruction{
			switch_on_term{
				IfVar:      instr{vowelA, 0},
				IfConstant: instr{nil, 1},
			},
			switch_on_constant{map[constant]instr{
				watom("a"): instr{vowelA, 1},
				watom("e"): instr{vowelE, 1},
				watom("i"): instr{vowelI, 1},
				watom("o"): instr{vowelO, 1},
				watom("u"): instr{vowelU, 1},
			}},
		},
	}

	// f(a) :- a().
	// f(a) :- b().
	// f(b).
	// f([]).
	// f(g(0)).
	// f([H|T]).
	// f(g(1)).
	// f([x]).
	// f([y]).
	fAtomA1 = clause(functor{"f", 1},
		try_me_else{instr{fAtomA2, 0}},
		get_constant{watom("a"), reg(0)},
		execute{functor{"a", 0}})
	fAtomA2 = clause(functor{"f", 1},
		retry_me_else{instr{fAtomB, 0}},
		get_constant{watom("a"), reg(0)},
		execute{functor{"b", 0}})
	fAtomB = clause(functor{"f", 1},
		retry_me_else{instr{fAtomNil, 0}},
		get_constant{watom("b"), reg(0)},
		proceed{})
	fAtomNil = clause(functor{"f", 1},
		retry_me_else{instr{fStructG1, 0}},
		get_constant{watom("[]"), reg(0)},
		proceed{})
	fStructG1 = clause(functor{"f", 1},
		retry_me_else{instr{fList1, 0}},
		get_struct{functor{"g", 1}, reg(0)},
		unify_constant{wint(0)},
		proceed{})
	fList1 = clause(functor{"f", 1},
		retry_me_else{instr{fStructG2, 0}},
		get_pair{list_pair, reg(0)},
		unify_variable{reg(1)},
		unify_variable{reg(2)},
		proceed{})
	fStructG2 = clause(functor{"f", 1},
		retry_me_else{instr{fList2, 0}},
		get_struct{functor{"g", 1}, reg(0)},
		unify_constant{watom("1")},
		proceed{})
	fList2 = clause(functor{"f", 1},
		retry_me_else{instr{fList3, 0}},
		get_pair{list_pair, reg(0)},
		unify_constant{watom("x")},
		unify_constant{watom("[]")},
		proceed{})
	fList3 = clause(functor{"f", 1},
		trust_me{},
		get_pair{list_pair, reg(0)},
		unify_constant{watom("y")},
		unify_constant{watom("[]")},
		proceed{})
	fIndex = &wam.Clause{
		Functor:      functor{"f", 1},
		NumRegisters: 3,
		Code: []wam.Instruction{
			/*0*/ switch_on_term{
				IfVar:      instr{fAtomA1, 0},
				IfConstant: instr{nil, 1},
				IfStruct:   instr{nil, 4},
				IfPair:     instr{nil, 7},
			},
			/*1*/ switch_on_constant{map[constant]instr{
				watom("a"):  instr{nil, 2},
				watom("b"):  instr{fAtomB, 1},
				watom("[]"): instr{fAtomNil, 1},
			}},
			/*2*/ try{instr{fAtomA1, 1}},
			/*3*/ trust{instr{fAtomA2, 1}},
			/*4*/ switch_on_struct{map[wam.Functor]instr{
				functor{"g", 1}: instr{nil, 5},
			}},
			/*5*/ try{instr{fStructG1, 1}},
			/*6*/ trust{instr{fStructG2, 1}},
			/*7*/ try{instr{fList1, 1}},
			/*8*/ retry{instr{fList2, 1}},
			/*9*/ trust{instr{fList3, 1}},
		},
	}
)

func TestCompileClauses(t *testing.T) {
	tests := []struct {
		clauses []*logic.Clause
		want    []*wam.Clause
	}{
		{
			dsl.Clauses(
				dsl.Clause(comp("vowel", atom("a"))),
				dsl.Clause(comp("vowel", atom("e"))),
				dsl.Clause(comp("vowel", atom("i"))),
				dsl.Clause(comp("vowel", atom("o"))),
				dsl.Clause(comp("vowel", atom("u")))),
			[]*wam.Clause{vowelIndex},
		},
		{

			dsl.Clauses(
				dsl.Clause(comp("f", atom("a")), comp("a")),
				dsl.Clause(comp("f", atom("a")), comp("b")),
				dsl.Clause(comp("f", atom("b"))),
				dsl.Clause(comp("f", atom("[]"))),
				dsl.Clause(comp("f", comp("g", int_(0)))),
				dsl.Clause(comp("f", ilist(var_("H"), var_("T")))),
				dsl.Clause(comp("f", comp("g", atom("1")))),
				dsl.Clause(comp("f", list(atom("x")))),
				dsl.Clause(comp("f", list(atom("y"))))),
			[]*wam.Clause{fIndex},
		},
		{
			dsl.Clauses(
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
			[]*wam.Clause{vowelIndex, fIndex},
		},
	}
	// Need to ignore self-referential fields because go-cmp can't handle them.
	opts := cmp.Options{
		cmpopts.IgnoreFields(switch_on_term{},
			"IfConstant.Clause",
			"IfStruct.Clause",
			"IfPair.Clause"),
		cmp.Transformer("switch_on_constant.Continuation", func(cont map[constant]instr) map[constant]instr {
			m := make(map[constant]instr)
			for key, ins := range cont {
				if ins.Pos == 1 {
					m[key] = ins
				} else {
					m[key] = instr{Clause: nil, Pos: ins.Pos}
				}
			}
			return m
		}),
		cmp.Transformer("switch_on_struct.Continuation", func(cont map[functor]instr) map[functor]instr {
			m := make(map[functor]instr)
			for key, ins := range cont {
				if ins.Pos == 1 {
					m[key] = ins
				} else {
					m[key] = instr{Clause: nil, Pos: ins.Pos}
				}
			}
			return m
		}),
	}
	for _, test := range tests {
		got, err := wam.CompileClauses(test.clauses)
		if err != nil {
			t.Fatalf("want nil, got %v", err)
		}
		if diff := cmp.Diff(test.want, got, opts); diff != "" {
			t.Errorf("%v: (-want, +got)%s", test.clauses, diff)
		}
	}
}
