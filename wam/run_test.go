package wam_test

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/test_helpers"
	"github.com/brunokim/logic-engine/wam"
)

var (
	// ?- p(Z, h(Z, W), f(W))
	queryInstrs = []wam.Instruction{
		put_struct{functor{"h", 2}, reg(2)},
		unify_variable{reg(1)},
		unify_variable{reg(4)},
		put_struct{functor{"f", 1}, reg(3)},
		unify_value{reg(4)},
		put_struct{functor{"p", 3}, reg(0)},
		unify_value{reg(1)},
		unify_value{reg(2)},
		unify_value{reg(3)},
	}

	// p(f(X), h(Y, f(a)), Y).
	programInstrs = []wam.Instruction{
		get_struct{functor{"p", 3}, reg(0)},
		unify_variable{reg(1)},
		unify_variable{reg(2)},
		unify_variable{reg(3)},
		get_struct{functor{"f", 1}, reg(1)},
		unify_variable{reg(5)},
		get_struct{functor{"h", 2}, reg(2)},
		unify_value{reg(3)},
		unify_variable{reg(5)},
		get_struct{functor{"f", 1}, reg(5)},
		unify_variable{reg(6)},
		get_constant{watom("a"), reg(6)},
	}
)

func TestRun_BuildQuery(t *testing.T) {
	query := clause(functor{}, queryInstrs...)
	query.Code = append(query.Code, halt{})
	m := wam.NewMachine()
	m.AddClause(query)
	m.IterLimit = 10
	m.DebugFilename = "debugtest/run-build-query.jsonl"
	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
	z, w := m.Reg[1], m.Reg[4]
	if z.String() != "_X1" {
		t.Errorf("Z = %v != _X1", z)
	}
	if w.String() != "_X2" {
		t.Errorf("W = %v != _X2", z)
	}
}

func TestRun_BuildQueryAndProgram(t *testing.T) {
	instrs := []wam.Instruction{}
	instrs = append(instrs, queryInstrs...)
	instrs = append(instrs, programInstrs...)
	instrs = append(instrs, halt{})
	query := clause(functor{}, instrs...)
	m := wam.NewMachine()
	m.AddClause(query)
	m.IterLimit = 30
	m.DebugFilename = "debugtest/run-build-query-and-program.jsonl"
	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
	z, w := m.Reg[1], m.Reg[4]
	zWant, wWant := "&f(&&f(&a))", "&f(&a)"
	if z.String() != zWant {
		t.Errorf("Z = %v != %s", z, zWant)
	}
	if w.String() != wWant {
		t.Errorf("W = %v != %s", w, wWant)
	}
}

func TestRun_Call(t *testing.T) {
	query := &wam.Clause{Code: queryInstrs, NumRegisters: 5}
	query.Code = append(query.Code, call{functor{"p", 3}}, halt{})

	program := &wam.Clause{Code: programInstrs, Functor: functor{"p", 3}, NumRegisters: 7}
	program.Code = append(program.Code, proceed{})

	m := wam.NewMachine()
	m.AddClause(query)
	m.AddClause(program)
	m.IterLimit = 40
	m.DebugFilename = "debugtest/run-call.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	z, w := m.Reg[1], m.Reg[4]
	zWant, wWant := "&f(&&f(&a))", "&f(&a)"
	if z.String() != zWant {
		t.Errorf("Z = %v != %s", z, zWant)
	}
	if w.String() != wWant {
		t.Errorf("W = %v != %s", w, wWant)
	}
}

var (
	// p(X, Y) :- q(X, Z), r(Z, Y).
	p2 = clause(functor{"p", 2},
		allocate{2},
		get_variable{reg(2), reg(0)},
		get_variable{stack(0), reg(1)},
		put_value{reg(2), reg(0)},
		put_variable{stack(1), reg(1)},
		call{functor{"q", 2}},
		put_value{stack(1), reg(0)},
		put_value{stack(0), reg(1)},
		call{functor{"r", 2}},
		deallocate{},
		proceed{})

	// q(a, f(a)).
	q2 = clause(functor{"q", 2},
		get_constant{watom("a"), reg(0)},
		get_struct{functor{"f", 1}, reg(1)},
		unify_constant{watom("a")},
		proceed{})

	// r(f(A), f(B)) :- s(B), t(A).
	r2 = clause(functor{"r", 2},
		allocate{1},
		get_struct{functor{"f", 1}, reg(0)},
		unify_variable{stack(0)},
		get_struct{functor{"f", 1}, reg(1)},
		unify_variable{reg(2)},
		put_value{reg(2), reg(0)},
		call{functor{"s", 1}},
		put_value{stack(0), reg(0)},
		call{functor{"t", 1}},
		deallocate{},
		proceed{})

	// s(g(b)).
	s1 = clause(functor{"s", 1},
		get_struct{functor{"g", 1}, reg(0)},
		unify_constant{watom("b")},
		proceed{})

	// t(a).
	t1 = clause(functor{"t", 1},
		get_constant{watom("a"), reg(0)},
		proceed{})
)

func TestRun_Allocate(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(p2)
	m.AddClause(q2)
	m.AddClause(r2)
	m.AddClause(s1)
	m.AddClause(t1)

	// ?- p(X, Y).
	m.AddClause(clause(functor{},
		// Save X and Y at regs X3 and X4, that are not used by any other clauses.
		put_variable{reg(3), reg(0)},
		put_variable{reg(4), reg(1)},
		call{functor{"p", 2}},
		halt{}))
	m.IterLimit = 100
	m.DebugFilename = "debugtest/run-allocate.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	x, y := m.Reg[3], m.Reg[4]
	xWant, yWant := "&a", "&f(&g(b))"
	if x.String() != xWant {
		t.Errorf("X = %v != %s", x, xWant)
	}
	if y.String() != yWant {
		t.Errorf("Y = %v != %s", y, yWant)
	}
}

var (
	// color(red).
	// color(green).
	// color(blue).
	colorRed = clause(functor{"color", 1},
		try_me_else{instr{colorGreen, 0}},
		get_constant{watom("red"), reg(0)},
		proceed{})
	colorGreen = clause(functor{"color", 1},
		retry_me_else{instr{colorBlue, 0}},
		get_constant{watom("green"), reg(0)},
		proceed{})
	colorBlue = clause(functor{"color", 1},
		trust_me{},
		get_constant{watom("blue"), reg(0)},
		proceed{})

	// bit(false).
	// bit(true).
	bitFalse = clause(functor{"bit", 1},
		try_me_else{instr{bitTrue, 0}},
		get_constant{watom("false"), reg(0)},
		proceed{})
	bitTrue = clause(functor{"bit", 1},
		trust_me{},
		get_constant{watom("true"), reg(0)},
		proceed{})

	// bit_color(Bit, Color) :- bit(Bit), color(Color).
	bitColor = clause(functor{"bit_color", 2},
		allocate{1},
		get_variable{reg(2), reg(0)},
		get_variable{stack(0), reg(1)},
		put_value{reg(2), reg(0)},
		call{functor{"bit", 1}},
		put_value{stack(0), reg(0)},
		call{functor{"color", 1}},
		deallocate{},
		proceed{})
)

func TestRun_ChoicePoints(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(colorRed)
	m.AddClause(bitFalse)
	m.AddClause(bitColor)

	// ?- bit_color(true, green).
	m.AddClause(clause(functor{},
		put_constant{watom("true"), reg(0)},
		put_constant{watom("green"), reg(1)},
		call{functor{"bit_color", 2}},
		halt{},
	))
	m.IterLimit = 30
	m.DebugFilename = "debugtest/run-choicepoint.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
}

var (
	// a(X, Z) :- b(Y, X), c(Z, f(Y)).
	// b(A, p) :- d(A).
	// c(S, f(S)).
	// c(S, g(S)).
	// d(q0).
	// d(q1).
	// d(q2).
	a2 = clause(functor{"a", 2},
		allocate{2},
		get_variable{reg(2), reg(0)},
		get_variable{stack(0), reg(1)},
		put_variable{stack(1), reg(0)},
		put_value{reg(2), reg(1)},
		call{functor{"b", 2}},
		put_value{stack(0), reg(0)},
		put_struct{functor{"f", 1}, reg(1)},
		unify_value{stack(1)},
		call{functor{"c", 2}},
		deallocate{},
		proceed{})
	b2 = clause(functor{"b", 2},
		allocate{0},
		get_variable{reg(2), reg(0)},
		get_constant{watom("p"), reg(1)},
		put_value{reg(2), reg(0)},
		call{functor{"d", 1}},
		deallocate{},
		proceed{})
	c2_1 = clause(functor{"c", 2},
		try_me_else{instr{c2_2, 0}},
		get_variable{reg(2), reg(0)},
		get_struct{functor{"f", 1}, reg(1)},
		unify_value{reg(2)},
		proceed{})
	c2_2 = clause(functor{"c", 2},
		trust_me{},
		get_variable{reg(2), reg(0)},
		get_struct{functor{"g", 1}, reg(1)},
		unify_value{reg(2)},
		proceed{})
	d1_1 = clause(functor{"d", 1},
		try_me_else{instr{d1_2, 0}},
		get_constant{watom("q0"), reg(0)},
		proceed{})
	d1_2 = clause(functor{"d", 1},
		retry_me_else{instr{d1_3, 0}},
		get_constant{watom("q1"), reg(0)},
		proceed{})
	d1_3 = clause(functor{"d", 1},
		trust_me{},
		get_constant{watom("q2"), reg(0)},
		proceed{})
)

func TestRun_Trail(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(a2)
	m.AddClause(b2)
	m.AddClause(c2_1)
	m.AddClause(d1_1)

	// ?- a(X, q1).
	m.AddClause(clause(functor{},
		// Save X at reg X3, that is not used by any other clauses.
		put_variable{reg(3), reg(0)},
		put_constant{watom("q1"), reg(1)},
		call{functor{"a", 2}},
		halt{}))
	m.IterLimit = 50
	m.DebugFilename = "debugtest/run-trail.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	x := m.Reg[3]
	xWant := "&p"
	if x.String() != xWant {
		t.Errorf("X = %v != %s", x, xWant)
	}
}

func TestRun_List(t *testing.T) {
	m := wam.NewMachine()
	// build_list((a . (b . []))).
	m.AddClause(clause(functor{"build_list", 1},
		get_pair{list_pair, reg(0)},
		unify_constant{watom("a")},
		unify_variable{reg(1)},
		get_pair{list_pair, reg(1)},
		unify_constant{watom("b")},
		unify_constant{watom("[]")},
		proceed{}))

	// =(X, X).
	m.AddClause(clause(functor{"=", 2},
		get_value{reg(0), reg(1)},
		proceed{}))

	// ?- build_list((a . T)), =(T, (X . [])).
	m.AddClause(clause(functor{},
		allocate{2},

		put_pair{list_pair, reg(0)},
		unify_constant{watom("a")},
		unify_variable{stack(0)},
		call{functor{"build_list", 1}},

		put_value{stack(0), reg(0)},
		put_pair{list_pair, reg(1)},
		unify_variable{stack(1)},
		unify_constant{watom("[]")},
		call{functor{"=", 2}},

		halt{}))
	m.IterLimit = 50
	m.DebugFilename = "debugtest/run-list.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	tail, x := m.Env.PermanentVars[0], m.Env.PermanentVars[1]
	tailWant, xWant := "&[b]", "&b"
	if tail.String() != tailWant {
		t.Errorf("T = %v != %s", tail, tailWant)
	}
	if x.String() != xWant {
		t.Errorf("X = %v != %s", x, xWant)
	}
}

func TestRun_Void(t *testing.T) {
	m := wam.NewMachine()
	// length3((_ . (_ . (_ . [])))).
	m.AddClause(clause(functor{"length3", 1},
		get_pair{list_pair, reg(0)},
		unify_void{},
		unify_variable{reg(1)},
		get_pair{list_pair, reg(1)},
		unify_void{},
		unify_variable{reg(2)},
		get_pair{list_pair, reg(2)},
		unify_void{},
		unify_constant{watom("[]")},
		proceed{}))

	// ?- length3((a . (X . (f(_, _, X) . []))))
	m.AddClause(clause(functor{},
		// f(_, _, X)
		put_struct{functor{"f", 3}, reg(3)},
		unify_void{},
		unify_void{},
		unify_variable{reg(4)},

		// (f(...) . [])
		put_pair{list_pair, reg(2)},
		unify_value{reg(3)},
		unify_constant{watom("[]")},

		// (X . (...))
		put_pair{list_pair, reg(1)},
		unify_value{reg(4)},
		unify_value{reg(2)},

		// (a . (...))
		put_pair{list_pair, reg(0)},
		unify_constant{watom("a")},
		unify_value{reg(1)},

		call{functor{"length3", 1}},
		halt{}))
	m.IterLimit = 50
	m.DebugFilename = "debugtest/run-void.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	x := m.Reg[4]
	xWant := "_X3"
	if x.String() != xWant {
		t.Errorf("X = %v != %s", x, xWant)
	}
}

var (
	// concat([H|T], L, [H|R]) :- concat(T, L, R).
	concat2 = clause(functor{"concat", 3},
		trust_me{},

		get_pair{list_pair, reg(0)},
		unify_variable{reg(3)},
		unify_variable{reg(4)},

		// L is already in position for next call.

		get_pair{list_pair, reg(2)},
		unify_value{reg(3)},
		unify_variable{reg(5)},

		put_value{reg(4), reg(0)},
		put_value{reg(5), reg(2)},
		execute{functor{"concat", 3}})
	// concat([], L, L).
	concat1 = clause(functor{"concat", 3},
		try_me_else{instr{concat2, 0}},
		get_constant{watom("[]"), reg(0)},
		get_value{reg(1), reg(2)},
		proceed{})
	// [a, b, c]
	buildList_abc = []wam.Instruction{
		put_pair{list_pair, reg(5)},
		unify_constant{watom("c")},
		unify_constant{watom("[]")},
		put_pair{list_pair, reg(4)},
		unify_constant{watom("b")},
		unify_value{reg(5)},
		put_pair{list_pair, reg(0)},
		unify_constant{watom("a")},
		unify_value{reg(4)},
	}
	// [d]
	buildList_d = []wam.Instruction{
		put_pair{list_pair, reg(1)},
		unify_constant{watom("d")},
		unify_constant{watom("[]")},
	}
)

func TestConcat(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(concat1)

	// ?- concat([a, b, c], [d], L).
	var instrs []wam.Instruction
	instrs = append(instrs, buildList_abc...)
	instrs = append(instrs, buildList_d...)
	instrs = append(instrs,
		put_variable{reg(6), reg(2)},
		call{functor{"concat", 3}},
		halt{})
	m.AddClause(clause(functor{}, instrs...))
	m.IterLimit = 75
	m.DebugFilename = "debugtest/run-concat.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	x := m.Reg[6]
	xWant := "&[a, b, c, d]"
	if x.String() != xWant {
		t.Errorf("X = %v != %s", x, xWant)
	}
}

func TestConcat_TryTrust(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(concat1)
	m.AddClause(clause(functor{"concat_tryelse", 0},
		try{instr{concat1, 1}},
		trust{instr{concat2, 1}}))

	// ?- concat([a, b, c], [d], L).
	var instrs []wam.Instruction
	instrs = append(instrs, buildList_abc...)
	instrs = append(instrs, buildList_d...)
	instrs = append(instrs,
		put_variable{reg(6), reg(2)},
		call{functor{"concat_tryelse", 0}},
		halt{})
	m.AddClause(clause(functor{}, instrs...))

	m.IterLimit = 75
	m.DebugFilename = "debugtest/run-concat-tryelse.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	x := m.Reg[6]
	xWant := "&[a, b, c, d]"
	if x.String() != xWant {
		t.Errorf("X = %v != %s", x, xWant)
	}
}

var (
	// call(...) subsequence 1
	//   - call(or(X, Y)) #1
	//   - call(trace)
	//   - call(or(X, Y)) #2
	//   - call(trace)
	//   - call(nl)
	call_s1 = clause(functor{"call", 1},
		try_me_else{instr{callBuiltin, 0}},
		switch_on_term{
			instr{callOr1, 0},
			instr{call_s1_constant, 0},
			instr{call_s1_struct, 0},
			instr{call_s1_list, 0},
			instr{},
			instr{},
		})
	call_s1_constant = clause(functor{"call", 1},
		switch_on_constant{map[constant]instr{
			watom("trace"):   instr{callTrace, 1},
			watom("notrace"): instr{callNotrace, 1},
			watom("nl"):      instr{callNl, 1},
		}})
	call_s1_list = clause(functor{"call", 1},
		execute{functor{"fail", 0}})
	call_s1_struct = clause(functor{"call", 1},
		switch_on_struct{map[functor]instr{
			functor{"or", 2}: instr{call_s1_struct_or2, 0},
		}})
	call_s1_struct_or2 = clause(functor{"call", 1},
		try{instr{callOr1, 1}},
		trust{instr{callOr2, 1}})
	// call(or(X, Y)) :- call(X).
	callOr1 = clause(functor{"call", 1},
		try_me_else{instr{callTrace, 0}},
		get_struct{functor{"or", 2}, reg(0)},
		unify_variable{reg(1)},
		unify_void{},
		put_value{reg(1), reg(0)},
		execute{functor{"call", 1}})
	// call(trace) :- trace().
	callTrace = clause(functor{"call", 1},
		retry_me_else{instr{callOr2, 0}},
		get_constant{watom("trace"), reg(0)},
		execute{functor{"trace", 0}})
	// call(or(X, Y)) :- call(Y).
	callOr2 = clause(functor{"call", 1},
		retry_me_else{instr{callNotrace, 0}},
		get_struct{functor{"or", 2}, reg(0)},
		unify_void{},
		unify_variable{reg(1)},
		put_value{reg(1), reg(0)},
		execute{functor{"call", 1}})
	// call(notrace) :- notrace().
	callNotrace = clause(functor{"call", 1},
		retry_me_else{instr{callNl, 0}},
		get_constant{watom("notrace"), reg(0)},
		execute{functor{"notrace", 0}})
	// call(nl) :- nl().
	callNl = clause(functor{"call", 1},
		trust_me{},
		get_constant{watom("nl"), reg(0)},
		execute{functor{"nl", 0}})
	// call(X) :- builtin(X).
	callBuiltin = clause(functor{"call", 1},
		retry_me_else{instr{callExtern, 0}},
		execute{functor{"builtin", 0}})
	// call(X) :- extern(X).
	callExtern = clause(functor{"call", 1},
		retry_me_else{instr{call_s2, 0}},
		execute{functor{"extern", 0}})
	// call(...) subsequence 2
	//   - call(call(X))
	//   - call(repeat) #1
	//   - call(repeat) #2
	//   - call(true)
	call_s2 = clause(functor{"call", 1},
		trust_me{},
		switch_on_term{
			instr{callCall, 0},
			instr{call_s2_constant, 0},
			instr{call_s2_struct, 0},
			instr{call_s2_list, 0},
			instr{},
			instr{},
		})
	call_s2_constant = clause(functor{"call", 1},
		switch_on_constant{map[constant]instr{
			watom("repeat"): instr{call_s2_constant_repeat, 0},
			watom("true"):   instr{callTrue, 1},
		}})
	call_s2_constant_repeat = clause(functor{"call", 1},
		try{instr{callRepeat1, 1}},
		trust{instr{callRepeat2, 1}})
	call_s2_list = clause(functor{"call", 1},
		execute{functor{"fail", 0}})
	call_s2_struct = clause(functor{"call", 1},
		switch_on_struct{map[functor]instr{
			functor{"call", 1}: instr{callCall, 1},
		}})
	// call(call(X)) :- call(X).
	callCall = clause(functor{"call", 1},
		retry_me_else{instr{callRepeat1, 0}},
		get_struct{functor{"call", 1}, reg(0)},
		unify_variable{reg(1)},
		put_value{reg(1), reg(0)},
		execute{functor{"call", 1}})
	// call(repeat).
	callRepeat1 = clause(functor{"call", 1},
		retry_me_else{instr{callRepeat2, 0}},
		get_constant{watom("repeat"), reg(0)},
		proceed{})
	// call(repeat) :- call(repeat).
	callRepeat2 = clause(functor{"call", 1},
		retry_me_else{instr{callTrue, 0}},
		get_constant{watom("repeat"), reg(0)},
		execute{functor{"call", 1}})
	// call(true).
	callTrue = clause(functor{"call", 1},
		trust_me{},
		get_constant{watom("true"), reg(0)},
		proceed{})
)

func TestSwitch(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(call_s1)

	// ?- call(true), call(or(call(a), repeat))
	m.AddClause(clause(functor{},
		put_constant{watom("true"), reg(0)},
		call{functor{"call", 1}},
		put_struct{functor{"call", 1}, reg(1)},
		unify_constant{watom("a")},
		put_struct{functor{"or", 2}, reg(0)},
		unify_value{reg(1)},
		unify_constant{watom("repeat")},
		call{functor{"call", 1}},
		halt{}))

	m.IterLimit = 75
	m.DebugFilename = "debugtest/run-switch.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
}

var (
	// member(X, [X|_]) :- !.
	// member(X, [_|T]) :- member(X, T).
	member1 = clause(functor{"member", 2},
		try_me_else{instr{member2, 0}},
		get_variable{reg(2), reg(0)},
		get_pair{list_pair, reg(1)},
		unify_value{reg(2)},
		unify_void{},
		neck_cut{},
		proceed{})
	member2 = clause(functor{"member", 2},
		trust_me{},
		get_variable{reg(2), reg(0)},
		get_pair{list_pair, reg(1)},
		unify_void{},
		unify_variable{reg(3)},
		put_value{reg(2), reg(0)},
		put_value{reg(3), reg(1)},
		execute{functor{"member", 2}})

	// set_add(Set, X, Set) :- member(X, Set), !.
	// set_add(Set, X, [X|Set]).
	setAdd1 = clause(functor{"set_add", 3},
		try_me_else{instr{setAdd2, 0}},
		allocate{0},
		get_variable{reg(3), reg(0)},
		get_variable{reg(4), reg(1)},
		get_value{reg(3), reg(2)},
		put_value{reg(4), reg(0)},
		put_value{reg(3), reg(1)},
		call{functor{"member", 2}},
		cut{},
		deallocate{},
		proceed{})
	setAdd2 = clause(functor{"set_add", 3},
		trust_me{},
		get_variable{reg(3), reg(0)},
		get_variable{reg(4), reg(1)},
		get_pair{list_pair, reg(2)},
		unify_value{reg(4)},
		unify_value{reg(3)},
		proceed{})
)

func TestCut(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(member1)
	m.AddClause(setAdd1)

	// ?- member(a, [c, a, b]), set_add([a, b], c, L1), set_add(L1, b, L2).
	m.AddClause(clause(functor{},
		// [a, b]
		put_pair{list_pair, reg(4)},
		unify_constant{watom("b")},
		unify_constant{watom("[]")},
		put_pair{list_pair, reg(3)},
		unify_constant{watom("a")},
		unify_value{reg(4)},
		// member(a, [c, a, b])
		put_constant{watom("a"), reg(0)},
		put_pair{list_pair, reg(1)},
		unify_constant{watom("c")},
		unify_value{reg(3)},
		call{functor{"member", 2}},
		// set_add([a, b], c, L1)
		put_value{reg(3), reg(0)},
		put_constant{watom("c"), reg(1)},
		put_variable{reg(5), reg(2)},
		call{functor{"set_add", 3}},
		// set_add(L1, b, L2)
		put_value{reg(5), reg(0)},
		put_constant{watom("b"), reg(1)},
		put_variable{reg(6), reg(2)},
		call{functor{"set_add", 3}},
		halt{}))

	m.IterLimit = 150
	m.DebugFilename = "debugtest/cut.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	l1, l2 := m.Reg[5], m.Reg[6]
	l1Want, l2Want := "&[c, a, b]", "&[c, a, b]"
	if l1.String() != l1Want {
		t.Errorf("L1 = %v != %s", l1, l1Want)
	}
	if l2.String() != l2Want {
		t.Errorf("L2 = %v != %s", l2, l2Want)
	}
}

// tree(nil, L, L).
// tree(node(Name, Left, Right), L1, L3) :-
//   tree(Left, L1, [Name|L2]),
//   tree(Right, L2, L3).
var (
	tree1 = clause(functor{"tree", 3},
		try_me_else{instr{tree2, 0}},
		get_constant{watom("nil"), reg(0)},
		get_value{reg(2), reg(1)},
		proceed{})

	tree2 = clause(functor{"tree", 3},
		trust_me{},
		allocate{3},
		get_struct{functor{"node", 3}, reg(0)},
		unify_variable{reg(3)},         // Name
		unify_variable{reg(4)},         // Left
		unify_variable{stack(0)},       // Right
		get_variable{reg(5), reg(1)},   // L1
		get_variable{stack(1), reg(2)}, // L3
		put_value{reg(4), reg(0)},
		put_value{reg(1), reg(1)},
		put_pair{list_pair, reg(2)},
		unify_value{reg(3)},
		unify_variable{stack(2)}, // L2
		call{functor{"tree", 3}},
		put_value{stack(0), reg(0)},
		put_value{stack(2), reg(1)},
		put_value{stack(1), reg(2)},
		deallocate{},
		execute{functor{"tree", 3}})
)

func TestNestedCalls(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(tree1)

	// ?- tree(node(a, node(b, nil, node(c, nil, nil)), node(d, nil, nil)), L, []).
	// L = [b, c, a, d]
	m.AddClause(clause(functor{},
		put_struct{functor{"node", 3}, reg(5)},
		unify_constant{watom("d")},
		unify_constant{watom("nil")},
		unify_constant{watom("nil")},
		put_struct{functor{"node", 3}, reg(4)},
		unify_constant{watom("c")},
		unify_constant{watom("nil")},
		unify_constant{watom("nil")},
		put_struct{functor{"node", 3}, reg(3)},
		unify_constant{watom("b")},
		unify_constant{watom("nil")},
		unify_value{reg(4)},
		put_struct{functor{"node", 3}, reg(0)},
		unify_constant{watom("a")},
		unify_value{reg(3)},
		unify_value{reg(5)},
		put_variable{reg(6), reg(1)},
		put_constant{watom("[]"), reg(2)},
		call{functor{"tree", 3}},
		halt{}))

	m.IterLimit = 150
	m.DebugFilename = "debugtest/nested-calls.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}

	l := m.Reg[6]
	lWant := `&[b, c, a, d]`
	if l.String() != lWant {
		t.Errorf("L = %v != %s", l, lWant)
	}
}

func TestCallMeta(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 30
	m.DebugFilename = "debugtest/call-meta.jsonl"

	// p(a).
	// q(b).
	// ?- call(p(), X)
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(comp("p", atom("a"))),
		dsl.Clause(comp("q", atom("b"))),
	})
	if err != nil {
		t.Fatalf("CompileClauses: %v", err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	bindings, err := m.RunQuery(comp("call", comp("p"), var_("X")))
	if err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
	if v := bindings[var_("X")]; v != atom("a") {
		t.Errorf("X = %v (want %v)", v, atom("a"))
	}
}

func TestMetaMetaCall(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 30
	m.DebugFilename = "debugtest/meta-meta-call.jsonl"

	// p(a).
	// ?- call(call(p, X))
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(comp("p", atom("a"))),
	})
	if err != nil {
		t.Fatalf("CompileClauses: %v", err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	bindings, err := m.RunQuery(comp("call", comp("call", atom("p"), var_("X"))))
	if err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
	if v := bindings[var_("X")]; v != atom("a") {
		t.Errorf("X = %v (want %v)", v, atom("a"))
	}
}

func TestIf(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 200
	m.DebugFilename = "debugtest/if.jsonl"

	// true.
	// member(X, [H|T]) :- if(=(X, H), true, member(X, T)).
	// ?- member(b, [a, c, b]), member(z, [a, c, b]).
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(atom("true")),
		dsl.Clause(comp("member", var_("X"), ilist(var_("H"), var_("T"))),
			comp("if",
				comp("=", var_("X"), var_("H")),
				atom("true"),
				comp("member", var_("X"), var_("T")))),
	})
	if err != nil {
		t.Fatalf("CompileClauses: %v", err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	_, err = m.RunQuery(
		comp("member", atom("b"), list(atom("a"), atom("c"), atom("b"))),
		comp("member", atom("z"), list(atom("a"), atom("c"), atom("b"))))
	if err == nil {
		t.Errorf("expected err, got nil")
	}
}

func TestNextSolution(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 200
	m.DebugFilename = "debugtest/next-solution.jsonl"

	// add(0, S, S).
	// add(s(A), B, s(S)) :- add(A, B, S).
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(comp("add", int_(0), var_("S"), var_("S"))),
		dsl.Clause(comp("add", comp("s", var_("A")), var_("B"), comp("s", var_("S"))),
			comp("add", var_("A"), var_("B"), var_("S"))),
	})
	if err != nil {
		t.Fatalf("CompileClauses: %v", err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}

	// ?- add(X, Y, s(s(s(0)))).
	var solutions [4]map[logic.Var]logic.Term
	solutions[0], err = m.RunQuery(
		comp("add", var_("X"), var_("Y"), comp("s", comp("s", comp("s", int_(0))))))
	if err != nil {
		t.Fatalf("RunQuery: got err: %v", err)
	}
	for i := 1; i < 4; i++ {
		solutions[i], err = m.NextSolution()
		if err != nil {
			t.Fatalf("NextSolution #%d: got err: %v", i, err)
		}
	}
	_, err = m.NextSolution()
	if err == nil {
		t.Errorf("want err, got nil")
	}
	wantSolutions := [4]map[logic.Var]logic.Term{
		map[logic.Var]logic.Term{
			var_("X"): int_(0),
			var_("Y"): comp("s", comp("s", comp("s", int_(0))))},
		map[logic.Var]logic.Term{
			var_("X"): comp("s", int_(0)),
			var_("Y"): comp("s", comp("s", int_(0)))},
		map[logic.Var]logic.Term{
			var_("X"): comp("s", comp("s", int_(0))),
			var_("Y"): comp("s", int_(0))},
		map[logic.Var]logic.Term{
			var_("X"): comp("s", comp("s", comp("s", int_(0)))),
			var_("Y"): int_(0)},
	}
	if diff := cmp.Diff(wantSolutions, solutions, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("-want, +got:%s", diff)
	}
}

func TestReset(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 200
	m.DebugFilename = "debugtest/reset.jsonl"

	// parent(charles, william).
	// parent(diana, william).
	// parent(charles, harry).
	// parent(diana, harry).
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(comp("parent", atom("charles"), atom("william"))),
		dsl.Clause(comp("parent", atom("diana"), atom("william"))),
		dsl.Clause(comp("parent", atom("charles"), atom("harry"))),
		dsl.Clause(comp("parent", atom("diana"), atom("harry"))),
	})
	if err != nil {
		t.Fatalf("CompileClauses: %v", err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}

	// ?- parent(charles, X).
	bindings1, err1 := m.RunQuery(comp("parent", atom("charles"), var_("X")))
	m = m.Reset()
	bindings2, err2 := m.RunQuery(comp("parent", var_("X"), atom("harry")))
	bindings3, err3 := m.NextSolution()
	bindings := []map[logic.Var]logic.Term{bindings1, bindings2, bindings3}

	if err1 != nil || err2 != nil || err3 != nil {
		t.Fatalf("got err, err1=%v err2=%v err3=%v", err1, err2, err3)
	}
	want1 := map[logic.Var]logic.Term{var_("X"): atom("william")}
	want2 := map[logic.Var]logic.Term{var_("X"): atom("charles")}
	want3 := map[logic.Var]logic.Term{var_("X"): atom("diana")}
	want := []map[logic.Var]logic.Term{want1, want2, want3}
	if diff := cmp.Diff(want, bindings, test_helpers.IgnoreUnexported); diff != "" {
		t.Errorf("-want, +got:%s", diff)
	}
}

func TestUnifyDicts(t *testing.T) {
	m := wam.NewMachine()
	m.IterLimit = 200

	tests := []struct {
		d1, d2 logic.Term
		want   map[logic.Var]logic.Term
	}{
		// ?- {a:1} = {a:X}.
		{
			dict(atom("a"), int_(1)),
			dict(atom("a"), var_("X")),
			map[logic.Var]logic.Term{var_("X"): int_(1)},
		},
		// ?- {a:1, b:Y} = {a:X, b:2}.
		{
			dict(atom("a"), int_(1), atom("b"), var_("Y")),
			dict(atom("b"), int_(2), atom("a"), var_("X")),
			map[logic.Var]logic.Term{var_("X"): int_(1), var_("Y"): int_(2)},
		},
		// ?- {a:1|Dict} = {b:2, a:X}.
		{
			idict(atom("a"), int_(1), var_("Dict")),
			dict(atom("b"), int_(2), atom("a"), var_("X")),
			map[logic.Var]logic.Term{
				var_("X"):    int_(1),
				var_("Dict"): dict(atom("b"), int_(2)),
			},
		},
		// ?- {a:1, b:2} = {a:X|Dict}.
		{
			dict(atom("a"), int_(1), atom("b"), int_(2)),
			idict(atom("a"), var_("X"), var_("Dict")),
			map[logic.Var]logic.Term{
				var_("X"):    int_(1),
				var_("Dict"): dict(atom("b"), int_(2)),
			},
		},
		// ?- {a:1, c:3|Dict1} = {a:X, b:2|Dict2}
		{
			idict(atom("a"), int_(1), atom("c"), int_(3), var_("Dict1")),
			idict(atom("b"), int_(2), atom("a"), var_("X"), var_("Dict2")),
			map[logic.Var]logic.Term{
				var_("X"):     int_(1),
				var_("Dict1"): idict(atom("b"), int_(2), svar("_X", 4)),
				var_("Dict2"): idict(atom("c"), int_(3), svar("_X", 4)),
			},
		},
		// ?- {} = {a:1}
		{
			atom("{}"),
			dict(atom("a"), int_(1)),
			map[logic.Var]logic.Term{},
		},
		// ?- {a:1} = {}
		{
			dict(atom("a"), int_(1)),
			atom("{}"),
			map[logic.Var]logic.Term{},
		},
		// ?- p(X, {a:1, b:2|X}) = p({b:20, c:30}, {a:A, b:B, c:C})
		{
			comp("p",
				var_("X"),
				idict(atom("a"), int_(1), atom("b"), int_(2), var_("X"))),
			comp("p",
				dict(atom("b"), int_(20), atom("c"), int_(30)),
				dict(atom("a"), var_("A"), atom("b"), var_("B"), atom("c"), var_("C"))),
			map[logic.Var]logic.Term{
				var_("X"): dict(atom("b"), int_(20), atom("c"), int_(30)),
				var_("A"): int_(1),
				var_("B"): int_(2),
				var_("C"): int_(30),
			},
		},
		// ?- p({a:1, b:2|X}, X) = p({a:A, b:B, c:C}, {b:20, c:30})
		{
			comp("p",
				idict(atom("a"), int_(1), atom("b"), int_(2), var_("X")),
				var_("X")),
			comp("p",
				dict(atom("a"), var_("A"), atom("b"), var_("B"), atom("c"), var_("C")),
				dict(atom("b"), int_(20), atom("c"), int_(30))),
			map[logic.Var]logic.Term{
				var_("X"): dict(atom("c"), var_("C")),
				var_("A"): int_(1),
				var_("B"): int_(2),
				var_("C"): int_(30),
			},
		},
		// ?- {a:1, b:2|P1} = {a:1, c:3|P2}, P1={d:4}, P2={c:30, d:4}.
		{
			comp("p",
				idict(atom("a"), int_(1), atom("b"), int_(2), var_("P1")),
				var_("P1"),
				var_("P2")),
			comp("p",
				idict(atom("a"), int_(1), atom("c"), int_(3), var_("P2")),
				dict(atom("d"), int_(4)),
				dict(atom("c"), int_(30), atom("d"), int_(4))),
			map[logic.Var]logic.Term{
				var_("P1"): dict(atom("c"), int_(3), atom("d"), int_(4)),
				var_("P2"): dict(atom("b"), int_(2), atom("d"), int_(4)),
			},
		},
	}
	for i, test := range tests {
		m := m.Reset()
		m.DebugFilename = fmt.Sprintf("debugtest/unify-dicts-%02d.jsonl", i)
		bindings, err := m.RunQuery(comp("=", test.d1, test.d2))
		if err != nil {
			t.Fatalf("%v = %v: got err: %v", test.d1, test.d2, err)
		}
		if diff := cmp.Diff(test.want, bindings, test_helpers.IgnoreUnexported); diff != "" {
			t.Errorf("%v = %v: (-want, +got)\n%s", test.d1, test.d2, diff)
		}
	}
}

func TestAttribute(t *testing.T) {
	m := wam.NewMachine()

	// check_attribute(domain(Min1, Max1), Value, domain(Min, Max)) :-
	//   get_attr(Value, range(Min2, Max2)),
	//   if(@<(Min1, Min2), Min = Min2, Min = Min1),
	//   if(@>(Max1, Max2), Max = Max2, Max = Max1),
	//   put_attr(Value, range(Min, Max)).
	clauses, err := wam.CompileClauses([]*logic.Clause{
		dsl.Clause(
			comp("check_attribute",
				comp("range", var_("Min1"), var_("Max1")),
				var_("Value"),
				comp("range", var_("Min"), var_("Max"))),
			comp("get_attr", var_("Value"), comp("range", var_("Min2"), var_("Max2"))),
			comp("if", comp("@<", var_("Min1"), var_("Min2")),
				comp("=", var_("Min"), var_("Min2")),
				comp("=", var_("Min"), var_("Min1"))),
			comp("if", comp("@>", var_("Max1"), var_("Max2")),
				comp("=", var_("Max"), var_("Max2")),
				comp("=", var_("Max"), var_("Max1"))),
			comp("put_attr", var_("Value"), comp("range", var_("Min"), var_("Max")))),
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	m.IterLimit = 150
	m.DebugFilename = "debugtest/attribute.jsonl"

	// ?-
	//   put_attr(X, range(1, 5)),
	//   put_attr(Y, range(3, 9)),
	//   X = Y,
	//   get_attr(Y, range(Min, Max)).
	solution, err := m.RunQuery(
		comp("put_attr", var_("X"), comp("range", int_(1), int_(5))),
		comp("put_attr", var_("Y"), comp("range", int_(3), int_(9))),
		comp("=", var_("X"), var_("Y")),
		comp("get_attr", var_("Y"), comp("range", var_("Min"), var_("Max"))))
	if err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
	min, max := solution[var_("Min")], solution[var_("Max")]
	minWant, maxWant := int_(3), int_(5)
	if min != minWant {
		t.Errorf("Min = %v != %s", min, minWant)
	}
	if max != maxWant {
		t.Errorf("Max = %v != %s", max, maxWant)
	}
}
