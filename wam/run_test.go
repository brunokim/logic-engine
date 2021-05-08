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
	queryInstrs = []*logic.Comp{
		comp("put_struct", atom("h/2"), var_("X2")),
		comp("unify_variable", var_("X1")),
		comp("unify_variable", var_("X4")),
		comp("put_struct", atom("f/1"), var_("X3")),
		comp("unify_value", var_("X4")),
		comp("put_struct", atom("p/3"), var_("X0")),
		comp("unify_value", var_("X1")),
		comp("unify_value", var_("X2")),
		comp("unify_value", var_("X3")),
	}

	// p(f(X), h(Y, f(a)), Y).
	programInstrs = []*logic.Comp{
		comp("get_struct", atom("p/3"), var_("X0")),
		comp("unify_variable", var_("X1")),
		comp("unify_variable", var_("X2")),
		comp("unify_variable", var_("X3")),
		comp("get_struct", atom("f/1"), var_("X1")),
		comp("unify_variable", var_("X5")),
		comp("get_struct", atom("h/2"), var_("X2")),
		comp("unify_value", var_("X3")),
		comp("unify_variable", var_("X5")),
		comp("get_struct", atom("f/1"), var_("X5")),
		comp("unify_variable", var_("X6")),
		comp("get_constant", atom("a"), var_("X6")),
	}
)

func TestRun_BuildQuery(t *testing.T) {
	var instrs []*logic.Comp
	instrs = append(instrs, queryInstrs...)
	instrs = append(instrs, comp("halt"))
	query := wam.DecodeClause(indicator("", 0), instrs...)
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
	instrs := []*logic.Comp{}
	instrs = append(instrs, queryInstrs...)
	instrs = append(instrs, programInstrs...)
	instrs = append(instrs, comp("halt"))
	query := wam.DecodeClause(indicator("", 0), instrs...)
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

func TestRun_call(t *testing.T) {
	var instrs []*logic.Comp
	instrs = append(instrs, queryInstrs...)
	instrs = append(instrs,
		comp("call", atom("p/3")),
		comp("halt"))
	query := wam.DecodeClause(indicator("", 0), instrs...)

	var prog []*logic.Comp
	prog = append(prog, programInstrs...)
	prog = append(prog, comp("proceed"))
	program := wam.DecodeClause(indicator("p", 3), prog...)

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
	p2 = wam.DecodeClause(indicator("p", 2),
		comp("allocate", int_(2)),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_variable", var_("Y0"), var_("X1")),
		comp("put_value", var_("X2"), var_("X0")),
		comp("put_variable", var_("Y1"), var_("X1")),
		comp("call", atom("q/2")),
		comp("put_value", var_("Y1"), var_("X0")),
		comp("put_value", var_("Y0"), var_("X1")),
		comp("call", atom("r/2")),
		comp("deallocate"),
		comp("proceed"))

	// q(a, f(a)).
	q2 = wam.DecodeClause(indicator("q", 2),
		comp("get_constant", atom("a"), var_("X0")),
		comp("get_struct", atom("f/1"), var_("X1")),
		comp("unify_constant", atom("a")),
		comp("proceed"))

	// r(f(A), f(B)) :- s(B), t(A).
	r2 = wam.DecodeClause(indicator("r", 2),
		comp("allocate", int_(1)),
		comp("get_struct", atom("f/1"), var_("X0")),
		comp("unify_variable", var_("Y0")),
		comp("get_struct", atom("f/1"), var_("X1")),
		comp("unify_variable", var_("X2")),
		comp("put_value", var_("X2"), var_("X0")),
		comp("call", atom("s/1")),
		comp("put_value", var_("Y0"), var_("X0")),
		comp("call", atom("t/1")),
		comp("deallocate"),
		comp("proceed"))

	// s(g(b)).
	s1 = wam.DecodeClause(indicator("s", 1),
		comp("get_struct", atom("g/1"), var_("X0")),
		comp("unify_constant", atom("b")),
		comp("proceed"))

	// t(a).
	t1 = wam.DecodeClause(indicator("t", 1),
		comp("get_constant", atom("a"), var_("X0")),
		comp("proceed"))
)

func TestRun_allocate(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(p2)
	m.AddClause(q2)
	m.AddClause(r2)
	m.AddClause(s1)
	m.AddClause(t1)

	// ?- p(X, Y).
	m.AddClause(wam.DecodeClause(indicator("", 0),
		// Save X and Y at regs X3 and X4, that are not used by any other clauses.
		comp("put_variable", var_("X3"), var_("X0")),
		comp("put_variable", var_("X4"), var_("X1")),
		comp("call", atom("p/2")),
		comp("halt")))
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
	colorRed = wam.DecodeClause(indicator("color", 1),
		comp("try_me_else", comp("instr", ptr(colorGreen), int_(0))),
		comp("get_constant", atom("red"), var_("X0")),
		comp("proceed"))
	colorGreen = wam.DecodeClause(indicator("color", 1),
		comp("retry_me_else", comp("instr", ptr(colorBlue), int_(0))),
		comp("get_constant", atom("green"), var_("X0")),
		comp("proceed"))
	colorBlue = wam.DecodeClause(indicator("color", 1),
		comp("trust_me"),
		comp("get_constant", atom("blue"), var_("X0")),
		comp("proceed"))

	// bit(false).
	// bit(true).
	bitFalse = wam.DecodeClause(indicator("bit", 1),
		comp("try_me_else", comp("instr", ptr(bitTrue), int_(0))),
		comp("get_constant", atom("false"), var_("X0")),
		comp("proceed"))
	bitTrue = wam.DecodeClause(indicator("bit", 1),
		comp("trust_me"),
		comp("get_constant", atom("true"), var_("X0")),
		comp("proceed"))

	// bit_color(Bit, Color) :- bit(Bit), color(Color).
	bitColor = wam.DecodeClause(indicator("bit_color", 2),
		comp("allocate", int_(1)),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_variable", var_("Y0"), var_("X1")),
		comp("put_value", var_("X2"), var_("X0")),
		comp("call", atom("bit/1")),
		comp("put_value", var_("Y0"), var_("X0")),
		comp("call", atom("color/1")),
		comp("deallocate"),
		comp("proceed"))
)

func TestRun_ChoicePoints(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(colorRed)
	m.AddClause(bitFalse)
	m.AddClause(bitColor)

	// ?- bit_color(true, green).
	m.AddClause(wam.DecodeClause(indicator("", 0),
		comp("put_constant", atom("true"), var_("X0")),
		comp("put_constant", atom("green"), var_("X1")),
		comp("call", atom("bit_color/2")),
		comp("halt"),
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
	a2 = wam.DecodeClause(indicator("a", 2),
		comp("allocate", int_(2)),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_variable", var_("Y0"), var_("X1")),
		comp("put_variable", var_("Y1"), var_("X0")),
		comp("put_value", var_("X2"), var_("X1")),
		comp("call", atom("b/2")),
		comp("put_value", var_("Y0"), var_("X0")),
		comp("put_struct", atom("f/1"), var_("X1")),
		comp("unify_value", var_("Y1")),
		comp("call", atom("c/2")),
		comp("deallocate"),
		comp("proceed"))
	b2 = wam.DecodeClause(indicator("b", 2),
		comp("allocate", int_(0)),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_constant", atom("p"), var_("X1")),
		comp("put_value", var_("X2"), var_("X0")),
		comp("call", atom("d/1")),
		comp("deallocate"),
		comp("proceed"))
	c2_1 = wam.DecodeClause(indicator("c", 2),
		comp("try_me_else", comp("instr", ptr(c2_2), int_(0))),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_struct", atom("f/1"), var_("X1")),
		comp("unify_value", var_("X2")),
		comp("proceed"))
	c2_2 = wam.DecodeClause(indicator("c", 2),
		comp("trust_me"),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_struct", atom("g/1"), var_("X1")),
		comp("unify_value", var_("X2")),
		comp("proceed"))
	d1_1 = wam.DecodeClause(indicator("d", 1),
		comp("try_me_else", comp("instr", ptr(d1_2), int_(0))),
		comp("get_constant", atom("q0"), var_("X0")),
		comp("proceed"))
	d1_2 = wam.DecodeClause(indicator("d", 1),
		comp("retry_me_else", comp("instr", ptr(d1_3), int_(0))),
		comp("get_constant", atom("q1"), var_("X0")),
		comp("proceed"))
	d1_3 = wam.DecodeClause(indicator("d", 1),
		comp("trust_me"),
		comp("get_constant", atom("q2"), var_("X0")),
		comp("proceed"))
)

func TestRun_Trail(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(a2)
	m.AddClause(b2)
	m.AddClause(c2_1)
	m.AddClause(d1_1)

	// ?- a(X, q1).
	m.AddClause(wam.DecodeClause(indicator("", 0),
		// Save X at reg X3, that is not used by any other clauses.
		comp("put_variable", var_("X3"), var_("X0")),
		comp("put_constant", atom("q1"), var_("X1")),
		comp("call", atom("a/2")),
		comp("halt")))
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
	m.AddClause(wam.DecodeClause(indicator("build_list", 1),
		comp("get_pair", atom("list"), var_("X0")),
		comp("unify_constant", atom("a")),
		comp("unify_variable", var_("X1")),
		comp("get_pair", atom("list"), var_("X1")),
		comp("unify_constant", atom("b")),
		comp("unify_constant", atom("[]")),
		comp("proceed")))

	// =(X, X).
	m.AddClause(wam.DecodeClause(indicator("=", 2),
		comp("get_value", var_("X0"), var_("X1")),
		comp("proceed")))

	// ?- build_list((a . T)), =(T, (X . [])).
	m.AddClause(wam.DecodeClause(indicator("", 0),
		comp("allocate", int_(2)),

		comp("put_pair", atom("list"), var_("X0")),
		comp("unify_constant", atom("a")),
		comp("unify_variable", var_("Y0")),
		comp("call", atom("build_list/1")),

		comp("put_value", var_("Y0"), var_("X0")),
		comp("put_pair", atom("list"), var_("X1")),
		comp("unify_variable", var_("Y1")),
		comp("unify_constant", atom("[]")),
		comp("call", atom("=/2")),

		comp("halt")))
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
	m.AddClause(wam.DecodeClause(indicator("length3", 1),
		comp("get_pair", atom("list"), var_("X0")),
		comp("unify_void"),
		comp("unify_variable", var_("X1")),
		comp("get_pair", atom("list"), var_("X1")),
		comp("unify_void"),
		comp("unify_variable", var_("X2")),
		comp("get_pair", atom("list"), var_("X2")),
		comp("unify_void"),
		comp("unify_constant", atom("[]")),
		comp("proceed")))

	// ?- length3((a . (X . (f(_, _, X) . []))))
	m.AddClause(wam.DecodeClause(indicator("", 0),
		// f(_, _, X)
		comp("put_struct", atom("f/3"), var_("X3")),
		comp("unify_void"),
		comp("unify_void"),
		comp("unify_variable", var_("X4")),

		// (f(...) . [])
		comp("put_pair", atom("list"), var_("X2")),
		comp("unify_value", var_("X3")),
		comp("unify_constant", atom("[]")),

		// (X . (...))
		comp("put_pair", atom("list"), var_("X1")),
		comp("unify_value", var_("X4")),
		comp("unify_value", var_("X2")),

		// (a . (...))
		comp("put_pair", atom("list"), var_("X0")),
		comp("unify_constant", atom("a")),
		comp("unify_value", var_("X1")),

		comp("call", atom("length3/1")),
		comp("halt")))
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
	concat2 = wam.DecodeClause(indicator("concat", 3),
		comp("trust_me"),

		comp("get_pair", atom("list"), var_("X0")),
		comp("unify_variable", var_("X3")),
		comp("unify_variable", var_("X4")),

		// L is already in position for next call.

		comp("get_pair", atom("list"), var_("X2")),
		comp("unify_value", var_("X3")),
		comp("unify_variable", var_("X5")),

		comp("put_value", var_("X4"), var_("X0")),
		comp("put_value", var_("X5"), var_("X2")),
		comp("execute", atom("concat/3")))
	// concat([], L, L).
	concat1 = wam.DecodeClause(indicator("concat", 3),
		comp("try_me_else", comp("instr", ptr(concat2), int_(0))),
		comp("get_constant", atom("[]"), var_("X0")),
		comp("get_value", var_("X1"), var_("X2")),
		comp("proceed"))
	// [a, b, c]
	buildList_abc = []*logic.Comp{
		comp("put_pair", atom("list"), var_("X5")),
		comp("unify_constant", atom("c")),
		comp("unify_constant", atom("[]")),
		comp("put_pair", atom("list"), var_("X4")),
		comp("unify_constant", atom("b")),
		comp("unify_value", var_("X5")),
		comp("put_pair", atom("list"), var_("X0")),
		comp("unify_constant", atom("a")),
		comp("unify_value", var_("X4")),
	}
	// [d]
	buildList_d = []*logic.Comp{
		comp("put_pair", atom("list"), var_("X1")),
		comp("unify_constant", atom("d")),
		comp("unify_constant", atom("[]")),
	}
)

func TestConcat(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(concat1)

	// ?- concat([a, b, c], [d], L).
	var instrs []*logic.Comp
	instrs = append(instrs, buildList_abc...)
	instrs = append(instrs, buildList_d...)
	instrs = append(instrs,
		comp("put_variable", var_("X6"), var_("X2")),
		comp("call", atom("concat/3")),
		comp("halt"))
	m.AddClause(wam.DecodeClause(indicator("", 0), instrs...))
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

func TestConcat_trytrust(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(concat1)
	m.AddClause(wam.DecodeClause(indicator("concat_tryelse", 0),
		comp("try", comp("instr", ptr(concat1), int_(1))),
		comp("trust", comp("instr", ptr(concat2), int_(1)))))

	// ?- concat([a, b, c], [d], L).
	var instrs []*logic.Comp
	instrs = append(instrs, buildList_abc...)
	instrs = append(instrs, buildList_d...)
	instrs = append(instrs,
		comp("put_variable", var_("X6"), var_("X2")),
		comp("call", atom("concat_tryelse/0")),
		comp("halt"))
	m.AddClause(wam.DecodeClause(indicator("", 0), instrs...))

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
	call_s1 = wam.DecodeClause(indicator("call", 1),
		comp("try_me_else", comp("instr", ptr(callbuiltin), int_(0))),
		comp("switch_on_term",
			comp("instr", ptr(callOr1), int_(0)),
			comp("instr", ptr(call_s1_constant), int_(0)),
			comp("instr", ptr(call_s1_struct), int_(0)),
			comp("instr", ptr(call_s1_list), int_(0)),
			comp("instr", ptr(nil), int_(0)),
			comp("instr", ptr(nil), int_(0))))
	call_s1_constant = wam.DecodeClause(indicator("call", 1),
		comp("switch_on_constant", dict(
			atom("trace"), comp("instr", ptr(callTrace), int_(1)),
			atom("notrace"), comp("instr", ptr(callNotrace), int_(1)),
			atom("nl"), comp("instr", ptr(callNl), int_(1)))))
	call_s1_list = wam.DecodeClause(indicator("call", 1),
		comp("execute", atom("fail/0")))
	call_s1_struct = wam.DecodeClause(indicator("call", 1),
		comp("switch_on_struct", dict(
			atom("or/2"), comp("instr", ptr(call_s1_struct_or2), int_(0)))))
	call_s1_struct_or2 = wam.DecodeClause(indicator("call", 1),
		comp("try", comp("instr", ptr(callOr1), int_(1))),
		comp("trust", comp("instr", ptr(callOr2), int_(1))))
	// call(or(X, Y)) :- call(X).
	callOr1 = wam.DecodeClause(indicator("call", 1),
		comp("try_me_else", comp("instr", ptr(callTrace), int_(0))),
		comp("get_struct", atom("or/2"), var_("X0")),
		comp("unify_variable", var_("X1")),
		comp("unify_void"),
		comp("put_value", var_("X1"), var_("X0")),
		comp("execute", atom("call/1")))
	// call(trace) :- trace().
	callTrace = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callOr2), int_(0))),
		comp("get_constant", atom("trace"), var_("X0")),
		comp("execute", atom("trace/0")))
	// call(or(X, Y)) :- call(Y).
	callOr2 = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callNotrace), int_(0))),
		comp("get_struct", atom("or/2"), var_("X0")),
		comp("unify_void"),
		comp("unify_variable", var_("X1")),
		comp("put_value", var_("X1"), var_("X0")),
		comp("execute", atom("call/1")))
	// call(notrace) :- notrace().
	callNotrace = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callNl), int_(0))),
		comp("get_constant", atom("notrace"), var_("X0")),
		comp("execute", atom("notrace/0")))
	// call(nl) :- nl().
	callNl = wam.DecodeClause(indicator("call", 1),
		comp("trust_me"),
		comp("get_constant", atom("nl"), var_("X0")),
		comp("execute", atom("nl/0")))
	// call(X) :- builtin(X).
	callbuiltin = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callExtern), int_(0))),
		comp("execute", atom("builtin/0")))
	// call(X) :- extern(X).
	callExtern = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(call_s2), int_(0))),
		comp("execute", atom("extern/0")))
	// call(...) subsequence 2
	//   - call(call(X))
	//   - call(repeat) #1
	//   - call(repeat) #2
	//   - call(true)
	call_s2 = wam.DecodeClause(indicator("call", 1),
		comp("trust_me"),
		comp("switch_on_term",
			comp("instr", ptr(callcall), int_(0)),
			comp("instr", ptr(call_s2_constant), int_(0)),
			comp("instr", ptr(call_s2_struct), int_(0)),
			comp("instr", ptr(call_s2_list), int_(0)),
			comp("instr", ptr(nil), int_(0)),
			comp("instr", ptr(nil), int_(0))))
	call_s2_constant = wam.DecodeClause(indicator("call", 1),
		comp("switch_on_constant", dict(
			atom("repeat"), comp("instr", ptr(call_s2_constant_repeat), int_(0)),
			atom("true"), comp("instr", ptr(callTrue), int_(1)))))
	call_s2_constant_repeat = wam.DecodeClause(indicator("call", 1),
		comp("try", comp("instr", ptr(callRepeat1), int_(1))),
		comp("trust", comp("instr", ptr(callRepeat2), int_(1))))
	call_s2_list = wam.DecodeClause(indicator("call", 1),
		comp("execute", atom("fail/0")))
	call_s2_struct = wam.DecodeClause(indicator("call", 1),
		comp("switch_on_struct", dict(
			atom("call/1"), comp("instr", ptr(callcall), int_(1)))))
	// call(call(X)) :- call(X).
	callcall = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callRepeat1), int_(0))),
		comp("get_struct", atom("call/1"), var_("X0")),
		comp("unify_variable", var_("X1")),
		comp("put_value", var_("X1"), var_("X0")),
		comp("execute", atom("call/1")))
	// call(repeat).
	callRepeat1 = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callRepeat2), int_(0))),
		comp("get_constant", atom("repeat"), var_("X0")),
		comp("proceed"))
	// call(repeat) :- call(repeat).
	callRepeat2 = wam.DecodeClause(indicator("call", 1),
		comp("retry_me_else", comp("instr", ptr(callTrue), int_(0))),
		comp("get_constant", atom("repeat"), var_("X0")),
		comp("execute", atom("call/1")))
	// call(true).
	callTrue = wam.DecodeClause(indicator("call", 1),
		comp("trust_me"),
		comp("get_constant", atom("true"), var_("X0")),
		comp("proceed"))
)

func TestSwitch(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(call_s1)

	// ?- call(true), call(or(call(a), repeat))
	m.AddClause(wam.DecodeClause(indicator("", 0),
		comp("put_constant", atom("true"), var_("X0")),
		comp("call", atom("call/1")),
		comp("put_struct", atom("call/1"), var_("X1")),
		comp("unify_constant", atom("a")),
		comp("put_struct", atom("or/2"), var_("X0")),
		comp("unify_value", var_("X1")),
		comp("unify_constant", atom("repeat")),
		comp("call", atom("call/1")),
		comp("halt")))

	m.IterLimit = 75
	m.DebugFilename = "debugtest/run-switch.jsonl"

	if err := m.Run(); err != nil {
		t.Fatalf("expected nil, got err: %v", err)
	}
}

var (
	// member(X, [X|_]) :- !.
	// member(X, [_|T]) :- member(X, T).
	member1 = wam.DecodeClause(indicator("member", 2),
		comp("try_me_else", comp("instr", ptr(member2), int_(0))),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_pair", atom("list"), var_("X1")),
		comp("unify_value", var_("X2")),
		comp("unify_void"),
		comp("neck_cut"),
		comp("proceed"))
	member2 = wam.DecodeClause(indicator("member", 2),
		comp("trust_me"),
		comp("get_variable", var_("X2"), var_("X0")),
		comp("get_pair", atom("list"), var_("X1")),
		comp("unify_void"),
		comp("unify_variable", var_("X3")),
		comp("put_value", var_("X2"), var_("X0")),
		comp("put_value", var_("X3"), var_("X1")),
		comp("execute", atom("member/2")))

	// set_add(Set, X, Set) :- member(X, Set), !.
	// set_add(Set, X, [X|Set]).
	setAdd1 = wam.DecodeClause(indicator("set_add", 3),
		comp("try_me_else", comp("instr", ptr(setAdd2), int_(0))),
		comp("allocate", int_(0)),
		comp("get_variable", var_("X3"), var_("X0")),
		comp("get_variable", var_("X4"), var_("X1")),
		comp("get_value", var_("X3"), var_("X2")),
		comp("put_value", var_("X4"), var_("X0")),
		comp("put_value", var_("X3"), var_("X1")),
		comp("call", atom("member/2")),
		comp("cut"),
		comp("deallocate"),
		comp("proceed"))
	setAdd2 = wam.DecodeClause(indicator("set_add", 3),
		comp("trust_me"),
		comp("get_variable", var_("X3"), var_("X0")),
		comp("get_variable", var_("X4"), var_("X1")),
		comp("get_pair", atom("list"), var_("X2")),
		comp("unify_value", var_("X4")),
		comp("unify_value", var_("X3")),
		comp("proceed"))
)

func Testcut(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(member1)
	m.AddClause(setAdd1)

	// ?- member(a, [c, a, b]), set_add([a, b], c, L1), set_add(L1, b, L2).
	m.AddClause(wam.DecodeClause(indicator("", 0),
		// [a, b]
		comp("put_pair", atom("list"), var_("X4")),
		comp("unify_constant", atom("b")),
		comp("unify_constant", atom("[]")),
		comp("put_pair", atom("list"), var_("X3")),
		comp("unify_constant", atom("a")),
		comp("unify_value", var_("X4")),
		// member(a, [c, a, b])
		comp("put_constant", atom("a"), var_("X0")),
		comp("put_pair", atom("list"), var_("X1")),
		comp("unify_constant", atom("c")),
		comp("unify_value", var_("X3")),
		comp("call", atom("member/2")),
		// set_add([a, b], c, L1)
		comp("put_value", var_("X3"), var_("X0")),
		comp("put_constant", atom("c"), var_("X1")),
		comp("put_variable", var_("X5"), var_("X2")),
		comp("call", atom("set_add/3")),
		// set_add(L1, b, L2)
		comp("put_value", var_("X5"), var_("X0")),
		comp("put_constant", atom("b"), var_("X1")),
		comp("put_variable", var_("X6"), var_("X2")),
		comp("call", atom("set_add/3")),
		comp("halt")))

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
	tree1 = wam.DecodeClause(indicator("tree", 3),
		comp("try_me_else", comp("instr", ptr(tree2), int_(0))),
		comp("get_constant", atom("nil"), var_("X0")),
		comp("get_value", var_("X2"), var_("X1")),
		comp("proceed"))

	tree2 = wam.DecodeClause(indicator("tree", 3),
		comp("trust_me"),
		comp("allocate", int_(3)),
		comp("get_struct", atom("node/3"), var_("X0")),
		comp("unify_variable", var_("X3")),           // Name
		comp("unify_variable", var_("X4")),           // Left
		comp("unify_variable", var_("Y0")),           // Right
		comp("get_variable", var_("X5"), var_("X1")), // L1
		comp("get_variable", var_("Y1"), var_("X2")), // L3
		comp("put_value", var_("X4"), var_("X0")),
		comp("put_value", var_("X1"), var_("X1")),
		comp("put_pair", atom("list"), var_("X2")),
		comp("unify_value", var_("X3")),
		comp("unify_variable", var_("Y2")), // L2
		comp("call", atom("tree/3")),
		comp("put_value", var_("Y0"), var_("X0")),
		comp("put_value", var_("Y2"), var_("X1")),
		comp("put_value", var_("Y1"), var_("X2")),
		comp("deallocate"),
		comp("execute", atom("tree/3")))
)

func TestNestedcalls(t *testing.T) {
	m := wam.NewMachine()
	m.AddClause(tree1)

	// ?- tree(node(a, node(b, nil, node(c, nil, nil)), node(d, nil, nil)), L, []).
	// L = [b, c, a, d]
	m.AddClause(wam.DecodeClause(indicator("", 0),
		comp("put_struct", atom("node/3"), var_("X5")),
		comp("unify_constant", atom("d")),
		comp("unify_constant", atom("nil")),
		comp("unify_constant", atom("nil")),
		comp("put_struct", atom("node/3"), var_("X4")),
		comp("unify_constant", atom("c")),
		comp("unify_constant", atom("nil")),
		comp("unify_constant", atom("nil")),
		comp("put_struct", atom("node/3"), var_("X3")),
		comp("unify_constant", atom("b")),
		comp("unify_constant", atom("nil")),
		comp("unify_value", var_("X4")),
		comp("put_struct", atom("node/3"), var_("X0")),
		comp("unify_constant", atom("a")),
		comp("unify_value", var_("X3")),
		comp("unify_value", var_("X5")),
		comp("put_variable", var_("X6"), var_("X1")),
		comp("put_constant", atom("[]"), var_("X2")),
		comp("call", atom("tree/3")),
		comp("halt")))

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

func TestcallMeta(t *testing.T) {
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

func TestMetaMetacall(t *testing.T) {
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
