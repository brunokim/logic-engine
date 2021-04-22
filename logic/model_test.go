package logic_test

import (
	"fmt"
	"testing"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/test_helpers"
)

func TestLess(t *testing.T) {
	order := []logic.Term{
		var_("A"),
		svar("A", 1),
		svar("A", 9),
		var_("Z"),
		int_(1),
		int_(9),
		atom("[]"),
		atom("a"),
		atom("a1"),
		atom("a9"),
		atom("z"),
		atom("{}"),
		comp("f"),
		comp("g"),
		comp("f", atom("a")),
		comp("f", atom("z")),
		comp("g", atom("a")),
		ilist(atom("a"), var_("Tail")),
		list(atom("a")),
		ilist(atom("a"), atom("z"), var_("Tail")),
		list(atom("a"), atom("z")),
		assoc(atom("a"), atom("b")),
		assoc(atom("a"), atom("z")),
		assoc(atom("z"), atom("b")),
		idict(atom("a"), var_("X"), atom("b"), var_("Y"), var_("Parent")),
		dict(atom("a"), var_("X"), atom("b"), var_("Y")),
		dict(atom("a"), var_("Y"), atom("b"), var_("Z")),
	}
	for i := 0; i < len(order)-1; i++ {
		if !logic.Less(order[i], order[i+1]) {
			t.Errorf("%v >= %v", order[i], order[i+1])
		}
	}
}

func TestEq(t *testing.T) {
	tests := []struct {
		x, y logic.Term
	}{
		{svar("A", 1), var_("A").WithSuffix(1)},
	}
	for _, test := range tests {
		if !logic.Eq(test.x, test.y) {
			t.Errorf("%v != %v", test.x, test.y)
		}
	}
}

func TestString(t *testing.T) {
	tests := []struct {
		term fmt.Stringer
		want string
	}{
		{atom("a"), `"a"`},
		{var_("A"), "A"},
		{svar("A", 1), "A_1_"},
		{comp("f"), "f()"},
		{comp("f", var_("A")), "f(A)"},
		{comp("f", var_("A"), var_("B")), "f(A, B)"},
		{list(), `"[]"`},
		{list(var_("A")), "[A]"},
		{list(var_("A"), var_("B")), "[A, B]"},
		{ilist(var_("A"), var_("B"), var_("Tail")), "[A, B|Tail]"},
		{assoc(var_("Key"), var_("Val")), "Key:Val"},
		{dict(var_("Key"), var_("Val")), "{Key:Val}"},
		{dict(atom("a"), var_("A")), `{"a":A}`},
		{dict(atom("a"), var_("A"), atom("b"), var_("B")), `{"a":A, "b":B}`},
		{idict(atom("a"), var_("A"), atom("b"), var_("B"), var_("Parent")), `{"a":A, "b":B|Parent}`},
		{clause(comp("add", atom("0"), var_("X"), var_("X"))), `add("0", X, X).`},
		{
			clause(comp("add", comp("s", var_("A")), var_("B"), comp("s", var_("Sum"))),
				comp("add", var_("A"), var_("B"), var_("Sum"))),
			`
            add(s(A), B, s(Sum)) :-
              add(A, B, Sum).`,
		},
		{
			clause(comp("mul", comp("s", var_("A")), var_("B"), var_("Product")),
				comp("mul", var_("A"), var_("B"), var_("Subproduct")),
				comp("add", var_("Subproduct"), var_("B"), var_("Product"))),
			`
            mul(s(A), B, Product) :-
              mul(A, B, Subproduct),
              add(Subproduct, B, Product).`,
		},
	}
	for _, test := range tests {
		want := test_helpers.Dedent(test.want)
		got := test.term.String()
		if got != want {
			t.Errorf("%#v.String() = %q (!= %q)", test.term, got, want)
		}
	}
}

func TestNewAssocSet(t *testing.T) {
	tests := []struct {
		assocs []*logic.Assoc
		want   string
	}{
		{[]*logic.Assoc{}, ""},
		{[]*logic.Assoc{assoc(atom("a"), int_(1))}, ""},
		{[]*logic.Assoc{assoc(atom("a"), int_(1)), assoc(atom("b"), int_(1))}, ""},
		{[]*logic.Assoc{assoc(atom("a"), int_(1)), assoc(atom("a"), int_(2))}, `duplicate keys in AssocSet: "a"`},
		{[]*logic.Assoc{
			assoc(atom("a"), int_(1)),
			assoc(atom("a"), int_(2)),
			assoc(atom("a"), int_(3))},
			`duplicate keys in AssocSet: "a"`},
		{[]*logic.Assoc{
			assoc(atom("a"), int_(1)),
			assoc(atom("b"), int_(2)),
			assoc(atom("b"), int_(3))},
			`duplicate keys in AssocSet: "b"`},
		{[]*logic.Assoc{
			assoc(atom("a"), int_(0)),
			assoc(atom("a"), int_(1)),
			assoc(atom("b"), int_(2)),
			assoc(atom("b"), int_(3))},
			`duplicate keys in AssocSet: "a"`},
	}
	for _, test := range tests {
		_, err := logic.NewAssocSet(test.assocs)
		if err == nil && test.want != "" {
			t.Errorf("%v: want err %q, got nil", test.assocs, test.want)
		}
		if err != nil && err.Error() != test.want {
			t.Errorf("%v: want err %q, got %v", test.assocs, test.want, err)
		}
	}
}
