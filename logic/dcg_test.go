package logic_test

import (
	"testing"

	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/test_helpers"

	"github.com/google/go-cmp/cmp"
)

func TestDCGToClause(t *testing.T) {
	tests := []struct {
		dcg  *logic.DCG
		want *logic.Clause
	}{
		{
			// ws --> [].
			dcg(atom("ws"), dcg_list()),
			// ws(_L0, _L1) :- _L0 = _L1.
			clause(comp("ws", var_("_L0"), var_("_L1")),
				comp("=", var_("_L0"), var_("_L1"))),
		},
		{
			// ws --> ' ', ws.
			dcg(atom("ws"), dcg_list(atom(" ")), atom("ws")),
			// ws(_L0, _L2) :- _L0 = [' '|_L1], ws(_L1, _L2).
			clause(comp("ws", var_("_L0"), var_("_L2")),
				comp("=", var_("_L0"), ilist(atom(" "), var_("_L1"))),
				comp("ws", var_("_L1"), var_("_L2"))),
		},
		{
			// quoted(Q, Ts) --> [Q], terms(Ts), [Q].
			dcg(comp("quoted", var_("Q"), var_("Ts")),
				dcg_list(var_("Q")),
				comp("terms", var_("Ts")),
				dcg_list(var_("Q"))),
			// quoted(Q, Ts, _L0, _L3) :- _L0 = [Q|_L1],  terms(Ts, _L1, _L2), _L2 = [Q|_L3].
			clause(comp("quoted", var_("Q"), var_("Ts"), var_("_L0"), var_("_L3")),
				comp("=", var_("_L0"), ilist(var_("Q"), var_("_L1"))),
				comp("terms", var_("Ts"), var_("_L1"), var_("_L2")),
				comp("=", var_("_L2"), ilist(var_("Q"), var_("_L3")))),
		},
		{
			// f(L) --> g(L), [a|L].
			dcg(comp("f", var_("L")), comp("g", var_("L")), dcg_ilist(atom("a"), var_("L"))),
			// f(L, _L0, _L2) :- g(L, _L0, _L1), '$append'([a|L], _L2, _L1).
			clause(comp("f", var_("L"), var_("_L0"), var_("_L2")),
				comp("g", var_("L"), var_("_L0"), var_("_L1")),
				comp("$append", ilist(atom("a"), var_("L")), var_("_L2"), var_("_L1"))),
		},
		{
			// lowers([Ch|Chars]) --> [Ch], {unicode_lower(Ch), !}, lowers(Chars).
			dcg(comp("lowers", ilist(var_("Ch"), var_("Chars"))),
				dcg_list(var_("Ch")),
				dcg_goals(comp("unicode_lower", var_("Ch")), atom("!")),
				comp("lowers", var_("Chars"))),
			// lowers([Ch|Chars], _L0, _L2) :- _L0 = [Ch|_L1], unicode_lower(Ch), !, lowers(Chars, _L1, _L2).
			clause(comp("lowers", ilist(var_("Ch"), var_("Chars")), var_("_L0"), var_("_L2")),
				comp("=", var_("_L0"), ilist(var_("Ch"), var_("_L1"))),
				comp("unicode_lower", var_("Ch")),
				atom("!"),
				comp("lowers", var_("Chars"), var_("_L1"), var_("_L2"))),
		},
		{
			// bad_list(Ls) --> Ls.
			dcg(comp("bad_list", var_("Ls")), var_("Ls")),
			// bad_list(Ls, _L0, _L1) :- phrase(Ls, _L0, _L1).
			clause(comp("bad_list", var_("Ls"), var_("_L0"), var_("_L1")),
				comp("phrase", var_("Ls"), var_("_L0"), var_("_L1"))),
		},
	}
	for _, test := range tests {
		got := test.dcg.ToClause()
		if diff := cmp.Diff(test.want, got, test_helpers.IgnoreUnexported); diff != "" {
			t.Errorf("%v: (-want, +got)\n%s", test.dcg, diff)
		}
	}
}
