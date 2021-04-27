package parser_test

import (
	"testing"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/parser"
	"github.com/brunokim/logic-engine/test_helpers"

	"github.com/google/go-cmp/cmp"
)

var (
	atom   = dsl.Atom
	int_   = dsl.Int
	var_   = dsl.Var
	comp   = dsl.Comp
	list   = dsl.List
	ilist  = dsl.IList
	assoc  = dsl.Assoc
	dict   = dsl.Dict
	idict  = dsl.IDict
	clause = dsl.Clause
)

func TestParse(t *testing.T) {
	tests := []struct {
		text string
		want logic.Term
	}{
		{`a`, atom("a")},
		{`  a`, atom("a")},
		{` a  `, atom("a")},
		{`word`, atom("word")},
		{`word_`, atom("word_")},
		{`word123`, atom("word123")},
		{`'word123'`, atom("word123")},
		{`'word 123'`, atom("word 123")},
		{`'word\n123'`, atom("word\n123")},
		{`'word\'123'`, atom("word'123")},
		{`123`, int_(123)},
		{`X`, var_("X")},
		{`X123`, var_("X123")},
		{`X_1`, var_("X_1")},
		{`_a_1__`, var_("_a_1__")},
		{`f()`, comp("f")},
		{`f( )`, comp("f")},
		{`f(1 )`, comp("f", int_(1))},
		{`f( 1)`, comp("f", int_(1))},
		{`f( 1 )`, comp("f", int_(1))},
		{`f( 1, )`, comp("f", int_(1))},
		{`f( 1,)`, comp("f", int_(1))},
		{`f(1,)`, comp("f", int_(1))},
		{`edge(1, 2)`, comp("edge", int_(1), int_(2))},
		{`edge(1,2)`, comp("edge", int_(1), int_(2))},
		{`edge(1,2,)`, comp("edge", int_(1), int_(2))},
		{`edge(1,2, )`, comp("edge", int_(1), int_(2))},
		{`f(g(1))`, comp("f", comp("g", int_(1)))},
		{`[]`, list()},
		{`[ ]`, list()},
		{`[1]`, list(int_(1))},
		{`[1 ]`, list(int_(1))},
		{`[1,]`, list(int_(1))},
		{`[1, ]`, list(int_(1))},
		{`[1 , ]`, list(int_(1))},
		{`[  1]`, list(int_(1))},
		{`[  1 ]`, list(int_(1))},
		{`[  1,]`, list(int_(1))},
		{`[  1, ]`, list(int_(1))},
		{`[1,2]`, list(int_(1), int_(2))},
		{`[1, 2]`, list(int_(1), int_(2))},
		{`[1, 2,]`, list(int_(1), int_(2))},
		{`[1|X]`, ilist(int_(1), var_("X"))},
		{`[1, 2|X]`, ilist(int_(1), int_(2), var_("X"))},
		{`[1, 2,|X]`, ilist(int_(1), int_(2), var_("X"))},
		{`[1, 2|a]`, ilist(int_(1), int_(2), atom("a"))},
		{`[1, 2 |a]`, ilist(int_(1), int_(2), atom("a"))},
		{`[1, 2| a]`, ilist(int_(1), int_(2), atom("a"))},
		{`[1, 2 | a]`, ilist(int_(1), int_(2), atom("a"))},
		{`""`, list()},
		{`"a"`, list(atom("a"))},
		{`"abc"`, list(atom("a"), atom("b"), atom("c"))},
		{`"ab\ncd"`, list(atom("a"), atom("b"), atom("\n"), atom("c"), atom("d"))},
		{`"ab\"cd"`, list(atom("a"), atom("b"), atom("\""), atom("c"), atom("d"))},
		{`"ab\\cd"`, list(atom("a"), atom("b"), atom("\\"), atom("c"), atom("d"))},
		{`:a 1`, assoc(atom("a"), int_(1))},
		{`: a 1`, assoc(atom("a"), int_(1))},
		{`:f(a) []`, assoc(comp("f", atom("a")), list())},
		{`{}`, dict()},
		{`{ }`, dict()},
		{`{:a 1}`, dict(atom("a"), int_(1))},
		{`{:a 1,}`, dict(atom("a"), int_(1))},
		{`{:a 1 ,}`, dict(atom("a"), int_(1))},
		{`{:a 1, }`, dict(atom("a"), int_(1))},
		{`{:a 1 , }`, dict(atom("a"), int_(1))},
		{`{:a 1,:b 2}`, dict(atom("a"), int_(1), atom("b"), int_(2))},
		{`{:b 2, :a 1}`, dict(atom("a"), int_(1), atom("b"), int_(2))},
		{`{:a 1|D}`, idict(atom("a"), int_(1), var_("D"))},
		{`{:a 1| D}`, idict(atom("a"), int_(1), var_("D"))},
		{`{:a 1 |D}`, idict(atom("a"), int_(1), var_("D"))},
		{`{:a 1 | D}`, idict(atom("a"), int_(1), var_("D"))},
		{`{:a 1, | D}`, idict(atom("a"), int_(1), var_("D"))},
	}
	for _, test := range tests {
		got, err := parser.ParseTerm(test.text)
		if err != nil {
			t.Fatalf("%q: got err: %v", test.text, err)
		}
		if diff := cmp.Diff(test.want, got, test_helpers.IgnoreUnexported); diff != "" {
			t.Errorf("%q: (-want, +got)\n%s", test.text, diff)
		}
	}
}
