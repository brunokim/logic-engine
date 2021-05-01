package wam

import (
	"fmt"
	"unicode"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/runes"
)

var builtins []*Clause

func init() {
	var err error
	builtins, err = CompileClauses(preamble)
	if err != nil {
		panic(err)
	}
	builtins = append(builtins, initCalls()...)
	builtins = append(builtins, fail)
	for _, checker := range unicodeCheckers {
		builtins = append(builtins, builtinUnicodeChecker(checker))
	}
}

var (
	comp = dsl.Comp
	var_ = dsl.Var
	atom = dsl.Atom
)

var (
	fail     = &Clause{Functor{"fail", 0}, 0, []Instruction{Fail{}}}
	preamble = []*logic.Clause{
		// =(X, X).
		dsl.Clause(comp("=", var_("X"), var_("X"))),
		// true.
		// false :- fail.
		dsl.Clause(atom("true")),
		dsl.Clause(atom("false"), atom("fail")),
		// if(Cond, Then,    _) :- Cond, !, Then.
		// if(   _,    _, Else) :- Else.
		dsl.Clause(comp("if", var_("Cond"), var_("Then"), var_("_")),
			var_("Cond"), atom("!"), var_("Then")),
		dsl.Clause(comp("if", var_("_"), var_("_"), var_("Else")),
			var_("Else")),
		// \+(Goal) :- if(Goal, false, true).
		// \=(X, Y) :- \+(=(X, Y)).
		dsl.Clause(comp("\\+", var_("Goal")),
			comp("if", var_("Goal"), atom("false"), atom("true"))),
		dsl.Clause(comp("\\=", var_("X"), var_("Y")),
			comp("\\+", comp("=", var_("X"), var_("Y")))),
	}
)

// Builtin call(...) clauses.
// Compiled calls to call/n are inlined into the instruction call_meta.
// These functions are used when referenced by (meta-)meta-calls, and
// are limited to arity 8.
func initCalls() []*Clause {
	calls := make([]*Clause, 8)
	for i := 0; i < 8; i++ {
		calls[i] = &Clause{
			Functor:      Functor{"call", i + 1},
			NumRegisters: i + 1,
			// Allocate an env frame to store the continuation. The inner
			// call(...) will be inlined by the compiler.
			Code: optimizeInstructions([]Instruction{
				Allocate{0},
				Call{Functor{"call", i + 1}},
				Deallocate{},
				Proceed{},
			}),
		}
	}
	return calls
}

type unicodeChecker struct {
	functor Functor
	table   *unicode.RangeTable
	msg     string
}

var (
	unicodeCheckers = []unicodeChecker{
		unicodeChecker{Functor{"unicode_digit", 1}, unicode.Digit, "not a digit"},
		unicodeChecker{Functor{"unicode_letter", 1}, unicode.Letter, "not a letter"},
		unicodeChecker{Functor{"unicode_lower", 1}, unicode.Lower, "not a lowercase letter"},
		unicodeChecker{Functor{"unicode_upper", 1}, unicode.Upper, "not an uppercase letter"},
		unicodeChecker{Functor{"unicode_symbol", 1}, unicode.Symbol, "not a symbol"},
		unicodeChecker{Functor{"unicode_punct", 1}, unicode.Punct, "not a punctuation"},
		unicodeChecker{Functor{"unicode_space", 1}, unicode.White_Space, "not whitespace"},
	}
)

func builtinUnicodeChecker(checker unicodeChecker) *Clause {
	clause := &Clause{Functor: checker.functor, NumRegisters: 1}
	start := makeUnicodePredicate(checker, clause)
	clause.Code = []Instruction{
		Builtin{Name: checker.functor.Name, Func: start},
		Proceed{},
	}
	return clause
}

// Succeeds if first arg is a rune from table; if first arg is a ref, replaces
// itself with iterator.
func makeUnicodePredicate(checker unicodeChecker, clause *Clause) func(*Machine) error {
	return func(m *Machine) error {
		cell := deref(m.Reg[0])
		switch c := cell.(type) {
		case WAtom:
			r, ok := runes.Single(string(c))
			if !ok {
				return fmt.Errorf("%v: not a single rune: %v", checker.functor, c)
			}
			if !unicode.Is(checker.table, r) {
				return fmt.Errorf("%v: %s: %c", checker.functor, checker.msg, r)
			}
			return nil
		case *Ref:
			iterator := makeUnicodeIterator(checker, clause)
			clause.Code = []Instruction{
				Builtin{Name: checker.functor.Name + "_ref", Func: iterator},
				Proceed{},
			}
			return iterator(m)
		default:
			return fmt.Errorf("%v: not an atom: %v", checker.functor, cell)
		}
	}
}

// The unicode iterator emits all runes from table via backtracking.
func makeUnicodeIterator(checker unicodeChecker, clause *Clause) func(*Machine) error {
	allRunes := runes.All(checker.table)
	var pos int
	return func(m *Machine) error {
		if pos == 0 {
			// try_me_else
			m.ChoicePoint = m.newChoicePoint(InstrAddr{clause, 0})
		} else if pos < len(allRunes)-1 {
			// retry_me_else
			m.restoreFromChoicePoint()
		} else {
			// trust_me
			m.restoreFromChoicePoint()
			m.ChoicePoint = m.ChoicePoint.Prev
		}
		x := deref(m.Reg[0])
		m.bind(x, WAtom(string(allRunes[pos])))
		pos++
		return nil
	}
}
