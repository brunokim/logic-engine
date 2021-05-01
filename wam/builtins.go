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
	builtins = append(builtins, unicodeClauses...)
}

var (
	comp = dsl.Comp
	var_ = dsl.Var
	atom = dsl.Atom
)

var (
	fail           = &Clause{Functor{"fail", 0}, 0, []Instruction{Fail{}}}
	unicodeClauses = []*Clause{
		&Clause{Functor{"unicode_digit", 1}, 1, []Instruction{Builtin{unicodeDigit}, Proceed{}}},
		&Clause{Functor{"unicode_letter", 1}, 1, []Instruction{Builtin{unicodeLetter}, Proceed{}}},
		&Clause{Functor{"unicode_lower", 1}, 1, []Instruction{Builtin{unicodeLower}, Proceed{}}},
		&Clause{Functor{"unicode_upper", 1}, 1, []Instruction{Builtin{unicodeUpper}, Proceed{}}},
		&Clause{Functor{"unicode_symbol", 1}, 1, []Instruction{Builtin{unicodeSymbol}, Proceed{}}},
		&Clause{Functor{"unicode_punct", 1}, 1, []Instruction{Builtin{unicodePunct}, Proceed{}}},
		&Clause{Functor{"unicode_space", 1}, 1, []Instruction{Builtin{unicodeSpace}, Proceed{}}},
	}
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

func unicodeDigit(m *Machine) error {
	return unicodeCheck(m, unicode.IsDigit, "unicode_digit/1", "not a digit")
}

func unicodeLetter(m *Machine) error {
	return unicodeCheck(m, unicode.IsLetter, "unicode_letter/1", "not a letter")
}

func unicodeLower(m *Machine) error {
	return unicodeCheck(m, unicode.IsLower, "unicode_lower/1", "not a lowercase letter")
}

func unicodeUpper(m *Machine) error {
	return unicodeCheck(m, unicode.IsUpper, "unicode_upper/1", "not an uppercase letter")
}

func unicodeSymbol(m *Machine) error {
	return unicodeCheck(m, unicode.IsSymbol, "unicode_symbol/1", "not a symbol")
}

func unicodePunct(m *Machine) error {
	return unicodeCheck(m, unicode.IsPunct, "unicode_punct/1", "not a punctuation")
}

func unicodeSpace(m *Machine) error {
	return unicodeCheck(m, unicode.IsSpace, "unicode_space/1", "not a space")
}

func unicodeCheck(m *Machine, pred func(rune) bool, predName, errorMsg string) error {
	cell := deref(m.Reg[0])
	switch c := cell.(type) {
	case WAtom:
		r, ok := runes.Single(string(c))
		if !ok {
			return fmt.Errorf("%s: not a single rune: %v", predName, c)
		}
		if !pred(r) {
			return fmt.Errorf("%s: %s: %c", predName, errorMsg, r)
		}
	case *Ref:
		return fmt.Errorf("%s: not sufficiently instantiated: %v", predName, c)
	default:
		return fmt.Errorf("%s: not an atom: %v", predName, cell)
	}
	return nil
}
