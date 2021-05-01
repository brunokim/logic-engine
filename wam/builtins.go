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
	cell := deref(m.Reg[0])
	switch c := cell.(type) {
	case WAtom:
		r, ok := runes.Single(string(c))
		if !ok {
			return fmt.Errorf("unicode_digit/1: not a single rune: %v", c)
		}
		if !unicode.IsDigit(r) {
			return fmt.Errorf("unicode_digit/1: not a digit: %c", r)
		}
	case *Ref:
		return fmt.Errorf("unicode_digit/1: not sufficiently instantiated: %v", c)
	default:
		return fmt.Errorf("unicode_digit/1: not an atom: %v", cell)
	}
	return nil
}
