package wam

import (
	"fmt"
	"sort"
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
	builtins = append(builtins, failClause)
	for _, pred := range unicodePredicates {
		builtins = append(builtins, builtinUnicodePredicate(pred))
	}
	for _, pred := range comparisonPredicates {
		builtins = append(builtins, builtinComparisonPredicate(pred))
	}
}

var (
	comp = dsl.Comp
	var_ = dsl.Var
	atom = dsl.Atom
	int_ = dsl.Int
	ptr  = dsl.Ptr
)

var (
	failClause = &Clause{Functor{"fail", 0}, 0, []Instruction{fail{}}}
	preamble   = []*logic.Clause{
		// =(X, X).
		dsl.Clause(comp("=", var_("X"), var_("X"))),

		// true.
		// false :- fail.
		dsl.Clause(atom("true")),
		dsl.Clause(atom("false"), atom("fail")),

		// ->(Cond, Then,    _) :- Cond, !, Then.
		// ->(   _,    _, Else) :- Else.
		dsl.Clause(comp("->", var_("Cond"), var_("Then"), var_("_")),
			var_("Cond"), atom("!"), var_("Then")),
		dsl.Clause(comp("->", var_("_"), var_("_"), var_("Else")),
			var_("Else")),

		// \+(Goal) :- if(Goal, false, true).
		// \=(X, Y) :- \+(=(X, Y)).
		dsl.Clause(comp("\\+", var_("Goal")),
			comp("->", var_("Goal"), atom("false"), atom("true"))),
		dsl.Clause(comp("\\=", var_("X"), var_("Y")),
			comp("\\+", comp("=", var_("X"), var_("Y")))),

		// Attributes
		dsl.Clause(comp("get_attr", var_("X"), var_("Attr")),
			comp("asm", comp("get_attr", var_("X0"), var_("X1")))),
		dsl.Clause(comp("put_attr", var_("X"), var_("Attr")),
			comp("asm", comp("put_attr", var_("X0"), var_("X1")))),
		dsl.Clause(comp("$join_attribute:unify", var_("AttrName"), var_("X"), var_("Y")),
			comp("asm", comp("call", atom("join_attribute/3")))),
		dsl.Clause(comp("$check_attribute:unify", var_("Attr"), var_("Value")),
			comp("asm", comp("call", atom("check_attribute/2")))),

		// Compiled calls to call/n are inlined into the instruction call_meta.
		// These functions are used when referenced by (meta-)meta-calls, and
		// are limited to 8 args.
		dsl.Clause(comp("call", var_("Functor")),
			comp("asm", comp("call", atom("call/1")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1")),
			comp("asm", comp("call", atom("call/2")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2")),
			comp("asm", comp("call", atom("call/3")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2"), var_("A3")),
			comp("asm", comp("call", atom("call/4")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2"), var_("A3"), var_("A4")),
			comp("asm", comp("call", atom("call/5")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2"), var_("A3"), var_("A4"), var_("A5")),
			comp("asm", comp("call", atom("call/6")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2"), var_("A3"), var_("A4"), var_("A5"), var_("A6")),
			comp("asm", comp("call", atom("call/7")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2"), var_("A3"), var_("A4"), var_("A5"), var_("A6"), var_("A7")),
			comp("asm", comp("call", atom("call/8")))),
		dsl.Clause(comp("call", var_("Functor"), var_("A1"), var_("A2"), var_("A3"), var_("A4"), var_("A5"), var_("A6"), var_("A7"), var_("A8")),
			comp("asm", comp("call", atom("call/9")))),

		// Unifiable
		dsl.Clause(comp("unifiable", var_("X"), var_("Y"), var_("Unifier")),
			comp("asm", comp("builtin", atom("unifiable"), ptr(builtinUnifiable)))),
	}
)

// ---- unicode predicates

type unicodePredicate struct {
	functor Functor
	table   *unicode.RangeTable
	msg     string
}

var (
	unicodePredicates = []unicodePredicate{
		unicodePredicate{Functor{"unicode_digit", 1}, unicode.Digit, "not a digit"},
		unicodePredicate{Functor{"unicode_letter", 1}, unicode.Letter, "not a letter"},
		unicodePredicate{Functor{"unicode_lower", 1}, unicode.Lower, "not a lowercase letter"},
		unicodePredicate{Functor{"unicode_upper", 1}, unicode.Upper, "not an uppercase letter"},
		unicodePredicate{Functor{"unicode_symbol", 1}, unicode.Symbol, "not a symbol"},
		unicodePredicate{Functor{"unicode_punct", 1}, unicode.Punct, "not a punctuation"},
		unicodePredicate{Functor{"unicode_space", 1}, unicode.White_Space, "not whitespace"},
	}
)

func builtinUnicodePredicate(pred unicodePredicate) *Clause {
	clause := &Clause{Functor: pred.functor, NumRegisters: 1}
	start := makeUnicodePredicate(pred, clause)
	clause.Code = []Instruction{
		builtin{Name: pred.functor.Name, Func: start},
		proceed{},
	}
	return clause
}

// Succeeds if first arg is a rune from table; if first arg is a ref, replaces
// itself with iterator.
func makeUnicodePredicate(pred unicodePredicate, clause *Clause) func(*Machine) error {
	return func(m *Machine) error {
		cell := deref(m.Reg[0])
		switch c := cell.(type) {
		case WAtom:
			r, ok := runes.Single(string(c))
			if !ok {
				return fmt.Errorf("%v: not a single rune: %v", pred.functor, c)
			}
			if !unicode.Is(pred.table, r) {
				return fmt.Errorf("%v: %s: %c", pred.functor, pred.msg, r)
			}
			return nil
		case *Ref:
			iterator := makeUnicodeIterator(pred, clause)
			clause.Code = []Instruction{
				builtin{Name: pred.functor.Name + "_ref", Func: iterator},
				proceed{},
			}
			return iterator(m)
		default:
			return fmt.Errorf("%v: not an atom: %v", pred.functor, cell)
		}
	}
}

// The unicode iterator emits all runes from table via backtracking.
func makeUnicodeIterator(pred unicodePredicate, clause *Clause) func(*Machine) error {
	allRunes := runes.All(pred.table)
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

// ---- comparisons

type comparisonPredicate struct {
	functor            Functor
	accepts1, accepts2 ordering
}

var comparisonPredicates = []comparisonPredicate{
	comparisonPredicate{Functor{"@<", 2}, less, less},
	comparisonPredicate{Functor{"@=<", 2}, less, equal},
	comparisonPredicate{Functor{"@>", 2}, more, more},
	comparisonPredicate{Functor{"@>=", 2}, more, equal},
	comparisonPredicate{Functor{"==", 2}, equal, equal},
	comparisonPredicate{Functor{"\\==", 2}, less, more},
}

func builtinComparisonPredicate(pred comparisonPredicate) *Clause {
	return &Clause{
		Functor:      pred.functor,
		NumRegisters: 2,
		Code: []Instruction{
			builtin{
				Name: pred.functor.Name,
				Func: makeComparisonPredicate(pred),
			},
			proceed{},
		},
	}
}

func makeComparisonPredicate(pred comparisonPredicate) func(m *Machine) error {
	return func(m *Machine) error {
		x1, x2 := deref(m.Reg[0]), deref(m.Reg[1])
		if o := compareCells(x1, x2); o == pred.accepts1 || o == pred.accepts2 {
			return nil
		}
		return fmt.Errorf("%v: %v %s %v is false", pred.functor, x1, pred.functor.Name, x2)
	}
}

// ---- dif

func builtinUnifiable(m *Machine) error {
	x, y, unifier := m.Reg[0], m.Reg[1], m.Reg[2]
	bindings, _, err := m.unifyBindings(x, y)
	if err != nil {
		// Unification failed, return empty list of bindings.
		m.bind(unifier, WAtom("[]"))
		return nil
	}
	// Undo bindings.
	for x := range bindings {
		x.Cell = nil
	}
	assocs := make([]*Pair, len(bindings))
	i := 0
	for x, value := range bindings {
		assocs[i] = assocPair(x, value)
		i++
	}
	// Sort in descending order, because list will be built backwards.
	sort.Slice(assocs, func(i, j int) bool {
		return compareCells(assocs[i], assocs[j]) != less
	})
	var list Cell = WAtom("[]")
	for _, assoc := range assocs {
		list = listPair(assoc, list)
	}
	m.bind(unifier, list)
	return nil
}
