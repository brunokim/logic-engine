package wam

import (
	"errors"
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
	for _, pred := range typeCheckPredicates {
		builtins = append(builtins, builtinTypeCheckPredicate(pred))
	}
}

var (
	comp  = dsl.Comp
	var_  = dsl.Var
	atom  = dsl.Atom
	int_  = dsl.Int
	ptr   = dsl.Ptr
	ilist = dsl.IList
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

		// \+(Goal) :- ->(Goal, false, true).
		// \=(X, Y) :- \+(=(X, Y)).
		dsl.Clause(comp("\\+", var_("Goal")),
			comp("->", var_("Goal"), atom("false"), atom("true"))),
		dsl.Clause(comp("\\=", var_("X"), var_("Y")),
			comp("\\+", comp("=", var_("X"), var_("Y")))),

		// Logical conective 'and'. In Prolog it's represented with the operator ','.
		//
		// and([]).
		// and([Goal|Rest]) :- Goal, and(Rest).
		dsl.Clause(comp("and", atom("[]"))),
		dsl.Clause(comp("and", ilist(var_("Goal"), var_("Rest"))),
			var_("Goal"),
			comp("and", var_("Rest"))),

		// Attributes
		dsl.Clause(comp("get_attr", var_("X"), var_("Attr")),
			comp("asm", comp("get_attr", var_("X0"), var_("X1")))),
		dsl.Clause(comp("put_attr", var_("X"), var_("Attr")),
			comp("asm", comp("put_attr", var_("X0"), var_("X1")))),
		dsl.Clause(comp("$join_attribute", var_("AttrName"), var_("X"), var_("Y")),
			comp("asm", comp("call", atom("join_attribute/3"))),
			comp("asm", comp("proceed", atom("unify")))),
		dsl.Clause(comp("$check_attribute", var_("Attr"), var_("Value")),
			comp("asm", comp("call", atom("check_attribute/2"))),
			comp("asm", comp("proceed", atom("unify")))),

		// Compiled calls to call/n are inlined into the instruction call_meta.
		// These functions are used when referenced by (meta-)meta-calls, and
		// are limited to 8 args.
		makeCall(0),
		makeCall(1),
		makeCall(2),
		makeCall(3),
		makeCall(4),
		makeCall(5),
		makeCall(6),
		makeCall(7),
		makeCall(8),

		// Unifiable
		dsl.Clause(comp("unifiable", var_("X"), var_("Y"), var_("Unifier")),
			comp("asm", comp("builtin", atom("unifiable"), ptr(builtinUnifiable), var_("X0"), var_("X1"), var_("X2")))),
	}
)

// ---- call predicates

// call(Functor, A1, A2,..., An) :-
//   asm(call("call/n+1")),
//   asm(proceed(run)).
func makeCall(numArgs int) *logic.Clause {
	vars := make([]logic.Term, numArgs+1)
	vars[0] = var_("Functor")
	for i := 1; i <= numArgs; i++ {
		vars[i] = var_(fmt.Sprintf("A%d", i))
	}
	functor := fmt.Sprintf("call/%d", numArgs+1)
	clause := dsl.Clause(comp("call", vars...),
		comp("asm", comp("call", atom(functor))),
		comp("asm", comp("proceed", atom("run"))))
	return clause
}

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
		builtin{Name: pred.functor.Name, Func: start, Args: []Addr{RegAddr(0)}},
		proceed{},
	}
	return clause
}

// Succeeds if first arg is a rune from table.
func makeUnicodePredicate(pred unicodePredicate, clause *Clause) func(*Machine, []Addr) error {
	return func(m *Machine, args []Addr) error {
		cell := deref(m.get(args[0]))
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
		default:
			return fmt.Errorf("%v: not an atom: %v", pred.functor, cell)
		}
	}
}

// ---- comparisons

type comparisonPredicate struct {
	functor            Functor
	accepts1, accepts2 ordering
}

var comparisonPredicates = map[string]comparisonPredicate{
	"@<":   comparisonPredicate{Functor{"@<", 2}, less, less},
	"@=<":  comparisonPredicate{Functor{"@=<", 2}, less, equal},
	"@>":   comparisonPredicate{Functor{"@>", 2}, more, more},
	"@>=":  comparisonPredicate{Functor{"@>=", 2}, more, equal},
	"==":   comparisonPredicate{Functor{"==", 2}, equal, equal},
	"\\==": comparisonPredicate{Functor{"\\==", 2}, less, more},
}

func builtinComparisonInstruction(pred comparisonPredicate, x, y Addr) builtin {
	return builtin{
		Name: pred.functor.Name,
		Args: []Addr{x, y},
		Func: makeComparisonPredicate(pred),
	}
}

func builtinComparisonPredicate(pred comparisonPredicate) *Clause {
	return &Clause{
		Functor:      pred.functor,
		NumRegisters: 2,
		Code: []Instruction{
			builtinComparisonInstruction(pred, RegAddr(0), RegAddr(1)),
			proceed{},
		},
	}
}

func makeComparisonPredicate(pred comparisonPredicate) func(*Machine, []Addr) error {
	return func(m *Machine, args []Addr) error {
		x1, x2 := deref(m.get(args[0])), deref(m.get(args[1]))
		if o := compareCells(x1, x2); o == pred.accepts1 || o == pred.accepts2 {
			return nil
		}
		return fmt.Errorf("%v: %v %s %v is false", pred.functor, x1, pred.functor.Name, x2)
	}
}

// ---- dif

func builtinUnifiable(m *Machine, args []Addr) error {
	x, y, unifier := m.get(args[0]), m.get(args[1]), m.get(args[2])
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

// ---- type checks

type typeCheckPredicate struct {
	name string
	err  error
}

var typeCheckPredicates = map[string]typeCheckPredicate{
	"atom":  {"atom", errors.New("not an atom")},
	"int":   {"int", errors.New("not an int")},
	"ptr":   {"ptr", errors.New("not a ptr")},
	"var":   {"var", errors.New("not a var")},
	"list":  {"list", errors.New("not a list")},
	"assoc": {"assoc", errors.New("not an assoc")},
	"dict":  {"dict", errors.New("not a dict")},
}

func builtinTypeCheckPredicate(pred typeCheckPredicate) *Clause {
	return &Clause{
		Functor:      Functor{pred.name, 1},
		NumRegisters: 1,
		Code: []Instruction{
			builtinTypeCheckInstruction(pred, RegAddr(0)),
			proceed{},
		},
	}
}

func builtinTypeCheckInstruction(pred typeCheckPredicate, x Addr) builtin {
	return builtin{
		Name: pred.name,
		Args: []Addr{x},
		Func: makeTypeCheckPredicate(pred),
	}
}

func makeTypeCheckPredicate(pred typeCheckPredicate) func(m *Machine, args []Addr) error {
	return func(m *Machine, args []Addr) error {
		if typeCheck(m, args[0]) == pred.name {
			return nil
		}
		return pred.err
	}
}

func typeCheck(m *Machine, addr Addr) string {
	cell := deref(m.get(addr))
	switch c := cell.(type) {
	case WAtom:
		return "atom"
	case WInt:
		return "int"
	case WPtr:
		return "ptr"
	case *Ref:
		return "var"
	case *Pair:
		switch c.Tag {
		case ListPair:
			return "list"
		case AssocPair:
			return "assoc"
		case DictPair:
			return "dict"
		}
	}
	panic(fmt.Sprintf("Unhandled cell type %T (%v)", cell, cell))
}
