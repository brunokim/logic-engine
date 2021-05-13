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
	builtins = append(builtins,
		failClause,
		builtinUnicodeIterPredicate())
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
	list  = dsl.List
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
		// They are not self-referential, since the body occurrence of
		// call(A, B, ...) is inlined to a call_meta instruction.
		dsl.Clause(
			comp("call", var_("Fn")),
			comp("call", var_("Fn"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A")),
			comp("call", var_("Fn"), var_("A"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B")),
			comp("call", var_("Fn"), var_("A"), var_("B"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C")),
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D")),
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E")),
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"), var_("F")),
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"), var_("F"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"), var_("F"), var_("G")),
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"), var_("F"), var_("G"))),
		dsl.Clause(
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"), var_("F"), var_("G"), var_("H")),
			comp("call", var_("Fn"), var_("A"), var_("B"), var_("C"), var_("D"), var_("E"), var_("F"), var_("G"), var_("H"))),

		// Unifiable
		dsl.Clause(comp("unifiable", var_("X"), var_("Y"), var_("Unifier")),
			comp("asm", comp("builtin", atom("unifiable"), ptr(builtinUnifiable), var_("X0"), var_("X1"), var_("X2")))),

		// Unicode
		dsl.Clause(comp("unicode_check", var_("Ch"), var_("Category")),
			comp("asm", comp("builtin", atom("unicode_check"), ptr(builtinUnicodeCheck), var_("X0"), var_("X1")))),
		dsl.Clause(comp("unicode_category", var_("Ch"), var_("Category")),
			comp("->", comp("var", var_("Ch")),
				comp("unicode_iter", var_("Ch"), int_(0), var_("Category")),
				comp("unicode_check", var_("Ch"), var_("Category")))),
		dsl.Clause(comp("unicode_digit", var_("Ch")), comp("unicode_category", var_("Ch"), atom("digit"))),
		dsl.Clause(comp("unicode_letter", var_("Ch")), comp("unicode_category", var_("Ch"), atom("letter"))),
		dsl.Clause(comp("unicode_lower", var_("Ch")), comp("unicode_category", var_("Ch"), atom("lower"))),
		dsl.Clause(comp("unicode_upper", var_("Ch")), comp("unicode_category", var_("Ch"), atom("upper"))),
		dsl.Clause(comp("unicode_symbol", var_("Ch")), comp("unicode_category", var_("Ch"), atom("symbol"))),
		dsl.Clause(comp("unicode_punct", var_("Ch")), comp("unicode_category", var_("Ch"), atom("punct"))),
		dsl.Clause(comp("unicode_space", var_("Ch")), comp("unicode_category", var_("Ch"), atom("space"))),
	}
)

// ---- unicode predicates

var unicodeTable = map[string]*unicode.RangeTable{
	"digit":  unicode.Digit,
	"letter": unicode.Letter,
	"lower":  unicode.Lower,
	"upper":  unicode.Upper,
	"symbol": unicode.Symbol,
	"punct":  unicode.Punct,
	"space":  unicode.White_Space,
}

func builtinUnicodeCheck(m *Machine, args []Addr) error {
	ch := deref(m.get(args[0])).(WAtom)
	category := deref(m.get(args[1])).(WAtom)

	r, ok := runes.Single(string(ch))
	if !ok {
		return errors.New("Ch is not a single rune")
	}
	table, ok := unicodeTable[string(category)]
	if !ok {
		return fmt.Errorf("Unknown Unicode table: %q", category)
	}
	if !unicode.Is(table, r) {
		return fmt.Errorf("%c is not %s", r, category)
	}
	return nil
}

var unicodeCharsCache = map[*unicode.RangeTable][]rune{}

func builtinUnicodeIter(m *Machine, args []Addr) error {
	m.restoreFromChoicePoint()
	ref := deref(m.Reg[0]).(*Ref)
	pos := deref(m.Reg[1]).(WInt)
	category := deref(m.Reg[2]).(WAtom)
	table, ok := unicodeTable[string(category)]
	if !ok {
		return fmt.Errorf("Unknown Unicode table: %q", category)
	}
	chars, ok := unicodeCharsCache[table]
	if !ok {
		chars = runes.All(table)
		unicodeCharsCache[table] = chars
	}
	i := int(pos)
	if i >= len(chars) {
		m.ChoicePoint = m.ChoicePoint.Prev
		return fmt.Errorf("no more chars")
	}
	ch := WAtom(string(chars[i]))
	m.bindRef(ref, ch) // TODO: handle attributed var.
	i++
	m.ChoicePoint.Args[1] = WInt(i) // Increment position for next backtrack.
	return nil
}

func builtinUnicodeIterPredicate() *Clause {
	c := &Clause{Functor: Functor{"unicode_iter", 3}, NumRegisters: 3}
	c.Code = []Instruction{
		tryMeElse{InstrAddr{c, 1}},
		builtin{"unicode_iter", nil, builtinUnicodeIter},
		proceed{},
	}
	return c
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
