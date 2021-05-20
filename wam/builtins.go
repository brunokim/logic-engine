package wam

import (
	"fmt"
	"sort"
	"unicode"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/errors"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/runes"
)

var builtinsPkg = NewPackage("") // global namespace

func init() {
	builtins := CompileClauses(preamble)
	for _, clause := range builtins {
		clause.Pkg = builtinsPkg
		addClause(builtinsPkg.Exported, clause)
	}
	addClause(builtinsPkg.Exported, failClause)
	addClause(builtinsPkg.Internal, builtinUnicodeIterPredicate())
	for _, pred := range comparisonPredicates {
		addClause(builtinsPkg.Exported, builtinComparisonPredicate(pred))
	}
	for _, pred := range typeCheckPredicates {
		addClause(builtinsPkg.Exported, builtinTypeCheckPredicate(pred))
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
	failClause = &Clause{builtinsPkg, Functor{"fail", 0}, 0, []Instruction{fail{}}}
	preamble   = []*logic.Clause{
		// This function can't be moved to base.pl because it's used by the parser.
		dsl.Clause(comp("\\=", var_("X"), var_("Y")),
			comp("\\+", comp("=", var_("X"), var_("Y")))),

		// Unifiable builtin.
		dsl.Clause(comp("unifiable", var_("X"), var_("Y"), var_("Unifier")),
			comp("asm", comp("builtin", atom("unifiable"), ptr(builtinUnifiable), var_("X0"), var_("X1"), var_("X2")))),

		// Unicode builtins.
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

func builtinUnicodeCheck(m *Machine, args []Addr) (InstrAddr, error) {
	ch := deref(m.get(args[0])).(WAtom)
	category := deref(m.get(args[1])).(WAtom)

	r, ok := runes.Single(string(ch))
	if !ok {
		return m.backtrack(errors.New("Ch is not a single rune"))
	}
	table, ok := unicodeTable[string(category)]
	if !ok {
		return m.backtrack(errors.New("Unknown Unicode table: %q", category))
	}
	if !unicode.Is(table, r) {
		return m.backtrack(errors.New("%c is not %s", r, category))
	}
	return m.forward()
}

var unicodeCharsCache = map[*unicode.RangeTable][]rune{}

func builtinUnicodeIter(m *Machine, args []Addr) (InstrAddr, error) {
	m.restoreFromChoicePoint()
	ref := deref(m.Reg[0]).(*Ref)
	pos := deref(m.Reg[1]).(WInt)
	category := deref(m.Reg[2]).(WAtom)
	table, ok := unicodeTable[string(category)]
	if !ok {
		return m.backtrack(errors.New("Unknown Unicode table: %q", category))
	}
	chars, ok := unicodeCharsCache[table]
	if !ok {
		chars = runes.All(table)
		unicodeCharsCache[table] = chars
	}
	i := int(pos)
	if i >= len(chars) {
		m.ChoicePoint = m.ChoicePoint.Prev
		return m.backtrack(errors.New("no more chars"))
	}
	ch := WAtom(string(chars[i]))
	i++
	m.ChoicePoint.Args[1] = WInt(i) // Increment position for next backtrack.
	return m.tryUnify(ref, ch)
}

func builtinUnicodeIterPredicate() *Clause {
	c := &Clause{Pkg: builtinsPkg, Functor: Functor{"unicode_iter", 3}, NumRegisters: 3}
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
		Pkg:          builtinsPkg,
		Functor:      pred.functor,
		NumRegisters: 2,
		Code: []Instruction{
			builtinComparisonInstruction(pred, RegAddr(0), RegAddr(1)),
			proceed{},
		},
	}
}

func makeComparisonPredicate(pred comparisonPredicate) func(*Machine, []Addr) (InstrAddr, error) {
	return func(m *Machine, args []Addr) (InstrAddr, error) {
		x1, x2 := deref(m.get(args[0])), deref(m.get(args[1]))
		if o := compareCells(x1, x2); o == pred.accepts1 || o == pred.accepts2 {
			return m.forward()
		}
		return m.backtrack(errors.New("%v: %v %s %v is false", pred.functor, x1, pred.functor.Name, x2))
	}
}

// ---- dif

func builtinUnifiable(m *Machine, args []Addr) (InstrAddr, error) {
	x, y, unifier := m.get(args[0]), m.get(args[1]), m.get(args[2])
	bindings, _, err := m.unifyBindings(x, y)
	if err != nil {
		// Unification failed.
		return m.backtrack(err)
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
	return m.tryUnify(unifier, list)
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
		Pkg:          builtinsPkg,
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

func makeTypeCheckPredicate(pred typeCheckPredicate) func(m *Machine, args []Addr) (InstrAddr, error) {
	return func(m *Machine, args []Addr) (InstrAddr, error) {
		if typeCheck(m, args[0]) == pred.name {
			return m.forward()
		}
		return m.backtrack(pred.err)
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
