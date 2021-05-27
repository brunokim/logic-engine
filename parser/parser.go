// Package parser implements a Prolog parser using the engine itself.
//
// NOTE: currently assoc pairs must be written as ':key val' instead of 'key:val',
// because we can't yet support infix operators.
package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

var (
	var_      = dsl.Var
	comp      = dsl.Comp
	atom      = dsl.Atom
	list      = dsl.List
	ilist     = dsl.IList
	clause    = dsl.Clause
	dcg       = dsl.DCG
	dcg_goals = dsl.DCGGoals
	dcg_list  = dsl.DCGList
)

var (
	// ParserPkg is the underlying package with already-compiled parsing rules.
	ParserPkg *wam.Package
	grammar   = []logic.Rule{
		// Package declaration
		clause(comp("package", atom("parser"),
			list(),
			list(atom("parse/2"), atom("parse_kb/2"), atom("parse_query/2")))),
		// Parse term
		clause(comp("parse", var_("Chars"), var_("Tree")),
			comp("ws", var_("Chars"), var_("Ch1")),
			comp("term", var_("Tree"), var_("Ch1"), var_("Ch2")),
			comp("ws", var_("Ch2"), list())),
		// Parse knowledge base
		clause(comp("parse_kb", var_("Chars"), var_("Rules")),
			comp("ws", var_("Chars"), var_("Ch1")),
			comp("rules", var_("Rules"), var_("Ch1"), var_("Ch2")),
			comp("ws", var_("Ch2"), list())),
		// Parse query
		clause(comp("parse_query", var_("Chars"), var_("Terms")),
			comp("ws", var_("Chars"), var_("Ch1")),
			comp("terms", var_("Terms"), var_("Ch1"), var_("Ch2")),
			comp("ws", var_("Ch2"), list())),
		// Whitespace
		dcg(atom("ws"),
			dcg_list(var_("Ch")),
			dcg_goals(comp("unicode_space", var_("Ch")), atom("!")),
			atom("ws")),
		dcg(atom("ws"), atom("comment"), atom("ws")),
		dcg(atom("ws"), dcg_list()),
		// Comments
		dcg(atom("comment"), dcg_list(atom("%")), atom("line"), dcg_list(atom("\n"))),
		clause(comp("comment", ilist(atom("%"), var_("L1")), atom("[]")),
			comp("line", var_("L1"), atom("[]"))),
		dcg(atom("line"),
			dcg_list(var_("Ch")),
			dcg_goals(comp("\\=", var_("Ch"), atom("\n"))),
			atom("line")),
		dcg(atom("line"), dcg_list()),
		// Identifier chars
		clause(comp("ident", var_("Ch")), comp("unicode_letter", var_("Ch")), atom("!")),
		clause(comp("ident", var_("Ch")), comp("unicode_digit", var_("Ch")), atom("!")),
		clause(comp("ident", atom("_"))),
		clause(comp("idents", ilist(var_("Ch"), var_("L")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("ident", var_("Ch")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		clause(comp("idents", list(), var_("L"), var_("L"))),
		// Symbol chars
		clause(comp("syntactic_char", atom("("))),
		clause(comp("syntactic_char", atom(")"))),
		clause(comp("syntactic_char", atom("{"))),
		clause(comp("syntactic_char", atom("}"))),
		clause(comp("syntactic_char", atom("["))),
		clause(comp("syntactic_char", atom("]"))),
		clause(comp("syntactic_char", atom("."))),
		clause(comp("syntactic_char", atom(","))),
		clause(comp("syntactic_char", atom(":"))),
		clause(comp("syntactic_char", atom("|"))),
		clause(comp("syntactic_char", atom(`"`))),
		clause(comp("syntactic_char", atom("_"))),
		clause(comp("syntactic_char", atom("'"))),
		clause(comp("atom_symbol", var_("Ch")),
			comp("unicode_symbol", var_("Ch")),
			comp("\\+", comp("syntactic_char", var_("Ch")))),
		clause(comp("atom_symbol", var_("Ch")),
			comp("unicode_punct", var_("Ch")),
			comp("\\+", comp("syntactic_char", var_("Ch")))),
		clause(comp("atom_symbols", ilist(var_("Ch"), var_("L")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("atom_symbol", var_("Ch")),
			comp("atom_symbols", var_("L"), var_("L1"), var_("L2"))),
		clause(comp("atom_symbols", list(), var_("L"), var_("L"))),
		// Digits
		clause(comp("digits", ilist(var_("Ch"), var_("L")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("unicode_digit", var_("Ch")),
			comp("digits", var_("L"), var_("L1"), var_("L2"))),
		clause(comp("digits", list(), var_("L"), var_("L"))),
		// Atom
		clause(comp("atom", comp("atom", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("unicode_lower", var_("Ch")), atom("!"),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		clause(comp("atom", comp("atom", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("atom_symbol", var_("Ch")), atom("!"),
			comp("atom_symbols", var_("L"), var_("L1"), var_("L2"))),
		clause(comp("atom", comp("atom", var_("Chars")), ilist(atom(`'`), var_("L1")), var_("L2")),
			comp("quoted", atom(`'`), var_("Chars"), var_("L1"), ilist(atom(`'`), var_("L2")))),
		// Quoted atoms and strings
		clause(comp("quoted", var_("Delim"), ilist(var_("Delim"), var_("Chars")), ilist(atom(`\`), var_("Delim"), var_("L1")), var_("L2")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		clause(comp("quoted", var_("Delim"), ilist(atom(`\`), var_("Chars")), ilist(atom(`\`), atom(`\`), var_("L1")), var_("L2")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		clause(comp("quoted", var_("Delim"), ilist(atom("\n"), var_("Chars")), ilist(atom(`\`), atom(`n`), var_("L1")), var_("L2")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		clause(comp("quoted", var_("Delim"), ilist(var_("Ch"), var_("Chars")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("\\=", var_("Ch"), var_("Delim")),
			comp("\\=", var_("Ch"), atom(`\`)),
			comp("\\=", var_("Ch"), atom("\n")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		clause(comp("quoted", var_("_"), list(), var_("L"), var_("L"))),
		// Int
		clause(comp("int", comp("int", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("unicode_digit", var_("Ch")),
			comp("digits", var_("L"), var_("L1"), var_("L2"))),
		// Var
		clause(comp("var", comp("var", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("unicode_upper", var_("Ch")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		clause(comp("var", comp("var", ilist(atom("_"), var_("L"))), ilist(atom("_"), var_("L1")), var_("L2")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		// Comp
		clause(comp("comp", comp("comp", var_("Functor"), var_("Args")), var_("L1"), var_("L5")),
			comp("atom", comp("atom", var_("Functor")), var_("L1"), ilist(atom("("), var_("L2"))),
			comp("ws", var_("L2"), var_("L3")),
			comp("terms", var_("Args"), var_("L3"), var_("L4")),
			comp("ws", var_("L4"), ilist(atom(")"), var_("L5")))),
		// List
		clause(comp("list", comp("list", var_("Terms")), ilist(atom("["), var_("L1")), var_("L4")),
			comp("ws", var_("L1"), var_("L2")),
			comp("terms", var_("Terms"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("]"), var_("L4")))),
		// Incomplete list
		clause(comp("list", comp("list", var_("Terms"), var_("Tail")), ilist(atom("["), var_("L1")), var_("L7")),
			comp("ws", var_("L1"), var_("L2")),
			comp("terms", var_("Terms"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("|"), var_("L4"))),
			comp("ws", var_("L4"), var_("L5")),
			comp("term", var_("Tail"), var_("L5"), var_("L6")),
			comp("ws", var_("L6"), ilist(atom("]"), var_("L7")))),
		// Strings: lists of single-rune atoms
		clause(comp("list", comp("list", var_("Terms")), ilist(atom(`"`), var_("L1")), var_("L2")),
			comp("quoted", atom(`"`), var_("Chars"), var_("L1"), ilist(atom(`"`), var_("L2"))),
			comp("atoms", var_("Chars"), var_("Terms"))),
		clause(comp("atoms", ilist(var_("Ch"), var_("Chars")), ilist(comp("atom", list(var_("Ch"))), var_("Terms"))),
			comp("atoms", var_("Chars"), var_("Terms"))),
		clause(comp("atoms", list(), list())),
		// Assoc
		clause(comp("assoc", comp("assoc", var_("Key"), var_("Val")), ilist(atom(":"), var_("L1")), var_("L5")),
			// Parsing limitation: can't have inline ':' yet, or we will loop!
			comp("ws", var_("L1"), var_("L2")),
			comp("term", var_("Key"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), var_("L4")),
			comp("term", var_("Val"), var_("L4"), var_("L5"))),
		// Dict
		clause(comp("dict", comp("dict", var_("Assocs")), ilist(atom("{"), var_("L1")), var_("L4")),
			comp("ws", var_("L1"), var_("L2")),
			comp("assocs", var_("Assocs"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("}"), var_("L4")))),
		// Incomplete dict
		clause(comp("dict", comp("dict", var_("Assocs"), var_("Parent")), ilist(atom("{"), var_("L1")), var_("L7")),
			comp("ws", var_("L1"), var_("L2")),
			comp("assocs", var_("Assocs"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("|"), var_("L4"))),
			comp("ws", var_("L4"), var_("L5")),
			comp("term", var_("Parent"), var_("L5"), var_("L6")),
			comp("ws", var_("L6"), ilist(atom("}"), var_("L7")))),
		// Assocs
		clause(comp("assocs", ilist(var_("Assoc"), var_("Assocs")), var_("L1"), var_("L5")),
			comp("assoc", var_("Assoc"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(","), var_("L3"))), atom("!"),
			comp("ws", var_("L3"), var_("L4")),
			comp("assocs", var_("Assocs"), var_("L4"), var_("L5"))),
		clause(comp("assocs", list(var_("Assoc")), var_("L1"), var_("L2")),
			comp("assoc", var_("Assoc"), var_("L1"), var_("L2"))),
		clause(comp("assocs", list(), var_("L"), var_("L"))),
		// Terms
		clause(comp("terms", ilist(var_("Term"), var_("Terms")), var_("L1"), var_("L5")),
			comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(","), var_("L3"))), atom("!"),
			comp("ws", var_("L3"), var_("L4")),
			comp("terms", var_("Terms"), var_("L4"), var_("L5"))),
		clause(comp("terms", list(var_("Term")), var_("L1"), var_("L2")),
			comp("term", var_("Term"), var_("L1"), var_("L2"))),
		clause(comp("terms", list(), var_("L"), var_("L"))),
		// Term
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("comp", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("atom", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("int", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("var", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("list", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("assoc", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("dict", var_("Term"), var_("L1"), var_("L2"))),
		// Clause head: atom and comp
		clause(comp("clause_head", var_("Term"), var_("L1"), var_("L2")),
			comp("comp", var_("Term"), var_("L1"), var_("L2"))),
		clause(comp("clause_head", var_("Term"), var_("L1"), var_("L2")),
			comp("atom", var_("Term"), var_("L1"), var_("L2"))),
		// Clause
		clause(comp("clause", comp("clause", var_("Fact")), var_("L1"), var_("L3")),
			comp("clause_head", var_("Fact"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom("."), var_("L3")))),
		clause(comp("clause", comp("clause", var_("Head"), var_("Body")), var_("L1"), var_("L6")),
			comp("clause_head", var_("Head"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(":"), atom("-"), var_("L3"))),
			comp("ws", var_("L3"), var_("L4")),
			comp("terms", var_("Body"), var_("L4"), var_("L5")),
			comp("ws", var_("L5"), ilist(atom("."), var_("L6")))),
		// DCGs
		clause(comp("dcg", comp("dcg", var_("Head"), var_("Body")), var_("L1"), var_("L6")),
			comp("clause_head", var_("Head"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom("-"), atom("-"), atom(">"), var_("L3"))),
			comp("ws", var_("L3"), var_("L4")),
			comp("dcg_terms", var_("Body"), var_("L4"), var_("L5")),
			comp("ws", var_("L5"), ilist(atom("."), var_("L6")))),
		clause(comp("dcg_terms", ilist(var_("Term"), var_("Terms")), var_("L1"), var_("L5")),
			comp("dcg_term", var_("Term"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(","), var_("L3"))), atom("!"),
			comp("ws", var_("L3"), var_("L4")),
			comp("dcg_terms", var_("Terms"), var_("L4"), var_("L5"))),
		clause(comp("dcg_terms", list(var_("Term")), var_("L1"), var_("L2")),
			comp("dcg_term", var_("Term"), var_("L1"), var_("L2"))),
		clause(comp("dcg_terms", list(), var_("L"), var_("L"))),
		clause(comp("dcg_term", var_("Term"), var_("L1"), var_("L2")),
			comp("comp", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("dcg_term", var_("Term"), var_("L1"), var_("L2")),
			comp("atom", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("dcg_term", var_("Term"), var_("L1"), var_("L2")),
			comp("list", var_("Term"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("dcg_term", comp("dcg_goals", var_("Terms")), ilist(atom("{"), var_("L1")), var_("L4")),
			comp("ws", var_("L1"), var_("L2")),
			comp("terms", var_("Terms"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("}"), var_("L4")))),
		// Rules
		clause(comp("rules", ilist(var_("Rule"), var_("L")), var_("L1"), var_("L4")),
			comp("rule", var_("Rule"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), var_("L3")),
			comp("rules", var_("L"), var_("L3"), var_("L4"))),
		clause(comp("rules", list(), var_("L"), var_("L"))),
		clause(comp("rule", var_("Rule"), var_("L1"), var_("L2")),
			comp("clause", var_("Rule"), var_("L1"), var_("L2")), atom("!")),
		clause(comp("rule", var_("Rule"), var_("L1"), var_("L2")),
			comp("dcg", var_("Rule"), var_("L1"), var_("L2"))),
	}
)

func init() {
	pkg, err := wam.CompilePackage(grammar)
	if err != nil {
		panic(err)
	}
	ParserPkg = pkg
}

// ---- parse functions

func stack(env *wam.Env) string {
	var b strings.Builder
	for env != nil {
		b.WriteString("  - ")
		clause := env.Continuation.Clause
		if clause != nil {
			fmt.Fprintf(&b, " functor: %v\n    ", clause.Functor)
		}
		fmt.Fprintf(&b, " vars: %v\n", env.PermanentVars)
		env = env.Prev
	}
	return b.String()
}

func parseError(m *wam.Machine) error {
	state := m.DeepestError
	clause := state.CodePtr.Clause
	return fmt.Errorf("deepest backtrack at %v:\nargs: %v\nframes:\n%s", clause.Functor, state.Args, stack(state.Env))
}

// ParseTerm parses a single term.
func ParseTerm(text string) (logic.Term, error) {
	var letters []logic.Term
	for _, ch := range text {
		letters = append(letters, dsl.Atom(string(ch)))
	}
	tree := dsl.Var("Tree")
	m := wam.NewMachine()
	m.AddPackage(ParserPkg)
	//m.DebugFilename = fmt.Sprintf("debugtest/%s.jsonl", text)
	bindings, err := m.RunQuery(
		dsl.Comp("import", dsl.Atom("parser")),
		dsl.Comp("parse", dsl.List(letters...), tree))
	if err != nil {
		return nil, parseError(m)
	}
	return parseTerm(bindings[tree])
}

func parseTerm(term logic.Term) (t logic.Term, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()
	return decodeTerm(term), nil
}

// ParseRules parses a sequence of facts, rules and DCGs.
func ParseRules(text string) ([]logic.Rule, error) {
	var letters []logic.Term
	for _, ch := range text {
		letters = append(letters, dsl.Atom(string(ch)))
	}
	tree := dsl.Var("Tree")
	m := wam.NewMachine()
	m.AddPackage(ParserPkg)
	bindings, err := m.RunQuery(
		dsl.Comp("import", dsl.Atom("parser")),
		dsl.Comp("parse_kb", dsl.List(letters...), tree))
	if err != nil {
		return nil, parseError(m)
	}
	return parseRules(bindings[tree])
}

func parseRules(term logic.Term) (rules []logic.Rule, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()
	return decodeRules(term), nil
}

// ParseQuery parses a sequence of terms.
func ParseQuery(text string) ([]logic.Term, error) {
	var letters []logic.Term
	for _, ch := range text {
		letters = append(letters, dsl.Atom(string(ch)))
	}
	x := dsl.Var("Terms")
	m := wam.NewMachine()
	m.AddPackage(ParserPkg)
	bindings, err := m.RunQuery(
		dsl.Comp("import", dsl.Atom("parser")),
		dsl.Comp("parse_query", dsl.List(letters...), x))
	if err != nil {
		return nil, parseError(m)
	}
	return parseQuery(bindings[x])
}

func parseQuery(term logic.Term) (terms []logic.Term, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()
	return decodeTerms(term), nil
}

// ---- decode

func decodeRules(term logic.Term) []logic.Rule {
	if term == logic.EmptyList {
		return nil
	}
	l := expectList(term)
	rules := make([]logic.Rule, len(l.Terms))
	for i, term := range l.Terms {
		rules[i] = decodeRule(term)
	}
	return rules
}

func decodeRule(term logic.Term) logic.Rule {
	c, ok := term.(*logic.Comp)
	if !ok {
		panic(fmt.Errorf("decodeRule: expected comp, got %v", term))
	}
	switch c.Functor {
	case "clause":
		return decodeClause(term)
	case "dcg":
		return decodeDCG(term)
	default:
		panic(fmt.Errorf(`decodeRule: expected "clause" or "dcg" functors, got %q`, c.Functor))
	}
}

func decodeClause(term logic.Term) *logic.Clause {
	c := expectComp(term, "clause", 1, 2)
	var head logic.Term
	var body []logic.Term
	switch len(c.Args) {
	case 1:
		head = decodeClauseHead(c.Args[0])
	case 2:
		head = decodeClauseHead(c.Args[0])
		body = decodeTerms(c.Args[1])
	}
	return logic.NewClause(head, body...)
}

func decodeDCG(term logic.Term) *logic.DCG {
	c := expectComp(term, "dcg", 2)
	head := decodeClauseHead(c.Args[0])
	body := decodeDCGTerms(c.Args[1])
	return logic.NewDCG(head, body...)
}

func decodeClauseHead(term logic.Term) logic.Term {
	c, ok := term.(*logic.Comp)
	if !ok {
		panic(fmt.Errorf("expected comp, got %v", term))
	}
	switch c.Functor {
	case "atom":
		return decodeAtom(c)
	case "comp":
		return decodeComp(c)
	default:
		panic(fmt.Errorf(`expected "atom" or "comp" functors, got %q`, c.Functor))
	}
}

func decodeTerm(term logic.Term) logic.Term {
	c, ok := term.(*logic.Comp)
	if !ok {
		panic(fmt.Errorf("expected comp, got %v", term))
	}
	switch c.Functor {
	case "atom":
		return decodeAtom(term)
	case "int":
		return decodeInt(term)
	case "var":
		return decodeVar(term)
	case "comp":
		return decodeComp(term)
	case "list":
		return decodeList(term)
	case "assoc":
		return decodeAssoc(term)
	case "dict":
		return decodeDict(term)
	default:
		panic(fmt.Errorf("unknown functor %q", c.Functor))
	}
}

func decodeDCGTerms(term logic.Term) []logic.DCGTerm {
	if term == logic.EmptyList {
		return nil
	}
	l := expectList(term)
	terms := make([]logic.DCGTerm, len(l.Terms))
	for i, t := range l.Terms {
		terms[i] = decodeDCGTerm(t)
	}
	return terms
}

func decodeDCGTerm(term logic.Term) logic.DCGTerm {
	c, ok := term.(*logic.Comp)
	if !ok {
		panic(fmt.Errorf("decodeDCGTerms: expected comp, got %v", term))
	}
	switch c.Functor {
	case "atom":
		return decodeAtom(term)
	case "comp":
		return decodeComp(term)
	case "list":
		l := decodeList(term)
		if l == logic.EmptyList {
			return logic.EmptyList
		}
		return l.(*logic.List)
	case "dcg_goals":
		return decodeDCGGoals(term)
	default:
		panic(fmt.Errorf("decodeDCGTerms: unknown functor %q", c.Functor))
	}
}

func decodeTerms(term logic.Term) []logic.Term {
	if term == logic.EmptyList {
		return nil
	}
	l := expectList(term)
	terms := make([]logic.Term, len(l.Terms))
	for i, t := range l.Terms {
		terms[i] = decodeTerm(t)
	}
	return terms
}

func decodeAtom(term logic.Term) logic.Atom {
	c := expectComp(term, "atom", 1)
	name := decodeString(c.Args[0])
	return logic.Atom{Name: name}
}

func decodeInt(term logic.Term) logic.Int {
	c := expectComp(term, "int", 1)
	value := decodeString(c.Args[0])
	i, err := strconv.Atoi(value)
	if err != nil {
		panic(fmt.Errorf(""))
	}
	return logic.Int{Value: i}
}

func decodeVar(term logic.Term) logic.Var {
	c := expectComp(term, "var", 1)
	name := decodeString(c.Args[0])
	return logic.NewVar(name)
}

func decodeComp(term logic.Term) *logic.Comp {
	c := expectComp(term, "comp", 2)
	functor := decodeString(c.Args[0])
	args := decodeTerms(c.Args[1])
	return logic.NewComp(functor, args...)
}

func decodeList(term logic.Term) logic.Term {
	c := expectComp(term, "list", 1, 2)
	var terms []logic.Term
	var tail logic.Term = logic.EmptyList
	switch len(c.Args) {
	case 1:
		terms = decodeTerms(c.Args[0])
	case 2:
		terms = decodeTerms(c.Args[0])
		tail = decodeTerm(c.Args[1])
	}
	return logic.NewIncompleteList(terms, tail)
}

func decodeAssoc(term logic.Term) *logic.Assoc {
	c := expectComp(term, "assoc", 2)
	key := decodeTerm(c.Args[0])
	val := decodeTerm(c.Args[1])
	return logic.NewAssoc(key, val)
}

func decodeAssocs(term logic.Term) []*logic.Assoc {
	if term == logic.EmptyList {
		return nil
	}
	l := expectList(term)
	assocs := make([]*logic.Assoc, len(l.Terms))
	for i, t := range l.Terms {
		assocs[i] = decodeAssoc(t)
	}
	return assocs
}

func decodeDict(term logic.Term) logic.Term {
	c := expectComp(term, "dict", 1, 2)
	var assocs []*logic.Assoc
	var parent logic.Term = logic.EmptyDict
	switch len(c.Args) {
	case 1:
		assocs = decodeAssocs(c.Args[0])
	case 2:
		assocs = decodeAssocs(c.Args[0])
		parent = decodeTerm(c.Args[1])
	}
	return logic.NewIncompleteDict(assocs, parent)
}

func decodeString(term logic.Term) string {
	l := expectList(term)
	chars := make([]string, len(l.Terms))
	for i, t := range l.Terms {
		a := expectAtom(t)
		chars[i] = a.Name
	}
	return strings.Join(chars, "")
}

func decodeDCGGoals(term logic.Term) logic.DCGGoals {
	c := expectComp(term, "dcg_goals", 1)
	terms := decodeTerms(c.Args[0])
	return logic.DCGGoals(terms)
}

// ---- expect

func expectAtom(term logic.Term) logic.Atom {
	a, ok := term.(logic.Atom)
	if !ok {
		panic(fmt.Errorf("expected atom, got %v", term))
	}
	return a
}

func expectComp(term logic.Term, functor string, arities ...int) *logic.Comp {
	c, ok := term.(*logic.Comp)
	if !ok {
		panic(fmt.Errorf("expected comp, got %v", term))
	}
	if c.Functor != functor {
		panic(fmt.Errorf("expected %q functor, got %q", functor, c.Functor))
	}
	n := len(c.Args)
	for _, arity := range arities {
		if n == arity {
			return c
		}
	}
	panic(fmt.Errorf("expected arities %v, got %d", arities, n))
}

func expectList(term logic.Term) *logic.List {
	l, ok := term.(*logic.List)
	if !ok {
		panic(fmt.Errorf("expected list, got %v", term))
	}
	if l.Tail != logic.EmptyList {
		panic(fmt.Errorf("expected complete list, got %v", l))
	}
	return l
}
