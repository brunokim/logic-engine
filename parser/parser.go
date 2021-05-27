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
		dcg(comp("parse_", var_("Tree")),
			atom("ws"), comp("term", var_("Tree")), atom("ws")),
		clause(comp("parse", var_("Chars"), var_("Tree")),
			comp("phrase", comp("parse_", var_("Tree")), var_("Chars"))),
		// Parse knowledge base
		dcg(comp("parse_kb_", var_("Rules")),
			atom("ws"), comp("rules", var_("Rules")), atom("ws")),
		clause(comp("parse_kb", var_("Chars"), var_("Rules")),
			comp("phrase", comp("parse_kb_", var_("Rules")), var_("Chars"))),
		// Parse query
		dcg(comp("parse_query_", var_("Terms")),
			atom("ws"), comp("terms", var_("Terms")), atom("ws")),
		clause(comp("parse_query", var_("Chars"), var_("Terms")),
			comp("phrase", comp("parse_query_", var_("Terms")), var_("Chars"))),
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

		dcg(comp("idents", ilist(var_("Ch"), var_("L"))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("ident", var_("Ch"))),
			comp("idents", var_("L"))),
		dcg(comp("idents", list()), dcg_list()),
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
		dcg(comp("atom_symbols", ilist(var_("Ch"), var_("L"))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("atom_symbol", var_("Ch"))),
			comp("atom_symbols", var_("L"))),
		dcg(comp("atom_symbols", list()), dcg_list()),
		// Digits
		dcg(comp("digits", ilist(var_("Ch"), var_("L"))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("unicode_digit", var_("Ch"))),
			comp("digits", var_("L"))),
		dcg(comp("digits", list()), dcg_list()),
		// Atom
		dcg(comp("atom", comp("atom", ilist(var_("Ch"), var_("L")))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("unicode_lower", var_("Ch")), atom("!")),
			comp("idents", var_("L"))),
		dcg(comp("atom", comp("atom", ilist(var_("Ch"), var_("L")))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("atom_symbol", var_("Ch")), atom("!")),
			comp("atom_symbols", var_("L"))),
		dcg(comp("atom", comp("atom", var_("Chars"))),
			dcg_list(atom(`'`)),
			comp("quoted", atom(`'`), var_("Chars")),
			dcg_list(atom(`'`))),
		// Quoted atoms and strings
		dcg(comp("quoted", var_("Delim"), ilist(var_("Delim"), var_("Chars"))),
			dcg_list(atom(`\`), var_("Delim")),
			comp("quoted", var_("Delim"), var_("Chars"))),
		dcg(comp("quoted", var_("Delim"), ilist(atom(`\`), var_("Chars"))),
			dcg_list(atom(`\`), atom(`\`)),
			comp("quoted", var_("Delim"), var_("Chars"))),
		dcg(comp("quoted", var_("Delim"), ilist(atom("\n"), var_("Chars"))),
			dcg_list(atom(`\`), atom(`n`)),
			comp("quoted", var_("Delim"), var_("Chars"))),
		dcg(comp("quoted", var_("Delim"), ilist(var_("Ch"), var_("Chars"))),
			dcg_list(var_("Ch")),
			dcg_goals(
				comp("\\=", var_("Ch"), var_("Delim")),
				comp("\\=", var_("Ch"), atom(`\`)),
				comp("\\=", var_("Ch"), atom("\n"))),
			comp("quoted", var_("Delim"), var_("Chars"))),
		dcg(comp("quoted", var_("_"), list()), dcg_list()),
		// Int
		dcg(comp("int", comp("int", ilist(var_("Ch"), var_("L")))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("unicode_digit", var_("Ch"))),
			comp("digits", var_("L"))),
		// Var
		dcg(comp("var", comp("var", ilist(var_("Ch"), var_("L")))),
			dcg_list(var_("Ch")),
			dcg_goals(comp("unicode_upper", var_("Ch"))),
			comp("idents", var_("L"))),
		dcg(comp("var", comp("var", ilist(atom("_"), var_("L")))),
			dcg_list(atom("_")),
			comp("idents", var_("L"))),
		// Comp
		dcg(comp("comp", comp("comp", var_("Functor"), var_("Args"))),
			comp("atom", comp("atom", var_("Functor"))),
			dcg_list(atom("(")),
			atom("ws"),
			comp("terms", var_("Args")),
			atom("ws"),
			dcg_list(atom(")"))),
		// List
		dcg(comp("list", comp("list", var_("Terms"))),
			dcg_list(atom("[")),
			atom("ws"),
			comp("terms", var_("Terms")),
			atom("ws"),
			dcg_list(atom("]"))),
		// Incomplete list
		dcg(comp("list", comp("list", var_("Terms"), var_("Tail"))),
			dcg_list(atom("[")), atom("ws"),
			comp("terms", var_("Terms")), atom("ws"),
			dcg_list(atom("|")), atom("ws"),
			comp("term", var_("Tail")), atom("ws"),
			dcg_list(atom("]"))),
		// Strings: lists of single-rune atoms
		dcg(comp("list", comp("list", var_("Terms"))),
			dcg_list(atom(`"`)),
			comp("quoted", atom(`"`), var_("Chars")),
			dcg_list(atom(`"`)),
			dcg_goals(comp("atoms", var_("Chars"), var_("Terms")))),
		clause(comp("atoms", ilist(var_("Ch"), var_("Chars")), ilist(comp("atom", list(var_("Ch"))), var_("Terms"))),
			comp("atoms", var_("Chars"), var_("Terms"))),
		clause(comp("atoms", list(), list())),
		// Assoc
		dcg(comp("assoc", comp("assoc", var_("Key"), var_("Val"))),
			// Parsing limitation: can't have inline ':' yet, or we will loop!
			dcg_list(atom(":")),
			atom("ws"),
			comp("term", var_("Key")),
			atom("ws"),
			comp("term", var_("Val"))),
		// Dict
		dcg(comp("dict", comp("dict", var_("Assocs"))),
			dcg_list(atom("{")),
			atom("ws"),
			comp("assocs", var_("Assocs")),
			atom("ws"),
			dcg_list(atom("}"))),
		// Incomplete dict
		dcg(comp("dict", comp("dict", var_("Assocs"), var_("Parent"))),
			dcg_list(atom("{")),
			atom("ws"),
			comp("assocs", var_("Assocs")),
			atom("ws"),
			dcg_list(atom("|")),
			atom("ws"),
			comp("term", var_("Parent")),
			atom("ws"),
			dcg_list(atom("}"))),
		// Assocs
		dcg(comp("assocs", ilist(var_("Assoc"), var_("Assocs"))),
			comp("assoc", var_("Assoc")),
			atom("ws"),
			dcg_list(atom(",")),
			dcg_goals(atom("!")),
			atom("ws"),
			comp("assocs", var_("Assocs"))),
		dcg(comp("assocs", list(var_("Assoc"))), comp("assoc", var_("Assoc"))),
		dcg(comp("assocs", list()), dcg_list()),
		// Terms
		dcg(comp("terms", ilist(var_("Term"), var_("Terms"))),
			comp("term", var_("Term")),
			atom("ws"),
			dcg_list(atom(",")),
			dcg_goals(atom("!")),
			atom("ws"),
			comp("terms", var_("Terms"))),
		dcg(comp("terms", list(var_("Term"))), comp("term", var_("Term"))),
		dcg(comp("terms", list()), dcg_list()),
		// Term
		dcg(comp("term", var_("Term")), comp("comp", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("term", var_("Term")), comp("atom", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("term", var_("Term")), comp("int", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("term", var_("Term")), comp("var", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("term", var_("Term")), comp("list", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("term", var_("Term")), comp("assoc", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("term", var_("Term")), comp("dict", var_("Term"))),
		// Clause head: atom and comp
		dcg(comp("clause_head", var_("Term")), comp("comp", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("clause_head", var_("Term")), comp("atom", var_("Term"))),
		// Clause
		dcg(comp("clause", comp("clause", var_("Fact"))),
			comp("clause_head", var_("Fact")),
			atom("ws"),
			dcg_list(atom("."))),
		dcg(comp("clause", comp("clause", var_("Head"), var_("Body"))),
			comp("clause_head", var_("Head")),
			atom("ws"),
			dcg_list(atom(":"), atom("-")),
			atom("ws"),
			comp("terms", var_("Body")),
			atom("ws"),
			dcg_list(atom("."))),
		// DCGs
		dcg(comp("dcg", comp("dcg", var_("Head"), var_("Body"))),
			comp("clause_head", var_("Head")),
			atom("ws"),
			dcg_list(atom("-"), atom("-"), atom(">")),
			atom("ws"),
			comp("dcg_terms", var_("Body")),
			atom("ws"),
			dcg_list(atom("."))),
		dcg(comp("dcg_terms", ilist(var_("Term"), var_("Terms"))),
			comp("dcg_term", var_("Term")),
			atom("ws"),
			dcg_list(atom(",")),
			dcg_goals(atom("!")),
			atom("ws"),
			comp("dcg_terms", var_("Terms"))),
		dcg(comp("dcg_terms", list(var_("Term"))), comp("dcg_term", var_("Term"))),
		dcg(comp("dcg_terms", list()), dcg_list()),

		dcg(comp("dcg_term", var_("Term")), comp("comp", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("dcg_term", var_("Term")), comp("atom", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("dcg_term", var_("Term")), comp("list", var_("Term")), dcg_goals(atom("!"))),
		dcg(comp("dcg_term", comp("dcg_goals", var_("Terms"))),
			dcg_list(atom("{")),
			atom("ws"),
			comp("terms", var_("Terms")),
			atom("ws"),
			dcg_list(atom("}"))),
		// Rules
		dcg(comp("rules", ilist(var_("Rule"), var_("L"))),
			comp("rule", var_("Rule")),
			atom("ws"),
			comp("rules", var_("L"))),
		dcg(comp("rules", list()), dcg_list()),
		dcg(comp("rule", var_("Rule")), comp("clause", var_("Rule")), dcg_goals(atom("!"))),
		dcg(comp("rule", var_("Rule")), comp("dcg", var_("Rule"))),
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
