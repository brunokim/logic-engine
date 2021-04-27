package wam_test

import (
	"testing"
	"unicode"

	"github.com/brunokim/logic-engine/dsl"
	"github.com/brunokim/logic-engine/logic"
	"github.com/brunokim/logic-engine/wam"
)

var (
	grammar = []*logic.Clause{
		// Parse term
		dsl.Clause(comp("parse", var_("Chars"), var_("Tree")),
			comp("ws", var_("Chars"), var_("Ch1")),
			comp("term", var_("Tree"), var_("Ch1"), var_("Ch2")),
			comp("ws", var_("Ch2"), list())),
		// Parse knowledge base
		dsl.Clause(comp("parse_kb", var_("Chars"), var_("Clauses")),
			comp("ws", var_("Chars"), var_("Ch1")),
			comp("clauses", var_("Clauses"), var_("Ch1"), var_("Ch2")),
			comp("ws", var_("Ch2"), list())),
		// Whitespace
		dsl.Clause(comp("ws", ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("space", var_("Ch")),
			comp("ws", var_("L1"), var_("L2"))),
		dsl.Clause(comp("ws", var_("L1"), var_("L3")),
			comp("comment", var_("L1"), var_("L2")),
			comp("ws", var_("L2"), var_("L3"))),
		dsl.Clause(comp("ws", var_("L"), var_("L"))),
		// Comments
		dsl.Clause(comp("comment", ilist(atom("%"), var_("L1")), var_("L2")),
			comp("line", var_("L1"), ilist(atom("\n"), var_("L2")))),
		dsl.Clause(comp("comment", ilist(atom("%"), var_("L1")), atom("[]")),
			comp("line", var_("L1"), atom("[]"))),
		dsl.Clause(comp("line", ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("\\=", var_("Ch"), atom("\n")),
			comp("line", var_("L1"), var_("L2"))),
		dsl.Clause(comp("line", var_("L"), var_("L"))),
		// Identifier chars
		dsl.Clause(comp("idents", ilist(var_("Ch"), var_("L")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("ident", var_("Ch")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("idents", list(), var_("L"), var_("L"))),
		// Symbol chars
		dsl.Clause(comp("symbols", ilist(var_("Ch"), var_("L")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("symbol", var_("Ch")),
			comp("symbols", var_("L"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("symbols", list(), var_("L"), var_("L"))),
		// Digits
		dsl.Clause(comp("digits", ilist(var_("Ch"), var_("L")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("digit", var_("Ch")),
			comp("digits", var_("L"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("digits", list(), var_("L"), var_("L"))),
		// Atom
		dsl.Clause(comp("atom", comp("atom", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("lower", var_("Ch")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("atom", comp("atom", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("symbol", var_("Ch")),
			comp("symbols", var_("L"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("atom", comp("atom", var_("Chars")), ilist(atom(`'`), var_("L1")), var_("L2")),
			comp("quoted", atom(`'`), var_("Chars"), var_("L1"), ilist(atom(`'`), var_("L2")))),
		// Quoted atoms and strings
		dsl.Clause(comp("quoted", var_("Delim"), ilist(var_("Delim"), var_("Chars")), ilist(atom(`\`), var_("Delim"), var_("L1")), var_("L2")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("quoted", var_("Delim"), ilist(atom(`\`), var_("Chars")), ilist(atom(`\`), atom(`\`), var_("L1")), var_("L2")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("quoted", var_("Delim"), ilist(atom("\n"), var_("Chars")), ilist(atom(`\`), atom(`n`), var_("L1")), var_("L2")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("quoted", var_("Delim"), ilist(var_("Ch"), var_("Chars")), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("\\=", var_("Ch"), var_("Delim")),
			comp("\\=", var_("Ch"), atom(`\`)),
			comp("\\=", var_("Ch"), atom("\n")),
			comp("quoted", var_("Delim"), var_("Chars"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("quoted", var_("_"), list(), var_("L"), var_("L"))),
		// Int
		dsl.Clause(comp("int", comp("int", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("digit", var_("Ch")),
			comp("digits", var_("L"), var_("L1"), var_("L2"))),
		// Var
		dsl.Clause(comp("var", comp("var", ilist(var_("Ch"), var_("L"))), ilist(var_("Ch"), var_("L1")), var_("L2")),
			comp("upper", var_("Ch")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("var", comp("var", ilist(atom("_"), var_("L"))), ilist(atom("_"), var_("L1")), var_("L2")),
			comp("idents", var_("L"), var_("L1"), var_("L2"))),
		// Comp
		dsl.Clause(comp("comp", comp("comp", var_("Functor"), var_("Args")), var_("L1"), var_("L5")),
			comp("atom", comp("atom", var_("Functor")), var_("L1"), ilist(atom("("), var_("L2"))),
			comp("ws", var_("L2"), var_("L3")),
			comp("terms", var_("Args"), var_("L3"), var_("L4")),
			comp("ws", var_("L4"), ilist(atom(")"), var_("L5")))),
		// List
		dsl.Clause(comp("list", comp("list", var_("Terms")), ilist(atom("["), var_("L1")), var_("L4")),
			comp("ws", var_("L1"), var_("L2")),
			comp("terms", var_("Terms"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("]"), var_("L4")))),
		// Incomplete list
		dsl.Clause(comp("list", comp("list", var_("Terms"), var_("Tail")), ilist(atom("["), var_("L1")), var_("L7")),
			comp("ws", var_("L1"), var_("L2")),
			comp("terms", var_("Terms"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("|"), var_("L4"))),
			comp("ws", var_("L4"), var_("L5")),
			comp("term", var_("Tail"), var_("L5"), var_("L6")),
			comp("ws", var_("L6"), ilist(atom("]"), var_("L7")))),
		// Assoc
		dsl.Clause(comp("assoc", comp("assoc", var_("Key"), var_("Val")), ilist(atom(":"), var_("L1")), var_("L5")),
			// Parsing limitation: can't have inline ':' yet, or we will loop!
			comp("ws", var_("L1"), var_("L2")),
			comp("term", var_("Key"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), var_("L4")),
			comp("term", var_("Val"), var_("L4"), var_("L5"))),
		// Dict
		dsl.Clause(comp("dict", comp("dict", var_("Assocs")), ilist(atom("{"), var_("L1")), var_("L4")),
			comp("ws", var_("L1"), var_("L2")),
			comp("assocs", var_("Assocs"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("}"), var_("L4")))),
		// Incomplete dict
		dsl.Clause(comp("dict", comp("dict", var_("Assocs"), var_("Parent")), ilist(atom("{"), var_("L1")), var_("L7")),
			comp("ws", var_("L1"), var_("L2")),
			comp("assocs", var_("Assocs"), var_("L2"), var_("L3")),
			comp("ws", var_("L3"), ilist(atom("|"), var_("L4"))),
			comp("ws", var_("L4"), var_("L5")),
			comp("term", var_("Parent"), var_("L5"), var_("L6")),
			comp("ws", var_("L6"), ilist(atom("}"), var_("L7")))),
		// Assocs
		dsl.Clause(comp("assocs", list(var_("Assoc")), var_("L1"), var_("L2")),
			comp("assoc", var_("Assoc"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("assocs", ilist(var_("Assoc"), var_("Assocs")), var_("L1"), var_("L5")),
			comp("assoc", var_("Assoc"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(","), var_("L3"))),
			comp("ws", var_("L3"), var_("L4")),
			comp("assocs", var_("Assocs"), var_("L4"), var_("L5"))),
		dsl.Clause(comp("assocs", list(), var_("L"), var_("L"))),
		// Terms
		dsl.Clause(comp("terms", list(var_("Term")), var_("L1"), var_("L2")),
			comp("term", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("terms", ilist(var_("Term"), var_("Terms")), var_("L1"), var_("L5")),
			comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(","), var_("L3"))),
			comp("ws", var_("L3"), var_("L4")),
			comp("terms", var_("Terms"), var_("L4"), var_("L5"))),
		dsl.Clause(comp("terms", list(), var_("L"), var_("L"))),
		// Term
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("atom", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("int", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("var", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("comp", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("list", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("assoc", var_("Term"), var_("L1"), var_("L2"))),
		dsl.Clause(comp("term", var_("Term"), var_("L1"), var_("L2")),
			comp("dict", var_("Term"), var_("L1"), var_("L2"))),
		// Clause
		dsl.Clause(comp("clause", comp("clause", var_("Fact")), var_("L1"), var_("L3")),
			comp("comp", var_("Fact"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom("."), var_("L3")))),
		dsl.Clause(comp("clause", comp("clause", var_("Head"), var_("Body")), var_("L1"), var_("L6")),
			comp("comp", var_("Head"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), ilist(atom(":"), atom("-"), var_("L3"))),
			comp("ws", var_("L3"), var_("L4")),
			comp("terms", var_("Body"), var_("L4"), var_("L5")),
			comp("ws", var_("L5"), ilist(atom("."), var_("L6")))),
		dsl.Clause(comp("clauses", ilist(var_("Clause"), var_("L")), var_("L1"), var_("L4")),
			comp("clause", var_("Clause"), var_("L1"), var_("L2")),
			comp("ws", var_("L2"), var_("L3")),
			comp("clauses", var_("L"), var_("L3"), var_("L4"))),
		dsl.Clause(comp("clauses", list(), var_("L"), var_("L"))),
	}
)

func facts() []*logic.Clause {
	syntactic := []rune{'(', ')', '{', '}', '[', ']', '.', ',', ':', '|', '"', '_'}
	s := make(map[rune]struct{})
	for _, ch := range syntactic {
		s[ch] = struct{}{}
	}
	var clauses []*logic.Clause
	for ch := rune(0); ch < rune(128); ch++ {
		if _, ok := s[ch]; ok {
			continue
		}
		if unicode.IsDigit(ch) {
			clauses = append(clauses, dsl.Clause(comp("digit", atom(string(ch)))))
		}
		if unicode.IsLetter(ch) {
			clauses = append(clauses, dsl.Clause(comp("letter", atom(string(ch)))))
		}
		if unicode.IsLower(ch) {
			clauses = append(clauses, dsl.Clause(comp("lower", atom(string(ch)))))
		}
		if unicode.IsUpper(ch) {
			clauses = append(clauses, dsl.Clause(comp("upper", atom(string(ch)))))
		}
		if unicode.IsPunct(ch) || unicode.IsSymbol(ch) {
			clauses = append(clauses, dsl.Clause(comp("symbol", atom(string(ch)))))
		}
		if unicode.IsSpace(ch) {
			clauses = append(clauses, dsl.Clause(comp("space", atom(string(ch)))))
		}
	}
	// identifier char = letter ; digit ; "_"
	clauses = append(clauses,
		dsl.Clause(comp("ident", var_("Ch")), comp("letter", var_("Ch"))),
		dsl.Clause(comp("ident", var_("Ch")), comp("digit", var_("Ch"))),
		dsl.Clause(comp("ident", atom("_"))))
	return clauses
}

var text = `
    % Parse term
    parse(Chars, Tree) :-
        ws(Chars, Ch1),
        term(Tree, Ch1, Ch2),
        ws(Ch2, []).

    % Parse knowledge base
    parse_kb(Chars, Clauses) :-
        ws(Chars, Ch1),
        clauses(Clauses, Ch1, Ch2),
        ws(Ch2, []).

    % Whitespace
    ws([Ch|L1], L2) :-
        space(Ch),
        ws(L1, L2).
    ws(L1, L3) :-
        comment(L1, L2),
        ws(L2, L3).
    ws(L, L).

    % Comments
    comment(['%'|L1], L2) :-
        line(L1, ['\n'|L2]).
    comment(['%'|L1], []) :-
        line(L1, []).
    line([Ch|L1], L2) :-
        \=(Ch, '\n'),
        line(L1, L2).
    line(L, L).

    % Identifier chars
    idents([Ch|L], [Ch|L1], L2) :-
        ident(Ch),
        idents(L, L1, L2).
    idents([], L, L).

    % Symbolic chars (e.g., "!=")
    symbols([Ch|L], [Ch|L1], L2) :-
        symbol(Ch),
        symbols(L, L1, L2).
    symbols([], L, L).

    % Digits
    digits([Ch|L], [Ch|L1], L2) :-
        digit(Ch),
        digits(L, L1, L2).
    digits([], L, L).

    % Plain atoms
    atom(atom([Ch|L]), [Ch|L1], L2) :-
        lower(Ch),
        idents(L, L1, L2).
    atom(atom([Ch|L]), [Ch|L1], L2) :-
        symbol(Ch),
        symbols(L, L1, L2).
    atom(atom(Chars), ['\''|L1], L2) :-
        quoted('\'', Chars, L1, ['\''|L2]).

    % Quoted atoms and strings
    quoted(Delim, [Delim|Chars], ['\\', Delim|L1], L2) :-
        quoted(Delim, Chars, L1, L2).
    quoted(Delim, ['\\'|Chars], ['\\', '\\'|L1], L2) :-
        quoted(Delim, Chars, L1, L2).
    quoted(Delim, ['\n'|Chars], ['\\', 'n'|L1], L2) :-
        quoted(Delim, Chars, L1, L2).
    quoted([Ch|Chars], [Ch|L1], L2) :-
        \=(Ch, Delim),
        \=(Ch, '\\'),
        \=(Ch, '\n'),
        quoted(Delim, Chars, L1, L2).
    quoted(_, [], L, L).

    % Int
    int(int([Ch|L]), [Ch|L1], L2) :-
        digit(Ch),
        digits(L, L1, L2).

    % Vars
    var(var([Ch|L]), [Ch|L1], L2) :-
        upper(Ch),
        idents(L, L1, L2).
    var(var(['_'|L]), ['_'|L1], L2) :-
        idents(L, L1, L2).

    % Compound terms
    comp(comp(Functor, Args), L1, L5) :-
        atom(atom(Functor), L1, ['('|L2]),
        ws(L2, L3),
        terms(Args, L3, L4),
        ws(L4, [')'|L5]).

    % List and incomplete lists
    list(list(Terms), ['['|L1], L4) :-
        ws(L1, L2),
        terms(Terms, L2, L3),
        ws(L3, [']'|L4]).
    list(list(Terms, Tail), ['['|L1], L7) :-
        ws(L1, L2),
        terms(Terms, L2, L3),
        ws(L3, ['|'|L4]),
        ws(L4, L5),
        term(Tail, L5, L6),
        ws(L6, [']'|L7]).

    % Assoc
    assoc(assoc(Key, Val), [':'|L1], L5) :-
        % Note: we need to have ':' as prefix instead of infix, otherwise we'll have a
        % left recursion.
        ws(L1, L2),
        term(Key, L2, L3),
        ws(L3, L4),
        term(Val, L4, L5).

    % Dict and incomplete dict
    dict(dict(Assocs), ['{'|L1], L4) :-
        ws(L1, L2),
        assocs(Assocs, L2, L3),
        ws(L3, ['}'|L4]).
    dict(dict(Assocs, Parent), ['{'|L1], L7) :-
        ws(L1, L2),
        assocs(Assocs, L2, L3),
        ws(L3, ['|'|L4]),
        ws(L4, L5),
        term(Parent, L5, L6),
        ws(L6, ['}'|L7]).

    % Assoc sequence
    assocs([Assoc], L1, L2) :-
        assoc(Assoc, L1, L2).
    assocs([Assoc|Assocs], L1, L5) :-
        assoc(Assoc, L1, L2),
        ws(L2, [','|L3]),
        ws(L3, L4),
        assocs(Assocs, L4, L5).
    assocs([], L, L).

    % Term sequence
    terms([Term], L1, L2) :-
        term(Term, L1, L2).
    terms([Term|Terms], L1, L5) :-
        term(Term, L1, L2),
        ws(L2, [','|L3]),
        ws(L3, L4),
        terms(Terms, L4, L5).
    terms([], L, L).

    % Terms
    term(Term, L1, L2) :- atom(Term, L1, L2).
    term(Term, L1, L2) :- int(Term, L1, L2).
    term(Term, L1, L2) :- var(Term, L1, L2).
    term(Term, L1, L2) :- comp(Term, L1, L2).
    term(Term, L1, L2) :- list(Term, L1, L2).
    term(Term, L1, L2) :- assoc(Term, L1, L2).
    term(Term, L1, L2) :- dict(Term, L1, L2).

    % Clause: fact and rule
	clause(clause(Fact), L1, L3) :-
        comp(Fact, L1, L2),
        ws(L2, ['.'|L3]).
    clause(clause(Head, Body), L1, L6) :-
        comp(Head, L1, L2),
        ws(L2, [':', '-'|L3]),
        ws(L3, L4),
        terms(Body, L4, L5),
        ws(L5, ['.'|L6]).

    % Clause list
    clauses([Clause|L], L1, L4) :-
        clause(Clause, L1, L2),
        ws(L2, L3),
        clauses(L, L3, L4).
    clauses([], L, L).
`

func TestRunCompiled(t *testing.T) {
	clauses, err := wam.CompileClauses(append(facts(), grammar...))
	if err != nil {
		t.Fatalf("CompileClauses: %v", err)
	}
	m := wam.NewMachine()
	for _, clause := range clauses {
		m.AddClause(clause)
	}
	var letters []logic.Term
	for _, ch := range text {
		letters = append(letters, atom(string(ch)))
	}
	//m.IterLimit = 15000
	//m.DebugFilename = "debugtest/run-compiled.jsonl"

	xs, err := m.RunQuery(comp("parse_kb", list(letters...), var_("Tree")))
	if err != nil {
		t.Fatalf("want nil, got err: %v", err)
	}
	if len(xs) != 1 {
		t.Fatalf("want 1 result, got %d: %v", len(xs), xs)
	}
	t.Logf("%v", xs[var_("Tree")])
}
