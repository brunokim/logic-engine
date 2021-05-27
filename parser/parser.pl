package(parser, [], ['parse/2', 'parse_kb/2', 'parse_query/2']).

% Parse term
parse(Chars, Tree) :-
    ws(Chars, Ch1),
    term(Tree, Ch1, Ch2),
    ws(Ch2, []).

% Parse knowledge base
parse_kb(Chars, Rules) :-
    ws(Chars, Ch1),
    rules(Rules, Ch1, Ch2),
    ws(Ch2, []).

% Parse query
parse_query(Chars, Terms) :-
    ws(Chars, Ch1),
    terms(Terms, Ch1, Ch2),
    ws(Ch2, []).

% Whitespace
ws --> [Ch], {unicode_space(Ch), !}, ws.
ws --> comment, ws.
ws --> [].

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
ident(Ch) :- unicode_letter(Ch), !.
ident(Ch) :- unicode_digit(Ch), !.
ident('_').
idents([Ch|L], [Ch|L1], L2) :-
    ident(Ch),
    idents(L, L1, L2).
idents([], L, L).

% Chars with syntactic meaning
syntactic_char('(').
syntactic_char(')').
syntactic_char('{').
syntactic_char('}').
syntactic_char('[').
syntactic_char(']').
syntactic_char('.').
syntactic_char(',').
syntactic_char(':').
syntactic_char('|').
syntactic_char('"').
syntactic_char('_').
syntactic_char('\'').

% Symbolic chars (e.g., "!=")
atom_symbol(Ch) :-
    unicode_symbol(Ch),
    \+(syntactic_char(Ch)).
atom_symbol(Ch) :-
    unicode_punct(Ch),
    \+(syntactic_char(Ch)).
atom_symbols([Ch|L], [Ch|L1], L2) :-
    atom_symbol(Ch),
    atom_symbols(L, L1, L2).
atom_symbols([], L, L).

% Digits
digits([Ch|L], [Ch|L1], L2) :-
    unicode_digit(Ch),
    digits(L, L1, L2).
digits([], L, L).

% Plain atoms
atom(atom([Ch|L]), [Ch|L1], L2) :-
    unicode_lower(Ch), !,
    idents(L, L1, L2).
atom(atom([Ch|L]), [Ch|L1], L2) :-
    atom_symbol(Ch), !,
    atom_symbols(L, L1, L2).
atom(atom(Chars), ['\''|L1], L2) :-
    quoted('\'', Chars, L1, ['\''|L2]).

% Quoted atoms and strings
quoted(Delim, [Delim|Chars], ['\\', Delim|L1], L2) :-
    quoted(Delim, Chars, L1, L2).
quoted(Delim, ['\\'|Chars], ['\\', '\\'|L1], L2) :-
    quoted(Delim, Chars, L1, L2).
quoted(Delim, ['\n'|Chars], ['\\', 'n'|L1], L2) :-
    quoted(Delim, Chars, L1, L2).
quoted(Delim, [Ch|Chars], [Ch|L1], L2) :-
    \=(Ch, Delim),
    \=(Ch, '\\'),
    \=(Ch, '\n'),
    quoted(Delim, Chars, L1, L2).
quoted(_, [], L, L).

% Int
int(int([Ch|L]), [Ch|L1], L2) :-
    unicode_digit(Ch),
    digits(L, L1, L2).

% Vars
var(var([Ch|L]), [Ch|L1], L2) :-
    unicode_upper(Ch),
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

% Strings: lists of single-rune atoms
list(list(Terms), ['"'|L1], L2) :-
    quoted('"', Chars, L1, ['"'|L2]),
    atoms(Chars, Terms).
atoms([Ch|Chars], [atom([Ch])|Terms]) :-
    atoms(Chars, Terms).
atoms([], []).

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
assocs([Assoc|Assocs], L1, L5) :-
    assoc(Assoc, L1, L2),
    ws(L2, [','|L3]), !,
    ws(L3, L4),
    assocs(Assocs, L4, L5).
assocs([Assoc], L1, L2) :-
    assoc(Assoc, L1, L2).
assocs([], L, L).

% Term sequence
terms([Term|Terms], L1, L5) :-
    term(Term, L1, L2),
    ws(L2, [','|L3]), !,
    ws(L3, L4),
    terms(Terms, L4, L5).
terms([Term], L1, L2) :-
    term(Term, L1, L2).
terms([], L, L).

% Terms
term(Term, L1, L2) :- comp(Term, L1, L2), !.
term(Term, L1, L2) :- atom(Term, L1, L2), !.
term(Term, L1, L2) :- int(Term, L1, L2), !.
term(Term, L1, L2) :- var(Term, L1, L2), !.
term(Term, L1, L2) :- list(Term, L1, L2), !.
term(Term, L1, L2) :- assoc(Term, L1, L2), !.
term(Term, L1, L2) :- dict(Term, L1, L2).

% Clause: fact and rule
clause_head(Term, L1, L2) :- comp(Term, L1, L2).
clause_head(Term, L1, L2) :- atom(Term, L1, L2).
clause(clause(Fact), L1, L3) :-
    clause_head(Fact, L1, L2),
    ws(L2, ['.'|L3]).
clause(clause(Head, Body), L1, L6) :-
    clause_head(Head, L1, L2),
    ws(L2, [':', '-'|L3]),
    ws(L3, L4),
    terms(Body, L4, L5),
    ws(L5, ['.'|L6]).

% DCGs
dcg(dcg(Head, Body), L1, L6) :-
    clause_head(Head, L1, L2),
    ws(L2, ['-', '-', '>'|L3]),
    ws(L3, L4),
    dcg_terms(Body, L4, L5),
    ws(L5, ['.'|L6]).

dcg_terms([Term|Terms], L1, L5) :-
    dcg_term(Term, L1, L2),
    ws(L2, [','|L3]), !,
    ws(L3, L4),
    dcg_terms(Terms, L4, L5).
dcg_terms([Term], L1, L2) :-
    dcg_term(Term, L1, L2).
dcg_terms([], L, L).

dcg_term(Term, L1, L2) :- comp(Term, L1, L2), !.
dcg_term(Term, L1, L2) :- atom(Term, L1, L2), !.
dcg_term(Term, L1, L2) :- list(Term, L1, L2), !.
dcg_term(dcg_goals(Terms), ['{'|L1], L4) :-
   ws(L1, L2),
   terms(Terms, L2, L3),
   ws(L3, ['}'|L4]).

% Sequence of rules (clauses and DCGs).
rules([Rule|L], L1, L4) :-
    rule(Rule, L1, L2),
    ws(L2, L3),
    rules(L, L3, L4).
rules([], L, L).

rule(Rule, L1, L2) :- clause(Rule, L1, L2), !.
rule(Rule, L1, L2) :- dcg(Rule, L1, L2).
