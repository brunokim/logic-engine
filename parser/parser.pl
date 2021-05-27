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
comment --> "%", line, "\n".
comment(['%'|L1], []) :- line(L1, []).  % TODO: use eos here.
line --> [Ch], {\=(Ch, '\n')}, line.
line --> [].

% Identifier chars
ident(Ch) :- unicode_letter(Ch), !.
ident(Ch) :- unicode_digit(Ch), !.
ident('_').

idents([Ch|L]) --> [Ch], {ident(Ch)}, idents(L).
idents([]) --> [].

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

atom_symbols([Ch|L]) -->
    [Ch],
    {atom_symbol(Ch)},
    atom_symbols(L).
atom_symbols([]) --> [].

% Digits
digits([Ch|L]) -->
    [Ch],
    {unicode_digit(Ch)},
    digits(L).
digits([]) --> [].

% Plain atoms
atom(atom([Ch|L])) -->
    [Ch],
    {unicode_lower(Ch), !},
    idents(L).
atom(atom([Ch|L])) -->
    [Ch],
    {atom_symbol(Ch), !},
    atom_symbols(L).
atom(atom(Chars)) -->
    "'", quoted('\'', Chars), "'".

% Quoted atoms and strings
quoted(Delim, [Delim|Chars]) --> ['\\', Delim], quoted(Delim, Chars).
quoted(Delim, ['\\'|Chars])  --> "\\\\", quoted(Delim, Chars).
quoted(Delim, ['\n'|Chars])  --> "\\n", quoted(Delim, Chars).
quoted(Delim, [Ch|Chars]) -->
    [Ch],
    {\=(Ch, Delim), \=(Ch, '\\'), \=(Ch, '\n')},
    quoted(Delim, Chars).
quoted(_, []) --> [].

% Int
int(int([Ch|L])) -->
    [Ch],
    {unicode_digit(Ch)},
    digits(L).

% Vars
var(var([Ch|L])) -->
    [Ch],
    {unicode_upper(Ch)},
    idents(L).
var(var(['_'|L])) --> "_", idents(L).

% Compound terms
comp(comp(Functor, Args)) -->
    atom(atom(Functor)), "(", ws, terms(Args), ws, ")".

% List and incomplete lists
list(list(Terms)) -->
    "[", ws, terms(Terms), ws, "]".
list(list(Terms, Tail)) -->
    "[", ws, terms(Terms), ws, "|", ws, term(Tail), ws, "]".

% Strings: lists of single-rune atoms
list(list(Terms)) -->
    "\"", quoted('"', Chars), "\"",
    {atoms(Chars, Terms)}.
atoms([Ch|Chars], [atom([Ch])|Terms]) :-
    atoms(Chars, Terms).
atoms([], []).

% Assoc
assoc(assoc(Key, Val)) -->
    % Note: we need to have ':' as prefix instead of infix, otherwise we'll have a
    % left recursion.
    ":", ws, term(Key), ws, term(Val).

% Dict and incomplete dict
dict(dict(Assocs)) -->
    "{", ws, assocs(Assocs), ws, "}".
dict(dict(Assocs, Parent)) -->
    "{", ws, assocs(Assocs), ws, "|", ws, term(Parent), ws, "}".

% Assoc sequence
assocs([Assoc|Assocs]) -->
    assoc(Assoc), ws, ",", {!}, ws, assocs(Assocs).
assocs([Assoc]) -->
    assoc(Assoc).
assocs([]) --> [].

% Term sequence
terms([Term|Terms]) -->
    term(Term), ws, ",", {!}, ws, terms(Terms).
terms([Term]) -->
    term(Term).
terms([]) --> [].

% Terms
term(Term) --> comp(Term), {!}.
term(Term) --> atom(Term), {!}.
term(Term) --> int(Term), {!}.
term(Term) --> var(Term), {!}.
term(Term) --> list(Term), {!}.
term(Term) --> assoc(Term), {!}.
term(Term) --> dict(Term).

% Clause: fact and rule
clause_head(Term) --> comp(Term), {!}.
clause_head(Term) --> atom(Term).

clause(clause(Fact)) -->
    clause_head(Fact), ws, ".".
clause(clause(Head, Body)) -->
    clause_head(Head), ws, ":-", ws, terms(Body), ws, ".".

% DCGs
dcg(dcg(Head, Body)) -->
    clause_head(Head), ws, "-->", ws, dcg_terms(Body), ws, ".".

dcg_terms([Term|Terms]) -->
    dcg_term(Term), ws,
    ",", {!}, ws,
    dcg_terms(Terms).
dcg_terms([Term]) -->
    dcg_term(Term).
dcg_terms([]) --> [].

dcg_term(Term) --> comp(Term), {!}.
dcg_term(Term) --> atom(Term), {!}.
dcg_term(Term) --> list(Term), {!}.
dcg_term(dcg_goals(Terms)) -->
    "{", ws, terms(Terms), ws, "}".

% Sequence of rules (clauses and DCGs).
rules([Rule|L]) --> rule(Rule), ws, rules(L).
rules([]) --> [].

rule(Rule) --> clause(Rule), {!}.
rule(Rule) --> dcg(Rule).
