% Studying how to add error reporting to parser.

:- module(parser, [tokens/2]).

:- use_module(library(clpfd)).

list([L|Ls]) --> [L], list(Ls).
list([]) --> [].

%%%%

tokens(Str, Tokens) :-
    ( var(Tokens) ->
        string_codes(Str, Codes),
        phrase(tokens_(Tokens, pos(1, 1), _), Codes), !
    ;   phrase(tokens_(Tokens, pos(1, 1), _), Codes), !,
        string_codes(Str, Codes)
    ).

tokens_([token(Type, Str, S0)|Tokens], S0, S3) -->
    token(Type, Codes),
    {
        string_codes(Str, Codes),
        length(Codes, Len),
        S0 = pos(Line, Col),
        S1 = pos(Line, Col1),
        Col1 #= Col + Len
    },
    ws(S1, S2),
    tokens_(Tokens, S2, S3).
tokens_([], S, S) --> [].

ws(pos(Line, Col), S1) -->
    [0'\s],
    {Col1 #= Col + 1},
    ws(pos(Line, Col1), S1).
ws(pos(Line, _), S1) -->
    [0'\n],
    {Line1 #= Line + 1},
    ws(pos(Line1, 1), S1).
ws(S, S) --> [].

token(atom, [Ch|Codes]) --> lower(Ch), idents(Codes).
token(atom, Codes)      --> `'`, quoted(0'', Codes), `'`.
token(var, [Ch|Codes])  --> varfirst(Ch), idents(Codes).
token(int, [Ch|Codes])  --> digit(Ch), digits(Codes).
token(str, Codes)       --> `"`, quoted(0'", Codes), `"`.

token(oparen, `(`)    --> `(`. 
token(cparen, `)`)    --> `)`. 
token(obracket, `[`)  --> `[`. 
token(cbracket, `]`)  --> `]`. 
token(obrace, `{`)    --> `{`. 
token(cbrace, `}`)    --> `}`. 
token(comma, `,`)     --> `,`. 
token(semicolon, `;`) --> `;`. 
token(colon, `:`)     --> `:`. 
token(period, `.`)    --> `.`. 

quoted(Delim, [Ch|Codes])    --> {dif(Ch, 0'\\), dif(Ch, 0'\n), dif(Ch, Delim)}, [Ch], quoted(Delim, Codes).
quoted(Delim, [Delim|Codes]) --> [0'\\, Delim], quoted(Delim, Codes).
quoted(Delim, [0'\\|Codes])  --> [0'\\, 0'\\], quoted(Delim, Codes).
quoted(Delim, [0'\n|Codes])  --> [0'\\, 0'n], quoted(Delim, Codes).
quoted(_, [])                --> [].

digits([Ch|Codes]) --> digit(Ch), digits(Codes).
digits([])         --> [].

ident(Ch) --> lower(Ch) | varfirst(Ch) | digit(Ch).

varfirst('_') --> ['_'].
varfirst(Ch)  --> upper(Ch).

idents([Ch|Codes]) --> ident(Ch), idents(Codes).
idents([])         --> [].

lower(Ch) --> [Ch], {unicode_property(Ch, category('Ll'))}.
upper(Ch) --> [Ch], {unicode_property(Ch, category('Lu'))}.
digit(Ch) --> [Ch], {unicode_property(Ch, category('N'))}.
