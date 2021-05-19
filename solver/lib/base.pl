%% These functions are all inlined by the compiler.
% They are here to be referenced by dynamic calls that are not
% optimized.

=(X, X).

true.
false :- fail.

->(Cond, Then,    _) :- Cond, !, Then.
->(   _,    _, Else) :- Else.

\+(Goal) :- ->(Goal, false, true).

and(A) :- A.
and(A, B) :- A, B.

get_attr(X, Attr) :- asm(get_attr(X0, X1)).
put_attr(X, Attr) :- asm(put_attr(X0, X1)).
del_attr(X, Attr) :- asm(del_attr(X0, X1)).

%% call/n predicates.
% These predicates are not self-referential, because the call within
% the body will be inlined with a call_meta instruction.

call(Fn) :- call(Fn).
call(Fn, A) :- call(Fn, A).
call(Fn, A, B) :- call(Fn, A, B).
call(Fn, A, B, C) :- call(Fn, A, B, C).
call(Fn, A, B, C, D) :- call(Fn, A, B, C, D).
call(Fn, A, B, C, D, E) :- call(Fn, A, B, C, D, E).
call(Fn, A, B, C, D, E, F) :- call(Fn, A, B, C, D, E, F).
call(Fn, A, B, C, D, E, F, G) :- call(Fn, A, B, C, D, E, F, G).
call(Fn, A, B, C, D, E, F, G, H) :- call(Fn, A, B, C, D, E, F, G, H).

%% Attribute checker machinery

'$join_attribute'(AttrName, X, Y) :-
    asm(call('join_attribute/3')),
    asm(proceed(unify)).

'$check_attribute'(Attr, Value) :-
    asm(call('check_attribute/2')),
    asm(proceed(unify)).

