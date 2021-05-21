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

'$join_attribute'(X, Y, Pkg) :-
    asm(call(X2, 'join_attribute/2')),
    asm(proceed(verify_attributes)).

'$check_attribute'(Attr, Value, Pkg) :-
    asm(call(X2, 'check_attribute/2')),
    asm(proceed(verify_attributes)).
