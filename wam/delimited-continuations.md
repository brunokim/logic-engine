
# Delimited Continuations for Prolog

## Sample code

```
reset(Goal, Cont, Term) :-
    call(Goal),
    reset_marker,
    asm(get_value(Cont, reg(0))),
    asm(get_value(Term, reg(1))).

shift(Term) :-
    % 1. Capture continuation
    next_env(first, Env, PC),
    get_chunks(Env, PC, Chunks),
    Cont = call_continuation(Chunks),
    
    % 2. Pass control to reset
    asm(put_value(Cont, reg(0))),
    asm(put_value(Term, reg(1))),
    unwind_environments.

% Builds a list of all pieces of work remaining from each
% environment up to the reset marker.
get_chunks(Env, PC, Chunks) :-
    (is_reset_marker(PC) ->
        Chunks = []
    ;
        get_chunk(Env, PC, Residual),
        Chunks = [Residual|Rest],
        next_env(Env, NextEnv, NextPC),
        get_chunks(NextEnv, NextPC, Rest)
    ).

call_continuation([]).
call_continuation([Residual|Rest]) :-
    call_chunk(Residual),
    call_continuation(Rest).
```

## New pieces

- `reset_marker`: keeps a mark on the clause to be checked by 'is\_reset\_marker/1'.
  Also populates registers X0 and X1 with zeros when executed, for the case where
  there's no shift and the call(Goal) in reset returns normally.
- `asm(+Instr)`: places a literal instruction in the clause.
- `next_env(+BaseEnv, -NextEnv, -NextPC)`: return environment and program pointers
  from the environment pointed by BaseEnv. If it's "first", take from the current one.
- `unwind_environments`: sets the WAM's environment to that of the reset call, and the
  program pointer to just *after* the reset marker. Doesn't change the choicepoints.
- `is_reset_marker(+PC)`: succeeds if the program counter points to a reset marker.
- `get_chunk(+Env, +PC, -Residual)`: returns a term `$cont$(PC, LEV)` to restore execution
  of the environment state. This term needs to store the program counter and the live
  environment variables (LEV).
- `call_chunk($cont$(PC, LEV))`: rebuilds the environment stored in the term.

## New data types

It's necessary to be able to store Golang pointers within the Prolog program. We can
do this by extending the Constant to not only store strings, but any Value.
