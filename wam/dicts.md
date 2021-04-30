# Dicts

Dicts or associative maps are common data structures nowadays, and deserve to be mapped into
this logic engine. They should provide fast (O(1) or O(log n)) key access and reasonable O(log n)
update operations.

## Rationale

This engine started to solve the type system of a language. As such, at any logical scope its
possible to re-define/shadow a symbol.

    {
        a := 10
        b := 20
        {
            a := "a"
            fmt.Println(a, b)  // a 20
        }
        fmt.Println(a, b)  // 10 20
    }

A scope can get very large, with hundreds of symbols, and the creation of an inner scope should
be cheap and allow for this overriding, without losing the original symbol.

## First attempt

The first attempt allows dicts to be open-ended, allowing fast adding or overwriting a key value.
A dict has a parent just like a list has a tail, and keys in the child override the keys in the
parent.

    ?- X = {a:10, b:20}, Y = {a:1|X}.
    Y = {a:1, b:20}.

This representation also allows for a nice unification expression, where we want to filter only a
handful of keys, from a potentially much larger dict.

    ?- X = {a:10, b:20, c:30}, {a:A|_} = X.
    A = 10.

However, this presents a difficulty if we want to unify the parent's value later, because it needs
to match non-determinstically any subset of keys of the child to be logically sound.

    ?- {a:1, b:2|X}.
    X = {} ;
    X = {a:_|_} ;
    X = {b:_|_} ;
    X = {a:_, b:_|_} .

This appears, for example, in the following query. X is already bound, but needs to match the
"parent" of Y after its keys [a, b] are unified. Since there's no way to anticipate which keys
are necessary to unify with another, potentially closed dict, we need to iterate over all possibilities.

    ?- X = {a:10, c:30}, Y = {a:A, b:B, c:C}, {a:1, b:2|X} = Y.
    % Wrong
    Y = {a:A, b:B|{c:C}}  =>  {c:C} = X  =>  {c:C} = {a:10, c:30}  % Don't unify
    % Right
    Y = {a:A, b:B|{a:_, c:C}}  =>  {a:_, c:C} = {a:10, c:30}  =>  C = 30


## SWI-Prolog

SWI-Prolog provides dicts and doesn't attempt to create open dicts. Instead, they are closed data
structures and unify if all their keys are the same; they also provide a '>:<' operator that unify only
their common keys, ignoring other ones.

If we choose to define this as unification, then getting a key is simple:

    ?- X = {a:10, c:30}, {a:A} = X.
    A = 10.

We may even define a Rest operation, to coalesce all keys that weren't used.

    ?- X = {a:10, c:30}, {a:A|Rest} = X.
    A = 10, Rest = {c:30}.

This can be used to add or even to override any existing keys.

    ?- X = {a:10, c:30}, Y = {b:20|X}, Y = {a:A, b:B, c:C}.
    A = 10, B = 20, C = 30.

    ?- X = {a:10, b:20, c:30}, Y = {a:1|X}, Y = {a:A, b:B, c:C}.
      {a:1|{a:10, b:20, c:30}} =?= {a:A, b:B, c:C}
      {a:1|{a:10, b:20, c:30}} =?= {a:A|{b:B, c:C}}
      A=1, {a:10, b:20, c:30}  =?= {b:B, c:C}
      A=1, B=20, C=30

The implementation must be smart *not* to mindlessly unify dict parents, or else weird stuff may happen.

    ?- X1={a:10}, X={a:1|X1}, Y1={a:A1}, Y= {a:A|Y1}, X=Y.
               X=Y
       {a:1|X1} = {a:A|X2}
               A=1,     X1=X2
               A=1, {a:10}={a:A1}
               A=1,     A1=10

## Monotonicity

What should happen when both parents are unbound, as in

    ?- {a:1, b:2|P1} = {a:X, c:3|P2}.

If you read `P1` as "input", then it may have keys `a` and `b`; the same for `P2` and `a` and `c`.
If you read them as "output" or "rest", they shouldn't have any of these keys.
Either way, they have the further **constraint** that any shared keys are now the same between both.

According to the first interpretation, we would have

    P1={c:3|P}, P2={b:2|P}, P={}; {a:_}; {b:_}; ...; {a:_, b:_, c:_}

And to the second, we would have

    P1={}, P2={}

I think I prefer the intermediate response

    P1={c:3}, P2={b:2}

because the unification can be seen as a symmetric difference.

**BUT**

If we further try to use `P1` and `P2`,


    ?- {a:1, b:2|P1} = {a:X, c:3|P2}, P1 = {c:2}.
    ?- {a:1, b:2|P1} = {a:X, c:3|P2}, P1 = {d:4}, P2={d:40}.

we should expect both of these to **fail** to preserve monotonicity. So the most general
semantics is the first interpretation, with simply

    P1={c:3|P}, P2={b:2|P}  % P is still unbound.

## Cases

    hasRest1 | hasRest2 | isComplete1 | isComplete2 | Example                       | Pairs
    ---------|----------|-------------|-------------|-------------------------------|-----------------
    false    | false    | false       | false       | {a:A, b:B|P1} = {a:1, b:2|P2} | A=1, B=2, P1=P2
    false    | false    | false       | true        | {a:A, b:B|P1} = {a:1, b:2}    | A=1, B=2
    false    | false    | true        | false       | {a:A, b:B} = {a:1, b:2|P2}    | A=1, B=2
    false    | false    | true        | true        | {a:A, b:B} = {a:1, b:2}       | A=1, B=2
    ---------|----------|-------------|-------------|-------------------------------|-----------------
    false    | true     | false       | false       | {a:A|P1} = {a:1, b:2|P2}      | A=1, P1={b:2|P2}
    false    | true     | false       | true        | {a:A|P1} = {a:1, b:2}         | A=1, P1={b:2}
    false    | true     | true        | false       | {a:A} = {a:1, b:2|P2}         | A=1
    false    | true     | true        | true        | {a:A} = {a:1, b:2}            | A=1
    ---------|----------|-------------|-------------|-------------------------------|-----------------
    true     | false    | false       | false       | {a:A, b:B|P1} = {a:1|P2}      | A=1, {b:B|P1}=P2
    true     | false    | false       | true        | {a:A, b:B|P1} = {a:1}         | A=1
    true     | false    | true        | false       | {a:A, b:B} = {a:1|P2}         | A=1, {b:B}=P2
    true     | false    | true        | true        | {a:A, b:B} = {a:1}            | A=1
    ---------|----------|-------------|-------------|-------------------------------|-----------------
    true     | true     | false       | false       | {a:A, c:C|P1} = {a:1, b:2|P2} | A=1, P1={b:2|P}, {c:C|P}=P2
    true     | true     | false       | true        | {a:A, c:C|P1} = {a:1, b:2}    | A=1, P1={b:2}
    true     | true     | true        | false       | {a:A, c:C} = {a:1, b:2|P2}    | A=1, {c:C}=P2
    true     | true     | true        | true        | {a:A, c:C} = {a:1, b:2}       | A=1


    case | build P1                      case | build P2
    -----|--------------------------     -----|--------------------------
    FFFF | P1 = P2                       FFFF | P1 = P2
    -----|--------------------------     -----|--------------------------
    FTFF | P1 = rollDict(rest2, P2)      TFFF | P2 = rollDict(rest1, P1)
    FTFT |                               TFTF |
    TTFT |                               TTTF |
    -----|--------------------------     -----|--------------------------
    TTFF | P1 = rollDict(rest2, P)       TTFF | P2 = rollDict(rest1, P)

    if !x1 && !x2 && !x3 && !x4 { P1 = P2 }
    if x2 && !x3 && (!x1 || x4) { P1 = rollDict(rest2, P2) }
    if x1 && !x4 && (!x2 || x3) { P2 = rollDict(rest1, P1) }
    if x1 && x2 && !x3 && !x4   { P1 = rollDict(rest2, P), P2 = rollDict(rest1, P) }

## Syntax

SWI also requires dicts to have a tag, that doesn't need to be always an atom like compound terms.
This presumably aids in giving more context about how it should be used, prevents unifying different
types, and also disambiguates the already existing braces for DCGs.

    point2D{x:10, y:20} = point3D{x:X, y:Y, z:Z}  % Tags don't match

    dicts --> {a:1}.                              % Is this a piece of code or a dict?

