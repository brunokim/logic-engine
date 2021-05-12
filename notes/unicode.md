# Unicode iterators

## What?

Unicode currently contains over 143 thousand codepoints, and we want to support them all.
Go offers access to codepoint categories with a library, so I want to bind a builtin
instruction into it.

A predicate like `unicode_digit(Ch)` is simple if `Ch` is bound, say, to `'1'` or `a` --
it's only necessary to lookup the character into the digits table and return whether it's
in there. This changes if we try to use an unbound `Ch`, in which case we would need to
enumerate all characters in that category.

In other words, we want the semantics as if `unicode_digit` was defined as below, but
without having to pollute the database with all these facts.

```
unicode_digit('0').
unicode_digit('1').
unicode_digit('2').
unicode_digit('3').
unicode_digit('4').
unicode_digit('5').
unicode_digit('6').
unicode_digit('7').
unicode_digit('8').
unicode_digit('9').
unicode_digit('٠').
unicode_digit('١').
unicode_digit('٢').
...
```

## Why?

We could just throw an error like 'not sufficiently instantiated', but I think it's more
fun (and feasible) not to. This preserves purity and may lead to interesting results, like
enumerating all atoms or numbers below a given length, for example.

## How?

The current solution doesn't work. We build the list with all atoms of a given category and
create a closure to hold it statefully, yielding the next one for each call. However, we rely on
replacing the actual *clause* with this code, and never replace it back! Thus, we modify global
state with baaad results.

We need a solution that makes any backtracking attempt reach the same piece of code, with a
stateful closure, but any new attempt must encounter the same pristine version.

### What if...

...we actually implemented it like the fact list above?

    %unicode_digit/1 #1
    switch_on_term:
      if_var: instr{unicode_digit#2, 0}
      if_const: instr{unicode_digit#1, 1}
    switch_on_const:
      '0': unicode_digit#2
      '1': unicode_digit#3
      '2': unicode_digit#4
      '3': unicode_digit#5
      '4': unicode_digit#6
      '5': unicode_digit#7
      ...
      '９': unicode_digit#741

    %unicode_digit/1 #2
    try_me_else instr{unicode_digit#3, 0}
    get_constant '0', X0
    proceed

    %unicode_digit/1 #3
    retry_me_else instr{unicode_digit#4, 0}
    get_constant '1', X0
    proceed

    %unicode_digit/1 #4
    retry_me_else instr{unicode_digit#5, 0}
    get_constant '2', X0
    proceed

    ...

    %unicode_digit/1 #741
    trust_me
    get_constant '９', X0
    proceed

This implementation relies on a static linked list of clauses, and stores the position
of the 'next' clause in the `Machine.ChoicePoint.Alternative` field. We may be able to
reuse the backtracking mechanism by storing within the choicepoint the index of the
list we're at. This may be doable by manipulating the Args field.

    unicode_digit(Ch) :-
        ( var(Ch) ->
            unicode_digit_ref(Ch, 0)
        ;   unicode_digit_check(Ch)
        ).

    unicode_digit_ref(Ch, Pos) :-
        asm(try_me_else(ptr(self), 0)),
        asm(buitin("unicode_digit_ref", ptr(unicodeDigitRef), list())).

    func unicodeDigitRef(m *Machine, args []Addr) error {
        x, pos := deref(args[0]).(*Ref), deref(args[1]).(Int)
        ch, err := unicodeChar(digitTable, pos.Value)
        if err != nil {
            return err
        }
        pos.Value++
        m.ChoicePoint.Args[1] = pos // Mutate the choicepoint arg for next call.
        m.bind(x, ch) // TODO: handle attributed vars
        return nil
    }
