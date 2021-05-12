## Unicode iterators

# What?

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

# Why?

We could just throw an error like 'not sufficiently instantiated', but I think it's more
fun (and feasible) not to. This preserves purity and may lead to interesting results, like
enumerating all atoms or numbers below a given length, for example.

# How?

The current solution doesn't work. We build the list with all atoms of a given category and
create a closure to hold it statefully, yielding the next one for each call. However, we rely on
replacing the actual *clause* with this code, and never replace it back! Thus, we modify global
state with baaad results.

We need a solution that makes any backtracking attempt reach the same piece of code, with a
stateful closure, but any new attempt must encounter the same pristine version.
