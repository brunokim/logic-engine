# Optimizations

## Simplify bindings

`unifyStep` collects all bindings that are executed, so they may be undone later if
there are attributes to verify first. This was done with a map, but is feasible with
a slice instead.

Memory: 78.51 MiB -> 25.50 MiB
CPU (mallocgc contribution): 0.10s -> 0.09s
Total: 13.292s

## Lazy AttrTrail

`newChoicePoint` consumes a lot of memory with choicepoints. One of its fields is the
`AttrTrail` map, which is only used when there are attribute changes. This map was
initialized for every choicepoint, but now it is lazily initialized on first write.

Memory: 123.51 MiB -> 95.01 MiB
CPU: 0.17s -> 0.07s
Total: 13.184s

## Optimized instr

`InstrAddr.instr` is called at every loop, but it's calculating `pos()` twice: one
during `isValid` and another for `instr()`. By inlining the checks, we got some good
results on timing -- and not only on this function *shrug*.

CPU: 1.28s -> 0.79s
Total: 12.379s

## getClause cache

`Machine.getClause` performs several map accesses until it finds the correct clause.
Assuming that, once a clause is found it won't change due to importing (since new
packages are consulted last), we can use a cache to return the computation result
quickly.

CPU: 0.89s -> 0.64s
Total: 12.183s
