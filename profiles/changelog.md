# Optimizations

## Simplify bindings

`unifyStep` collects all bindings that are executed, so they may be undone later if
there are attributes to verify first. This was done with a map, but is feasible with
a slice instead.

Memory: 78.51 MiB -> 25.50 MiB
CPU (mallocgc contribution): 0.10 s -> 0.09 s

## Lazy AttrTrail

`newChoicePoint` consumes a lot of memory with choicepoints. One of its fields is the
`AttrTrail` map, which is only used when there are attribute changes. This map was
initialized for every choicepoint, but now it is lazily initialized on first write.

Memory: 123.51 MiB -> 95.01 MiB
CPU: 0.17 s -> 0.07 s
