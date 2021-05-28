# Optimizations

## Simplify bindings

`unifyStep` collects all bindings that are executed, so they may be undone later if
there are attributes to verify first. This was done with a map, but is feasible with
a slice instead.

Memory: 78.51 MiB -> 25.50 MiB
CPU (mallocgc contribution): 0.10 s -> 0.09 s
