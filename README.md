# logic-engine

Engine for logic programming hosted in Go.

## Why

Logic programming is a paradigm well suited to solving combinatory and constrained problems, where we
can express a set of relationships and query the solver for a solution that satisfies all constraints
simultaneously.

This library was born as a simple unification solver that eventually grew too big and warranted its own
project. It is not intended as a standalone language, but rather a hosted one that is able to manipulate
Go objects and call Go code. As such, it may be used as an engine instead of shelling out to a
full Prolog environment, which necessarily involve some ser-/deserialization.

## Features

- [x] Last-call optimization
- [x] Dicts
- [x] Prolog parser
- [ ] DCGs
- [X] Attributed variables
- [X] Packages
- [ ] Go integration
- [ ] Delimited continuations
- [ ] Tabling
- [ ] Finite-domain constraint solver

Lots of work ahead :)

## About

The execution environment is an interpreter of the Warren Abstract Machine (WAM), that use regular Go pointers
instead of managing memory regions directly (heap, stack, choicepoints). This way, we get garbage collection
for free and simplify many other concepts -- for example, we keep separate stacks for environments and
choicepoints, and environments that are still referenced won't get lost after they return. We also don't need
instructions like `put_unsafe_value`, that promote a value from the register to the stack, since there's no
heap region.

To learn more about the WAM, see [Aït-Kaci's tutorial](https://direct.mit.edu/books/book/4253/Warren-s-Abstract-MachineA-Tutorial-Reconstruction).
