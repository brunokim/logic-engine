## Packages

Prolog systems provide modules to organize code, avoiding namespace clashes
and easing dependency tracking. Example:

    :- module(mymod, [pred/2, parse_stuff//1, op(600, yfx, <~>)]).

    :- use_module(library(lists)).
    :- use_module('/home/prolog/lib/recur').
    :- use_module(library(xml), [tree//1, node/2 as xml_node]).

    % Defines pred/2, parse_stuff//1 and op(<~>)

Golang has a much simpler approach, where packages are specified by URL, relative
to a Go root. Only the module name can be rewritten; function names may be aliased
externally to the import stanza. A package exports every var, type and function
that starts with a capital letter. Vars need to be referenced suffixed by the package
name, and may also be aliased.

    package mypack

    import (
        "lists"
        "xml"

        "/home/go/lib/recur"
    )

    var (
        tree = xml.Tree
        xml_node = xml.Node
    )

    ... recur.Call() ...

## Why?

Working with attributed variables requires creating hooking predicates
`check_attribute` and `join_attribute` for each attribute. Predicates created for
one attribute should not conflict with another.

With no namespace, this can be accomplished by requiring that each one carries a 
"tag" for their attribute in the signature, that ideally also allows being indexed
separately. It's also necessary to ensure that compiling one won't shadow the other.
This part is not implemented, requiring that all code be compiled at once, including
libraries that could be pre-compiled.

One could solve this by allowing incremental compilation, but this introduces complexity
into indexing, that currently relies on having all predicates with the same functor
available during compilation. Also, libraries should be compiled in the same deterministic
order, to ensure that their order of execution is always the same.

The alternative is doing what other Prolog systems do: require that these functions
are implemented within a module. The module itself hosts the hook function (`verify_attributes`
in SICSTus, `attr_unify_hook` in SWI-Prolog), and calls to `put_attr` and `get_attr`
specify the module that must be called when they participate in a unification.

## Design

I don't yet support headless clauses like ':- module()', so I'll use a regular fact.
To avoid reusing a term that is already in use by Prolog developers, I'll call them packages,
as in Go.

    package(mypkg,
        [lists, dif],                    % imports
        ['pred/2', 'node/1', 'node/2']). % exports

Imported packages are looked up in `<name>.pl` files in the library directory.
All exported predicates from imported packages are placed in the current package, with
no renaming.
   
    // Global variable
    var Packages = map[string]*Package{}

    type Package struct {
        Name string
        Imported map[Functor]*Clause
        Exported map[Functor]*Clause
        Internal map[Functor]*Clause
    }

    type Clause struct {
        Package *Package
        Functor Functor
        NumRegisters int
        Code []Instruction
    }

Call clause lookup is performed first in Internal, then in Exported, then in Imported.
This allows a package to shadow an imported predicate.
