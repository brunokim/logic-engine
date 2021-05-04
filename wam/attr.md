# Attributed variables

Attributed variables are a way to extend unification. Variables may contain
attributes with associated behavior that is checked before unification, and that
may in turn modify the final value the variable is bound to.

This feature is a building block for many others, like `dif/2` and `freeze/2`, that
provide more pure constructs for the language. Also, constraint solvers use attributed
variables extensively.

[Markus Triska](https://www.metalevel.at/prolog/attributedvariables) provides the following
basic operations on attributed variables:

- `put_atts(X, +(Attribute))`: add or replace the provided attribute to X;
- `put_atts(X, -(Attribute))`: remove the provided attribute from X;
- `get_atts(X, +(Attribute))`: get the associate attribute from X;
- `verify_attributes(X, Value, Goals)`: succeeds if X may unify with Value. Execute Goals after X is bound.

These predicates should be called namespaced to a module, and each attribute is associated
to a module. Since we lack modules, we'll need to find another way to key an attribute to
its behavior.

## Example

    % Implementation for 'domain' attribute
    verify_attributes(domain, X, Value, []) :-
        if(get_atts(X, domain(Dom1)),
            if(get_atts(Value, domain(Dom2)),
                % If both X and Value are vars with domain() attribute.
                % Check that their intersection is not empty.
                intersection(Dom1, Dom2, Dom),
                dif(Dom, []),
                if(Dom = [Val],
                    % If intersection is exactly equal to Val, bind Value to it.
                    % Otherwise, associate this domain to Value.
                    Value = Val,
                    put_atts(Value, domain(Dom))),
                % Otherwise, check that Value is part of X's domain.
                member(Dom1, Value)),
            true).
   
    % Overlapping domains: X and Y are still unbound, but with associated attributes.
    ?- put_atts(X, domain([1, 2, 3])),
        put_atts(Y, domain([2, 3, 4])),
        X = Y.
    X = Y, put_atts(Y, domain([2, 3])).
    
    % |intersection| = 1: X and Y are bound to the unique value in intersection.
    ?- put_atts(X, domain([1, 2, 3])),
        put_atts(Y, domain([3, 4, 5])),
        X = Y.
    X = Y, Y = 3.

## Hosted feature

One of the main goals of this library is providing deep integration with the runtime.
Attributed variables are valuable not only to extend unification, but to simply track
pieces of data external to the engine throughout the solution. I've used this to
implement a type checker where pointers to the AST nodes were associated to their
type variables. Once the solution is provided, it's trivial to iterate over the
variables and populate the nodes, without any kind of indexing or traversal.

The interface for attributes accessible to the runtime splits `verify_attributes` in
two operations: *join* for when X is bound to a Value that has the same attribute,
that should return the new joint attribute OR a term; and *check* for when Value is
not a var, that returns the new attribute of X.

    type Attribute interface {
        Join(other Attribute) (Attribute, Term, error)
        Check(x Var, nonvar Term) (Attribute, error)
    }

The domain example can be implemented natively as

    type map[int]struct{} domain

    func (d1 domain) Join(other Attribute) (Attribute, Term, error) {
        d2 := other.(domain)
        intersection := make(domain)
        for i := range d1 {
            if _, ok := d2[i]; ok {
                intersection[i] = struct{}{}
            }
        }
        if len(intersection) == 0 {
            return nil, nil, fmt.Errorf("empty intersection")
        }
        if len(intersection) == 1 {
            for i := range intersection {
                return nil, Int{i}, nil
            }
        }
        return intersection, nil, nil
    }

    func (d domain) Check(x Var, nonvar Term) (Attribute, error) {
        i, ok := nonvar.(Int)
        if !ok {
            return nil, fmt.Errorf("not an int")
        }
        if _, ok := d[i.Value]; !ok {
            return nil, fmt.Errorf("out of range")
        }
        return d, nil
    }

The implementation of a similar interface in the Prolog side can be done by trusting
the first arg indexing and implementing `join_attribute/3` and `check_attribute/3`:

    join_attribute(domain, X, Y) :-
        get_atts(X, domain(Dom1)),
        get_atts(Y, domain(Dom2)),
        intersection(Dom1, Dom2, Dom),
        Dom \= [],
        if(Dom = [Value],
            Y = Value,                 % <<<NOTE
            put_atts(Y, domain(Dom))).
        
    check_attribute(domain(Dom), Value, domain(Dom)) :-
        member(Dom, Value).

Note that we need to be smart not to call `check_attribute` for Y where indicated,
since I fear this may engage in a recursion of join/check/join.

## WAM representation

Any Ref may contain an attribute, so we will add a new field to \*Ref. This is
probably a waste of space, since most instances will have this empty; we may
revisit this decision.

    type Ref struct {
        // Cell contains the ref's value once it's bound.
        Cell Cell
        // Attrs may contain Struct or NativeAttr cells.
        Attrs map[string]Cell
    }

    // NativeAttr wraps a Golang-native Attribute.
    type NativeAttr struct {
        Name string
        Ptr Attribute
    }

    func (AttrRef) isCell() {}
    func (NativeAttr) isCell() {}

    // PutAttr instruction: put_attr X0, Y1
    type PutAttr struct{
        // Addr must point to an unbound Ref
        Addr Addr
        // Attr must contain a Struct or NativeAttr cell.
        Attr Addr
    }

    // GetAttr instruction: get_attr X0, Y1
    type GetAttr struct{
        // Addr must point to an unbound Ref
        Addr Addr
        // Attr must contain an unifiable cell to store the attribute.
        Attr Addr
    }

Another approach that minimizes memory waste is having a map in Machine
from ref to attribute list. One problem of this approach is that the map would
keep a strong reference even if there's nobody else pointing to it. Golang still
doesn't have [weak maps](https://github.com/golang/go/issues/43615) or weak references.
It does have finalizers, but they seem non-kosher?

Solution: we will use the ref ID, which is unique per choicepoint branch.

    type Machine struct {
        ...

        // List of attributes per ref ID.
        attributes map[int]map[string]Cell
    }

    func (m *Machine) putAttribute(ref *Ref, attr Cell) {
        attrs, ok := m.attributes[ref.id]
        if !ok {
            attrs = make(map[string]Cell)
            m.attributes[ref.id] = attrs
        }
        name := attrName(attr)
        attrs[name] = attr
    }

This brings another question: how to undo them on backtrack? We shouldn't undo
all associations, only those that happened since the latest choicepoint. Also, we
need to restore any association that was present before that choicepoint. That is,
we need to keep a trail of attribute changes.

    type ChoicePoint struct {
        AttrTrail map[*Ref]map[string]Cell
    }

    func (m *Machine) trailAttribute(ref *Ref, attr Cell) {
        trail, ok := m.ChoicePoint.AttrTrail[ref]
        if !ok {
            trail = make(map[string]Cell)
        }
        name := attrName(attr)
        if _, ok := trail[name]; ok {
            // Don't overwrite attribute already recorded.
            return
        }
        var attr Cell
        if attrs, ok := m.attributes[ref.id]; ok {
            attr = attrs[name]
        }
        trail[name] = attr
    }

    func (m *Machine) unwindTrail() {
        ...
        for _, ref := m.ChoicePoint.Trail {
            ref.Cell = nil
        }
        for ref, changes := range m.ChoicePoint.AttrTrail {
            for _, change := range changes {
                attrs := m.attributes[ref.id]
                if change.Attr == nil {
                    delete(attrs, change.Name)
                } else {
                    attrs[change.Name] = change.Attr
                }
                if len(attrs) == 0 {
                    delete(m.attributes, ref.id)
                }
            }
        }
    }

## Suspending unification/binding

Since we may need to execute arbitrary Prolog code prior to a binding, some operations
that seemed to be atomic are not anymore.

- Binding structs. While we could do it all args within a single instruction, we can't
  anymore because any arg may bind an attributed var and require suspension. This was
  solved by going back to the original WAM behavior, to keep in the machine state the
  current struct being worked on, and in which arg we are operating.

- Unification itself. Unifying any two terms may require arbitrary unifications if they
  are complex terms. We need to be able to suspend an unification, run another code (that
  most likely will also require another unification), and then return to it.

Ah, and we want that without using Golang's call stack, managing the machine stack
ourselves.

### Attempt 1: make unification a function.

The first approach implemented a `$unify` function that operates over a stack of term
pairs, represented as a difference list:

    $unify([X:Y|S1], S3) :-
        $unify_step(X, Y, S1, S2),
        unify(S2, S3).
    $unify([], []).

`$unify_step` is an internal function that matches X and Y. If they are complex terms,
`$unify_step` pushes all matching pairs to S2. For example, if `X=f(A1,B1)` and
`Y=f(A2,B2)`, the state after is `S2=[A1:A2, B1:B2|S1]`.

This solution makes use of the Prolog call stack itself to keep context. However, it
also requires that the complex term unification state -- Mode, Complex, ArgIndex --
be saved at each call, or at least each one of $unify, as the function itself uses them.

This may be circumvented by letting the "extraction" part be handled by `$unify_step`:

    $unify(S1, S3) :-
        $unify_step(S1, S2), % Takes first assoc from S1
        unify(S2, S3).
    $unify([], []).

However, this function isn't indexed due to the variable in the first arg, and a
choicepoint is created for each call. We can add a cut, but this doesn't prevent a
backtrack, which also has the unintended consequence of resetting the unification mode.

### Attempt 2: manage suspending a stack of unifications.

Using the call stack for unification seems error-prone, and not to say inefficient,
in what is the most central operation of Prolog. It *would* allow for tracing all steps
in a complex unification, but this isn't a necessary debugging feature.

Instead, let's consider the state that needs to be kept once we start executing arbitrary
code within a unification.

- The unification stack of term pairs to be matched;
- The complex term unification state;
- The continuation, like a regular function call;

A complication is that an unification may bind several variables, and each variable may
have several attributes associated, where each one might fail. We need to process all of
them *before* binding variables.

    def unify(self, a, b):
        # Recursively walk structures, annotating bindings
        stack = [a, b]
        bindings = {}
        while stack:
            a, b, *stack = stack
            stack0, bindings0 = self.unify_step(a, b)
            stack.extend(stack0)
            bindings.update(bindings0)

        # Remove bindings
        for x in bindings:
            del(self.bindings, x)

        # Check bindings. TODO: think about traversal order.
        for x, value in bindings.items():
            attrs = self.attributes[x]
            if not attrs:
                continue
            if not is_var(value):
                new_attrs = []
                for attr in attrs:
                    #### Yield to runtime #1 ####
                    new_attrs.append(attr.check(value))
                self.attributes[x] = attrs
                continue

            # Handle case of ref-ref binding
            y = value
            attrs2 = self.attributes[y]
            common_keys = intersection(attrs, attrs2)
            for key in common_keys:
                #### Yield to runtime #2 ####
                self.join_attr(key, x, y)
            self.attributes[y].update(attrs - attrs2)

        # Set bindings
        for x, value in bindings:
            self.bindings[x] = value

The points marked as "yield to runtime" are the points where user-defined code
may be called, that require a complete runtime and stack (and potentially other
unifications). We need to store the full state at these points to resume later.

- Point #1, `attr.check`: curr x, curr value, remaining bindings, remaining attrs 
- Point #2, `join_attr`: curr x, curr value, remaining bindings, remaining keys

