%% dif/2
% This module was adapted from the SWI-Prolog implementation of dif/2.
%
%    Author:        Tom Schrijvers, Markus Triska and Jan Wielemaker
%    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
%    WWW:           http://www.swi-prolog.org
%    Copyright (c)  2004-2016, K.U.Leuven
%    All rights reserved.
%
%    Redistribution and use in source and binary forms, with or without
%    modification, are permitted provided that the following conditions
%    are met:
%    1. Redistributions of source code must retain the above copyright
%       notice, this list of conditions and the following disclaimer.
%    2. Redistributions in binary form must reproduce the above copyright
%       notice, this list of conditions and the following disclaimer in
%       the documentation and/or other materials provided with the
%       distribution.
%    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%    POSSIBILITY OF SUCH DAMAGE.

% dif(A, B) adds attributes to any unbound variables within both terms, to
% ensure that they are not unified with a value that would make them equal.
%
% For example, 'f(X, g(Y)) = f(g(a), X)' would succeed with {X=g(Y), Y=a}.
% If we want 'dif(f(X, g(Y)), f(g(a), X))', then we must ensure that
% *either* X =\= g(a) *or* Y =\= a.
%
% - If at any time we have 'Y=b', then the difference succeeds and we can
%   remove all attributes.
% - If, on the other hand, we have 'Y=a', then we must update the constraints
%   to show that the only one left is 'X =\= g(a)'.
% - If all constraints are removed from a variable, than unification must
%   fail.
%
% The substitution list is called an 'or node', due to each pair representing
% an 'or' case that, if satisfied, would satisfy the whole difference. A variable
% may be constrained in multiple dif/2 calls, so it keeps a list of ornodes,
% associated to the forbidden value.
%
%     dif(f(X, g(Y)), f(g(a), X)),  % X=g(Y), Y=a
%     dif(p(X, q(Z)), p(q(b), X)),  % X=q(Z), Z=b
%     dif(W, Y).                    % W=Y
%
%     X:   [edge(Or1, g(Y)), edge(Or2, q(Z))]
%     Y:   [edge(Or1, a), edge(Or3, W)]
%     Z:   [edge(Or2, b)]
%     W:   [edge(Or3, Y)]
%     Or1: [X:g(Y), Y:a]
%     Or2: [X:q(Z), Z:b]
%     Or3: [W:Y]
%
% We keep the Or node information as a `dif_node([...])` attribute instead of
% simply binding to it, so that it is mutable. We bind 'success' to the variable
% when one of its substitutions become definitely different, and remove it from
% each referencing variable edge list.
%
% For some reason that's not specially clear from SWI-Prolog's implementation,
% the edge list is kept as two separate lists: one with incoming (left) relations,
% and another with outgoing (right) relations. For the `A:B` substitution, the Or
% node for A will be stored in the left list, and the node for B in the right
% list. The stored attribute is `dif_edges(Incoming, Outgoing)`.
%
% The restrictions in the case above are written as:
%
%     put_attr(dif, X, dif_edges([edge(Or1, g(Y)), edge(Or2, q(Z))], [])),
%     put_attr(dif, Y, dif_edges([edge(Or1, a)], [edge(Or3, W)])),
%     put_attr(dif, Z, dif_edges([edge(Or2, b)], [])),
%     put_attr(dif, W, dif_edges([edge(Or3, Y)], [])),
%     put_attr(dif, Or1, dif_node([X:g(Y), Y:a])),
%     put_attr(dif, Or2, dif_node([X:q(Z), Z:b])),
%     put_attr(dif, Or3, dif_node([W:Y])).
package(dif, [lists], ['dif/2']).

dif(X, Y) :-
    \==(X, Y),
    dif_compounds(X, Y, _).

% TODO: there's a bug when allocating vars to registers when using ->/3, and/n
% or other control predicates, because there is a single functor and all variables
% are then considered temporary.
dif_compounds(X, Y, Or) :- unifiable(X, Y, Unifier), !, dif_comp2(Unifier, Or).
% X is different from Y, succeed Or node.
dif_compounds(_, _, Or) :- or_succeed(Or).
% X=Y, remove this association from Or node.
dif_comp2(Unifier, Or) :- ==(Unifier, []), !, or_one_fail(Or).
% X \= Y, add relations to X, Y and Or.
dif_comp2(Unifier, Or) :- append_edges(Unifier, Or).

append_edges(Unifier, Or) :-
    ->(get_attr(dif, Or, dif_node(L2)),
        true,
        =(L2, [])),
    put_attr(dif, Or, dif_node(L1)),
    add_edges(Unifier, Or, L1, L2).

add_edges([], _, L, L).
add_edges([:X Y|Unifier], Or, [:X Y|L1], L2) :-
    add_edge(X, Y, Or),
    add_edges(Unifier, Or, L1, L2).

get_dif_edges(X, Incoming, Outgoing) :-
    ->(get_attr(dif, X, dif_edges(Incoming, Outgoing)),
        true,
        and(=(Incoming, []), =(Outgoing, []))).

add_edge(X, Y, Or) :-
    add_incoming_edge(X, Y, Or),
    ->(var(Y),
        add_outgoing_edge(X, Y, Or),
        true).

add_incoming_edge(X, Y, Or) :-
    get_dif_edges(X, Inc, Out),
    put_attr(dif, X, dif_edges([edge(Or, Y)|Inc], Out)).

add_outgoing_edge(X, Y, Or) :-
    get_dif_edges(Y, Inc, Out),
    put_attr(dif, Y, dif_edges(Inc, [edge(Or, X)|Out])).

%%%%

% Some association within Or is definitely different, bind Or node
% and remove it from all referencing attributed vars.
or_succeed(Or) :-
    get_attr(dif, Or, dif_node(Unifier)),
    del_attr(dif, Or),
    =(Or, success),
    del_or_dif(Unifier).

% Cleanup all vars associated to an Or node.
del_or_dif([]).
del_or_dif([:X Y|Unifier]) :-
    cleanup_dead_nodes(X),
    cleanup_dead_nodes(Y),
    del_or_dif(Unifier).

% Remove bound Or nodes from X's incoming and outgoing edges.
% If there are none left, remove dif attribute from X.
cleanup_dead_nodes(X) :-
    get_dif_edges(X, Inc, Out),
    filter_dead_ors(Inc, NewInc),
    filter_dead_ors(Out, NewOut),
    ->(and(==(NewInc, []), ==(NewOut, [])),
        del_attr(dif, X),
        put_attr(dif, X, dif_edges(NewInc, NewOut))).

% Remove edges associated to bound Or nodes.
filter_dead_ors([], []).
filter_dead_ors([edge(Or, Y)|Rest], Filtered) :-
    ->(==(Or, success),
        =(Filtered, Rest),
        =(Filtered, [edge(Or, Y)|NRest])),
    filter_dead_ors(Rest, NRest).

%%%%

% One association within this or-node unifies (is not different).
% Filter non-different assocs from node. If there are none left, fail.
or_one_fail(Or) :-
    get_attr(dif, Or, dif_node(L1)),
    filter_assocs(L1, L2),
    \==(L2, []),
    put_attr(dif, Or, dif_node(L2)).

filter_assocs([], []).
filter_assocs([:X Y|L1], L2) :-
    ->(==(X, Y),
        =(L2, L1),
        =(L2, [:X Y|L3])),
    filter_assocs(L1, L3).

or_one_fails([]).
or_one_fails([Or|Ors]) :-
    or_one_fail(Or),
    or_one_fails(Ors).

%%%% Join dif edges

join_attribute(dif_edges, X, Y) :-
    get_dif_edges(X, IncX, OutX),
    get_dif_edges(Y, IncY, OutY),
    update_dif_attrs(IncX, OutX, Y, NewIncX, NewOutX),
    update_dif_attrs(IncY, OutY, X, NewIncY, NewOutY),
    append(NewIncX, NewIncY, JoinedInc),
    append(NewOutX, NewOutY, JoinedOut),
    ->(and(==(JoinedInc, []), ==(JoinedOut, [])),
        del_attr(dif, Y),
        put_attr(dif, Y, dif_edges(JoinedInc, JoinedOut))).

update_dif_attrs(Inc, Out, Value, NewInc, NewOut) :-
    reverse_lookups(Inc, Value, Ors, NewInc),
    or_one_fails(Ors),
    reverse_lookups(Out, Value, _, NewOut).

% reverse_lookups(+Edges, +Value, -Ors, -NewEdges)
% Search for edges that are associated with Value. The Or node
% associated to them is stored in Ors, and all other edges in NewEdges.
reverse_lookups([], _, [], []).
reverse_lookups([edge(Or, X)|Edges], Y, Ors, Rest) :-
    ->(==(X, Y),
        and(=(Ors, [Or|ROrs]), =(Rest, RRest)),
        and(=(Ors, ROrs), =(Rest, [edge(Or, X)|RRest]))),
    reverse_lookups(Edges, Value, ROrs, RRest).

%%%% Check that value is different from all constraints.

check_attribute(dif_edges(Inc, Out), Value) :-
    verify_compounds(Inc, Value),
    verify_compounds(Out, Value).

verify_compounds([], _).
verify_compounds([edge(Or, Y)|Rest], Value) :-
    ->(var(Y),
        true,
        ->(==(Or, success),
            true,
            dif_compounds(Value, Y, Or))),
    verify_compounds(Rest, Value).

