package(lists, [], [
    'member/2', 'append/3', 'append/2', 'same_length/2',
]).

% member(?Elem, ?List)
% Succeeds if Elem is a member of List.
member(E, [H|T]) :-
    member_(T, E, H).

member_(_, E, E).
member_([H|T], E, _) :-
    member_(T, E, H).

% append(?L1, ?L2, ?L1+L2)
% L1+L2 is the concatenation of L1 and L2.
append(L1, L2, L3) :- '$append'(L1, L2, L3).

% append(+ListOfLists, ?List)
% List is the concatenation of lists in ListOfLists.
append([], []).
append([Xs|Xss], Zs) :-
    append(Xs, Ys, Zs),
    append(Xss, Ys).

% same_length(?L1, ?L2)
% Succeeds if both are lists with the same length.
same_length([], []).
same_length([_|T1], [_|T2]) :-
    same_length(T1, T2).
