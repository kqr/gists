% GNU Prolog

powerset([], [[]]).
powerset([X|Xs], Solution) :-
  powerset(Xs, Subpowersets),
  foreachprepend(X, Subpowersets, [], Partial),
  append(Partial, Subpowersets, Solution).


foreachprepend(_, [], Reverse, Result) :-
  reverse(Reverse, Result).
foreachprepend(Elem, [X|Xs], Partial, Result) :-
  foreachprepend(Elem, Xs, [[Elem|X]|Partial], Result).