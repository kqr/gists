% Erlang

powerset([]) -> [[]];
powerset([Elem|Remaining]) ->
  Subpowersets = powerset(Remaining),
  map(fun(Subpowerset) -> [Elem|Subpowerset] end, Subpowersets) ++ Subpowersets.