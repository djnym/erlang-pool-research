-module (pt_overhead).


-export ([do/2]).

do (N, Data) ->
  {ok, pt_util:work (N, Data)}.
