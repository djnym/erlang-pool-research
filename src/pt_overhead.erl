-module (pt_overhead).

-export ([do/2, superconfig/2]).

superconfig (_,_) -> [].

do (N, Data) ->
  {ok, pt_util:work (N, Data)}.
