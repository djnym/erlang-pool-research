-module (pt_util).

-export ([work/2]).

work (N, _Data) ->
  timer:sleep (N),
  {worked, N}.
