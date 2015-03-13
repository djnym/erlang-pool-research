-module (pt_leo_pod_sup).

-export ([ start_link/0, do/2 ]).

-define (POOL_ID, pt_leo_pod_pool).
-define (POOL_SIZE, 100).

start_link () ->
  MaxOverflow = 10,
  GenServerArgs = [],
  leo_pod:start_link (?POOL_ID, ?POOL_SIZE,
                      MaxOverflow, pt_baseline_worker,
                      GenServerArgs, fun (_) -> void end).

do (N, Data) ->
  case leo_pod:checkout (?POOL_ID) of
    {ok, P} ->
      Res = pt_baseline_worker:do (P, N, Data),
      ok = leo_pod:checkin (?POOL_ID, P),
      Res;
    E -> E
  end.
