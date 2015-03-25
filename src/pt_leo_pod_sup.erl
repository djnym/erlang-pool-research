-module (pt_leo_pod_sup).

-export ([start_link/2, do/2, superconfig/2]).

-define (POOL_ID, pt_leo_pod_pool).

superconfig (MinPool, MaxPool) ->
  [ { pt_leo_pod_sup,
      {pt_leo_pod_sup, start_link, [MinPool, MaxPool]},
      permanent,
      2000,
      worker,
      [pt_leo_pod_sup]
    }
  ].

start_link (MinPool, MaxPool) ->
  GenServerArgs = [],
  leo_pod:start_link (?POOL_ID, MinPool,
                      MaxPool, pt_baseline_worker,
                      GenServerArgs, fun (_) -> void end).

do (N, Data) ->
  case leo_pod:checkout (?POOL_ID) of
    {ok, P} ->
      Res = pt_baseline_worker:do (P, N, Data),
      ok = leo_pod:checkin (?POOL_ID, P),
      Res;
    E -> E
  end.
