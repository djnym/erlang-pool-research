init ([_MinPool, MaxPool]) ->
  { ok,
    { {one_for_one, 10, 10},
      [ { ?POOL_ID,
          {poolboy, start_link,
           [ [{name, {local, ?POOL_ID}},
              {worker_module, pt_baseline_worker},
              {size, MaxPool}, {max_overflow, 0} ],
             [] ]},
          permanent, 5000, worker, [poolboy] } ] } }.

do (N, Data) ->
  case poolboy:checkout (?POOL_ID, false) of
    full -> {error, busy};
    Worker ->
      Res = pt_baseline_worker:do (Worker, N, Data),
      poolboy:checkin (?POOL_ID, Worker), Res
  end.
