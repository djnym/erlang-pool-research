start_link (MinPool, MaxPool) ->
  PoolOptions =
    [ { min_pool_size, MinPool },
      { max_pool_size, MaxPool },
      { idle_timeout, 60 }, % seconds
      { max_worker_age, 60 }, % seconds
      { max_queue, infinity },
      { mondemand, false } ],
  GenServerOptions = [],
  gen_server_pool:start_link ({local, ?POOL_ID}, pt_baseline_worker,
                              GenServerOptions, [], PoolOptions).

do (N, Data) ->
  case pt_baseline_worker:do (?POOL_ID, N, Data) of
    {error, request_dropped} -> {error, busy};
    R -> R
  end.
