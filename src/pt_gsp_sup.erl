-module (pt_gsp_sup).

%% API
-export ([ start_link/0, do/2 ]).

start_link () ->
  PoolOptions =
    [ % minimum number of resources
      { min_pool_size, 0 },
      % maximum number of resources
      { max_pool_size, 100 },
      % idle timeout in seconds
      { idle_timeout, 60 },
      % max age in seconds
      { max_worker_age, 60 },
      % max number to queue
      { max_queue, infinity },
      % emit stats
      { mondemand, false }
    ],
  GenServerOptions = [],
  gen_server_pool:start_link ({local, get_id()}, pt_baseline_worker,
                              GenServerOptions, [], PoolOptions).

get_id () ->
  pt_gsp_worker_pool.

do (N, Data) ->
  pt_baseline_worker:do (get_id(), N, Data).
