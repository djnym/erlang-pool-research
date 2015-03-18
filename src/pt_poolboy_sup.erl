-module (pt_poolboy_sup).

%% API
-export ([start_link/2, do/2]).

%% supervisor callbacks
-export ([init/1]).

-define (POOL_ID, pt_poolboy_pool).

start_link (MinPool, MaxPool) ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, [MinPool, MaxPool]).

do (N, Data) ->
  poolboy:transaction(?POOL_ID,
    fun (Worker) ->
      pt_baseline_worker:do (Worker, N, Data)
    end).

init ([MinPool, MaxPool]) ->
  { ok,
    { {one_for_one, 10, 10},
      [ { ?POOL_ID,
          {poolboy, start_link, [[{name, {local, ?POOL_ID}},
                                  {worker_module, pt_baseline_worker},
                                  {size, MinPool},  % min_size?
                                  {max_overflow, MaxPool}
                                 ],
                                 [] % args for gen_server:init
                               ]},
          permanent,
          5000,
          worker,
          [poolboy]
        }
      ]
    }
  }.
