-module (pt_poolboy_sup).

%% API
-export ([ start_link/0, do/2 ]).

%% supervisor callbacks
-export ([init/1]).

-define (POOL_ID, pt_poolboy_pool).

start_link () ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

do (N, Data) ->
  poolboy:transaction(?POOL_ID,
    fun (Worker) ->
      pt_baseline_worker:do (Worker, N, Data)
    end).

init ([]) ->
  PoolSize=100,
  { ok,
    { {one_for_one, 10, 10},
      [ { ?POOL_ID,
          {poolboy, start_link, [[{name, {local, ?POOL_ID}},
                                  {worker_module, pt_baseline_worker},
                                  {size, PoolSize},
                                  {max_overflow, 10}],
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
