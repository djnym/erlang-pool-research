-module (pt_poolboy_sup).

%% API
-export ([start_link/2, do/2, superconfig/2]).

%% supervisor callbacks
-export ([init/1]).

-define (POOL_ID, pt_poolboy_pool).

superconfig (MinPool, MaxPool) ->
  [ { pt_poolboy_sup,
      {pt_poolboy_sup, start_link, [MinPool, MaxPool]},
      permanent,
      2000,
      supervisor,
      [pt_poolboy_sup]
    }
  ].

start_link (MinPool, MaxPool) ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, [MinPool, MaxPool]).

do (N, Data) ->
  case poolboy:checkout (?POOL_ID, false) of
    full -> {error, busy};
    Worker ->
      Res = pt_baseline_worker:do (Worker, N, Data),
      poolboy:checkin (?POOL_ID, Worker),
      Res
  end.

init ([_MinPool, MaxPool]) ->
  { ok,
    { {one_for_one, 10, 10},
      [ { ?POOL_ID,
          {poolboy, start_link, [[{name, {local, ?POOL_ID}},
                                  {worker_module, pt_baseline_worker},
                                  {size, MaxPool},
                                  {max_overflow, 0} % fixed size
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
