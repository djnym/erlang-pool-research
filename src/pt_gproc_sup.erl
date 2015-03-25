-module (pt_gproc_sup).

-behaviour (supervisor).

%% API
-export ([start_link/2, do/2, superconfig/2]).

%% supervisor callbacks
-export ([init/1]).

-define (POOL_ID, pt_gproc_pool).

superconfig (MinPool, MaxPool) ->
  [ { gproc,
      {gproc_sup, start_link, [[]]},
      permanent,
      2000,
      supervisor,
      [gproc]
    },
    { pt_gproc_sup,
      {pt_gproc_sup, start_link, [MinPool, MaxPool]},
      permanent,
      2000,
      supervisor,
      [pt_gproc_sup]
    } ].

start_link (MinPool, MaxPool) ->
  supervisor:start_link ({local, ?POOL_ID}, ?MODULE, [MinPool, MaxPool]).

do (N, Data) ->
  case
    gproc_pool:claim (?POOL_ID,
                      fun (_,Pid) ->
                        pt_gproc_worker:do (Pid, N, Data)
                      end) of
      {true, Res} -> Res;
      false -> {error, busy}
  end.

%%====================================================================
%% supervisor callbacks
%%====================================================================
init ([_MinPool, MaxPool]) ->
  ok = gproc_pool:new (?POOL_ID, claim, []),
  { ok,
    { {one_for_one, 10, 10},
      [ begin
          WorkerName = {?POOL_ID, N},
          gproc_pool:add_worker (?POOL_ID, WorkerName),
          { WorkerName,
            {pt_gproc_worker, start_link, [?POOL_ID, WorkerName]},
            transient, 2000, worker, [pt_gproc_worker] }
        end
        || N <- lists:seq (1, MaxPool)
      ]
    }
  }.
