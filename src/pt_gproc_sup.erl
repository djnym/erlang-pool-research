-module (pt_gproc_sup).

-behaviour (supervisor).

%% API
-export ([start_link/2, do/2]).

%% supervisor callbacks
-export ([init/1]).

-define (POOL_ID, pt_gproc_pool).

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

% get_id () ->
%   gproc:where (gproc_pool:pick (?POOL_ID)).
% do (N, Data) ->
%   pt_gproc_worker:do (get_id (), N, Data).

%%====================================================================
%% supervisor callbacks
%%====================================================================
init ([_MinPool, MaxPool]) ->
  ok = gproc_pool:new (?POOL_ID, claim, []),
  { ok,
    { {one_for_one, 10, 10},
      [ begin
          WorkerName = {?POOL_ID, N},
          WorkerAtom = worker_atom (N),
          gproc_pool:add_worker (?POOL_ID, WorkerName),
          { WorkerAtom,
            {pt_gproc_worker, start_link, [?POOL_ID, WorkerAtom, WorkerName]},
            transient,
            2000,
            worker,
            [pt_gproc_worker]
          }
        end
        || N <- lists:seq (1, MaxPool)
      ]
    }
  }.

worker_atom (N) ->
  list_to_atom (atom_to_list (?POOL_ID)++"_"++integer_to_list (N)).
