-module (pt_baseline_worker).

%% API
-export ([ start_link/1, do/3 ]).

start_link(WorkerArgs) ->
  gen_server:start_link(?MODULE, WorkerArgs, []).

do (Pid, N, Data) ->
  gen_server:call (Pid, {work, N, Data}).

handle_call ({work, N, Data}, _From, State) ->
  { reply, {ok, pt_util:work (N, Data)}, State}.
