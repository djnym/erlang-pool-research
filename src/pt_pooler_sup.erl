-module (pt_pooler_sup).

-behaviour (gen_server).

%% API
-export ([start_link/2, do/2, superconfig/2]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {}).

superconfig (MinPool, MaxPool) ->
  [ { pooler_sup,
      {pooler_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [pooler_sup]
    },
    { pt_pooler_sup,
      {pt_pooler_sup, start_link, [MinPool, MaxPool]},
      permanent,
      2000,
      worker,
      [pt_pooler_sup]
    }
  ].

start_link (MinPool, MaxPool) ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [MinPool, MaxPool], []).

do (N, Data) ->
  case pooler:take_member (pt_pooler_pool) of
    error_no_members -> {error, busy};
    P ->
      Res = pt_baseline_worker:do (P, N, Data),
      pooler:return_member (pt_pooler_pool, P, ok),
      Res
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([_MinPool, MaxPool]) ->
  % ensure terminate is called
  process_flag( trap_exit, true ),
  WorkerArgs = [],
  PoolConfig = [{name, pt_pooler_pool},
                {max_count, MaxPool},
                {init_count, MaxPool},
                {max_age, {60, min}},
                {start_mfa, { pt_baseline_worker, start_link, [WorkerArgs] } }
               ],
  pooler:new_pool (PoolConfig),
  {ok, #state {}}.

handle_call (Request, From, State) ->
  error_logger:warning_msg ("~p : Unrecognized call ~p from ~p~n",
                            [?MODULE, Request, From]),
  { reply, ok, State }.

handle_cast (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized cast ~p~n",[?MODULE, Request]),
  { noreply, State }.

handle_info (Request, State) ->
  error_logger:warning_msg ("~p : Unrecognized info ~p~n",[?MODULE, Request]),
  { noreply, State }.

terminate (_Reason, #state {}) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
