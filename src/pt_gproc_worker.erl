-module (pt_gproc_worker).

-behaviour (gen_server).

%% API
-export ([start_link/3]).
-export ([do/3]).

%% gen_server callbacks
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
         ]).

-record (state, {supervisor, name}).

start_link (SupervisorName, Atom, Name) ->
  gen_server:start_link ({local, Atom}, ?MODULE, [SupervisorName, Name], []).

do (Pid, N, Data) ->
  gen_server:call (Pid, {work, N, Data}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init ([SupervisorName, Name]) ->
  % ensure terminate is called
  process_flag( trap_exit, true ),
  gproc_pool:connect_worker (SupervisorName, Name),
  {ok, #state {supervisor = SupervisorName, name = Name}}.

handle_call ({work, N, _Data}, _From, State) ->
  timer:sleep (N),
  { reply, {ok, {worked, N}}, State};
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

terminate (_Reason, #state {supervisor = SupervisorName, name = Name}) ->
  gproc_pool:disconnect_worker (SupervisorName, Name),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.
