-module (pt_vmstats).

-behaviour (gen_server).

%% API
-export ([start_link/0,
          start_sampling/1,
          stop_sampling/0,
          fetch/0]).

%% gen_server callbacks
-export ( [ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3
          ]).

-record (state, {run_queue = 0, total_messages = 0, timer}).

%-=====================================================================-
%-                                  API                                -
%-=====================================================================-
start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

start_sampling (Millis) ->
  gen_server:cast (?MODULE, {start_sampling, Millis}).

stop_sampling () ->
  gen_server:cast (?MODULE, {stop_sampling}).

fetch () ->
  gen_server:call (?MODULE, {fetch}, 10000).

%-=====================================================================-
%-                        gen_server callbacks                         -
%-=====================================================================-
init([]) ->
  {ok, #state { } }.

handle_call ({fetch}, _From,
             State = #state { run_queue = RQ, total_messages = TM}) ->
  {reply, {RQ, TM}, State#state { run_queue = 0, total_messages = 0 } }.

handle_cast ({start_sampling, Millis}, State = #state { timer = OldTref }) ->
  timer:cancel (OldTref),
  TRef = timer:send_interval (Millis, collect),
  {noreply, State#state { timer = TRef, run_queue = 0, total_messages = 0 } };
handle_cast ({stop_sampling}, State = #state { timer = TRef }) ->
  timer:cancel (TRef),
  {noreply, State#state { timer = undefined } };
handle_cast (_Request, State) ->
  {noreply, State}.

handle_info (collect,
             State = #state { run_queue = RQIn, total_messages = TMIn }) ->
  { RQ, TM } = collect_sample (),
  { noreply,
   State#state {
      run_queue = case RQ > RQIn of true -> RQ ; false -> RQIn end,
      total_messages = case TM > TMIn of true -> TM ; false -> TMIn end
   }
  }.

terminate (_Reason, _State) ->
  ok.
code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%-=====================================================================-
%-                        private functions                            -
%-=====================================================================-
collect_sample () ->
  % Returns the total length of the run queues, that is, the number of
  % processes that are ready to run on all available run queues.
  RunQueue = erlang:statistics (run_queue),
  % total length of all message queues
  TotalMessages =
    lists:foldl (
      fun (Pid, Acc) ->
        case process_info(Pid, message_queue_len) of
          undefined -> Acc;
          {message_queue_len, Count} when Count > Acc -> Count;
          {message_queue_len, _ } -> Acc
        end
      end,
      0,
      processes()
    ),
  { RunQueue, TotalMessages }.
