-behaviour (dispcount).
-record (state, {pid, given=false, args}).
init(WorkerArgs) ->
  {ok, P} = pt_baseline_worker:start_link (WorkerArgs),
  {ok, #state {pid = P, args = WorkerArgs}}.
checkout(_From, State = #state{given=true}) ->
  {error, busy, State};
checkout(_From, State = #state{pid=Pid}) ->
  {ok, Pid, State#state{given=true}}.
checkin(Pid, State = #state{pid=Pid, given=true}) ->
  {ok, State#state{given=false}};
checkin(_Pid, State) ->
  {ignore, State}.
dead(State = #state {args = WorkerArgs}) ->
  {ok, P} = pt_baseline_worker:start_link (WorkerArgs),
  %% lost resource so start a new one
  {ok, State#state{pid=P,given=false}}.
