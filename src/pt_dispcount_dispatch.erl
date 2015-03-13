-module (pt_dispcount_dispatch).

-behaviour (dispcount).
-export([init/1, checkout/2, checkin/2,
         handle_info/2, dead/1, terminate/2, code_change/3]).

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

handle_info(_Msg, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
