init ([_MinPool, MaxPool]) ->
  ok = dispcount:start_dispatch (
         ?POOL_ID, {pt_dispcount_dispatch, []},
         [{restart, permanent}, {shutdown, 4000},
           {maxr, 10}, {maxt, 60}, {resources, MaxPool}]
       ),
  {ok, Info} = dispcount:dispatcher_info (?POOL_ID),
  mochiglobal:put (?MOCHIGLOBAL_ID, Info),
  { ok, #state { info = Info } }.

do (N, Data) ->
  PoolInfo = mochiglobal:get (?MOCHIGLOBAL_ID),
  case dispcount:checkout (PoolInfo) of
    {ok, CheckinReference, Pid} ->
      pt_baseline_worker:do (Pid, N, Data),
      dispcount:checkin(PoolInfo, CheckinReference, Pid),
      {ok, {worked, N}};
    {error, busy} -> {error, busy }
  end.
