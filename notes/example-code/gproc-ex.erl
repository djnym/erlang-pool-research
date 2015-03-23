init ([_MinPool, MaxPool]) ->
  ok = gproc_pool:new (?POOL_ID, claim, []),
  { ok, { {one_for_one, 10, 10},
      [ begin
          WorkerName = {?POOL_ID, N},
          gproc_pool:add_worker (?POOL_ID, WorkerName),
          { WorkerName,
            {pt_gproc_worker, start_link, [?POOL_ID, WorkerName]},
            transient, 2000, worker, [pt_gproc_worker] }
        end
        || N <- lists:seq (1, MaxPool)
      ] } }.
do (N, Data) ->
  case gproc_pool:claim (?POOL_ID,
    fun (_,Pid) -> pt_gproc_worker:do (Pid, N, Data) end) of
      {true, Res} -> Res;
      false -> {error, busy}
  end.
