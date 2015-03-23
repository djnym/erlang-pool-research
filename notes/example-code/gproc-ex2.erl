% gen_server init function
init ([PoolName, Name]) ->
  % ensure terminate is called
  process_flag( trap_exit, true ),
  gproc_pool:connect_worker (PoolName, Name),
  {ok, #state {supervisor = PoolName, name = Name}}.
