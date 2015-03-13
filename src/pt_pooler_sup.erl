-module (pt_pooler_sup).

%% API
-export ([ init/0, do/2 ]).

init () ->
  WorkerArgs = [],
  PoolConfig = [{name, pt_pooler_pool},
                {max_count, 100},
                {init_count, 10},
                {max_age, {60, min}},
                {start_mfa, { pt_baseline_worker, start_link, [WorkerArgs] } }
               ],

  pooler:new_pool (PoolConfig).

do (N, Data) ->
  case pooler:take_member (pt_pooler_pool) of
    error_no_members -> {error, busy};
    P ->
      Res = pt_baseline_worker:do (P, N, Data),
      pooler:return_member (pt_pooler_pool, P, ok),
      Res
  end.
