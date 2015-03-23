init ([MinPool, MaxPool]) ->
  pooler:new_pool (
    [{name, pt_pooler_pool},
     {max_count, MaxPool},
     {init_count, MinPool},
     {max_age, {60, min}},
     {start_mfa, {pt_baseline_worker, start_link, []}}],
  {ok, #state {}}.

do (N, Data) ->
  case pooler:take_member (pt_pooler_pool) of
    error_no_members -> {error, busy};
    P -> Res = pt_baseline_worker:do (P, N, Data),
         pooler:return_member (pt_pooler_pool, P, ok),
         Res
  end.
