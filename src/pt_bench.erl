-module (pt_bench).

-export ([setup/0, teardown/1]).
-export ([warm/0,do_one/1, do/0, do/7, spawn_test_many/5, test_many_response/5,
          random_string/1, random_dict/1, all/0, pools/0 ]).

setup () ->
  ok.

teardown (ok) ->
  ok.

all() -> [ pt_baseline_sup | pools() ].

pools() -> [ pt_gsp_sup, pt_gproc_sup, pt_poolboy_sup,
             pt_dispcount_sup, pt_pooler_sup ].

do_one (Mod) ->
  io:format("library\tnum_callers\tcalls_per_caller\tcontext\treductions\tgood\tbad\trun_queue\tmessage_queues\tmin_time\tavg_time\tmax_time~n",[]),
  do (ok, 1, 5, 1000, [Mod], 5, undefined),
  do (ok, 1,10, 1000, [Mod], 5, undefined),
  do (ok, 1,15, 1000, [Mod], 5, undefined),
  do (ok, 1,20, 1000, [Mod], 5, undefined),
  do (ok, 1,25, 1000, [Mod], 5, undefined),
  do (ok, 1,30, 1000, [Mod], 5, undefined),
  do (ok, 1,35, 1000, [Mod], 5, undefined),
  do (ok, 1,40, 1000, [Mod], 5, undefined),
  do (ok, 1,45, 1000, [Mod], 5, undefined),
  do (ok, 1,50, 1000, [Mod], 5, undefined).

warm () ->
  % warm up
  do (undefined, 1, 5, 10, all(), 2, undefined).

do () ->
  warm (),
  lists:foreach (fun do_one/1, all()).

do(Device, Count, NumberToSpawn, NumberToRun, Modules, Pause, Data) ->
  Deps = setup(),
  bench (Device, NumberToSpawn, NumberToRun, Modules, Pause, Data, Count),
  teardown (Deps).

collect_stats () ->
  { ContextSwitches, _ } = erlang:statistics (context_switches),
  { TotalReductions, _ } = erlang:statistics (exact_reductions),

  { ContextSwitches, TotalReductions }.

bench(_,_, _, _, _, _, 0) ->
  ok;
bench(Device, NumberToSpawn, NumberToRun, Modules, Pause, Data, N) ->
  lists:foreach(
    fun (Mod) ->
      bench_one (
        [Device, Mod, NumberToSpawn, NumberToRun,
          {?MODULE, spawn_test_many,
           [NumberToSpawn, NumberToRun, {Mod, do}, Pause, Data]}
        ]
      )
    end,
    Modules
  ),
  bench(Device, NumberToSpawn, NumberToRun, Modules, Pause, Data, N-1).

bench_one([Device, Name, NumberToSpawn, NumberToRun, {Module, Fun, Args}]) ->
  garbage_collect(),
  {ContextSwitches0, Reductions0 } = collect_stats (),
  pt_vmstats:start_sampling (5),
  {{Good, Busy, _Errs}, {Min, Max, Sum,Count}} =
    erlang:apply (Module, Fun, Args),
  pt_vmstats:stop_sampling (),
  garbage_collect(),
  {ContextSwitches1, Reductions1} = collect_stats (),
  {RunQueueMax, MessageQueueMax} = pt_vmstats:fetch(),
  TimingAvg = trunc(Sum/ Count),

  case Device of
    undefined -> ok;
    _ ->
      io:format("~s\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n",
                [ Name,
                  NumberToSpawn,
                  NumberToRun,
                  ContextSwitches1 - ContextSwitches0,
                  Reductions1 - Reductions0,
                  Good,
                  Busy,
                  RunQueueMax,
                  MessageQueueMax,
                  Min,
                  TimingAvg,
                  Max
                ])
  end,
  ok.

random_chars () ->
  { $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
    $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
    $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
    $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
    $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_, $- }.

random_string (B) when is_binary (B) ->
  Chrs = random_chars (),
  Bin = crypto:hash (sha,B),
  << <<(element(N+1, Chrs)):8>> || <<N:6>> <= <<Bin/binary>> >>;
random_string (F) when is_float (F) ->
  Chrs = random_chars (),
  << <<(element(N+1,Chrs)):8>> || <<N:6>> <= <<F/float>> >>;
random_string (L) when is_integer (L) ->
  Chrs = random_chars (),
  F = fun(_, R) -> [element(random:uniform(64), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, L)).

random_dict (N) when is_integer (N) ->
  lists:foldl (fun (_, Dict) ->
                 dict:store (random_string (20), random_string (20), Dict)
               end,
               dict:new(),
               lists:seq (1,N)).

spawn_test_many (NumberToSpawn, NumberToRun, {M,F}, Pause, Data) ->
  Pids =
    [ spawn (?MODULE, test_many_response,
             [self(), {M,F}, Pause, Data, NumberToRun])
      || _
      <- lists:seq (1, NumberToSpawn)
    ],
  receive_until_done (Pids,[]).

receive_until_done ([], Accum) ->
  lists:foldl (fun ({{Good, Busy, Bad}, {Min, Max, Sum, Count}},
                    {{A1, A2, A3},{AMin, AMax, ASum, ACount}}) ->
                 {{A1+Good, A2+Busy, A3+Bad},
                  {min(Min, AMin), max(Max, AMax), Sum + ASum, Count + ACount}}
               end,
               {{0,0,0},{100000000000, 0, 0, 0}},
               Accum);
receive_until_done (Pids, Accum) ->
  receive
    {Pid, Data} -> receive_until_done (lists:delete (Pid, Pids), [Data|Accum])
  after
    30000000 -> io:format ("receive_until_done: ~p, ~p~n",[Pids,Accum])
  end.

test_many_response (Parent, {M,F}, Pause, Data, N) ->
  random:seed (now()),
  Results = test_many ({M,F}, Pause, Data, N,
                       {{0,0,0},{100000000000, 0, 0, 0}}),
  Parent ! { self(), Results }.

test_many (_, _, _, 0, A) ->
  A;
test_many ({M,F}, Pause, Data, N, {{Good, Busy, Bad},{Min, Max, Sum, Count}}) ->
  RealData = case Data of
               undefined -> undefined;
               {dict, NumberInDict} -> random_dict (NumberInDict)
             end,
  timer:sleep (random:uniform (5)),
  {NewAccum, Elapsed} =
    case catch test_one (M,F,[Pause, RealData]) of
      {E, {ok,_} } ->
        { {Good+1, Busy, Bad},  E };
      {E, {error, busy} } ->
        { {Good, Busy+1, Bad}, E };
      {E, Other} ->
        io:format ("error ~p~n",[Other]),
        { {Good, Busy, Bad+1}, E }
    end,
  NewMin = min (Elapsed, Min),
  NewMax = max (Elapsed, Max),
  NewSum = Elapsed + Sum,
  NewCount = Count + 1,
  test_many ({M,F}, Pause, Data, N-1,
             {NewAccum,{NewMin, NewMax, NewSum, NewCount}}).

test_one (Module, Function, Args) ->
  timer:tc(fun () ->
             erlang:apply (Module, Function, Args)
           end).
