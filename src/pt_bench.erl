-module (pt_bench).

-export ([setup/0, teardown/1]).
-export ([do/1, do/4, do/6, spawn_test_many/5, test_many_response/5,
          random_string/1, random_dict/1, all/0, pools/0 ]).

setup () ->
  ok.

teardown (ok) ->
  ok.

median(L) ->
  %% It's possible to do this without sorting.
  %% http://en.wikipedia.org/wiki/Selection_algorithm
  LSort = lists:sort(L),
  N = trunc(length(L) / 2),
  lists:nth(N + 1, LSort).

% transpose a matrix represented as list of lists
% aka "zip" list of lists
% This implementation originates from Haskell
transpose([[X | Xs] | Xss]) ->
  [[X | [H || [H | _] <- Xss]]
    | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [];

transpose(Tuple) when is_tuple(Tuple) ->  % wrapper to emulate zip
  Xs = transpose(tuple_to_list(Tuple)),
  [list_to_tuple(X) || X <- Xs].

%% Takes a list of triples, where the first element is a key and the
%% list is sorted on this key, which may be repeated, and creates a
%% list in which each key is represented once and the value is a list
%% of the second elements in the original list.
hoist(List) ->
  lists:reverse(hoist(List, [])).

hoist([{K,V1,_} | Rest], [{K,L} | Acc]) ->
  hoist(Rest, [{K, [V1 | L]} | Acc]);
hoist([{K,V1,_} | Rest], Acc) ->
  hoist(Rest, [{K, [V1]} | Acc]);
hoist([], Acc) ->
  Acc.

all() -> [ pt_baseline_sup | pools() ].

pools() -> [ pt_gsp_sup, pt_gproc_sup, pt_poolboy_sup,
             pt_dispcount_sup, pt_pooler_sup, pt_leo_pod_sup ].

do(Count) ->
  do (Count, 10, 100, all(), 2, undefined).

do(Count, NumberToSpawn, NumberToRun, Modules) ->
  do(Count, NumberToSpawn, NumberToRun, Modules, 2, undefined).

do(Count, NumberToSpawn, NumberToRun, Modules, Pause, Data) ->
  Deps = setup(),
  Times = bench (NumberToSpawn, NumberToRun, Modules, Pause, Data, Count, []),
  Times2 = lists:flatten(Times),
  Times3 = hoist(Times2),
  Times4 =
    lists:map(fun ({Name, Values}) ->
                {Name, median(Values)}
              end, Times3),

  MinElapsed = max( lists:min(
                      lists:map(fun ({_Name, Elapsed}) -> Elapsed end,
                                Times4)
                    ),
                    0.0000005
                  ),

  lists:foreach(fun ({Name, Elapsed}) ->
                  io:format("~-30s ~15.6f ms/iter (~8.2fx)~n",
                  [Name, Elapsed*1000/Count, Elapsed/MinElapsed])
                end, Times4),
  teardown (Deps).

bench(_, _, _, _, _, 0, Acc) ->
  transpose(Acc);
bench(NumberToSpawn, NumberToRun, Modules, Pause, Data, N, Acc) ->
  io:format("** round ~w **~n", [N]),
  io:format("~11s  ~13s ~9s ~15s ~15s ~10s ~10s ~10s~n",
            [ "", "Elapsed", "CPU", "Switches", "Reductions",
              "Good", "Busy", "Errs" ]),
  Results = lists:reverse(
              lists:foldl(
                fun (Mod, A) ->
                  bench_one (
                    [Mod,
                      {?MODULE, spawn_test_many,
                       [NumberToSpawn, NumberToRun, {Mod, do}, Pause, Data]}
                    ],
                    A)
                end,
                [],
                Modules
                )),
  %% We no longer need the data; drop it.
  ResultsWithoutData =
    lists:map(fun ({Name, Elapsed, CPU, _Data}) ->
                {Name, Elapsed, CPU}
              end,
              Results),
  bench(NumberToSpawn, NumberToRun, Modules, Pause, Data, N-1, [ResultsWithoutData | Acc]).

collect_stats () ->
  { TotalRuntime, _ } = erlang:statistics (runtime),
  { ContextSwitches, _ } = erlang:statistics (context_switches),
  { TotalReductions, _ } =
    erlang:statistics (exact_reductions),

  Memory = erlang:memory(),
  TotalMemory = proplists:get_value (total, Memory),
  ProcessMemory = proplists:get_value (processes_used, Memory),
  SystemMemory = proplists:get_value (system, Memory),
  AtomUsed = proplists:get_value (atom_used, Memory),
  BinaryMemory = proplists:get_value (binary, Memory),
  EtsMemory = proplists:get_value (ets, Memory),

  { TotalRuntime, ContextSwitches, TotalReductions,
    TotalMemory, ProcessMemory, SystemMemory,
    AtomUsed, BinaryMemory, EtsMemory }.

bench_one([Name, {Module, Fun, Args}], Acc) ->
  garbage_collect(),
  {CPU0, ContextSwitches0, Reductions0,
   _TotalMemory0, _ProcessMemory0, _SystemMemory0,
   _AtomUsed0, _BinaryMemory0, _EtsMemory0 } = collect_stats (),
  {Elapsed, Results = {Good, Busy, Errs}} =
    timer:tc(fun () -> erlang:apply (Module, Fun, Args) end),
  garbage_collect(),
  {CPU1, ContextSwitches1, Reductions1,
   _TotalMemory1, _ProcessMemory1, _SystemMemory1,
   _AtomUsed1, _BinaryMemory1, _EtsMemory1 } = collect_stats (),
  CPU = CPU1 - CPU0,
  io:format("~11s: ~10.3f ms ~6w ms ~15w ~15w ~10w ~10w ~10w~n",
            [ Name,
              Elapsed/1000,
              CPU,
              ContextSwitches1 - ContextSwitches0,
              Reductions1 - Reductions0,
              Good,
              Busy,
              Errs
            ]),
%  io:format ("T:~p P:~p S:~p A:~p B:~p E:~p~n",[
%               TotalMemory1 - TotalMemory0,
%               ProcessMemory1 - ProcessMemory0,
%               SystemMemory1 - SystemMemory0,
%               AtomUsed1 - AtomUsed0,
%               BinaryMemory1 - BinaryMemory0,
%               EtsMemory1 - EtsMemory0
%             ]),
  [{Name, Elapsed/1000000, CPU/1000, Results} | Acc].

random_chars () ->
  { $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
    $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
    $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
    $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
    $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_, $- }.

random_string (B) when is_binary (B) ->
  Chrs = random_chars (),
  Bin = crypto:sha (B),
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
    [ spawn (?MODULE, test_many_response, [self(), {M,F}, Pause, Data, NumberToRun])
      || _
      <- lists:seq (1, NumberToSpawn)
    ],
  receive_until_done (Pids,[]).

receive_until_done ([], Accum) ->
  lists:foldl (fun ({Good, Busy, Bad},{A1, A2, A3}) ->
                 {A1+Good, A2+Busy, A3+Bad}
               end,
               {0,0,0},
               Accum);
receive_until_done (Pids, Accum) ->
  receive
    {Pid, Data} -> receive_until_done (lists:delete (Pid, Pids), [Data|Accum])
  after
    300000 -> io:format ("receive_until_done: ~p, ~p~n",[Pids,Accum])
  end.

test_many_response (Parent, {M,F}, Pause, Data, N) ->
  random:seed (now()),
  Results = test_many ({M,F}, Pause, Data, N, {0,0,0}),
  Parent ! { self(), Results }.

test_many (_, _, _, 0, A) ->
  A;
test_many ({M,F}, Pause, Data, N, {Good, Busy, Bad}) ->
  RealData = case Data of
               undefined -> undefined;
               {dict, NumberInDict} -> random_dict (NumberInDict)
             end,
  timer:sleep (random:uniform (10)),
  NewAccum =
    case catch test_one (M,F,[Pause, RealData]) of
      {ok,_} -> {Good+1, Busy, Bad};
      {error, busy} -> {Good, Busy+1, Bad};
      {error, empty} ->
        % leo_pod busy error
        {Good, Busy+1, Bad};
      {error, request_dropped} -> {Good, Busy+1, Bad};
      {_Error, {timeout, _CallStack}} -> {Good, Busy+1, Bad};
      Other ->
        io:format ("error ~p~n",[Other]),
        {Good, Busy, Bad+1}
    end,
  test_many ({M,F}, Pause, Data, N-1, NewAccum).

test_one (Module, Function, Args) ->
  erlang:apply (Module, Function, Args).
