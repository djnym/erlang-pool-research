-module (pt_baseline_sup).

-behaviour (supervisor).

-export ([start_link/2, do/2]).
-export ([init/1]).

-define (ID, baseline_worker).

start_link (_Min, _Max) ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

do (N, Data) ->
  pt_baseline_worker:do (?ID, N, Data).

init ([]) ->
  { ok,
    { {one_for_one, 10, 10},
      [
        { pt_baseline_worker,
          { gen_server, start_link,
            [ {local, ?ID}, pt_baseline_worker, [], [] ]
          },
          permanent,
          2000,
          worker,
          [pt_baseline_worker]
        }
      ]
    }
  }.
