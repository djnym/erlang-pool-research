-module (pt_sup).

-behaviour (supervisor).

%% API
-export ([ start_link/1 ]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link (Mods) ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, [Mods]).

%%====================================================================
%% supervisor callbacks
%%====================================================================
init ([Mods]) ->
  MinPool = 20,
  MaxPool = 20,
  { ok,
    { {one_for_one, 10, 10},
      lists:flatten ([
        { pt_vmstats,
          {pt_vmstats, start_link, []},
          permanent,
          2000,
          worker,
          [pt_vmstats]
        },
        [ M:superconfig (MinPool, MaxPool) || M <- Mods ]
      ])
    }
  }.
