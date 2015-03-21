-module (pt_sup).

-behaviour (supervisor).

%% API
-export ([ start_link/0 ]).

%% supervisor callbacks
-export ([init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link () ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================
init ([]) ->
  MinPool = 20,
  MaxPool = 20,
  { ok,
    { {one_for_one, 10, 10},
      [
        { pt_vmstats,
          {pt_vmstats, start_link, []},
          permanent,
          2000,
          worker,
          [pt_vmstats]
        },
        { pt_baseline_sup,
          {pt_baseline_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          supervisor,
          [pt_baseline_sup]
        },
        { pt_gsp_sup,
          {pt_gsp_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          worker,
          [pt_gsp_sup]
        },
        { pt_gproc_sup,
          {pt_gproc_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          supervisor,
          [pt_gproc_sup]
        },
        { pt_poolboy_sup,
          {pt_poolboy_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          supervisor,
          [pt_poolboy_sup]
        },
        { dispcount_supersup,
          {dispcount_supersup, start_link, []},
          permanent,
          infinity,
          supervisor,
          [dispcount_supersup]
        },
        { pt_dispcount_sup,
          {pt_dispcount_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          supervisor,
          [pt_dispcount_sup]
        },
        { pt_leo_pod_sup,
          {pt_leo_pod_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          worker,
          [pt_leo_pod_sup]
        },
        { pooler_sup,
          {pooler_sup, start_link, []},
          permanent,
          infinity,
          supervisor,
          [pooler_sup]
        },
        { pt_pooler_sup,
          {pt_pooler_sup, start_link, [MinPool, MaxPool]},
          permanent,
          2000,
          worker,
          [pt_pooler_sup]
        }
      ]
    }
  }.
