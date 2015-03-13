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
  pt_pooler_sup:init (),
  { ok,
    { {one_for_one, 10, 10},
      [
        { pt_baseline_sup,
          {pt_baseline_sup, start_link, []},
          permanent,
          2000,
          supervisor,
          [pt_baseline_sup]
        },
        { pt_gsp_sup,
          {pt_gsp_sup, start_link, []},
          permanent,
          2000,
          worker,
          [pt_gsp_sup]
        },
        { pt_gproc_sup,
          {pt_gproc_sup, start_link, []},
          permanent,
          2000,
          supervisor,
          [pt_gproc_sup]
        },
        { pt_poolboy_sup,
          {pt_poolboy_sup, start_link, []},
          permanent,
          2000,
          supervisor,
          [pt_poolboy_sup]
        },
        { pt_dispcount_sup,
          {pt_dispcount_sup, start_link, []},
          permanent,
          2000,
          supervisor,
          [pt_dispcount_sup]
        },
        { pt_leo_pod_sup,
          {pt_leo_pod_sup, start_link, []},
          permanent,
          2000,
          worker,
          [pt_leo_pod_sup]
        }
      ]
    }
  }.
