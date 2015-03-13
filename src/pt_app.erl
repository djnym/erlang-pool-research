-module (pt_app).

-behaviour (application).

-export ([start/0]).
-export ([start/2, stop/1]).

start () ->
  [ ensure_started (A) || A <- [ gproc, poolboy, dispcount, leo_pod, pt ] ].

start (_Type, _StartArgs) ->
  pt_sup:start_link ().

stop (_State) ->
  ok.

ensure_started(App) ->
  case application:start(App) of
    ok ->
      App;
    {error, {already_started, App}} ->
      App;
    Other ->
      io:format ("ensure_started (~p) : ~p~n",[App, Other]),
      App
  end.
