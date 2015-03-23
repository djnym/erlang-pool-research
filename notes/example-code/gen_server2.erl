-module (stuff).

start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

do (Params) ->
  gen_server:call (?MODULE, {do, Params}).

init ([]) ->
  {ok, State}.

handle_call ({do, Params}, _From, State) ->
  Answer = do_some_work (Params, State),
  {reply, {ok, Answer}, State}.
