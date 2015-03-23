-module (stuff).

start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

get_state () ->
  gen_server:call (?MODULE, {get_state}).

init ([]) ->
  State = get_state_from_somewhere (),
  {ok, State}.

handle_call ({get_state}, _From, State) ->
  {reply, {ok, State}, State}.
