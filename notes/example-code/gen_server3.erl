-module (stuff).

start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

get_data (Query) ->
  gen_server:call (?MODULE, {get_data, Query}).

init ([]) ->
  Connection = connect_to_somewhere (),
  {ok, Connection}.

handle_call ({get_data, Query}, _From, Connection) ->
  Answer = query (Query, Connection),
  {reply, Answer, Connection}.
