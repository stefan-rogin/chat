-module(chat_user).
-behavior(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Implementation: delegate to handler

handle_info(Msg, State) ->
    user_handler:handle_info(Msg, State).

%% Server

start_link(Username, Socket) ->
    gen_server:start_link(?MODULE, {Username, Socket}, []).

init({Username, Socket}) ->
    chat_server:add_user(Username, Socket),
    inet:setopts(Socket, [{active, once}]),
    gen_tcp:send(
        Socket,
        io_lib:format(
            text:txt(welcome_arg), [Username]
        )
    ),
    {ok, #{username => Username}}.

%% Unsupported
handle_call(_, _From, State) -> 
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {reply, unhandled, State}.

%% Unsupported
handle_cast(_, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {noreply, State}.
