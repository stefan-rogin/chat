-module(chat_user).
-behavior(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Socket, Username) ->
    gen_server:start_link(?MODULE, {Socket, Username}, []).

init({Socket, Username}) ->
    io:format("DEBUG: 21\n"),
    chat_server:add_user(Username, self()),
    inet:setopts(Socket, [{active, once}]),
    gen_tcp:send(Socket, io_lib:format("Welcome, ~s~n", [Username])),
    {ok, #{socket => Socket, username => Username}}.

handle_call(_, _From, State) ->
    io:format("DEBUG: 22\n"),
    {noreply, State}.

handle_cast(_, State) ->
    io:format("DEBUG: 23\n"),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("DEBUG: 24\n"),
    gen_tcp:send(Socket, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    io:format("DEBUG: 25\n"),
    chat_server:remove_user(maps:get(username, State)),
    io:format("User disconnected: ~s~n", [maps:get(username, State)]),
    {stop, normal, State};

handle_info(_, State) ->
    io:format("DEBUG: 26\n"),
    {noreply, State}.