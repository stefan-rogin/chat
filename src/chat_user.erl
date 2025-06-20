-module(chat_user).
-behavior(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Socket, Username) ->
    gen_server:start_link(?MODULE, {Socket, Username}, []).

init({Socket, Username}) ->
    chat_server:add_user(Username, self()),
    inet:setopts(Socket, [active, once]),
    gen_tcp:send(Socket, "Welcome, " ++ Username),
    {ok, #{socket => Socket, username => Username}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.