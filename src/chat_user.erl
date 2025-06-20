-module(chat_user).
-behavior(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Implementation

handle_info({tcp, Socket, Data}, State) ->
    Msg = string:trim(binary_to_list(Data)),
    Response = 
        case Msg of
            "/help" ->
                "Available commands: /users, /help\n";
            "/users" ->
                Users = chat_server:get_users(),
                "Online users: " ++ string:join(Users, ", ") ++ "\n";
            _ ->
                "Command not known, type /help to see available commands.\n"
        end,
    gen_tcp:send(Socket, Response),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    %% Notify server to remove user when disconnected.
    chat_server:remove_user(maps:get(username, State)),
    io:format("User disconnected: ~s~n", [maps:get(username, State)]),
    {stop, normal, State};

handle_info(_, State) ->
    {noreply, State}.

%% Server

start_link(Socket, Username) ->
    gen_server:start_link(?MODULE, {Socket, Username}, []).

init({Socket, Username}) ->
    chat_server:add_user(Username, self()),
    inet:setopts(Socket, [{active, once}]),
    gen_tcp:send(Socket, io_lib:format(
        "Welcome, ~s.~nType /help to see available commands.~n", [Username])),
    {ok, #{socket => Socket, username => Username}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.