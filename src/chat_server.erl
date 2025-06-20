-module(chat_server).
-behavior(gen_server).

-export([add_user/2, remove_user/1]).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

% Public interface

add_user(Username, Pid) ->
    gen_server:cast(?MODULE, {add_user, Username, Pid}).

remove_user(Username) ->
    gen_server:cast(?MODULE, {remove_user, Username}).

% Implementation

handle_cast({add_user, Username, Pid}, State) ->
    Users = maps:put(Username, Pid, maps:get(users, State)),
    {noreply, State#{users := Users}};

handle_cast({remove_user, Username}, State) ->
    Users = maps:remove(Username, maps:get(users, State)),
    {noreply, State#{users := Users}}.

% Server

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    io:format("Chat server listening on port ~p~n", [Port]),
    spawn(fun() -> accept(ListenSocket) end),
    {ok, #{users => #{}}}.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept(ListenSocket) end),
    handle_client(Socket).

handle_client(Socket) ->
    io:format("DEBUG: 11\n"),
    gen_tcp:send(Socket, "Login:"),
    case gen_tcp:recv(Socket, 0) of
        {ok, UsernameBin} ->
            Username = string:trim(binary_to_list(UsernameBin)),
            io:format("User connected: ~s~n", [Username]),
            {ok, Pid} = chat_user:start_link(Socket, Username),
            ok = gen_tcp:controlling_process(Socket, Pid);
        _ ->
            io:format("DEBUG: 14\n"),
            gen_tcp:close(Socket)
    end.

handle_call(_, _From, State) ->
    io:format("DEBUG: 12\n"),
    {noreply, State}.

handle_info(_, State) ->
    io:format("DEBUG: 13\n"),
    {noreply, State}.