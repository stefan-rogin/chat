-module(chat_server).
-behavior(gen_server).

-export([add_user/2, remove_user/1, get_users/0]).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% Public interface

add_user(Username, Pid) ->
    gen_server:cast(?MODULE, {add_user, Username, Pid}).

remove_user(Username) ->
    gen_server:cast(?MODULE, {remove_user, Username}).

get_users() ->
    gen_server:call(?MODULE, get_users).

%% Implementation

handle_cast({add_user, Username, Pid}, State) ->
    Users = maps:put(Username, Pid, maps:get(users, State)),
    {noreply, State#{users := Users}};

handle_cast({remove_user, Username}, State) ->
    Users = maps:remove(Username, maps:get(users, State)),
    {noreply, State#{users := Users}}.

handle_call(get_users, _From, State) ->
    {reply, maps:keys(maps:get(users, State)), State};

handle_call(_, _From, State) ->
    {noreply, State}.

%% Server

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(
        Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    io:format("Chat server listening on port ~p~n", [Port]),
    %% Spawn acceptor process
    spawn(fun() -> accept(ListenSocket) end),
    {ok, #{users => #{}}}.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    
    %% Handle new connection and spawn a new available acceptor
    spawn(fun() -> accept(ListenSocket) end),
    handle_client(Socket).

handle_client(Socket) ->
    %% Prompt user to authenticate
    gen_tcp:send(Socket, "Login:"),

    case gen_tcp:recv(Socket, 0) of
        {ok, UsernameBin} ->
            %% Match reply to a username-like or drop connection 
            case re:run(UsernameBin, "^[A-Za-z0-9_]+", [{capture, first, list}]) of
                {match, [Username]} ->
                    io:format("User connected: ~s~n", [Username]),
                    %% Start a dedicated process for the new user
                    {ok, Pid} = chat_user:start_link(Socket, Username),
                    %% Pass socket ownership to the spawned process
                    ok = gen_tcp:controlling_process(Socket, Pid);
                nomatch ->
                    gen_tcp:send(Socket, "Invalid username.\n"),
                    gen_tcp:close(Socket)
            end;
        _ ->
            gen_tcp:close(Socket)
    end.

handle_info(_, State) ->
    {noreply, State}.