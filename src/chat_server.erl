-module(chat_server).
-behavior(gen_server).

-export([add_user/2, remove_user/1, get_users/0, is_user_online/1]).
-export([create_room/2, get_rooms/0, destroy_room/2]).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TXT, #{
    login => "Login:",
    username_not_valid => "Invalid username.\n",
    username_not_offline => "You're already logged in from a different client, disconnecting.\n"
}).
%% Public interface

add_user(Username, Socket) ->
    gen_server:cast(?MODULE, {add_user, Username, Socket}).

remove_user(Username) ->
    gen_server:cast(?MODULE, {remove_user, Username}).

get_users() ->
    gen_server:call(?MODULE, get_users).

is_user_online(Username) ->
    gen_server:call(?MODULE, {is_user_online, Username}).

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

create_room(RoomName, Username) ->
    gen_server:call(?MODULE, {create_room, RoomName, Username}).

destroy_room(RoomName, Username) ->
    gen_server:call(?MODULE, {destroy_room, RoomName, Username}).

%% Implementation

handle_cast({add_user, Username, Socket}, State) ->
    Users = maps:put(Username, Socket, maps:get(users, State)),
    {noreply, State#{users := Users}};

handle_cast({remove_user, Username}, State) ->
    Users = maps:remove(Username, maps:get(users, State)),
    {noreply, State#{users := Users}}.

handle_call({create_room, RoomName, Username}, _From, State) ->
    Rooms = maps:get(rooms, State),
    case maps:is_key(RoomName, Rooms) of
        true ->
            {reply, {error, room_not_available}, State};
        false ->
            NewRooms = maps:put(RoomName, Username, Rooms),
            {reply, ok, State#{rooms := NewRooms}}
    end;

handle_call({destroy_room, RoomName, Username}, _From, State) ->
    Rooms = maps:get(rooms, State),
    case maps:find(RoomName, Rooms) of
        {ok, Username} ->
            NewRooms = maps:remove(RoomName, Rooms),
            {reply, ok, State#{rooms := NewRooms}};
        {ok, _OtherUsername} ->
            {reply, {error, room_not_owned}, State};
        error ->
            {reply, {error, room_not_present}, State}
    end;

handle_call(get_users, _From, State) ->
    {reply, maps:keys(maps:get(users, State)), State};

handle_call({is_user_online, Username}, _From, State) ->
    IsOnline = maps:is_key(Username, maps:get(users, State)),
    {reply, IsOnline, State};

handle_call(get_rooms, _From, State) ->
    {reply, maps:keys(maps:get(rooms, State)), State};

handle_call(_, _From, State) ->
    {noreply, State}.

txt(Key) -> maps:get(Key, ?TXT).

%% Server

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(
        Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    io:format("Chat server listening on port ~p~n", [Port]),
    %% Spawn acceptor process
    spawn(fun() -> accept(ListenSocket) end),
    {ok, #{users => #{}, rooms => #{}}}.

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    
    %% Handle new connection and spawn a new available acceptor
    spawn(fun() -> accept(ListenSocket) end),
    handle_client(Socket).

handle_client(Socket) ->
    %% Prompt user to authenticate
    gen_tcp:send(Socket, txt(login)),

    %% TODO: Fix nesting
    case gen_tcp:recv(Socket, 0) of
        {ok, UsernameBin} ->
            %% Match reply to a username-like or drop connection 
            case re:run(UsernameBin, "^[A-Za-z0-9_]+", [{capture, first, list}]) of
                {match, [Username]} ->
                    %% Disallow multiple connections from the same user
                    case chat_server:is_user_online(Username) of 
                        true ->
                            gen_tcp:send(Socket, txt(username_not_offline)),
                            gen_tcp:close(Socket);
                        false ->
                            io:format("User connected: ~s~n", [Username]),
                            %% Start a dedicated process for the new user
                            {ok, Pid} = chat_user:start_link(Username, Socket),
                            %% Pass socket ownership to the spawned process
                            ok = gen_tcp:controlling_process(Socket, Pid)
                    end;
                nomatch ->
                    gen_tcp:send(Socket, txt(username_not_valid)),
                    gen_tcp:close(Socket)
            end;
        _ ->
            gen_tcp:close(Socket)
    end.

handle_info(_, State) ->
    {noreply, State}.