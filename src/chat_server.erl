-module(chat_server).
-behavior(gen_server).

-export([add_user/2, remove_user/1, get_users/0, is_user_online/1]).
-export([create_room/2, get_rooms/0, destroy_room/2]).
-export([join_room/2, leave_room/1, send_message/2, whisper_message/2]).
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

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

join_room(RoomName, Username) ->
    gen_server:call(?MODULE, {join_room, RoomName, Username}).

leave_room(Username) ->
    gen_server:call(?MODULE, {leave_room, Username}).

send_message(Username, Message) ->
    gen_server:call(?MODULE, {send_message, Username, Message}).

whisper_message(Username, Message) ->
    gen_server:call(?MODULE, {whisper_message, Username, Message}).

%% Implementation: delegate to handler

handle_cast(Msg, State) ->
    server_handler:handle_cast(Msg, State).

handle_call(Msg, _From, State) ->
    server_handler:handle_call(Msg, _From, State).

%% Server

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(
        Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]
    ),
    io:format("Chat server listening on port ~p~n", [Port]),
    %% Spawn acceptor process
    spawn(fun() -> accept(ListenSocket) end),
    {ok, #{users => #{}, rooms => #{}, users_rooms => #{}}}.

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            %% Handle new connection and spawn a new available acceptor
            spawn(fun() -> accept(ListenSocket) end),
            login_handler:handle_login(Socket);
        {error, closed} ->
            ok
    end.

%% Unsupported
handle_info(_, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {noreply, State}.