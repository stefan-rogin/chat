-module(chat_user).
-behavior(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(TXT, #{
    help =>
        "Available commands: /users\n"
        "/rooms, /create <Room>, /destroy <Room>\n"
        "/quit, /help\n",
    online_users => "Online users: ",
    rooms => "Rooms: ",
    no_rooms => "There are no rooms yet. Be the first to make one. :)\n",
    room_arg_created => "Room ~s created.~n",
    room_arg_destroyed => "Room ~s destroyed.~n",
    room_not_available => "A room with this name already exists. Please choose a different name.\n",
    room_not_owned => "You can only destroy rooms created by you.\n",
    room_not_present => "There is no room with this name.\n",
    bye => "Bye.\n",
    default =>
        "Incomplete command or message sent without joining a room first.\n"
        "Type /help to see available commands.\n",
    welcome_arg => "Welcome, ~s.~nType /help to see available commands.~n",
    user_arg_joined_room => "User ~s joined ~s.~n",
    room_joined_same => "You are already in this room.\n"
}).

%% Implementation

handle_info({tcp, Socket, Data}, State) ->
    Username = maps:get(username, State),
    {Cmd, Arg} = parse(Data),
    Response =
        case {Cmd, Arg} of
            {"/help", _} ->
                txt(help);
            {"/users", _} ->
                Users = chat_server:get_users(),
                txt(online_users) ++ string:join(Users, ", ") ++ "\n";
            {"/rooms", _} ->
                Rooms = chat_server:get_rooms(),
                case Rooms of
                    [_One | _] ->
                        txt(rooms) ++ string:join(Rooms, ", ") ++ "\n";
                    [] ->
                        txt(no_rooms)
                end;
            {"/create", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:create_room(RoomName, Username) of
                    ok ->
                        io_lib:format(txt(room_arg_created), [RoomName]);
                    {error, room_not_available} ->
                        txt(room_not_available)
                end;
            {"/destroy", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:destroy_room(RoomName, Username) of
                    ok ->
                        io_lib:format(txt(room_arg_destroyed), [RoomName]);
                    {error, room_not_owned} ->
                        txt(room_not_owned);
                    {error, room_not_present} ->
                        txt(room_not_present)
                end;
            {"/join", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:join_room(RoomName, Username) of
                    ok ->
                        io_lib:format(txt(user_arg_joined_room), [Username, RoomName]);
                    {error, room_joined_same} ->
                        txt(room_joined_same);
                    {error, room_not_present} ->
                        txt(room_not_present)
                end;
            {"/quit", _} ->
                gen_tcp:send(Socket, txt(bye)),
                gen_tcp:close(Socket),
                disconnect(State);
            _ ->
                txt(default)
        end,
    gen_tcp:send(Socket, Response),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    %% Notify server to remove user when disconnected.
    disconnect(State),
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

disconnect(State) ->
    Username = maps:get(username, State),
    chat_server:remove_user(Username),
    io:format("User disconnected: ~s~n", [Username]),
    {noreply, State}.

parse(Message) ->
    Trimmed = binary_to_list(string:trim(Message)),
    Tokens = string:tokens(Trimmed, " "),
    case Tokens of
        [Cmd, Arg | _] -> {Cmd, Arg};
        [Cmd] -> {Cmd, ""};
        _ -> {"", ""}
    end.

txt(Key) -> maps:get(Key, ?TXT).
%% Server

start_link(Username, Socket) ->
    gen_server:start_link(?MODULE, {Username, Socket}, []).

init({Username, Socket}) ->
    chat_server:add_user(Username, Socket),
    inet:setopts(Socket, [{active, once}]),
    gen_tcp:send(
        Socket,
        io_lib:format(
            txt(welcome_arg), [Username]
        )
    ),
    {ok, #{username => Username, socket => Socket}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.
