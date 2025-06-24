-module(user_handler).
-export([handle_info/2]).

handle_info({tcp, Socket, Data}, State) ->
    Username = maps:get(username, State),
    {Cmd, Arg} = parse(Data),
    Response =
        case {Cmd, Arg} of
            {"/help", _} ->
                text:txt(help);
            {"/users", _} ->
                Users = chat_server:get_users(),
                text:txt(online_users) ++ string:join(Users, ", ") ++ "\n";
            {"/rooms", _} ->
                Rooms = chat_server:get_rooms(),
                case Rooms of
                    [_One | _] ->
                        text:txt(rooms) ++ string:join(Rooms, ", ") ++ "\n";
                    [] ->
                        text:txt(no_rooms)
                end;
            {"/create", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:create_room(RoomName, Username) of
                    ok ->
                        io_lib:format(text:txt(room_arg_created), [RoomName]);
                    {error, room_not_available} ->
                        text:txt(room_not_available)
                end;
            {"/destroy", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:destroy_room(RoomName, Username) of
                    ok ->
                        io_lib:format(text:txt(room_arg_destroyed), [RoomName]);
                    {error, room_not_owned} ->
                        text:txt(room_not_owned);
                    {error, room_not_present} ->
                        text:txt(room_not_present)
                end;
            {"/join", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:join_room(RoomName, Username) of
                    ok ->
                        %% TODO: Move in messages, not trace
                        io_lib:format(text:txt(user_arg_joined_room), [Username, RoomName]);
                    {error, room_joined_same} ->
                        text:txt(room_joined_same);
                    {error, room_not_present} ->
                        text:txt(room_not_present)
                end;
            {"/leave", _}  ->
                case chat_server:leave_room(Username) of
                    {error, user_not_in_room} ->
                        text:txt(user_not_in_room);                    
                    RoomName ->
                        %% TODO: Move in messages, not trace
                        io_lib:format(text:txt(user_arg_left_room), [Username, RoomName])
                end;
            {"/quit", _} ->
                gen_tcp:send(Socket, text:txt(bye)),
                gen_tcp:close(Socket),
                disconnect(State);
            _ ->
                text:txt(default)
        end,
    gen_tcp:send(Socket, Response),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

%% Connection closed
handle_info({tcp_closed, _Socket}, State) ->
    %% Notify server to remove user when disconnected.
    disconnect(State),
    {stop, normal, State};

%% Unsupported
handle_info(_, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {noreply, State}.

%% Disconnect user
disconnect(State) ->
    Username = maps:get(username, State),
    chat_server:remove_user(Username),
    io:format("User disconnected: ~s~n", [Username]),
    {noreply, State}.

%% Parse message
parse(Message) ->
    Trimmed = string:trim(binary_to_list(Message)),
    Tokens = string:tokens(Trimmed, " "),
    case Tokens of
        [Cmd, Arg | _] -> {Cmd, Arg};
        [Cmd] -> {Cmd, ""};
        _ -> {"", ""}
    end.