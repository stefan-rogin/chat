-module(user_handler).
-export([handle_info/2]).

%% Message exchange handler between user and service. 
%% It identifies actions, sends them to server for execution,
%% then provides user feedback, based on their result.
%% Actions not involving server state are resolve locally.
handle_info({tcp, Socket, Data}, State) ->
    Username = maps:get(username, State),
    {Cmd, Arg} = parse(Data),
    Response =
        case {Cmd, Arg} of
            %% Show help
            {"/help", _} ->
                text:txt(help);
            %% Show users
            {"/users", _} ->
                Users = chat_server:get_users(),
                text:txt(online_users) ++ string:join(Users, ", ") ++ "\n";
            %% Show rooms
            {"/rooms", _} ->
                Rooms = chat_server:get_rooms(),
                case Rooms of
                    [_One | _] ->
                        text:txt(rooms) ++ string:join(Rooms, ", ") ++ "\n";
                    [] ->
                        text:txt(no_rooms)
                end;
            %% Create room
            {"/create", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:create_room(RoomName, Username) of
                    ok ->
                        io_lib:format(text:txt(room_arg_created), [RoomName]);
                    {error, room_not_available} ->
                        text:txt(room_not_available)
                end;
            %% Destroy room
            {"/destroy", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:destroy_room(RoomName, Username) of
                    ok ->
                        io_lib:format(text:txt(room_arg_destroyed), [RoomName]);
                    {error, room_not_owned} ->
                        text:txt(room_not_owned);
                    {error, room_not_present} ->
                        text:txt(room_not_present)
                end;
            %% Join room
            {"/join", RoomName} when is_list(RoomName), RoomName =/= [] ->
                case chat_server:join_room(RoomName, Username) of
                    ok -> ok;
                    {error, room_joined_same} ->
                        text:txt(room_joined_same);
                    {error, room_not_present} ->
                        text:txt(room_not_present)
                end;
            %% Leave room
            {"/leave", _}  ->
                case chat_server:leave_room(Username) of
                    {error, user_not_in_room} ->
                        text:txt(user_not_in_room);                    
                    RoomName ->
                        io_lib:format(text:txt(user_left_room), [RoomName])
                end;
            %% Send private message
            {"/whisper", Target} ->
                %% Strip first two tokens (Cmd, Arg) from original message
                Message = sanitize_message(Data),
                case chat_server:whisper_message(Target, Message) of
                    ok ->ok;
                    {error, user_not_online} ->
                        text:txt(user_not_online)
                end;
            %% Disconnect from service
            {"/quit", _} ->
                %% TODO: Move disconnect outside Response case
                gen_tcp:send(Socket, text:txt(bye)),
                gen_tcp:close(Socket),
                disconnect(State);
            %% Send message
            {"/send", Message} ->
                %% Not a command, ask server to send message
                case chat_server:send_message(Username, Message) of
                    ok -> ok;
                    {error, user_not_in_room} ->
                        text:txt(default)
                end;
            %% Unknown
            _ ->
                text:txt(default)
        end,
    %% Send feedback to user, if any
    case Response of
        R when is_list(R); is_binary(R) ->
            gen_tcp:send(Socket, R);
        _ -> ok
    end,
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
    %% TODO: Maybe discard empty messages?
    case Tokens of
        [Cmd, Arg | _] when hd(Cmd) =:= $/ -> {Cmd, Arg}; %% Command with arg
        [Cmd] when hd(Cmd) =:= $/ -> {Cmd, ""}; %% Command without arg
        _ -> {"/send", Trimmed} %% Implicit command /send
    end.

%% Strip a message of command and arg
sanitize_message(Message) ->
    Trimmed = string:trim(binary_to_list(Message)),
    re:replace(Trimmed, "^\\S+\\s+\\S+\\s+", "", [{return, list}]).