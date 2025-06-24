-module(server_handler).

-export([handle_cast/2, handle_call/3]).

-define(SERVER, chat_server).

%% Create room
handle_call({create_room, RoomName, Username}, _From, State) ->
    Rooms = maps:get(rooms, State),
    case maps:is_key(RoomName, Rooms) of
        %% Don't allow duplicate rooms
        true ->
            {reply, {error, room_not_available}, State};
        false ->
            NewRooms = maps:put(RoomName, Username, Rooms),
            {reply, ok, State#{rooms := NewRooms}}
    end;

%% Destroy room
handle_call({destroy_room, RoomName, Username}, _From, State) ->
    Rooms = maps:get(rooms, State),
    case maps:find(RoomName, Rooms) of
        {ok, Username} ->
            %% FIXME: Notify room members about destruction
            %% Sockets are removed before async call done
            % Notification = text:txt(room_destroyed),
            % gen_server:cast(?SERVER, {notify_room, RoomName, Notification}),
            UsersInRooms = maps:get(users_rooms, State),
            %% Remove from users_rooms entries with value of deleted room
            NewUsersInRooms = maps:filter(fun(_, Room) -> Room =/= RoomName end, UsersInRooms),
            NewRooms = maps:remove(RoomName, Rooms),
            {reply, ok, State#{rooms := NewRooms, users_rooms := NewUsersInRooms}};
        {ok, _OtherUsername} ->
            {reply, {error, room_not_owned}, State};
        error ->
            {reply, {error, room_not_present}, State}
    end;

%% Join room
handle_call({join_room, RoomName, Username}, _From, State) ->
    Rooms = maps:get(rooms, State),
    %% Hold state in an user => room map
    UsersInRooms = maps:get(users_rooms, State),
    case maps:is_key(RoomName, Rooms) of
        false ->
            {reply, {error, room_not_present}, State};
        true ->
            case maps:find(Username, UsersInRooms) of
                %% Already in same room
                {ok, RoomName} ->
                    {reply, {error, room_joined_same}, State};
                {ok, OtherRoom} ->
                    %% User is in other room. Automatically leave and join the new one
                    InterimUsersInRooms = leave_room_internal(Username, OtherRoom, UsersInRooms),
                    NewUsersInRooms = join_room_internal(RoomName, Username, InterimUsersInRooms),
                    {reply, ok, State#{users_rooms := NewUsersInRooms}};
                error ->
                    %% User not in any room already, just join
                    NewUsersInRooms = join_room_internal(RoomName, Username, UsersInRooms),
                    {reply, ok, State#{users_rooms := NewUsersInRooms}}
            end
    end;

%% Leave room
handle_call({leave_room, Username}, _From, State) ->
    UsersInRooms = maps:get(users_rooms, State),
    case maps:find(Username, UsersInRooms) of
        {ok, RoomName} ->
            %% Invoke helper to notify room about user exit, 
            %% then prepare state changes for State#{users_rooms} 
            NewUsersInRooms = leave_room_internal(Username, RoomName, UsersInRooms),
            {reply, RoomName, State#{users_rooms := NewUsersInRooms}};
        error ->
            {reply, {error, user_not_in_room}, State}
    end;

%% Send message
handle_call({send_message, Username, Message}, _From, State) ->
    case maps:find(Username, maps:get(users_rooms, State)) of
        {ok, RoomName} ->
            RoomMessage = io_lib:format("[~s]: ~s~n", [Username, Message]),
            %% Inoke helper to identify matching users/sockets as room recipients
            %% The message author is excluded
            RoomSockets = get_room_sockets(Username, RoomName, State),
            [gen_tcp:send(Socket, RoomMessage) || Socket <- RoomSockets],
            {reply, ok, State};
        error ->
            {reply, {error, user_not_in_room}, State}
    end;

%% Get users
handle_call(get_users, _From, State) ->
    {reply, maps:keys(maps:get(users, State)), State};

%% Is user online
handle_call({is_user_online, Username}, _From, State) ->
    IsOnline = maps:is_key(Username, maps:get(users, State)),
    {reply, IsOnline, State};

%% Get rooms
handle_call(get_rooms, _From, State) ->
    {reply, maps:keys(maps:get(rooms, State)), State};

%% Unsupported
handle_call(_, _From, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {reply, unhandled, State}.

%% Add user
handle_cast({add_user, Username, Socket}, State) ->
    Users = maps:put(Username, Socket, maps:get(users, State)),
    {noreply, State#{users := Users}};

%% Remove user
handle_cast({remove_user, Username}, State) ->
    %% Update both State maps: users, users_rooms
    NewUsers = maps:remove(Username, maps:get(users, State)),
    NewUsersInRooms = maps:remove(Username, maps:get(users, State)),
    {noreply, State#{users := NewUsers, users_rooms := NewUsersInRooms}};

%% Send a server notification to a room
handle_cast({notify_room, RoomName, Notification}, State) ->
    RoomSockets = get_room_sockets("", RoomName, State),
    [gen_tcp:send(Socket, "[*]: " ++ Notification) || Socket <- RoomSockets],
    {noreply, State};

%% Unsupported
handle_cast(_, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {noreply, State}.

%% Helper for leave room, reused in switching rooms
leave_room_internal(Username, RoomName, UsersInRooms) ->
    Notification = io_lib:format(text:txt(user_arg_left_room), [Username, RoomName]),
    gen_server:cast(?SERVER, {notify_room, RoomName, Notification}),
    maps:remove(Username, UsersInRooms).

%% Helper for join room
join_room_internal(RoomName, Username, UsersInRooms) ->
    Notification = io_lib:format(text:txt(user_arg_joined_room), [Username, RoomName]),
    gen_server:cast(?SERVER, {notify_room, RoomName, Notification}),
    maps:put(Username, RoomName, UsersInRooms).

%% Helper for getting sockets for a given room
get_room_sockets(WithoutUsername, RoomName, State) ->
    Users = maps:get(users, State),
    UsersInRooms = maps:get(users_rooms, State),
    [maps:get(User, Users) || {User, Room} 
        <- maps:to_list(UsersInRooms), Room =:= RoomName, User =/= WithoutUsername].
