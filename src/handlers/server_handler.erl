-module(server_handler).

-export([handle_cast/2, handle_call/3]).

%% Create room
handle_call({create_room, RoomName, Username}, _From, State) ->
    Rooms = maps:get(rooms, State),
    case maps:is_key(RoomName, Rooms) of
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
            NewRooms = maps:remove(RoomName, Rooms),
            {reply, ok, State#{rooms := NewRooms}};
        {ok, _OtherUsername} ->
            {reply, {error, room_not_owned}, State};
        error ->
            {reply, {error, room_not_present}, State}
    end;

%% Join room
handle_call({join_room, RoomName, Username}, _From, State) ->
    case maps:is_key(RoomName, maps:get(rooms, State)) of
        true ->
            %% User can either be in neither, another or same room
            UsersInRooms = maps:get(users_rooms, State),
            case maps:find(Username, UsersInRooms) of
                {ok, RoomName} ->
                    {reply, {error, room_joined_same}, State};
                _ ->
                    NewUsersInRooms = maps:put(Username, RoomName, UsersInRooms),
                    {reply, ok, State#{users_rooms := NewUsersInRooms}}
            end;
        false ->
            {reply, {error, room_not_present}, State}
    end;

%% Leave room
handle_call({leave_room, Username}, _From, State) ->
    case maps:take(Username, maps:get(users_rooms, State)) of
        {RoomName, NewUsersInRooms} ->
            {reply, RoomName, State#{users_rooms := NewUsersInRooms}};
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
    Users = maps:remove(Username, maps:get(users, State)),
    {noreply, State#{users := Users}};

%% Unsupported
handle_cast(_, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {noreply, State}.