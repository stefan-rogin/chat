-module(server_handler).

-include_lib("../include/records.hrl").

-export([handle_cast/2, handle_call/3]).

-define(SERVER, chat_server).

%% Create room
handle_call({create_room, RoomName, Username, IsPrivate}, _From, State) ->
    Rooms = State#state.rooms,
    case maps:is_key(RoomName, Rooms) of
        %% Don't allow duplicate rooms
        true ->
            {reply, {error, room_not_available}, State};
        false ->
            NewRoom = #room{
                owner = Username, 
                isPrivate = IsPrivate, 
                members = [Username]},
            NewRooms = maps:put(RoomName, NewRoom, Rooms),
            {reply, ok, State#state{rooms = NewRooms}}
    end;

%% Destroy room
handle_call({destroy_room, RoomName, Username}, _From, State) ->
    Rooms = State#state.rooms,
    case maps:find(RoomName, Rooms) of
        {ok, Room} when Room#room.owner =:= Username ->
            %% TODO: Notify room members about destruction
            %% Remove from users_rooms entries with value of deleted room
            NewUsersInRooms = maps:filter(
                fun(_, R) -> R =/= RoomName end, 
                State#state.users_rooms),
            NewRooms = maps:remove(RoomName, Rooms),
            {reply, ok, State#state{
                rooms = NewRooms, 
                users_rooms = NewUsersInRooms}};
        {ok, _OtherRoom} ->
            {reply, {error, room_not_owned}, State};
        error ->
            {reply, {error, room_not_present}, State}
    end;

%% Join room
handle_call({join_room, RoomName, Username}, _From, State) ->
    %% Hold state in an user => room map
    UsersInRooms = State#state.users_rooms,
    %% Room access is resolved by chat_server:get_rooms
    case lists:member(RoomName, get_visible_rooms(Username, State#state.rooms)) of
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
                    {reply, ok, State#state{users_rooms = NewUsersInRooms}};
                error ->
                    %% User not in any room already, just join
                    NewUsersInRooms = join_room_internal(RoomName, Username, UsersInRooms),
                    {reply, ok, State#state{users_rooms = NewUsersInRooms}}
            end
    end;

%% Add member to owned private room
handle_call({invite_user, Username, TargetUsername}, _From, State) ->
    %% Get the room where the user making the invite is in
    case maps:find(Username, State#state.users_rooms) of
        {ok, RoomName} -> 
            %% Check if user is owner and target is not a member already 
            case can_invite(Username, TargetUsername, RoomName, State) of
                {ok, NewRooms} ->
                    {reply, {ok, RoomName}, State#state{rooms = NewRooms}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error -> 
            {reply, {error, room_not_joined_for_invite}, State}
    end; 

%% Leave room
handle_call({leave_room, Username}, _From, State) ->
    UsersInRooms = State#state.users_rooms,
    case maps:find(Username, UsersInRooms) of
        {ok, RoomName} ->
            %% Invoke helper to notify room about user exit, 
            %% then prepare state changes for State#{users_rooms} 
            NewUsersInRooms = leave_room_internal(Username, RoomName, UsersInRooms),
            {reply, RoomName, State#state{users_rooms = NewUsersInRooms}};
        error ->
            {reply, {error, user_not_in_room}, State}
    end;

%% Get users
handle_call(get_users, _From, State) ->
    {reply, maps:keys(State#state.users), State};

%% Is user online
handle_call({is_user_online, Username}, _From, State) ->
    IsOnline = maps:is_key(Username, State#state.users),
    {reply, IsOnline, State};

%% Get rooms
handle_call({get_rooms, Username}, _From, State) -> 
    %% Filter out rooms where Room is private and Username is member
    VisibleRooms = get_visible_rooms(Username, State#state.rooms),
    {reply, VisibleRooms, State};

%% Send message
handle_call({send_message, Username, Message}, _From, State) ->
    case maps:find(Username, State#state.users_rooms) of
        {ok, RoomName} ->
            RoomMessage = io_lib:format("[~s]: ~s~n", [Username, Message]),
            %% Invoke helper to identify matching users/sockets as room recipients
            %% The message author is excluded
            RoomSockets = get_room_sockets(Username, RoomName, State),
            [gen_tcp:send(Socket, RoomMessage) || Socket <- RoomSockets],
            {reply, ok, State};
        error ->
            {reply, {error, user_not_in_room}, State}
    end;

%% Whisper message
handle_call({whisper_message, Username, TargetUsername, Message}, _From, State) ->
    case maps:find(TargetUsername, State#state.users) of
        {ok, Socket} ->
            Whisper = io_lib:format("<<~s>>: ~s~n", [Username, Message]),
            gen_tcp:send(Socket, Whisper),
            {reply, ok, State};
        error ->
            {reply, {error, user_not_online}, State}
    end;

%% Unsupported
handle_call(_, _From, State) ->
    io:format("Unsupported: ~s, ~s~n", [?MODULE, ?FUNCTION_NAME]),
    {reply, unhandled, State}.

%% Add user
handle_cast({add_user, Username, Socket}, State) ->
    NewUsers = maps:put(Username, Socket, State#state.users),
    {noreply, State#state{users = NewUsers}};

%% Remove user
handle_cast({remove_user, Username}, State) ->
    %% Update both State maps: users, users_rooms
    %% If user is in room, notify members about exit
    UsersInRooms = State#state.users_rooms,
    case maps:find(Username, UsersInRooms) of
        {ok, RoomName} ->
            NewUsersInRooms = leave_room_internal(Username, RoomName, UsersInRooms);
        error ->
            NewUsersInRooms = UsersInRooms
    end,
    NewUsers = maps:remove(Username, State#state.users),
    {noreply, State#state{
        users = NewUsers, 
        users_rooms = NewUsersInRooms}};

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
    Users = State#state.users,
    UsersInRooms = State#state.users_rooms,
    [maps:get(User, Users) || {User, Room} 
        <- maps:to_list(UsersInRooms), Room =:= RoomName, User =/= WithoutUsername].

%% Helper for getting visible rooms, according to permissions
%% Filter out rooms where Room is private and Username is member
get_visible_rooms(Username, Rooms) ->
    VisibleRooms = maps:filter(
        fun(_RoomName, #room{isPrivate = true, members = Members}) -> 
                lists:member(Username, Members);
            (_RoomName, #room{isPrivate = false}) ->
                true 
        end, 
        Rooms),
    maps:keys(VisibleRooms).

%% Helper for adding member to a private room, 
%% if criteria are met: room is private, invite is 
%% sent by owner and target user is not a member 
can_invite(Username, TargetUsername, RoomName, State) ->
    Users = State#state.users,
    case maps:is_key(TargetUsername, Users) of
        false ->
            {error, user_not_online};
        true ->
            case maps:find(RoomName, State#state.rooms) of
                {ok, #room{isPrivate = true, owner = Username, members = Members} = Room} ->
                    case lists:member(TargetUsername, Members) of
                        true -> {error, user_already_private_member};
                        false ->
                            NewMembers = [TargetUsername | Members],
                            NewRoom = Room#room{members = NewMembers},
                            UpdatedRooms = maps:put(RoomName, NewRoom, State#state.rooms),
                            {ok, UpdatedRooms}
                    end;
                {ok, _Other} ->
                    {error, room_not_private_or_not_owner};
                error ->
                    {error, room_not_found}
            end
    end.