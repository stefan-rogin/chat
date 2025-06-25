-module(server_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/include/records.hrl").

get_room_sockets_test() ->
    Users = #{"one" => socket1, "two" => socket2, "three" => socket3},
    UsersRooms = #{"one" => "room1", "two" => "room1", "three" => "room2"},
    State = #state{users = Users, users_rooms = UsersRooms},
    ?assertEqual([socket2], server_handler:get_room_sockets("one", "room1", State)),
    ?assertEqual([socket1], server_handler:get_room_sockets("two", "room1", State)),
    ?assertEqual([], server_handler:get_room_sockets("three", "room2", State)),
    ?assertEqual([socket1, socket2], lists:sort(server_handler:get_room_sockets("", "room1", State))).

get_visible_rooms_test() ->
    Room1 = #room{owner = "one", isPrivate = false, members = ["one"]},
    Room2 = #room{owner = "one", isPrivate = true, members = ["one", "two"]},
    Room3 = #room{owner = "one", isPrivate = true, members = ["one"]},
    Rooms = #{"public" => Room1, "private" => Room2, "secret" => Room3},
    ?assertEqual(["private", "public", "secret"], lists:sort(server_handler:get_visible_rooms("one", Rooms))),
    ?assertEqual(["private", "public"], lists:sort(server_handler:get_visible_rooms("two", Rooms))),
    ?assertEqual(["public"], server_handler:get_visible_rooms("nobody", Rooms)).

can_invite_test() ->
    Room = #room{owner = "one", isPrivate = true, members = ["one"]},
    State = #state{
        users = #{"one" => socket1, "two" => socket2},
        rooms = #{"private" => Room}
    },
    %% Can invite if target is online and not already a member
    {ok, _} = server_handler:can_invite("one", "two", "private", State),
    %% Cannot invite if target is already a member
    State2 = State#state{rooms = #{"private" => Room#room{members = ["one", "two"]}}},
    {error, user_already_private_member} = server_handler:can_invite("one", "two", "private", State2),
    %% Cannot invite if not owner
    {error, room_not_private_or_not_owner} = server_handler:can_invite("two", "one", "private", State),
    %% Cannot invite if target not online
    {error, user_not_online} = server_handler:can_invite("one", "ghost", "private", State),
    %% Cannot invite if room not found
    {error, room_not_found} = server_handler:can_invite("one", "two", "nope", State).