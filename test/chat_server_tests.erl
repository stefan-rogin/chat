-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").

chat_server_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun test_add_user/0,
        fun test_remove_user/0,
        fun test_get_users/0,
        fun test_create_room/0,
        fun test_destroy_owned_room/0,
        fun test_destroy_unowned_room_fail/0,
        fun test_duplicate_room_fail/0
     ]}.

setup() ->
    catch gen_server:stop(chat_server),
    {ok, Pid} = chat_server:start(0),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid).

test_add_user() ->
    chat_server:add_user("one", socket),
    timer:sleep(10),
    ?assertEqual(["one"], chat_server:get_users()),
    ?assertEqual(true, chat_server:is_user_online("one")).

test_remove_user() ->
    chat_server:add_user("one", socket),
    timer:sleep(10),
    ?assertEqual(true, chat_server:is_user_online("one")),
    chat_server:remove_user("one"),
    timer:sleep(10),
    ?assertEqual(false, chat_server:is_user_online("one")).

test_get_users() ->
    chat_server:add_user("one", socket),
    chat_server:add_user("two", socket),
    timer:sleep(10),
    Users = chat_server:get_users(),
    ?assert(lists:member("one", Users)),
    ?assert(lists:member("two", Users)).

test_create_room() ->
    ok = chat_server:create_room("first", "one"),
    ?assertEqual(["first"], chat_server:get_rooms()).

test_destroy_owned_room() ->
    ok = chat_server:create_room("first", "one"),
    ?assertEqual(["first"], chat_server:get_rooms()),
    ok = chat_server:destroy_room("first", "one"),
    ?assertEqual([], chat_server:get_rooms()).

test_destroy_unowned_room_fail() ->
    ok = chat_server:create_room("first", "one"),
    {error, room_not_owned} = chat_server:destroy_room("first", "two").

test_duplicate_room_fail() ->
    ok = chat_server:create_room("first", "one"),
    {error, room_not_available} = chat_server:create_room("first", "two").


