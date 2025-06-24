-module(text).
-export([txt/1]).

-define(TXT, #{
    login => "Login:",
    username_not_valid => "Invalid username.\n",
    username_not_offline => "You're already logged in from a different client, disconnecting.\n",
    help =>
        "Available commands: /users\n"
        "/rooms, /create <Room>, /join<Room>, /leave, /destroy <Room>\n"
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
    user_arg_left_room => "User ~s left ~s.~n",
    room_joined_same => "You are already in this room.\n",
    user_not_in_room => "You are already in the lobby.\n"
}).

%% TODO: Handle missing key
%% TODO: Change to line()
txt(Key) -> maps:get(Key, ?TXT).