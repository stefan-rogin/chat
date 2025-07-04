-module(text).
-export([txt/1]).

-define(TXT, #{
    login => "Login:",
    username_not_valid => "Invalid username.\n",
    username_not_offline => "You're already logged in from a different client, disconnecting.\n",
    help =>
        "\n"
        "Available commands:\n"
        "/rooms                      List existing rooms (without private ones where you are not a member).\n"
        "/create <Room>             Create free access room.\n"
        "/create_private <Room>      Create private room.\n"
        "/join <Room>                Join room.\n"
        "/invite <User>              Add user to private room members (requires ownership of private room).\n"
        "/leave                      Leave currently joined room.\n"
        "/destroy <Room>             Destroy owned room.\n"
        "/users                      List online users.\n"
        "/whisper <User> <Message>   Send private message to an user.\n"
        "/quit                       Leave server.\n"
        "/help                       This.\n"
        "\n"
        "Join a room to send messages to its members.\n",
    online_users => "Online users: ",
    rooms => "Rooms: ",
    no_rooms => "There are no rooms yet. Be the first to make one. :)",
    room_arg_created => "Room ~s created.",
    room_arg_destroyed => "Room ~s destroyed.",
    room_not_available => "A room with this name already exists. Please choose a different name.",
    room_not_owned => "You can only destroy rooms created by you.",
    room_not_present => "There is no room with this name.",
    bye => "Bye.",
    default =>
        "Incomplete command or message sent without joining a room first.\n"
        "Type /help to see available commands.",
    welcome_arg => "Welcome, ~s.~nType /help to see available commands.",
    user_arg_joined_room => "User ~s joined ~s.",
    user_arg_left_room => "User ~s left ~s.",
    user_left_room => "You left room ~s.",
    room_joined_same => "You are already in this room.",
    user_not_in_room => "You are already in the lobby.",
    room_destroyed => "This room is being destroyed by its owner.",
    user_not_online => "This user is not online. Use /users to see online users.",
    room_not_joined_for_invite => "Join a private room you own to invite members.",
    user_already_private_member => "User is already a member.",
    room_not_private_or_not_owner => "Cannot invite member, it needs to be private and owned by you.",
    member_invited => "User added to private room members.",
    invited_arg_room => "I invited you to my private room ~s."
}).

txt(Key) -> 
    case maps:find(Key, ?TXT) of
        {ok, Fragment} -> Fragment;
        error ->
            io:format("Missing text fragment for key ~s.~n", [Key]), 
            ""
    end.