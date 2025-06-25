-record(room, {
    owner,               % Username (string)
    isPrivate = false,   % boolean()
    members = []         % [Username] (for private rooms)
}).

-record(state, {
    users = #{},         % Username => Socket
    rooms = #{},         % RoomName => #room{}
    users_rooms = #{}    % Username => RoomName
}).