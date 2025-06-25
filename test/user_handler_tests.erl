-module(user_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test parse
parse_test() ->
    [
        ?assertEqual({"/help", ""}, user_handler:parse(<<"/help">>)),
        ?assertEqual({"/help", ""}, user_handler:parse(<<"/help ">>)),
        ?assertEqual({"/create", "first"}, user_handler:parse(<<"/create first">>)),
        ?assertEqual({"/create", "first"}, user_handler:parse(<<"/create first third">>)),
        ?assertEqual({"/destroy", "second"}, user_handler:parse(<<"/destroy   second">>)),
        ?assertEqual({"/send", ""}, user_handler:parse(<<"">>))
    ].

%% Test sanitize_message
sanitize_message_test() ->
    [
        ?_assertEqual("message body", user_handler:sanitize_message(<<"/whisper bob message body">>)),
        ?_assertEqual("multiple   spaces used", user_handler:sanitize_message(<<"/command user   multiple   spaces used">>)),
        ?_assertEqual("more content", user_handler:sanitize_message(<<"   /send  user  more content   ">>)),
        ?_assertEqual("", user_handler:sanitize_message(<<"">>)),
        ?_assertEqual("/help", user_handler:sanitize_message(<<"/help">>))
    ].