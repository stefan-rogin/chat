-module(user_handler_tests).
-include_lib("eunit/include/eunit.hrl").

% Test parse/1
parse_test() ->
    [
        %% Command only nominal
        ?assertEqual({"/help", ""}, user_handler:parse(<<"/help">>)),
        %% Trim
        ?assertEqual({"/help", ""}, user_handler:parse(<<"/help ">>)),
        %% Command + arg nominal 
        ?assertEqual({"/create", "first"}, user_handler:parse(<<"/create first">>)),
        %% Third arg ignored
        ?assertEqual({"/create", "first"}, user_handler:parse(<<"/create first third">>)),
        %% Extra spaces ok
        ?assertEqual({"/destroy", "second"}, user_handler:parse(<<"/destroy   second">>)),
        %% Empty
        ?assertEqual({"/send", ""}, user_handler:parse(<<"">>))
    ].

sanitize_message_test_() ->
    [
        ?_assertEqual("message body", user_handler:sanitize_message(<<"/whisper bob message body">>)),
        ?_assertEqual("multiple   spaces used", user_handler:sanitize_message(<<"/command user   multiple   spaces used">>)),
        ?_assertEqual("more content", user_handler:sanitize_message(<<"   /send  user  more content   ">>)),
        ?_assertEqual("", user_handler:sanitize_message(<<"">>)),
        ?_assertEqual("/help", user_handler:sanitize_message(<<"/help">>))
    ].