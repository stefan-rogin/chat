-module(chat_user_tests).
-include_lib("eunit/include/eunit.hrl").

% Test parse/1
parse_test() ->
    [
        %% Command only nominal
        ?assertEqual({"/help", ""}, chat_user:parse(<<"/help">>)),
        %% Trim
        ?assertEqual({"/help", ""}, chat_user:parse(<<"/help ">>)),
        %% Command + arg nominal 
        ?assertEqual({"/create", "first"}, chat_user:parse(<<"/create first">>)),
        %% Third arg ignored
        ?assertEqual({"/create", "first"}, chat_user:parse(<<"/create first third">>)),
        %% Extra spaces ok
        ?assertEqual({"/destroy", "second"}, chat_user:parse(<<"/destroy   second">>)),
        %% Empty
        ?assertEqual({"", ""}, chat_user:parse(<<"">>))
    ].
