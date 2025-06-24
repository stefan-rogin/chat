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
        ?assertEqual({"", ""}, user_handler:parse(<<"">>))
    ].
