-module(comms).

-export([send_line/2]).

send_line(Socket, MaybeString) ->
    Line = case MaybeString of
        B when is_binary(B) -> B;
        L when is_list(L) -> list_to_binary(L);
        A when is_atom(A) -> atom_to_binary(A, utf8);
        W -> list_to_binary(io_lib:format("~p", [W]))
    end,
    gen_tcp:send(Socket, <<Line/binary, "\n">>).
