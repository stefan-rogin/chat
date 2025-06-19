-module(echo).
-export([start_server/1]).

start_server(Port) ->
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, true}]),
        spawn(fun() -> acceptor(Listen) end),
        timer:sleep(infinity)
    end ),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).

handle(Socket) ->
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            handle(Socket)
    end.
