-module(chat_server).
-behavior(gen_server).

-record(state, {users}).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    spawn(fun() -> acceptor(ListenSocket) end),
    {ok, #state{users = []}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, State) ->
    {noreply, State}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).

handle(Socket) ->
    receive
        {tcp, Socket, <<"user:", User/binary>>} ->
            gen_tcp:send(Socket, "Logged in: " ++ User),
            handle(Socket);        
        {tcp, Socket, Unknown} ->
            gen_tcp:send(Socket, "Unknown command: " ++ Unknown),
            handle(Socket);
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket)
    end.
