-module(login_handler).
-export([handle_login/1]).

handle_login(Socket) ->
    %% Prompt user to authenticate
    gen_tcp:send(Socket, text:txt(login)),

    %% TODO: Fix nesting
    case gen_tcp:recv(Socket, 0) of
        {ok, UsernameBin} ->
            %% Match reply to a username-like or drop connection
            case re:run(UsernameBin, "^[A-Za-z0-9_]+", [{capture, first, list}]) of
                {match, [Username]} ->
                    %% Disallow multiple connections from the same user
                    case chat_server:is_user_online(Username) of
                        true ->
                            gen_tcp:send(Socket, text:txt(username_not_offline)),
                            gen_tcp:close(Socket);
                        false ->
                            io:format("User connected: ~s~n", [Username]),
                            %% Start a dedicated process for the new user
                            {ok, Pid} = chat_user:start_link(Username, Socket),
                            %% Pass socket ownership to the spawned process
                            ok = gen_tcp:controlling_process(Socket, Pid)
                    end;
                nomatch ->
                    gen_tcp:send(Socket, text:txt(username_not_valid)),
                    gen_tcp:close(Socket)
            end;
        _ ->
            gen_tcp:close(Socket)
    end.