-module(server).
-export([start/1, stop/1]).

start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handler/2).

handler(S, {join, Ch, Client}) ->
    case lists:member(Ch, S) of
        true ->
            Result = genserver:request(list_to_atom(Ch), {join, Client}),
            case Result of
                joined ->
                    {reply, joined, S};
                failed ->
                    {reply, failed, S}
            end;
        false ->
            genserver:start(list_to_atom(Ch), [Client], fun channel/2),
            {reply, joined, [Ch | S]}
    end;

handler(S, kill_channels) ->
    lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, S),
    {reply, ok, []}.

channel(Clients, {join, Client}) ->
    case lists:member(Client, Clients) of
        true ->
            {reply, failed, Clients};
        false ->
            {reply, joined, [Client | Clients]}
    end;

channel(Clients, {leave, Client}) ->
    case lists:member(Client, Clients) of
        true ->
            NewClients = lists:delete(Client, Clients),
            {reply, ok, NewClients};
        false ->
            {reply, never_joined, [Clients]}
    end;

channel(Clients, {message, Channel, Nick, Msg, From}) ->
    case lists:member(From, Clients) of
        true ->
            spawn(fun() -> lists:foreach(
                fun(Pid) ->
                    case Pid == From of
                        true -> ok;
                        false -> genserver:request(Pid, {message_receive, Channel, Nick, Msg})
                    end
                end,
                Clients) end),
            {reply, ok, Clients};
        false ->
            {reply, failed, Clients}
    end.

stop(ServerAtom) ->
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).
