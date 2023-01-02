-module(nodeClient).
-compile(export_all).

start() ->
    register(server, spawn(nodeServer, init, [])).

add(Data) ->
    server ! {add_local, Data},
    receive
        {add, ok} -> added;
        _  -> error
    end.
