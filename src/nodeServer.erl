-module(nodeServer).
-compile(export_all).

% TODO: Change 'mainFrame@pop-os' to something static
init() ->
    net_kernel:connect_node('comService@pop-os'),
    {comService, 'comService@pop-os'} ! {node(), connected},
    DB = spawn_link(fileHandler, init, []),
    loop(DB).

loop(DB) ->
    receive
        {Sender, add, Data} ->
            DB ! {add_local, Data},
            loop(DB);c
        accepted ->
            io:format("You are now connected"),
            loop(DB);
        _ -> ok
    end.
