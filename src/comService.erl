-module(comService).
-compile(export_all).

% TODO: Change 'mainFrame@pop-os' to something static
init() ->
    % Disabled for now
    %% net_kernel:connect_node('nameServer@pop-os'),
    %% {nameServer, 'nameServer@pop-os'} ! {node(), connected},
    DB = spawn_link(fileHandler, init, []),
    loop(DB).

loop(DB) ->
    Self = node(),
    receive
        % add operation coming from the UI
        {Sender, add_local, Data} ->
            DB ! {add_local, Data},
                receive
                    {Tree, added_local} -> Sender ! {Tree, added}
                after
                    1000 -> timeout
                end,
            loop(DB);
        % add operation coming from another client
        {Sender, add_global, Hash_tree, Add_hash} ->
            DB ! {add_global, Hash_tree, Add_hash},
            loop(DB);
        {Self, accepted} ->
            io:format("You are now connected"),
            loop(DB);
        _ -> ok
    end.
