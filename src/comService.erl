-module(comService).
-compile(export_all).

% TODO: Change 'mainFrame@pop-os' to something static
init(UI) ->
    % Disabled for now
    net_kernel:connect_node('nameServer@pop-os'),
    {nameServer, 'nameServer@pop-os'} ! {node(), connected},
    DB = spawn_link(fileHandler, init, []),
    loop(DB, UI).

loop(DB, UI) ->
    Self = node(),
    receive
        % add operation coming from the UI
        {Sender, add_local, Data} ->
            DB ! {add_local, Data},
            receive
                % Send reply to UI and tell all other nodes about the new hash and root (for validation)
                {added_succ, Hash, Root} ->
                    Sender ! {added, Root},
                    [{comService, User} ! {add_global, Hash, Root} || User <- nodes()]
            after
                1000 -> Sender ! timeout
            end,
            loop(DB, UI);
        % add operation coming from another client
        {Sender, add_global, Hash, Root} ->
            DB ! {add_global, Hash, Root},
            receive
                % TODO: Also tell sender about failure
                {global_added_succ, _Hash, _Root} ->
                    UI ! global_added_succ;
                global_added_failed ->
                    UI ! global_added_failed
            after
                1000 -> timeout
            end,
            loop(DB, UI);
        {Self, accepted} ->
            io:format("You are now connected ~n"),
            loop(DB, UI);
        _ -> UI ! error
    end.
