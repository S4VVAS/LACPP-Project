-module(comService).
-compile(export_all).

% TODO: Change 'mainFrame@pop-os' to something static
init(UI) ->
    % Disabled for now
    net_kernel:connect_node('nameServer@pop-os'),
    {nameServer, 'nameServer@pop-os'} ! {node(), connected},
    DB = spawn_link(fileHandler, init, []),
    receive_db(DB), % Receive db from other nodes
    loop(DB, UI).

loop(DB, UI) ->
    Self = node(),
    receive
        %
        {db_request, User} ->
            db_request(User, DB),
            loop(DB, UI);
        % Receive tree from other client
        {db_received, Tree, UI} ->
            DB ! {db_received, Tree},
            UI ! received_db,
            loop(DB, UI);
        % add operation coming from the UI
        {add_local, Sender, File, Data} ->
            add_local(Sender, DB, File, Data),
            loop(DB, UI);
        % add operation coming from another client
        {add_global, Hash, Root} ->
            add_global(DB, UI, Hash, Root),
            loop(DB, UI);
        {view_local, Sender, File} ->
            view_local(Sender, DB, File),
            loop(DB, UI);
        {view_global, User, Hash} ->
            view_global(User, DB, Hash),
            loop(DB, UI);
        {Self, accepted} ->
            io:format("You are now connected ~n"),
            loop(DB, UI);
        _ -> UI ! error
    end.

% --------- Sync the db when joinging the cluster ---------

% TODO Doesn't work apperantly

% Ask to receive db from other nodes
receive_db(DB) ->
    [{comService, User} ! {db_request, node()} || User <- nodes()],
    receive
        % TODO: Here we will just receive the first tree and assume it is correct!
        {db_received, Tree} ->
            DB ! {db_received, Tree}
    after
        1500 -> timeout
    end.

%
db_request(User, DB) ->
    DB ! db_deliver,
    receive
        {db_deliver_reply, Tree} ->
            {comService, User} ! {db_received, Tree}
    after
        1000 -> timeout
    end.

% ---------- Concurrent operations to the db

% Add an item from a local client
add_local(Sender, DB, File, Data) ->
    DB ! {add_local, File, Data},
    receive
        % After added, send reply to local client and tell all other clients
        % about the new hash and root (for validation)
        {added_succ, Hash, Root} ->
            Sender ! {added, Root},
            [{comService, User} ! {add_global, Hash, Root} || User <- nodes()]
    after
        1000 -> Sender ! timeout
    end.

% Add an item from another client
add_global(DB, UI, Hash, Root) ->
    DB ! {add_global, Hash, Root},
    receive
        {global_added_succ, _Hash, _Root} ->
            UI ! global_added_succ;
        global_added_failed ->
            UI ! global_added_failed
    after
        1000 -> timeout
    end.

view_local(Sender, DB, File) ->
    DB ! {view_local, File},
    receive
        {view_local_succ, Data} ->
            Sender ! {view_succ, Data};
        {view_local_fail, Hash} ->
            [{comService, User} ! {view_global, node(), Hash} || User <- nodes()],
            receive
                {view_succ_return, Data} ->
                    Sender ! {view_succ, Data}
            after
                1500 -> timeout
            end
    after
        2000 -> Sender ! timeout
    end.

view_global(User, DB, File) ->
    DB ! {view_global, File},
    receive
        {view_global_succ, Data} ->
            {comService, User} ! {view_succ_return, Data}
    after
        1000 -> timeout
    end.
