-module(fileHandler).
-compile(export_all).

%____________________LOOP__________________

init() ->
    loop(merkelTree:empty(), orddict:new()).

loop(Tree, Storage) ->
    receive
        % Deliver tree to other client
        db_deliver ->
            comService ! {db_deliver_reply, Tree},
            loop(Tree, Storage);
        % Receive tree from other client
        {db_received, New_tree} ->
            New_size = bit_size(merkleTree:get_hash(New_tree)),
            Prev_size = bit_size(merkleTree:get_hash(Tree)),
            if New_size > Prev_size -> loop(New_tree, Storage);
            true -> loop(Tree, Storage)
            end;
        {add_local, File, Data} ->
            Hash = crypto:hash(sha256, File), % Encrypt data
            {Root, New_tree} = merkelTree:add(Hash, Tree),
            comService ! {added_succ, Hash, Root}, % Send the hash and root back to comService to pass on to other clients
            loop(New_tree, orddict:store(Hash, Data, Storage)); % Store data in local client
        {add_global, Hash, Received_root} ->
            {Root, New_tree} = merkelTree:add(Hash, Tree),
            % Validation
            if Root =:= Received_root ->
                comService ! {global_added_succ, Hash, Root},
                loop(New_tree, Storage);
            true ->
                comService ! global_added_failed,
                loop(Tree, Storage)
            end;
        {remove_local, Data} ->
            ok;
        {remove_global, Data, Hash} ->
            ok;
        {view_local, File} ->
            Hash = crypto:hash(sha256, File),
            case orddict:find(Hash, Storage) of
                {ok, Value} -> comService ! {view_local_succ, Value};
                error -> comService ! {view_local_fail, Hash}
            end,
            loop(Tree, Storage);
        {view_global, Hash} ->
            case orddict:find(Hash, Storage) of
                {ok, Value} -> comService ! {view_global_succ, Value};
                error -> ok % We don't need to propagate this
            end,
            loop(Tree, Storage);
        {count} ->
            ok;
        _ -> loop(Tree, Storage)
    end.
