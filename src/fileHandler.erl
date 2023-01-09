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
            {Root, New_tree} = merkelTree:add(File, Hash, Tree),
            comService ! {added_succ, File, Hash, Root}, % Send the hash and root back to comService to pass on to other clients
            loop(New_tree, orddict:store(File, Data, Storage)); % Store data in local client
        {add_global, File, Hash, Received_root} ->
            {Root, New_tree} = merkelTree:add(File, Hash, Tree),
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
            case orddict:find(File, Storage) of
                {ok, Value} -> comService ! {view_local_succ, Value};
                error -> Exists = merkelTree:find_file(File, Tree),
                         if Exists == exists ->
                                 comService ! {view_local_fail, File};
                            true  -> comService ! {no_such_file, File}
                        end
            end,
            loop(Tree, Storage);
        {view_global, File} ->
            case orddict:find(File, Storage) of
                {ok, Value} -> comService ! {view_global_succ, Value};
                error -> ok % We don't need to propagate this
            end,
            loop(Tree, Storage);
        {count} ->
            ok;
        _ -> loop(Tree, Storage)
    end.
