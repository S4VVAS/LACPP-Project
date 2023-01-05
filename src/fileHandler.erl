-module(fileHandler).
-compile(export_all).

%____________________LOOP__________________

init() ->
    loop(merkelTree:empty(), orddict:new()).

loop(Tree, Storage) ->
    receive
        {init_tree, New_tree} ->
            loop(New_tree, Storage);
        {add_local, Data} ->
            Hash = crypto:hash(sha256, Data),
            {Root, New_tree} = merkelTree:add(Hash, Tree),
            comService ! {added_succ, Hash, Root}, % Send the hash and root back to comService to pass on to other nodes
            loop(New_tree, orddict:store(Hash, Data, Storage)); % Store the newly added data in this node
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
        {view, Data} ->
            ok;
        {count} ->
            ok;
        _ -> loop(Tree, Storage)
    end.
