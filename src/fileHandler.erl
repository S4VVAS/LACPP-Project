-module(fileHandler).
-compile(export_all).

%____________________LOOP__________________

init() ->
    loop(merkelTree:empty()).

loop(Tree) ->
    receive
        {init_tree, New_tree} ->
            loop(New_tree);
        {add_local, Data} ->
            New_tree = merkelTree:add(Data, Tree),
            comService ! {New_tree, added_local},
            loop(New_tree);
        {add_global, Data, Hash} ->
            ok;
        {remove_local, Data} ->
            ok;
        {remove_global, Data, Hash} ->
            ok;
        {view, Data} ->
            ok;
        {count} ->
            ok;
        _ -> loop(Tree)
    end.
