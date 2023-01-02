-module(fileHandler).
-compile(export_all).

%____________________LOOP__________________

init() ->
    loop(orddice:new(), gb_trees:empty()).

loop(Files, Tree) ->
    receive
        {add_local, Data} ->
            ok;
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
        _ -> loop(Files, Tree)
    end.
