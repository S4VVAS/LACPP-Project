
-module(filehandler).

-import(filestore,[addChunk/2]).

%____________________LISENER/LOOP__________________

listen(FileKeys,OtherMachines) -> 
    receive
        {init, Keys, OtherMachinez} ->
            listen(Keys,OtherMachinez);
        {fetch, RequestR, Key} ->
            RequestR ! fetchHere(Key);
        {remove, RequestR, Key} ->
            RequestR ! deleteHere(Key);
        {add, RequestR, Key, ONr ,Chunk} ->
            RequestR ! addHere(Key, ONr ,Chunk)
    end.


%_________________HELPER_METHODS_________________


add(File, NChunks) ->
    %For nChunks break file, allocate to n Machines
    ok.

addHere(Key, ONr, Chunk) ->
     %For nChunks break file, allocate to n Machines
     ok.

deleteHere(Key) ->
    ok.

fetchHere(Key) ->
    ok.

%_________________INTERFACE_METHODS______________

count() -> 
    ok.

add(Key, File) ->
    ok.

remove(Key) ->
    ok.

get(Key) -> 
    ok.
