-module(filestore). 
-export([addChunk/2]).

addChunk(Key, Chunk) -> 
   {ok, Fd} = file:open("Newfile.txt", [append]), 
   file:write(Fd,"New Line").