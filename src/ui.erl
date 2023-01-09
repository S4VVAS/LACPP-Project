-module(ui).
-compile(export_all).
% ------------------------
% How to use:
% 1. Start a terminal:
%  a. $erl -sname nameServer
%  b. > nameServer:start().
% 2. Start N amounts of terminals:
%  a. $erl -sname some_name
%  b. > ui:start().
% 3. Add nodes by: > ui:add("something")
% ------------------------

% Start a new client. This action creates a comService and register it locally and connects to the cluster.
%
% Example: >> ui:start()
start() ->
    Response = spawn_link(?MODULE, response_loop, []),
    register(comService, spawn(comService, init, [Response])).

% ------------ INTERFACE -----------------
% After executing start() all methods below can be used without thinking about
% the backend.

% Adds data to the database
%
% Example: >> ui:add(file1, "hello world")
add(File, Data) ->
    comService ! {add_local, self(), list_to_binary(File), list_to_binary(Data)},
    receive
        {added, Root} -> {added, bit_size(Root)}; % TODO: For testing only, to see the root size actually changes
        _  -> timeout_from_db % Means the fileHandler failed to communicate
    after
        2000 -> timeout_from_comService % Means the comService failed to communicate
    end.


% TODO: Known error, even if node doesnt exist in users merkleTree it can be fetched from
% other nodes whos merkleTree is not synced.
view(File) ->
    comService ! {view_local, self(), list_to_binary(File)},
    receive
        {view_succ, Data} -> {found, binary_to_list(Data)};
        {no_such_file, _File} -> no_such_file;
        _  -> timeout_from_db
    after
        2500 -> timeout_from_comService
    end.

% A response look to send messages to the user
response_loop() ->
    receive
        global_added_succ -> io:format("Got successful add from other node!~n");
        global_added_failed -> io:format("Validation failed during add from other node!~n");
        error -> io:format("Error from comService!~n");
        received_db -> io:format("DB sync complete!~n");
        view_local_fail -> io:format("View local fail, fetching from other nodes!~n")
    end,
    response_loop().
