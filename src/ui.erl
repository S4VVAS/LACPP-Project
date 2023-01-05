-module(ui).
-compile(export_all).

% Start a new client. This action creates a comService and register it locally.
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
% Example: >> ui:add("hello world")
add(Data) ->
    comService ! {self(), add_local, term_to_binary(Data)},
    receive
        {added, Root} -> {added, Root, bit_size(Root)};
        _  -> timeout_from_db
    after
        1000 -> timeout_from_comService
    end.

% A response look to send messages to the user
response_loop() ->
    receive
        global_added_succ -> io:format("Got successful add from other node!~n");
        global_added_failed -> io:format("Validation failed during add from other node!~n");
        error -> io:format("Error from comService!~n")
    end,
    response_loop().
