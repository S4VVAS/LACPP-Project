-module(ui).
-compile(export_all).

% Start a new client. This action creates a comService and register it locally.
%
% Example: >> ui:start()
start() ->
    register(comService, spawn(comService, init, [])).

% ------------ INTERFACE -----------------
% After executing start() all methods below can be used without thinking about
% the backend.

% Adds data to the database
%
% Example: >> ui:add("hello world")
add(Data) ->
    comService ! {self(), add_local, term_to_binary(Data)},
    receive
        {Tree, added} -> {Tree, added};
        _  -> error
    after
        1000 -> timeout
    end.
