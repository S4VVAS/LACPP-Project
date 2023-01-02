-module(comService).
-compile(export_all).

start() ->
    spawn_link(?MODULE, init, []).

init() ->
    register(comService, self()),
    loop([]).

loop(Users) ->
    receive
        {User, connected} ->
            {server, User} ! accepted,
            loop([]);
        _ -> error
    end.
