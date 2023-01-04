-module(nameServer).
-compile(export_all).

start() ->
    spawn_link(?MODULE, init, []).

init() ->
    register(nameServer, self()),
    loop([]).

loop(Users) ->
    receive
        {User, connected} ->
            {comService, User} ! {User, accepted},
            loop([]);
        _ -> error
    end.
