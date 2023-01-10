-module(misc).

-export([compute_hid/2, hash/1, timedout/2]).

compute_hid(Alias, Id) when is_atom(Alias) andalso is_integer(Id) ->
    Bin = atom_to_binary(Alias),
    hash(<<Bin/binary, Id>>).

hash(Bin) ->
    crypto:hash(sha256, Bin).

timedout({_, OldSecs, _}, Timeout) ->
    {_, CurSecs, _} = os:timestamp(),
    Timeout < abs(CurSecs - OldSecs).
