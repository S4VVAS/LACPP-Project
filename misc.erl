-module(misc).

-export([compute_hid/2, hash/1, timedout/2]).

compute_hid(Pid, Id) when is_integer(Id) ->
    Bin = term_to_binary(Pid),
    hash(<<Bin/binary, Id>>).

hash(Bin) ->
    crypto:hash(sha256, Bin).

timedout({_, OldSecs, _}, Timeout) ->
    {_, CurSecs, _} = os:timestamp(),
    Timeout < abs(CurSecs - OldSecs).
