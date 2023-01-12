-module(misc).

-export([compute_hid/2, hash/1, timedout/2, convert/1]).

compute_hid(Pid, Id) when is_integer(Id) ->
    Bin = term_to_binary(Pid),
    hash(<<Bin/binary, Id>>).

hash(Bin) ->
    crypto:hash(sha256, Bin).

timedout({_, OldSecs, _}, Timeout) ->
    {_, CurSecs, _} = os:timestamp(),
    Timeout < abs(CurSecs - OldSecs).

% Convert supported non-binary data to binary data
convert(Data) ->
    if is_binary(Data) ->
            Data;
       true -> if is_list(Data) ->
                       list_to_binary(Data);
                  true -> if is_number(Data) ->
                               term_to_binary(Data);
                          true -> data_type_unsupported
                        end
                  end
            end.
