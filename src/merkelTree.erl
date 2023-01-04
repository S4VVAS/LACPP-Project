-module(merkelTree).
-compile(export_all).

% tree() -> {bit-string, bit-string, merkleTree, merkleTree}
% which represents {hash, data, left, right}

empty() ->
    {nil, nil, nil, nil}.

add(Data, Tree) ->
    case Tree of
        % Adding to empty tree
        {nil, nil, nil, nil} ->
            create_node(Data);
        % Adding to tree with only a root
        {Hash1, _Data1, nil, nil} ->
            {Hash2, Data2, nil, nil} = create_node(Data),
            % Convert bit-strings to int for merging
            MergedHash = <<Hash1/binary, Hash2/binary>>,
            % Create new tree
            {MergedHash, nil, Tree, {Hash2, Data2, nil, nil}};
        % Adding to a tree with two branches
        {_HashRoot, nil, L, R} ->
            % Add node to the branch with the smallest hash
            L_hash = get_hash(L),
            R_hash = get_hash(R),
            if
                bit_size(L_hash) < bit_size(R_hash) ->
                    New_node = add(Data, L),
                    New_hash = get_hash(New_node),
                    MergedHash = <<New_hash/binary, R_hash/binary>>,
                    {MergedHash, nil, New_node, R};
               true ->
                    New_node = add(Data, R),
                    New_hash = get_hash(New_node),
                    MergedHash = <<L_hash/binary, New_hash/binary>>,
                    {MergedHash, nil, L, New_node}
            end;
        _ ->
            error
    end.

% Create a tree node from a piece of data
create_node(Data) ->
    {crypto:hash(sha256, Data), Data, nil, nil}.

% Get hash from a tree node
get_hash({nil, nil, nil, nil}) ->
    term_to_binary(0);
get_hash({Hash, _data, _l, _r}) ->
    Hash.
