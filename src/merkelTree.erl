-module(merkelTree).
-compile(export_all).

% Changes
% 5 jan 2022 ->
%   Removed data part, data should not be in the tree


% tree() -> {bit-string, merkleTree, merkleTree}
% which represents {hash, left, right}

empty() ->
    {nil, nil, nil}.

% Takes the hash of the data and a previous merkelTree state
% returns: A tuple containing the root hash and merkelTree
% add :: bit-string -> merkelTree -> {bit-string, merkelTree}
add(Data, Tree) ->
    case Tree of
        % Adding to empty tree
        {nil, nil, nil} ->
            New_tree = create_node(Data),
            {Data, New_tree};
        % Adding to tree with only a root
        {Hash1, nil, nil} ->
            {Hash2, nil, nil} = create_node(Data),
            % Merged hash for new root
            Merged_hash = <<Hash1/binary, Hash2/binary>>,
            % Create new tree
            New_tree = {Merged_hash, Tree, {Hash2, nil, nil}},
            {Merged_hash, New_tree};
        % Adding to a tree with two branches
        {_HashRoot, L, R} ->
            % Add node to the branch with the smallest hash
            L_hash = get_hash(L),
            R_hash = get_hash(R),
            if
                bit_size(L_hash) < bit_size(R_hash) ->
                    {New_hash, New_node} = add(Data, L),
                    Merged_hash = <<New_hash/binary, R_hash/binary>>,
                    New_tree = {Merged_hash, New_node, R},
                    {Merged_hash, New_tree};
               true ->
                    {New_hash, New_node} = add(Data, R),
                    Merged_hash = <<L_hash/binary, New_hash/binary>>,
                    New_tree = {Merged_hash, L, New_node},
                    {Merged_hash, New_tree}
            end;
        _ ->
            error
    end.

% Create a tree node from a piece of data
create_node(Data) ->
    {Data, nil, nil}.

% Get hash from a tree node
get_hash(nil) ->
    term_to_binary(0);
get_hash({nil, nil, nil}) ->
    term_to_binary(0);
get_hash({Hash, _l, _r}) ->
    Hash.
