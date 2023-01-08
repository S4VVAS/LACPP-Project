-module(node).

-behaviour(gen_server).

%% gen_server behaviour exports
-export([start/1, init/1, handle_call/3, handle_cast/2]).

-export([compute_hid/2]).

-define(PREPARE_TIMEOUT, 10). %% Currently in seconds
-define(COMMIT_TIMEOUT, 30). %% Currently in seconds
-define(MAX_CHUNK, 3).
-define(DEFAULT_MEMORY, 4).

%% record definitions
-record(state,
        { alias         = undefined
        , pid           = self()
        , neighbours    = dict:new()
        , memory        = ?DEFAULT_MEMORY
        , data          = dict:new()
        , candidate     = []
        , queued        = []
        , id            = 0
        , queued_mem    = 0
        , add_info      = available
        }
       ).

-record(chunk,
        { position = 0
        , contents = <<>>
        , hash = <<>>
        }
       ).

%% We are creating the first node of our network so we just initiate it
start({create, Alias}) ->
    gen_server:start(?MODULE, {Alias}, []);
%% Otherwise, we will need to specify any endpoint of the network and connect
%% through that point. We initiate our node and then we connect to the network
%% to some level of Depth.
start({connect, Alias, To, Depth}) ->
    {ok, Pid} = gen_server:start(?MODULE, {Alias}, []),
    gen_server:cast(Pid, {connect, Depth, To}),
    {ok, Pid}.


%% Initiate a node
init({Alias}) ->
    {ok, #state{alias = Alias}}.



%% CALL DEFINITION AND BINDINGS
handle_call({register, Alias, Pid}, _From, State) ->
    do_register(Alias, Pid, State);
handle_call(print, _From, State) ->
    {reply, {ok, dict:fetch_keys(State#state.data)}, State};
handle_call({add, RequiredSize}, {UIPid, _}, State) ->
    start_add(UIPid, RequiredSize, State);
handle_call(_Req, _From, #state{} = State) ->
    {reply, ok, State}.



%% CAST DEFINITION AND BINDINGS
handle_cast({connect, 0, _ToPid}, State) ->
    %% We have reached the requested depth of our network connection
    {noreply, State};
handle_cast({connect, Depth, ToPid}, State) ->
    %% We will iterate a level in our network connection call
    do_connect(Depth, ToPid, State);
handle_cast({reserve, {HId, Visited}, RemSize, ChunkSize}, State) ->
    do_reserve(HId, Visited, RemSize, ChunkSize, State);
handle_cast({ready, HId, ReservedMap, Path}, State) ->
    do_ready(HId, ReservedMap, Path, State);
handle_cast({prepare, From, FileName, Chunk}, State) ->
    do_prepare(From, FileName, Chunk, State);
handle_cast({notify, From}, State) ->
    do_notify(From, State);
handle_cast({commit, FileName}, State) ->
    do_commit(FileName, State);
handle_cast(_Req, #state{} = State) ->
    {noreply, State}.




do_register(Alias, Pid,
            #state{alias = MyAlias, neighbours = Neighbours} = State) ->
    %% We first store our new node registering itself
    Neighbours1 = dict:store(Alias, Pid, Neighbours),
    %% Then we reply with our current neighbours and our alias for the node
    %% to continue its registration
    {reply, {ok, MyAlias, Neighbours}, State#state{neighbours = Neighbours1}}.

%% NOTE: if Depth < 0 then we will connect to all searchable nodes and
%% it WILL terminate
do_connect(Depth, ToPid, #state{alias = Alias, pid = Pid} = State) ->
    %% We first get the neighbours of the node we want to connect to
    {ok, ToAlias, Neighbours} = gen_server:call(ToPid, {register, Alias, Pid}),

    %% This function will add any new neighbours to our local record and
    %% then queue a call to connect to those nodes as well if our Depth is
    %% not reached
    Fun = fun(Key, Val, Acc) ->
                  case dict:is_key(Key, Acc) of
                      true ->
                          Acc;
                      false ->
                          gen_server:cast(Pid, {connect, Depth - 1, Val}),
                          dict:store(Key, Val, Acc)
                  end
          end,

    %% We then fold with the function on our received neighbours
    Neighbours1 = dict:fold(Fun, State#state.neighbours, Neighbours),
    %% And we record the Pid that we have just requested to our copy
    Neighbours2 = dict:store(ToAlias, ToPid, Neighbours1),
    {noreply, State#state{neighbours = Neighbours2}}.

do_reserve(HId, Visited, RemSize, ChunkSize,
           #state{alias = MyAlias, pid = MyPid, queued = Q,
                  queued_mem = QMem, memory = Mem} = State) ->

    %% If we have visited for this transaction that we can take it into
    %% account
    AlreadyAllocated = case lists:keytake(HId, 1, Q) of
                        {_, _, Allocated} ->
                            Allocated;
                        _ ->
                            0
                       end,
    %% Then we can compute what we are able to store
    Available = Mem - QMem + AlreadyAllocated,
    NeededToStore = min(RemSize, min(ChunkSize, Available)),
    ToStore = case NeededToStore < Available of
                  true -> NeededToStore;
                  false -> 0
              end,
    QMem1 =  QMem - ToStore + AlreadyAllocated,
    RemSize1 = RemSize - ToStore,

    %% If we have enough room to store it then we can add it to our queue
    %% of chunks to store
    Q = State#state.queued,
    Q1 = case 0 < ToStore andalso 0 < RemSize of
             true ->
                 FromAlias = hd(Visited),
                 [{HId, FromAlias, ToStore} | Q];
             false ->
                 Q
         end,

    %% We will stop the recursion if we already have enough space,
    %% or the node has no more neighbours to visit
    case 0 < RemSize1 of
        true ->
            %% Define our recursive reserve function call
            Fun = fun(Alias, Pid) ->
                    case lists:member(Alias, Visited) of
                        true ->
                            skip;
                        false ->
                            gen_server:cast(Pid, {reserve,
                                                  {HId, [MyAlias| Visited]},
                                                  RemSize1, ChunkSize})
                    end
                  end,
            %% Visit the unvisited nodes
            dict:map(Fun, State#state.neighbours);
        false ->
            %% We have allocated enough and can return
            gen_server:cast(MyPid, {ready, HId, [], Visited})
    end,
    {noreply, State#state{queued = Q1, queued_mem = QMem1}}.

do_ready(HId, RMap, Path,
         #state{alias = MyAlias, id = Id, pid = MyPid, queued = Q,
                neighbours = Neighbours} = State) ->

    %% We take a step on our path
    FromAlias = hd(Path),
    Rest = tl(Path),
    %% Then we want to find our parent in Q
    PossibleQ = lists:filter(
                  fun({CurHId, _, _}) -> CurHId == HId end,
                  Q),
    %% Filter out any other paths for this transaction id since
    %% we have found an allocation
    Q1 = lists:filter(
           fun({CurHId, _, _}) -> CurHId /= HId end,
           Q), 
    State1 = State#state{queued = Q1},
    MyHId = compute_hid(MyAlias, Id),
    case lists:keyfind(FromAlias, 2, PossibleQ) of
        {_HId, _FromAlias, ToStore} when MyHId == HId ->
            %% We have an RMap path that is allocated and are at the adding
            %% node again, we can now continue the add phase
            RMap1 = [{MyPid, ToStore} | RMap],
            continue_add(RMap1, State1);
        {_HId, _FromAlias, ToStore} ->
            %% Great, we are on a path that hasn't been returned yet
            %% Then we will recurse
            RMap1 = [{MyPid, ToStore} | RMap],
            {ok, Pid} = dict:find(FromAlias, Neighbours),  %% FIXME
            gen_server:cast(Pid, {ready, HId, RMap1, Rest}),
            {noreply, State1};
        _ ->
            %% Either we have a faulty call, or this parent has
            %% already received a response from another child
            %% (should only be the second
            {noreply, State1}
    end.

%% We are currently already adding a file from this node
start_add(_, _, State) when State#state.add_info /= available ->
       {reply, not_available, State};
start_add(UIPid, RSize, #state{alias = MyAlias, pid = Pid, id = Id} = State) ->
    %% We first want to reserve nodes of our network for the transaction to
    %% take place
    HId = compute_hid(MyAlias, Id),
    gen_server:cast(Pid, {reserve, {HId, [MyAlias]}, RSize, ?MAX_CHUNK}),

    %% When the reserve phases finishes it will automatically invoke continue_add
    %% so we can just let our calling node that we have started to reserve things
    {reply, started_reserving, State#state{add_info = UIPid}}.

continue_add(RMap, #state{pid = MyPid, add_info = UIPid} = State) ->
    %% We now want to retreive the file that is to be stored from the front-end
    {ok, FileName, AllContents} = ui:request_file(UIPid),

    %% Then we define our function to split the file into chunks to be stored
    %% and send those chunks out to the respective nodes (directly)
    Fun = fun({Pid, ToStore}, {Posn, Contents, Hash}) ->
            <<Cur:ToStore/binary, Rest/binary>> = Contents,
            Hash1 = hash(<<Hash/binary, Cur/binary>>),
            Chunk = #chunk{position = Posn, contents = Cur, hash = Hash1},
            gen_server:cast(Pid, {prepare, MyPid, FileName, Chunk}),
            {Posn + 1, Rest, Hash1}
          end,
    {_, <<>>, _} = lists:foldl(Fun, {0, AllContents, hash(<<>>)}, RMap),

    %% We keep track of the nodes that we have sent a chunk to and we
    %% will now have to wait until all those nodes respond that they
    %% are prepared to commit, or we timedout on the request and rollback
    Pids = lists:foldl(fun({Pid, _}, Pids) -> [{Pid, false} | Pids] end,
                      [], RMap),
    %% Keep track of our current timestamp for the sake of timingout
    CurTS = os:timestamp(),
    State1 = State#state{add_info = {UIPid, FileName, CurTS, Pids}},
    {noreply, State1}.

end_add(UIPid, Status, State) ->
    %% We have either timed out or successfully added the file to our network.
    %% Either way we will send a confirmation to the front-end
    ui:confirmation(UIPid, Status),
    {noreply, State#state{add_info = available}}.

do_prepare(From, FileName, #chunk{} = Chunk,
           #state{pid = MyPid, candidate = C} = State) ->
    %% We will keep track of our time for the sake of a potential timeout
    CurTS = os:timestamp(),
    %% We will add the chunk to our candidate database so it is easy to
    %% rollback if we timeout
    C1 = [{FileName, Chunk, CurTS} | C],
    %% We then notify the distributing node that we are prepared
    gen_server:cast(From, {notify, MyPid}),
    {noreply, State#state{candidate = C1}}.

do_notify(From, #state{add_info = {UIPid, FileName, TS, Pids}} = State) ->
    %% We are in the process of waiting for all nodes that we have
    %% distributed a chunk to, to repsond that they are prepared

    %% At this point we have received a notice that a Pid is prepared
    %% so we can set it to be true
    Pids1 = case lists:keytake(From, 1, Pids) of
                {value, {_From, false}, TPids} ->
                    [{From, true} | TPids];
                _ ->
                    Pids
            end,
   
    %% We then check if this add has timedout yet, and if not then we check
    %% if we have all nodes responding they have prepared
    case {not timedout(TS, ?PREPARE_TIMEOUT),
          lists:foldl(fun({_Pid, Prepared}, AllPrepared) ->
                              AllPrepared andalso Prepared
                      end,
                      true, Pids1)
         }
    of
        {true, true} -> %% We have not timedout and all nodes have replyed
                        %% so we can tell all the nodes to commit
                        %% (point of no return)
            lists:foreach(fun({Pid, _}) ->
                            gen_server:cast(Pid, {commit, FileName})
                          end, Pids),
            end_add(UIPid, ok, State);
        {false, _} ->
            %% Otherwise, we timedout and can send the front-end this info
            end_add(UIPid, fail, State);
        {true, false} ->
            %% We are still waiting for another process to reply and have time
            {noreply, State#state{add_info = {UIPid, FileName, TS, Pids1}}}
    end;
do_notify(_, State) ->
    %% In this case, we have timedout and so we can ignore the request since
    %% it took to long for the node to respond
    %% FIXME: maybe add a warning?
    {noreply, State}.

do_commit(FileName, #state{data = Data, candidate = C} = State) ->
    %% We get the go ahead to commit the change to our main database for
    %% candidate
    case lists:keytake(FileName, 1, C) of
        {value, {_FN, Chunk, _}, C1} ->
            %% Add the data to main database
            Data1 = dict:store(FileName, Chunk, Data),
            %% Update our remaining and queued memory accordingly
            Amt = size(Chunk#chunk.contents),
            Mem1 = State#state.memory - Amt,
            QMem1 = State#state.queued_mem + Amt,
            %% Increment our ID so the next transaction has a different hash
            Id1 = State#state.id + 1,
            {noreply, State#state{data = Data1, candidate = C1,
                                  memory = Mem1, queued_mem = QMem1,
                                  id = Id1}};
        _ ->
            {noreply, State}
    end.

%% clean(State) ->
%%     State.

%% MISC HELPERS SHOULD BE MOVED TO SEPERATE MODULE?
compute_hid(Alias, Id) when is_atom(Alias) andalso is_integer(Id) ->
    Bin = atom_to_binary(Alias),
    hash(<<Bin/binary, Id>>).

hash(Bin) ->
    crypto:hash(sha256, Bin).

timedout({_, OldSecs, _}, Timeout) ->
    {_, CurSecs, _} = os:timestamp(),
    abs(CurSecs - OldSecs) > Timeout.
