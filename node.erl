-module(node).

-behaviour(gen_server).

%% gen_server behaviour exports
-export([start/1, init/1, handle_call/3, handle_cast/2]).

%% --------------------------------------------------
%% HIGH LEVEL OVERVIEW
%% --------------------------------------------------

%% Adding a file, f, from any node of the network, n (root node below):
%%  - We first determine if the network has enough memory (storage) to hold
%%  the file. To do so we ping the nodes of the network with a reserve request
%%  and we want to construct a path of nodes in our network that are able to
%%  store the file. Therefore, when a node in the network receives a reserve
%%  request, if the node does not have enough space then it will simply pass on
%%  the request to its unvisited neighbours. Otherwise, it will add itself to
%%  the path and pass on the request with itself attached. When it adds itself
%%  to the path it also will reserve the promised about of memory to. If a node
%%  has received a request and after adding itself, it deems that we have enough
%%  memory required. Then it begins a ready respond.
%%
%%  - The ready response will simply follow the path that was created back to
%%  the root node with all the required information for the root node to
%%  directly contact each node in the path.
%%
%%  - We can then load the file into the root node and create the chunks
%%  of the sizes denoted from the path (when a node adds itself to the path
%%  it also says how much it can reserved to store). We then also create a
%%  cumunilative hash of the file for verification and it is stored along side
%%  the contents in a 'chunk'.
%%  
%%  - We then distribute the chunks to the nodes directly from the given contact
%%  information. And record a timestamp of when this occured for the sake of
%%  timing out. When a node receives its chunk of the file, it does a prepare
%%  phase. In the sense that it stores the chunk in a candidate database that
%%  is seperate from the database (this is to allow an easy rollback). Once
%%  the node has finished storing the chunk in its candidate database, it
%%  will respond to the root node that it is prepared.
%%
%%  - Once the root node has received a confirmation that all of the storing
%%  nodes are prepared, it will send a final commmit to each of the nodes
%%  (the point of no return). Each node that receives the commit request will
%%  then move the file in the candidate database into the main database. At this
%%  point, if some node did not receive the commit request, then we will have
%%  lost some data (we can add some rollback at this stage to if we want).
%%
%%  - If a node does not receive a commit request within the specified
%%  COMMIT_TIMEOUT, then it will remove the chunk from its candidate database.
%%
%%
%%
%%  Viewing a file, f, from any of the network, n (root node below) with a ui:
%%  - We first do a breadth first search of our entire connected network.
%%  - Each node that we visit that has a file associated with the FileName we
%%  are looking for will directly ping the ui with their chunk
%%  - Whenever the viewing ui receives a chunk, it will try to verify that
%%  the chunks it has accumlated are valid. If they are valid then we return,
%%  otherwise, we will continue waiting.
%%  - After the specified timeout period, the viewing node will return with
%%  a timeout code.

-define(RESERVE_TIMEOUT, 1). %% Currently in seconds
-define(PREPARE_TIMEOUT, 2). %% Currently in seconds
-define(COMMIT_TIMEOUT, 5). %% Currently in seconds
-define(MAX_CHUNK, 3).
-define(DEFAULT_MEMORY, 4).

%% record definitions
-record(state,
        { pid           = self()
        , neighbours    = []
        , memory        = ?DEFAULT_MEMORY
        , data          = dict:new()
        , candidate     = {0, []}
        , queued        = []
        , id            = 0
        , queued_mem    = 0
        , add_info      = available
        }
       ).

-record(chunk,
        { position  = 0
        , contents  = <<>>
        , hash      = <<>>
        , size      = 0
        , eof       = false 
        }
       ).

%% We are creating the first node of our network so we just initiate it
start(create) ->
    gen_server:start(?MODULE, [], []);
%% Otherwise, we will need to specify any endpoint of the network and connect
%% through that point. We initiate our node and then we connect to the network
%% to some level of Depth.
start({connect, To, Depth}) ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    gen_server:cast(Pid, {connect, Depth, To}),
    {ok, Pid}.


%% Initiate a node
init([]) ->
    {ok, #state{}}.



%% CALL DEFINITION AND BINDINGS
handle_call({register, Pid}, _From, State) ->
    do_register(Pid, State);
handle_call(print, _From, State0) -> %% USED JUST FOR DEBUGGING PURPOSES
    State = clean(State0),
    {reply, {ok, dict:fetch_keys(State#state.neighbours),
             dict:fetch_keys(State#state.data),
             State#state.memory, State#state.queued, State#state.queued_mem
            }, State};
handle_call({add, RequiredSize}, {UIPid, _}, State) ->
    start_add(UIPid, RequiredSize, State);
handle_call({view, FileName}, {UIPid, _}, State) ->
    start_view(UIPid, FileName, State);
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
handle_cast({view, UIPid, FileName, Visited}, State) ->
    do_view(UIPid, FileName, Visited, State);
handle_cast(_Req, #state{} = State) ->
    {noreply, State}.




do_register(Pid, #state{neighbours = Neighbours} = State) ->
    %% We first store our new node registering itself
    Neighbours1 = [Pid | Neighbours],
    %% Then we reply with our current neighbours for the node
    %% to continue its registration
    {reply, {ok, Neighbours}, State#state{neighbours = Neighbours1}}.

%% NOTE: if Depth < 0 then we will connect to all searchable nodes and
%% it WILL terminate
do_connect(Depth, ToPid, #state{pid = Pid} = State) ->
    %% We first get the neighbours of the node we want to connect to
    {ok, Neighbours} = gen_server:call(ToPid, {register, Pid}),

    %% This function will add any new neighbours to our local record and
    %% then queue a call to connect to those nodes as well if our Depth is
    %% not reached
    Fun = fun(Val, Acc) ->
                  case lists:member(Val, Neighbours) of
                      true ->
                          Acc;
                      false ->
                          gen_server:cast(Pid, {connect, Depth - 1, Val}),
                          [Val | Acc]
                  end
          end,

    %% We then fold with the function on our received neighbours
    Neighbours1 = lists:foldl(Fun, State#state.neighbours, Neighbours),
    %% And we record the Pid that we have just requested to our copy
    Neighbours2 = [ToPid | Neighbours1],
    {noreply, State#state{neighbours = Neighbours2}}.

do_reserve(HId, Visited, RemSize, ChunkSize,
           #state{pid = MyPid, queued = Q,
                  queued_mem = QMem, memory = Mem,
                  candidate = {CSize, _C}} = State) ->

    %% If we have visited to reserve for this transaction then we can take into
    %% account that it is allocated
    AlreadyAllocated = lists:foldl(
                         fun({CurHId, _, Allocated, _}, Max) ->
                                 case CurHId == HId
                                      andalso Max < Allocated
                                 of
                                     true -> Allocated;
                                     false -> Max
                                 end
                         end,
                         0, Q),
    %% Then we can compute what we are able to store
    Available = Mem - CSize - QMem + AlreadyAllocated,
    NeededToStore = min(RemSize, min(ChunkSize, Available)),
    ToStore = case NeededToStore =< Available of
                  true -> NeededToStore;
                  false -> 0
              end,
    QMem1 =  QMem - AlreadyAllocated + max(AlreadyAllocated, ToStore),
    RemSize1 = RemSize - ToStore,

    %% If we have enough room to store it then we can add it to our queue
    %% of chunks to store
    Q = State#state.queued,
    Q1 = case 0 < ToStore andalso 0 < RemSize of
             true ->
                 FromPid = hd(Visited),
                 TS = os:timestamp(),
                 [{HId, FromPid, ToStore, TS} | Q];
             false ->
                 Q
         end,

    %% We will stop the recursion if we already have enough space,
    %% or the node has no more neighbours to visit
    case 0 < RemSize1 of
        true ->
            %% Define our recursive reserve function call
            Fun = fun(Pid) ->
                    case lists:member(Pid, Visited) of
                        true ->
                            skip;
                        false ->
                            gen_server:cast(Pid, {reserve,
                                                  {HId, [MyPid | Visited]},
                                                  RemSize1, ChunkSize})
                    end
                  end,
            %% Visit the unvisited nodes
            lists:foreach(Fun, State#state.neighbours);
        false ->
            %% We have allocated enough and can return
            gen_server:cast(MyPid, {ready, HId, [], Visited})
    end,
    State1 = State#state{queued = Q1, queued_mem = QMem1},
    {noreply, clean(State1)}.

do_ready(HId, RMap, Path,
         #state{id = Id, pid = MyPid, queued = Q, queued_mem = QMem} = State) ->

    %% We take a step on our path
    FromPid = hd(Path),
    Rest = tl(Path),
    %% Then we want to find our parent in Q
    PossibleQ = lists:filter(
                  fun({CurHId, _, _, _}) -> CurHId == HId end,
                  Q),
    %% Filter out any other paths for this transaction id since
    %% we have found an allocation
    {Q1, Rmvd} = lists:partition(
           fun({CurHId, _, _, _}) -> CurHId /= HId end,
           Q),
    Allocated =lists:foldl(
                     fun({_, _, Reserved, _}, Max) -> max(Reserved, Max) end,
                     0, Rmvd),
 
    %% Return the allocated amount to our available memory
    QMem1 = QMem - Allocated,

    State1 = State#state{queued = Q1, queued_mem = QMem1},
    MyHId = misc:compute_hid(MyPid, Id),
    case lists:keyfind(FromPid, 2, PossibleQ) of
        {_HId, _FromPid, ToStore, _TS} when MyHId == HId ->
            %% We have an RMap path that is allocated and are at the adding
            %% node again, we can now continue the add phase
            RMap1 = [{MyPid, ToStore} | RMap],
            continue_add(RMap1, State1);
        {_HId, _FromPid, ToStore, _TS} ->
            %% Great, we are on a path that hasn't been returned yet
            %% Then we will recurse
            RMap1 = [{MyPid, ToStore} | RMap],
            gen_server:cast(FromPid, {ready, HId, RMap1, Rest}),
            {noreply, clean(State1)};
        _ ->
            %% Either we have a faulty call, or this parent has
            %% already received a response from another child
            %% (should only be the second)
            {noreply, clean(State1)}
    end.

%% We are currently already adding a file from this node
start_add(_, _, State) when State#state.add_info /= available ->
       {reply, not_available, clean(State)};
start_add(UIPid, RSize, #state{pid = MyPid, id = Id} = State) ->
    %% We first want to reserve nodes of our network for the transaction to
    %% take place
    HId = misc:compute_hid(MyPid, Id),
    gen_server:cast(MyPid, {reserve, {HId, [MyPid]}, RSize, ?MAX_CHUNK}),

    %% When the reserve phases finishes it will automatically invoke continue_add
    %% so we can just let our calling node that we have started to reserve things
    
    %% Keep track of our current timestamp for the sake of timingout from
    %% reserve
    TS = os:timestamp(),
    State1 = State#state{add_info = {UIPid, TS}},
    {reply, started_adding, clean(State1)}.

continue_add(RMap, #state{pid = MyPid, add_info = {UIPid, _TS}} = State) ->
    %% We now want to retreive the file that is to be stored from the front-end
    {ok, FileName, AllContents} = ui:request_file(UIPid),

    Length = length(RMap),

    %% Then we define our function to split the file into chunks to be stored
    %% and send those chunks out to the respective nodes (directly)
    Fun = fun({Pid, ToStore}, {Posn, Contents, Hash}) ->
            <<Cur:ToStore/binary, Rest/binary>> = Contents,
            Hash1 = misc:hash(<<Hash/binary, Cur/binary>>),
            Chunk = #chunk{position = Posn, contents = Cur,
                           hash = Hash1, size = size(Cur),
                           eof = Posn == Length},
            gen_server:cast(Pid, {prepare, MyPid, FileName, Chunk}),
            {Posn + 1, Rest, Hash1}
          end,
    case lists:foldl(Fun, {1, AllContents, misc:hash(<<>>)}, RMap) of
        {_, <<>>, _} ->
            %% We keep track of the nodes that we have sent a chunk to and we
            %% will now have to wait until all those nodes respond that they
            %% are prepared to commit, or we timedout on the request and rollback
            Pids = lists:foldl(fun({Pid, _}, Pids) -> [{Pid, false} | Pids] end,
                              [], RMap),
            %% Keep track of our current timestamp for the sake of timingout
            CurTS = os:timestamp(),
            State1 = State#state{add_info = {UIPid, FileName, CurTS, Pids}},
            {noreply, clean(State1)};
        {_, _NotAllocated, _} ->
            %% A node was not able to hold its promise of size
            end_add(UIPid, FileName, unable_to_reserve, State)
    end.

end_add(UIPid, FileName, Status, State) ->
    %% We have either timed out or successfully added the file to our network.
    %% Either way we will send a confirmation to the front-end
    ui:add_status(UIPid, FileName, Status),
    State1 = State#state{add_info = available},
    {noreply, clean(State1)}.

do_prepare(From, FileName, #chunk{} = Chunk,
           #state{pid = MyPid, candidate = {CSize, C}} = State) ->
    %% We will keep track of our time for the sake of a potential timeout
    CurTS = os:timestamp(),
    %% We will add the chunk to our candidate database so it is easy to
    %% rollback if we timeout
    C1 = [{FileName, Chunk, CurTS} | C],
    CSize1 = CSize + Chunk#chunk.size,
    %% We then notify the distributing node that we are prepared
    gen_server:cast(From, {notify, MyPid}),
    State1 = State#state{candidate = {CSize1, C1}},
    {noreply, clean(State1)}.

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
    case {not misc:timedout(TS, ?PREPARE_TIMEOUT),
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
            end_add(UIPid, FileName, ok, State);
        {false, _} ->
            %% Otherwise, we timedout and can send the front-end this info
            end_add(UIPid, FileName, fail, State);
        {true, false} ->
            State1 = State#state{add_info = {UIPid, FileName, TS, Pids1}},
            %% We are still waiting for another process to reply and have time
            {noreply, clean(State1)}
    end;
do_notify(_, State) ->
    %% In this case, we have timedout and so we can ignore the request since
    %% it took to long for the node to respond
    %% FIXME: maybe add a warning?
    {noreply, clean(State)}.

do_commit(FileName, #state{data = Data, candidate = {CSize, C}} = State) ->
    %% We get the go ahead to commit the change to our main database for
    %% candidate
    case lists:keytake(FileName, 1, C) of
        {value, {_FN, Chunk, _}, C1} ->
            CSize1 = CSize - Chunk#chunk.size,
            %% Add the data to main database
            Data1 = dict:store(FileName, Chunk, Data),
            %% Update our remaining and queued memory accordingly
            Amt = size(Chunk#chunk.contents),
            Mem1 = State#state.memory - Amt,
            %% Increment our ID so the next transaction has a different hash
            Id1 = State#state.id + 1,
            State1 = State#state{data = Data1, candidate = {CSize1, C1},
                            memory = Mem1, id = Id1},
            {noreply, clean(State1)};
        _ ->
            %% FIXME: This is bad, we timed out locally but not globally
            %% (shouldn't happen if the TIMEOUT are correctly proportioned
            {noreply, clean(State)}
    end.

start_view(UIPid, FileName, #state{pid = Pid} = State) ->
    %% We begin our depth first search
    gen_server:cast(Pid, {view, UIPid, FileName, []}),
    {reply, collecting_file, State}.

do_view(UIPid, FileName, Visited, #state{neighbours = Neighbours,
                                         data = Data} = State) ->
    %%% If we have a chunk then we ping the ui
    case dict:find(FileName, Data) of
        error -> skip;
        {ok, Chunk} ->
            ui:give_chunk(UIPid, FileName, Chunk)
    end,

    %% Then, we recursively invoke on our unvisited neighbours
    Visited1 = Neighbours ++ Visited,
    lists:foreach(fun(Pid) ->
                case lists:member(Pid, Visited) of
                    true -> skip;
                    false ->
                        gen_server:cast(Pid, {view, UIPid, FileName, Visited1})
                end
             end, Neighbours),
    {noreply, State}.

%% CLEAN FUNCTION TO CLEAN UP TIMEDOUT REQUESTS
clean(#state{add_info = {UIPid, TS}} = State) ->
    case misc:timedout(TS, ?RESERVE_TIMEOUT) of
        true ->
            {noreply, State1} = end_add(UIPid, no_memory, fail, State),
            State1;
        false -> do_clean(State)
    end;
clean(State) ->
    do_clean(State).
        
do_clean(#state{candidate = {_CSize, C},
                queued = Q, queued_mem = QMem} = State) ->
    %% Removed the timedout out commits in our candidate database
    C1 = lists:filter(fun({_, _, TS}) ->
                              not misc:timedout(TS, ?COMMIT_TIMEOUT)
                      end, C),
    %% Update the size accordingly
    CSize1 = lists:foldl(fun({_, Chunk, _}, Acc) ->
                                 Acc + Chunk#chunk.size
                         end, 0, C1),

    %% Remove the timedout out reserve request
    {Q1, Rmvd} = lists:partition(fun({_, _, _, TS}) ->
                                         not misc:timedout(TS, ?RESERVE_TIMEOUT)
                                 end, Q),
    %% Add the memory back to our available memory
    UniqueRmvd = lists:foldl(fun({HId, _, Removed, _}, Acc) ->
                                     case lists:keytake(HId, 1, Acc) of
                                         false ->
                                             [{HId, Removed} | Acc];
                                         {value, {_HId, MaxRmved}, Acc1} ->
                                             [{HId, max(Removed, MaxRmved)} | Acc1]
                                     end
                             end, [], Rmvd),
    QMem1 = lists:foldl(fun({_HId, Reserved}, AccQMem) ->
                                AccQMem - Reserved
                          end, QMem, UniqueRmvd),
    State#state{candidate = {CSize1, C1}, queued = Q1, queued_mem = QMem1}.
