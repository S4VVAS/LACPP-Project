-module(ui).

-behaviour(gen_server).

%% gen_server behaviour exports
-export([init/1, handle_call/3, handle_cast/2]).

-export([start/0, start/1, add/3, view/2]).

-export([add_status/3, request_file/1, give_chunk/3]).

-define(DEFAULT_DEPTH, 3).
-define(VIEW_TIMEOUT, 100). % In milliseconds
-define(ADD_TIMEOUT, 100). % In milliseconds

%% record definitions
-record(state,
        { add_info  = unallocated
        , node      = undefined
        , view_info = unallocated
        }
       ).

-record(file,
        { name = ""
        , contents = <<>>
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

%% FOR THE USER TO USE
start() ->
    gen_server:start(?MODULE, [undefined], []).

start(EndPoint) ->
    gen_server:start(?MODULE, [EndPoint], []).

init([undefined]) ->
    {ok, Node} = node:start({create}),
    {ok, #state{node = Node}};
init([EndPoint]) ->
    {ok, Node} = node:start({connect, EndPoint, ?DEFAULT_DEPTH}),
    {ok, #state{node = Node}}.


add(UIPid, FileName, Contents) ->
    BinFileName = list_to_binary(FileName),
    BinContents = list_to_binary(Contents),
    case gen_server:call(UIPid, {add, BinFileName, BinContents}) of
        started_adding ->
            receive
                {status, FileName, ok} ->
                    case view(UIPid, FileName) of
                        {ok, File} ->
                            {ok, File};
                        {corrupted_chunks, X} when size(BinContents) < X ->
                            %% We have a namespace collision. So we will
                            %% delete and then retry.
                            %% FIXME we need to add a call to rollback the
                            %% add of filename
                            add(UIPid, FileName, Contents);
                        Error ->
                            %% FIXME we need to add a call to rollback the
                            %% add of filename
                            {corrupted, FileName, Error}
                    end;
                {status, FileName, Error} ->
                    {fail, FileName, Error}
            after ?ADD_TIMEOUT ->
                      gen_server:cast(UIPid, {timeout, add}),
                      timedout
            end;
        Response ->
            Response
    end.


view(UIPid, FileName) ->
    BinFileName = list_to_binary(FileName),
    case gen_server:call(UIPid, {view, BinFileName}) of
        collecting_file ->
            receive
                {file, File} ->
                    {ok, File};
                Error ->
                    Error
            after ?VIEW_TIMEOUT ->
                      gen_server:call(UIPid, {timeout, view})
            end;
        Response ->
            Response
    end.

%% USED BY node.erl
request_file(UIPid) ->
    gen_server:call(UIPid, contents).

add_status(UIPid, FileName, Status) ->
    gen_server:cast(UIPid, {add_status, FileName, Status}).

give_chunk(UIPid, FileName, Chunk) ->
    gen_server:cast(UIPid, {give, FileName, Chunk}).


handle_call(node, _From, #state{node = Node} = State) ->
    {reply, {ok, Node}, State};

handle_call({add, FileName, Contents}, {From, _},
            #state{node = Node} = State) ->
    File = #file{name = FileName, contents = Contents},
    Response = gen_server:call(Node, {add, size(Contents)}),
    {reply, Response, State#state{add_info = {From, File}}};

handle_call({view, FileName}, {From, _}, #state{node = Node} = State) ->
    View = {FileName, [], From},
    Response = gen_server:call(Node, {view, FileName}),
    {reply, Response, State#state{view_info = View}};

handle_call(contents, _From, State) ->
    {_, File} = State#state.add_info,
    #file{name = FileName, contents = Contents} = File,
    {reply, {ok, FileName, Contents}, State};

handle_call({timeout, Type}, _From, State) ->
    case Type of
        add ->
            {noreply, State#state{add_info = unallocated}};
        view ->
            case State#state.view_info of
                unallocated ->
                    {reply, timedout, State};
                {_FileName, Chunks, _} ->
                    case length(Chunks) of
                        0 -> does_not_exist;
                        X -> {corrupted_chunks, X}
                    end
            end
    end;
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({add_status, FileName, Status}, #state{add_info = Add} = State)
  when Add /= unallocated ->
    {_From, File} = Add,
    AddingFileName = File#file.name,
    case AddingFileName == FileName of
        false -> {noreply, State};
        true ->
            do_status(Status, Add, State)
    end;
handle_cast({give, FileName, Chunk}, #state{view_info = View} = State)
  when View /= unallocated ->
    {ViewingFileName, Chunks, From} = View,
    case ViewingFileName == FileName of
        false -> {noreply, State};
        true ->
            case chunk_collected(Chunk, Chunks) of
                true ->
                    %% We have already received the chunk and can ignore it
                    {noreply, State};
                false ->
                    %% Otherwise, we will try to verify and return
                    do_give(FileName, [Chunk | Chunks], From, State)
            end
    end;
handle_cast(_Req, State) ->
    {noreply, State}.

do_status(Status, {From, File}, State) ->
    From ! {status, File#file.name, Status},
    {noreply, State#state{add_info = unallocated}}.

%% Chunks is guarenteed to have size > 0
do_give(FileName, Chunks0, From, State) ->
    Chunks = sort_chunks(Chunks0),
    MaybeFinal = case lists:last(Chunks) of
                     Chunk when Chunk#chunk.eof == true ->
                         Chunk#chunk.position;
                     _ ->
                         nothing
                 end,
    Length = length(Chunks),
    case MaybeFinal == Length andalso verify_hashes(Chunks) of
        true ->
            File = mk_file(FileName, Chunks),
            From ! {file, File},
            {noreply, State#state{view_info = unallocated}};
        _ ->
            View = {FileName, Chunks, From},
            {noreply, State#state{view_info = View}}
    end.

verify_hashes(Chunks) ->
    Fun = fun(Chunk, {Valid, Hash}) ->
                  ExpectedHash = Chunk#chunk.hash,
                  Cur = Chunk#chunk.contents,
                  Hash1 = misc:hash(<<Hash/binary, Cur/binary>>),
                  Valid1 = Valid andalso (ExpectedHash == Hash1),
                  {Valid1, Hash1}
          end,
    {IsValid, _} = lists:foldl(Fun, {true, misc:hash(<<>>)}, Chunks),
    IsValid.

mk_file(FileName, Chunks) ->
    Contents = lists:foldr(fun(Chunk, AccContents) ->
                                CurContents = Chunk#chunk.contents,
                                <<CurContents/binary, AccContents/binary>>
                           end, <<>>, Chunks),
    #file{name = FileName, contents = Contents}.

sort_chunks(Chunks) ->
    Fun = fun(Chunk1, Chunk2) ->
            Chunk1#chunk.position < Chunk2#chunk.position
          end,
    lists:sort(Fun, Chunks).

chunk_collected(Chunk, Chunks) ->
    ChunkHashes = lists:map(fun(CurChunk) ->
                                  CurChunk#chunk.hash
                            end, Chunks),
    ChunkHash = Chunk#chunk.hash,
    lists:member(ChunkHash, ChunkHashes).
