-module(ui).

-behaviour(gen_server).

%% gen_server behaviour exports
-export([init/1, handle_call/3, handle_cast/2]).

-export([start/1, start/2, add/3, view/2]).

-export([notify/2, request_file/1, get_endpoint/1, give_chunk/3]).

-define(DEFAULT_DEPTH, 3).
-define(VIEW_TIMEOUT, 1000). % In milliseconds

%% record definitions
-record(state,
        { file      = unallocated
        , node      = undefined
        , view      = unallocated
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
start(Alias) ->
    gen_server:start(?MODULE, [Alias, undefined], []).

start(Alias, EndPoint) ->
    gen_server:start(?MODULE, [Alias, EndPoint], []).

add(UIPid, FileName, Contents) ->
    gen_server:call(UIPid, {add, FileName, Contents}).

view(UIPid, FileName) ->
    collecting_file = gen_server:call(UIPid, {view, FileName}),
    receive
        {file, File} ->
            File;
        Error ->
            Error
    after ?VIEW_TIMEOUT ->
          timedout
    end.

get_endpoint(UIPid) ->
    gen_server:call(UIPid, node).

%% USED BY node.erl
request_file(UIPid) ->
    gen_server:call(UIPid, contents).

notify(UIPid, Notif) ->
    gen_server:cast(UIPid, {notify, Notif}).

give_chunk(UIPid, FileName, Chunk) ->
    gen_server:cast(UIPid, {give, FileName, Chunk}).

init([Alias, undefined]) ->
    {ok, Node} = node:start({create, Alias}),
    {ok, #state{node = Node}};
init([Alias, EndPoint]) ->
    {ok, Node} = node:start({connect, Alias, EndPoint, ?DEFAULT_DEPTH}),
    {ok, #state{node = Node}}.


handle_call(node, _From, #state{node = Node} = State) ->
    {reply, {ok, Node}, State};

handle_call({add, FileName, Contents}, _From,
            #state{file = unallocated, node = Node} = State) ->
    File = #file{name = FileName, contents = Contents},
    Response = gen_server:call(Node, {add, size(Contents)}),
    {reply, Response, State#state{file = File}};
handle_call({add, _, _}, _From, State) ->
    {reply, already_adding, State};

handle_call({view, FileName}, {From, _},
            #state{node = Node, view = unallocated} = State) ->
    View = {FileName, [], From},
    Response = gen_server:call(Node, {view, FileName}),
    {reply, Response, State#state{view = View}};
handle_call({view, _}, _From, State) ->
    {reply, already_viewing, State};

handle_call(contents, _From, State) ->
    #file{name = FileName, contents = Contents} = State#state.file,
    {reply, {ok, FileName, Contents}, State#state{file = unallocated}};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({notify, Notif}, State) ->
    TS = os:timestamp(),
    io:format("<~p>: ~p~n", [TS, Notif]),
    {noreply, State};
handle_cast({give, FileName, Chunk}, #state{view = View} = State)
  when View /= unallocated ->
    {ViewingFileName, Chunks, From} = View,
    case ViewingFileName == FileName of
        false -> {noreply, State};
        true ->
            do_give(FileName, [Chunk | Chunks], From, State)
    end;
handle_cast(_Req, State) ->
    {noreply, State}.


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
            {noreply, State#state{view = unallocated}};
        _ ->
            View = {FileName, Chunks, From},
            {noreply, State#state{view = View}}
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
