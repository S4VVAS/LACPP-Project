-module(ui).

-behaviour(gen_server).

%% gen_server behaviour exports
-export([start/0, init/1, handle_call/3, handle_cast/2]).

-export([confirmation/2, request_file/1, add/3]).

start() ->
    gen_server:start(?MODULE, [], []).

request_file(UIPid) ->
    gen_server:call(UIPid, contents).

confirmation(UIPid, Status) ->
    gen_server:cast(UIPid, {confirmation, Status}).

add(UIPid, Node, Size) ->
    gen_server:call(UIPid, {add, Node, Size}).

init([]) ->
    {ok, []}.

handle_call({add, Node, Size}, _From, State) ->
    Response = gen_server:call(Node, {add, Size}),
    {reply, Response, State};
handle_call(contents, _From, State) ->
    {ok, {FileName, Contents}} = {ok, {"test", <<"DUMMY">>}},
    {reply, {ok, FileName, Contents}, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({confirmation, Status}, State) ->
    io:format("added result: ~p~n", [Status]),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.
