%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(basicgenserv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/2, start_link/0,
  code_change/3, handle_cast/2, handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0, stop/0, play/1, play_async/1]).
-define(STARTLIST,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> start_link().

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ?STARTLIST, []).

stop() -> gen_server:call(?MODULE, stop).

terminate(_Reason, _State) -> ok.

init(Arg) -> {ok, ["test" | Arg]}. 

play(Data) -> gen_server:call(?MODULE, {play, Data}).

play_async(Data) -> gen_server:cast(?MODULE, {play, Data}).

handle_call({play, Data}, _From, LoopData) -> 
  NewLoopData = do_play(Data, LoopData),
  {reply, ok, NewLoopData};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast({play, Data}, LoopData) -> 
  {noreply, do_play(Data, LoopData)};
handle_cast(stop, _State) ->
  {stop, normal}.

handle_info({play,Data}, LoopData) ->
  NewLoopData = do_play(Data, LoopData),
  {reply, ok, NewLoopData};
handle_info(stop, State) ->
  {stop, normal, State}.

code_change(_, _, _) -> 
  io:format("**Code change**~n"),
  ok.

do_play(Data, LoopData) ->
  NewLoopData = [Data|LoopData],
  io:format("********************~n"),
  io:format("Data: ~p~n",[Data]),
  io:format("Loop data: ~p~n",[NewLoopData]),
  io:format("********************~n~n"),
  NewLoopData.

