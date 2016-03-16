-module(atm_sup).
-behaviour(supervisor).

-export([start_link/2, stop/0]).
-export([init/1]).


start_link(Name, Port) -> 
  supervisor:start_link(?MODULE, {Name, Port}).


stop() ->
  io:format("*******************atm_sup stop~n"), 
  exit(whereis(?MODULE), shutdown).
  
init({Name, Port}) ->
  {ok, {
    {rest_for_one, 5, 2000},
    [child(atm, [Name]), child(webatm, [Name, Port])]
    }
  }.
  
child(Module, Args) ->
  {Module, {Module, start_link, Args}, permanent, brutal_kill, worker, [Module]}.