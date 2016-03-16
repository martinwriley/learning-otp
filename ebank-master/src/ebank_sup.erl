-module(ebank_sup).
-vsn('1.0').
-behaviour(supervisor).
-export([start_link/0, start_atm/2, stop_atm/1]).

-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, no_args).

start_atm(Name, Port) -> supervisor:start_child(?MODULE, child(atm_sup, Name, [Port])).

stop_atm(Name) ->
 io:format("*******************ebank_sup stop_atm~n"), 
 supervisor:terminate_child(?MODULE, {atm_sup, Name}),
 supervisor:delete_child(?MODULE, {atm_sup, Name}).
  

init(no_args) ->
  {ok, {{rest_for_one, 5, 2000}, [child(backend, none)]}}.
  

child(Module, none) ->
  {Module, {Module, start_link, []}, permanent, brutal_kill, worker, [Module]};
child(Module, Name) ->
  {{Module, Name}, {Module, start_link, [Name]}, permanent, brutal_kill, worker, [Module]}.
child(Module, Name, Args) ->
  {{Module, Name}, {Module, start_link, [Name] ++ Args}, permanent, brutal_kill, worker, [Module]}.
