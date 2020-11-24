-module(dactyl_app).
-behaviour(application).

%% ------------------------------------------------------------------
%% application Function Exports
%% ------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).

%% ------------------------------------------------------------------
%% application Function Definitions
%% ------------------------------------------------------------------

-spec start(StartType, StartArgs) -> Result
      when StartType :: application:start_type(),
           StartArgs :: term(),
           Result :: {ok, pid()}.
start(_StartType, _StartArgs) ->
    {ok, _Pid} = dactyl_sup:start_link().

-spec stop(State) -> Result
      when State :: term(),
           Result :: ok.
stop(_State) ->
    ok.
