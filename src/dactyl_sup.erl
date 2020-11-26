%% @hidden
-module(dactyl_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref(start_link/0).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link()
      -> {ok, Pid :: pid()}.
start_link() ->
    {ok, _Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([])
      -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{ strategy => one_for_one,
                  intensity => 60,
                  period => 3600 },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
