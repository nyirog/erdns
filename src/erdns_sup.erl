%%%-------------------------------------------------------------------
%% @doc erdns top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erdns_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) -> {
    ok,
    {
        #{
            strategy => one_for_all,
            intensity => 1,
            period => 5
        },
        []
    }
}.

%%====================================================================
%% Internal functions
%%====================================================================
