%%%-------------------------------------------------------------------
%% @doc erdns public API
%% @end
%%%-------------------------------------------------------------------

-module(erdns_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {
            '_',
            [
               {"/", erdns_handler, []}
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(
        erdns,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    erdns_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
