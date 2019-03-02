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
    rfc4627_jsonrpc:start(),
    inets:start(
       httpd,
       [
           {port, 12345},
           {server_name, "erdns"},
           {server_root,"/tmp"},
           {document_root,"/tmp"},
           {bind_address, "localhost"},
           {
               mod,
               [
                   mod_alias,
                   rfc4627_jsonrpc_inets,
                   mod_actions,
                   mod_cgi,
                   mod_responsecontrol,
                   mod_trace,
                   mod_range,
                   mod_head,
                   mod_include,
                   mod_dir,
                   mod_get,
                   mod_log,
                   mod_disk_log
               ]
           },
           {json_rpc_alias, "/rpc"}
        ]
    ),
    erdns_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
