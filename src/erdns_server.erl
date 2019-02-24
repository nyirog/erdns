%%%-----------------------------------------------------------------------------
%% @doc erdns server: reverse dns lookup
%% @end
%%%-----------------------------------------------------------------------------

-module(erdns_server).

-behaviour(gen_server).

-include_lib("kernel/src/inet_dns.hrl").

-record(lookup, {address = {0,0,0,0}, name = "", ttl = 0, expiration = 0}).

%% Application callbacks
-export([start_link/0, stop/0, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2]).

-export([resolve/1]).

%%==============================================================================
%% API
%%==============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(), []).

stop() -> gen_server:call(?MODULE, stop).

init(State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%------------------------------------------------------------------------------

resolve(Address) -> gen_server:call(?MODULE, {resolve, Address}).

%%------------------------------------------------------------------------------

handle_call(
        {resolve, Address},
        _From,
        State = #{nameservers := NameServers, addresses := Addresses}
) ->
    Now = erlang:system_time(second),
    Lookup = maps:get(Address, Addresses, #lookup{}),
    case is_valid_at(Now, Lookup) of
        true ->
            {reply, norm_ttl(Now, Lookup), State};
        false -> 
            NewLookup = resolve(Address, [{nameservers, NameServers}], Now),
            {
                reply,
                norm_ttl(Now, NewLookup),
                State#{addresses := Addresses#{Address => Lookup}}
            }
    end;
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

init_state() -> #{nameservers => [{{192,168,0,1},53}], addresses => #{}}.

resolve(Address, Opts, Now) ->
    {ok, #dns_rec{anlist = [Answer]}} = inet_res:resolve(Address, in, ptr, Opts),
    #lookup{
        address = Address,
        name = Answer#dns_rr.data,
        ttl = Answer#dns_rr.ttl,
        expiration = Answer#dns_rr.ttl + Now
    }.

is_valid_at(Now, #lookup{expiration = Expiration}) -> Expiration > Now.

norm_ttl(Now, #lookup{expiration = Expiration, name = Name}) ->
    {Name, erlang:max(Expiration - Now, 0)}.