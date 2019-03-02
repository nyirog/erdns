-module(erdns_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2
]).
-export([handle/2, from_json/2, to_json/2]).
-include_lib("kernel/src/inet_dns.hrl").

init(Request, Opts) -> {cowboy_rest, Request, Opts}.

allowed_methods(Request, State) ->
  {[<<"POST">>], Request, State}.

content_types_accepted(Request, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Request, State}.

from_json(Request, State) ->
    {ok, Body, ReqLeft} = cowboy_req:read_body(Request),
    {reply, Back} = jsonrpc2:handle(
        Body,
        fun handle/2,
        fun jiffy:decode/1,
        fun jiffy:encode/1
    ),
    Response = cowboy_req:set_resp_body(Back, ReqLeft),
    {true, Response, State}.

content_types_provided(Request, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Request, State}.

to_json(Request, State) -> {true, Request, State}.

handle(<<"rdns">>, [Address]) ->
    {ok, Ip} = inet:parse_address(erlang:binary_to_list(Address)),
    case inet_res:resolve(Ip, in, ptr) of
        {ok, #dns_rec{anlist = [Answer]}} ->
            erlang:list_to_binary(Answer#dns_rr.data);
        {error, _} ->
            throw(internal_error)
    end;
handle(_Name, _Params) -> throw(method_not_found).
