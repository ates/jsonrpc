-module(jsonrpc).

%% API
-export([make_req/3]).
-export([make_req/4]).
-export([make_get_req/2]).
-export([make_get_req/3]).

make_req(Pid, Method, Params) ->
    make_req(Pid, Method, Params, 5000).

make_req(Pid, Method, Params, Timeout) ->
    gen_server:call(Pid, {make_req, Method, Params}, Timeout).

make_get_req(Pid, URI) ->
    make_get_req(Pid, URI, 5000).

make_get_req(Pid, URI, Timeout) ->
    gen_server:call(Pid, {make_get_req, URI}, Timeout).
