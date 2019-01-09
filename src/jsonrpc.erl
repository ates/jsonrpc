-module(jsonrpc).

%% API
-export([make_req/3]).
-export([make_req/4]).
-export([make_get_req/2]).
-export([make_get_req/3]).
-export([make_batch_req/3]).
-export([make_batch_req/4]).
-export([format_request/3]).
-export([format_batch_request/3]).

make_req(Pid, Method, Params) ->
    make_req(Pid, Method, Params, 5000).

make_req(Pid, Method, Params, Timeout) ->
    gen_server:call(Pid, {make_req, Method, Params}, Timeout).

make_get_req(Pid, URI) ->
    make_get_req(Pid, URI, 5000).

make_get_req(Pid, URI, Timeout) ->
    gen_server:call(Pid, {make_get_req, URI}, Timeout).

make_batch_req(Pid, Method, Params) ->
    make_batch_req(Pid, Method, Params, 5000).

make_batch_req(Pid, Method, Params, Timeout) ->
    gen_server:call(Pid, {make_batch_req, Method, Params}, Timeout).

-spec format_request(Id :: non_neg_integer(), Method :: atom(), Params :: map() | list()) -> binary().
format_request(Id, Method, Params) ->
    jsx:encode(#{
        jsonrpc => <<"2.0">>,
        id      => Id,
        method  => Method,
        params  => Params
    }).

format_batch_request(Id, Method, Params) ->
    F = fun(Param, {N, Acc}) ->
        Request = #{
            jsonrpc => <<"2.0">>,
            id      => N,
            method  => Method,
            params  => Param
        },
        {N + 1, [Request | Acc]}
    end,
    {_N, Batch} = lists:foldl(F, {Id, []}, Params),
    jsx:encode(Batch).
