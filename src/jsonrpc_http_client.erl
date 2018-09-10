-module(jsonrpc_http_client).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_continue/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-type opts() :: #{
    ip               := inet:ip_address(),
    port             := inet:port_number(),
    uri              => binary(),
    response_timeout => non_neg_integer(),
    connect_timeout  => non_neg_integer(),
    send_timeout     => non_neg_integer(),
    retry_timeout    => non_neg_integer(),
    retry            => pos_integer(),
    keepalive        => non_neg_integer() | infinity
}.

-export_type([opts/0]).

-record(state, {
    id = 0 :: non_neg_integer(),
    pid    :: undefined | pid(),
    opts   :: map()
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, #state{opts = make_opts(Args)}, {continue, start_gun}}.

handle_call({make_req, Method, Params}, _From, #state{opts = #{uri := URI}} = State) ->
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"content-type">>, <<"application/json">>}
    ],
    Request = jsonrpc:format_request(State#state.id, Method, Params),
    StreamRef = gun:post(State#state.pid, URI, Headers, Request),
    {reply, wait_for_response(StreamRef, State), State#state{id = State#state.id + 1}};

handle_call({make_get_req, URI}, _From, State) ->
    StreamRef = gun:get(State#state.pid, URI),
    {reply, wait_for_response(StreamRef, State), State};

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_continue(start_gun, State) ->
    #{
        ip       := IP,
        port     := Port,
        gun_opts := GunOpts
    } = State#state.opts,
    {ok, Pid} = gun:open(IP, Port, GunOpts),
    erlang:link(Pid),
    {noreply, State#state{pid = Pid}}.

handle_info({gun_up, _Pid, http}, State) ->
    logger:info("connection is up"),
    {noreply, State};

handle_info({gun_down, _Pid, http, Reason, _Killed, _Unprocessed}, State) ->
    logger:info("connection is terminated, reason: ~p", [Reason]),
    {noreply, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:error("connection is terminated, reason: ~p", [Reason]),
    {noreply, State, {continue, start_gun}};

handle_info(_Msg, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{pid = Pid} = _State) ->
    case is_pid(Pid) of
        true ->
            gun:shutdown(Pid);
        false -> ok
    end.

wait_for_response(StreamRef, #state{pid = Pid, opts = #{timeout := Timeout}} = State) ->
    receive
        {gun_response, Pid, StreamRef, fin, _Status, _Headers} ->
            {error, no_response};
        {gun_response, Pid, StreamRef, nofin, Status, _Headers} ->
            case collect_response(StreamRef, State, <<>>) of
                {ok, Data} when Status =:= 200 ->
                    {ok, jsx:decode(Data, [return_maps, {labels, atom}])};
                {ok, Error} when Status =/= 200 ->
                    {error, Error};
                Error -> Error
            end
    after Timeout ->
        {error, response_timeout}
    end.

collect_response(StreamRef, #state{pid = Pid, opts = #{timeout := Timeout}} = State, Acc) ->
    receive
        {gun_data, Pid, StreamRef, nofin, Data} ->
            collect_response(StreamRef, State, <<Acc/binary, Data/binary>>);
        {gun_data, Pid, StreamRef, fin, Data} ->
            {ok, <<Acc/binary, Data/binary>>}
    after Timeout ->
        {error, response_timeout}
    end.

make_opts(#{ip := IP, port := Port} = Args) ->
    #{
        ip       => IP,
        port     => Port,
        timeout  => maps:get(response_timeout, Args, 5000),
        uri      => maps:get(uri, Args, <<"/json_rpc">>), 
        gun_opts => #{
            retry           => maps:get(retry, Args, 5),
            retry_timeout   => maps:get(retry_timeout, Args, 5000),
            connect_timeout => maps:get(connect_timeout, Args, 5000),
            http_opts       => #{keepalive => maps:get(keepalive, Args, infinity)},
            transport_opts  => [
                {send_timeout, maps:get(send_timeout, Args, 500)}
            ]
        }
    }.
