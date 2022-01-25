-module(jsonrpc_tcp_client).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_continue/2]).
-export([handle_info/2]).
-export([terminate/2]).

-type opts() :: #{
    ip               := inet:ip_address(),
    port             := inet:port_number(),
    oneshot          => boolean(),
    response_timeout => non_neg_integer(),
    send_timeout     => non_neg_integer(),
    retry_timeout    => non_neg_integer()
}.

-export_type([opts/0]).

-record(state, {
    id = 0 :: non_neg_integer(),
    socket :: undefined | gen_tcp:socket(),
    opts   :: map()
}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    #{oneshot := OneShot} = Opts = make_opts(Args),
    case OneShot of
        true ->
            {ok, #state{opts = Opts}};
        false ->
            {ok, #state{opts = Opts}, {continue, connect}}
    end.

handle_call({make_req, Method, Params}, _From, #state{opts = #{oneshot := true} = Opts} = State) ->
    Reply =
        case connect(State) of
            {ok, Socket} ->
                case send(Socket, jsonrpc:format_request(0, Method, Params)) of
                    ok ->
                        gen_tcp:recv(Socket, 0, maps:get(timeout, Opts));
                    Error ->
                        Error
                end;
            Error -> Error
        end,
    {reply, Reply, State};

handle_call({make_req, Method, Params}, _From, #state{opts = #{timeout := Timeout}} = State) ->
    Request = jsonrpc:format_request(State#state.id, Method, Params),
    case send(State#state.socket, Request) of
        ok ->
            Response = gen_tcp:recv(State#state.socket, 0, Timeout),
            {reply, Response, State#state{id = State#state.id + 1}};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.

handle_continue(connect, #state{opts = #{retry_timeout := RetryTimeout}} = State) ->
    case connect(State) of
        {ok, Socket} ->
            {noreply, State#state{socket = Socket}};
        {error, _Reason} ->
            timer:sleep(RetryTimeout),
            {noreply, State, {continue, connect}}
    end.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #state{socket = Socket} = _State) ->
    case is_port(Socket) of
        true ->
            gen_tcp:close(Socket);
        false -> ok
    end.

connect(State) ->
    #{
        ip            := IP,
        port          := Port,
        send_timeout  := SendTimeout
    } = State#state.opts,
    SocketOpts = [
        binary,
        {active, false},
        {send_timeout, SendTimeout}
    ],
    case gen_tcp:connect(IP, Port, SocketOpts) of
        {ok, Socket} ->
            logger:info("connection is up"),
            {ok, Socket};
        {error, Reason} ->
            logger:error("can't connect ~p:~p, reason: ~s~n", [
                IP, Port, inet:format_error(Reason)
            ]),
            {error, Reason}
    end.

send(Socket, Request) ->
    try
        case erlang:port_command(Socket, Request, [nosuspend]) of
            false ->
                {error, socket_busy};
            true -> ok
        end
    catch
        _:Reason:_Stack ->
            {error, Reason}
    end.

make_opts(#{ip := IP, port := Port} = Args) ->
    #{
        ip            => IP,
        port          => Port,
        oneshot       => maps:get(oneshot, Args, false),
        timeout       => maps:get(response_timeout, Args, 5000),
        retry_timeout => maps:get(retry_timeout, Args, 5000),
        send_timeout  => maps:get(send_timeout, Args, 500)
    }.
