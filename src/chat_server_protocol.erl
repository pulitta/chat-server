-module(chat_server_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([
    start_link/4
]).

-export([
    init/4,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3 
]).

-define(TIMEOUT, 300000).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

-record(state, {
            socket          :: gen_tcp:socket(), 
            transport       :: ranch_tcp
        }).

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}, {packet, 4}]),
    {ok, {Ip, Port}} = inet:peername(Socket),
    ok = chat_server_broker:connect({Ip, Port}),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Socket, transport=Transport},
        ?TIMEOUT).

handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
    {ok, {Ip, Port}} = inet:peername(Socket),
    io:format("Client's IP: ~p Port: ~p",[Ip, Port]),
    {noreply, State, ?TIMEOUT}; 

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.