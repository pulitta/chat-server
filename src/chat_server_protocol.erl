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
            transport       :: ranch_tcp,
            client_id       :: integer
        }).

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}, {packet, 4}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Socket, transport=Transport},
        ?TIMEOUT).

handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
    case handle_data(Data, State) of
        {ok, NewState} -> {noreply, NewState, ?TIMEOUT};
        {error, Reason, NewState} -> {stop, {error, Reason}, NewState}
    end; 

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

% Internal
handle_data(<<"new">>, #state{socket = Socket} = State) ->
    {ok, {Ip, Port}} = inet:peername(Socket),
    case chat_server_broker:new_client({Ip, Port}) of
        {ok, Id} ->
            BinId = integer_to_binary(Id),
            gen_tcp:send(State#state.socket, <<"id ", BinId/binary>>),
            {ok, State#state{client_id = Id}};
        {error, Reason} -> {error, Reason, State}
    end;

handle_data(<<"id"," ",Id/binary>>, #state{socket = Socket} = State) ->
    IntId = binary_to_integer(Id),
    {ok, {Ip, Port}} = inet:peername(Socket),
    case chat_server_broker:update_client(IntId, {ipport, {Ip, Port}}) of
        ok -> {ok, State#state{client_id = IntId}};
        {error, Reason} -> {error, Reason, State}
    end;

handle_data(<<"setnick"," ",Nick/binary>>, #state{client_id = ClientId} = State) ->
    case chat_server_broker:update_client(ClientId, {nickname, Nick}) of
        ok -> {ok, State};
        {error, Reason} -> {error, Reason, State}
    end;

handle_data(Message, #state{client_id = ClientId} = State) ->
    chat_server_broker:message(ClientId, Message),
    {ok, State}.
    