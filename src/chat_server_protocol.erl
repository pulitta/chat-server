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
            socket                  :: gen_tcp:socket(), 
            transport               :: ranch_tcp,
            client_id               :: integer,
            max_message_length      :: integer,
            bad_words               :: list,
            history_message_count   :: integer
        }).

init(Ref, Socket, Transport, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}, binary]),
    MaxLength = proplists:get_value(max_message_length, Opts),
    BadWords = proplists:get_value(bad_words, Opts),
    HistoryMessageCount = proplists:get_value(history_message_count, Opts),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Socket, 
               transport=Transport, 
               max_message_length=MaxLength,
               bad_words = BadWords,
               history_message_count = HistoryMessageCount},
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

handle_call({send, Data}, _From, State) ->
    gen_tcp:send(State#state.socket, Data),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

terminate(_Reason, #state{client_id = ClientId}) ->
    ok = chat_server_broker:update_client(ClientId, {connection_pid, undefined}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal
handle_data(<<"new\r\n">>, #state{socket = Socket, history_message_count = HistoryMessageCount} = State) ->
    {ok, {Ip, Port}} = inet:peername(Socket),
    case chat_server_broker:new_client({Ip, Port}, self(), HistoryMessageCount) of
        {ok, Id} ->
            BinId = integer_to_binary(Id),
            gen_tcp:send(State#state.socket, <<"id ", BinId/binary, "\n">>),
            {ok, State#state{client_id = Id}};
        {error, Reason} -> {error, Reason, State}
    end;

handle_data(<<"id"," ",Id/integer, "\r\n">>, #state{socket = Socket} = State) ->
    IntId = binary_to_integer(<<Id>>),
    {ok, {Ip, Port}} = inet:peername(Socket),
    ok = chat_server_broker:update_client(IntId, {ipport, {Ip, Port}}),
    ok = chat_server_broker:update_client(IntId, {connection_pid, self()}),
    {ok, State#state{client_id = IntId}};

handle_data(<<"setnick <", Nick/binary>>, #state{client_id = ClientId} = State) ->
    Nick1 = binary:replace(Nick, <<"\r">>, <<"">>),
    Nick2 = binary:replace(Nick1, <<"\n">>, <<"">>),
    Nick3 = binary:replace(Nick2, <<">">>, <<"">>),
    case chat_server_broker:update_client(ClientId, {nickname, Nick3}) of
        ok -> {ok, State};
        {error, Reason} -> {error, Reason, State}
    end;

handle_data(Message, #state{client_id = ClientId, max_message_length = MaxLength, bad_words = BadWords} = State) ->
    Message1 = binary:replace(Message, <<"\r">>, <<"">>),
    Message2 = binary:replace(Message1, <<"\n">>, <<"">>),
    MessageList = binary_to_list(Message2),
    case kick_client(Message2, BadWords) of
        true ->
            {error, bad_word_found, State};
        _ ->  
            NewMessage = if 
                MaxLength < length(MessageList) ->
                    io_lib:format("~s [cut. message too long]", [string:substr(MessageList, 1, MaxLength)]);
                true ->
                    MessageList
            end,
            chat_server_broker:message(ClientId, NewMessage),
            {ok, State}
    end.
    
kick_client(_, []) -> false;
kick_client(Message, [BadWord|Other]) ->
    case re:run(Message,BadWord,[]) of
        {match, _} -> 
            Value = rand:uniform(),
            if 
                Value =< 0.1 -> true;
                true -> false
            end;
        nomatch -> kick_client(Message, Other)
    end.
