-module(chat_server_worker).

-behaviour(gen_server).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([start_link/1]).

-define(CHECK_TIMER, 100).
-define(FIRST_NULL(N),  if 
    						(N >= 0) and (N =< 9) -> io_lib:format("0~p", [N]);
    						true -> integer_to_list(N)
    					end
    	).

-record(state, {
			connection_pid,
			timer,
            messages,
            ip,
            port,
            nickname
        }).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(#{connection_pid:=ConnectionPid}) ->
	Messages = queue:new(),
	TimerRef = erlang:send_after(1000, self(), check_queue),
    {ok, #state{messages = Messages, timer = TimerRef, connection_pid = ConnectionPid}}.

handle_call({ipport, {Ip, Port}}, _From, State) ->
    {reply, ok, State#state{ip = Ip, port = Port}};

handle_call({nickname, Nickname}, _From, State) ->
    {reply, ok, State#state{nickname = Nickname}};

handle_call({message, {Datetime, Message}}, _From, #state{messages = Messages} = State) ->
	NewMessages = queue:in({Datetime, Message}, Messages),
    {reply, ok, State#state{messages = NewMessages}};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_queue, #state{timer = TimerRef, 
								messages = Messages, 
								connection_pid = ConnectionPid, 
								ip = Ip, 
								port = Port,
								nickname = Nickname} = State) ->
    erlang:cancel_timer(TimerRef),
    OtherMessages = case queue:out(Messages) of
    	{empty,_} -> Messages;
    	{{value,{{_, {H,M,S}}, Message}},Other} ->
    		IpOrNick = case Nickname of
    			undefined -> 
    				{A,B,C,D} = Ip,
    				io_lib:format("~p.~p.~p.~p:~p", [A,B,C,D, Port]);
    			_ -> Nickname
    		end,
    		ok = gen_server:call(ConnectionPid, {send, io_lib:format("[~s:~s](~s): <~s> ~n", [?FIRST_NULL(H), ?FIRST_NULL(M), IpOrNick, Message])}, 3000),
    		Other
   	end,
    NewTimerRef = erlang:send_after(?CHECK_TIMER, self(), check_queue),
    {noreply, State#state{timer = NewTimerRef, messages = OtherMessages}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
