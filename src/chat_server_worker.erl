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

-record(state, {
            messages
        }).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
	Messages = queue:new(),
    {ok, #state{messages = Messages}}.

handle_call({message, Message}, _From, #state{messages = Messages}) ->
	NewMessages = queue:in(Message, Messages),
    {reply, ok, #state{messages = NewMessages}};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
