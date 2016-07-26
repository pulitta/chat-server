-module(chat_server_broker).

-behaviour(gen_server).

-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

-export([
	start_link/1,
	connect/1]).

-record(state, {
            clients
        }).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

connect(IPPort) ->
	gen_server:call(?MODULE, {connect, IPPort}, 1000).

init(Args) ->
    Clients = ets:new(clients, [protected, set]),
    {ok, #state{clients = Clients}}.

handle_call({connect, IPPort}, From, #state{clients = Clients} = State) ->
    {ok, Pid} = supervisor:start_child(worker_sub, []),
    ets:insert(Clients, {IPPort, undefined, Pid}),
    {reply, ok, State};

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
