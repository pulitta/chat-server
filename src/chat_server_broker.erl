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
	new_client/1,
	update_client/2]).

-record(state, {
            clients,
            id = 0
        }).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

new_client(IPPort) ->
	gen_server:call(?MODULE, {new_client, IPPort}, 1000).

update_client(Id, IPPort) ->
	gen_server:call(?MODULE, {update_client, Id, IPPort}, 1000).

init(Args) ->
    Clients = ets:new(clients, [protected, set]),
    {ok, #state{clients = Clients}}.

handle_call({new_client, IPPort}, From, #state{clients = Clients, id = Id} = State) ->
	NewId = Id + 1,
    {ok, Pid} = supervisor:start_child(worker_sub, []),
    ets:insert(Clients, {Id, IPPort, undefined, Pid}),
    {reply, {ok, NewId}, State#state{id = NewId}};

handle_call({update_client, Id, IPPort}, From, #state{clients = Clients} = State) ->
	case ets:lookup(Clients, Id) of
		[{_, _, Nickname, Pid}] -> 
			ets:delete(Clients, Id),
			ets:insert(Clients, {Id, IPPort, Nickname, Pid});
		_ -> undefined
    end,
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
