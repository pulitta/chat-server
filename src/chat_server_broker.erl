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
	new_client/2,
	update_client/2,
	message/2]).

-record(state, {
            clients,
            id = 0
        }).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

new_client(IPPort, Pid) ->
	gen_server:call(?MODULE, {new_client, IPPort, Pid}, 1000).

update_client(Id, {Param, Value}) ->
	gen_server:call(?MODULE, {update_client, Id, {Param, Value}}, 1000).

message(Id, Message) ->
	gen_server:call(?MODULE, {message, Id, Message}, 1000).

init(_Args) ->
    Clients = ets:new(clients, [protected, set]),
    {ok, #state{clients = Clients}}.

handle_call({new_client, IPPort, ConnectionPid}, _From, #state{clients = Clients, id = Id} = State) ->
	NewId = Id + 1,
    {ok, Pid} = supervisor:start_child(worker_sub, [#{connection_pid=>ConnectionPid}]),
    ok = gen_server:call(Pid, {ipport, IPPort}, 1000), 
    ets:insert(Clients, {NewId, Pid}),
    {reply, {ok, NewId}, State#state{id = NewId}};

handle_call({update_client, Id, ParamToUpdate}, _From, #state{clients = Clients} = State) ->
	case ets:select(Clients,[{{'$1','$2'},[{'/=', '$1', Id}],['$2']}]) of
		[Pid] ->
			ok = gen_server:call(Pid, ParamToUpdate, 1000);
		_ -> undefined
	end,
    {reply, ok, State};

handle_call({message, Id, Message}, _From, #state{clients = Clients} = State) ->
	Pids = ets:select(Clients,[{{'$1','$2'},[{'/=', '$1', Id}],['$2']}]),
	Datetime = calendar:local_time(),
	ok = lists:foreach(
		fun(Pid) -> 
			ok = gen_server:call(Pid, {message, {Datetime, Message}}, 1000)
		end,
	Pids),
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
