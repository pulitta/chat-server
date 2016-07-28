-module(chat_server_sup).

-behaviour(supervisor).

-export([start_link/0,
		 start_link_sub/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, main).
start_link_sub() ->
    supervisor:start_link({local, worker_sub}, ?MODULE, sub).


init(main) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => chat_server_broker,
                    start => {chat_server_broker, start_link, [[]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => []},
    			 #{id => ?MODULE,
                    start => {?MODULE, start_link_sub, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => supervisor,
                    modules => [?MODULE]}],
    {ok, {SupFlags, ChildSpecs}};

init(sub) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    ChildSpecs = [#{id => undefined,
                    start => {chat_server_worker, start_link, [[]]},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.