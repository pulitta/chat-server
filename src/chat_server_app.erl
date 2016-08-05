-module(chat_server_app).

-behaviour(application).

-export([
    start/0,
    start/2, 
    stop/1 
]).

start() ->
    application:start(chat_server).

start(_, _) ->
	chat_server_sup:start_link(),
    start_listener().

stop(_) ->
    ok.

start_listener() ->
	Settings = application:get_all_env(chat_server),
    {ok, _} = ranch:start_listener(default_listener, 10,
        ranch_tcp, [{port, 1234}], chat_server_protocol, Settings).
