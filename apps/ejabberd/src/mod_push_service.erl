-module(mod_push_service).

-behaviour(gen_mod).

-export([start/2,
         init/2,
         stop/1,
         send_notification/3]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_post", []),
    register(?PROCNAME, spawn(?MODULE, init, [Host, Opts])),
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notification, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_offline_post", []),
    ejabberd_hooks:delete(offline_message_hook, Host,
                          ?MODULE, send_notification, 10),
    ok.

send_notification(From, To, Packet) ->
    ok.

create_notification() ->
    {alert, [{<<"test">>}]}.