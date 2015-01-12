-module(mod_push_service).

-behaviour(gen_mod).

-export([start/2,
    init/2,
    stop/1]).

-export([send_notification/3, get_host_server/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push_service.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_push_service", []),
    register(?MODULE, spawn(?MODULE, init, [Host, Opts])),
    ok.

init(Host, _Opts) ->
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notification, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_push_service", []),
    ejabberd_hooks:delete(offline_message_hook, Host,
        ?MODULE, send_notification, 10),
    ok.

-spec send_notification(
    From :: jlib:jid(),
    ToJid :: jlib:jid(),
    Packet :: jlib:xmlel()) -> ok.
send_notification(From, To, Packet) ->
    #jid{luser = LUser, lserver = LServer} = To,
    ToJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    #jid{luser = LUserFrom, lserver = LServerFrom} = From,
    FromJid = jlib:jid_to_binary({LUserFrom, LServerFrom, <<>>}),
    case odbc_push_service:get_tokens_by_jid(LServer, ToJid) of
        {ok, [_ | _] = PushTokenList} ->
            lists:foreach(fun(PushToken) ->
                send_notification(PushToken, create_notification_content(FromJid, ToJid, Packet))
            end, PushTokenList);
        {error, _} ->
            none
    end.

-spec send_notification(#push_token{}, Content :: binary()) -> {ok | not_implement}.
send_notification(#push_token{type = PushType, token = DeviceToken}, Content) ->
    case PushType of
        <<"1">> -> %% iOS push notification
            mod_push_service_ios:send(binary_to_list(DeviceToken), Content);
        _ -> %% Android or other
            not_implement
    end.

-spec create_notification_content(FromJid :: binary(),
    ToJid :: binary(),
    _Packet :: jlib:xmlel()) -> ok.
create_notification_content(FromJid, ToJid, _Packet) ->
    %TODO
    #push_message{
        type = 1,
        content = <<"Hello, welcome to use kissnapp!">>,
        from = FromJid,
        to = ToJid
    }.

get_host_server() ->
    case ejabberd_config:get_global_option(hosts) of
        [Host | _] ->
            Host;
        _ ->
            <<"localhost">>
    end.