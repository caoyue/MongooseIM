-module(mod_push_service_ios).

-export([send/1, send/2]).

%% define
-define(APNS_NAME, kissnapp_push).
-define(KISSNAP_CONNECTION, #apns_connection{
    apple_host = "gateway.sandbox.push.apple.com",
    apple_port = 2195,
    cert = undefined,
    cert_file = "priv/kissnapp.pem",
    key = undefined,
    key_file = undefined,
    cert_password = undefined,
    timeout = 30000,
    error_fun = fun handle_apns_error/2,
    feedback_host = "feedback.sandbox.push.apple.com",
    feedback_port = 2196,
    feedback_fun = fun handle_apns_delete_subscription/1,
    feedback_timeout = 30 * 60 * 1000
}).
-define(LSERVER, <<"localhost">>).

-include_lib("apns/include/apns.hrl").

-spec connect() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: term()}.
connect() ->
    apns:connect(?APNS_NAME, ?KISSNAP_CONNECTION).

-spec send(Msg :: #apns_msg{}) -> ok.
send(Msg) ->
    connect(),
    apns:send_message(?APNS_NAME, Msg).

-spec send(DeviceToken :: string(), Content :: binary()) -> ok.
send(DeviceToken, Content) ->
    send(create_notification(DeviceToken, Content)).

handle_apns_error(_MsgId, _Status) ->
    %error_logger:error_msg("error: ~p - ~p~n", [MsgId, Status]).
    none.


%% @doc delete token records when APNs feedback
handle_apns_delete_subscription(Data) ->
    error_logger:info_msg("delete subscription: ~p~n", [Data]),
    case Data of
        [_ | _] ->
            lists:foreach(fun(X) ->
                odbc_push_service:remove(?LSERVER, X)
            end, Data);
        _ ->
            none
    end.

%% ===========================================
%% functions
%% ===========================================

-spec create_notification(DeviceToken :: string(),
    Content :: binary()) -> ok.
create_notification(DeviceToken, Content) ->
    #apns_msg{
        alert = Content,
        badge = 1,
        sound = "chime",
        expiry = apns:expiry(86400),
        device_token = DeviceToken
    }.

