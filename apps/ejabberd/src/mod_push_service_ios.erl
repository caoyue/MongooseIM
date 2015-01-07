-module(mod_push_service_ios).

-export([send/1, send/2, send_test/1]).

%% define
-define(APNS_NAME, kissnapp_push).
-define(KISSNAP_CONNECTION, #apns_connection{
    apple_host = "gateway.sandbox.push.apple.com",
    apple_port = 2195,
    cert = undefined,
    cert_file = "priv/cert.pem",
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

-include_lib("apns/include/apns.hrl").

-spec connect() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: term()}.
connect() ->
    apns:connect(?APNS_NAME, ?KISSNAP_CONNECTION).

-spec send(Msg :: #apns_msg{}) -> ok.
send(Msg) ->
    connect(),
    apns:send_message(?APNS_NAME, Msg).

-spec send(DeviceToken :: binary(), Content :: binary()) -> ok.
send(DeviceToken, Content) ->
    send(create_notification(DeviceToken, Content)).

-spec send_test(Content :: binary()) -> ok.
send_test(Content) ->
    TestToken = <<"ff496f96352abb7c875bedfc755287f0bf72e14bfadade9ff6ba75360de65441">>,
    send(TestToken, Content).

handle_apns_error(MsgId, Status) ->
    error_logger:error_msg("error: ~p - ~p~n", [MsgId, Status]).

handle_apns_delete_subscription(Data) ->
    error_logger:info_msg("delete subscription: ~p~n", [Data]).

%% =========================
%% function
%% =========================

-spec create_notification(Token :: binary(),
    Content :: binary()) -> ok.
create_notification(DeviceToken, Content) ->
    #apns_msg{
        alert = Content,
        badge = 1,
        sound = "chime",
        expiry = apns:expiry(86400),
        device_token = DeviceToken
    }.

