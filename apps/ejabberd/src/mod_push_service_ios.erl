%%%===================================================================
%%% @doc Implement Apple Push Notification Service (APNs)
%%% fetures:
%%% 1.Send notification to iOS device
%%% 2.Log error on fail
%%% 3.Get feedbck from apns and remove token record from server
%%%===================================================================

-module(mod_push_service_ios).

%% exports
-export([send/1, send/2]).

%% Define
-define(APNS_NAME, kissnapp_push).

-include("mod_push_service.hrl").
-include("mod_apns.hrl").


%%%===================================================================
%%% Functions
%%%===================================================================

-spec connect() -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: term()}.
connect() ->
    mod_apns:connect(?APNS_NAME,
        fun handle_apns_error/2,
        fun handle_apns_delete_subscription/1).

-spec send(Msg :: #apns_msg{}) -> ok.
send(Msg) ->
    case connect() of
        {ok, _} ->
            mod_apns:send_message(?APNS_NAME, Msg);
        {error, {already_started, _}} ->
            mod_apns:send_message(?APNS_NAME, Msg);
        {error, Reason} ->
            error_logger:error_msg("error: ~p ~n", [Reason])
    end.

-spec send(DeviceToken :: binary(), Content :: binary()) -> ok.
send(DeviceToken, Content) ->
    send(create_notification(DeviceToken, Content)).

handle_apns_error(MsgId, Status) ->
    error_logger:error_msg("apns send message error: ~p - ~p~n", [MsgId, Status]).

%% @doc delete token records when APNs feedback
handle_apns_delete_subscription(Data) ->
    error_logger:info_msg("delete apns subscription: ~p~n", [Data]),
    case Data of
        [_ | _] ->
            lists:foreach(fun(X) ->
                odbc_push_service:remove(?ODBC_SERVER, X)
            end, Data);
        _ ->
            none
    end.

-spec create_notification(DeviceToken :: binary(),
    Content :: binary()) -> ok.
create_notification(DeviceToken, Content) ->
    #apns_msg{
        alert = Content,
        badge = 1,
        sound = "chime",
        expiry = mod_apns:expiry(86400),
        device_token = binary_to_list(DeviceToken)
    }.