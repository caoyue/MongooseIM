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
    case extract_message(From, To, Packet) of
        not_valid_message -> none;
        #push_message{
            lserver = LServer,
            tojid = ToJid
        } = Message ->
            PushContent = message_substring(make_push_message(Message)),
            
            case odbc_push_service:get_tokens_by_jid(LServer, ToJid) of
                {ok, [_ | _] = PushTokenList} ->
                    lists:foreach(fun(PushToken) ->
                        send_notification(PushToken, PushContent)
                    end, PushTokenList);
                _ ->
                    none
            end
    end.

-spec send_notification(#push_token{}, Content :: binary()) -> {ok | not_implement}.
send_notification(#push_token{type = PushType, token = DeviceToken}, Content) ->
    case PushType of
        <<"1">> -> %% iOS push notification
            mod_push_service_ios:send(binary_to_list(DeviceToken), Content);
        _ -> %% Android or other
            not_implemented
    end.

%% ========================================================
%% Message Functions
%% ========================================================

-spec extract_message(
    From :: jlib:jid(),
    To :: jlib:jid(),
    Packet :: jlib:xmlel()) -> {#push_message{} | not_valid_message}.
extract_message(From, To, Packet) ->
    #jid{luser = LUser, lserver = LServer} = To,
    ToJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    #jid{luser = LUserFrom, lserver = LServerFrom} = From,
    FromJid = jlib:jid_to_binary({LUserFrom, LServerFrom, <<>>}),

    InfoTag = xml:get_subtag(Packet, <<"info">>),
    case InfoTag of
        false ->
            not_valid_message;
        _ ->
            case xml:get_tag_attr_s(<<"xmlns">>, InfoTag) of
                <<"aft:message">> ->
                    Message = case {xml:get_tag_attr_s(<<"groupChat">>, InfoTag),
                        xml:get_subtag(InfoTag, <<"sender">>)} of
                                  {<<"1">>, false} ->
                                      not_valid_message;
                                  {<<"1">>, SenderTag} ->
                                      Sender = xml:get_tag_cdata(SenderTag),
                                      #jid{luser = SenderUser} = jlib:binary_to_jid(Sender),
                                      #push_message{
                                          lserver = LServer,
                                          fromuser = SenderUser,
                                          fromjid = Sender,
                                          touser = LUser,
                                          tojid = ToJid,
                                          groupid = LUserFrom,
                                          content = undefined
                                      };
                                  {<<"0">>, _} ->
                                      #push_message{
                                          lserver = LServer,
                                          fromuser = LUserFrom,
                                          fromjid = FromJid,
                                          touser = LUser,
                                          tojid = ToJid,
                                          groupid = undefined,
                                          content = undefined
                                      }
                              end,
                    case Message of
                        not_valid_message -> not_valid_message;
                        _ ->
                            case xml:get_tag_attr_s(<<"type">>, InfoTag) of
                                <<"0">> ->
                                    case xml:get_subtag(InfoTag, <<"text">>) of
                                        false -> not_valid_message;
                                        TextTag ->
                                            Content = xml:get_tag_cdata(TextTag),
                                            Message#push_message{
                                                type = text,
                                                content = Content
                                            }
                                    end;
                                <<"1">> ->
                                    Message#push_message{
                                        type = audio
                                    };
                                <<"2">> ->
                                    Message#push_message{
                                        type = video
                                    };
                                <<"3">> ->
                                    Message#push_message{
                                        type = picture
                                    };
                                <<"4">> ->
                                    Message#push_message{
                                        type = location
                                    };
                                _ ->
                                    Message#push_message{
                                        type = undefined
                                    }
                            end
                    end;
                _ ->
                    not_valid_message
            end
    end.

-spec make_push_message(
    #push_message{}) -> binary().
make_push_message(#push_message{
    lserver = LServer,
    fromjid = FromJid,
    touser = ToUser,
    type = MessageType,
    content = Content,
    groupid = GroupId
}) ->
    {NickName, GroupTitle} = case GroupId of
                                 undefined ->
                                     {get_roster_nick(LServer, FromJid, ToUser), <<>>};
                                 _ ->
                                     GroupName = get_group_name(LServer, GroupId),
                                     {get_group_nick(LServer, FromJid, GroupId), <<"[", GroupName/binary, "]">>}
                             end,
    HeadTitle = <<GroupTitle/binary, NickName/binary>>,
    case MessageType of
        text ->
            case Content of
                undefined ->
                    <<HeadTitle/binary, " send a message to you">>;
                _ ->
                    <<HeadTitle/binary, ": ", Content/binary>>
            end;
        audio ->
            <<HeadTitle/binary, " send an audio to you">>;
        video ->
            <<HeadTitle/binary, " send a video to you">>;
        picture ->
            <<HeadTitle/binary, " send a photo to you">>;
        location ->
            <<HeadTitle/binary, " send location to you">>;
        _ ->
            <<HeadTitle/binary, " send a message to you">>
    end.

-spec get_roster_nick(LServer :: binary(), FromJid :: binary(), ToJid :: binary()) -> binary().
get_roster_nick(LServer, FromJid, ToUser) ->
    case odbc_push_service:get_roster_nick(LServer, FromJid, ToUser) of
        {ok, Name} -> Name;
        _ -> <<"unkonw">>
    end.

-spec get_group_nick(LServer :: binary(), ToJid :: binary(), GroupId :: binary()) -> binary().
get_group_nick(LServer, FromJid, GroupId) ->
    case odbc_push_service:get_group_nick(LServer, FromJid, GroupId) of
        {ok, Name} -> Name;
        _ -> <<"unknow">>
    end.

get_group_name(LServer, GroupId) ->
    case odbc_push_service:get_group_name(LServer, GroupId) of
        {ok, Name} -> Name;
        _ -> <<"unknow">>
    end.

-spec message_substring(Content :: binary()) -> binary().
message_substring(Content) ->
    Size = byte_size(Content),
    case Size > 100 of
        true ->
            SubContent = binary:part(Content, {0, 100}),
            <<SubContent/binary, "...">>;
        false ->
            binary:part(Content, {0, Size})
    end.

get_host_server() ->
    case ejabberd_config:get_global_option(hosts) of
        [Host | _] ->
            Host;
        _ ->
            <<"localhost">>
    end.