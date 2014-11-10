-module(mod_groupchat).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_GROUPCHAT, <<"aft:groupchat">>).

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_GROUPCHAT, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_GROUPCHAT).

%% @doc create group
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#3-%E5%88%9B%E5%BB%BA%E4%B8%80%E4%B8%AA%E7%BE%A4%E7%BB%84
create_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    GroupName = xml:get_tag_attr_s(<<"groupname">>, SubEl),
    case odbc_groupchat:create_group(LServer, UserJid, GroupName) of
        {ok, GroupId} ->
            Res = SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_GROUPCHAT}], children =
                                  [{xmlcdata, group_to_json(GroupName, GroupId, UserJid)}]},
            IQ#iq{type = result, sub_el = [Res]};
        {error, _} ->
            IQ#iq{type = error, sub_el = []}
    end.

group_to_json(GroupName, GroupId, Owner) ->
    Json = mochijson2:encode({struct, [{<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}, {<<"master">>, Owner}]}),
    iolist_to_binary(Json).

%% @doc add members
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#4-%E5%90%91%E7%BE%A4%E7%BB%84%E4%B8%AD%E6%B7%BB%E5%8A%A0%E6%88%90%E5%91%98
add_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    case odbc_groupchat:is_user_in_group(LServer, UserJid, GroupId) of
        true ->
            MembersList = mochijson2:decode(xml:get_tag_cdata(SubEl)),
            case MembersList of
                [] -> IQ#iq{type = error, sub_el = []};
                _ ->
                    case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                        {ok, MembersInfoList} when is_list(MembersInfoList) ->
                            case MembersInfoList of
                                [] ->
                                    IQ#iq{type = error, sub_el = []};
                                _ ->
                                    ExistsMembers = [Jid || {Jid, _} <- MembersInfoList],
                                    NewMembers = lists:filter(fun(X) ->
                                                                      not lists:member(X, ExistsMembers)
                                                              end, MembersList),
                                    do_add_members(LServer, GroupId, ExistsMembers, NewMembers, IQ, SubEl)
                            end;
                        {error, _} ->
                            IQ#iq{type = error, sub_el = []}
                    end
            end;
        _ ->
            IQ#iq{type = error, sub_el = []}
    end.

do_add_members(LServer, GroupId, ExistsMembers, NewMembers, IQ, SubEl) ->
    case NewMembers of
        [] ->
            IQ#iq{type = result, sub_el =
                      [SubEl#xmlel{
                         attrs = [{<<"xmlns">>, ?NS_GROUPCHAT}, {<<"groupid">>, GroupId},
                                  {<<"query_type">>, <<"add_member">>}],
                         children = []}]};
        _ ->
            case odbc_groupchat:add_members(LServer, GroupId, NewMembers) of
                {ok, MembersResult} ->
                    case odbc_groupchat:get_groupname_by_groupid(LServer, GroupId) of
                        {ok, GroupName} ->
                            push_groupmember(GroupId, GroupName, LServer,
                                             ExistsMembers ++ NewMembers, MembersResult, <<"add">>),
                            IQ#iq{type = result,
                                  sub_el = [SubEl#xmlel{attrs =
                                                            [{<<"xmlns">>, ?NS_GROUPCHAT},
                                                             {<<"groupid">>, GroupId},
                                                             {<<"query_type">>, <<"add_member">>}],
                                                        children = [{xmlcdata,
                                                                     members_to_json(MembersResult)}]}]};
                        {error, _} ->
                            IQ#iq{type = error, sub_el = []}
                    end;
                {error, _} ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc create and add members to group
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#5-%E5%88%9B%E5%BB%BA%E7%BE%A4%E7%BB%84%E5%90%8C%E6%97%B6%E6%B7%BB%E5%8A%A0%E6%88%90%E5%91%98
create_and_add(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    GroupName = xml:get_tag_attr_s(<<"groupname">>, SubEl),
    MembersList = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    case MembersList of
        [] ->
            IQ#iq{type = error, sub_el = []};
        _ ->
            case odbc_groupchat:create_and_add(LServer, GroupName, MembersList, UserJid) of
                {ok, GroupMembersInfo} ->
                    [{GroupId, _, _} | _] = GroupMembersInfo,
                    MembersInfoList = [{Jid, NickName} || {_, Jid, NickName} <- GroupMembersInfo],
                    push_groupmember(GroupId, GroupName, LServer, [Jid || {Jid, _} <- MembersInfoList], MembersInfoList, <<"add">>),
                    IQ#iq{type = result, sub_el = [SubEl#xmlel{
                                                     attrs = [{<<"xmlns">>, ?NS_GROUPCHAT},
                                                              {<<"groupid">>, GroupId},
                                                              {<<"query_type">>, <<"group_member">>},
                                                              {<<"groupname">>, GroupName},
                                                              {<<"master">>, UserJid}
                                                             ],
                                                     children = [{xmlcdata, members_to_json(MembersInfoList)}]}]};
                {error, _} ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc get_members by groupid
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#2-%E6%A0%B9%E6%8D%AE%E7%BE%A4%E7%BB%84-groupid-%E8%8E%B7%E5%8F%96%E7%BE%A4%E7%BB%84%E6%89%80%E6%9C%89%E6%88%90%E5%91%98
get_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    case odbc_groupchat:is_user_in_group(LServer, UserJid, GroupId) of
        true ->
            case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                {ok, MembersInfoList} ->
                    IQ#iq{type = result, sub_el = [SubEl#xmlel{children =
                                                                   [{xmlcdata, members_to_json(MembersInfoList)}]}]};
                {error, _} ->
                    IQ#iq{type = error, sub_el = []}
            end;
        _ ->
            IQ#iq{type = error, sub_el = []}
    end.

members_to_json(Members) ->
    JsonArray = [{struct, [{<<"userjid">>, UserJid}, {<<"nickname">>, NickName}]} || {UserJid, NickName} <- Members],
    iolist_to_binary(mochijson2:encode(JsonArray)).

%% @doc get groups by jid
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#1-%E8%8E%B7%E5%8F%96%E8%87%AA%E5%B7%B1%E7%9A%84%E6%89%80%E6%9C%89%E7%BE%A4%E7%BB%84
get_groups(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    case odbc_groupchat:get_groups_by_jid(LServer, UserJid) of
        {ok, Rs} when is_list(Rs) ->
            IQ#iq{type = result, sub_el = [SubEl#xmlel{children = [{xmlcdata, grouplist_to_json(Rs)}]}]};
        {error, _} ->
            IQ#iq{type = error, sub_el = []}
    end.

grouplist_to_json(List) ->
    JsonArray = [{struct, [{<<"groupid">>, GroupId},
                           {<<"groupname">>, GroupName},
                           {<<"master">>, Owner}]} || {GroupId, GroupName, Owner} <- List],
    iolist_to_binary(mochijson2:encode(JsonArray)).


%% @doc set group name
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#6-%E4%BF%AE%E6%94%B9%E7%BE%A4%E7%BB%84%E5%90%8D%E7%A7%B0
set_groupname(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
        {ok, MembersInfoList} ->
            MembersList = [Jid || {Jid, _} <- MembersInfoList],
            case lists:member(UserJid, MembersList) of
                true ->
                    GroupName = xml:get_tag_attr_s(<<"groupname">>, SubEl),
                    case odbc_groupchat:set_groupname(LServer, GroupId, GroupName) of
                        ok ->
                            push_groupinfo(GroupId, GroupName, LServer, MembersList, <<"rename">>),
                            Res = SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_GROUPCHAT},
                                                       {<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}]},
                            IQ#iq{type = result, sub_el = [Res]};
                        {error, _} ->
                            IQ#iq{type = error, sub_el = []}
                    end;
                false ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc remove members from group
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#8-%E5%88%A0%E9%99%A4%E6%88%90%E5%91%98%E5%8C%85%E6%8B%AC%E5%88%A0%E9%99%A4%E5%85%B6%E4%BB%96%E6%88%90%E5%91%98%E5%92%8C%E8%87%AA%E5%B7%B1%E9%80%80%E5%87%BA%E7%BE%A4
remove_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    MembersList = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    case odbc_groupchat:is_user_own_group(LServer, UserJid, GroupId) of
        true ->
            case odbc_groupchat:remove_members(LServer, GroupId, MembersList) of
                {ok, MembersInfoList} ->
                    case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                        {ok, RemainMembers} ->
                            RemainJid = [Jid || {Jid, _} <- RemainMembers],
                            push_groupmember(GroupId, <<>>, LServer, RemainJid ++ MembersList, MembersInfoList, <<"remove">>);
                        _ ->
                            push_groupmember(GroupId, <<>>, LServer, MembersList, MembersInfoList, <<"remove">>)
                    end,
                    IQ#iq{type = result, sub_el = [SubEl]};
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        _ ->
            case odbc_groupchat:is_user_in_group(LServer, UserJid, GroupId) of
                true ->
                    case MembersList of
                        [UserJid] ->
                            case odbc_groupchat:remove_members(LServer, GroupId, MembersList) of
                                ok ->
                                    IQ#iq{type = result, sub_el = [SubEl]};
                                _ ->
                                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
                            end;
                        _ ->
                            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
                    end;
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end
    end.

%% @doc dismiss group, must be owner of the group
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#7-%E8%A7%A3%E6%95%A3%E7%BE%A4%E7%BE%A4%E4%B8%BB
dismiss_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    case odbc_groupchat:is_user_own_group(LServer, UserJid, GroupId) of
        true ->
            case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                {ok, MembersInfoList} ->
                    case odbc_groupchat:dismiss_group(LServer, GroupId, MembersInfoList) of
                        ok ->
                            push_groupinfo(GroupId, <<>>, LServer, [Jid || {Jid, _} <- MembersInfoList], <<"dismiss">>),
                            IQ#iq{type = result, sub_el = [SubEl]};
                        _ ->
                            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
                    end;
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end.

%% @doc set nickname in group
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#9-%E7%BE%A4%E7%BB%84%E6%88%90%E5%91%98%E4%BF%AE%E6%94%B9%E7%BE%A4%E5%86%85%E6%98%B5%E7%A7%B0
set_nickname(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    NickName = xml:get_tag_attr_s(<<"nickname">>, SubEl),
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    case odbc_groupchat:set_nickname_in_group(LServer, GroupId, UserJid, NickName) of
        ok ->
            case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                {ok, MembersInfoList} ->
                    push_groupmember(GroupId, <<>>, LServer, [Jid || {Jid, _} <- MembersInfoList], [{UserJid, NickName}], <<"rename">>);
                _ -> nopush
            end,
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end.

push_groupmember(GroupId, GroupName, Server, ToList, MembersInfoList, Action) ->
    From = <<GroupId/binary, $@, Server/binary>>,
    LangAttr = {<<"xml:lang">>, <<"en">>},
    FromAttr = {<<"from">>, From},
    TypeAttr = {<<"type">>, <<"chat">>},
    Contents = groupmember_json(MembersInfoList, Action),
    Packet = {xmlel, <<"message">>, [],
              [{xmlel, <<"push">>, [{<<"xmlns">>, ?NS_GROUPCHAT}, {<<"type">>, <<"groupmember">>},
                                    {<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}],
                [{xmlcdata, Contents}]}
              ]},
    FromJid = jlib:make_jid(<<"aftgroup_", GroupId/binary>>, Server, <<>>),
    lists:foreach(fun(ToJid) ->
                          ToAttr = {<<"to">>, ToJid},
                          ejabberd_router:route(FromJid, jlib:binary_to_jid(ToJid),
                                                Packet#xmlel{attrs = [FromAttr, ToAttr, TypeAttr, LangAttr]}) end,
                  ToList).

groupmember_json(MembersInfoList, Action) ->
    JsonArray = [{struct, [{<<"jid">>, Jid}, {<<"nickname">>, NickName},
                           {<<"action">>, Action}]} || {Jid, NickName} <- MembersInfoList],
    iolist_to_binary(mochijson2:encode(JsonArray)).

push_groupinfo(GroupId, GroupName, Server, ToList, Action) ->
    From = <<GroupId/binary, $@, Server/binary>>,
    LangAttr = {<<"xml:lang">>, <<"en">>},
    FromAttr = {<<"from">>, From},
    TypeAttr = {<<"type">>, <<"chat">>},
    Contents = groupinfo_json(GroupId, GroupName, Action),
    Packet = {xmlel, <<"message">>, [],
              [{xmlel, <<"push">>, [{<<"xmlns">>, ?NS_GROUPCHAT}, {<<"type">>, <<"groupinfo">>}],
                [{xmlcdata, Contents}]}
              ]},
    FromJid = jlib:make_jid(<<"aftgroup_", GroupId/binary>>, Server, <<>>),
    lists:foreach(fun(ToJid) ->
                          ToAttr = {<<"to">>, ToJid},
                          ejabberd_router:route(FromJid, jlib:binary_to_jid(ToJid),
                                                Packet#xmlel{attrs = [FromAttr, ToAttr, TypeAttr, LangAttr]}) end,
                  ToList).

groupinfo_json(GroupId, GroupName, Action) ->
    JsonArray = [{struct, [{<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}, {<<"action">>, Action}]}],
    iolist_to_binary(mochijson2:encode(JsonArray)).

is_query_groupchat(Packet) ->
    case Packet of
        #xmlel{name = <<"query">>} -> true;
        _ -> false
    end.

process_iq(From, To, #iq{xmlns = ?NS_GROUPCHAT, type = _Type, sub_el = SubEl} = IQ) ->
    case is_query_groupchat(SubEl) of
        true ->
            case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
                <<"create_group">> ->
                    create_group(From, To, IQ);
                <<"add_member">> ->
                    add_members(From, To, IQ);
                <<"get_groups">> ->
                    get_groups(From, To, IQ);
                <<"get_members">> ->
                    get_members(From, To, IQ);
                <<"group_member">> ->
                    create_and_add(From, To, IQ);
                <<"set_groupname">> ->
                    set_groupname(From, To, IQ);
                <<"remove_members">> ->
                    remove_members(From, To, IQ);
                <<"dismiss_group">> ->
                    dismiss_group(From, To, IQ);
                <<"set_nickname">> ->
                    set_nickname(From, To, IQ);
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        false ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;

process_iq(_, _, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.