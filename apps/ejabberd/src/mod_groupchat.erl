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
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="create_group" groupname="FirstGroup"></query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="aad5a">
%% <query xmlns="aft:iq:groupchat">
%% {"groupid":"5","groupname":"FirstGroup","master":"13412345678@localhost"}</query>
%% </iq>
create_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
    GroupName = xml:get_tag_attr_s(<<"groupname">>, SubEl),
    case odbc_groupchat:create_group(LServer, UserJid, GroupName) of
        {ok, GroupId} ->
            Res = SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_GROUPCHAT}], children =
                                  [{xmlcdata, list_to_binary(group_to_json(GroupName, GroupId, UserJid))}]},
            IQ#iq{type = result, sub_el = [Res]};
        {error, _} ->
            IQ#iq{type = error, sub_el = []}
    end.

group_to_json(GroupName, GroupId, Owner) ->
    mochijson2:encode({struct, [{<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}, {<<"master">>, Owner}]}).

%% @doc add members
%% <iq from="13412345678@localhost/caoyue-PC" id="aad5a" type="set">
%% <query xmlns="aft:iq:groupchat" groupid="3" query_type="add_member">
%%    ["13411111111@localhost","13422222222@localhost"]</query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="add_member" groupid="1">
%%   [{"userjid":"13411111111@localhost","nickname":"张三"},{"userjid":"13422222222@localhost","nickname":"李四"}]
%% </query>
%% </iq>
add_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
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
                                                        children = [{xmlcdata, list_to_binary(
                                                                                 members_to_json(MembersResult))}]}]};
                        {error, _} ->
                            IQ#iq{type = error, sub_el = []}
                    end;
                {error, _} ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc create and add members to group
%% <iq from="13412345678@localhost/caoyue-PC" id="2115763" type="set">
%% <query xmlns="aft:iq:groupchat" query_type="group_member" groupname="FirstGroup">
%%   ["13411111111@localhost","13422222222@localhost"]</query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="2115763">
%% <query xmlns="aft:iq:groupchat" master="13412345678@localhost" query_type="group_member" groupid="33" groupname="FirstGroup">
%% [{"userjid":"13422222222@localhost","nickname":"n2"},{"userjid":"13411111111@localhost","nickname":"n1"}]</query>
%% </iq>
create_and_add(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
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
                                                     children = [{xmlcdata, list_to_binary(
                                                                              members_to_json(MembersInfoList))}]}]};
                {error, _} ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc get_members by groupid
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5ba">
%% <query xmlns="aft:iq:groupchat" query_type="get_members" groupid="1"></query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="aad5ba">
%% <query xmlns="aft:iq:groupchat" query_type="get_members" groupid="1">
%% [{"userjid":"13411111111@localhost","nickname":"default"}]</query>
%% </iq>
get_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    case odbc_groupchat:is_user_in_group(LServer, UserJid, GroupId) of
        true ->
            case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                {ok, MembersInfoList} ->
                    IQ#iq{type = result, sub_el = [SubEl#xmlel{children =
                                                                   [{xmlcdata, list_to_binary(members_to_json(MembersInfoList))}]}]};
                {error, _} ->
                    IQ#iq{type = error, sub_el = []}
            end;
        _ ->
            IQ#iq{type = error, sub_el = []}
    end.

members_to_json(Members) ->
    JsonArray = [{struct, [{<<"userjid">>, UserJid}, {<<"nickname">>, NickName}]} || {UserJid, NickName} <- Members],
    mochijson2:encode(JsonArray).

%% @doc get groups by jid
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5ba">
%% <query xmlns="aft:iq:groupchat" query_type="get_groups"></query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="aad5ba">
%% <query xmlns="aft:iq:groupchat" query_type="get_groups">
%% [{"groupid":"2","groupname":"FirstGroup","master":"13412345678@localhost"},
%% {"groupid":"3","groupname":"FirstGroup","master":"13412345678@localhost"},
%% {"groupid":"4","groupname":"FirstGroup","master":"13412345678@localhost"}]
%% </query>
%% </iq>
get_groups(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
    case odbc_groupchat:get_groups_by_jid(LServer, UserJid) of
        {ok, Rs} when is_list(Rs) ->
            IQ#iq{type = result, sub_el = [SubEl#xmlel{children = [{xmlcdata, list_to_binary(grouplist_to_json(Rs))}]}]};
        {error, _} ->
            IQ#iq{type = error, sub_el = []}
    end.

grouplist_to_json(List) ->
    case List of
        [] ->
            "[]";
        _ ->
            JsonArray = [{struct, [{<<"groupid">>, GroupId},
                                   {<<"groupname">>, GroupName},
                                   {<<"master">>, Owner}]} || {GroupId, GroupName, Owner} <- List],
            mochijson2:encode(JsonArray)
    end.

%% @doc set group name
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5a">
%%    <query xmlns="aft:iq:groupchat" query_type="set_groupname" groupid="1" groupname="NewName"></query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="aad5a">
%%    <query xmlns="aft:iq:groupchat" groupid="1" groupname="NewName"/>
%% </iq>
set_groupname(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    UserJid = <<LUser/binary, $@, LServer/binary>>,
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
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="remove_members" groupid="1">
%%     ["13411111111@localhost","13422222222@localhost"]</query>
%% </iq>
%%
%% <iq from="1341234578@localhost" type="result" to="1341234578@localhost/caoyue-PC" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="remove_members" groupid="1">
%%   ["13411111111@localhost","13422222222@localhost"]</query>
%% </iq>
remove_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    UserJid = <<LUser/binary, $@, LServer/binary>>,
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
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="dismiss_group" groupid="1"></query>
%% </iq>
%%
%% <iq from="1341234578@localhost" type="result" to="1341234578@localhost/caoyue-PC" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="dismiss_group" groupid="1"></query>
%% </iq>
dismiss_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    UserJid = <<LUser/binary, $@, LServer/binary>>,
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
%% <iq from="13412345678@localhost/caoyue-PC" type="set" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="set_nickname" groupid="3" nickname="testnick"
%%       userjid="13412345678@localhost"></query>
%% </iq>
%%
%% <iq from="13412345678@localhost" type="result" to="13412345678@localhost/caoyue-PC" id="aad5a">
%% <query xmlns="aft:iq:groupchat" query_type="set_nickname" groupid="3"
%%     nickname="testnick" userjid="13412345678@localhost"/>
%% </iq>
set_nickname(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    NickName = xml:get_tag_attr_s(<<"nickname">>, SubEl),
    UserJid = <<LUser/binary, $@, LServer/binary>>,
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
    FromString = <<<<"aftgroup_">>/binary, GroupId/binary, $@, Server/binary>>,
    Lang = {<<"xml:lang">>, <<"en">>},
    From = {<<"from">>, FromString},
    Type = {<<"type">>, <<"chat">>},
    Contents = groupmember_json(MembersInfoList, Action),
    Packet = {xmlel, <<"message">>, [],
              [{xmlel, <<"push">>, [{<<"xmlns">>, ?NS_GROUPCHAT}, {<<"type">>, <<"groupmember">>},
                                    {<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}],
                [{xmlcdata, list_to_binary(Contents)}]}
              ]},
    FromJid = jlib:make_jid(<<<<"aftgroup_">>/binary, GroupId/binary>>, Server, <<>>),
    lists:foreach(fun(ToJid) ->
                          ToAttr = {<<"to">>, ToJid},
                          ejabberd_router:route(FromJid, jlib:binary_to_jid(ToJid),
                                                Packet#xmlel{attrs = [From, ToAttr, Type, Lang]}) end,
                  ToList).

groupmember_json(MembersInfoList, Action) ->
    JsonArray = [{struct, [{<<"jid">>, Jid}, {<<"nickname">>, NickName},
                           {<<"action">>, Action}]} || {Jid, NickName} <- MembersInfoList],
    mochijson2:encode(JsonArray).

push_groupinfo(GroupId, GroupName, Server, ToList, Action) ->
    FromString = <<<<"aftgroup_">>/binary, GroupId/binary, $@, Server/binary>>,
    Lang = {<<"xml:lang">>, <<"en">>},
    From = {<<"from">>, FromString},
    Type = {<<"type">>, <<"chat">>},
    Contents = groupinfo_json(GroupId, GroupName, Action),
    Packet = {xmlel, <<"message">>, [],
              [{xmlel, <<"push">>, [{<<"xmlns">>, ?NS_GROUPCHAT}, {<<"type">>, <<"groupinfo">>}],
                [{xmlcdata, list_to_binary(Contents)}]}
              ]},
    FromJid = jlib:make_jid(<<<<"aftgroup_">>/binary, GroupId/binary>>, Server, <<>>),
    lists:foreach(fun(ToJid) ->
                          ToAttr = {<<"to">>, ToJid},
                          ejabberd_router:route(FromJid, jlib:binary_to_jid(ToJid),
                                                Packet#xmlel{attrs = [From, ToAttr, Type, Lang]}) end,
                  ToList).

groupinfo_json(GroupId, GroupName, Action) ->
    JsonArray = [{struct, [{<<"groupid">>, GroupId}, {<<"groupname">>, GroupName}, {<<"action">>, Action}]}],
    mochijson2:encode(JsonArray).

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