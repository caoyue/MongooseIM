-module(mod_groupchat).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3, get_members_api/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  <<"jabber:iq:aft_groupchat">>, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, <<"jabber:iq:aft_groupchat">>).

is_query_groupchat(Packet) ->
    case Packet of
        #xmlel{name = <<"query">>} -> true;
        _ -> false
    end.

aft_query_type(SubEl) ->
    case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
        <<"aft_get_members">> -> aft_get_members;
        <<"aft_get_groups">> -> aft_get_groups;
        <<"aft_group_member">> -> aft_group_member;
        <<"aft_create_group">> -> aft_create_group;
        <<"aft_add_member">> -> aft_add_member;
        _ -> undefined
    end.

%% @doc create group
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/caoyue-PC" type="set" id="aad5a">
%%     <query xmlns="jabber:iq:aft_groupchat" query_type="aft_create_group">FirstGroup</query>
%% </iq>
%%
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167" type="result"
%%      to="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="aad5a">
%%  <query xmlns="jabber:iq:aft_groupchat" query_type="aft_create_group" groupid="1">FirstGroup</query>
%% </iq>
create_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
    GroupName = xml:get_tag_cdata(SubEl),
    case odbc_groupchat:create_group(LServer, UserJid, GroupName) of
        {atomic, {selected, _, [{ResultId}]}} ->
            Res = SubEl#xmlel{attrs = [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>}, {<<"groupid">>, ResultId}]},
            IQ#iq{type = result, sub_el = [Res]};
        _ ->
            IQ#iq{type = error, sub_el = []}
    end.

%% @doc add members
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="2115763" type="set">
%% <query xmlns="jabber:iq:aft_groupchat" groupid="1" query_type="aft_add_member">
%%     ["08aa4f13-b3a3-49d2-820c-849d1a9c1bb7@192.168.1.167","cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167"]
%% </query>
%% </iq>
%%
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167" type="result"
%%       to="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="2115763">
%%    <query xmlns="jabber:iq:aft_groupchat" query_type="aft_add_member" groupid="1">
%%      ["08aa4f13-b3a3-49d2-820c-849d1a9c1bb7@192.168.1.167","cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167"]
%%    </query>
%% </iq>
add_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = _, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    Members = binary_to_list(xml:get_tag_cdata(SubEl)),
    MembersList = string:tokens(Members, "\n ,\"[]"),
    case MembersList of
        [] -> IQ#iq{type = error, sub_el = []};
        _ ->
            case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
                {selected, [<<"jid">>], Rs} when is_list(Rs) ->
                    case Rs of
                        [] ->
                            IQ#iq{type = error, sub_el = []};
                        _ ->
                            ExistsMembers = [binary_to_list(X) || {X} <- Rs],
                            NewMembers = lists:filter(fun(X) ->
                                                              not lists:member(X, ExistsMembers)
                                                      end, MembersList),
                            case NewMembers of
                                [] ->
                                    IQ#iq{type = result, sub_el =
                                              [SubEl#xmlel{
                                                 attrs = [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>}, {<<"groupid">>, GroupId},
                                                          {<<"query_type">>, <<"aft_add_member">>}]}]};
                                _ ->
                                    case odbc_groupchat:add_members(LServer, GroupId, NewMembers) of
                                        {atomic, _} ->
                                            {selected, [<<"name">>], [{GroupName}]} =
                                                odbc_groupchat:get_groupname_by_groupid(LServer, GroupId),
                                            ExistsMembersJid = get_user_from_jid(ExistsMembers, []),
                                            NewMembersJid = get_user_from_jid(NewMembers,[]),
                                            push_event_message(GroupId, GroupName, LServer, ExistsMembersJid ++ NewMembersJid, NewMembersJid),
                                            IQ#iq{type = result,
                                                  sub_el = [SubEl#xmlel{attrs =
                                                                            [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>},
                                                                             {<<"groupid">>, GroupId},
                                                                             {<<"query_type">>, <<"aft_add_member">>}]}]};
                                        _ ->
                                            IQ#iq{type = error, sub_el = []}
                                    end
                            end
                    end;
                _ ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc create and add members to group
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="2115763" type="set">
%%   <query xmlns="jabber:iq:aft_groupchat" query_type="aft_group_member" nickname="FirstGroup">
%%     ["08aa4f13-b3a3-49d2-820c-849d1a9c1bb7@192.168.1.167","cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167"]
%%   </query>
%% </iq>
%%
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167" type="result"
%%       to="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/caoyue-PC" id="2115763">
%%   <query xmlns="jabber:iq:aft_groupchat" query_type="aft_group_member" groupid="1" nickname="FirstGroup">
%%     ["08aa4f13-b3a3-49d2-820c-849d1a9c1bb7@192.168.1.167","cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167"]
%%   </query>
%% </iq>
create_and_add(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = <<LUser/binary, $@, LServer/binary>>,
    GroupName = xml:get_tag_attr_s(<<"nickname">>, SubEl),
    MembersList = string:tokens(binary_to_list(xml:get_tag_cdata(SubEl)), "\n ,\"[]"),
    case MembersList of
        [] ->
            IQ#iq{type = error, sub_el = []};
        _ ->
            case odbc_groupchat:create_and_add(LServer, GroupName, MembersList, UserJid) of
                {atomic, {selected, _, [{ResultId}]}} ->
                    MembersJid = [{LUser, LServer} | get_user_from_jid(MembersList, [])],
                    push_event_message(ResultId, GroupName, LServer, MembersJid, MembersJid),
                    IQ#iq{type = result, sub_el = [SubEl#xmlel{
                                                     attrs = [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>},
                                                              {<<"groupid">>, ResultId},
                                                              {<<"query_type">>, <<"aft_group_member">>},
                                                              {<<"nickname">>, GroupName}
                                                             ]}]};
                _ ->
                    IQ#iq{type = error, sub_el = []}
            end
    end.

%% @doc get_members by groupid
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="aad5a" type="get">
%%   <query xmlns="jabber:iq:aft_groupchat" query_type="aft_get_members" groupid="1"/>
%% </iq>
%%
%% <iq to="jid" id="2115763" type="result">
%%   <query xmlns="jabber:iq:aft_groupchat" query_type="aft_get_members">
%%     ["cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167","cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167"]
%%   </query>
%% </iq>
get_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = _LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
    case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
        {selected, [<<"jid">>], Members} ->
            Result = string:join([binary_to_list(X) || {X} <- Members], "\",\""),
            IQ#iq{type = result, sub_el = [SubEl#xmlel{children =
                                                           [{xmlcdata, list_to_binary("[\"" ++ Result ++ "\"]")}]}]};
        _ ->
            IQ#iq{type = error, sub_el = []}
    end.

get_members_api(LServer, GroupId) ->
    case odbc_groupchat:get_members_by_groupid(LServer, GroupId) of
        {selected, [<<"jid">>], Members} ->
            R = [jlib:binary_to_jid(X) || {X} <- Members],
            {ok, [{U, S} || {jid, U, S, _, _, _, _} <- R]};
        _ ->
            {error, notexist}
    end.

%% @doc get groups by jid
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="aad5a" type="get">
%%    <query xmlns="jabber:iq:aft_groupchat" query_type="aft_get_groups"/>
%% </iq>
%% <iq from="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167" type="result"
%%       to="cc4bc427-eeaa-41eb-84a7-f713c0205a9f@192.168.1.167/resource" id="aad5a">
%%   <query xmlns="jabber:iq:aft_groupchat" query_type="aft_get_groups">
%%     [{"jid":"1","nickname":"FirstGroup"},{"jid":"2","nickname":"SecondGroup"}]
%%   </query>
%% </iq>
get_groups(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = list_to_binary(binary_to_list(LUser) ++ "@" ++ binary_to_list(LServer)),
    case odbc_groupchat:get_groups_by_jid(LServer, UserJid) of
        {selected, [<<"groupid">>, <<"name">>,<<"owner">>], Rs} when is_list(Rs) ->
            IQ#iq{type = result, sub_el = [SubEl#xmlel{children = [{xmlcdata,list_to_binary(grouplist_to_json(Rs))}]}]};
        _ ->
            IQ#iq{type = error, sub_el = []}
    end.

grouplist_to_json(List) ->
    JsonArray = [ {struct,[{<<"jid">>, GroupId},
                           {<<"nickname">>,GroupName},
                           {<<"master">>,Owner}]} || {GroupId,GroupName,Owner} <- List],
    mochijson2:encode(JsonArray).

push_event_message(GroupId, Nickname, Server, ToList, MemberList) ->
    FromString = <<<<"aftgroup_">>/binary, GroupId/binary, $@, Server/binary>>,
    Lang = {<<"xml:lang">>, <<"en">>},
    From = {<<"from">>, FromString},
    Type = {<<"type">>, <<"aft_groupchat">>},
    Push = {<<"push">>, <<"true">>},
    Contents = event_member_json(MemberList, <<"add">>),
    Packet = {xmlel, <<"message">>, [],
              [{xmlcdata, <<"\n     ">>},
               {xmlel, <<"body">>, [{<<"groupid">>, GroupId}, {<<"nickname">>, Nickname}],
                [{xmlcdata, list_to_binary(Contents)}]},
               {xmlcdata, <<"\n">>}]},
    FromJID = jlib:make_jid(<<<<"aftgroup_">>/binary, GroupId/binary>>, Server, <<"">>),
    lists:foreach(fun({U, S}) ->
                          ToJID = jlib:make_jid(U, S, <<"">>),
                          ToAttr = {<<"to">>, <<U/binary, $@, S/binary>>},
                          ejabberd_router:route(FromJID, ToJID,
                                                Packet#xmlel{attrs = [From, ToAttr, Type, Lang, Push]}) end,
                  ToList).

event_member_json(MemberList,Action) ->
    JsonArray = [ {struct,[{<<"jid">>, <<U/binary,$@,S/binary>>},{<<"action">>,Action}]} || {U,S} <- MemberList],
    mochijson2:encode(JsonArray).

get_user_from_jid([H | R], Result) ->
    #jid{luser = U, lserver = S} = jlib:binary_to_jid(list_to_binary(H)),
    get_user_from_jid(R, [{U, S} | Result]);
get_user_from_jid([], Result) ->
    Result.

process_iq(From, To, #iq{xmlns = <<"jabber:iq:aft_groupchat">>, type = _Type, sub_el = SubEl} = IQ) ->
    case is_query_groupchat(SubEl) of
        true ->
            case aft_query_type(SubEl) of
                aft_create_group ->
                    create_group(From, To, IQ);
                aft_add_member ->
                    add_members(From, To, IQ);
                aft_get_groups ->
                    get_groups(From, To, IQ);
                aft_get_members ->
                    get_members(From, To, IQ);
                aft_group_member ->
                    create_and_add(From, To, IQ);
                undefined ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        false -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;

process_iq(_, _, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.