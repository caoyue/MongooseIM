-module(mod_groupchat).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3]).

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

create_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
  #jid{luser = LUser, lserver = LServer} = From,
  UserJid = list_to_binary(binary_to_list(LUser) ++ "@" ++ binary_to_list(LServer)),
  GroupName = list_to_binary(string:strip(string:strip(binary_to_list(xml:get_tag_cdata(SubEl)), both, 32), both, $\n)),
  F = fun() ->
    ejabberd_odbc:sql_query_t([<<"insert into groupinfo(name,owner) values('">>, GroupName, "','", UserJid, "');"]),
    Result = ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]),
    ejabberd_odbc:sql_query_t([<<"insert into groupuser(groupid,jid) values(last_insert_id(),'">>, UserJid, "');"]),
    Result
  end,
  case ejabberd_odbc:sql_transaction(LServer, F) of
    {atomic, {selected, _, [{ResultId}]}} ->
      Res = SubEl#xmlel{attrs = [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>}, {<<"groupid">>, ResultId}]},
      IQ#iq{type = result, sub_el = [Res]};
    _ ->
      IQ#iq{type = error, sub_el = []}
  end.

add_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
  #jid{luser = _, lserver = LServer} = From,
  GroupID = xml:get_tag_attr_s(<<"groupid">>, SubEl),
  MemberBinary = binary_to_list(xml:get_tag_cdata(SubEl)),
  MembersList = string:tokens(MemberBinary, "\n ,\"[]"),
  case MembersList of
    [] -> IQ#iq{type = error, sub_el = []};
    _ ->
      case ejabberd_odbc:sql_query(
        LServer,
        ["select jid from groupuser where groupid = '", GroupID, "';"]) of
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
                  IQ#iq{type = result, sub_el = [SubEl#xmlel{
                    attrs = [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>}, {<<"groupid">>, GroupID},
                      {<<"query_type">>, <<"aft_add_member">>}]}]};
                _ ->
                  AddQuery = make_add_query(NewMembers, [], GroupID),
                  case ejabberd_odbc:sql_transaction(LServer, AddQuery) of
                    {atomic, _} ->
                      {selected, [<<"name">>], [{GroupName}]} = ejabberd_odbc:sql_query(
                        LServer,
                        ["select name from groupinfo where groupid = '", GroupID, "';"]),
                      if length(NewMembers) > 0 ->
                        MembersJid = get_us_from_jid(NewMembers ++ ExistsMembers, []),
                        push_event_message(GroupID, GroupName, LServer, MembersJid, get_us_from_jid(NewMembers, []));
                        true -> non_push
                      end,
                      IQ#iq{type = result, sub_el = [SubEl#xmlel{
                        attrs = [{<<"xmlns">>, <<"jabber:iq:aft_groupchat">>}, {<<"groupid">>, GroupID},
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

create_and_add(From, _To, #iq{sub_el = SubEl} = IQ) ->
  #jid{luser = LUser, lserver = LServer} = From,
  UserJid = list_to_binary(binary_to_list(LUser) ++ "@" ++ binary_to_list(LServer)),
  GroupName = xml:get_tag_attr_s(<<"nickname">>, SubEl),
  MembersList = string:tokens(binary_to_list(xml:get_tag_cdata(SubEl)), "\n ,\"[]"),
  case MembersList of
    [] ->
      IQ#iq{type = error, sub_el = []};
    _ ->
      F = fun() ->
        ejabberd_odbc:sql_query_t([<<"insert into groupinfo(name,owner) values('">>, GroupName, "','", UserJid, "');"]),
        Result = ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]),
        {selected, _, [{RId}]} = Result,
        QueryList = make_add_query([binary_to_list(UserJid)] ++ MembersList, "", RId),
        lists:foreach(fun(X) ->
          ejabberd_odbc:sql_query_t(X)
        end, QueryList),
        Result
      end,
      T = ejabberd_odbc:sql_transaction(LServer, F),
      case T of
        {atomic, {selected, _, [{ResultId}]}} ->
          MembersJid = get_us_from_jid(MembersList, []),
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

make_add_query([H | T], Result, GroupID) ->
  Query =
    [[<<"insert into groupuser(groupid, jid) "
    "values ('">>, GroupID, "', '", list_to_binary(H), "');"]],
  make_add_query(T, Result ++ Query, GroupID);
make_add_query([], Result, _GroupID) ->
  Result.

get_members(From, _To, #iq{sub_el = SubEl} = IQ) ->
  #jid{luser = _LUser, lserver = LServer} = From,
  GroupId = xml:get_tag_attr_s(<<"groupid">>, SubEl),
  T = ejabberd_odbc:sql_query(
    LServer,
    [<<"select jid from groupuser"
    " where groupid = '">>, GroupId, "';"]),
  case T
  of
    {selected, [<<"jid">>], Rs} when is_list(Rs) ->
      Result = string:join([binary_to_list(X) || {X} <- Rs], "\",\""),
      IQ#iq{type = result, sub_el = [SubEl#xmlel{children =
      [{xmlcdata, list_to_binary("[\"" ++ Result ++ "\"]")}]}]};
    _ ->
      IQ#iq{type = error, sub_el = []}
  end.

get_groups(From, _To, #iq{sub_el = SubEl} = IQ) ->
  #jid{luser = LUser, lserver = LServer} = From,
  UserJid = list_to_binary(binary_to_list(LUser) ++ "@" ++ binary_to_list(LServer)),
  T = ejabberd_odbc:sql_query(
    LServer,
    [<<"select groupinfo.groupid,groupinfo.name from groupinfo,groupuser"
    " where groupuser.jid = '">>, UserJid, "' and groupinfo.groupid = groupuser.groupid;"]),
  case T
  of
    {selected, [<<"groupid">>, <<"name">>], Rs} when is_list(Rs) ->
      IQ#iq{type = result, sub_el = [SubEl#xmlel{children = [grouplist_to_json(Rs, "")]}]};
    _ ->
      IQ#iq{type = error, sub_el = []}
  end.

grouplist_to_json([H | T], Result) ->
  {GroupId, GroupName} = H,
  grouplist_to_json(T, Result ++ "{\"jid\":\"" ++ binary_to_list(GroupId)
    ++ "\",\"nickname\":\"" ++ binary_to_list(GroupName) ++ "\"},");
grouplist_to_json([], Result) ->
  {xmlcdata, list_to_binary("[" ++ string:sub_string(Result, 1, string:len(Result) - 1) ++ "]")}.

push_event_message(GroupID, Nickname, Server, ToList, MemberList) ->
  FromString = "aftgroup_" ++ binary_to_list(GroupID) ++ "@" ++ binary_to_list(Server),
  Lang = {<<"xml:lang">>, <<"en">>},
  From = {<<"from">>, list_to_binary(FromString)},
  Type = {<<"type">>, <<"aft_groupchat">>},
  Push = {<<"push">>, <<"true">>},

  Contents = event_member_json(MemberList, "add"),
  Packet = {xmlel, <<"message">>, [],
    [{xmlcdata, <<"\n     ">>},
      {xmlel, <<"body">>, [{<<"groupid">>, GroupID}, {<<"nickname">>, Nickname}],
        [{xmlcdata, list_to_binary(Contents)}]},
      {xmlcdata, <<"\n">>}]},
  FromJID = jlib:make_jid(list_to_binary("aftgroup_" ++ binary_to_list(GroupID)), Server, <<"">>),

  lists:foreach(fun({U, S}) ->
    ToJID = jlib:make_jid(U, S, <<"">>),
    ToAttr = {<<"to">>, list_to_binary(binary_to_list(U) ++ "@" ++ binary_to_list(S))},
    ejabberd_router:route(FromJID, ToJID,
      Packet#xmlel{attrs = [From, ToAttr, Type, Lang, Push]}) end,
    ToList).

event_member_json(MemberList, Action) ->
  Contents = lists:foldl(fun({U, S}, Acc) ->
    Acc1 = if Acc /= "" ->
      Acc ++ ",";
             true -> Acc
           end,
    Acc1 ++ "{\"jid\":\"" ++ binary_to_list(U) ++ "@" ++ binary_to_list(S) ++ "\", \"action\":\"" ++ Action ++ "\"}"
  end,
    "", MemberList),
  "[" ++ Contents ++ "]".

get_us_from_jid([H | R], Result) ->
  #jid{luser = U, lserver = S} = jlib:binary_to_jid(list_to_binary(H)),
  get_us_from_jid(R, [{U, S} | Result]);
get_us_from_jid([], Result) ->
  lists:reverse(Result).

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
          io:format("undefined query type"),
          IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
      end;
    false -> IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
  end;

process_iq(_, _, IQ) ->
  #iq{sub_el = SubEl} = IQ,
  IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.