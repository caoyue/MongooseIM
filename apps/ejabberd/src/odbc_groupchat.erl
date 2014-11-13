-module(odbc_groupchat).

%% API
-export([create_group/3,
         add_members/3,
         get_groupinfo_by_groupid/2,
         create_and_add/4,
         get_members_by_groupid/2,
         get_groups_by_jid/2,
         dismiss_group/3,
         is_user_exists/2,
         is_user_in_group/3,
         is_user_own_group/3,
         set_groupname/3,
         set_nickname_in_group/4,
         remove_members/3]).

-include("jlib.hrl").

create_group(LServer, UserJid, GroupName) ->
    F = fun() ->
                ejabberd_odbc:sql_query_t([<<"insert into groupinfo(name,owner) values('">>,
                                           ejabberd_odbc:escape(GroupName), "','", ejabberd_odbc:escape(UserJid), "');"]),
                {selected, _, [{GroupId}]} = ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]),
                T = make_add_query([UserJid], [], GroupId),
                ejabberd_odbc:sql_query_t(T),
                GroupId
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, GroupId} ->
            {ok, GroupId};
        Error ->
            {error, Error}
    end.

add_members(LServer, GroupId, MembersList) ->
    AddQuery = make_add_query(MembersList, [], GroupId),
    MembersString = join_memberslist(MembersList),
    SelectQuery = [<<"select jid,nickname from groupuser where groupid ='">>, ejabberd_odbc:escape(GroupId),
                   <<"' and jid in ('">>, MembersString, <<"');">>],
    F = fun() ->
                lists:foreach(fun(X) ->
                                      ejabberd_odbc:sql_query_t(X)
                              end, AddQuery),
                ejabberd_odbc:sql_query_t(SelectQuery)
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, [<<"jid">>, <<"nickname">>], Rs}} ->
            {ok, Rs};
        Error ->
            {error, Error}
    end.

get_groupinfo_by_groupid(LServer, GroupId) ->
    case ejabberd_odbc:sql_query(
           LServer,
           ["select name,owner from groupinfo where groupid = '", ejabberd_odbc:escape(GroupId), "';"]) of
        {selected, [<<"name">>, <<"owner">>], [{GroupName,GroupOwner}]} ->
            {ok, GroupId, GroupName, GroupOwner};
        Error ->
            {error, Error}
    end.

create_and_add(LServer, GroupName, MembersList, UserJid) ->
    F = fun() ->
                ejabberd_odbc:sql_query_t([<<"insert into groupinfo(name,owner) values('">>,
                                           ejabberd_odbc:escape(GroupName), "','", ejabberd_odbc:escape(UserJid), "');"]),
                Result = ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]),
                {selected, _, [{RId}]} = Result,
                QueryList = make_add_query([UserJid | MembersList], [], RId),
                lists:foreach(fun(X) ->
                                      ejabberd_odbc:sql_query_t(X)
                              end, QueryList),
                ejabberd_odbc:sql_query_t([<<"select groupid,jid,nickname from groupuser where groupid ='">>,
                                           ejabberd_odbc:escape(RId), <<"' and jid <> '">>,
                                           ejabberd_odbc:escape(UserJid), <<"';">>])
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, [<<"groupid">>, <<"jid">>, <<"nickname">>], Rs}} ->
            {ok, Rs};
        Error ->
            {error, Error}
    end.

make_add_query([H | T], Result, GroupID) ->
    #jid{luser = LUser} = jlib:binary_to_jid(H),
    Query =
        [<<"insert into groupuser(groupid, jid, nickname) select '">>
        , ejabberd_odbc:escape(GroupID), "', '", ejabberd_odbc:escape(H),
         <<"', nickname from vcard_search where username ='">>, ejabberd_odbc:escape(LUser), <<"';">>],
    make_add_query(T, [Query | Result], GroupID);
make_add_query([], Result, _GroupID) ->
    Result.

get_members_by_groupid(LServer, GroupId) ->
    case ejabberd_odbc:sql_query(
           LServer,
           [<<"select jid,nickname from groupuser"
              " where groupid = '">>, ejabberd_odbc:escape(GroupId), "';"]) of
        {selected, [<<"jid">>, <<"nickname">>], Rs} ->
            {ok, Rs};
        Error ->
            {error, Error}
    end.

get_groups_by_jid(LServer, UserJid) ->
    case ejabberd_odbc:sql_query(
           LServer,
           [<<"select groupinfo.groupid,groupinfo.name,groupinfo.owner from groupinfo,groupuser"
              " where groupuser.jid = '">>, ejabberd_odbc:escape(UserJid), "' and groupinfo.groupid = groupuser.groupid;"]) of
        {selected, [<<"groupid">>, <<"name">>, <<"owner">>], Rs} ->
            {ok, Rs};
        Error ->
            {error, Error}
    end.

is_user_exists(LServer, UserName) ->
    T = ejabberd_odbc:sql_query(LServer, [<<"select count(username) from users where username ='">>,
                                          ejabberd_odbc:escape(UserName), <<"';">>]),
    case T of
        {selected, [<<"count(username)">>], Count} ->
            case Count of
                [{<<"0">>}] ->
                    false;
                _ ->
                    true
            end;
        Error ->
            {error, Error}
    end.

is_user_in_group(LServer, UserJid, GroupId) ->
    T = ejabberd_odbc:sql_query(LServer, [<<"select count(id) from groupuser where groupid ='">>,
                                          ejabberd_odbc:escape(GroupId), <<"' and jid = '">>,
                                          ejabberd_odbc:escape(UserJid), <<"';">>]),
    case T of
        {selected, [<<"count(id)">>], Count} ->
            case Count of
                [{<<"0">>}] ->
                    false;
                _ ->
                    true
            end;
        Error ->
            {error, Error}
    end.

is_user_own_group(LServer, UserJid, GroupId) ->
    T = ejabberd_odbc:sql_query(LServer, [<<"select count(groupid) from groupinfo where groupid ='">>,
                                          ejabberd_odbc:escape(GroupId), <<"' and owner = '">>,
                                          ejabberd_odbc:escape(UserJid), <<"';">>]),
    case T of
        {selected, [<<"count(groupid)">>], Count} ->
            case Count of
                [{<<"0">>}] ->
                    false;
                _ ->
                    true
            end;
        Error ->
            {error, Error}
    end.

set_groupname(LServer, GroupId, GroupName) ->
    T = ejabberd_odbc:sql_query(LServer, [<<"update groupinfo set name ='">>,
                                          ejabberd_odbc:escape(GroupName), <<"' where groupid = '">>,
                                          ejabberd_odbc:escape(GroupId), <<"';">>]),
    case T of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.

dismiss_group(LServer, GroupId, MembersInfoList) ->
    MembersString = join_memberslist([Jid || {Jid, _} <- MembersInfoList]),
    Query = [[<<"delete from groupinfo where groupid = '">>, ejabberd_odbc:escape(GroupId), <<"';">>],
             [<<"delete from groupuser where groupid ='">>, ejabberd_odbc:escape(GroupId),
              <<"' and jid in ('">>, MembersString, <<"');">>]
            ],
    T = ejabberd_odbc:sql_transaction(LServer, Query),
    case T of
        {atomic, _} ->
            ok;
        Error ->
            {error, Error}
    end.

remove_members(LServer, GroupId, MembersList) ->
    MembersString = join_memberslist(MembersList),
    F = fun() ->
                Result = ejabberd_odbc:sql_query_t([<<"select jid,nickname from groupuser where groupid ='">>,
                                                    ejabberd_odbc:escape(GroupId), <<"' and jid in ('">>, MembersString, <<"');">>]),
                ejabberd_odbc:sql_query_t([<<"delete from groupuser where groupid ='">>, ejabberd_odbc:escape(GroupId),
                                           <<"' and jid in ('">>, MembersString, <<"');">>]),
                Result
        end,
    T = ejabberd_odbc:sql_transaction(LServer, F),
    case T of
        {atomic, {selected, [<<"jid">>, <<"nickname">>], Rs}} ->
            {ok, Rs};
        Error ->
            {error, Error}
    end.

set_nickname_in_group(LServer, GroupId, UserId, NickName) ->
    T = ejabberd_odbc:sql_query(LServer, [<<"update groupuser set nickname ='">>,
                                          ejabberd_odbc:escape(NickName), <<"' where groupid = '">>,
                                          ejabberd_odbc:escape(GroupId), <<"' and jid ='">>,
                                          ejabberd_odbc:escape(UserId), <<"';">>]),
    case T of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.

join_memberslist(MembersList) ->
    binary_join(MembersList, <<"','">>).


-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun(A, B) ->
                        if
                            bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
                            true -> A
                        end
                end, <<>>, List).