
-module(odbc_groupchat).

%% API
-export([create_group/3,
         add_members/3,
         get_groupname_by_groupid/2,
         create_and_add/4,
         get_members_by_groupid/2,
         get_groups_by_jid/2]).


create_group(LServer,UserJid,GroupName) ->
    F = fun() ->
                ejabberd_odbc:sql_query_t([<<"insert into groupinfo(name,owner) values('">>, GroupName, "','", UserJid, "');"]),
                Result = ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]),
                ejabberd_odbc:sql_query_t([<<"insert into groupuser(groupid,jid) values(last_insert_id(),'">>, UserJid, "');"]),
                Result
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

add_members(LServer,GroupId,Members) ->
    AddQuery = make_add_query(Members, [], GroupId),
    ejabberd_odbc:sql_transaction(LServer, AddQuery).

get_groupname_by_groupid(LServer,GroupId) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select name from groupinfo where groupid = '", GroupId, "';"]).

create_and_add(LServer,GroupName,MembersList,UserJid) ->
    F = fun() ->
                ejabberd_odbc:sql_query_t([<<"insert into groupinfo(name,owner) values('">>,
                                           GroupName, "','", UserJid, "');"]),
                Result = ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]),
                {selected, _, [{RId}]} = Result,
                QueryList = make_add_query([binary_to_list(UserJid)] ++ MembersList, "", RId),
                lists:foreach(fun(X) ->
                                      ejabberd_odbc:sql_query_t(X)
                              end, QueryList),
                Result
        end,
    ejabberd_odbc:sql_transaction(LServer, F).

make_add_query([H | T], Result, GroupID) ->
    Query =
        [<<"insert into groupuser(groupid, jid) "
           "values ('">>, GroupID, "', '", list_to_binary(H), "');"],
    make_add_query(T, [Query | Result], GroupID);
make_add_query([], Result, _GroupID) ->
    Result.

get_members_by_groupid(LServer,GroupId) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select jid from groupuser"
         " where groupid = '">>, GroupId, "';"]).

get_groups_by_jid(LServer,UserJid) ->
    ejabberd_odbc:sql_query(
      LServer,
      [<<"select groupinfo.groupid,groupinfo.name from groupinfo,groupuser"
         " where groupuser.jid = '">>, UserJid, "' and groupinfo.groupid = groupuser.groupid;"]).