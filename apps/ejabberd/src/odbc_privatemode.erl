%%%===================================================================
%%% @doc Private mode odbc layer
%%%===================================================================
-module(odbc_privatemode).

%% API
-export([set_contact/4, set_group/4]).


%%%===================================================================
%%% API
%%%===================================================================
-spec set_contact(ejabberd:lserver(), binary(), binary(), binary()) -> ok|{error, any()}.
set_contact(LServer, UserName, ContactJid, Private) ->
    Query = [<<"update rosterusers set private ='">>, ejabberd_odbc:escape(Private),
        <<"' where jid='">>, ejabberd_odbc:escape(ContactJid),
        <<"' and username='">>, ejabberd_odbc:escape(UserName), <<"';">>],
    Result = ejabberd_odbc:sql_query(LServer, Query),
    case Result of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.

-spec set_group(ejabberd:lserver(), binary(), binary(), binary()) -> ok|{error, any()}.
set_group(LServer, UserJid, GroupId, Private) ->
    Query = [<<"update groupuser set private ='">>, ejabberd_odbc:escape(Private),
        <<"' where jid='">>, ejabberd_odbc:escape(UserJid),
        <<"' and groupid='">>, ejabberd_odbc:escape(GroupId), <<"';">>],
    Result = ejabberd_odbc:sql_query(LServer, Query),
    case Result of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.