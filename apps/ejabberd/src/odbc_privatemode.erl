-module(odbc_privatemode).

%% API
-export([set_contact/4, set_group/4]).


set_contact(LServer, UserName, ContactJid, Private) ->
    Query = [<<"update rosterusers set private ='">>, ejabberd_odbc:escape(Private),
             <<"' where jid='">>, ejabberd_odbc:escape(ContactJid),
             <<"' and username='">>, ejabberd_odbc:escape(UserName), <<"';">>],
    T = ejabberd_odbc:sql_query(LServer, Query),
    case T of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.

set_group(LServer, UserJid, GroupId, Private) ->
    Query = [<<"update groupuser set private ='">>, ejabberd_odbc:escape(Private),
             <<"' where jid='">>, ejabberd_odbc:escape(UserJid),
             <<"' and groupid='">>, ejabberd_odbc:escape(GroupId), <<"';">>],
    T = ejabberd_odbc:sql_query(LServer, Query),
    case T of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.