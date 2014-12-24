%%%===================================================================
%%% @doc Private mode odbc layer
%%%===================================================================
-module(odbc_privatemode).

%% API
-export([set_contact/4, set_group/4, set_password/3, get_password/2]).


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

set_password(LServer, UserJid, CurrentPassword) ->
    case get_password(LServer, UserJid) of
        {ok, []} ->
            Query = [<<"insert into privatemode(jid,password) values('">>, ejabberd_odbc:escape(UserJid), <<"','">>,
                     ejabberd_odbc:escape(CurrentPassword), <<"');">>],
            case ejabberd_odbc:sql_query(LServer, Query) of
                {updated, 1} ->
                    ok;
                Error ->
                    {error, Error}
            end;
        {ok, [_]} ->
            Query = [<<"update privatemode set password ='">>, ejabberd_odbc:escape(CurrentPassword),
                     <<"' where jid='">>, ejabberd_odbc:escape(UserJid), <<"';">>],
            case ejabberd_odbc:sql_query(LServer, Query) of
                {updated, 1} ->
                    ok;
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

get_password(LServer, UserJid) ->
    Query = [<<"select password from privatemode where jid ='">>, ejabberd_odbc:escape(UserJid), <<"';">>],
    Result = ejabberd_odbc:sql_query(LServer, Query),
    case Result of
        {selected, [<<"password">>], Rs} ->
            {ok, Rs};
        Error ->
            {error, Error}
    end.