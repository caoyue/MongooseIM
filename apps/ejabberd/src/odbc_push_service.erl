-module(odbc_push_service).

%% API
-export([add/2, remove/2]).


-include("jlib.hrl").
-include("mod_push_service.hrl").

add(LServer, TokenRecord) ->
    #push_token{jid = UserJid, token = Token, type = PushType} = TokenRecord,
    Query = [<<"insert into push_service (jid, token, push_type) values ('">>, ejabberd_odbc:escape(UserJid), <<"','">>,
        ejabberd_odbc:escape(Token), <<"',">>, integer_to_list(PushType), <<");">>],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.

remove(LServer, TokenRecord) ->
    #push_token{jid = UserJid, token = Token, type = _PushType} = TokenRecord,
    Query = [<<"delete from push_service where jid = '">>, ejabberd_odbc:escape(UserJid), <<"' and token = '">>,
        ejabberd_odbc:escape(Token), <<"';">>],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Error ->
            {error, Error}
    end.