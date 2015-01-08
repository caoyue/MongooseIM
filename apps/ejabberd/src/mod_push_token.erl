-module(mod_push_token).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% IQ handlers
-export([process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_push_service.hrl").

-define(NS_PUSH_SERVICE, <<"aft:push_service">>).

%%%===================================================================
%%% gen_mod callbacks
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
        ?NS_PUSH_SERVICE, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PUSH_SERVICE).


%%%===================================================================
%%% IQ handlers
%%%===================================================================

process_iq(From, To, #iq{xmlns = ?NS_PUSH_SERVICE, type = _Type, sub_el = SubEl} = IQ) ->
    case is_query(SubEl) of
        true ->
            case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
                <<"add">> ->
                    add(From, To, IQ);
                <<"remove">> ->
                    remove(From, To, IQ);
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        false ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;
process_iq(_, _, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @doc add a record for push notification
add(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    TokenRecord = get_token(SubEl, UserJid),
    case odbc_push_service:add(LServer, TokenRecord) of
        ok ->
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

%% @doc remove a record of user push notification config
remove(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    TokenRecord = get_token(SubEl, UserJid),
    case odbc_push_service:remove(LServer, TokenRecord#push_token.token) of
        ok ->
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

get_token(SubEl, UserJid) ->
    Tag = xml:get_subtag(SubEl, <<"token">>),
    case Tag of
        false ->
            not_exists;
        _ ->
            #push_token{jid = UserJid, token = xml:get_tag_cdata(Tag),
                type = binary_to_integer(xml:get_tag_attr_s(<<"type">>, Tag))}
    end.

is_query(Packet) ->
    case Packet of
        #xmlel{name = <<"query">>} -> true;
        _ -> false
    end.