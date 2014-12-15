%%%===================================================================
%%% @doc Private mode: to mark a contact or group as private
%%%     The client should protect a user's conversation with a private
%%%     contact or group.
%%%     https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#private-mode
%%%===================================================================
-module(mod_privatemode).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% IQ handlers
-export([process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_PRIVATEMODE, <<"aft:privatemode">>).


%%%===================================================================
%%% gen_mod callbacks
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
        ?NS_PRIVATEMODE, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATEMODE).


%%%===================================================================
%%% IQ handlers
%%%===================================================================

process_iq(From, To, #iq{xmlns = ?NS_PRIVATEMODE, type = _Type, sub_el = SubEl} = IQ) ->
    case is_query(SubEl) of
        true ->
            case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
                <<"contact">> ->
                    set_contact(From, To, IQ);
                <<"group">> ->
                    set_group(From, To, IQ);
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

%% @doc to mark a contact as private or not private
set_contact(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    ContactJid = xml:get_tag_attr_s(<<"subject">>, SubEl),
    Private = xml:get_tag_attr_s(<<"private">>, SubEl),
    case odbc_privatemode:set_contact(LServer, LUser, ContactJid, Private) of
        ok ->
            multiple_resources_broadcast(LUser, LServer, ContactJid, Private, <<"contact">>),
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

%% @doc to mark a group as private or not private
set_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"subject">>, SubEl),
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    Private = xml:get_tag_attr_s(<<"private">>, SubEl),
    case odbc_privatemode:set_group(LServer, UserJid, GroupId, Private) of
        ok ->
            multiple_resources_broadcast(LUser, LServer, GroupId, Private, <<"group">>),
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

multiple_resources_broadcast(LUser, LServer, ContactJid, Private, Mode) ->
    FromJid = jlib:make_jid(<<>>, <<>>, <<>>),
    ToJid = jlib:make_jid(LUser, LServer, <<>>),
    LangAttr = {<<"xml:lang">>, <<"en">>},
    ToAttr = {<<"to">>, jlib:jid_to_binary(ToJid)},
    TypeAttr = {<<"type">>, <<"chat">>},
    Packet = {xmlel, <<"message">>, [],
        [{xmlel, <<"push">>, [{<<"xmlns">>, ?NS_PRIVATEMODE}, {<<"type">>, <<"privatemode">>},
            {<<"mode">>, Mode}, {<<"subject">>, ContactJid}, {<<"private">>, Private}],
            [{xmlcdata, <<>>}]}
        ]},
    ejabberd_router:route(FromJid, ToJid,
        Packet#xmlel{attrs = [ToAttr, TypeAttr, LangAttr]}).

is_query(Packet) ->
    case Packet of
        #xmlel{name = <<"query">>} -> true;
        _ -> false
    end.

