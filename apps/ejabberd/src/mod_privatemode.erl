-module(mod_privatemode).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_PRIVATEMODE, <<"aft:privatemode">>).

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_PRIVATEMODE, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PRIVATEMODE).

%% @doc set contact
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#private-mode
set_contact(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    ContactJid = xml:get_tag_attr_s(<<"subject">>, SubEl),
    Private = xml:get_tag_attr_s(<<"private">>, SubEl),
    case odbc_privatemode:set_contact(LServer, LUser, ContactJid, Private) of
        ok ->
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

%% @doc set group
%% https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#private-mode
set_group(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = From,
    GroupId = xml:get_tag_attr_s(<<"subject">>, SubEl),
    UserJid = jlib:jid_to_binary({LUser, LServer, <<>>}),
    Private = xml:get_tag_attr_s(<<"private">>, SubEl),
    case odbc_privatemode:set_group(LServer, UserJid, GroupId, Private) of
        ok ->
            IQ#iq{type = result, sub_el = [SubEl]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

is_query(Packet) ->
    case Packet of
        #xmlel{name = <<"query">>} -> true;
        _ -> false
    end.

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