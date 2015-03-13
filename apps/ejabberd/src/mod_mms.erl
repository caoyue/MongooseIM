-module(mod_mms).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_MMS, <<"aft:mms">>).

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
        ?NS_MMS, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MMS).


%% -----------------------------------------------
%% IQ Handler
%% -----------------------------------------------

process_iq(From, To, #iq{xmlns = ?NS_MMS, type = _Type, sub_el = SubEl} = IQ) ->
    case is_query(SubEl) of
        true ->
            case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
                <<"download">> ->
                    download(From, To, IQ);
                <<"upload">> ->
                    upload(From, To, IQ);
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        false ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;

process_iq(_, _, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.

is_query(Packet) ->
    case Packet of
        #xmlel{name = <<"query">>} -> true;
        _ -> false
    end.


%% -----------------------------------------------
%% Internal functions
%% -----------------------------------------------

download(#jid{lserver = LServer} = _From, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml:get_tag_cdata(SubEl) of
        <<>> ->
            IQ#iq{type = error, sub_el = [SubEl]};
        Uid ->
            case verify(LServer, Uid) of
                ok ->
                    case get_url(Uid) of
                        error ->
                            IQ#iq{type = error, sub_el = [SubEl]};
                        Url ->
                            IQ#iq{type = result, sub_el = [
                                SubEl#xmlel{children = [{xmlcdata, Url}]}]}
                    end;
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl]}
            end
    end.


upload(_From, _To, #iq{sub_el = SubEl} = IQ) ->
    Token = generate_token(),
    case ejabberd_redis:cmd(["SET", <<"upload_token:", Token/binary>>, Token]) of
        <<"OK">> ->
            IQ#iq{type = result, sub_el = [SubEl#xmlel{children = [{xmlcdata, Token}]}]};
        _ ->
            IQ#iq{type = error, sub_el = [SubEl]}
    end.

-spec get_url(binary()) -> binary() | error.
get_url(Uid) ->
    mod_mms_s3:get(Uid, 30 * 60).

generate_token() ->
    uuid:generate().

verify(LServer, Uid) ->
    Query = [<<"select uid, filename, owner from mms_file where uid = '">>, ejabberd_odbc:escape(Uid), <<"';">>],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"uid">>, <<"filename">>, <<"owner">>], Rs} when is_list(Rs) ->
            case Rs of
                [] -> {error, not_found};
                _ -> ok
            end;
        Error -> {error, Error}
    end.