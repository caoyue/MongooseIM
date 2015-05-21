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
    case get_fileid(SubEl) of
        emptyid -> IQ#iq{type = error, sub_el = [SubEl]};
        FileUid ->
            case verify(LServer, FileUid) of
                ok ->
                    case get_url(FileUid) of
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
    E = integer_to_binary(generate_expiration()),
    Secret = iolist_to_binary(mod_mms_s3:upload_secret()),
    case get_fileid(SubEl) of
        emptyid ->
            F = generate_fileid(),
            T = generate_token(F, E, Secret),
            case ejabberd_redis:cmd(["SET", <<"upload:", F/binary>>, E]) of
                <<"OK">> ->
                    return_newel(T, F, E, IQ);
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl]}
            end;
        F ->
            case ejabberd_redis:cmd(["GET", <<"upload:", F/binary>>]) of
                undefined ->
                    IQ#iq{type = error, sub_el = [SubEl]};
                _ ->
                    T = generate_token(F, E, Secret),
                    return_newel(T, F, E, IQ)
            end
    end.

get_fileid(SubEl) ->
    case xml:get_subtag(SubEl, <<"file">>) of
        false ->
            emptyid;
        FileEl ->
            case xml:get_tag_cdata(FileEl) of
                <<>> ->
                    emptyid;
                FileUid ->
                    FileUid
            end
    end.

return_newel(T, F, E, #iq{sub_el = SubEl} = IQ) ->
    Token = #xmlel{
        name = <<"token">>,
        children = [{xmlcdata, T}]},
    File = #xmlel{
        name = <<"file">>,
        children = [{xmlcdata, F}]},
    Expiration = #xmlel{
        name = <<"expiration">>,
        children = [{xmlcdata, E}]},
    NEl = SubEl#xmlel{children = [Token, File, Expiration]},
    IQ#iq{type = result, sub_el = [NEl]}.

-spec get_url(binary()) -> binary() | error.
get_url(Uid) ->
    mod_mms_s3:get(Uid, 30 * 60).  % 30 minutes expiration for download url

-spec generate_token(binary(), binary(), binary()) -> string().
generate_token(FileId, Expiration, Secret) ->
    md5(<<FileId/binary, Expiration/binary, Secret/binary>>).

generate_expiration() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S + 30 * 60. % 30 minutes expiration for upload token

generate_fileid() ->
    uuid:generate().

verify(LServer, Uid) ->
    Query = [<<"select uid, filename, owner from mms_file where uid = '">>, ejabberd_odbc:escape(Uid), <<"';">>],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"uid">>, <<"filename">>, <<"owner">>], Rs} when is_list(Rs) ->
            case Rs of
                [] -> not_found;
                _ -> ok
            end;
        Error -> {error, Error}
    end.

-spec md5(string() | binary()) -> string().
md5(Text) ->
    lists:flatten([io_lib:format("~.16b", [N]) || N <- binary_to_list(erlang:md5(Text))]).