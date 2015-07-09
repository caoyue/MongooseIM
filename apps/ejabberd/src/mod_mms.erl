-module(mod_mms).

-behaviour(gen_mod).

-export([start/2, stop/1, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_mms.hrl").

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
    case SubEl of
        #xmlel{name = <<"query">>} ->
            case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
                <<"download">> ->
                    download(From, To, IQ);
                <<"upload">> ->
                    upload(From, To, IQ);
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
            end;
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;
process_iq(_, _, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.


%% -----------------------------------------------
%% Internal functions
%% -----------------------------------------------

download(#jid{lserver = LServer} = _From, _To, #iq{sub_el = SubEl} = IQ) ->
    case get_fileid(SubEl) of
        undefined -> make_error_reply(IQ, <<"19001">>);
        FileUid ->
            case verify(LServer, FileUid) of
                ok ->
                    case get_url(LServer, FileUid) of
                        error ->
                            make_error_reply(IQ, <<"19001">>);
                        Url ->
                            IQ#iq{type = result, sub_el = [
                                SubEl#xmlel{children = [{xmlcdata, Url}]}]}
                    end;
                _ ->
                    make_error_reply(IQ, <<"19001">>)
            end
    end.

upload(#jid{lserver = LServer, luser = LUser} = _From, _To, #iq{sub_el = SubEl} = IQ) ->
    E = integer_to_binary(generate_expiration()),
    S = list_to_binary(?MMS_SECRET),
    Jid = <<LUser/binary, $@, LServer/binary>>,
    Private = case xml:get_tag_attr_s(<<"private">>, SubEl) of
                  ?PUBLIC -> ?PUBLIC;
                  _ -> ?PRIVATE
              end,
    case get_fileid(SubEl) of
        undefined ->
            F = generate_fileid(),
            T = generate_token(Jid, F, E, S, Private),
            Time = integer_to_binary(stamp_now()),
            case ejabberd_redis:cmd(["SET", <<"upload:", F/binary>>, <<Private/binary, $:, Time/binary>>]) of
                <<"OK">> ->
                    make_reply(T, F, E, IQ);
                _ ->
                    make_error_reply(IQ, <<"19001">>)
            end;
        F ->
            case ejabberd_redis:cmd(["GET", <<"upload:", F/binary>>]) of
                {ok, <<Private:1/binary, $:, _/binary>>} ->
                    T = generate_token(Jid, F, E, S, Private),
                    make_reply(T, F, E, IQ);
                _ ->
                    make_error_reply(IQ, <<"19001">>)
            end
    end.


%% ===============================
%% helper
%% ===============================

-spec make_error_reply(#iq{}, binary()) -> #iq{}.
make_error_reply(#iq{sub_el = SubEl} = IQ, Code) ->
    Error = #xmlel{name = <<"error">>, attrs = [{<<"code">>, Code}]},
    IQ#iq{type = error, sub_el = [SubEl, Error]}.

make_reply(T, F, E, #iq{sub_el = SubEl} = IQ) ->
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

get_fileid(SubEl) ->
    case xml:get_subtag(SubEl, <<"file">>) of
        false ->
            undefined;
        FileEl ->
            case xml:get_tag_cdata(FileEl) of
                <<>> ->
                    undefined;
                FileUid ->
                    FileUid
            end
    end.

-spec get_url(binary(), binary()) -> binary() | error.
get_url(LServer, Uid) ->
    case get_file(LServer, Uid) of
        {error, _} ->
            error;
        File ->
            mod_mms_s3:get(File, 30 * 60)
    end.

-spec generate_token(binary(), binary(), binary(), binary(), binary()) -> string().
generate_token(Jid, FileId, Expiration, Secret, Private) ->
    md5(<<Jid/binary, FileId/binary, Expiration/binary, Secret/binary, Private/binary>>).

-spec generate_expiration() -> integer().
generate_expiration() ->
    stamp_now() + 30 * 60. % 30 minutes expiration for upload token

stamp_now() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

-spec generate_fileid() -> binary().
generate_fileid() ->
    uuid:generate().

-spec md5(string() | binary()) -> string().
md5(Text) ->
    lists:flatten([io_lib:format("~.16b", [N]) || N <- binary_to_list(erlang:md5(Text))]).

%% ===========================
%% odbc
%% ===========================

-spec get_file(binary(), binary()) -> #mms_file{} | {error, _}.
get_file(LServer, Uid) ->
    Query = [<<"select filename,owner,private,UNIX_timestamp(created_at) from mms_file where uid= '">>, Uid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, _, []} ->
            {error, not_exists};
        {selected, _, [{FileName, Owner, Private, CreatedAt}]} ->
            #mms_file{uid = Uid, filename = FileName, owner = Owner,
                private = case Private of
                              <<0>> -> ?PUBLIC;
                              _ -> ?PRIVATE
                          end, created_at = CreatedAt};
        Reason ->
            {error, Reason}
    end.

-spec verify(binary(), binary()) -> ok | {error, _}.
verify(LServer, Uid) ->
    Query = [<<"select count(uid) from mms_file where uid = '">>, ejabberd_odbc:escape(Uid), <<"';">>],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, _, [{<<"0">>}]} ->
            {error, not_found};
        {selected, _, [{<<"1">>}]} ->
            ok;
        Error ->
            {error, Error}
    end.