%%%===================================================================
%%% @doc An extension to XEP-0077 (http://xmpp.org/extensions/xep-0077.html)
%%%      Allow users to register account with telephone number or email.
%%%      It's based on mod_register.erl with these changes:
%%%         1. remove ip checking
%%%         2. remove acl checking
%%%         3. don't support get IQ
%%%         4. require activation of new account (by SMS or email)
%%%===================================================================

-module(mod_register_aft).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         clean_opts/1]).

%% hooks
-export([stream_feature_register/2,
         unauthenticated_iq_register/4]).

%% iq handler
-export([process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").


%%%===================================================================
%%% gen_mod callbacks
%%%===================================================================

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_AFT_REGISTER,
                                  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_AFT_REGISTER,
                                  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, Host,
                       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
                       ?MODULE, unauthenticated_iq_register, 50),
    mnesia:create_table(mod_register_aft_ip,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_aft_ip, node(), ram_copies),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_stream_features, Host,
                          ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
                          ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_AFT_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_AFT_REGISTER).

clean_opts(Opts) ->
    lists:map(fun clean_opt/1, Opts).

clean_opt({registration_watchers, Watchers}) ->
    CleanWatchers = lists:map(fun ejabberd_binary:string_to_binary/1, Watchers),
    {registration_watchers, CleanWatchers};
clean_opt(Item) ->
    Item.

%%%===================================================================
%%% hooks
%%%===================================================================

stream_feature_register(Acc, _Host) ->
    [#xmlel{name = <<"aft-register">>,
            attrs = [{<<"xmlns">>, ?NS_AFT_FEATURE_IQREGISTER}]} | Acc].

unauthenticated_iq_register(_Acc,
                            Server, #iq{xmlns = ?NS_AFT_REGISTER} = IQ, IP) ->
    Address = case IP of
                  {A, _Port} -> A;
                  _ -> undefined
              end,
    ResIQ = process_unauthenticated_iq(Server, IQ, Address),
    Res1 = jlib:replace_from_to(jlib:make_jid(<<>>, Server, <<>>),
                                jlib:make_jid(<<>>, <<>>, <<>>),
                                jlib:iq_to_xml(ResIQ)),
    jlib:remove_attr(<<"to">>, Res1);
unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

%%%===================================================================
%%% iq handler
%%%===================================================================

process_iq(#jid{user = User, lserver = Server, lresource = Resource} = _From,
           #jid{lserver = Server} = _To,
           #iq{type = set, lang = Lang1, sub_el = SubEl} = IQ) ->
    Lang = binary_to_list(Lang1),
    RemoveTag = xml:get_subtag(SubEl, <<"remove">>),
    OldPasswordTag = xml:get_subtag(SubEl, <<"old_password">>),
    PasswordTag = xml:get_subtag(SubEl, <<"password">>),
    if
        (RemoveTag /= false) and (OldPasswordTag =:= false) and (PasswordTag =:= false) ->
            ResIQ = IQ#iq{type = result, sub_el = [SubEl]},
            ejabberd_router:route(
                jlib:make_jid(User, Server, Resource),
                jlib:make_jid(User, Server, Resource),
                jlib:iq_to_xml(ResIQ)),
            ejabberd_auth:remove_user(User, Server),
            ignore;
        (RemoveTag =:= false) and (OldPasswordTag /= false) and (PasswordTag /= false) ->
            try_set_password(User, Server, xml:get_tag_cdata(OldPasswordTag),
                             xml:get_tag_cdata(PasswordTag), IQ, SubEl, Lang);
        true ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;
process_iq(_From, _To, #iq{sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_unauthenticated_iq(Server,
                           #iq{type = set, lang = Lang1, sub_el = SubEl} = IQ,
                           IpAddress) ->
    Lang = binary_to_list(Lang1),
    case check_timeout(IpAddress) of
        true ->
            PhoneTag = xml:get_subtag(SubEl, <<"phone">>),
            EmailTag = xml:get_subtag(SubEl, <<"email">>),
            PasswordTag = xml:get_subtag(SubEl, <<"password">>),
            NickTag = xml:get_subtag(SubEl, <<"nick">>),
            if ((PhoneTag =:= false) and (EmailTag =:= false)) or (PasswordTag =:= false) or (NickTag =:= false) ->
                    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
               true ->
                    case try_register(get_tag_cdata(PhoneTag),
                                      get_tag_cdata(EmailTag),
                                      get_tag_cdata(PasswordTag),
                                      get_tag_cdata(NickTag),
                                      Server, Lang, IpAddress) of
                        ok ->
                            IQ#iq{type = result,
                                  sub_el = [SubEl]};
                        {error, Error} ->
                            IQ#iq{type = error,
                                  sub_el = [SubEl, Error]}
                    end
            end;
        false ->
            ErrText = "Users are not allowed to register "
                "accounts so quickly",
            {error, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)}
    end.

try_register(Phone, Email, Password, Nick, Server, Lang, IpAddress) ->
    case ejabberd_auth:check_phone_and_email(Phone, Email, Server) of
        {error, _Reason} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR};
        {info, _} ->
            {error, ?ERR_CONFLICT};
        true ->
            case is_strong_password(Server, Password) of
                true ->
                    GUID = generate_guid(),
                    JID = jlib:make_jid(GUID, Server, <<>>),
                    case ejabberd_auth:try_register_with_phone_or_email(GUID, Server, Password, Phone, Email, Nick) of
                        {atomic, ok} ->
                            send_register_validation(GUID, Phone, Email, Server),
                            send_welcome_message(JID),
                            send_registration_notifications(JID, IpAddress),
                            ok;
                        Error ->
                            remove_timeout(IpAddress),
                            case Error of
                                {atomic, exists} ->
                                    {error, ?ERR_CONFLICT};
                                {error, invalid_jid} ->
                                    {error, ?ERR_JID_MALFORMED};
                                {error, not_allowed} ->
                                    {error, ?ERR_NOT_ALLOWED};
                                {error, _Reason} ->
                                    {error, ?ERR_INTERNAL_SERVER_ERROR}
                            end
                    end;
                false ->
                    ErrText = "The password is too weak",
                    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
            end
    end.

get_tag_cdata(false) ->
    <<>>;
get_tag_cdata(Tag) ->
    xml:get_tag_cdata(Tag).

%% @doc Try to change password and return IQ response
try_set_password(User, Server, OldPassword, Password, IQ, SubEl, Lang) ->
    case ejabberd_auth:check_password(User, Server, OldPassword) of
        true ->
            case is_strong_password(Server, Password) of
                true ->
                    case ejabberd_auth:set_password(User, Server, Password) of
                        ok ->
                            IQ#iq{type = result, sub_el = [SubEl]};
                        {error, empty_password} ->
                            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]};
                        {error, not_allowed} ->
                            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
                        {error, invalid_jid} ->
                            IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
                        _ ->
                            IQ#iq{type = error, sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
                    end;
                false ->
                    ErrText = "The password is too weak",
                    IQ#iq{type = error,
                          sub_el = [SubEl, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)]}
            end;
        _ ->
            ErrText = "The old password is wrong",
            IQ#iq{type = error,
                  sub_el = [SubEl, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)]}
    end.

%% generate UUID. _begin.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

to_string(UUID) ->
    io_lib:format("~8.16.0b~4.16.0b~4.16.0b~2.16.0b~2.16.0b~12.16.0b", get_parts(UUID)).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>.

generate_guid() ->
    list_to_binary(to_string(
                     v4(crypto:rand_uniform(1, round(math:pow(2, 48))) - 1,
                        crypto:rand_uniform(1, round(math:pow(2, 12))) - 1,
                        crypto:rand_uniform(1, round(math:pow(2, 32))) - 1,
                        crypto:rand_uniform(1, round(math:pow(2, 30))) - 1))).
%% _end.

%% TOFIX: send sms
send_register_validation(_GUID, Phone, _Email, _Server) when Phone /= <<>> ->
    sms;
send_register_validation(GUID, _Phone, Email, Server) when Email /= <<>> ->
    LServer = jlib:nameprep(Server),
    BareJID = <<GUID/binary, $@, LServer/binary>>,
    %% TOFIX: how to deploy the web server and the XMPP server?
    Href = <<"http://", LServer/binary, ":5280/verify?token=", BareJID/binary>>,
    kissnapp_email:send_register_validate("Kissnapp Register Validation", binary_to_list(Email), binary_to_list(Href)).


send_welcome_message(JID) ->
    Host = JID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
        {"", ""} ->
            ok;
        {Subj, Body} ->
            ejabberd_router:route(
              jlib:make_jid(<<>>, Host, <<>>),
              JID,
              #xmlel{name = <<"message">>, attrs = [{<<"type">>, <<"normal">>}],
                     children = [#xmlel{name = <<"subject">>,
                                        children = [#xmlcdata{content = Subj}]},
                                 #xmlel{name = <<"body">>,
                                        children = [#xmlcdata{content = Body}]}]});
        _ ->
            ok
    end.

send_registration_notifications(UJID, Source) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body = lists:flatten(
                     io_lib:format(
                       "[~s] The account ~s was registered from IP address ~s "
                       "on node ~w using ~p.",
                       [get_time_string(), jlib:jid_to_binary(UJID),
                        ip_to_string(Source), node(), ?MODULE])),
            lists:foreach(
              fun(S) ->
                      case jlib:binary_to_jid(S) of
                          error -> ok;
                          JID ->
                              ejabberd_router:route(
                                jlib:make_jid(<<>>, Host, <<>>),
                                JID,
                                #xmlel{name = <<"message">>,
                                       attrs = [{<<"type">>, <<"chat">>}],
                                       children = [#xmlel{name = <<"body">>,
                                                          children = [#xmlcdata{content = Body}]}]})
                      end
              end, JIDs);
        _ ->
            ok
    end.

check_timeout(undefined) ->
    true;
check_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
                  undefined -> 600;
                  TO -> TO
              end,
    if
        is_integer(Timeout) ->
            {MSec, Sec, _USec} = now(),
            Priority = -(MSec * 1000000 + Sec),
            CleanPriority = Priority + Timeout,
            F = fun() ->
                        Treap = case mnesia:read(mod_register_aft_ip, treap,
                                                 write) of
                                    [] ->
                                        treap:empty();
                                    [{mod_register_aft_ip, treap, T}] -> T
                                end,
                        Treap1 = clean_treap(Treap, CleanPriority),
                        case treap:lookup(Source, Treap1) of
                            error ->
                                Treap2 = treap:insert(Source, Priority, [],
                                                      Treap1),
                                mnesia:write({mod_register_aft_ip, treap, Treap2}),
                                true;
                            {ok, _, _} ->
                                mnesia:write({mod_register_aft_ip, treap, Treap1}),
                                false
                        end
                end,

            case mnesia:transaction(F) of
                {atomic, Res} ->
                    Res;
                {aborted, Reason} ->
                    ?ERROR_MSG("mod_register_aft: timeout check error: ~p~n",
                               [Reason]),
                    true
            end;
        true ->
            true
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
        true ->
            Treap;
        false ->
            {_Key, Priority, _Value} = treap:get_root(Treap),
            if
                Priority > CleanPriority ->
                    clean_treap(treap:delete_root(Treap), CleanPriority);
                true ->
                    Treap
            end
    end.

remove_timeout(undefined) ->
    true;
remove_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
                  undefined -> 600;
                  TO -> TO
              end,
    if
        is_integer(Timeout) ->
            F = fun() ->
                        Treap = case mnesia:read(mod_register_aft_ip, treap,
                                                 write) of
                                    [] ->
                                        treap:empty();
                                    [{mod_register_aft_ip, treap, T}] -> T
                                end,
                        Treap1 = treap:delete(Source, Treap),
                        mnesia:write({mod_register_aft_ip, treap, Treap1}),
                        ok
                end,
            case mnesia:transaction(F) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    ?ERROR_MSG("mod_register_aft: timeout remove error: ~p~n",
                               [Reason]),
                    ok
            end;
        true ->
            ok
    end.

ip_to_string(Source) when is_tuple(Source) -> inet_parse:ntoa(Source);
ip_to_string(undefined) -> "undefined";
ip_to_string(_) -> "unknown".

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized
write_time({{Y, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                  [Y, Mo, D, H, Mi, S]).

is_strong_password(Server, Password) ->
    LServer = jlib:nameprep(Server),
    case gen_mod:get_module_opt(LServer, ?MODULE, password_strength, 0) of
        Entropy when is_number(Entropy), Entropy >= 0 ->
            if Entropy == 0 ->
                    true;
               true ->
                    ejabberd_auth:entropy(Password) >= Entropy
            end;
        Wrong ->
            ?WARNING_MSG("Wrong value for password_strength option: ~p",
                         [Wrong]),
            true
    end.
