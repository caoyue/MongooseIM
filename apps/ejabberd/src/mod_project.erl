-module(mod_project).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("organization.hrl").

-define(NS_AFT_PROJECT, <<"aft:project">>).


start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_AFT_PROJECT,
        ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_AFT_PROJECT).

%% TOFIX: check project status, some operation is not allowed when project is finished.
process_iq(From, To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    case xml:get_tag_attr_s(<<"type">>, SubEl) of
        <<"list_project">> ->
            list_project(From, To, IQ);
        <<"list_template">> ->
            list_template(From, To, IQ);
        <<"list_member">> ->
            list_member(From, To, IQ);
        <<"create">> ->
            create(From, To, IQ);
        <<"finish">> ->
            finish(From, To, IQ);
        <<"list_children_jobs">> ->
            list_children_jobs(From, To, IQ);
        <<"add_member">> ->
            add_member(From, To, IQ);
        <<"delete_member">> ->
            delete_member(From, To, IQ);
        <<"project_name_exist">> ->
            is_project_name_exist(From, To, IQ);
        _ ->
            io:format("unknow type~n"),
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}
    end;
process_iq(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.


%% ------------------------------------------------------------------
%% higher function. called by process_iq.
%% ------------------------------------------------------------------

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-template
list_template(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    case list_template_ex(S, BaseJID) of
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]};
        Result ->
            IQ#iq{type = result,
                sub_el = [SubEl#xmlel{attrs =
                [{<<"xmlns">>, ?NS_AFT_PROJECT},
                    {<<"type">>, <<"list_template">>}],
                    children = [{xmlcdata,
                        Result}]}]}
    end;
list_template(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#create-project
create(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    io:format("create Data=~p~n", [Data]),
    {_, Name} = lists:keyfind(<<"name">>, 1, Data),
    {_, Template} = lists:keyfind(<<"template">>, 1, Data),

    %% TOFIX: case create_ex(S, Name, BaseJID, Template) of
    case create_ex(S, Name, BaseJID, <<"0">>) of  %% test set template <<"0">>
        {ok, Result} ->
            IQ#iq{type = result,
                sub_el = [SubEl#xmlel{attrs =
                [{<<"xmlns">>, ?NS_AFT_PROJECT},
                    {<<"type">>, <<"create">>}],
                    children = [{xmlcdata,
                        Result}]}]};
        conflict ->
            IQ#iq{type = error, sub_el = [?ERR_CONFLICT]};
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]}
    end;
create(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#finish-project
finish(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    {jid, U, S, _, _, _, _} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    case finish_ex(S, ProID, BaseJID) of
        ok ->
            %% TOFIX: finished all task and all event and all data r-s.
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    push_message(ProID, S, Result, <<"finished">>, <<>>);
                _ ->
                    ok
            end,
            IQ#iq{type = result};
        {error, not_allowed} ->
            IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]};
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]}
    end;
finish(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-project
list_project(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    case list_project_ex(S, BaseJID) of
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]};
        Projects ->
            IQ#iq{type = result,
                sub_el = [SubEl#xmlel{attrs =
                [{<<"xmlns">>, ?NS_AFT_PROJECT},
                    {<<"type">>, <<"list_project">>}],
                    children = [{xmlcdata,
                        Projects}]}]}
    end;
list_project(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-job-that-can-add-by-self
list_children_jobs(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    {jid, U, S, _, _, _, _} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    case list_children_jobs_ex(S, BaseJID, ProID) of
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]};
        Result ->
            IQ#iq{type = result,
                sub_el = [SubEl#xmlel{attrs =
                [{<<"xmlns">>, ?NS_AFT_PROJECT},
                    {<<"type">>, <<"list_children_jobs">>}],
                    children = [{xmlcdata,
                        Result}]}]}
    end;
list_children_jobs(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#add-member
add_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    {jid, U, S, _, _, _, _} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),

    case Data of
        [{ProID, List}] ->
            Ls = [{ID, Name, JID} || {struct, [{_, ID}, {_, Name}, {_, JID}]} <- List],
            case add_member_ex(S, ProID, BaseJID, Ls) of
                failed ->
                    IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]};
                ok ->
                    case odbc_organization:get_all_jid(S, ProID) of
                        {ok, Result} ->
                            Content = mochijson2:encode(List),
                            push_message(ProID, S, Result, <<"add_member">>, Content);
                        _ ->
                            ok
                    end,
                    IQ#iq{type = result}
            end;
        _ ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}
    end;
add_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#delete-member
delete_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    {jid, U, S, _, _, _, _} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    {_, JID} = lists:keyfind(<<"jid">>, 1, Data),
    case delete_member_ex(S, ProID, BaseJID, JID) of
        ok ->
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    Content = <<"{\"jid\":\"", JID, "\"}">>,
                    push_message(ProID, S, Result, <<"delete_member">>, Content);
                _ ->
                    ok
            end,
            IQ#iq{type = result};

    %% TOFIX:push message to delete user or no.
    %% TOFIX: finish task by this user, delete it in event, push msg into task and event, any more???
        nost_exist ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]};
        not_allowed ->
            IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]};
        error ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]}
    end;
delete_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-member
list_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    {jid, U, S, _, _, _, _} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Project} = lists:keyfind(<<"project">>, 1, Data),

    case list_member_ex(S, Project, BaseJID) of
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]};
        not_allowed ->
            IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]};
        {ok, Result} ->
            IQ#iq{type = result,
                sub_el = [SubEl#xmlel{attrs =
                [{<<"xmlns">>, ?NS_AFT_PROJECT},
                    {<<"type">>, <<"list_member">>}],
                    children = [{xmlcdata,
                        Result}]}]}
    end;
list_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% current not used.
is_project_name_exist(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    {jid, _U, S, _, _, _, _} = From,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Name} = lists:keyfind(<<"name">>, 1, Data),
    case odbc_organization:is_project_name_exist(S, Name) of
        {ok, true} ->
            IQ#iq{type = error, sub_el = [?ERR_CONFLICT]};
        {ok, false} ->
            IQ#iq{type = result, sub_el = []};
        {error, _Reson} ->
            IQ#iq{type = error, sub_el = [?ERR_SERVICE_UNAVAILABLE]}
    end;
is_project_name_exist(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.


%% ------------------------------------------------------------------
%% helper function.
%% ------------------------------------------------------------------

is_admin(LServer, BaseJID, ProID) ->
    case odbc_organization:get_admin(LServer, ProID) of
        {ok, []} ->
            false;
        {ok, [{Admin}]} ->
            if Admin =:= BaseJID -> true;
               true -> false
            end;
        {error, _Reason} ->
            failed
    end.

format_jobs_result(ProID, Result) ->
    case Result of
        [] ->
            <<>>;
        _R ->
            Init = <<"{\"", ProID/binary, "\":[">>,
            lists:foldl(
                fun(E, AccIn) ->
                    {E_ID, E_Name} = E,
                    AccIn1 = case AccIn of
                                 Init ->
                                     Init;
                                 _ ->
                                     <<AccIn/binary, ",">>
                             end,
                    <<AccIn1/binary, "{\"job_id\":\"", E_ID/binary,
                    "\", \"job_name\":\"", E_Name/binary, "\"}">>
                end,
                Init,
                Result)
    end.

push_message(ProID, Server, ToList, Type, Contents) ->
    From = jlib:jid_to_binary({ProID, Server, <<>>}),
    LangAttr = {<<"xml:lang">>, <<"en">>},
    FromAttr = {<<"from">>, From},
    TypeAttr = {<<"type">>, <<"chat">>},
    FromJID = jlib:make_jid(ProID, Server, <<>>),
    SubAttrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, Type},{<<"projectid">>, ProID}],

    Packet = {xmlel, <<"message">>, [], [{xmlel, <<"sys">>, SubAttrs, [{xmlcdata, Contents}]}]},
    lists:foreach(
        fun(ToIn) ->
            {To} = ToIn,
            ToAttr = {<<"to">>, To},
            ejabberd_router:route(FromJID, jlib:binary_to_jid(To),
                Packet#xmlel{attrs = [FromAttr, ToAttr, TypeAttr, LangAttr]})
        end,
        ToList).


%% ------------------------------------------------------------------
%% lower function. called by high function, bridge between odbc and higher.
%% ------------------------------------------------------------------
list_project_ex(LServer, BaseJID) ->
    case odbc_organization:list_project(LServer, BaseJID) of
        {ok, Result} ->
            Rs = lists:foldl(
                fun(E, AccIn) ->
                    {ID, Name} = E,
                    AccIn1 = if AccIn =:= <<>> ->
                                    <<>>;
                                 true ->
                                    <<AccIn/binary, ",">>
                             end,
                    <<AccIn1/binary, "{\"id\":\"", ID/binary, "\", \"name\":\"", Name/binary, "\"}">>
                end,
                <<>>,
                Result),
            <<"[", Rs/binary, "]">>;
        _ ->
            failed
    end.

list_template_ex(LServer, BaseJID) ->
    case odbc_organization:list_template(LServer, BaseJID) of
        {ok, Result} ->
            R = lists:foldl(
                fun(E, AccIn) ->
                    {template, ProjectID, ProjectName} = E,
                    AccIn1 = if <<"{\"template\":[">> =:= AccIn ->
                                    AccIn;
                                 true ->
                                    <<AccIn/binary, ",">>
                             end,
                    <<AccIn1/binary, "{\"", ProjectID/binary, "\":\"", ProjectName/binary, "\"}">>
                end,
                <<"{\"template\":[">>,
                Result),

            <<R/binary, "]}">>;
        _ ->
            failed
    end.

create_ex(LServer, ProjectName, BaseJID, Template) ->
    case odbc_organization:is_project_name_exist(LServer, ProjectName) of
        false ->
            case odbc_organization:add_project(LServer, #project{name = ProjectName, description = <<"">>, admin = BaseJID}, Template) of
                {ok, #project{id = Id, name = _Name, description = _Desc}} ->
                    {ok, <<"{\"id\":\"", Id/binary, "\",\"name\":\"", ProjectName/binary, "\"}">>};
                errorReason ->
                    failed
            end;
        true ->
            conflict;
        error ->
            failed
    end.

finish_ex(LServer, ProID, BaseJID) ->
    case is_admin(LServer, BaseJID, ProID) of
        true ->
            case odbc_organization:finish_project(LServer, ProID) of
                ok ->
                    ok;
                {error, _Reason} ->
                    failed
            end;
        false ->
            {error, not_allowed}
    end.

list_children_jobs_ex(LServer, JID, ProID) ->
    case is_admin(LServer, JID, ProID) of
        true ->
            case odbc_organization:get_all_nodes(LServer, ProID) of
                {ok, Result} ->
                    format_jobs_result(ProID, Result);
                {error, _Reason} ->
                    failed
            end;
        false ->
            case odbc_organization:get_job(LServer, JID, ProID) of
                {ok, []} ->
                    {error, not_exist};
                {ok, [JobID]} ->
                    case odbc_organization:get_children_job(LServer, JobID, ProID) of
                        {ok, Result} ->
                            format_jobs_result(ProID, Result);
                        {error, _Reason} ->
                            failed
                    end;
                {error, _Reason} ->
                    failed
            end;
        failed ->
            failed
    end.

add_member_ex(LServer, ProID, BaseJID, List) ->
    %% TOFIX: check add same jid with same job.
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            case odbc_organization:add_employees(LServer, ProID, BaseJID, List) of
                ok ->
                    ok;
                {error, _Reason} ->
                    failed
            end;
        {ok, false} ->
            not_allowed;
        {error, _Reason} ->
            failed
    end.

list_member_ex(LServer, ProID, BaseJID) ->
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            Head = <<"{\"", ProID/binary, "\":[">>,
            case odbc_organization:get_all(LServer, ProID) of
                {ok, Result} ->
                    Rs = lists:foldl(
                        fun(E, AccIn) ->
                            {JID, JobID, JobName, Part} = E,
                            AccIn1 = if AccIn =:= Head -> Head;
                                         true -> <<AccIn/binary, ",">>
                                     end,
                            <<AccIn1/binary, "{\"jid\":\"", JID/binary, "\", \"job_id\":\"", JobID/binary,
                            "\", \"job_name\":\"", JobName/binary, "\", \"part\":\"", Part/binary, "\"}">>
                        end,
                        Head,
                        Result),
                    {ok, <<Rs/binary, "]}">>};
                {error, _Reason} ->
                    failed
            end;
        {ok, false} ->
            not_allowed;
        {error, _Reason} ->
            failed
    end.

delete_member_ex(LServer, ProID, BaseJID, JID) ->
    case is_admin(LServer, BaseJID, ProID) of
        true ->
            case odbc_organization:delete_employee(LServer, ProID, JID) of
                ok ->
                    ok;
                {error, not_exist} ->
                    nost_exist;
                {error, _Reason} ->
                    failed
            end;
        false ->
            case odbc_organization:get_parent_jids(LServer, JID, ProID) of
                {error, nost_exist} ->
                    not_allowed;
                {error, _Reason} ->
                    failed;
                {ok, []} ->
                    not_allowed;
                {ok, List} ->
                    Parent_jid_list = [P_JID || #employee{jid = P_JID} <- List],
                    case lists:member(BaseJID, Parent_jid_list) of
                        true ->
                            case odbc_organization:delete_employee(LServer, ProID, JID) of
                                ok ->
                                    ok;
                                {error, _Reason} ->
                                    failed
                            end;
                        false ->
                            not_allowed
                    end
            end;
        failed ->
            failed
    end.