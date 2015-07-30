-module(mod_project).

-behaviour(gen_mod).

%% API
-export([start/2, stop/1, process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("organization.hrl").

-define(NS_AFT_PROJECT, <<"aft:project">>).


start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_AFT_PROJECT,
                                  ?MODULE, process_iq, no_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_AFT_PROJECT,
                                  ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_AFT_PROJECT),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_AFT_PROJECT).

process_iq(From, To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    case xml:get_tag_attr_s(<<"type">>, SubEl) of
        <<"list_project">> ->
            list_project(From, To, IQ, list_project);
        <<"list_template">> ->
            list_project(From, To, IQ, list_template);
        <<"search_project">> ->
            search_project(From, To, IQ);
        <<"get_project">> ->
            get_project(From, To, IQ);
        <<"get_structure">> ->
            get_structure(From, To, IQ);
        <<"create">> ->
            create(From, To, IQ);
        <<"finish">> ->
            finish(From, To, IQ);
        <<"set_photo">> ->
            set_photo(From, To, IQ);
        <<"get_photo">> ->
            get_photo(From, To, IQ);
        <<"subscribe">> ->
            subscribe(From, To, IQ, subscribe);
        <<"subscribed">> ->
            subscribe(From, To, IQ, subscribed);
        <<"unsubscribed">> ->
            subscribe(From, To, IQ, unsubscribed);
        <<"unsubscribe">> ->
            subscribe(From, To, IQ, unsubscribe);
        <<"list_member">> ->
            list_member(From, To, IQ);
        <<"list_link_project">> ->
            list_link_project(From, To, IQ);
        <<"list_children_jobs">> ->
            list_children_jobs(From, To, IQ);
        <<"add_member">> ->
            add_member(From, To, IQ);
        <<"add_job">> ->
            add_job(From, To, IQ);
        <<"delete_member">> ->
            delete_member(From, To, IQ);
        <<"project_name_exist">> ->
            is_project_name_exist(From, To, IQ);
        _ ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}
    end;
process_iq(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.


%% ------------------------------------------------------------------
%% higher function. called by process_iq.
%% ------------------------------------------------------------------

get_project(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Project} = lists:keyfind(<<"project">>, 1, Data),

    case get_project_ex(S, BaseJID, Project) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"get_project">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
get_project(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

get_structure(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Project} = lists:keyfind(<<"project">>, 1, Data),
    ProjectTarget = case lists:keyfind(<<"project_target">>, 1, Data) of
                        false -> false;
                        {_, Target} -> Target
                    end,

    case get_structure_ex(S, BaseJID, Project, ProjectTarget) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"get_structure">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
get_structure(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

create(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Name} = lists:keyfind(<<"name">>, 1, Data),
    {_, Template} = lists:keyfind(<<"template">>, 1, Data),
    {_, Job} = lists:keyfind(<<"job">>, 1, Data),

    case create_ex(S, Name, BaseJID, Template, Job) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"create">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
create(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

finish(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),

    case finish_ex(S, ProID, BaseJID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, EndTime} ->
            %% TOFIX: finished all task and all event and all data r-s.
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    Content = <<"{\"end_time\":\"", EndTime/binary, "\"}">>,
                    push_message(ProID, S, Result, <<"finished">>, Content);
                _ ->
                    %% how to fix: check finish status before operator???
                    ?ERROR_MSG("[Project:~p] push finish msg failed.", [ProID])
            end,
            IQ#iq{type = result}
    end;
finish(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

set_photo(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    {_, Photo} = lists:keyfind(<<"photo">>, 1, Data),

    case set_photo_ex(S, ProID, BaseJID, Photo) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            F = mochijson2:encoder([{utf8, true}]),
            Content = iolist_to_binary(F({struct, [{<<"project">>, ProID}, {<<"photo">>, Photo}]})) ,
            {ok, Result} = odbc_organization:get_all_jid(S, ProID),
            push_message(ProID, S, Result, <<"set_photo">>, Content),
            IQ#iq{type = result}
    end;
set_photo(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.


get_photo(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),

    case get_photo_ex(S, ProID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"get_photo">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
get_photo(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.




list_project(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ, Type) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,

    case list_project_ex(S, BaseJID, Type) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            SubType = atom_to_binary(Type, latin1),
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, SubType}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_project(_, _, IQ, _Type) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

list_children_jobs(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    case list_children_jobs_ex(S, BaseJID, ProID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_children_jobs">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_children_jobs(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

add_job(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    {_, ParentJobID} = lists:keyfind(<<"parent_job_id">>, 1, Data),
    {_, JobName} = lists:keyfind(<<"job_name">>, 1, Data),
    {_, Part} = lists:keyfind(<<"part">>, 1, Data),

    case add_job_ex(S, ProID, BaseJID, ParentJobID, JobName, Part) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result, JobTag} ->
            {ok, AllMembers} = odbc_organization:get_all_jid(S, ProID),
            Content = <<"{\"job_tag\":\"", JobTag/binary, "\"}">>,
            push_message(ProID, S, AllMembers, <<"add_job">>, Content),
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"add_job">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
add_job(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

add_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    {_, Members} = lists:keyfind(<<"member">>, 1, Data),

    if length(Members) > 0 ->
        Ls = parse_json_list([<<"job_id">>, <<"jid">>], Members, []),
        case add_member_ex(S, ProID, BaseJID, Ls) of
            {error, Error} ->
                IQ#iq{type = error, sub_el = [SubEl, Error]};
            {ok, MemberTag, ValidMembers} ->
                Content1 = [{struct, [{<<"job_id">>, V1}, {<<"jid">>, V2}]} || {V1, V2} <- ValidMembers],
                F = mochijson2:encoder([{utf8, true}]),
                Content = iolist_to_binary(F({struct, [{<<"project">>, ProID}, {<<"member_tag">>, MemberTag}, {<<"member">>, Content1}]})) ,
                case odbc_organization:get_all_jid(S, ProID) of
                    {ok, Result} ->
                        push_message(ProID, S, Result, <<"add_member">>, Content),
                        %% push notice to all link project members.
                        {ok, Result1} = odbc_organization:get_link_project(S, ProID),
                        lists:foreach(fun({Project}) ->
                            {ok, Result2} = odbc_organization:get_all_jid(S, Project),
                            push_message(ProID, S, Result2, <<"add_member">>, Content)
                        end,
                        Result1),
                        IQ#iq{type = result};
                    _ ->
                        IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_DATABASE]}
                end
        end;
       true ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}
    end;
add_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

delete_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    {_, JID} = lists:keyfind(<<"jid">>, 1, Data),
    case delete_member_ex(S, ProID, BaseJID, JID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, MemberTag} ->
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    Result0 = [{JID} | Result], %% include delete member.
                    Content = <<"{\"project\":\"", ProID/binary,  "\", \"member_tag\":\"", MemberTag/binary,  "\", \"member\":[\"", JID/binary, "\"]}">>,
                    push_message(ProID, S, Result0, <<"delete_member">>, Content),
                    %% push notice to all link project member.
                    {ok, Result1} = odbc_organization:get_link_project(S, ProID),
                    lists:foreach(fun({Project}) ->
                        {ok, Result2} = odbc_organization:get_all_jid(S, Project),
                         push_message(ProID, S, Result2, <<"delete_member">>, Content)
                    end,
                    Result1),
                    IQ#iq{type = result};
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_DATABASE]}
            end
            %% TOFIX: finish task by this user, delete it in task? , push notice into task and event, any more???
    end;
delete_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

list_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),

    {_, Project} = lists:keyfind(<<"project">>, 1, Data),
    ProjectTarget = case lists:keyfind(<<"project_target">>, 1, Data) of
                        false -> false;
                        {_, Target} -> Target
                    end,

    case list_member_ex(S, Project, BaseJID, ProjectTarget) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_member">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

list_link_project(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Project} = lists:keyfind(<<"project">>, 1, Data),

    case list_link_project_ex(S, Project, BaseJID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_link_project">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_link_project(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

subscribe(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ, Type) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, IDSelf} = lists:keyfind(<<"id_self">>, 1, Data),
    {_, IDTarget} = lists:keyfind(<<"id_target">>, 1, Data),

    case subscribe_ex(S, BaseJID, IDSelf, IDTarget, Type) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            {ok, [{ProNameSelf}]} = odbc_organization:project_name(S, IDSelf),
            {ok, [{ProNameTarget}]} = odbc_organization:project_name(S, IDTarget),
            Content = <<"{\"id_self\":\"", IDTarget/binary, "\", \"name_self\":\"", ProNameTarget/binary,
                        "\", \"id_target\":\"", IDSelf/binary, "\", \"name_target\":\"", ProNameSelf/binary, "\"}">>,
            case get_admin(S, IDTarget) of
                {true, Admin} ->
                    push_message(IDSelf, S, [{Admin}], atom_to_binary(Type, latin1), Content);
                _ ->
                    ?ERROR_MSG("[Project:~p] has no admin.", [IDTarget])
            end,
            IQ#iq{type = result};
        {ok, LinkTag} ->
            {ok, [{ProNameSelf}]} = odbc_organization:project_name(S, IDSelf),
            {ok, [{ProNameTarget}]} = odbc_organization:project_name(S, IDTarget),
            {ok, IDSelf_Member} = odbc_organization:get_all_jid(S, IDSelf),
            {ok, IDTarget_Member} = odbc_organization:get_all_jid(S, IDTarget),
            Content1 = <<"{\"id_self\":\"", IDSelf/binary, "\", \"name_self\":\"", ProNameSelf/binary, "\", \"link_tag\":\"", LinkTag/binary,
                         "\", \"id_target\":\"", IDTarget/binary, "\", \"name_target\":\"", ProNameTarget/binary, "\"}">>,
            Content2 = <<"{\"id_self\":\"", IDTarget/binary, "\", \"name\":\"", ProNameTarget/binary, "\", \"link_tag\":\"", LinkTag/binary,
                         "\", \"id_target\":\"", IDSelf/binary, "\", \"name_target\":\"", ProNameSelf/binary, "\"}">>,
            push_message(IDSelf, S, IDSelf_Member, atom_to_binary(Type, latin1), Content1),
            push_message(IDTarget, S, IDTarget_Member, atom_to_binary(Type, latin1), Content2),
            IQ#iq{type = result}
    end;
subscribe(_, _, IQ, _) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

search_project(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    #jid{server = S} = From,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Name} = lists:keyfind(<<"name">>, 1, Data),

    case search_project_ex(S, Name) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"search_project">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
search_project(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% current not used.
is_project_name_exist(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Name} = lists:keyfind(<<"name">>, 1, Data),
    case odbc_organization:is_project_name_exist(S, Name) of
        {ok, true} ->
            IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_PROJECT_NAME_EXIST]};
        {ok, false} ->
            IQ#iq{type = result, sub_el = [SubEl]};
        {error, _Reson} ->
            IQ#iq{type = error, sub_el = [?AFT_ERR_DATABASE]}
    end;
is_project_name_exist(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.


%% ------------------------------------------------------------------
%% lower function. called by higher function, bridge between odbc and higher function.
%% ------------------------------------------------------------------

get_project_ex(LServer, BaseJID, ProID) ->
    Valid = case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
                {ok, true} -> true;
                {ok, false} -> is_predefine_template(LServer, ProID);
                _ -> failed
            end,
    case Valid of
        true ->
            {ok, Result} = odbc_organization:get_project(LServer, ProID),
            F = mochijson2:encoder([{utf8, true}]),
            Json1 = [{struct, [{"id", R1}, {"name", R2}, {"description", R3}, {"status", R4},
                               {"admin", R5}, {"start_time", R6}, {"end_time", R7}, {"job_tag", R8},
                               {"member_tag", R9}, {"link_tag", R10}]}
                     || {R1, R2, R3, R4, R5, R6, R7, R8, R9, R10} <- Result ],
            Json = iolist_to_binary( F( Json1 ) ),
            {ok, Json};
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

list_project_ex(LServer, BaseJID, Type) ->
    R = case Type of
            list_project ->
                odbc_organization:list_project(LServer, BaseJID, false);
            list_template ->
                odbc_organization:list_project(LServer, BaseJID, true)
        end,
    case R of
        {ok, Result} ->
            F = mochijson2:encoder([{utf8, true}]),
            Json1 = [{struct, [{"id", R1}, {"name", R2}, {"description", R3},
                                {"photo",
                                    case R4 of
                                        null -> <<>>;
                                        <<>> -> <<>>;
                                        _ -> list_to_binary(make_head_url(binary_to_list(R4)))
                                    end },
                               {"status", R5}, {"admin", R6}, {"start_time", R7}, {"end_time", R8},
                               {"job_tag", R9}, {"member_tag", R10}, {"link_tag", R11}]}
                     || {R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11} <- Result ],
            Json = iolist_to_binary( F( Json1 ) ),
            {ok, Json};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.

%% remove check authority???  %% (LServer, ProjectSelf, Jid, ProjectTarget)
get_structure_ex(LServer, BaseJID, ProID, ProjectTarget) ->
    Valid = case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
                {ok, true} ->
                    case odbc_organization:is_link_member(LServer, ProID, BaseJID, ProjectTarget) of
                        {ok, true} -> {true, ProjectTarget};
                        {ok, false} ->{true, ProID}
                    end;
                {ok, false} -> {is_predefine_template(LServer, ProID), ProID}
            end,
    case Valid of
        {true, Project} ->
            {ok, Result} = odbc_organization:get_structure(LServer, Project),
            Json1 = [{struct,[{<<"id">>, R1}, {<<"name">>, R2}, {<<"left">>, R3}, {<<"right">>, R4}, {<<"part">>, R5}]}
                     || {R1, R2, R3, R4, R5}<- Result],
            F = mochijson2:encoder([{utf8, true}]),
            Json = iolist_to_binary( F( {struct, [{<<"project">>, Project}, {<<"structure">>, Json1}]} )),
            {ok, Json};
        {false, _} ->
            {error, ?AFT_ERR_INVALID_TEMPLATE}
    end.

create_ex(LServer, ProjectName, BaseJID, Template, Job) ->
    case odbc_organization:is_project_name_exist(LServer, ProjectName) of
        {ok, false} ->
            case odbc_organization:node_exist(LServer, Template, Job) of
                {ok, true} ->
                    case odbc_organization:add_project(LServer, #project{name = ProjectName, description = <<"">>, admin = BaseJID}, Template, Job) of
                        {ok, #project{id = Id, name = _Name, photo = Photo ,description = _Desc, job_tag = JobTag, start_at = StartTime},
                         #node{id = JobId, name=JobName, lft = Left, rgt = Right, department = Part}} ->
                            PhotoURL = list_to_binary(make_head_url(binary_to_list(Photo))),
                            {ok, <<"{\"project\":{\"id\":\"", Id/binary, "\",\"name\":\"", ProjectName/binary,
                                    "\",\"photo\":\"", PhotoURL/binary, "\",\"job_tag\":\"", JobTag/binary,
                                    "\",\"member_tag\":\"", JobTag/binary, "\",\"link_tag\":\"", JobTag/binary,
                                    "\",\"start_time\":\"", StartTime/binary, "\"},
                                    \"job\":{\"job_id\":\"", JobId/binary, "\",\"job_name\":\"", JobName/binary,
                                    "\",\"left\":\"", Left/binary, "\",\"right\":\"", Right/binary,
                                    "\",\"part\":\"", Part/binary, "\"},
                                    \"member\":{\"jid\":\"", BaseJID/binary, "\"}}">>};
                        {error, _} ->
                            ?ERROR_MSG("Create Project failed, ProjectName=~p.", [ProjectName]),
                            {error, ?AFT_ERR_DATABASE}
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_INVALID_JOB}
            end;
        {ok, true} ->
            {error, ?AFT_ERR_PROJECT_NAME_EXIST};
        {error, _} ->
            {error, ?AFT_ERR_DATABASE}
    end.

finish_ex(LServer, ProID, BaseJID) ->
    case is_admin(LServer, BaseJID, ProID) of
        true ->
            case odbc_organization:project_status(LServer, ProID) of
                {ok, [{<<"1">>}]} ->
                    case odbc_organization:finish_project(LServer, ProID) of
                        {ok, EndTime} -> {ok, EndTime};
                        {error, _Reason} -> {error, ?AFT_ERR_DATABASE}
                    end;
                {ok, [{<<"0">>}]} ->
                    {error, ?AFT_ERR_ALLREADY_FINISHED};
                {ok, _} ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

subscribe_status({ok, true},  {ok, false}, subscribe) -> subscribe_again;
subscribe_status({ok, false}, {ok, true},  subscribe) -> subscribed;
subscribe_status({ok, false}, {ok, false}, subscribe) -> subscribe;
subscribe_status(_,           {ok, false}, subscribed) -> {error, ?AFT_ERR_TARGET_NO_SUBSCRIBE_REQUEST};
subscribe_status({ok, false}, {ok, true},  subscribed) -> subscribed;
subscribe_status(_,           {ok, false}, unsubscribed) -> {error, ?AFT_ERR_TARGET_NO_SUBSCRIBE_REQUEST};
subscribe_status({ok, false}, {ok, true},  unsubscribed) -> unsubscribed;
subscribe_status({ok, true},  {ok, true},  unsubscribe) -> unsubscribe;
subscribe_status(_,           _,           unsubscribe) -> {error, ?AFT_ERR_NO_SUBSCRIBED};
subscribe_status({ok, true},  {ok, true},  _) -> {error, ?AFT_ERR_ALLREADY_SUBSCRIBED}.

subscribe_ex(LServer, JID, ProSource, ProTarget, Type) ->
    case is_admin(LServer, JID, ProSource) of
        true ->
            case odbc_organization:is_project_exist(LServer, ProTarget) of
                {ok, true} ->
                    To = odbc_organization:is_subscribe_exist(LServer, ProSource, ProTarget),
                    From = odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource),
                    case subscribe_status(To, From, Type) of
                        {error, Error} -> {error, Error};
                        subscribe ->
                            case odbc_organization:subscribe(LServer, ProSource, ProTarget) of
                                ok -> ok;
                                _ -> {error, ?AFT_ERR_DATABASE}
                            end;
                        subscribe_again ->
                            ok;
                        subscribed ->
                            case odbc_organization:subscribed(LServer, ProSource, ProTarget) of
                                {ok, LinkTag} -> {ok, LinkTag};
                                _ -> {error, ?AFT_ERR_DATABASE}
                            end;
                        unsubscribed ->
                            odbc_organization:unsubscribed(LServer, ProTarget, ProSource);
                        unsubscribe ->
                            odbc_organization:unsubscribe(LServer, ProSource, ProTarget)
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_PROJECT_NOT_EXIST}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        failed ->
            {error, ?AFT_ERR_DATABASE}
    end.

search_project_ex(LServer, Name) ->
    case odbc_organization:search_project(LServer, Name) of
        {ok, Result} ->
            %%{ok, build_json(["id", "name"], Result, true, <<>>)};
            Json = [{struct, [{<<"id">>, R1}, {<<"name">>, R2}]} || {R1, R2} <- Result],
            F = mochijson2:encoder([{utf8, true}]),
            {ok, iolist_to_binary(F(Json))};
        {error, _Reson} ->
            failed
    end.

list_children_jobs_ex(LServer, JID, ProID) ->
    case is_admin(LServer, JID, ProID) of
        true ->
            {ok, Result} = odbc_organization:get_all_nodes(LServer, ProID),
            %%{ok, build_json(["job_id", "job_name", "part"], Result, true, ProID)};
            Json = [{struct, [{<<"job_id">>, R1}, {<<"job_name">>, R2}, {<<"part">>, R3}]}
                    || {R1, R2, R3} <- Result],
            F = mochijson2:encoder([{utf8, true}]),
            {ok, iolist_to_binary(F({struct, [{<<"project">>, ProID}, {<<"job">>, Json}]}))};
        false ->
            case odbc_organization:get_job(LServer, JID, ProID) of
                {ok, []} ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                {ok, [{JobID}]} ->
                    {ok, Result} = odbc_organization:get_children_job(LServer, JobID, ProID),
                                                %{ok, build_json(["job_id", "job_name", "part"], Result, true, ProID)}
                    Json = [{struct, [{<<"job_id">>, R1}, {<<"job_name">>, R2}, {<<"part">>, R3}]}
                            || {R1, R2, R3} <- Result],
                    F = mochijson2:encoder([{utf8, true}]),
                    {ok, iolist_to_binary(F({struct, [{<<"project">>, ProID}, {<<"job">>, Json}]}))}
            end;
        failed ->
            {ok, ?AFT_ERR_DATABASE}
    end.

check_add_member_valid(LServer, ProID, BaseJID, List) ->
    %% TOFIX: check BaseJID is List jid 's parent in project ???
    %% use this solution:
    %% 1 BaseJID must in project;
    %% 2 update jid and id in to organization_user, ignore other property.
    case odbc_organization:get_structure(LServer, ProID) of
        {ok, Result} ->
            {ValidJobList, _InvalidJobList} = lists:partition(
                                                fun({E_ID, _}) ->
                                                        case lists:keyfind(E_ID, 1, Result) of
                                                            false -> false;
                                                            _ -> true
                                                        end
                                                end,
                                                List),
            {ok, Result2} = odbc_organization:get_all(LServer, ProID),
            ExistList = lists:filtermap(
                          fun({E_JID, E_ID, _E_Name, _E_Part}) ->
                                  {true, {E_ID, E_JID}}
                          end,
                          Result2),
            {ValidList, _DuplicationList} = lists:partition(
                                              fun(E) ->
                                                      case lists:member(E, ExistList) of
                                                          false -> true;
                                                          _ -> false
                                                      end
                                              end,
                                              ValidJobList),
            ?ERROR_MSG("add_member invalid job list:~p~n,duplication list:~p",
                       [_InvalidJobList, _DuplicationList]),
            {ok, ValidList};
        _ ->
            {error, ?AFT_ERR_DATABASE}
    end.

add_member_ex(LServer, ProID, BaseJID, List) ->
    case odbc_organization:project_status(LServer, ProID) of
        {ok, [{<<"1">>}]} ->
            case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
                {ok, true} ->
                    case check_add_member_valid(LServer, ProID, BaseJID, List) of
                        {error, Error} ->
                            {error, Error};
                        {ok, ValidList} ->
                            if length(ValidList) > 0 ->
                                    case odbc_organization:add_employees(LServer, ProID, BaseJID, ValidList) of
                                        {ok, MemberTag} ->
                                            {ok, MemberTag, ValidList};
                                        {error, _Reason} ->
                                            {error, ?AFT_ERR_DATABASE}
                                    end;
                               true ->
                                    {error, ?AFT_ERR_MEMBER_INVALID}
                            end
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        {ok, [{<<"0">>}]} ->
            {error, ?AFT_ERR_ALLREADY_FINISHED};
        {ok, _} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

add_job_ex(LServer, ProID, BaseJID, ParentJobID, JobName, Part) ->
    case odbc_organization:project_status(LServer, ProID) of
        {ok, [{<<"1">>}]} ->
            case odbc_organization:get_job_info(LServer, ParentJobID) of
                {ok, []} ->
                    {error, ?AFT_ERR_PARENT_NOT_EXIST};
                {ok, [{_, _, _, _, _, ParentPart, _}]} ->
                    Valid = case is_admin(LServer, BaseJID, ProID) of
                                false ->
                                    case odbc_organization:get_job(LServer, BaseJID, ProID) of
                                        {ok, [{MyJob}]} ->
                                            if MyJob =:= ParentJobID -> true;
                                                true -> false
                                            end;
                                        {ok, []} -> false
                                    end;
                                true -> true
                            end,
                    case Valid of
                        false -> {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                        true ->
                            if ParentPart =:= Part ->
                                case odbc_organization:add_node(LServer, ParentJobID, #node{name = JobName, department = Part}) of
                                    {ok, JobTag, #node{id = Id, lft = Left, rgt = Right}} ->
                                        {ok, build_json([{"project", ProID}, {"job", {["id", "name", "left", "right", "part"],
                                            [{Id, JobName, Left, Right, Part}], false}}], <<>>), JobTag};
                                    {error, _} ->
                                        {error, ?AFT_ERR_DATABASE}
                                end;
                                true ->
                                    case odbc_organization:get_department_member_parent(LServer, ProID, Part) of
                                        {ok, ParentJobList} ->
                                            case lists:member({ParentJobID}, ParentJobList) of
                                                true ->
                                                    case odbc_organization:add_node(LServer, ParentJobID, #node{name = JobName, department = Part}) of
                                                        {ok, JobTag, #node{id = Id, lft = Left, rgt = Right}} ->
                                                            {ok, build_json([{"project", ProID}, {"job", {["id", "name", "left", "right", "part"],
                                                                [Id, JobName, Left, Right, Part], false}}], <<>>), JobTag};
                                                        {error, _} ->
                                                            {error, ?AFT_ERR_DATABASE}
                                                    end;
                                                false ->
                                                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
                                            end
                                    end
                            end
                    end;
                _ ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        {ok, [{<<"0">>}]} ->
            {error, ?AFT_ERR_ALLREADY_FINISHED};
        {ok, _} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

list_link_project_ex(LServer, ProID, BaseJID) ->
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            {ok, Result} = odbc_organization:get_link_project(LServer, ProID),
            %%{ok, build_json([ {"self_project", ProID}, {"link_project", {["id", "name"], Result, true}} ], <<>>)};
            Json = [{struct, [{<<"id">>, R1}, {<<"name">>, R2}]} || {R1, R2} <- Result],
            F = mochijson2:encoder([{utf8, true}]),
            {ok, iolist_to_binary(F({struct, [{<<"self_project">>, ProID}, {<<"link_project">>, Json}]}))};
        {ok, false} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.

list_member_ex(LServer, ProID, BaseJID, ProjectTarget) ->
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            case ProjectTarget of
                false ->
                    {ok, Result} = odbc_organization:get_all(LServer, ProID),
                    %%{ok, build_json([ {"project", ProID}, {"member", {["jid", "job_id", "job_name", "part"], Result, true}} ], <<>>)};
                    Json = [{struct, [{<<"jid">>, R1}, {<<"job_id">>, R2}]}
                            || {R1, R2, _R3, _R4} <- Result],
                    F = mochijson2:encoder([{utf8, true}]),
                    {ok, iolist_to_binary(F({struct, [{<<"project">>, ProID}, {<<"member">>, Json}]}))};
                _ ->
                    case odbc_organization:is_link_member(LServer, ProID, BaseJID, ProjectTarget) of
                        {ok, true} ->
                            {ok, Result} = odbc_organization:get_all(LServer, ProjectTarget),
                            %%{ok, build_json([ {"project", ProjectTarget}, {"member", {["jid", "job_id", "job_name", "part"], Result, true}} ], <<>>)};
                            Json = [{struct, [{<<"jid">>, R1}, {<<"job_id">>, R2}]}
                                    || {R1, R2, _R3, _R4} <- Result],
                            F = mochijson2:encoder([{utf8, true}]),
                            {ok, iolist_to_binary(F({struct, [{<<"project">>, ProjectTarget}, {<<"member">>, Json}]}))};
                        {ok, false} ->
                            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
                    end
            end;
        {ok, false} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.

delete_member_ex(LServer, ProID, BaseJID, JID) ->
    case odbc_organization:project_status(LServer, ProID) of
        {ok, [{<<"1">>}]} ->
            case BaseJID =:= JID of
                true ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                _ ->
                    case is_admin(LServer, BaseJID, ProID) of
                        true ->
                            case odbc_organization:delete_employee(LServer, ProID, JID) of
                                {ok, MemberTag} ->
                                    {ok, MemberTag};
                                {error, not_exist} ->
                                    {error, ?AFT_ERR_MEMBER_NOT_EXIST};
                                {error, _Reason} ->
                                    {error, ?AFT_ERR_DATABASE}
                            end;
                        false ->
                            case odbc_organization:get_parent_jids(LServer, JID, ProID) of
                                {error, not_exists} ->
                                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                                {ok, []} ->
                                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                                {ok, List} ->
                                    Parent_jid_list = [P_JID || #employee{jid = P_JID} <- List],
                                    case lists:member(BaseJID, Parent_jid_list) of
                                        true ->
                                            case odbc_organization:delete_employee(LServer, ProID, JID) of
                                                {ok, MemberTag} ->
                                                    {ok, MemberTag};
                                                {error, _Reason} ->
                                                    {error, ?AFT_ERR_DATABASE}
                                            end;
                                        false ->
                                            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
                                    end
                            end
                    end
            end;
        {ok, [{<<"0">>}]} ->
            {error, ?AFT_ERR_ALLREADY_FINISHED};
        {ok, _} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

set_photo_ex(LServer, ProID, BaseJID, Photo) ->
    case is_admin(LServer, BaseJID, ProID) of
        true ->
            case odbc_organization:set_photo(LServer, ProID, Photo) of
                ok -> ok;
                _ -> {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

get_photo_ex(LServer, ProID) ->
    case odbc_organization:get_photo(LServer, ProID) of
        {ok, []} ->
            {ok, <<"{\"project\":\"", ProID/binary, "\", \"photo\":\"\"}">>};
        {ok, [{Photo}]} ->
            {ok, <<"{\"project\":\"", ProID/binary, "\", \"photo\":\"", Photo/binary ,"\"}">>}
    end.


%% ------------------------------------------------------------------
%% helper function.
%% ------------------------------------------------------------------

is_predefine_template(LServer, ProID) ->
    case odbc_organization:list_project(LServer, <<"0">>, true) of
        {ok, Result} ->
            case lists:keyfind(ProID, 1, Result) of
                false ->
                    false;
                _ ->
                    true
            end;
        {error, _Reason} ->
            failed
    end.

get_admin(LServer, ProID)->
    case odbc_organization:get_admin(LServer, ProID) of
        {ok, []} ->
            {false, empty};
        {ok, [{Admin}]} ->
            {true, Admin};
        {error, _Reason} ->
            {false, failed}
    end.

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

push_message(ProID, Server, ToList, Type, Contents) ->
    From = jlib:jid_to_binary({ProID, Server, <<>>}),
    LangAttr = {<<"xml:lang">>, <<"en">>},
    FromAttr = {<<"from">>, From},
    TypeAttr = {<<"type">>, <<"chat">>},
    FromJID = jlib:make_jid(ProID, Server, <<>>),
    SubAttrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, Type}, {<<"projectid">>, ProID}],
    Packet = {xmlel, <<"message">>, [], [{xmlel, <<"sys">>, SubAttrs, [{xmlcdata, Contents}]}]},
    lists:foreach(
      fun(ToIn) ->
              {To} = ToIn,
              ToAttr = {<<"to">>, To},
              ejabberd_router:route(FromJID, jlib:binary_to_jid(To),
                                    Packet#xmlel{attrs = [FromAttr, ToAttr, TypeAttr, LangAttr]})
      end,
      ToList).

build_item([], [], Result) ->
    Result;
build_item([F1 | T1], [F2 | T2], Result) ->
    NF2 = if F2 =:= null -> <<>>; true -> F2 end,
    NewResult = if Result =:= <<>> ->
                        iolist_to_binary([Result, "\"", F1, "\":\"", NF2, "\""]);
                   true -> iolist_to_binary([Result, ", \"", F1, "\":\"", NF2, "\""])
                end,
    build_item(T1, T2, NewResult).

build_json([{Key, {ItemNameList, TupleItemValueList, AddBracket}} | Tail], Result) ->
    Count = length(TupleItemValueList),
    Array = lists:foldl(
              fun(E, AccIn) ->
                      AccIn1 = if AccIn =:= <<>> -> AccIn;
                                  true -> <<AccIn/binary, ",">>
                               end,
                      Item = build_item(ItemNameList, tuple_to_list(E), <<>>),
                      if Item =:= <<>> -> AccIn1;
                         true -> <<AccIn1/binary, "{", Item/binary, "}">>
                      end
              end,
              <<>>, TupleItemValueList),
    Temp = if (Result =:= <<>>) or (Array =:=<<>>) -> Result;
              true -> <<Result/binary, ",">>
           end,
    NewResult =
        if (Count > 1) or (AddBracket =:= true) ->
                iolist_to_binary( [Temp, "\"", Key, "\":[", Array, "]"  ]);
           true ->
                iolist_to_binary( [Temp, "\"", Key, "\":", Array ] )
        end,
    build_json(Tail, NewResult);
build_json([{Key, Value} | Tail], Result) ->
    build_json( [{Key, Value, false} | Tail], Result);
build_json([{Key, Value, AddBracket} | Tail], Result) ->
    Temp = if (Result =:= <<>>) or (Value =:=<<>>) -> Result;
              true -> <<Result/binary, ",">> end,
    NewResult =
        if (AddBracket =:= true) ->
                iolist_to_binary( [Temp, "\"", Key, "\":[\"", Value, "\"]"  ]);
           true ->
                iolist_to_binary( [Temp, "\"", Key, "\":\"", Value, "\"" ] )
        end,
    build_json(Tail, NewResult);
build_json([], Result) ->
    <<"{", Result/binary, "}">>.


parse_item([], _, Result) ->
    list_to_tuple(lists:reverse(Result));
parse_item([H | T], List, Result) ->
    R = case lists:keyfind(H, 1, List) of
            false -> Result;
            {H, Value} -> [Value | Result]
        end,
    parse_item(T, List, R).
parse_json_list(_, [], Result) ->
    lists:reverse(Result);
parse_json_list(Keys, [{struct, H} | T], Result) ->
    R = parse_item(Keys, H, []),
    parse_json_list(Keys, T, [R | Result]).


s3_bucket_url(PublicBucket) ->
    {ok, Host} = application:get_env(ejabberd, s3_host),
    Bucket = case PublicBucket of
                     true ->
                         {ok, Result} = application:get_env(ejabberd, s3_public_bucket),
                         Result;
                     _ ->
                         {ok, Result} = application:get_env(ejabberd, s3_bucket),
                         Result
                 end,

    %% which method is more effective ???
    %% list:flatten( ["http://" | [Bucket | ["." | Host]] ] ).
    "http://" ++ Bucket ++ "." ++ Host.

make_head_url( File ) ->
    URL = s3_bucket_url(true),
    case string:right(URL, 1) of
        "/" ->
            string:concat(URL, File);
        _ ->
            URL ++ "/" ++ File
    end.

