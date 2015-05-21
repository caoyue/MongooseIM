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
        <<"search_project">> ->
            search_project(From, To, IQ);
        <<"list_template">> ->
            list_template(From, To, IQ);
        <<"get_structure">> ->
            get_structure(From, To, IQ);
        <<"create">> ->
            create(From, To, IQ);
        <<"finish">> ->
            finish(From, To, IQ);
        <<"subscribe">> ->
            subscribe(From, To, IQ);
        <<"subscribed">> ->
            subscribed(From, To, IQ);
        <<"unsubscribed">> ->
            unsubscribed(From, To, IQ);
        <<"unsubscribe">> ->
            unsubscribe(From, To, IQ);
        <<"list_member">> ->
            list_member(From, To, IQ);
        <<"list_member_and_link">> ->
            list_member_and_link(From, To, IQ);
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
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs =[{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_template">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_template(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-structure
get_structure(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, Project} = lists:keyfind(<<"template">>, 1, Data),
    %R1 = get_structure_ex(S, BaseJID, Project),
    case get_structure_ex(S, BaseJID, Project) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"get_structure">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
get_structure(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#create-project
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

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#finish-project
finish(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    case finish_ex(S, ProID, BaseJID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            %% TOFIX: finished all task and all event and all data r-s.
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    push_message(ProID, S, Result, <<"finished">>, <<>>);
                _ ->
                    %% how to fix: check finish statuc before operator???
                    ?ERROR_MSG("[Project:~p] push finish msg failed.", [ProID])
            end,
            IQ#iq{type = result}
    end;
finish(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-project
list_project(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    case list_project_ex(S, BaseJID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_project">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_project(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-job-that-can-add-by-self
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

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#add-member
add_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),

    case Data of
        [{ProID, List}] ->
            Ls = [{ID, Name, JID, Part} || {struct, [{_, ID}, {_, Name}, {_, JID}, {_, Part}]} <- List],
            case add_member_ex(S, ProID, BaseJID, Ls) of
                {error, Error} ->
                    IQ#iq{type = error, sub_el = [SubEl, Error]};
                {ok, ValidList} ->
                    %% why use this, client can show ascaii character.
                    %ListTemp = [{struct, [{<<"job_id">>, V_ID}, {<<"job_name">>, V_Name}, {<<"jid">>, V_JID}, {<<"part">>, V_Part}]}
                    %                    || {V_ID, V_Name, V_JID, V_Part} <- ValidList ],
                    %%ListTemp2 = [ {struct, [{<<"project">>, ProID}]} | ListTemp],
                    %Content = iolist_to_binary(mochijson2:encode(ListTemp)),
                    Content = format_add_content(ValidList),
                    case odbc_organization:get_all_jid(S, ProID) of
                        {ok, Result} ->
                            push_message(ProID, S, Result, <<"add_member">>, Content),
                            %% push_add_message to all link project members.
                            case odbc_organization:get_link_project(S, ProID) of
                                {ok, Result1} ->
                                    lists:foreach(fun({Project}) ->
                                        case odbc_organization:get_all_jid(S, Project) of
                                            {ok, Result2} ->
                                                push_message(ProID, S, Result2, <<"add_member">>, Content),
                                                IQ#iq{type = result};
                                            _ ->
                                                %% if failed how to synchronous to client.
                                                ?ERROR_MSG("[Project:~p] push add member msg failed.", [ProID]),
                                                IQ#iq{type = result}
                                        end
                                        end,
                                        Result1);
                                {error, _} ->
                                    IQ#iq{type = error, sub_el = [?AFT_ERR_DATABASE]}
                            end;
                        _ ->
                            IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_DATABASE]}
                    end,
                    IQ#iq{type = result}
            end;
        _ ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}
    end;
add_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#add-custom-job
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
        ok ->
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    push_message(ProID, S, Result, <<"add_job">>, <<>>),
                    IQ#iq{type = result};
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_DATABASE]}
            end
    end;
add_job(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#delete-member
delete_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = set, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ProID} = lists:keyfind(<<"project">>, 1, Data),
    {_, JID} = lists:keyfind(<<"jid">>, 1, Data),
    case delete_member_ex(S, ProID, BaseJID, JID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            case odbc_organization:get_all_jid(S, ProID) of
                {ok, Result} ->
                    Result0 = [{JID} | Result],
                    Content = <<"{\"jid\":\"", JID, "\"}">>,
                    push_message(ProID, S, Result0, <<"delete_member">>, Content),
                    %% push_add_message to all link project member.
                    case odbc_organization:get_link_project(S, ProID) of
                        {ok, Result1} ->
                            lists:foreach(fun({Project}) ->
                                case odbc_organization:get_all_jid(S, Project) of
                                    {ok, Result2} ->
                                        push_message(ProID, S, Result2, <<"delete_member">>, Content),
                                        IQ#iq{type = result};
                                    _ ->
                                        %% if failed how to synchronous to client.
                                        ?ERROR_MSG("[Project:~p] push delete member msg failed.", [ProID]),
                                        IQ#iq{type = result}
                                end
                                end,
                                Result1);
                        {error, _} ->
                            IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_DATABASE]}
                    end;
                _ ->
                    IQ#iq{type = error, sub_el = [SubEl, ?AFT_ERR_DATABASE]}
            end,
            %% TOFIX:push message to delete user or no ???
            %% TOFIX: finish task by this user, delete it in event, push msg into task and event, any more???
            IQ#iq{type = result}
    end;
delete_member(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.


list_member_and_link(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),

    {_, Project} = lists:keyfind(<<"project">>, 1, Data),
    case list_member_and_link_ex(S, Project, BaseJID) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        {ok, Result} ->
            IQ#iq{type = result,
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_member_and_link_ex">>}],
                                      children = [{xmlcdata, Result}]}]}
    end;
list_member_and_link(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

%% @doc https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#get-member
list_member(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, type = get, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),

    {_, Project} = lists:keyfind(<<"project">>, 1, Data),
    ProjectTarget = case lists:keyfind(<<"project_target">>, 1, Data) of
                         false ->
                             false;
                         {_, Target} ->
                             Target
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
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"list_member_and_link_ex">>}],
                                        children = [{xmlcdata, Result}]}]}
    end;
list_link_project(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

subscribe(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ID1} = lists:keyfind(<<"id_self">>, 1, Data),
    {_, ID2} = lists:keyfind(<<"id_target">>, 1, Data),

    case subscribe_ex(S, BaseJID, ID1, ID2) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            {ok, [{ProName}] } = odbc_organization:project_name(S, ID1),
            Content = <<"{\"id\":\"", ID1/binary, "\"name\":\"", ProName/binary, "\"}">>,
            case get_admin(S, ID2) of
                {true, Admin} ->
                    push_message(ID1, S, [{Admin}], <<"subscribe">>, Content);
                _ ->
                    ?ERROR_MSG("[Project:~p] has no admin.", [ID2])
            end,
            IQ#iq{type = result};
        {ok, ok} ->
            {ok, [{ProName1}] } = odbc_organization:project_name(S, ID1),
            {ok, [{ProName2}] } = odbc_organization:project_name(S, ID2),
            {ok, ID1_Member} = odbc_organization:get_all_jid(S, ID1),
            {ok, ID2_Member} = odbc_organization:get_all_jid(S, ID2),
            Content1 = <<"{\"id\":\"", ID1/binary, "\"name\":\"", ProName1/binary, "\"}">>,
            Content2 = <<"{\"id\":\"", ID2/binary, "\"name\":\"", ProName2/binary, "\"}">>,
            push_message(ID1, S, ID1_Member, <<"subscribed">>, Content2 ),
            push_message(ID1, S, ID2_Member, <<"subscribed">>, Content1 ),
            IQ#iq{type = result}
    end;
subscribe(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

subscribed(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ID1} = lists:keyfind(<<"id_self">>, 1, Data),
    {_, ID2} = lists:keyfind(<<"id_target">>, 1, Data),

    case subscribed_ex(S, BaseJID, ID1, ID2) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            {ok, [{ProName1}] } = odbc_organization:project_name(S, ID1),
            {ok, [{ProName2}] } = odbc_organization:project_name(S, ID2),
            {ok, ID1_Member} = odbc_organization:get_all_jid(S, ID1),
            {ok, ID2_Member} = odbc_organization:get_all_jid(S, ID2),
            Content1 = <<"{\"id\":\"", ID1/binary, "\"name\":\"", ProName1/binary, "\"}">>,
            Content2 = <<"{\"id\":\"", ID2/binary, "\"name\":\"", ProName2/binary, "\"}">>,
            push_message(ID1, S, ID1_Member, <<"subscribed">>, Content2 ),
            push_message(ID1, S, ID2_Member, <<"subscribed">>, Content1 ),
            IQ#iq{type = result}
    end;
subscribed(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

unsubscribed(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ID1} = lists:keyfind(<<"id_self">>, 1, Data),
    {_, ID2} = lists:keyfind(<<"id_target">>, 1, Data),

    case unsubscribed_ex(S, BaseJID, ID1, ID2) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        ok ->
            {ok, [{ProName}] } = odbc_organization:project_name(S, ID1),
            Content = <<"{\"id\":\"", ID1/binary, "\"name\":\"", ProName/binary, "\"}">>,
            case get_admin(S, ID2) of
                {true, Admin} ->
                    push_message(ID1, S, [{Admin}], <<"unsubscribed">>, Content);
                _ ->
                    ?ERROR_MSG("[Project:~p] has no subscrited~p.", [ID2, ID1])
            end,

            IQ#iq{type = result}
    end;
unsubscribed(_, _, IQ) ->
    IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}.

unsubscribe(From, _To, #iq{xmlns = ?NS_AFT_PROJECT, sub_el = SubEl} = IQ) ->
    #jid{user = U, server = S} = From,
    BaseJID = <<U/binary, "@", S/binary>>,
    {struct, Data} = mochijson2:decode(xml:get_tag_cdata(SubEl)),
    {_, ID1} = lists:keyfind(<<"id_self">>, 1, Data),
    {_, ID2} = lists:keyfind(<<"id_target">>, 1, Data),

    case unsubscribe_ex(S, BaseJID, ID1, ID2) of
        ok ->
            {ok, [{ProName1}] } = odbc_organization:project_name(S, ID1),
            {ok, [{ProName2}] } = odbc_organization:project_name(S, ID2),
            {ok, ID1_Member} = odbc_organization:get_all_jid(S, ID1),
            {ok, ID2_Member} = odbc_organization:get_all_jid(S, ID2),
            Content1 = <<"{\"id\":\"", ID1/binary, "\"name\":\"", ProName1/binary, "\"}">>,
            Content2 = <<"{\"id\":\"", ID2/binary, "\"name\":\"", ProName2/binary, "\"}">>,
            push_message(ID1, S, ID1_Member, <<"unsubscribe">>, Content2 ),
            push_message(ID1, S, ID2_Member, <<"unsubscribe">>, Content1 ),
            IQ#iq{type = result};
        {ok, clear} ->
            IQ#iq{type = result};
        not_exist ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]};
        not_allowed ->
            IQ#iq{type = error, sub_el = [?ERR_NOT_ALLOWED]};
        failed ->
            IQ#iq{type = error, sub_el = [?ERR_BAD_REQUEST]}
    end;
unsubscribe(_, _, IQ) ->
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
                  sub_el = [SubEl#xmlel{attrs = [{<<"xmlns">>, ?NS_AFT_PROJECT}, {<<"type">>, <<"create">>}],
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
            {ok, <<"[", Rs/binary, "]">>};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.

list_template_ex(LServer, BaseJID) ->
    case odbc_organization:list_template(LServer, BaseJID) of
        {ok, Result} ->
            R = lists:foldl(
                fun(E, AccIn) ->
                    {ProjectID, ProjectName} = E,
                    AccIn1 = if <<"{\"template\":[">> =:= AccIn ->
                                    AccIn;
                                true ->
                                    <<AccIn/binary, ",">>
                             end,
                    <<AccIn1/binary, "{\"id\":\"", ProjectID/binary, "\", \"name\":\"", ProjectName/binary, "\"}">>
                end,
                <<"{\"template\":[">>,
                Result),
            {ok, <<R/binary, "]}">>};
        _ ->
            {error, ?AFT_ERR_DATABASE}
    end.

get_structure_ex(LServer, BaseJID, ProID) ->
    Valid = case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
                {ok, true} ->
                    true;
                {ok, false} ->
                    is_predefine_template(LServer, ProID);
                _ ->
                    failed
            end,
    case Valid of
        true ->
            case odbc_organization:get_structure(LServer, ProID) of
                {ok, Result} ->
                    Rs = lists:foldl(
                        fun(E, AccIn) ->
                            {I, N, L, R, P} = E,
                            AccIn1 = if <<>> =:= AccIn ->
                                            AccIn;
                                        true ->
                                            <<AccIn/binary, ",">>
                                     end,
                            <<AccIn1/binary, "{\"id\":\"", I/binary, "\", \"name\":\"", N/binary,
                            "\", \"left\":\"", L/binary, "\", \"right\":\"", R/binary, "\", \"part\":\"", P/binary, "\"}">>
                        end,
                        <<>>,
                        Result),
                    {ok, <<"[", Rs/binary, "]">>};
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            {error, ?AFT_ERR_INVALID_TEMPLATE};
        failed ->
            {error, ?AFT_ERR_DATABASE}
    end.

create_ex(LServer, ProjectName, BaseJID, Template, Job) ->
    case odbc_organization:is_project_name_exist(LServer, ProjectName) of
        {ok, false} ->
            case odbc_organization:node_exist(LServer, Template, Job) of
                {ok, true} ->
                    case odbc_organization:add_project(LServer, #project{name = ProjectName, description = <<"">>, admin = BaseJID}, Template, Job) of
                        {ok, #project{id = Id, name = _Name, description = _Desc}, Part} ->
                            {ok, <<"{\"id\":\"", Id/binary, "\",\"name\":\"", ProjectName/binary,
                            "\",\"job\":\"", Job/binary, "\",\"part\":\"", Part/binary, "\"}">>};
                        {error, _} ->
                            {error, ?AFT_ERR_DATABASE}
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_INVALID_JOB};
                {error, _} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        {ok, true} ->
            {error, ?AFT_ERR_PROJECT_NAME_EXIST};
        {error, _} ->
            {error, ?AFT_ERR_DATABASE}
    end.

finish_ex(LServer, ProID, BaseJID) ->
    case is_admin(LServer, BaseJID, ProID) of
        true ->
            case odbc_organization:finish_project(LServer, ProID) of
                ok ->
                    ok;
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
    end.

subscribe_ex(LServer, JID, ProSource, ProTarget) ->
    case is_admin(LServer, JID, ProSource) of
        true ->
            case odbc_organization:is_project_exist(LServer, ProTarget) of
                {ok, true} ->
                    case odbc_organization:is_subscribe_exist(LServer, ProSource, ProTarget) of
                        {ok, true} ->
                            case odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource) of
                                {ok, true} -> {error, ?AFT_ERR_ALLREADY_SUBSCRIBED};
                                {ok, false} -> ok;
                                {error, _} -> {error, ?AFT_ERR_DATABASE}
                            end;
                        {ok, false} ->
                            case odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource) of
                                {ok, true} ->
                                    case odbc_organization:subscribe(LServer, ProSource, ProTarget) of
                                        ok -> {ok, ok};
                                        {error, _} -> {error, ?AFT_ERR_DATABASE}
                                    end;
                                {ok, false} ->
                                    case odbc_organization:subscribe(LServer, ProSource, ProTarget) of
                                        ok -> ok;
                                        {error, _} -> {error, ?AFT_ERR_DATABASE}
                                    end;
                                {error, _} ->
                                    {error, ?AFT_ERR_DATABASE}
                            end;
                        {error, _} ->
                            {error, ?AFT_ERR_DATABASE}
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_PROJECT_NOT_EXIST}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        failed ->
            {error, ?AFT_ERR_DATABASE}
    end.

subscribed_ex(LServer, JID, ProSource, ProTarget) ->
    case is_admin(LServer, JID, ProSource) of
        true ->
            case odbc_organization:is_project_exist(LServer, ProTarget) of
                {ok, true} ->
                    case odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource) of
                        {ok, true} ->
                            case odbc_organization:is_subscribe_exist(LServer, ProSource, ProTarget) of
                                {ok, true} ->
                                    {error, ?AFT_ERR_ALLREADY_SUBSCRIBED};
                                {ok, false} ->
                                    case odbc_organization:subscribe(LServer, ProSource, ProTarget) of
                                        ok -> ok;
                                        {error, _} -> {error, ?AFT_ERR_DATABASE}
                                    end;
                                {error, _} ->
                                    {error, ?AFT_ERR_DATABASE}
                            end;
                        {ok, false} ->
                            {error, ?AFT_ERR_TARGET_NO_SUBSCRIBE_REQUEST};
                        {error, _} ->
                            {error, ?AFT_ERR_DATABASE}
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_PROJECT_NOT_EXIST};
                {error, _} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        failed ->
            {error, ?AFT_ERR_DATABASE}
    end.


unsubscribed_ex(LServer, JID, ProSource, ProTarget) ->
    case is_admin(LServer, JID, ProSource) of
        true ->
            case  odbc_organization:is_project_exist(LServer, ProTarget) of
                {ok, true} ->
                    case odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource) of
                        {ok, true} ->
                            case odbc_organization:is_subscribe_exist(LServer, ProSource, ProTarget) of
                                {ok, true} ->
                                    {error, ?AFT_ERR_ALLREADY_SUBSCRIBED};
                                {ok, false} ->
                                    odbc_organization:unsubscribe(LServer, ProTarget, ProSource);
                                {error, _} ->
                                    {error, ?AFT_ERR_DATABASE}
                            end;
                        {ok, false} ->
                            {error, ?AFT_ERR_TARGET_NO_SUBSCRIBE_REQUEST};
                        {error, _} ->
                            {error, ?AFT_ERR_DATABASE}
                    end;
                {ok, false} ->
                    {error, ?AFT_ERR_PROJECT_NOT_EXIST};
                {error, _} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        failed ->
            {error, ?AFT_ERR_DATABASE}
    end.

unsubscribe_ex(LServer, JID, ProSource, ProTarget) ->
    case is_admin(LServer, JID, ProSource) of
        true ->
            case odbc_organization:is_project_exist(LServer, ProTarget) of
                {ok, true} ->
%%                     case odbc_organization:is_subscribe_exist(LServer, ProSource, ProTarget) of
%%                         {ok, true} ->
%%                             case odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource) of
%%                                 {ok, true} ->
%%                                     odbc_organization:unsubscribe(LServer, ProSource, ProTarget),
%%                                     odbc_organization:unsubscribe(LServer, ProTarget, ProSource);
%%                                 {ok, false} ->
%%                                     odbc_organization:unsubscribe(LServer, ProSource, ProTarget),
%%                                     {ok, clear};
%%                                 {error, _} ->
%%                                     {error, ?AFT_ERR_DATABASE}
%%                             end;
%%                         {ok, false} ->
%%                             case odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource) of
%%                                 {ok, true} ->
%%                                     odbc_organization:unsubscribe(LServer, ProTarget, ProSource),
%%                                     {ok, clear};
%%                                 {ok, false} ->
%%                                     {error, ?AFT_ERR_NO_SUBSCRIBED};
%%                                 {error, _} ->
%%                                     {error, ?AFT_ERR_DATABASE}
%%                             end;
%%                         {error, _} ->
%%                             {error, ?AFT_ERR_DATABASE}
%%                     end;
                    case {odbc_organization:is_subscribe_exist(LServer, ProSource, ProTarget),
                          odbc_organization:is_subscribe_exist(LServer, ProTarget, ProSource)} of
                        {{ok, true}, {ok, true}} ->
                            odbc_organization:unsubscribe(LServer, ProSource, ProTarget),
                            odbc_organization:unsubscribe(LServer, ProTarget, ProSource);
                        {{error, _}, _} ->
                            {error, ?AFT_ERR_DATABASE};
                        {_, {error, _}} ->
                            {error, ?AFT_ERR_DATABASE};
                        _ ->
                            {error, ?AFT_ERR_NO_SUBSCRIBED}
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
            R = lists:foldl(
                fun(E, AccIn) ->
                    {E_ID, E_Name} = E,
                    AccIn1 = if AccIn =:= <<>> -> <<>>;
                                 ture -> <<AccIn/binary, ",">>
                             end,
                    <<AccIn1/binary, "{\"id\":\"", E_ID/binary, "\", \"name\":\"", E_Name/binary, "\"}">>
                end,
                <<>>,
                Result),
            {ok, <<"[", R/binary, "]">>};
        {error, _Reson} ->
            failed
    end.

list_children_jobs_ex(LServer, JID, ProID) ->
    case is_admin(LServer, JID, ProID) of
        true ->
            case odbc_organization:get_all_nodes(LServer, ProID) of
                {ok, Result} ->
                    {ok, format_jobs_result(ProID, Result)};
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            case odbc_organization:get_job(LServer, JID, ProID) of
                {ok, []} ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                {ok, [JobID]} ->
                    case odbc_organization:get_children_job(LServer, JobID, ProID) of
                        {ok, Result} ->
                            {ok, format_jobs_result(ProID, Result)};
                        {error, _Reason} ->
                            {ok, ?AFT_ERR_DATABASE}
                    end;
                {error, _Reason} ->
                    {ok, ?AFT_ERR_DATABASE}
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
            {ValidJobList, _InvalidJobList} = lists:partition(fun({E_ID, _, _, _}) ->
                                            case lists:keyfind(E_ID, 1, Result) of
                                                false -> false;
                                                _ -> true
                                            end
                                            end,
                                            List),
            case odbc_organization:get_all(LServer, ProID) of
                {ok, Result2} ->
                    ExistList = lists:filtermap(fun({E_JID, E_ID, E_Name, E_Part}) ->
                                                    {true, {E_ID, E_Name, E_JID, E_Part}}
                                                end,
                                                Result2),
                    {ValidList, _DuplicationList} = lists:partition(fun(E) ->
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
            end;
        _ ->
            {error, ?AFT_ERR_DATABASE}
    end.

add_member_ex(LServer, ProID, BaseJID, List) ->
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            case check_add_member_valid(LServer, ProID, BaseJID, List) of
                {error, ERROR} ->
                    {error, ERROR};
                {ok, ValidList} ->
                    case odbc_organization:add_employees(LServer, ProID, BaseJID, ValidList) of
                        ok ->
                            {ok, ValidList};
                        {error, _Reason} ->
                            {error, ?AFT_ERR_DATABASE}
                    end
            end;
        {ok, false} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.

add_job_ex(LServer, ProID, BaseJID, ParentJobID, JobName, Part) ->
    case odbc_organization:get_job_info(LServer, ParentJobID) of
        {ok, []} ->
            {error, ?AFT_ERR_PARENT_NOT_EXIST};
        {ok, [{_,_,_,_,_,ParentPart,_}]} ->
            Valid = case is_admin(LServer, BaseJID, ProID) of
                        false ->
                            case odbc_organization:get_job(LServer, BaseJID, ProID) of
                                {ok, [{MyJob}]} ->
                                    if MyJob =:= ParentJobID -> true;
                                       true -> false
                                    end;
                                {ok, []} -> false;
                                _ -> failed
                            end;
                        true -> true
                    end,
            case Valid of
                failed -> {error, ?AFT_ERR_DATABASE};
                false -> {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                true ->
                    if  ParentPart =:= Part ->
                        case odbc_organization:add_node(LServer, ParentJobID, #node{name=JobName, description=Part}) of
                            {ok, _} ->
                                ok;
                            {error, _} ->
                                {error, ?AFT_ERR_DATABASE}
                        end;
                        true ->
                            case odbc_organization:get_description_member_parent(LServer, ProID, Part) of
                                {ok, ParentJobList} ->
                                    case lists:member({ParentJobID}, ParentJobList) of
                                        true ->
                                            case odbc_organization:add_node(LServer, ParentJobID, #node{name=JobName, description=Part}) of
                                                {ok, _} ->
                                                    ok;
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
    end.


list_link_project_ex(LServer, ProID, BaseJID) ->
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            case odbc_organization:get_link_project(LServer, ProID) of
                {ok, Result} ->
                    Array = lists:foldl(
                        fun(E1, AccIn) ->
                            {E1_ID, E1_Name} = E1,
                            AccIn1 = if AccIn =:= <<>> -> <<>>;
                                        ture -> <<AccIn/binary, ",">>
                                     end,
                            <<AccIn1/binary, "{\"id\":\"", E1_ID/binary, "\", \"name\":\"", E1_Name/binary, "\"}">>
                        end,
                        <<>>,
                        Result),
                    {ok, <<"{\"", ProID/binary, "\":[", Array/binary, "]}">>};
                {error, _Reason2} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        {ok, false} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.


list_member_and_link_ex(LServer, ProID, BaseJID) ->
    case odbc_organization:is_memeber(LServer, ProID, BaseJID) of
        {ok, true} ->
            case odbc_organization:get_all(LServer, ProID) of
                {ok, Result} ->
                    case odbc_organization:get_link_project(LServer, ProID) of
                        {ok, Result2} ->
                            FirstArray = lists:foldl(
                                fun(E1, AccIn) ->
                                    {E1_ID, E1_Name} = E1,
                                    AccIn1 = if AccIn =:= <<>> -> <<>>;
                                                 ture -> <<AccIn/binary, ",">>
                                             end,
                                    <<AccIn1/binary, "{\"id\":\"", E1_ID/binary, "\", \"name\":\"", E1_Name/binary, "\"}">>
                                end,
                                <<>>,
                                Result2),
                            SecondArray = lists:foldl(
                                fun(E, AccIn) ->
                                    {JID, JobID, JobName, Part} = E,
                                    AccIn1 = if AccIn =:= <<>> -> <<>>;
                                                 true -> <<AccIn/binary, ",">>
                                             end,
                                    <<AccIn1/binary, "{\"jid\":\"", JID/binary, "\", \"job_id\":\"", JobID/binary,
                                    "\", \"job_name\":\"", JobName/binary, "\", \"part\":\"", Part/binary, "\"}">>
                                end,
                                <<>>,
                                Result),
                            {ok, <<"{\"", ProID/binary, "\":[[", FirstArray/binary, "], [", SecondArray/binary, "]}">>};
                        {error, _Reason2} ->
                            {error, ?AFT_ERR_DATABASE}
                    end;
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
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
                    case odbc_organization:get_all(LServer, ProID) of
                        {ok, Result} ->
                            {ok, format_member_list(ProID, Result)};
                        {error, _Reason} ->
                            {error, ?AFT_ERR_DATABASE}
                    end;
                _ ->
                    case odbc_organization:is_link_member(LServer, ProID, BaseJID, ProjectTarget) of
                        {ok, true} ->
                            case odbc_organization:get_all(LServer, ProjectTarget) of
                                {ok, Result} ->
                                    {ok, format_member_list(ProjectTarget, Result)};
                                {error, _Reason} ->
                                    {error, ?AFT_ERR_DATABASE}
                            end;
                        {ok, false} ->
                            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                        {error, _Reason} ->
                            {error, ?AFT_ERR_DATABASE}
                    end
            end;
        {ok, false} ->
            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
        {error, _Reason} ->
            {error, ?AFT_ERR_DATABASE}
    end.

delete_member_ex(LServer, ProID, BaseJID, JID) ->
    case is_admin(LServer, BaseJID, ProID) of
        true ->
            case odbc_organization:delete_employee(LServer, ProID, JID) of
                ok ->
                    ok;
                {error, not_exist} ->
                    {error, ?AFT_ERR_MEMBER_NOT_EXIST};
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE}
            end;
        false ->
            case odbc_organization:get_parent_jids(LServer, JID, ProID) of
                {error, nost_exist} ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                {error, _Reason} ->
                    {error, ?AFT_ERR_DATABASE};
                {ok, []} ->
                    {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH};
                {ok, List} ->
                    Parent_jid_list = [P_JID || #employee{jid = P_JID} <- List],
                    case lists:member(BaseJID, Parent_jid_list) of
                        true ->
                            case odbc_organization:delete_employee(LServer, ProID, JID) of
                                ok ->
                                    ok;
                                {error, _Reason} ->
                                    {error, ?AFT_ERR_DATABASE}
                            end;
                        false ->
                            {error, ?AFT_ERR_PRIVILEGE_NOT_ENOUGH}
                    end
            end;
        failed ->
            {error, ?AFT_ERR_DATABASE}
    end.


%% ------------------------------------------------------------------
%% helper function.
%% ------------------------------------------------------------------

is_predefine_template(LServer, ProID) ->
    case odbc_organization:list_template(LServer, <<"0">>) of
        {ok, Result} ->
            case lists:keyfind(ProID, 1, Result) of
                false ->
                    false;
                {_, _} ->
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

format_jobs_result(ProID, Result) ->
    case Result of
        [] ->
            <<>>;
        _R ->
            Init = <<"{\"", ProID/binary, "\":[">>,
            R1 =
                lists:foldl(
                    fun(E, AccIn) ->
                        {E_ID, E_Name, E_Part} = E,
                        AccIn1 = case AccIn of
                                     Init -> Init;
                                     _ -> <<AccIn/binary, ",">>
                                 end,
                        <<AccIn1/binary, "{\"job_id\":\"", E_ID/binary, "\", \"job_name\":\"", E_Name/binary,
                        "\", \"part\":\"", E_Part/binary, "\"}">>
                    end,
                    Init,
                    Result),
            <<R1/binary, "]}">>
    end.

format_add_content(Members) ->
    Array = lists:foldl(
        fun({E_ID, E_Name, E_JID, E_Part}, AccIn) ->
            AccIn1 = if AccIn =:= <<>> -> <<>>;
                         true -> <<AccIn/binary, ",">>
                     end,
            <<AccIn1/binary, "{\"jod_id\":\"", E_ID/binary,   "\", \"job_name\":\"", E_Name/binary,
            "\", \"jid\":\"", E_JID/binary, "\", \"part\":\"", E_Part/binary, "\"}">>
        end,
        <<>>,
        Members),
    <<"[", Array/binary, "]">>.

%% Members is result of odbc_organization:get_all()
format_member_list(ProID, Members) ->
    Array = lists:foldl(
        fun(E, AccIn) ->
            {JID, JobID, JobName, Part} = E,
            AccIn1 = if AccIn =:= <<>> -> <<>>;
                         true -> <<AccIn/binary, ",">>
                     end,
            <<AccIn1/binary, "{\"jid\":\"", JID/binary, "\", \"job_id\":\"", JobID/binary,
            "\", \"job_name\":\"", JobName/binary, "\", \"part\":\"", Part/binary, "\"}">>
        end,
        <<>>,
        Members),
    <<"{\"", ProID/binary, "\":[", Array/binary, "]}">>.

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
