-module(odbc_organization).

%% API
-export([
         %%test/1,
         get_all/2,
         get_parents/2,
         get_children/2,
         delete_node/2,
         update_node/3,
         add_node/3,
         node_exist/3,
         get_all_nodes/2,
         get_structure/2,
         add_employees/4,
         delete_employee/3,
         add_employee/3,
         remove_employee/3,
         add_project/4,
         finish_project/2,
         list_project/3,
         get_project/2,
         set_photo/3,
         get_photo/2,
         project_status/2,
         project_name/2,
         get_link_project/2,
         is_project_exist/2,
         is_project_name_exist/2,
         get_admin/2,
         get_all_jid/2,
         get_parent_jids/3,
         get_children_job/3,
         get_job/3,
         is_memeber/3,
         is_link_member/4,
         search_project/2,
         subscribe/3,
         subscribed/3,
         is_subscribe_exist/3,
         unsubscribed/3,
         unsubscribe/3,
         get_job_info/2,
         get_department_member_parent/3
        ]).

-include("jlib.hrl").
-include("organization.hrl").

-type pro_type() :: binary() | integer().

%% test(C) ->
%%     A = case C of
%%             1 ->
%%                 get_all(<<"localhost">>, <<"1">>);
%%             2 ->
%%                 get_parents(<<"localhost">>, #node{lft = <<"2">>, rgt = <<"7">>, project = <<"1">>});
%%             3 ->
%%                 get_children(<<"localhost">>, #node{lft = <<"2">>, rgt = <<"7">>, project = <<"1">>});
%%             4 ->
%%                 delete_node(<<"localhost">>, <<"2">>);
%%             5 ->
%%                 delete_node(<<"localhost">>, <<"5">>);
%%             6 ->
%%                 add_node(<<"localhost">>, <<"2">>, #node{name = <<"testadd">>, description = <<"adfadf">>, project = <<"1">>});
%%             7 ->
%%                 add_project(<<"localhost">>, #project{name = <<"testproject">>, description = <<"">>, admin = <<"jid@test">>}, <<"0">>)
%%         end,
%%     io:format("~p~n", [A]).

%% =====================================================================================================
%% api function
%% =====================================================================================================

-spec get_all(binary(), integer()) -> {ok, list()} | {error, _}.
get_all(LServer, Project) ->
    Query = ["select ou.jid, o.id, o.name, o.department from organization as o left join organization_user as ou ",
             "on o.id = ou.organization where ou.project = ", Project, " ;"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>, <<"id">>, <<"name">>, <<"department">>], Rs} ->
            %%{ok, employees_to_records(Rs)};
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_all_jid(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_all_jid(LServer, Project) ->
    Query = ["select jid from organization_user where project=", Project, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec is_link_member(binary(), odbc_organization:pro_type(), binary(), odbc_organization:pro_type()) -> {ok, true} | {ok, false} | {error, _}.
is_link_member(LServer, ProjectSelf, Jid, ProjectTarget) ->
    Query = ["select count(id1) from project_link join organization_user on id1 = project and id2='",
             ProjectTarget, "' or id1='", ProjectTarget,"'and id2 = project where organization_user.jid='",
             Jid,"' and project='", ProjectSelf , "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected,[<<"count(id1)">>],[{<<"2">>}]} ->
            {ok, true};
        {selected,[<<"count(id1)">>],[{<<"0">>}]} ->
            {ok, false};
        Reason ->
            {error, Reason}
    end.

-spec get_parent_jids(binary(), binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_parent_jids(LServer, Jid, Project)->
    Query = ["select lft, rgt from organization where id in (select organization from organization_user where project=",
             Project, " and  jid='", Jid, "');" ],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"lft">>, <<"rgt">>], []} ->
            {error, not_exists};
        {selected, [<<"lft">>, <<"rgt">>], [{Left, Right}]} ->
            get_parents(LServer, #node{lft=Left, rgt=Right, project=Project});
        Reason ->
            {error, Reason}
    end.

-spec get_parents(binary(), #node{}) -> {ok, _} | {error, _}.
get_parents(LServer, #node{lft = Left, rgt = Right, project = Project}) ->
    Query = ["select ou.jid, o.id, o.name from organization as o left join organization_user as ou ",
             "on o.id = ou.organization where o.lft < ", Left, " and o.rgt > ",
             Right, " and o.project = ", Project, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>, <<"id">>, <<"name">>], Rs} ->
            {ok, employees_to_records(Rs)};
        Reason ->
            {error, Reason}
    end.

-spec get_structure(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_structure(LServer, Project) ->
    Query = ["select id, name, lft, rgt, department from organization where project='", Project, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>, <<"lft">>, <<"rgt">>, <<"department">>], Rs } ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_all_nodes(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_all_nodes(LServer, Project) ->
    Query = ["select id, name, department from organization where project = '", Project, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>, <<"department">>], Rs } ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_link_project(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_link_project(LServer, Project) ->
    Query = ["select project_link.id2, project.name, project.photo, project.job_tag, project.member_tag
             from project_link, project where project_link.id1='", Project, "' and project_link.id2 =
             project.id and project_link.id2 in (select id1 from project_link where id2='", Project ,"');"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id2">>, <<"name">>, <<"photo">>, <<"job_tag">>, <<"member_tag">>], Rs } ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec is_memeber(binary(), odbc_organization:pro_type(), binary()) -> {ok, true} | {ok, false} | {error, _}.
is_memeber(LServer, Project, Jid) ->
    Query = ["select id from organization_user where project ='", Project, "' and jid = '", ejabberd_odbc:escape(Jid), "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>], []} ->
            Query1 = ["select id from project where id ='", Project, "' and admin ='", ejabberd_odbc:escape(Jid), "';"],
            case ejabberd_odbc:sql_query(LServer, Query1) of
                {selected, [<<"id">>], []} ->
                    {ok, false};
                {selected, [<<"id">>], _Rs} ->
                    {ok, true};
                Reason ->
                    {error, Reason}
            end;
        {selected, [<<"id">>], _R} ->
            {ok, true};
        Reason ->
            {error, Reason}
    end.

%% get a random member in department, then get it all parent job id.
-spec get_department_member_parent(binary(), odbc_organization:pro_type(), binary()) -> {ok, list()} | {error, _}.
get_department_member_parent(LServer, Project, Department) ->
    Query = ["select lft, rgt from organization where project='", Project, "' and department='", ejabberd_odbc:escape(Department), "' limit 1;"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"lft">>, <<"rgt">>], [{Left, Right}]} ->
            Query1 = ["select id from organization where lft<'", Left, "' and rgt >'", Right, "' and project='", Project, "';"],
            {selected, [<<"id">>], Rs } = ejabberd_odbc:sql_query(LServer, Query1),
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_job_info(binary(), binary()) -> {ok, list()} | {error, _}.
get_job_info(LServer, Job) ->
    Query = ["select id, name, lft, rgt, depth, department, project from organization where id ='", Job, "';" ],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>, <<"lft">>, <<"rgt">>, <<"depth">>, <<"department">>, <<"project">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_job(binary(), binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_job(LServer, Jid, Project) ->
    Query = ["select organization from organization_user where project ='", Project, "' and jid = '", Jid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"organization">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_children_job(binary(), binary(), odbc_organization:pro_type()) -> {ok, _} | {error, _}.
get_children_job(LServer, Id, Project) ->
    case ejabberd_odbc:sql_query( LServer, ["select lft, rgt from organization where id='", Id, "';"] ) of
        {selected, [<<"lft">>, <<"rgt">>], [{Left, Right}]} ->
            case ejabberd_odbc:sql_query(LServer, ["select id, name, department from organization where project='", Project, "' and lft>", Left, " and rgt<", Right, ";" ]) of
                {selected, [<<"id">>, <<"name">>, <<"department">>], Rs} ->
                    {ok, Rs};
                Reason ->
                    {error, Reason}
            end;
        {selected, [<<"lft">>, <<"rgt">>],  []} ->
            {error, no_exists};
        Reason ->
            {error, Reason}
    end.

-spec get_children(binary(), #node{}) -> {ok, _} | {error, _}.
get_children(LServer, #node{lft = Left, rgt = Right, project = Project}) ->
    Query = ["select ou.jid, o.id, o.name from organization as o left join organization_user as ou ",
             "on o.id = ou.organization where o.lft > ", Left, " and o.rgt < ",
             Right, " and project = ", Project, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>, <<"id">>, <<"name">>], Rs} ->
            {ok, employees_to_records(Rs)};
        Reason ->
            {error, Reason}
    end.

-spec delete_node(binary(), binary()) -> ok | {error, _}.
delete_node(LServer, NodeId) ->
    F = fun() ->
        Q1 = ["select lft,rgt,project from organization where id=", NodeId, ";"],
        {selected, [<<"lft">>, <<"rgt">>, <<"project">>], [{Left, Right, Project}]} = ejabberd_odbc:sql_query_t(Q1),
        case binary_to_integer(Right) - binary_to_integer(Left) of
            1 ->
                Q2 = [["delete from organization where id = ", NodeId, ";"],
                    ["update organization set lft=lft-2 where lft > ", Left, " and project = ", Project, ";"],
                    ["update organization set rgt=rgt-2 where rgt > ", Right, " and project = ", Project, ";"]],
                sql_list(Q2);
            _ ->
                error
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, ok} ->
            ok;
        {atomic, error} ->
            {error, not_valid};
        Reason ->
            {error, Reason}
    end.

-spec update_node(binary(), #node{}, binary()) -> ok | {error, _}.
update_node(LServer, #node{id = Id}, NewName) ->
    Query = ["update organization set name = ", ejabberd_odbc:escape(NewName), " where id = ", Id, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec add_node(binary(), integer(), #node{}) -> {ok, #node{}} | {error, _}.
add_node(LServer, ParentId, #node{name = Name, department = Department} = _Node) ->
    TimeString = now_random(),
    F = fun() ->
        {selected, [<<"lft">>, <<"rgt">>, <<"depth">>, <<"project">>], [{Left, Right, Depth, Project}]} =
            ejabberd_odbc:sql_query_t(["select lft,rgt,depth,project from organization where id=", ParentId, ";"]),
        {Left1, Right1, Depth1} = {plus(Left, 1), plus(Left, 2), plus(Depth, 1)},
        Query = [
            ["update organization set lft=lft+2 where lft > ", Left, " and project = ", Project, ";"],
            ["update organization set rgt=rgt+2 where rgt > ", Left, " and project = ", Project, ";"],
            ["insert into organization(name,lft,rgt,depth,department,project) values('",
                ejabberd_odbc:escape(Name), "',", Left1, ",", Right1, ",", Depth1, ",'",
                ejabberd_odbc:escape(Department), "',", Project, ");"],
            ["update project set job_tag='", TimeString, "' where id='", Project, "';"]

        ],
        sql_list(Query),
        {selected, [<<"id">>], [{Id}]} = ejabberd_odbc:sql_query_t(["select last_insert_id() as id;"]),
        {ok, #node{id = Id, lft = Left1, rgt = Right1, depth = Depth1, name = Name, department = Department}}
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {ok, Rs}} ->
            {ok, TimeString, Rs};
        Reason ->
            {error, Reason}
    end.

-spec node_exist(binary(), binary(), binary()) -> {ok, _} | {error, _}.
node_exist(LServer, Project, Id) ->
    Query = ["select id from organization where id ='", Id, "' and project='", Project, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>], []} ->
            {ok, false};
        {selected, [<<"id">>], _Rs} ->
            {ok, true};
        Reason ->
            {error, Reason}
    end.

-spec add_employees(binary(), binary(), binary(), list()) -> ok | {error, _}.
add_employees(LServer, Project, _Adder, List) ->
    TimeString = now_random(),
    Init = <<"insert into organization_user(organization, jid, project) values">>,
    Temp = lists:foldl(fun(E, AccIn) ->
        {Id, Jid} = E,
        AccIn1 = if AccIn =:= Init ->
            Init;
                     true ->
                         <<AccIn/binary, ",">>
                 end,
        <<AccIn1/binary, "(", Id/binary, ", '", Jid/binary, "', ", Project/binary, ")">>
    end,
        Init,
        List),
    Query1 = <<Temp/binary, ";">>,
    Size = lists:flatlength(List),
    Query2 = ["update project set member_tag='", TimeString, "' where id='", Project, "';"],

    F = fun() ->
        case ejabberd_odbc:sql_query_t(Query1) of
            {updated, Size} ->
                ejabberd_odbc:sql_query_t(Query2),
                ok;
            Reason ->
                Reason
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, ok} ->
            {ok, TimeString};
        Reason ->
            {error, Reason}
    end.

-spec delete_employee(binary(), odbc_organization:pro_type(), binary()) -> ok | {error, no_exist} | {error, _}.
delete_employee(LServer, Project, Jid) ->
    TimeString = now_random(),
    Query1 = ["delete from organization_user where project=", Project, " and jid='", Jid, "';"],
    Query2 = ["update project set member_tag='", TimeString, "' where id='", Project, "';"],

    F = fun() ->
        case ejabberd_odbc:sql_query_t(Query1) of
            {updated, 1} ->
                ejabberd_odbc:sql_query_t(Query2),
                ok;
            {updated, 0} ->
                not_exist;
            Reason ->
                Reason
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, ok} ->
            {ok, TimeString};
        not_exist ->
            {error, not_exist};
        Reason ->
            {error, Reason}
    end.

-spec add_employee(binary(), integer(), binary()) -> ok | {error, _}.
add_employee(LServer, NodeId, Jid) ->
    Query = ["insert into organization_user(organization, jid) values(", NodeId, ",'", Jid, "');"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec remove_employee(binary(), integer(), binary()) -> ok | {error, _}.
remove_employee(LServer, NodeId, Jid) ->
    Query = ["delete from organization where organization_user = ", NodeId, " and jid = '", Jid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            {error, not_exists};
        Reason ->
            {error, Reason}
    end.

-spec add_project(binary(), #project{}, binary(), binary) -> {ok, #project{}, #node{}} | {error, _}.
add_project(LServer, #project{name = Name, description = Desc, admin = Admin}, TemplateId, Job) ->
    TimeString = now_random(),
    F = fun() ->
        {selected, [<<"photo">>], [{Photo}]} = ejabberd_odbc:sql_query_t(["select photo from project where id='", TemplateId, "';"]),
        Query1 = ["insert into project(name,description,photo,admin,job_tag,member_tag, link_tag) values('",
            ejabberd_odbc:escape(Name), "','", ejabberd_odbc:escape(Desc), "','", Photo, "','",
            ejabberd_odbc:escape(Admin), "','", TimeString, "','", TimeString, "','", TimeString, "');"],
        ejabberd_odbc:sql_query_t(Query1),
        {selected, [<<"id">>, <<"start_at">>], [{Id, StartTime}]} = ejabberd_odbc:sql_query_t(["select id, start_at from project where id=last_insert_id();"]),
        Query2 = ["insert into organization(name,lft,rgt,depth,department,project) select name,lft,rgt,depth,department,",
            Id, " from organization where project =", TemplateId, ";"],
        ejabberd_odbc:sql_query_t(Query2),
        Query3 = ["select o1.id, o1.name, o1.lft, o1.rgt, o1.department from organization o1, organization o2 where o1.lft=o2.lft and o2.id='", Job, "' and o1.project='", Id, "';"],
        {selected, [<<"id">>, <<"name">>, <<"lft">>, <<"rgt">>, <<"department">>], [{NewJob, JobName, Left, Right, Department}]} = ejabberd_odbc:sql_query_t(Query3),
        Query4 = ["insert into organization_user(organization, jid, project) values('", NewJob, "', '", Admin, "', '", Id, "');"],
        ejabberd_odbc:sql_query_t(Query4),
        {Id, StartTime, Photo, NewJob, JobName, Left, Right, Department}
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {Id, StartTime, Photo, NewJob, JobName, Left, Right, Department}} ->
            {ok, #project{id = Id, name = Name, description = Desc, photo = Photo, job_tag = TimeString, start_at = StartTime},
                #node{id = NewJob, name = JobName, lft = Left, rgt = Right, department = Department}};
        Reason ->
            {error, Reason}
    end.

-spec finish_project(binary(), odbc_organization:pro_type()) -> ok | {error, _}.
finish_project(LServer, Project) ->
    F = fun() ->
                {selected, [<<"time">>], [{Time}]} = ejabberd_odbc:sql_query_t(["select current_timestamp() as time;"]),
                Query = ["update project set status='0', end_at='", Time, "' where id=", Project],
                %% TOFIX:update task, event, data r-s.
                R = ejabberd_odbc:sql_query_t(Query),
                {R, Time}
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {{updated, _Counter}, Time}} ->
            {ok, Time};
        Reason ->
            {error, Reason}
    end.

-spec get_project(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_project(LServer, Project) ->
    Query = ["select id, name, description, photo, status, admin, start_at, end_at, job_tag, member_tag, link_tag from project where id='", Project, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>, <<"description">>, <<"photo">>, <<"status">>, <<"admin">>,
                    <<"start_at">>, <<"end_at">>, <<"job_tag">>, <<"member_tag">>, <<"link_tag">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec list_project(binary(), binary(), boolean()) -> {ok, list()} | {error, _}.
list_project(LServer, Jid, IsTemplate) ->
    Query = case IsTemplate of
                true ->
                    ["select id, name, description, photo, status, admin, start_at, end_at, job_tag, member_tag, link_tag from project where status='-1';"];
                _ ->
                    ["select id, name, description, photo, status, admin, start_at, end_at, job_tag, member_tag, link_tag from project where id in (
                    select project from organization_user where jid='", Jid, "');"]
            end,
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>, <<"description">>, <<"photo">>, <<"status">>, <<"admin">>,
                    <<"start_at">>, <<"end_at">>, <<"job_tag">>, <<"member_tag">>, <<"link_tag">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec set_photo(binary(), odbc_organization:pro_type(), binary()) -> ok | {error, _}.
set_photo(LServer, ProID, Photo) ->
    Query = ["update project set photo='", ejabberd_odbc:escape(Photo), "' where id='", ProID, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec get_photo(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_photo(LServer, ProID) ->
    Query = ["select photo from project where id='", ProID, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"photo">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec project_status(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
project_status(LServer, Project) ->
    Query = ["select status from project where id=", Project],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"status">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec project_name(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
project_name(LServer, Project) ->
    Query = ["select name from project where id=", Project],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"name">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec is_project_exist(binary(), odbc_organization:pro_type()) -> {ok, false} | {ok, true} | {error, _}.
is_project_exist(LServer, Project) ->
    Query = ["select id from project where id='", Project, "' and status <> '-1';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>], [] } ->
            {ok, false};
        {selected, [<<"id">>], _Rs } ->
            {ok, true};
        Reason ->
            {error, Reason}
    end.

-spec is_project_name_exist(binary(), binary()) -> {ok, true} | {ok, false} | {error, _}.
is_project_name_exist(LServer, ProjectName) ->
    Query = ["select id from project where name='", ejabberd_odbc:escape(ProjectName), "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>], [] } ->
            {ok, false};
        {selected, [<<"id">>], _Rs } ->
            {ok, true};
        Reason ->
            {error, Reason}
    end.

-spec subscribe(binary(), odbc_organization:pro_type(), odbc_organization:pro_type()) -> ok | {error, _}.
subscribe(LServer, ProSource, ProTarget) ->
    Query = ["insert into project_link(id1, id2) values('", ProSource, "', '", ProTarget, "');"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec subscribed(binary(), odbc_organization:pro_type(), odbc_organization:pro_type()) -> ok | {error, _}.
subscribed(LServer, ProSource, ProTarget) ->
    TimeString = now_random(),
    Query1 = ["insert into project_link(id1, id2) values('", ProSource, "', '", ProTarget, "');"],
    Query2 = ["update project set link_tag='", TimeString, "' where id in('", ProSource, "','", ProTarget, "');"],

    F = fun() ->
                case ejabberd_odbc:sql_query_t(Query1) of
                    {updated, 1} ->
                        ejabberd_odbc:sql_query_t(Query2),
                        {ok, TimeString};
                    Reason ->
                        Reason
                end
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic,  {ok, TimeString}} ->
            {ok, TimeString};
        Reason ->
            {error, Reason}
    end.

-spec unsubscribed(binary(), odbc_organization:pro_type(), odbc_organization:pro_type()) -> ok | {error, _}.
unsubscribed(LServer, ProSource, ProTarget) ->
    Query =["delete from project_link where id1='", ProSource, "' and id2='", ProTarget, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            ok;
        Reason ->
            {error, Reason}
    end.

%% @doc make sure source and target project is subscribed for each other.
-spec unsubscribe(binary(), odbc_organization:pro_type(), odbc_organization:pro_type()) -> ok | {error, _}.
unsubscribe(LServer, ProSource, ProTarget) ->
    TimeString = now_random(),
    Query1 = ["delete from project_link where id1='", ProSource, "' and id2='", ProTarget, "';"],
    Query2 = ["delete from project_link where id1='", ProTarget, "' and id2='", ProSource, "';"],
    Query3 = ["update project set link_tag='", TimeString, "' where id in('", ProSource, "','", ProTarget, "');"],

    F = fun() ->
                ejabberd_odbc:sql_query_t(Query1),
                ejabberd_odbc:sql_query_t(Query2),
                ejabberd_odbc:sql_query_t(Query3),
                ok
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, ok} ->
            {ok, TimeString};
        Reason ->
            {error, Reason}
    end.

-spec is_subscribe_exist(binary(), odbc_organization:pro_type(), odbc_organization:pro_type()) -> {ok, false} | {ok, true} | {error, _}.
is_subscribe_exist(LServer, ProSource, ProTarget) ->
    Query = ["select id1 from project_link where id1='", ProSource, "' and id2='", ProTarget, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id1">>], [] } ->
            {ok, false};
        {selected, [<<"id1">>], _Rs } ->
            {ok, true};
        Reason ->
            {error, Reason}
    end.

-spec search_project(binary(), binary()) -> {ok, list} | {error, _}.
search_project(LServer, ProjectName) ->
    Query = ["select id, name from project where name like '%", ProjectName, "%';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_admin(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_admin(LServer, Project) ->
    Query = ["select admin from project where id='", Project, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"admin">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.


%% =================================================================================
%% helper function
%% =================================================================================

-spec sql_list([string()]) -> ok.
sql_list(QueryList) ->
    lists:foreach(fun(X) ->
                          ejabberd_odbc:sql_query_t(X)
                  end, QueryList).

-spec plus(binary(), integer()) -> binary().
plus(I, P) ->
    integer_to_binary(binary_to_integer(I) + P).

-spec employees_to_records(list()) -> list().
employees_to_records(Result) ->
    [#employee{jid = Jid, organization_id = Id, organization_name = Name} || {Jid, Id, Name} <- Result].

now_random() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Ran = random:uniform(9999),
    list_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ@~4..0w",
                                 [Year, Month, Day, Hour, Minute, Second, Ran])).
