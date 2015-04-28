-module(odbc_organization).

%% API
-export([
    test/1,
    get_all/2,
    get_parents/2,
    get_children/2,
    delete_node/2,
    update_node/3,
    add_node/3,
    get_all_nodes/2,
    add_employees/4,
    delete_employee/3,
    add_employee/3,
    remove_employee/3,
    list_template/2,
    add_project/3,
    finish_project/2,
    list_project/2,
    project_name/2,
    is_project_name_exist/2,
    get_admin/2,
    get_all_jid/2,
    get_parent_jids/3,
    get_children_job/3,
    get_job/3,
    is_memeber/3
]).

-include("jlib.hrl").
-include("organization.hrl").

-type pro_type() :: binary() | integer().

test(C) ->
    A = case C of
            1 ->
                get_all(<<"localhost">>, <<"1">>);
            2 ->
                get_parents(<<"localhost">>, #node{lft = <<"2">>, rgt = <<"7">>, project = <<"1">>});
            3 ->
                get_children(<<"localhost">>, #node{lft = <<"2">>, rgt = <<"7">>, project = <<"1">>});
            4 ->
                delete_node(<<"localhost">>, <<"2">>);
            5 ->
                delete_node(<<"localhost">>, <<"5">>);
            6 ->
                add_node(<<"localhost">>, <<"2">>, #node{name = <<"testadd">>, description = <<"adfadf">>, project = <<"1">>});
            7 ->
                add_project(<<"localhost">>, #project{name = <<"testproject">>, description = <<"">>, admin = <<"jid@test">>}, <<"0">>)
        end,
    io:format("~p~n", [A]).

%% =====================================================================================================
%% api function
%% =====================================================================================================

-spec get_all(binary(), integer()) -> {ok, list()} | {error, _}.
get_all(LServer, Project) ->
    Query = ["select ou.jid, o.id, o.name, o.description from organization as o left join organization_user as ou ",
        "on o.id = ou.organization where ou.project = ", Project, " ;"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>, <<"id">>, <<"name">>, <<"description">>], Rs} ->
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

-spec get_parent_jids(binary(), binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_parent_jids(LServer, Jid, Project)->
    Query = ["select lft, rgt from organization where id=(select organization from organization_user where project=",
            Project, " and  jid='", Jid, "');" ],
    case ejabberd_odbc:sql_query(LServer, Query) of
    {selected, [<<"lft">>, <<"rgt">>], []} ->
        {error, nost_exist};
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

-spec get_all_nodes(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_all_nodes(LServer, Project) ->
    Query = ["select id, name from organization where project = '", Project, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>], Rs } ->
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

-spec get_job(binary(), binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
get_job(LServer, Jid, Project) ->
    Query = ["select organization from organization_user where id ='", Project, "' and jid = '", Jid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"organization">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec get_children_job(binary(), binary(), odbc_organization:pro_type()) -> {ok, _} | {error, _}.
get_children_job(LServer, Id, Project) ->
    case ejabberd_odbc:sql_query( LServer, ["select lft, rgt from organization where id='", Id, "';"] ) of
        {selected, [<<"lft">>, <<"rgt">>], [{Left}, {Right}]} ->
            case ejabberd_odbc:sql_query( LServer, ["select id, name from organization where project='", Project, "' and lft>", Left, " and rgt<", Right, ";" ] ) of
                {selected, [<<"id">>, <<"name">>], Rs} ->
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
add_node(LServer, ParentId, #node{name = Name, description = Description} = _Node) ->
    F = fun() ->
        {selected, [<<"lft">>, <<"rgt">>, <<"depth">>, <<"project">>], [{Left, Right, Depth, Project}]} =
            ejabberd_odbc:sql_query_t(["select lft,rgt,depth,project from organization where id=", ParentId, ";"]),
        {Left1, Right1, Depth1} = {plus(Left, 1), plus(Left, 2), plus(Depth, 1)},
        Query = [
            ["update organization set lft=lft+2 where lft > ", Left, " and project = ", Project, ";"],
            ["update organization set rgt=rgt+2 where rgt > ", Left, " and project = ", Project, ";"],
            ["insert into organization(name,lft,rgt,depth,description,project) values('",
                ejabberd_odbc:escape(Name), "',", Left1, ",", Right1, ",", Depth1, ",'",
                ejabberd_odbc:escape(Description), "',", Project, ");"]
        ],
        sql_list(Query),
        {selected, [<<"id">>], [{Id}]} = ejabberd_odbc:sql_query_t(["select last_insert_id() as id;"]),
        {ok, #node{id = Id, lft = Left1, rgt = Right1, depth = Depth1, name = Name, description = Description}}
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {ok, Rs}} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec add_employees(binary(), binary(), binary(), list()) -> ok | {error, _}.
add_employees(LServer, Project, Adder, List) ->
    %% TOFIX: need split List to predefine job and customize job, insert predefine first, then insert customize.
    %% assume all is predefine job.
    Init = <<"insert into organization_user(organization, jid, project) values">>,
    Query0 = lists:foldl( fun(E, AccIn) ->
                    {Id, _, Jid} = E,
                    AccIn1 = if AccIn =:= Init ->
                                    Init;
                                true ->
                                    <<AccIn/binary, ",">>
                             end,
                    <<AccIn1/binary, "(", Id/binary, ", '", Jid/binary, "', ", Project/binary, ")">>
                 end,
                 Init,
                 List ),

    Query = <<Query0/binary, ";">>,
    Size = lists:flatlength( List ),
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, Size} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec delete_employee(binary(), odbc_organization:pro_type(), binary()) -> ok | {error, no_exist} | {error, _}.
delete_employee(LServer, Project, Jid) ->
    Query = ["delete from organization_user where project=", Project, " and jid='", Jid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
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

-spec list_template(binary(), binary()) -> {ok, list()} | {error, _}.
list_template(LServer, AdminJid) ->
    %% TOFIX:select project_id and project_name from admin save organization table.
    %% TOFIX:default organization project is 0.
    {ok, [#template{id = <<"0">>, name = <<"default">>}]}.

-spec add_project(binary(), #project{}, binary()) -> {ok, #project{}} | {error, _}.
add_project(LServer, #project{name = Name, description = Desc, admin = Admin}, TemplateId) ->
    F = fun() ->
        Query1 = ["insert into project(name,description,admin) values('",
            ejabberd_odbc:escape(Name), "','", ejabberd_odbc:escape(Desc), "','", ejabberd_odbc:escape(Admin), "');"],
        ejabberd_odbc:sql_query_t(Query1),
        {selected, [<<"id">>], [{Id}]} = ejabberd_odbc:sql_query_t(["select last_insert_id() as id;"]),
        Query2 = ["insert into organization(name,lft,rgt,depth,description,project) select name,lft,rgt,depth,description,",
            Id, " from organization where project =", TemplateId, ";"],
        ejabberd_odbc:sql_query_t(Query2),
        Id
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, Id} ->
            {ok, #project{id = Id, name = Name, description = Desc}};
        Reason ->
            {error, Reason}
    end.

-spec finish_project(binary(), odbc_organization:pro_type()) -> ok | {error, _}.
finish_project(LServer, Project) ->
    F = fun() ->
        Query1 = ["update project set status='0' where id=", Project],
        %% update task, event, data r-s.
        ejabberd_odbc:sql_query_t(Query1)
        end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {updated, _Counter}} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec list_project(binary(), binary()) -> {ok, list()} | {error, _}.
list_project(LServer, Jid) ->
    Query = ["select id, name from project where id=(select project from organization_user where jid='", Jid, "');"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"id">>, <<"name">>], Rs} ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec project_name(binary(), odbc_organization:pro_type()) -> {ok, list()} | {error, _}.
project_name(LServer, Project) ->
    Query = ["select name from project where id=", Project],
    case ejabberd_odbc:sql_query(LServer, Query) of
        [selected, [<<"name">>], Rs] ->
            {ok, Rs};
        Reason ->
            {error, Reason}
    end.

-spec is_project_name_exist(binary(), binary()) -> {ok, true} | {ok, false} | {error, _}.
is_project_name_exist(LServer, ProjectName) ->
    Query = ["select id from project where name='", ejabberd_odbc:escape(ProjectName), "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        [selected, [<<"id">>], [] ] ->
            {ok, false};
        {selected, [<<"id">>], _Rs } ->
            {ok, true};
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