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
    add_employee/3,
    remove_employee/3,
    add_project/3
]).

-include("jlib.hrl").
-include("organization.hrl").

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
                add_project(<<"localhost">>, #project{name = <<"testproject">>, description = <<"">>}, <<"1">>)
        end,
    io:format("~p~n", [A]).

%% =====================================================================================================
%% api function
%% =====================================================================================================

-spec get_all(binary(), integer()) -> list().
get_all(LServer, Project) ->
    Query = ["select ou.jid, o.id, o.name from organization as o left join organization_user as ou ",
        "on o.id = ou.organization where project = ", Project, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>, <<"id">>, <<"name">>], Rs} ->
            {ok, employees_to_records(Rs)};
        Reason ->
            {error, Reason}
    end.

-spec get_parents(binary(), #node{}) -> {ok, _} | {error, _}.
get_parents(LServer, #node{lft = Left, rgt = Right, project = Project}) ->
    Query = ["select ou.jid, o.id, o.name from organization as o left join organization_user as ou ",
        "on o.id = ou.organization where o.lft > ", Left, " and o.rgt < ",
        Right, " and project = ", Project, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, [<<"jid">>, <<"id">>, <<"name">>], Rs} ->
            {ok, employees_to_records(Rs)};
        Reason ->
            {error, Reason}
    end.

-spec get_children(binary(), #node{}) -> {ok, _} | {error, _}.
get_children(LServer, #node{lft = Left, rgt = Right, project = Project}) ->
    Query = ["select ou.jid, o.id, o.name from organization as o left join organization_user as ou ",
        "on o.id = ou.organization where o.lft < ", Left, " and o.rgt > ",
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

-spec add_employee(binary(), integer(), binary()) -> ok | {error, _}.
add_employee(LServer, NodeId, Jid) ->
    Query = ["insert into organization_user(organization, jid) values(", NodeId, ",'", Jid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec remove_employee(binary(), integer(), binary()) -> ok | {error, _}.
remove_employee(LServer, NodeId, Jid) ->
    Query = ["delete from organization_user where organization = ", NodeId, " and jid = '", Jid, "';"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            {error, not_exists};
        Reason ->
            {error, Reason}
    end.

-spec add_project(binary(), #project{}, binary()) -> {ok, #project{}} | {error, _}.
add_project(LServer, #project{name = Name, description = Desc}, TemplateId) ->
    F = fun() ->
        Query1 = ["insert into project(name,description) values('",
            ejabberd_odbc:escape(Name), "','", ejabberd_odbc:escape(Desc), "');"],
        ejabberd_odbc:sql_query_t(Query1),
        {selected, [<<"id">>], [{Id}]} = ejabberd_odbc:sql_query_t(["select last_insert_id() as id;"]),
        Query2 = ["insert into organization(name,lft,rgt,depth,project) select name,lft,rgt,depth,",
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