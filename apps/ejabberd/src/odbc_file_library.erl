-module(odbc_file_library).

-export([
    allow_folder_view/3,
    allow_file_view/3,
    allow_add/3,
    allow_permission_edit/3,
    list_folders_add/3,
    list_folders_view/3,
    list_files/3,
    add_folder/2,
    add_root_folder/2,
    remove_folder/2,
    add_file/2,
    add_file_version/2,
    list_file_version/2,
    remove_file/2,
    add_permission/2,
    remove_permission/2
]).

-include("mod_file_library.hrl").

%% ==================================
%% permission
%% ==================================

%% @doc is user allowed to view this folder
-spec allow_folder_view(binary(), bint(), bint()) -> true | false | {error, _}.
allow_folder_view(LServer, FolderId, Jid) ->
    F = fun() ->
        Query0 = [<<"select count(id) from library_folder where id = ">>, FolderId, " and created_by = '", Jid, "';"],
        case ejabberd_odbc:sql_query_t(Query0) of
            {selected, _, [{<<"1">>}]} ->
                true;
            {selected, _, [{<<"0">>}]} ->
                Query1 = [<<"select o.lft,o.rgt from organization as o ">>,
                    <<"left join organization_user as u on u.organization = o.id where u.jid='">>, Jid, "';"],
                case ejabberd_odbc:sql_query_t(Query1) of
                    {selected, _, [{Left, Right}]} ->
                        Query2 = [<<"select count(id) from library_permission as p ">>,
                            <<"left join organization as o on o.id = p.organization ">>,
                            <<"where p.folder = ">>, FolderId, " and o.left >= ", Left, " and o.right <= ", Right, ";"],
                        case ejabberd_odbc:sql_query_t(Query2) of
                            {selected, _, [{<<"0">>}]} ->
                                false;
                            {selected, _, _} ->
                                true
                        end;
                    _ ->
                        false
                end
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, true} ->
            true;
        {atomic, false} ->
            false;
        Reason ->
            {error, Reason}
    end.

%% @doc is user allowed to view this file
-spec allow_file_view(binary(), bint(), bint()) -> true | false | {error, _}.
allow_file_view(LServer, FileId, Jid) ->
    F = fun() ->
        Query0 = [<<"select uploaded_by,folder from library_file where id = ">>, FileId, ";"],
        case ejabberd_odbc:sql_query_t(Query0) of
            {selected,_,[]} ->
                not_exists;
            {selected, _, [{Jid, _}]} ->
                true;
            {selected, _, [{_, FolderId}]} ->
                allow_folder_view(LServer, FolderId, Jid)
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, true} ->
            true;
        {atomic, false} ->
            false;
        Reason ->
            {error, Reason}
    end.

%% @doc is user allowed to add file or folder to this folder
-spec allow_add(binary(), bint(), bint()) -> true | false | {error, _}.
allow_add(LServer, FolderId, Jid) ->
    F = fun() ->
        Query0 = [<<"select count(id) from library_folder where id = ">>, FolderId, " and created_by = '", Jid, "';"],
        case ejabberd_odbc:sql_query_t(Query0) of
            {selected, _, [{<<"1">>}]} ->
                true;
            {selected, _, [{<<"0">>}]} ->
                Query1 = [<<"select o.lft,o.rgt from organization as o ">>,
                    <<"left join organization_user as u on u.organization = o.id where u.jid='">>, Jid, "';"],
                case ejabberd_odbc:sql_query_t(Query1) of
                    {selected, _, [{Left, Right}]} ->
                        Query2 = [<<"select count(id) from library_permission as p ">>,
                            <<"left join organization as o on o.id = p.organization ">>,
                            <<"where p.folder = ">>, FolderId, " and ((o.lft >= ", Left, " and o.rgt <= ", Right, ") ",
                            <<"or (o.left < ">>, Left, " and o.rgt > ", Right, "));"],
                        case ejabberd_odbc:sql_query_t(Query2) of
                            {selected, _, [{<<"0">>}]} ->
                                false;
                            {selected, _, _} ->
                                true
                        end;
                    _ ->
                        false
                end
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, true} ->
            true;
        {atomic, false} ->
            false;
        Reason ->
            {error, Reason}
    end.


%% @doc allow permission edit
allow_permission_edit(LServer, PermissionId, Jid) ->
    F = fun() ->
        case ejabberd_odbc:sql_query_t([<<"select folder from library_permission where id = ">>, PermissionId, ";"]) of
            {selected, _, [{Folder}]} ->
                allow_folder_view(LServer, Folder, Jid);
            _ ->
                not_exists
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, true} ->
            true;
        {atomic, false} ->
            false;
        {atomic, not_exists} ->
            {error, not_exists};
        Reason ->
            {error, Reason}
    end.

%% @doc add permission to folder
-spec add_permission(binary(), #permission{}) -> ok | {error, _}.
add_permission(LServer, Permission) ->
    F = fun() ->
        Query = [<<"insert into library_permission(folder,organization,created_by) values(">>,
            Permission#permission.folder, ",", Permission#permission.organization,
            ",'", Permission#permission.created_by, "');"],
        {updated, 1} = ejabberd_odbc:sql_query_t(Query),
        ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>])
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, [{Id}]}} ->
            {ok, Permission#permission{id = Id}};
        Reason ->
            {error, Reason}
    end.

%% @doc remove permission from folder
-spec remove_permission(binary(), bint()) -> ok | {error, _}.
remove_permission(LServer, PermissionId) ->
    Query = [<<"delete from library_permission where id = ">>, PermissionId, ";"],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            {error, not_exists};
        Reason ->
            {error, Reason}
    end.


%% ==================================
%% folder
%% ==================================

%% @doc list folders that user has permission to view
-spec list_folders_view(binary(), bint(), binary()) -> {ok, [#folder{}]} | {error, _}.
list_folders_view(LServer, Project, Jid) ->
    F = fun() ->
        Query1 = [<<"select o.lft,o.rgt from organization as o ">>,
            <<"left join organization_user as u on u.organization = o.id where u.jid='">>, Jid, "';"],
        case ejabberd_odbc:sql_query_t(Query1) of
            {selected, _, [{Left, Right}]} ->
                Query2 = [<<"select f.id,f.name,f.parent,f.created_by,UNIX_TIMESTAMP(f.created_at) ">>,
                    <<"from library_folder as f ">>,
                    <<"left join library_permission as p on p.folder = f.id ">>,
                    <<"left join organization as o on o.id = p.organization ">>,
                    <<"where f.project = ">>, Project, " and ((o.lft >= ", Left, " and o.rgt <= ", Right, ") ",
                    "or f.created_by = '", Jid, "');"],
                ejabberd_odbc:sql_query_t(Query2);
            _ ->
                not_exists
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, R}} ->
            {ok, [#folder{id = Id, name = Name, parent = Parent, created_by = CreatedBy, created_at = CreatedAt}
                || {Id, Name, Parent, CreatedBy, CreatedAt} <- R]};
        {atomic, not_exists} ->
            {ok, []};
        Reason ->
            {error, Reason}
    end.

%% @doc list folders that user has permission to add file or folder
-spec list_folders_add(binary(), bint(), binary()) -> {ok, [#folder{}]} | {error, _}.
list_folders_add(LServer, Project, Jid) ->
    F = fun() ->
        Query1 = [<<"select o.lft,o.rgt from organization as o ">>,
            <<"left join organization_user as u on u.organization = o.id where u.jid='">>, Jid, "';"],
        case ejabberd_odbc:sql_query_t(Query1) of
            {selected, _, [{Left, Right}]} ->
                Query2 = [<<"select f.id,f.name,f.parent,f.created_by,UNIX_TIMESTAMP(f.created_at) ">>,
                    <<"from library_folder as f ">>,
                    <<"left join library_permission as p on p.folder = f.id ">>,
                    <<"left join organization as o on o.id = p.organization ">>,
                    <<"where f.project = ">>, Project, " and ((o.lft >= ", Left, " and o.rgt <= ", Right,
                    ") or (o.lft <", Left, " and o.rgt > ", Right,
                    ") or f.created_by = '", Jid, "');"],
                ejabberd_odbc:sql_query_t(Query2);
            _ ->
                not_exists
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, R}} ->
            {ok, [#folder{id = Id, name = Name, parent = Parent, created_by = CreatedBy, created_at = CreatedAt}
                || {Id, Name, Parent, CreatedBy, CreatedAt} <- R]};
        {atomic, not_exists} ->
            {ok, []};
        Reason ->
            {error, Reason}
    end.


%% @doc add folder to parent folder
-spec add_folder(binary(), #folder{}) -> {ok, #folder{}} | {error, _}.
add_folder(LServer, Folder) ->
    F = fun() ->
        Query0 = [<<"select count(id) from library_folder where parent = ">>, Folder#folder.parent,
            " and name ='", ejabberd_odbc:escape(Folder#folder.name), "';"],
        case ejabberd_odbc:sql_query_t(Query0) of
            {selected, _, [{<<"0">>}]} ->
                Query = [<<"insert into library_folder(parent,name,description,project,created_by) select ">>,
                    Folder#folder.parent, ",'", Folder#folder.name, "','", Folder#folder.description, "', project,'",
                    Folder#folder.created_by, <<"' from  library_folder where id = ">>, Folder#folder.parent, ";"],
                {updated, 1} = ejabberd_odbc:sql_query_t(Query),
                ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]);
            _ ->
                duplicate_name
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, [{Id}]}} ->
            {ok, Folder#folder{id = Id}};
        {atomic, duplicate_name} ->
            {error, duplicate_name};
        Reason ->
            {error, Reason}
    end.

-spec add_root_folder(binary(), #folder{}) -> {ok, #folder{}} | {error, _}.
add_root_folder(LServer, Folder) ->
    F = fun() ->
        Query0 = [<<"select count(id) from library_folder where project = ">>,
            Folder#folder.project, " and parent IS NULL and name ='", ejabberd_odbc:escape(Folder#folder.name), "';"],
        case ejabberd_odbc:sql_query_t(Query0) of
            {selected, _, [{<<"0">>}]} ->
                Query = [<<"insert into library_folder(name,description,project,created_by) values('">>,
                    ejabberd_odbc:escape(Folder#folder.name), "','",
                    ejabberd_odbc:escape(Folder#folder.description), "', ",
                    Folder#folder.project, ",'", Folder#folder.created_by, "');"],
                {updated, 1} = ejabberd_odbc:sql_query_t(Query),
                ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>]);
            _ ->
                duplicate_name
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, [{Id}]}} ->
            {ok, Folder#folder{id = Id}};
        {atomic, duplicate_name} ->
            {error, duplicate_name};
        Reason ->
            {error, Reason}
    end.

%% @doc remove a folder
-spec remove_folder(binary(), bint()) -> ok | {error, _}.
remove_folder(LServer, FolderId) ->
    F = fun() ->
        Query = [<<"select count(id) from library_folder where parent = ">>, FolderId, ";"],
        case ejabberd_odbc:sql_query_t(Query) of
            {selected, _, {[<<"0">>]}} ->
                ejabberd_odbc:sql_query_t([<<"delete from library_folder where id = ">>, FolderId, ";"]);
            _ ->
                not_valid
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, not_valid} ->
            {error, not_valid};
        {atomic, {updated, 1}} ->
            ok;
        {atomic, {updated, 0}} ->
            {error, not_exists};
        Reason ->
            {error, Reason}
    end.


%% ==================================
%% file
%% ==================================

%% @doc list files in folder that use have permission to view
-spec list_files(binary(), bint(), binary()) -> {ok, [#file{}]} | {error, _}.
list_files(LServer, FolderId, Jid) ->
    Query = [<<"select id,name,uid,version,uploaded_by,UNIX_TIMESTAMP(uploaded_at) from library_file ">>,
        <<"where folder = ">>, FolderId, case allow_folder_view(LServer, FolderId, Jid) of
                                             true ->
                                                 ";";
                                             false ->
                                                 [" and uploaded_by = '", Jid, ";"]
                                         end],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {selected, _, R} ->
            {ok, [#file{id = Id, name = Name, uid = Uid, version = Version,
                uploaded_by = UploadedBy, uploaded_at = UploadedAt}
                || {Id, Name, Uid, Version, UploadedBy, UploadedAt} <- R]};
        Reason ->
            {error, Reason}
    end.

%% @doc add file to folder
-spec add_file(binary(), #file{}) -> ok | {error, _}.
add_file(LServer, File) ->
    F = fun() ->
        Query = [<<"insert into library_file(uid,folder,name,uploaded_by) values('">>, File#file.uid, "',",
            File#file.folder, ",'", File#file.name, "','", File#file.uploaded_by, "');"],
        {updated, 1} = ejabberd_odbc:sql_query_t(Query),
        ejabberd_odbc:sql_query_t([<<"select last_insert_id();">>])
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, [{Id}]}} ->
            {ok, File#file{id = Id}};
        Reason ->
            {error, Reason}
    end.

%% @doc add a new version of file
-spec add_file_version(binary(), #file{}) -> ok | {error, _}.
add_file_version(LServer, File) ->
    F = fun() ->
        Query0 = [<<"insert into library_file_version(file,folder,uid,name,version,uploaded_by,uploaded_at) ">>,
            <<"select id,folder,uid,name,version,uploaded_by,uploaded_at from library_file where id = ">>,
            File#file.id, ";"],
        case ejabberd_odbc:sql_query_t(Query0) of
            {updated, 1} ->
                Query1 = [<<"update library_file set uid = '">>, File#file.uid, "',name='", File#file.name,
                    <<"',uploaded_by='">>, File#file.uploaded_by,
                    <<"',version = version + 1, uploaded_at = NOW() where id = ">>, File#file.id, ";"],
                case ejabberd_odbc:sql_query_t(Query1) of
                    {updated, 1} ->
                        ejabberd_odbc:sql_query_t([<<"select folder,version,uploaded_at ">>,
                            <<"from library_file where id = ">>, File#file.id, ";"]);
                    _ ->
                        {error, not_exists}
                end;
            _ ->
                {error, not_exists}
        end
    end,
    case ejabberd_odbc:sql_transaction(LServer, F) of
        {atomic, {selected, _, [{Folder, Version, UploadedAt}]}} ->
            {ok, File#file{folder = Folder, version = Version, uploaded_at = UploadedAt}};
        {atomic, {error, Reason}} ->
            {error, Reason}
    end.

%% @doc list file version
-spec list_file_version(binary(), bint()) -> {ok, [#file{}]} | {error, _}.
list_file_version(LServer, FileId) ->
    case ejabberd_odbc:sql_query(LServer, [<<"select uid,folder,name,version,uploaded_by,UNIX_TIMESTAMP(uploaded_at)">>,
        <<" from library_file_version where file = ">>, FileId, ";"]) of
        {selected, _, R} ->
            {ok, [#file{id = FileId, uid = Uid, folder = Folder, name = Name, version = Version,
                uploaded_by = UploadedBy, uploaded_at = UploadedAt}
                || {Uid, Folder, Name, Version, UploadedBy, UploadedAt} <- R]};
        Reason ->
            {error, Reason}
    end.

%% @doc remove file
-spec remove_file(binary(), bint()) -> ok | {error, _}.
remove_file(LServer, FileId) ->
    Query = [<<"delete from library_file where id = '">>, FileId],
    case ejabberd_odbc:sql_query(LServer, Query) of
        {updated, 1} ->
            ok;
        {updated, 0} ->
            {error, not_exists};
        Reason ->
            {error, Reason}
    end.
