%% ============================
%% @doc file library module
%% doc at https://github.com/ZekeLu/MongooseIM/wiki/Extending-XMPP#file-library
%% ============================

-module(mod_file_library).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% IQ handlers
-export([process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_file_library.hrl").

-define(NS_LIBRARY, <<"aft:library">>).

%%%===================================================================
%%% gen_mod callbacks
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
        ?NS_LIBRARY, ?MODULE, process_iq, no_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_LIBRARY).


%%%===================================================================
%%% IQ handlers
%%%===================================================================

process_iq(From, To, #iq{xmlns = ?NS_LIBRARY, type = _Type, sub_el = SubEl} = IQ) ->
    case xml:get_tag_attr_s(<<"query_type">>, SubEl) of
        <<"add_folder">> ->
            add_folder(From, To, IQ);
        <<"add_root_folder">> ->
            add_root_folder(From, To, IQ);
        <<"add_file">> ->
            add_file(From, To, IQ);
        <<"add_file_version">> ->
            add_file_version(From, To, IQ);
        <<"list_folders_view">> ->
            list_folders_view(From, To, IQ);
        <<"list_folders_add">> ->
            list_folders_add(From, To, IQ);
        <<"list_files">> ->
            list_files(From, To, IQ);
        <<"list_file_version">> ->
            list_file_version(From, To, IQ);
        <<"add_permission">> ->
            add_permission(From, To, IQ);
        <<"remove_permission">> ->
            remove_permission(From, To, IQ);
        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}
    end;
process_iq(_, _, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_BAD_REQUEST]}.


%% ===================================================================
%% Internal functions
%% ===================================================================

add_folder(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, folder, record_info(fields, folder)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        Folder ->
            case lists:member(undefined, [Folder#folder.name, Folder#folder.parent, Folder#folder.description]) of
                true ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                false ->
                    Jid = make_jid(LServer, LUser),
                    case odbc_file_library:allow_folder_view(LServer, Folder#folder.parent, Jid) of
                        true ->
                            case odbc_file_library:add_folder(LServer, Folder#folder{created_by = Jid}) of
                                {ok, F} ->
                                    IQ#iq{type = result, sub_el = SubEl#xmlel{
                                        children = [record_to_xml(folder, F, record_info(fields, folder))]
                                    }};
                                {error, duplicate_name} ->
                                    make_error_reply(IQ, ?DUPLICATE_FOLDER_NAME);
                                {error, _} ->
                                    make_error_reply(IQ, ?OBJECT_NOT_EXISTS)

                            end;
                        false ->
                            make_error_reply(IQ, ?FOLDER_READ_DENY)
                    end
            end
    end.

add_root_folder(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, folder, record_info(fields, folder)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        Folder ->
            case lists:member(undefined, [Folder#folder.name, Folder#folder.project, Folder#folder.description]) of
                true ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                false ->
                    Jid = make_jid(LServer, LUser),
                    case odbc_file_library:add_root_folder(LServer, Folder#folder{created_by = Jid}) of
                        {ok, F} ->
                            IQ#iq{type = result, sub_el = SubEl#xmlel{
                                children = [record_to_xml(folder, F, record_info(fields, folder))]
                            }};
                        {error, duplicate_name} ->
                            make_error_reply(IQ, ?DUPLICATE_FOLDER_NAME);
                        {error, _} ->
                            make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                    end
            end
    end.

add_file(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, file, record_info(fields, file)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        File ->
            case lists:member(undefined, [File#file.name, File#file.folder, File#file.uid]) of
                true ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                false ->
                    Jid = make_jid(LServer, LUser),
                    case odbc_file_library:allow_folder_view(LServer, File#file.folder, Jid) of
                        true ->
                            case odbc_file_library:add_file(LServer, File#file{uploaded_by = Jid}) of
                                {ok, F} ->
                                    IQ#iq{type = result, sub_el = SubEl#xmlel{
                                        children = [record_to_xml(file, F, record_info(fields, file))]
                                    }};
                                {error, _} ->
                                    make_error_reply(IQ, ?OBJECT_NOT_EXISTS)

                            end;
                        false ->
                            make_error_reply(IQ, ?FOLDER_WRITE_DENY)
                    end
            end
    end.

add_file_version(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, file, record_info(fields, file)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        File ->
            case lists:member(undefined, [File#file.name, File#file.id, File#file.uid]) of
                true ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                false ->
                    Jid = make_jid(LServer, LUser),
                    case odbc_file_library:allow_file_view(LServer, File#file.id, Jid) of
                        true ->
                            case odbc_file_library:add_file_version(LServer, File#file{uploaded_by = Jid}) of
                                {ok, F} ->
                                    IQ#iq{type = result, sub_el = SubEl#xmlel{
                                        children = [record_to_xml(file, F, record_info(fields, file))]
                                    }};
                                {error, _} ->
                                    make_error_reply(IQ, ?DUPLICATE_FOLDER_NAME)

                            end;
                        false ->
                            make_error_reply(IQ, ?FOLDER_WRITE_DENY)
                    end
            end
    end.

list_folders_view(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, folder, record_info(fields, folder)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        Folder ->
            case Folder#folder.project of
                undefined ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                P ->
                    case odbc_file_library:list_folders_view(LServer, P, make_jid(LServer, LUser)) of
                        {ok, R} ->
                            IQ#iq{type = result, sub_el = SubEl#xmlel{
                                children = [record_to_xml(folder, F, record_info(fields, folder)) || F <- R]
                            }};
                        {error, _} ->
                            make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                    end
            end
    end.

list_folders_add(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, folder, record_info(fields, folder)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        Folder ->
            case Folder#folder.project of
                undefined ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                P ->
                    case odbc_file_library:list_folders_add(LServer, P, make_jid(LServer, LUser)) of
                        {ok, R} ->
                            IQ#iq{type = result, sub_el = SubEl#xmlel{
                                children = [record_to_xml(folder, F, record_info(fields, folder)) || F <- R]
                            }};
                        {error, _} ->
                            make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                    end
            end
    end.

list_files(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, file, record_info(fields, file)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        File ->
            case File#file.folder of
                undefined ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                T ->
                    case odbc_file_library:list_files(LServer, T, make_jid(LServer, LUser)) of
                        {ok, R} ->
                            IQ#iq{type = result, sub_el = SubEl#xmlel{
                                children = [record_to_xml(file, F, record_info(fields, file)) || F <- R]
                            }};
                        {error, _} ->
                            make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                    end
            end
    end.

list_file_version(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, file, record_info(fields, file)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        File ->
            case File#file.id of
                undefined ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                T ->
                    case odbc_file_library:allow_file_view(LServer, T, make_jid(LServer, LUser)) of
                        true ->
                            case odbc_file_library:list_file_version(LServer, T) of
                                {ok, R} ->
                                    IQ#iq{type = result, sub_el = SubEl#xmlel{
                                        children = [record_to_xml(file, F, record_info(fields, file)) || F <- R]
                                    }};
                                _ ->
                                    make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                            end;
                        false ->
                            make_error_reply(IQ, ?FOLDER_READ_DENY);
                        {error, _} ->
                            make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                    end
            end
    end.

add_permission(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, permission, record_info(fields, permission)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        Permission ->
            case lists:member(undefined, [Permission#permission.folder, Permission#permission.organization]) of
                true ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                false ->
                    Jid = make_jid(LServer, LUser),
                    case odbc_file_library:allow_folder_view(LServer, Permission#permission.folder, Jid) of
                        true ->
                            case odbc_file_library:add_permission(LServer, Permission#permission{created_by = Jid}) of
                                {ok, P} ->
                                    IQ#iq{type = result, sub_el = SubEl#xmlel{
                                        children = [record_to_xml(permission, P, record_info(fields, permission))]}};
                                {error, _} ->
                                    make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                            end;
                        false ->
                            make_error_reply(IQ, ?FOLDER_READ_DENY);
                        {error, _} ->
                            make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                    end
            end
    end.

remove_permission(#jid{lserver = LServer, luser = LUser}, _To, #iq{sub_el = SubEl} = IQ) ->
    case xml_to_record(SubEl, permission, record_info(fields, permission)) of
        undefined ->
            make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
        Permission ->
            case Permission#permission.id of
                undefined ->
                    make_error_reply(IQ, ?REQUEST_PARAM_ERROR);
                Pid ->
                    Jid = make_jid(LServer, LUser),
                    case odbc_file_library:allow_folder_view(LServer, Permission#permission.folder, Jid) of
                        true ->
                            case odbc_file_library:remove_permission(LServer, Pid) of
                                ok ->
                                    IQ#iq{type = result};
                                {error, _} ->
                                    make_error_reply(IQ, ?OBJECT_NOT_EXISTS)
                            end;
                        false ->
                            make_error_reply(IQ, ?FOLDER_READ_DENY)
                    end
            end
    end.


%% ==============================================
%% helper
%% ==============================================

-spec make_jid(binary(), binary()) -> binary().
make_jid(LServer, LUser) ->
    <<LUser/binary, $@, LServer/binary>>.

-spec make_error_reply(#iq{}, binary()) -> #iq{}.
make_error_reply(#iq{sub_el = SubEl} = IQ, Code) ->
    IQ#iq{type = error, sub_el = [SubEl, #xmlel{name = <<"error">>, attrs = [{<<"code">>, Code}]}]}.

xml_to_record(SubEl, Record, RecordInfo) ->
    Tag = atom_to_binary(Record, utf8),
    case xml:get_subtag(SubEl, Tag) of
        false ->
            undefined;
        TagEl ->
            list_to_tuple([Record | lists:map(fun(X) ->
                T = atom_to_binary(X, utf8),
                case xml:get_tag_attr_s(T, TagEl) of
                    <<>> -> undefined;
                    R -> R
                end
            end, RecordInfo)])
    end.

record_to_xml(Name, Record, RecordInfo) ->
    [_ | L] = tuple_to_list(Record),
    #xmlel{name = atom_to_binary(Name, utf8), attrs = lists:filter(fun(X) ->
        case X of
            {_, undefined} -> false;
            {_, null} -> false;
            _ -> true
        end
    end,
        lists:zipwith(fun(X, Y) ->
            {atom_to_binary(X, utf8), Y}
        end, RecordInfo, L)
    )}.
