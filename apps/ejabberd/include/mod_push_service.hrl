%% ============================================
%% for mod_push_service
%% ============================================

-define(ODBC_SERVER, mod_push_service:get_host_server()).

-record(push_token, {jid :: binary(),
    token :: binary(),
    type :: integer()
}).

-record(push_message, {type :: integer(),
    content :: binary(),
    from :: binary(),
    to :: binary()
}).