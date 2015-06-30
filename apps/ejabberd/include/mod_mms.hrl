%%%-------------------------------------------------------------------
%%% @doc mms module
%%%
%%%-------------------------------------------------------------------

%% is private
-define(PUBLIC, <<"0">>).
-define(PRIVATE, <<"1">>).

-define(ENV(X), mod_mms_s3:get_env(X)).
-define(S3_CONFIG, mod_mms_s3:config()).
-define(MMS_SECRET, mod_mms_s3:secret()).

%% ==================
%% mms records
%% ==================

-record(mms_file, {
    filename :: binary(),
    owner :: binary(),
    uid :: binary(),
    private :: public | private,
    created_at :: integer()
}).