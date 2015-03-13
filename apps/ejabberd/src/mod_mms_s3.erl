%%%-------------------------------------------------------------------
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(mod_mms_s3).

-export([config/0, bucket/0, start/0, get/2, upload/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-define(ENV(X), get_env(X)).

-define(BUCKET, bucket()).

-define(S3_CONFIG, config()).

start() ->
    erlcloud:start().

get_env(Key) ->
    case application:get_env(ejabberd, Key) of
        {ok, Val} -> Val;
        _ -> undefined
    end.

config() ->
    #aws_config{
        access_key_id = ?ENV(s3_key),
        secret_access_key = ?ENV(s3_secret),
        timeout = ?ENV(s3_timeout)
    }.

bucket() ->
    ?ENV(s3_bucket).

-spec get(binary(), integer()) -> binary().
get(Uid, Expire) ->
    try erlcloud_s3:make_link(Expire, ?BUCKET, binary_to_list(Uid), ?S3_CONFIG) of
        {_, H, P} -> H ++ P
    catch
        _:_ -> error
    end.

-spec upload(binary(), binary()) -> ok.
upload(Uid, Data) ->
    try erlcloud_s3:put_object(?BUCKET, binary_to_list(Uid), Data, ?S3_CONFIG) of
        _ -> ok
    catch
        _:_ -> error
    end.