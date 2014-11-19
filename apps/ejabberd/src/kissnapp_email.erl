-module(kissnapp_email).

-include("ejabberd.hrl").

%% API.
-export([send_validation_email/3, send_email/3]).

-export([start_link/3]).

%% TOFIX: Send many time to one address will be junk email, add a random string append Subject???
-spec send_validation_email(Subject :: binary,
                            ToAddress :: binary,
                            Active :: binary()) -> supervisor:startchild_ret().
send_validation_email(Subject, ToAdress, ActiveLink) ->
    supervisor:start_child(kissnapp_email_sup, [Subject, ToAdress, ActiveLink]).


start_link(Subject, ToAddress, ActiveLink) ->
    Pid = spawn_link(?MODULE, send_email, [Subject, ToAddress, ActiveLink]),
    {ok, Pid}.

get_html_file_content(ActiveLink) ->
    case code:lib_dir(ejabberd) of
        {error, bad_name} ->
            {error, notexist};
        Path ->
            FilePath = Path ++ "/priv/validation.html",
            case file:read_file(FilePath) of
                {ok, Binary} ->
                    binary:replace(Binary, <<"replace me">>, ActiveLink);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec send_email(Subject :: binary(),
                 ToAddress :: binary(),
                 ActiveLink :: binary()) -> ok.
send_email(Subject, ToAddress, ActiveLink) ->
    HtmlFileContent = get_html_file_content(ActiveLink),
    case HtmlFileContent of
        {error, notexist} ->
            ?ERROR_MSG("Send Email failed, Reason='can't find html file path',[info: ToAddress=~p, ActiveLink=~p]",
                       [ToAddress, ActiveLink]);
        {error, Reason} ->
            ?ERROR_MSG("Send Email failed, Reason='read html file failed:~p',[info: ToAddress=~p, ActiveLink=~p]",
                       [Reason, ToAddress, ActiveLink]);
        _ ->
            inets:start(),
            ssl:start(),
            Method = post,
            URL = "https://api:key-c3fa23e26ecd8d5f61c0397932e29fbc@api.mailgun.net/v2/yoolosoft.com/messages",
            Header = [],
            Type = "application/x-www-form-urlencoded",
            Body = <<"from=postmaster@yoolosoft.com", "&to=", ToAddress/binary,
                     "&subject=", Subject/binary, "&html=", HtmlFileContent/binary>>,
            HTTPOptions = [],
            Options = [],
            Result = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
            case Result of
                {error, Reason2} ->
                    ?ERROR_MSG("Send Email failed, Reason='httpc:request failed:~p',[info: ToAddress=~p, ActiveLink=~p]",
                               [Reason2, ToAddress, ActiveLink]);
                _ -> ok
            end
    end.