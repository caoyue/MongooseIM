
-module( mod_verify ).
-behaviour( gen_mod ).

-export( [start/2,
          stop/1,
          init/3,
          terminate/3,
          handle/2] ).

-define( SUCCESS_PAGE,
  "
<!DOCTYPE html>
<html>
<head>
    <title>Kissnapp Register Success!</title>
    <style type=\"text/css\">
        body{
        font-family: \"Open Sans\",\"lucida grande\",\"Segoe UI\",arial,verdana,\"lucida sans unicode\",tahoma,sans-serif;
        background-color:#C6E9EF;
        text-align:center;
        }
        .button {
        display: inline-block;
        position: relative;
        margin: 10px;
        padding: 0 20px;
        text-align: center;
        text-decoration: none;
        font: bold 12px/25px Arial, sans-serif;

        text-shadow: 1px 1px 1px rgba(255,255,255, .22);

        -webkit-border-radius: 30px;
        -moz-border-radius: 30px;
        border-radius: 30px;

        -webkit-box-shadow: 1px 1px 1px rgba(0,0,0, .29), inset 1px 1px 1px rgba(255,255,255, .44);
        -moz-box-shadow: 1px 1px 1px rgba(0,0,0, .29), inset 1px 1px 1px rgba(255,255,255, .44);
        box-shadow: 1px 1px 1px rgba(0,0,0, .29), inset 1px 1px 1px rgba(255,255,255, .44);

        -webkit-transition: all 0.15s ease;
        -moz-transition: all 0.15s ease;
        -o-transition: all 0.15s ease;
        -ms-transition: all 0.15s ease;
        transition: all 0.15s ease;
        }
        .blue {
        color: #F1F6FC;

        background: #3096D3; /* Old browsers */
        background: -moz-linear-gradient(top,  #3096D3 0%, #39a0be 100%); /* FF3.6+ */
        background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#3096D3), color-stop(100%,#39a0be)); /* Chrome,Safari4+ */
        background: -webkit-linear-gradient(top,  #3096D3 0%,#39a0be 100%); /* Chrome10+,Safari5.1+ */
        background: -o-linear-gradient(top,  #3096D3 0%,#39a0be 100%); /* Opera 11.10+ */
        background: -ms-linear-gradient(top,  #3096D3 0%,#39a0be 100%); /* IE10+ */
        background: linear-gradient(top,  #3096D3 0%,#39a0be 100%); /* W3C */
        }
        .big {
        padding: 0 33px;
        padding-top: 10px;
        height: 66px;
        text-transform: uppercase;
        font: bold 50px/60px Arial, sans-serif;
        }
    </style>
</head>
<body>
<h1 style=\"color:#5E6261;font-size:80px;margin-top:80px\">SUCCESS!</h1>
    <p style=\"padding:0px\"><img style=\"width:200px;height:200px\" src=\"http://54.255.140.120:8080/ic_launcher.png\" />
    <h6 style=\"color:#65696B;margin:0px;font-size:30px\">&nbsp;KISSNAPP</h6>
    </p>
<p style=\"color:#95A6AB;font-size:40px\">
    Your account is confirmed.<br>
    Welcome to KISSNAPP.
</p>
<a href=\"#\" class=\"button big blue\">LAUNCH APP</span></a>
</body>
</html>" ).

-define( PAGE_FRONT, <<"<html><head></head><body bgcolor=\"#C6E9E\"><font style ='color:#FF0000; font-size:60 px'><b>">> ).
-define( PAGE_END, <<"</b></body></html>">>).

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

init(_Transport, Req, Opts) ->
    {ok, Req, Opts}.

is_register_activation( Req ) ->
    case cowboy_req:qs_val( <<"token">>, Req ) of
        {Value, _} ->
            if (Value =:= true ) or ( Value =:= undefined ) or ( Value =:= <<>> ) ->
                false;
                true ->
                    {true, Value}
            end;
        _ ->
            false
    end.

is_email_activation( Req ) ->
    case cowboy_req:qs_val( <<"activetoken">>, Req ) of
        {Value, _} ->
            if (Value =:= true ) or ( Value =:= undefined ) or ( Value =:= <<>> ) ->
                false;
                true ->
                    {true, Value}
            end;
        _ ->
            false
    end.

parse_link_type(Req) ->
    {Varlist, _} = cowboy_req:qs_vals(Req),
    if (length(Varlist) /= 1) ->
        {false, error, error};
        true ->
            case {is_register_activation( Req ),
                  is_email_activation( Req ) } of
                { {ture, Value}, {false, _} } ->
                    {Host, _} = cowboy_req:host( Req ),
                    { register, Value, Host };
                { {false, _}, {true, Value} } ->
                    {Host, _} = cowboy_req:host( Req ),
                    { email, Value, Host };
                _ ->
                    {error, error, error}


            end
    end.


response_not_fount( Req ) ->
    cowboy_req:reply( 404, [
                            {<<"content-type">>, <<"text/html">>}
                           ], <<"Page not found">>, Req ).

response_register_ok( Req ) ->
    cowboy_req:reply( 200,
                      [{<<"content-type">>,
                        <<"text/html">>}
                      ],
                      <<?SUCCESS_PAGE>>,
                      Req ).

response_activation_ok(Req) ->
    cowboy_req:reply(200,
                    [{<<"content-type">>,
                    <<"text/html">>}
                    ],
                    <<?PAGE_FRONT/binary, "Your email validation ok.", ?PAGE_END/binary>>, Req ).


response_failed( Req ) ->
    cowboy_req:reply( 200,
                      [{<<"content-type">>,
                        <<"text/html">>}
                      ], <<?PAGE_FRONT/binary, "Your account active failed, please try again later.", ?PAGE_END/binary>>, Req ).


response_not_author( Req ) ->
    cowboy_req:reply( 400,
                      [{<<"content-type">>,
                        <<"text/html">>}
                      ], <<"Unauthorized">>, Req ).

response_not_author_or_actived( Req ) ->
    cowboy_req:reply( 200,
        [{<<"content-type">>,
            <<"text/html">>}
        ], <<?PAGE_FRONT/binary, "Unauthorized or perpase already acvtived.", ?PAGE_END/binary>>, Req ).


handle(Req, State) ->
    case parse_link_type(Req) of
        {register, Token, Server} ->
            {ok, Req2} = case ejabberd_auth:activate_email_register(Token, Server) of
                             ok ->
                                 response_register_ok(Req);
                             {error, bad_request} ->
                                 response_not_author(Req);
                             {error, bad_request_or_actived} ->
                                 response_not_author_or_actived(Req);
                             {error, _Reason} ->
                                 response_failed(Req)
                         end,
            {ok, Req2, State};
        {email, Token, Server} ->
            {ok, Req2} = case ejabberd_auth:activate_email(Token, Server) of
                             ok ->
                                 response_activation_ok(Req);
                             {error, bad_request} ->
                                 response_not_author(Req);
                             {error, bad_request_or_actived} ->
                                 response_not_author_or_actived(Req);
                             {error, _Reason} ->
                                 response_failed(Req)
                         end,
            {ok, Req2, State};
        _ ->
            {ok, Req2} = response_not_fount(Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

