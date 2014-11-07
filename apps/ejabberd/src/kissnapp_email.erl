%%%-------------------------------------------------------------------
%%% @doc
%%% send email with mailgun(https://mailgun.com/cp) https API.
%%%-------------------------------------------------------------------
-module(kissnapp_email).


%% API
-export([send_text/3,
         send_html/3,
         send_register_validate/3,
         test_sendtext/0,
         test_sendhtml/0,
         test_send_register_validate/0]).

%% desc:    send text  email.
%% Subject: email subject.
%% ToEmail: recipient email adress.
%% html: content.
send_text(Subject,ToEmail,Text) ->
  inets:start(),
  ssl:start(),
  %io:format("ssl Status:~p~n",[SSLStatus]),
  %application:start(inets),
  Method = post,
  URL = "https://api:key-c3fa23e26ecd8d5f61c0397932e29fbc@api.mailgun.net/v2/yoolosoft.com/messages",
  Header = [],
  Type = "application/x-www-form-urlencoded",
  Body = lists:concat(["from=postmaster@yoolosoft.com" ,"&to=", ToEmail ,"&subject=", Subject ,"&text=", Text]),
  %Body ="from=postmaster@yoolosoft.com&to=jaypkwyl@163.com&subject=Kissnapp Validate Email&text=Hello,World!",
  HTTPOptions = [],
  Options = [],
  %R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  %R=httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options),
  %io:format("Send Result ~p~n ~p~n",[Body, R]).
  httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options).


%% desc:    send html file email.
%% Subject: email subject.
%% ToEmail: recipient email adress.
%% html: content.
send_html(Subject, ToEmail, Html) ->
  inets:start(),
  ssl:start(),
  %io:format("ssl Status:~p~n",[SSLStatus]),
  %application:start(inets),
  Method = post,
  URL = "https://api:key-c3fa23e26ecd8d5f61c0397932e29fbc@api.mailgun.net/v2/yoolosoft.com/messages",
  Header = [],
  Type = "application/x-www-form-urlencoded",
  Body = lists:concat(["from=postmaster@yoolosoft.com" ,"&to=", ToEmail ,"&subject=", Subject ,"&html=", Html]),
  %Body ="from=postmaster@yoolosoft.com&to=jaypkwyl@163.com&subject=Kissnapp Validate Email&text=Hello,World!",
  HTTPOptions = [],
  Options = [],
  %R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  %R=httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options),
  %io:format("Send Result ~p~n ~p~n",[Body, R]).
  httpc:request(Method,{URL, Header, Type, Body}, HTTPOptions, Options).


%% desc:    interface for send email for verification.
%% Subject: email subject.
%% ToEmail: recipient email adress.
%% Content: link address.
send_register_validate(Subject,ToEmail,Content)->
  Chtml="<!DOCTYPE html>
  <html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
  </head>
  <body>
  <div style=\"width:100%;font-size:20px;text-align:center\">
  Welcome to register  Kissnapp <br/> <br/>
  <a style=\"color:blue\" href=\""++Content++"\">Click Validate Email</a>
  </div>
  </body>
  </html>",
  send_html(Subject,ToEmail,Chtml).

%% test send tesxt.
test_sendtext()->
  send_text("Kissnapp Validate Email","jaypkwyl@163.com","Hello,World!").

%% test send html.
test_sendhtml()->
  send_html("Kissnapp Validate Email","jaypkwyl@163.com","Hello,World!").

%% test send email for verification.
test_send_register_validate()->
  send_register_validate("Kissnapp Register Vilidate","jaypkwyl@163.com","http://www.baidu.com").
