%% @doc This provider looks up credential information from web identity token
%% Environment parameters:
%% <ul>
%%   <li> &lt;&lt;"role_session_name"&gt;&gt; - this is provided to the credential fetch endpoint,
%%   and will label the provided session with that name, see:
%%   https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html#API_AssumeRoleWithWebIdentity_RequestParameters
%%   By default this is `erlang_aws_credentials'</li>
%% </ul>
%% @end
-module(aws_credentials_web_identity).
-behaviour(aws_credentials_provider).

-include_lib("xmerl/include/xmerl.hrl").

-define(ASSUME_ROLE_URL,
        "https://sts.amazonaws.com/?Action=AssumeRoleWithWebIdentity&Version=2011-06-15" ++
        "&RoleArn=~s&WebIdentityToken=~s&RoleSessionName=~s").
-define(DEFAULT_SESSION_NAME, "erlang_aws_credentials").

-export([fetch/1]).

-spec fetch(aws_credentials_provider:options()) ->
        {error, _}
      | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
fetch(Options) ->
  RoleArn = os:getenv("AWS_ROLE_ARN"),
  TokenFile = os:getenv("AWS_WEB_IDENTITY_TOKEN_FILE"),
  AuthToken = read_token(TokenFile),
  SessionName = maps:get(role_session_name, Options, ?DEFAULT_SESSION_NAME),
  Response = fetch_assume_role_token(RoleArn, AuthToken, SessionName),
  make_map(Response).

-spec read_token(false | string()) -> {error, _} | {ok, binary()}.
read_token(false) -> {error, no_credentials};
read_token(Path) -> file:read_file(Path).

-spec fetch_assume_role_token(false | string(), {error, _} | {ok, binary()}, binary()) ->
        {error, _}
      | {ok, aws_credentials_httpc:status_code(),
              aws_credentials_httpc:body(),
              aws_credentials_httpc:headers()}.
fetch_assume_role_token(false, _AuthToken, _SessionName) -> {error, no_credentials};
fetch_assume_role_token(_RoleArn, {error, _Error} = Error, _SessionName) -> Error;
fetch_assume_role_token(RoleArn, {ok, AuthToken}, SessionName) ->
  Url = lists:flatten(io_lib:format(?ASSUME_ROLE_URL, [RoleArn, AuthToken, SessionName])),
  aws_credentials_httpc:request(get, Url).

-spec make_map({error, _}
| {ok, aws_credentials_httpc:status_code(),
                    aws_credentials_httpc:body(),
                    aws_credentials_httpc:headers()}) ->
        {error, _}
      | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
make_map({error, _Error} = Error) -> Error;
make_map({ok, _Status, Body, _Headers}) ->
  {Doc, []} = xmerl_scan:string(binary_to_list(Body)),
  [#xmlText{value = AccessKeyId}] = xmerl_xpath:string("//Credentials/AccessKeyId/text()", Doc),
  [#xmlText{value = SecretAccessKey}] =
    xmerl_xpath:string("//Credentials/SecretAccessKey/text()", Doc),
  [#xmlText{value = Token}] = xmerl_xpath:string("//Credentials/SessionToken/text()", Doc),
  [#xmlText{value = Expiration}] = xmerl_xpath:string("//Credentials/Expiration/text()", Doc),
  Creds = aws_credentials:make_map(?MODULE,
    list_to_binary(AccessKeyId),
    list_to_binary(SecretAccessKey),
    list_to_binary(Token)),
  {ok, Creds, list_to_binary(Expiration)}.
