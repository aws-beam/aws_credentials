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
-define(AWS_REGION, ["AWS_REGION", "AWS_DEFAULT_REGION"]).

-export([fetch/1]).

-spec get_region() -> undefined | binary().
get_region() ->
    get_env(?AWS_REGION).

-spec get_env([string()]) -> undefined | binary().
get_env([]) -> undefined;
get_env([Head | Tail]) ->
    case os:getenv(Head) of
        false -> get_env(Tail);
        Val -> list_to_binary(Val)
    end.

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
make_map({ok, Status, Body, Headers}) ->
  handle_response(Status, Body, Headers).

-spec handle_response(aws_credentials_httpc:status_code(),
                      aws_credentials_httpc:body(),
                      aws_credentials_httpc:headers()) ->
        {error, _}
      | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
handle_response(200, Body, _Headers) ->
  parse_credentials(Body);
handle_response(Status, Body, Headers) ->
  {error, build_error_payload(Status, Body, Headers)}.

-spec parse_credentials(aws_credentials_httpc:body()) ->
        {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
parse_credentials(Body) ->
  {Doc, []} = xmerl_scan:string(binary_to_list(Body)),
  [#xmlText{value = AccessKeyId}] = xmerl_xpath:string("//Credentials/AccessKeyId/text()", Doc),
  [#xmlText{value = SecretAccessKey}] =
    xmerl_xpath:string("//Credentials/SecretAccessKey/text()", Doc),
  [#xmlText{value = Token}] = xmerl_xpath:string("//Credentials/SessionToken/text()", Doc),
  [#xmlText{value = Expiration}] = xmerl_xpath:string("//Credentials/Expiration/text()", Doc),
  Creds = case get_region() of
    undefined ->
      aws_credentials:make_map(?MODULE,
        list_to_binary(AccessKeyId),
        list_to_binary(SecretAccessKey),
        list_to_binary(Token));
    Region ->
      aws_credentials:make_map(?MODULE,
        list_to_binary(AccessKeyId),
        list_to_binary(SecretAccessKey),
        list_to_binary(Token),
        Region)
  end,
  {ok, Creds, list_to_binary(Expiration)}.

-spec build_error_payload(aws_credentials_httpc:status_code(),
                          aws_credentials_httpc:body(),
                          aws_credentials_httpc:headers()) -> map().
build_error_payload(Status, Body, _Headers) ->
  try xmerl_scan:string(binary_to_list(Body)) of
    {Doc, []} ->
      #{ status => Status,
        code => extract_code(Doc),
        message => extract_error_message(Doc)
      }
  catch
   _ ->
    <<"unable to parse response as XML">>
  end.

-spec extract_code(XmlElement::any()) -> binary().
extract_code(Doc) ->
  case xmerl_xpath:string("//*[local-name()='Code']/text()", Doc) of
    [#xmlText{value = C}] -> list_to_binary(C);
    _ -> <<"Unable to find error code in AssumeRoleWithWebIdentity response">>
  end.

-spec extract_error_message(XmlElement::any()) -> binary().
extract_error_message(Doc) ->
  case xmerl_xpath:string("//*[local-name()='Message']/text()", Doc) of
    [#xmlText{value = Msg}] -> list_to_binary(Msg);
    _ -> <<"Unable to find error message in AssumeRoleWithWebIdentity response">>
  end.
