%% @doc This provider looks up credential information from EKS Pod Identity
%% @end
-module(aws_credentials_eks).
-behaviour(aws_credentials_provider).

-define(AUTHORIZATION_HEADER, "authorization").

-export([fetch/1]).

-spec fetch(aws_credentials_provider:options()) ->
        {error, _}
      | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
fetch(_Options) ->
  FullUri = os:getenv("AWS_CONTAINER_CREDENTIALS_FULL_URI"),
  TokenFile = os:getenv("AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE"),
  AuthToken = read_token(TokenFile),
  Response = fetch_session_token(FullUri, AuthToken),
  make_map(Response).

-spec read_token(false | string()) -> {error, _} | {ok, binary()}.
read_token(false) -> {error, no_credentials};
read_token(Path) -> file:read_file(Path).

-spec fetch_session_token(false | string(), {error, _} | {ok, string()}) ->
        {error, _}
      | {ok, aws_credentials_httpc:status_code(),
              aws_credentials_httpc:body(),
              aws_credentials_httpc:headers()}.
fetch_session_token(false, _AuthToken) -> {error, no_credentials};
fetch_session_token(_FullUri, {error, _Error} = Error) -> Error;
fetch_session_token(FullUri, {ok, AuthToken}) ->
  aws_credentials_httpc:request(get, FullUri, [{?AUTHORIZATION_HEADER, AuthToken}]).


-spec make_map({error, _}
             | {ok, aws_credentials_httpc:status_code(),
                    aws_credentials_httpc:body(),
                    aws_credentials_httpc:headers()}) ->
        {error, _}
      | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
make_map({error, _Error} = Error) -> Error;
make_map({ok, _Status, Body, _Headers}) ->
  #{ <<"AccessKeyId">> := AccessKeyId
   , <<"SecretAccessKey">> := SecretAccessKey
   , <<"Token">> := Token
   , <<"Expiration">> := Expiration
  } = jsx:decode(Body),
  Creds = aws_credentials:make_map(?MODULE, AccessKeyId, SecretAccessKey, Token),
  {ok, Creds, Expiration}.
