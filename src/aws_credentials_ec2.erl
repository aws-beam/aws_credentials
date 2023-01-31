%% @doc This provider looks up credential information from EC2 metadata.
%% @end
-module(aws_credentials_ec2).
-behaviour(aws_credentials_provider).

-define(SESSION_TOKEN_URL,
        "http://169.254.169.254/latest/api/token").
-define(SESSION_TOKEN_TTL_HEADER,
        "x-aws-ec2-metadata-token-ttl-seconds").
-define(SESSION_TOKEN_TTL_SECONDS,
        "21600").
-define(SESSION_TOKEN_HEADER,
        "X-aws-ec2-metadata-token").
-define(CREDENTIAL_URL,
        "http://169.254.169.254/latest/meta-data/iam/security-credentials/").
-define(DOCUMENT_URL,
        "http://169.254.169.254/latest/dynamic/instance-identity/document").

-type role() :: binary().
-type session_token() :: binary().
-type field() :: string().
-type value() :: string().
-type request_header() :: {field(), value()}.
-type request_headers() :: [request_header()].

-export([fetch/1]).

-spec fetch(aws_credentials_provider:options()) ->
        {ok, aws_credentials_provider:credentials(), aws_credentials_provider:expiration()} |
        {error, any()}.
fetch(_Options) ->
    % Fetch the IMDS session token and convert it to the request headers.
    % Then pass the request headers to subsequent IMDS requests.
    {ok, SessionToken} = fetch_session_token(),
    {ok, RequestHeaders} = request_headers(SessionToken),
    {ok, Role} = fetch_role(RequestHeaders),
    {ok, AccessKeyID, SecretAccessKey, ExpirationTime, Token} =
      fetch_metadata(Role, RequestHeaders),
    {ok, Region} = fetch_document(RequestHeaders),
    Credentials = aws_credentials:make_map(?MODULE, AccessKeyID, SecretAccessKey, Token, Region),
    {ok, Credentials, ExpirationTime}.

-spec fetch_session_token() -> {ok, session_token()}.
fetch_session_token() ->
  RequestHeaders = [{?SESSION_TOKEN_TTL_HEADER, ?SESSION_TOKEN_TTL_SECONDS}],
  {ok, 200, Body, _Headers} =
    aws_credentials_httpc:request(put, ?SESSION_TOKEN_URL, RequestHeaders),
  {ok, Body}.

-spec request_headers(session_token()) -> {ok, request_headers()}.
request_headers(SessionToken) ->
  SessionTokenString = binary:bin_to_list(SessionToken),
  RequestHeaders = [{?SESSION_TOKEN_HEADER, SessionTokenString}],
    {ok, RequestHeaders}.

-spec fetch_role(request_headers()) -> {ok, role()}.
fetch_role(RequestHeaders) ->
    {ok, 200, Body, _Headers} =
      aws_credentials_httpc:request(get, ?CREDENTIAL_URL, RequestHeaders),
    {ok, Body}.

-spec fetch_metadata(role(), request_headers()) -> {'ok', binary(), binary(), binary(), binary()}.
fetch_metadata(Role, RequestHeaders) ->
    Url = lists:flatten(io_lib:format("~s~s", [?CREDENTIAL_URL, Role])),
    {ok, 200, Body, _Headers} = aws_credentials_httpc:request(get, Url, RequestHeaders),
    Map = jsx:decode(Body),
    {ok, maps:get(<<"AccessKeyId">>, Map),
     maps:get(<<"SecretAccessKey">>, Map),
     maps:get(<<"Expiration">>, Map),
     maps:get(<<"Token">>, Map)}.

-spec fetch_document(request_headers()) -> {ok, binary()}.
fetch_document(RequestHeaders) ->
    {ok, 200, Body, _Headers} = aws_credentials_httpc:request(get, ?DOCUMENT_URL, RequestHeaders),
    Map = jsx:decode(Body),
    {ok, maps:get(<<"region">>, Map)}.
