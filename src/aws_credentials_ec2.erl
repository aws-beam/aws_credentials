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

-type non_200(Tag) :: {Tag, {non_200,
                             aws_credentials_httpc:status_code(),
                             aws_credentials_httpc:body(),
                             aws_credentials_httpc:headers()}}
                    | {Tag, aws_credentials_httpc:response_error()}.

-export([fetch/1]).

-spec fetch(aws_credentials_provider:options()) ->
        {error, _}
      | {ok, aws_credentials_provider:credentials(), aws_credentials_provider:expiration()}.
fetch(_Options) ->
    % Fetch the IMDS session token and convert it to the request headers.
    % Then pass the request headers to subsequent IMDS requests.
    SessionTokenR = fetch_session_token(),
    RequestHeadersR = request_headers(SessionTokenR),
    RoleR = fetch_role(RequestHeadersR),
    MetadataR = fetch_metadata(RoleR, RequestHeadersR),
    DocumentR = fetch_document(RequestHeadersR),
    make_map(MetadataR, DocumentR).

-spec fetch_session_token() ->
        {error, non_200(ec2_session_token_unavailable)}
      | {ok, session_token()}.
fetch_session_token() ->
  RequestHeaders = [{?SESSION_TOKEN_TTL_HEADER, ?SESSION_TOKEN_TTL_SECONDS}],
  case aws_credentials_httpc:request(put, ?SESSION_TOKEN_URL, RequestHeaders) of
    {ok, 200, Body, _Headers} -> {ok, Body};
    Other -> {error, non_200(ec2_session_token_unavailable, Other)}
  end.

-spec request_headers({error, _}
                    | {ok, session_token()}) ->
        {error, _}
      | {ok, request_headers()}.
request_headers({error, _Error} = Error) -> Error;
request_headers({ok, SessionToken}) ->
  SessionTokenString = binary:bin_to_list(SessionToken),
  RequestHeaders = [{?SESSION_TOKEN_HEADER, SessionTokenString}],
  {ok, RequestHeaders}.

-spec fetch_role({error, _}
               | {ok, request_headers()}) ->
        {error, non_200(ec2_role_unavailable)}
      | {error, _}
      | {ok, role()}.
fetch_role({error, _Error} = Error) -> Error;
fetch_role({ok, RequestHeaders}) ->
    case aws_credentials_httpc:request(get, ?CREDENTIAL_URL, RequestHeaders) of
      {ok, 200, Body, _Headers} -> {ok, Body};
      Other -> {error, non_200(ec2_role_unavailable, Other)}
    end.

-spec fetch_metadata({error, _}
                   | {ok, role()},
                     {error, _}
                   | {ok, request_headers()}) ->
        {error, non_200(ec2_metadata_unavailable)}
      | {error, _}
      | {ok, {aws_credentials:access_key_id(),
              aws_credentials:secret_access_key(),
              aws_credentials_provider:expiration(),
              aws_credentials:token()}}.
fetch_metadata({error, _Error} = Error, _RequestHeadersR) -> Error;
fetch_metadata(_RoleR, {error, _Error} = Error) -> Error;
fetch_metadata({ok, Role}, {ok, RequestHeaders}) ->
    Url = lists:flatten(io_lib:format("~s~s", [?CREDENTIAL_URL, Role])),
    case aws_credentials_httpc:request(get, Url, RequestHeaders) of
      {ok, 200, Body, _Headers} ->
        Map = jsx:decode(Body),
        {ok, {maps:get(<<"AccessKeyId">>, Map),
              maps:get(<<"SecretAccessKey">>, Map),
              maps:get(<<"Expiration">>, Map),
              maps:get(<<"Token">>, Map)}};
       Other -> {error, non_200(ec2_metadata_unavailable, Other)}
     end.

-spec fetch_document({error, _}
                   | {ok, request_headers()}) ->
        {error, non_200(ec2_document_unavailable)}
      | {error, _}
      | {ok, aws_credentials:region()}.
fetch_document({error, _Error} = Error) -> Error;
fetch_document({ok, RequestHeaders}) ->
    case aws_credentials_httpc:request(get, ?DOCUMENT_URL, RequestHeaders) of
      {ok, 200, Body, _Headers} ->
        Map = jsx:decode(Body),
        {ok, maps:get(<<"region">>, Map)};
      Other -> {error, non_200(ec2_document_unavailable, Other)}
    end.

-spec make_map({error, _}
             | {ok, {aws_credentials:access_key_id(),
                     aws_credentials:secret_access_key(),
                     aws_credentials_provider:expiration(),
                     aws_credentials:token()}},
               {error, _}
             | {ok, aws_credentials:region()}) ->
        {error, _}
      | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
make_map({error, _Error} = Error, _DocumentR) -> Error;
make_map(_MetadataR, {error, _Error} = Error) -> Error;
make_map({ok, {AccessKeyID, SecretAccessKey, ExpirationTime, Token}}, {ok, Region}) ->
  Credentials = aws_credentials:make_map(?MODULE, AccessKeyID, SecretAccessKey, Token, Region),
  {ok, Credentials, ExpirationTime}.

-spec non_200(Tag, aws_credentials_httpc:response()) -> non_200(Tag).
non_200(Tag, {ok, StatusCode, Body, Headers}) ->
  {Tag, {non_200, StatusCode, Body, Headers}};
non_200(Tag, {error, Error}) ->
  {Tag, Error}.
