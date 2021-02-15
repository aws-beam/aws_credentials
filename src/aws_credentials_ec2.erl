%% @doc This provider looks up credential information from EC2 metadata.
%% @end
-module(aws_credentials_ec2).
-behaviour(aws_credentials_provider).

-define(CREDENTIAL_URL,
        "http://169.254.169.254/latest/meta-data/iam/security-credentials/").
-define(DOCUMENT_URL,
        "http://169.254.169.254/latest/dynamic/instance-identity/document").

-type role() :: binary().

-export([fetch/1]).

-spec fetch(aws_credentials_provider:options()) ->
        {ok, aws_credentials_provider:credentials(), aws_credentials_provider:expiration()} |
        {error, any()}.
fetch(_Options) ->
    {ok, Role} = fetch_role(),
    {ok, AccessKeyID, SecretAccessKey, ExpirationTime, Token} = fetch_metadata(Role),
    {ok, Region} = fetch_document(),
    Credentials = aws_credentials:make_map(?MODULE, AccessKeyID, SecretAccessKey, Token, Region),
    {ok, Credentials, ExpirationTime}.

-spec fetch_role() -> {ok, role()}.
fetch_role() ->
    {ok, 200, Body, _Headers} = aws_credentials_httpc:get(?CREDENTIAL_URL),
    {ok, Body}.

fetch_metadata(Role) ->
    Url = lists:flatten(io_lib:format("~s~s", [?CREDENTIAL_URL, Role])),
    {ok, 200, Body, _Headers} = aws_credentials_httpc:get(Url),
    Map = jsx:decode(Body),
    {ok, maps:get(<<"AccessKeyId">>, Map),
     maps:get(<<"SecretAccessKey">>, Map),
     maps:get(<<"Expiration">>, Map),
     maps:get(<<"Token">>, Map)}.

fetch_document() ->
    {ok, 200, Body, _Headers} = aws_credentials_httpc:get(?DOCUMENT_URL),
    Map = jsx:decode(Body),
    {ok, maps:get(<<"region">>, Map)}.
