%% @doc This provider looks up credential information from EC2 metadata.
%% @end
-module(aws_credentials_ec2).
-behaviour(aws_credentials_provider).

-define(CREDENTIAL_URL,
        <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>).
-define(DOCUMENT_URL,
        <<"http://169.254.169.254/latest/dynamic/instance-identity/document">>).

-export([fetch/1]).

fetch(_Options) ->
    {ok, Role} = fetch_role(),
    {ok, AccessKeyID, SecretAccessKey, ExpirationTime, Token} = fetch_metadata(Role),
    {ok, Region} = fetch_document(),
    Credentials = aws_credentials:make_map(?MODULE, AccessKeyID, SecretAccessKey, Token, Region),
    {ok, Credentials, ExpirationTime}.

fetch_role() ->
    {ok, 200, Body, _Headers} = aws_credentials_httpc:get(?CREDENTIAL_URL),
    {ok, Body}.

fetch_metadata(Role) ->
    {ok, 200, Body, _Headers} = aws_credentials_httpc:get(iolist_to_binary([?CREDENTIAL_URL, Role])),
    Map = jsone:decode(Body),
    {ok, maps:get(<<"AccessKeyId">>, Map),
     maps:get(<<"SecretAccessKey">>, Map),
     maps:get(<<"Expiration">>, Map),
     maps:get(<<"Token">>, Map)}.

fetch_document() ->
    {ok, 200, Body, _Headers} = aws_credentials_httpc:get(?DOCUMENT_URL),
    Map = jsone:decode(Body),
    {ok, maps:get(<<"region">>, Map)}.

