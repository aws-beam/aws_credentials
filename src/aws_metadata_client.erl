-module(aws_metadata_client).

-export([fetch/0]).

-define(CREDENTIAL_URL,
        <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>).
-define(DOCUMENT_URL,
        <<"http://169.254.169.254/latest/dynamic/instance-identity/document">>).

fetch() ->
    {ok, Role} = fetch_role(),
    {ok, AccessKeyID, SecretAccessKey, ExpirationTime, Token} = fetch_metadata(Role),
    {ok, Region} = fetch_document(),
    Client = aws_client:make_temporary_client(AccessKeyID, SecretAccessKey, Token, Region),
    {ok, Client, ExpirationTime}.

fetch_role() ->
    {ok, 200, _, ClientRef} = hackney:get(?CREDENTIAL_URL),
    hackney:body(ClientRef).

fetch_metadata(Role) ->
    {ok, 200, _, ClientRef} = hackney:get(iolist_to_binary([?CREDENTIAL_URL, Role])),
    {ok, Body} = hackney:body(ClientRef),
    Map = jsx:decode(Body, [return_maps]),
    {ok, maps:get(<<"AccessKeyId">>, Map),
     maps:get(<<"SecretAccessKey">>, Map),
     maps:get(<<"Expiration">>, Map),
     maps:get(<<"Token">>, Map)}.

fetch_document() ->
    {ok, 200, _, ClientRef} = hackney:get(?DOCUMENT_URL),
    {ok, Body} = hackney:body(ClientRef),
    Map = jsx:decode(Body, [return_maps]),
    {ok, maps:get(<<"region">>, Map)}.
