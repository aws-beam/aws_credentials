%% @doc This implements a provider which fetches credentials from an ECS task. There
%% are currently no options or environment variables.
%% @end
-module(aws_credentials_ecs).
-behaviour(aws_credentials_provider).

-export([fetch/1]).

fetch(_Options) ->
    RelativeUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
    case RelativeUri of
      false -> {error, container_credentials_unavailable};
      _ ->
        CredentialsPath = "http://169.254.170.2" ++ RelativeUri,
        {ok, _Status, Body, _Headers} = aws_credentials_httpc:get(CredentialsPath),
        Map = jsone:decode(Body),
        Creds = aws_credentials:make_map(
                  ?MODULE,
                  maps:get(<<"AccessKeyId">>, Map),
                  maps:get(<<"SecretAccessKey">>, Map),
                  maps:get(<<"Token">>, Map)),
        {ok, Creds, maps:get(<<"Expiration">>, Map)}
    end.
