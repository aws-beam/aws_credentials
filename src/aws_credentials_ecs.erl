%% @doc Implements a provider which fetches credentials from an ECS
%% task. There are currently no options or environment variables. @end
-module(aws_credentials_ecs).
-behaviour(aws_credentials_provider).

-export([fetch/1]).

-spec fetch(any()) ->
        {ok, aws_credentials_provider:credentials(), aws_credentials_provider:expiration()} |
        {error, any()}.
fetch(_Options) ->
  RelativeUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
  case RelativeUri of
    false -> {error, container_credentials_unavailable};
    _ ->
      Path = "http://169.254.170.2" ++ RelativeUri,
      case aws_credentials_httpc:request(get, Path) of
        {ok, _Status, Body, _Headers} ->
          #{ <<"AccessKeyId">> := AccessKeyId
           , <<"SecretAccessKey">> := SecretAccessKey
           , <<"Token">> := Token
           , <<"Expiration">> := Expiration
           } = jsx:decode(Body),
          Creds =
            aws_credentials:make_map(?MODULE, AccessKeyId, SecretAccessKey, Token),
          {ok, Creds, Expiration};
        {error, Reason} ->
          {error, Reason}
      end
  end.
