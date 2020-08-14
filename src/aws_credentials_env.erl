%% @doc This provider checks the Erlang environment and OS environment
%% variables for AWS credentials.  The following names can be used:
%% (They are case sensitive)
%% <pre>
%% AWS_ACCESS_KEY_ID
%% AWS_SECRET_ACCESS_KEY
%% AWS_SESSION_TOKEN
%% AWS_SECURITY_TOKEN
%% AWS_DEFAULT_REGION
%% AWS_REGION
%% </pre>
%%
%% At a minimum, the access key and secret keys must be defined.
%%
%% If used in an Erlang context, these environment variable names
%% should be strings. (I.e., is_list(EnvName) returns `true').
%% @end
-module(aws_credentials_env).
-behaviour(aws_credentials_provider).

-define(AWS_ACCESS,  ["AWS_ACCESS_KEY_ID"]).
-define(AWS_SECRET,  ["AWS_SECRET_ACCESS_KEY"]).
-define(AWS_SESSION, ["AWS_SESSION_TOKEN", "AWS_SECURITY_TOKEN"]).
-define(AWS_REGION,  ["AWS_DEFAULT_REGION", "AWS_REGION"]).

-export([fetch/1]).

fetch(_Options) ->
    case {get_env(?AWS_ACCESS), get_env(?AWS_SECRET),
          get_env(?AWS_SESSION), get_env(?AWS_REGION)} of

        {K, S, undefined, undefined} when is_binary(K),
                                          is_binary(S) ->
            {ok, aws_credentials:make_map(?MODULE, K, S), infinity};

        {K, S, T, undefined} when is_binary(K),
                                  is_binary(S),
                                  is_binary(T) ->
            {ok, aws_credentials:make_map(?MODULE, K, S, T), infinity};

        {K, S, T, R} when is_binary(K),
                          is_binary(S),
                          is_binary(T),
                          is_binary(R) ->
            {ok, aws_credentials:make_map(?MODULE, K, S, T, R), infinity};

        _Other -> {error, environment_credentials_unavailable}
    end.

get_env([]) -> undefined;
get_env([Head | Tail]) ->
    case {erlang_get_env(Head), os_getenv(Head)} of
        {undefined, false} -> get_env(Tail);
        {EnvVar, false} -> list_to_binary(EnvVar);
        {_,  OSVal} -> list_to_binary(OSVal)
    end.

os_getenv(Var) when is_list(Var) ->
    os:getenv(Var).

erlang_get_env(Var) when is_list(Var) ->
    Atom = make_env_var(string:to_lower(Var)),
    case application:get_env(aws_credentials, Atom, undefined) of
        undefined -> undefined;
        {ok, Value} -> Value
    end.

make_env_var(Var) when is_list(Var) -> list_to_atom(Var).
