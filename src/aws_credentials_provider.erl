%% @doc This is the behaviour definition for a credential provider module
%% and it iterates over a list of providers. You may set the `credential_providers`
%% Erlang environment variable if you want to restrict checking only a certain
%% subset of the default list.
%%
%% Default order of checking for credentials is:
%% <ol>
%%   <li>Erlang application environment</li>
%%   <li>OS environment</li>
%%   <li>Credentials from AWS file</li>
%%   <li>ECS Task credentials</li>
%%   <li>EC2 credentials</li>
%% </ol>
%%
%% Providers are expected to implement a function called `fetch/1' which
%% takes as its argument a proplist of options which may influence the
%% operation of the provider.  The fetch/1 function should return either
%% `{ok, Credentials, Expiration}' or `{error, Reason}'.
%%
%% If a provider returns {ok, ...} then evaluation stops at that provider.
%% If it returns {error, ...} then the next provider is executed in order
%% until either a set of credentials are returns or the tuple
%% `{error, no_credentials}' is returned.
%%
%% If a new provider is desired, the behaviour interface should be
%% implemented and its module name added to the default list.
%% @end
-module(aws_credentials_provider).

-export([fetch/0, fetch/1]).

-callback fetch ( Options :: proplists:proplist() ) -> {ok, Credentials :: map(),
                                                        Expiration :: binary() | pos_integer() | infinity}
                                                       | {error, Reason :: term()}.


-define(DEFAULT_PROVIDERS, [aws_credentials_env, aws_credentials_file, aws_credentials_ecs, aws_credentials_ec2]).

fetch() ->
    fetch([]).

fetch(Options) ->
    Providers = get_env(credential_providers, ?DEFAULT_PROVIDERS),
    evaluate_providers(Providers, Options).

evaluate_providers([], _Options) -> {error, no_credentials};
evaluate_providers([ H | T ], Options) ->
    case H:fetch(Options) of
        {error, _} = Error ->
            error_logger:error_msg("Provider ~p reports ~p", [H, Error]),
            evaluate_providers(T, Options);
        Credentials -> Credentials
    end.

get_env(Key, Default) ->
    case application:get_env(aws_credentials, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

