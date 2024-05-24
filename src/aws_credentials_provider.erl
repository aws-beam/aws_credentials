%% @doc This is the behaviour definition for a credential provider
%% module and it iterates over a list of providers. You may set the
%% `credential_providers` Erlang environment variable if you want to
%% restrict checking only a certain subset of the default list or
%% if you want to use your own custom providers.
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
%% until either a set of credentials are returned or the tuple
%% `{error, no_credentials}' is returned.
%%
%% If a new provider is desired, the behaviour interface should be
%% implemented and its module name added to the default list.
%% @end
-module(aws_credentials_provider).

-export([fetch/0, fetch/1]).

%% `credential_path' and `profile' are treated as common options,
%% and their values are inherited by `provider_options()'
%% unless the same options exist in `provider_options()'.
%% Note: This behaviour is for compatibility reason only, and
%% do not add any other common options.
-type options() :: #{ credential_path => string()
                    , profile => binary()
                    , provider() => provider_options()
                    }.
-type provider_options() :: #{ credential_path => string()
                             , profile => binary()
                             , any() => any()
                             }.
-type expiration() :: binary() | pos_integer() | infinity.
-type provider() :: aws_credentials_env
                  | aws_credentials_file
                  | aws_credentials_ecs
                  | aws_credentials_ec2
                  | module().
-type error_log() :: [{provider(), term()}].
-export_type([ options/0, expiration/0, provider/0 ]).

-callback fetch(provider_options()) ->
  {ok, aws_credentials:credentials(), expiration()} | {error, any()}.

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_PROVIDERS, [aws_credentials_env,
                            aws_credentials_file,
                            aws_credentials_ecs,
                            aws_credentials_ec2]).

-spec fetch() ->
        {ok, aws_credentials:credentials(), expiration()} |
        {'error', 'no_credentials'} |
        {'error', error_log()}.
fetch() ->
    fetch(#{}).

-spec fetch(options()) ->
        {ok, aws_credentials:credentials(), expiration()} |
        {'error', 'no_credentials'} |
        {'error', error_log()}.
fetch(Options) ->
    Providers = get_env(credential_providers, ?DEFAULT_PROVIDERS),
    evaluate_providers(Providers, Options, []).

-spec evaluate_providers([provider() | {provider(), options()}], options(), error_log()) ->
        {ok, aws_credentials:credentials(), expiration()} |
        {'error', no_credentials} |
        {'error', error_log()}.
evaluate_providers([], _Options, []) ->
    {error, no_credentials};
evaluate_providers([], _Options, Errors) when is_list(Errors) ->
    {error, lists:reverse(Errors)};
evaluate_providers([ Provider | Providers ], Options, Errors) ->
    ProviderOptions = get_provider_options(Provider, Options),
    case Provider:fetch(ProviderOptions) of
        {error, _} = Error ->
            evaluate_providers(Providers, Options, [{Provider, Error} | Errors]);
        {ok, Credentials, Expiration} ->
            {ok, Credentials, Expiration}
    end.

-spec get_env(atom(), [provider()]) -> any().
get_env(Key, Default) ->
    case application:get_env(aws_credentials, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

-spec get_provider_options(provider(), options()) -> provider_options().
get_provider_options(Provider, Options) ->
    ProviderOptions = maps:get(Provider, Options, #{}),
    IsCommonOptions = fun(profile, _) -> true; (credential_path, _) -> true; (_, _) -> false end,
    CommonOptions = maps:filter(IsCommonOptions, Options),
    %% If an option exists in both ProviderOptions and CommonOptions,
    %% the value in ProviderOptions should be adopted.
    maps:merge(CommonOptions, ProviderOptions).
