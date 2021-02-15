-module(aws_credentials_providers_SUITE).

%% Common test callbacks
-export([ all/0
        , groups/0
        , end_per_suite/1
        , end_per_group/2
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_group/2
        , init_per_testcase/2
        ]).

%% Test cases
-export([ get_credentials/1 ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(DUMMY_ACCESS_KEY, <<"dummy_access_key">>).
-define(DUMMY_SECRET_ACCESS_KEY, <<"dummy_secret_access_key">>).

all() ->
  [ {group, file}
  ].

groups() ->
  [ {file, [], all_testcases()}
  ].

all_testcases() ->
  ExcludedFuns = [init_per_suite, end_per_suite, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

%% Common test ================================================================

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

init_per_group(GroupName, Config) ->
  Provider = provider(GroupName),
  ProviderOpts = provider_opts(GroupName, Config),
  application:set_env(aws_credentials, credential_providers,
                      [{Provider, ProviderOpts}]),
  {ok, Started} = application:ensure_all_started(aws_credentials),
  [{started, Started}|Config].

end_per_group(_GroupName, Config) ->
  Started = ?config(started, Config),
  [application:stop(App) || App <- Started],
  Config.

init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({init, Config}).

end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({fin, Config}).

%% Test cases =================================================================

get_credentials(suite) -> [];
get_credentials({init, Config}) ->
  Config;
get_credentials({fin, Config}) ->
  Config;
get_credentials(Config) ->
  GroupName = group_name(Config),
  Credentials = aws_credentials:get_credentials(),
  ExpectedCredentials = #{ access_key_id => ?DUMMY_ACCESS_KEY
                         , credential_provider => provider(GroupName)
                         , secret_access_key => ?DUMMY_SECRET_ACCESS_KEY
                         },
  ?assertEqual(ExpectedCredentials, Credentials).

%% Helpers ====================================================================
provider(GroupName) ->
  list_to_existing_atom("aws_credentials_" ++ atom_to_list(GroupName)).

provider_opts(file, Config) ->
  DataDir = ?config(data_dir, Config),
  CredentialsPath = filename:join([DataDir, "credentials"]),
  [{<<"credential_path">>, CredentialsPath}].

group_name(Config) ->
  GroupProperties = ?config(tc_group_properties, Config),
  proplists:get_value(name, GroupProperties).
