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
-define(DUMMY_ACCESS_KEY2, <<"dummy_access_key2">>).
-define(DUMMY_SECRET_ACCESS_KEY, <<"dummy_secret_access_key">>).
-define(DUMMY_SECRET_ACCESS_KEY2, <<"dummy_secret_access_key2">>).
-define(DUMMY_SESSION_TOKEN, "dummy-session-token").
-define(DUMMY_REGION, <<"us-east-1">>).
-define(DUMMY_REGION2, <<"us-east-2">>).

all() ->
  [ {group, file}
  , {group, config_credential}
  , {group, config_env}
  , {group, credential_env}
  , {group, profile_env}
  , {group, ec2}
  , {group, env}
  , {group, application_env}
  , {group, ecs}
  , {group, credential_process}
  ].

groups() ->
  [ {file, [], all_testcases()}
  , {config_credential, [], all_testcases()}
  , {config_env, [], all_testcases()}
  , {credential_env, [], all_testcases()}
  , {profile_env, [], all_testcases()}
  , {ec2, [], all_testcases()}
  , {env, [], all_testcases()}
  , {application_env, [], all_testcases()}
  , {ecs, [], all_testcases()}
  , {credential_process, [], all_testcases()}
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
  case GroupName of
    config_credential -> init_group(config_credential, provider(file), config_credential, Config);
    config_env -> init_group(config_env, provider(file), file, Config);
    credential_env -> init_group(credential_env, provider(file), credential_env, Config);
    profile_env -> init_group(profile_env, provider(file), config_credential, Config);
    application_env -> init_group(application_env, provider(env), application_env, Config);
    credential_process ->
        init_group(credential_process, provider(file), credential_process, Config);
    GroupName -> init_group(GroupName, Config)
  end.

end_per_group(_GroupName, Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  teardown_provider(?config(context, Config)),
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
  assert_test(GroupName).

%% Test assertions =================================================================

assert_test(config_credential) ->
  Provider = provider(file),
  assert_values(?DUMMY_ACCESS_KEY, ?DUMMY_SECRET_ACCESS_KEY, Provider, ?DUMMY_REGION);
assert_test(config_env) ->
  Provider = provider(file),
  assert_values(?DUMMY_ACCESS_KEY, ?DUMMY_SECRET_ACCESS_KEY, Provider, ?DUMMY_REGION2);
assert_test(application_env) ->
  Provider = provider(env),
  assert_values(?DUMMY_ACCESS_KEY, ?DUMMY_SECRET_ACCESS_KEY, Provider);
assert_test(credential_env) ->
  Provider = provider(file),
  assert_values(?DUMMY_ACCESS_KEY2, ?DUMMY_SECRET_ACCESS_KEY2, Provider);
assert_test(profile_env) ->
  Provider = provider(file),
  assert_values(?DUMMY_ACCESS_KEY2, ?DUMMY_SECRET_ACCESS_KEY2, Provider);
assert_test(credential_process) ->
  Provider = provider(file),
  assert_values(?DUMMY_ACCESS_KEY2, ?DUMMY_SECRET_ACCESS_KEY2, Provider);
assert_test(GroupName) ->
  Provider = provider(GroupName),
  assert_values(?DUMMY_ACCESS_KEY, ?DUMMY_SECRET_ACCESS_KEY, Provider).

assert_values(DummyAccessKey, DummySecretAccessKey, Provider) ->
  #{ access_key_id := AccessKeyId
   , credential_provider := CredentialProvider
   , secret_access_key := SecretAccessKey
   } = aws_credentials:get_credentials(),
  ?assertEqual(DummyAccessKey, AccessKeyId),
  ?assertEqual(Provider, CredentialProvider),
  ?assertEqual(DummySecretAccessKey, SecretAccessKey).

assert_values(DummyAccessKey, DummySecretAccessKey, Provider, DummyRegion) ->
  #{ access_key_id := AccessKeyId
   , credential_provider := CredentialProvider
   , secret_access_key := SecretAccessKey
   , region := Region
   } = aws_credentials:get_credentials(),
  ?assertEqual(DummyAccessKey, AccessKeyId),
  ?assertEqual(Provider, CredentialProvider),
  ?assertEqual(DummySecretAccessKey, SecretAccessKey),
  ?assertEqual(DummyRegion, Region).

%% Helpers ====================================================================
provider(GroupName) ->
  list_to_existing_atom("aws_credentials_" ++ atom_to_list(GroupName)).

provider_opts(file, Config) ->
  #{credential_path => ?config(data_dir, Config)};
provider_opts(config_credential, Config) ->
  #{credential_path => ?config(data_dir, Config) ++ "config_credential/"};
provider_opts(credential_env, _Config) ->
  #{credential_path => os:getenv("HOME")};
provider_opts(credential_process, Config) ->
  #{credential_path => ?config(data_dir, Config) ++ "credential_process/"};
provider_opts(_GroupName, _Config) ->
  #{}.

init_group(GroupName, Config) ->
  init_group(GroupName, provider(GroupName), GroupName, Config).

init_group(GroupName, Provider, ProviderName, Config) ->
  Context = setup_provider(GroupName, Config),
  ProviderOpts = provider_opts(ProviderName, Config),
  application:set_env(aws_credentials, credential_providers, [Provider]),
  application:set_env(aws_credentials, provider_options, ProviderOpts),
  {ok, Started} = application:ensure_all_started(aws_credentials),
  [{started, Started}, {context, Context}|Config].

group_name(Config) ->
  GroupProperties = ?config(tc_group_properties, Config),
  proplists:get_value(name, GroupProperties).

setup_provider(ec2, _Config) ->
  meck:new(httpc, [no_link, passthrough]),
  meck:expect(httpc, request, fun mock_httpc_request_ec2/5),
  #{ mocks => [httpc]
   , env => []
   };
setup_provider(env, _Config) ->
  OldAccessKeyId = os:getenv("AWS_ACCESS_KEY_ID"),
  OldSecretAccessKey = os:getenv("AWS_SECRET_ACCESS_KEY"),
  os:putenv("AWS_ACCESS_KEY_ID", binary_to_list(?DUMMY_ACCESS_KEY)),
  os:putenv("AWS_SECRET_ACCESS_KEY", binary_to_list(?DUMMY_SECRET_ACCESS_KEY)),
  #{ mocks => []
   , env => [ {"AWS_ACCESS_KEY_ID", OldAccessKeyId}
            , {"AWS_SECRET_ACCESS_KEY", OldSecretAccessKey}
            ]
   };
setup_provider(ecs, _Config) ->
  OldUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
  os:putenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/dummy-uri"),
  meck:new(httpc, [no_link, passthrough]),
  meck:expect(httpc, request, fun mock_httpc_request_ecs/5),
  #{ mocks => [httpc]
   , env => [{"AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", OldUri}]
   };
setup_provider(config_env, Config) ->
  Old = os:getenv("AWS_CONFIG_FILE"),
  os:putenv("AWS_CONFIG_FILE", ?config(data_dir, Config) ++ "env/config"),
  #{ mocks => []
   , env => [{"AWS_CONFIG_FILE", Old}]
   };
setup_provider(credential_env, Config) ->
  Old = os:getenv("AWS_SHARED_CREDENTIALS_FILE"),
  os:putenv("AWS_SHARED_CREDENTIALS_FILE", ?config(data_dir, Config) ++ "env/credentials"),
  #{ mocks => []
   , env => [{"AWS_SHARED_CREDENTIALS_FILE", Old}]
   };
setup_provider(profile_env, _Config) ->
  Old = os:getenv("AWS_PROFILE"),
  os:putenv("AWS_PROFILE", "foo"),
  #{ mocks => []
   , env => [{"AWS_PROFILE", Old}]
   };
setup_provider(application_env, _Config) ->
  application:set_env(aws_credentials
                     , aws_access_key_id
                     , binary_to_list(?DUMMY_ACCESS_KEY)),
  application:set_env(aws_credentials
                     , aws_secret_access_key
                     , binary_to_list(?DUMMY_SECRET_ACCESS_KEY)),
  #{ mocks => []
   , env => []
   };
setup_provider(_GroupName, _Config) ->
  #{ mocks => []
   , env => []
   }.

teardown_provider(Context) ->
  #{mocks := Mocks, env := Env} = Context,
  [meck:unload(Mock) || Mock <- Mocks],
  [maybe_put_env(Key, Value) || {Key, Value} <- Env],
  ok.

mock_httpc_request_ec2(Method, Request, HTTPOptions, Options, Profile) ->
  Headers = [{"X-aws-ec2-metadata-token", ?DUMMY_SESSION_TOKEN}],
  case Request of
    {"http://169.254.169.254/latest/api/token", _Headers} ->
      {ok, response('session-token')};
    {"http://169.254.169.254/latest/meta-data/iam/security-credentials/", Headers} ->
      {ok, response('security-credentials')};
    {"http://169.254.169.254/latest/meta-data/iam/security-credentials/dummy-role", Headers} ->
                  {ok, response('dummy-role')};
    {"http://169.254.169.254/latest/dynamic/instance-identity/document", Headers} ->
      {ok, response('document')};
    _ ->
      meck:passthrough([Method, Request, HTTPOptions, Options, Profile])
end.

mock_httpc_request_ecs(Method, Request, HTTPOptions, Options, Profile) ->
  case Request of
    {"http://169.254.170.2/dummy-uri", []} ->
      {ok, response('dummy-uri')};
    _ ->
      meck:passthrough([Method, Request, HTTPOptions, Options, Profile])
  end.

response(BodyTag) ->
  StatusLine = {unused, 200, unused},
  Headers = [],
  Body = body(BodyTag),
  {StatusLine, Headers, Body}.

body('session-token') ->
  <<?DUMMY_SESSION_TOKEN>>;
body('security-credentials') ->
  <<"dummy-role">>;
body('dummy-role') ->
  jsx:encode(#{ 'AccessKeyId' => ?DUMMY_ACCESS_KEY
              , 'SecretAccessKey' => ?DUMMY_SECRET_ACCESS_KEY
              , 'Expiration' => <<"2025-09-25T23:43:56Z">>
              , 'Token' => unused
              });
body('document') ->
  jsx:encode(#{ 'region' => unused });
body('dummy-uri') ->
  jsx:encode(#{ 'AccessKeyId' => ?DUMMY_ACCESS_KEY
              , 'SecretAccessKey' => ?DUMMY_SECRET_ACCESS_KEY
              , 'Expiration' => <<"2025-09-25T23:43:56Z">>
              , 'Token' => unused
              }).

maybe_put_env(Key, false) ->
  os:unsetenv(Key);
maybe_put_env(Key, Value) ->
  os:putenv(Key, Value).
