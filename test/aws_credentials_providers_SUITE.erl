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
  , {group, ec2}
  , {group, env}
  , {group, ecs}
  ].

groups() ->
  [ {file, [], all_testcases()}
  , {ec2, [], all_testcases()}
  , {env, [], all_testcases()}
  , {ecs, [], all_testcases()}
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
  Context = setup_provider(GroupName),
  Provider = provider(GroupName),
  ProviderOpts = provider_opts(GroupName, Config),
  application:set_env(aws_credentials, credential_providers,
                      [{Provider, ProviderOpts}]),
  {ok, Started} = application:ensure_all_started(aws_credentials),
  [{started, Started}, {context, Context}|Config].

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
  Provider = provider(GroupName),
  #{ access_key_id := AccessKeyId
   , credential_provider := CredentialProvider
   , secret_access_key := SecretAccessKey
   } = aws_credentials:get_credentials(),
  ?assertEqual(?DUMMY_ACCESS_KEY, AccessKeyId),
  ?assertEqual(Provider, CredentialProvider),
  ?assertEqual(?DUMMY_SECRET_ACCESS_KEY, SecretAccessKey).

%% Helpers ====================================================================
provider(GroupName) ->
  list_to_existing_atom("aws_credentials_" ++ atom_to_list(GroupName)).

provider_opts(file, Config) ->
  DataDir = ?config(data_dir, Config),
  CredentialsPath = filename:join([DataDir, "credentials"]),
  [{<<"credential_path">>, CredentialsPath}];
provider_opts(ec2, _Config) ->
  [];
provider_opts(env, _Config) ->
  [];
provider_opts(ecs, _Config) ->
  [].

group_name(Config) ->
  GroupProperties = ?config(tc_group_properties, Config),
  proplists:get_value(name, GroupProperties).

setup_provider(ec2) ->
  meck:new(httpc, [no_link, passthrough]),
  meck:expect(httpc, request, fun mock_httpc_request_ec2/5),
  #{ mocks => [httpc]
   , env => []
   };
setup_provider(env) ->
  OldAccessKeyId = os:getenv("AWS_ACCESS_KEY_ID"),
  OldSecretAccessKey = os:getenv("AWS_SECRET_ACCESS_KEY"),
  os:putenv("AWS_ACCESS_KEY_ID", binary_to_list(?DUMMY_ACCESS_KEY)),
  os:putenv("AWS_SECRET_ACCESS_KEY", binary_to_list(?DUMMY_SECRET_ACCESS_KEY)),
  #{ mocks => []
   , env => [ {"AWS_ACCESS_KEY_ID", OldAccessKeyId}
            , {"AWS_SECRET_ACCESS_KEY", OldSecretAccessKey}
            ]
   };
setup_provider(ecs) ->
  OldUri = os:getenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"),
  os:putenv("AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/dummy-uri"),
  meck:new(httpc, [no_link, passthrough]),
  meck:expect(httpc, request, fun mock_httpc_request_ecs/5),
  #{ mocks => [httpc]
   , env => [{"AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", OldUri}]
   };
setup_provider(_GroupName) ->
  #{ mocks => []
   , env => []
   }.

teardown_provider(Context) ->
  #{mocks := Mocks, env := Env} = Context,
  [meck:unload(Mock) || Mock <- Mocks],
  [maybe_put_env(Key, Value) || {Key, Value} <- Env],
  ok.

mock_httpc_request_ec2(Method, Request, HTTPOptions, Options, Profile) ->
  case Request of
    {"http://169.254.169.254/latest/meta-data/iam/security-credentials/", []} ->
      {ok, response('security-credentials')};
    {"http://169.254.169.254/latest/meta-data/iam/security-credentials/dummy-role", []} ->
      {ok, response('dummy-role')};
    {"http://169.254.169.254/latest/dynamic/instance-identity/document", []} ->
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

maybe_put_env(_Key, false) ->
  ok;
maybe_put_env(Key, Value) ->
  os:putenv(Key, Value).
