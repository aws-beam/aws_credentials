-module(aws_credentials_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CREDENTIAL_URL,
        "http://169.254.169.254/latest/meta-data/iam/security-credentials/").
-define(DOCUMENT_URL,
        "http://169.254.169.254/latest/dynamic/instance-identity/document").

all() -> [{group, boot}].

groups() -> [{boot, [],
              [fail_boot, fail_noboot]}].

init_per_group(mecked_metadata, Config) ->
    Role = <<"aws-metadata-user">>,
    AccessKeyID = <<"AccessKeyID">>,
    SecretAccessKey = <<"SecretAccessKey">>,
    Expiry = <<"2025-09-25T23:43:56Z">>,
    Region = <<"ap-southeast-1">>,
    Token = <<"token">>,
    Credentials = <<"{
      \"Code\" : \"Success\",
      \"LastUpdated\" : \"2015-09-25T17:19:52Z\",
      \"Type\" : \"AWS-HMAC\",
      \"AccessKeyId\" : \"AccessKeyID\",
      \"SecretAccessKey\" : \"SecretAccessKey\",
      \"Token\" : \"token\",
      \"Expiration\" : \"2025-09-25T23:43:56Z\"
    }">>,
    Document = <<"{
      \"instanceType\" : \"t2.micro\",
      \"instanceId\" : \"i-a40e9300\",
      \"billingProducts\" : null,
      \"accountId\" : \"173533974591\",
      \"imageId\" : \"ami-52978200\",
      \"pendingTime\" : \"2015-10-31T21:15:54Z\",
      \"kernelId\" : null,
      \"ramdiskId\" : null,
      \"architecture\" : \"x86_64\",
      \"region\" : \"ap-southeast-1\",
      \"version\" : \"2010-08-31\",
      \"availabilityZone\" : \"ap-southeast-1a\",
      \"privateIp\" : \"172.30.0.79\",
      \"devpayProductCodes\" : null
    }">>,
    meck:new(aws_credentials_httpc, [no_link, passthrough]),
    meck:expect(aws_credentials_httpc, get, fun(<<?CREDENTIAL_URL>>) ->
                                                {ok, 200, Role, []};
                                               (<<?CREDENTIAL_URL, Rolef/binary>>) when Role == Rolef ->
                                                {ok, 200, Credentials, []};
                                               (<<?DOCUMENT_URL>>) ->
                                                {ok, 200, Document, []}
                                            end),
    [{access_key, AccessKeyID},
     {secret_key, SecretAccessKey},
     {expiry, Expiry},
     {token, Token},
     {region, Region}|Config];
init_per_group(boot, Config) ->
    meck:new(aws_credentials_ec2, [no_link, passthrough]),
    meck:expect(aws_credentials_ec2, fetch, fun(_) -> error(mocked_bad) end),
    Config.

end_per_group(mecked_metadata, Config) ->
    meck:unload(aws_credentials_httpc),
    Config;
end_per_group(boot, Config) ->
    meck:unload(aws_credentials_ec2),
    Config.

init_per_testcase(fail_boot, Config) ->
    Config;
init_per_testcase(fail_noboot, Config) ->
    Config;
init_per_testcase(_, Config) ->
  meck:new(aws_credentials_file, [no_link, passthrough]),
  meck:expect(aws_credentials_file, fetch, 1, {error, mocked}),
    application:load(aws_credentials),
    {ok, Apps} = application:ensure_all_started(aws_credentials),
    [{apps, Apps}|Config].

end_per_testcase(Testcase, Config)
  when Testcase == fail_boot;
       Testcase == fail_noboot ->
    Config;
end_per_testcase(_, Config) ->
    Apps = ?config(apps, Config),
    lists:foreach(fun(App) -> ok = application:stop(App) end,
                  lists:reverse(Apps)),
    Config.

fail_boot(_Config) ->
    application:load(aws_credentials),
    application:set_env(aws_credentials, fail_if_unavailable, false),
    {ok, Apps} = application:ensure_all_started(aws_credentials),
    _ = [application:stop(App) || App <- Apps],
    application:set_env(aws_credentials, fail_if_unavailable, true),
    ok.

fail_noboot(_Config) ->
    application:load(aws_credentials),
    application:set_env(aws_credentials, fail_if_unavailable, true),
    ?assertMatch({error, {aws_credentials, _}},
                 application:ensure_all_started(aws_credentials)).
