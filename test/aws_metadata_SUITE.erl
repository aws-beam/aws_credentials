-module(aws_metadata_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(CREDENTIAL_URL,
        "http://169.254.169.254/latest/meta-data/iam/security-credentials/").
-define(DOCUMENT_URL,
        "http://169.254.169.254/latest/dynamic/instance-identity/document").

all() -> [{group, boot}, {group, mecked_metadata}].

groups() -> [{mecked_metadata, [],
              [test_get_client]},
             {boot, [],
              [fail_boot, fail_noboot]}].

init_per_group(mecked_metadata, Config) ->
    RoleList = make_ref(),
    Credentials = make_ref(),
    Document = make_ref(),
    Role = <<"aws-metadata-user">>,
    AccessKeyID = <<"AccessKeyID">>,
    SecretAccessKey = <<"SecretAccessKey">>,
    Expiry = <<"2019-09-25T23:43:56Z">>,
    Region = <<"ap-southeast-1">>,
    CredentialsBody = <<"{
      \"Code\" : \"Success\",
      \"LastUpdated\" : \"2015-09-25T17:19:52Z\",
      \"Type\" : \"AWS-HMAC\",
      \"AccessKeyId\" : \"AccessKeyID\",
      \"SecretAccessKey\" : \"SecretAccessKey\",
      \"Token\" : \"token\",
      \"Expiration\" : \"2019-09-25T23:43:56Z\"
    }">>,
    DocumentBody = <<"{
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
    meck:expect(hackney, get, fun(<<?CREDENTIAL_URL>>) ->
                                      {ok, 200, {}, RoleList};
                                 (<<?CREDENTIAL_URL, Rolef/binary>>) when Role == Rolef ->
                                      {ok, 200, {}, Credentials};
                                 (<<?DOCUMENT_URL>>) ->
                                      {ok, 200, {}, Document}
                              end),

    meck:expect(hackney, body, fun(Ref) when Ref == RoleList ->
                                       {ok, Role};
                                  (Ref) when Ref == Credentials ->
                                       {ok, CredentialsBody};
                                  (Ref) when Ref == Document ->
                                       {ok, DocumentBody}
                               end),
    [{access_key, AccessKeyID},
     {secret_key, SecretAccessKey},
     {expiry, Expiry},
     {region, Region}|Config];
init_per_group(boot, Config) ->
    meck:new(aws_metadata_client, [no_link, passthrough]),
    meck:expect(aws_metadata_client, fetch, fun() -> error(mocked_bad) end),
    Config.

end_per_group(mecked_metadata, Config) ->
    meck:unload(hackney),
    Config;
end_per_group(boot, Config) ->
    meck:unload(aws_metadata_client),
    Config.

init_per_testcase(fail_boot, Config) ->
    Config;
init_per_testcase(fail_noboot, Config) ->
    Config;
init_per_testcase(_, Config) ->
    application:load(aws_metadata),
    {ok, Apps} = application:ensure_all_started(aws_metadata),
    [{apps, Apps}|Config].

end_per_testcase(_, Config) ->
    Apps = ?config(apps, Config),
    lists:foreach(fun(App) -> ok = application:stop(App) end,
                  lists:reverse(Apps)),
    Config.

test_get_client(Config) ->
    AccessKeyID = ?config(access_key, Config),
    SecretAccessKey = ?config(secret_key, Config),
    Region = ?config(region, Config),
    Client = aws_client:make_client(AccessKeyID, SecretAccessKey, Region),
    ?assertMatch(Client, aws_metadata:get_client()).

fail_boot(_Config) ->
    application:load(aws_metadata),
    application:set_env(aws_metadata, fail_if_unavailable, false),
    {ok, Apps} = application:ensure_all_started(aws_metadata),
    _ = [application:stop(App) || App <- Apps],
    application:set_env(aws_metadata, fail_if_unavailable, true),
    ok.

fail_noboot(_Config) ->
    application:load(aws_metadata),
    application:set_env(aws_metadata, fail_if_unavailable, true),
    ?assertMatch({error, {aws_metadata,_}},
                 application:ensure_all_started(aws_metadata)).
