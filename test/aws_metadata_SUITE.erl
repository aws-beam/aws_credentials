-module(aws_metadata_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [{group, mecked_metadata}].

groups() -> [{mecked_metadata, [],
              [test_get_client]}].

init_per_group(mecked_metadata, Config) ->
    RoleList = make_ref(),
    Creds = make_ref(),
    Role = <<"aws-metadata-user">>,
    AccessKeyID = <<"AccessKeyID">>,
    SecretAccessKey = <<"SecretAccessKey">>,
    Expiry = <<"2019-09-25T23:43:56Z">>,
    Body = <<"{
      \"Code\" : \"Success\",
      \"LastUpdated\" : \"2015-09-25T17:19:52Z\",
      \"Type\" : \"AWS-HMAC\",
      \"AccessKeyId\" : \"AccessKeyID\",
      \"SecretAccessKey\" : \"SecretAccessKey\",
      \"Token\" : \"token\",
      \"Expiration\" : \"2019-09-25T23:43:56Z\"
    }">>,
    meck:expect(hackney, get, fun(URL) when is_binary(URL) ->
                                      {ok, 200, {}, RoleList};
                                 ([URL, Rolef]) when is_binary(URL)
                                                     andalso Role == Rolef ->
                                      {ok, 200, {}, Creds}
                              end),

    meck:expect(hackney, body, fun(Ref) when Ref == RoleList ->
                                       {ok, Role};
                                  (Ref) when Ref == Creds ->
                                       {ok, Body}
                               end),
    [{access_key, AccessKeyID},
     {secret_key, SecretAccessKey},
     {expiry, Expiry}|Config].

end_per_group(mecked_metadata, Config) ->
    meck:unload(hackney),
    Config.

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
    Region = <<"us-east-1">>,
    Client = aws_client:make_client(AccessKeyID, SecretAccessKey, Region),
    ?assertMatch(Client, aws_metadata:get_client()).
