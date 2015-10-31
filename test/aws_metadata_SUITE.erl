-module(aws_metadata_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [{group, mecked_metadata}].

groups() -> [{mecked_metadata, [],
              [test_make_client_with_defaults,
               test_make_client_with_options]}].

init_per_suite(Config) ->
    application:load(aws_metadata),
    {ok, Apps} = application:ensure_all_started(aws_metadata),
    [{apps, Apps}|Config].

end_per_suite(Config) ->
    Apps = ?config(apps, Config),
        lists:foreach(fun(App) -> ok = application:stop(App) end,
                                      lists:reverse(Apps)),
    Config.

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
    Config.

end_per_testcase(_, Config) ->
    aws_metadata:delete_client(),
    Config.

test_make_client_with_defaults(Config) ->
    AccessKeyID = ?config(access_key, Config),
    SecretAccessKey = ?config(secret_key, Config),
    {ok, ClientRef} = aws_metadata:make_client(),
    ?assertMatch(#{region     := <<"us-east-1">>,
                   endpoint   := <<"amazonaws.com">>,
                   access_key := AccessKeyID,
                   secret_key := SecretAccessKey},
                 aws_metadata:get_client(ClientRef)).

test_make_client_with_options(Config) ->
    AccessKeyID = ?config(access_key, Config),
    SecretAccessKey = ?config(secret_key, Config),
    Region = <<"eu-west-1">>,
    Endpoint = <<"example.com">>,
    {ok, ClientRef} = aws_metadata:make_client([{region, Region},
                                                {endpoint, Endpoint}]),
    ?assertMatch(#{region     := Region,
                   endpoint   := Endpoint,
                   access_key := AccessKeyID,
                   secret_key := SecretAccessKey},
                 aws_metadata:get_client(ClientRef)).
