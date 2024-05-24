%% @doc This provider reads the credentials and config files for the Amazon
%% CLI tools. <a href="https://docs.aws.amazon.com/cli/latest/userguide/cli-config-files.html">
%% This format is documented here</a> but an example might look
%% like:
%%
%% `~/.aws/credentials'
%%
%% <pre>
%% [default]
%% aws_access_key_id = AKIAIOSFODNN7EXAMPLE
%% aws_secret_access_key = wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
%% aws_session_token = wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLETOKEN
%% </pre>
%%
%% `~/.aws/config'
%%
%% <pre>
%% [default]
%% region = us-east-1
%% </pre>
%%
%% Environment parameters:
%% <ul>
%%   <li> 'credential_path' - this is the base path to the both CLI configuration files.
%%   And based on this path, credentials file should exist and config file is optional.
%%   By default this is "~/.aws/"</li>
%%   <li> 'profile'; - this is the desired profile to use in the credentials file.
%%   The profile can also be provided via the "AWS_PROFILE" os env.
%%   By default this is &lt;&lt;"default"&gt;&gt;</li>
%% </ul>
%% @end
-module(aws_credentials_file).
-behaviour(aws_credentials_provider).

-export([fetch/1]).

-type options() :: #{ credential_path => string()
                    , profile => binary()
                    }.
-export_type([options/0]).

-spec fetch(options()) ->
        {error, any()} | {ok, aws_credentials:credentials(), 'infinity'}.
fetch(Options) ->
    FilePath = get_file_path(Options),
    ConfigPath = does_credentials_file_exist(FilePath, config),
    case does_credentials_file_exist(FilePath, credentials) of
        {error, Error} ->
            {error, {credentials_file_does_not_exist, Error}};
        CredentialsPath ->
            CredFile = parse_credentials_file(CredentialsPath, Options),
            maybe_add_region(CredFile, ConfigPath, Options)
    end.

-spec does_credentials_file_exist(string(), atom()) -> {error, any()} | string().
does_credentials_file_exist({error, _} = Error, _File) -> Error;
does_credentials_file_exist(Path, credentials) ->
    maybe_path_from_env("AWS_SHARED_CREDENTIALS_FILE", check_path_exists(Path ++ "credentials"));
does_credentials_file_exist(Path, config) ->
    maybe_path_from_env("AWS_CONFIG_FILE", check_path_exists(Path ++ "config")).

-spec maybe_path_from_env(string(), string()) -> {error, any()} | string().
maybe_path_from_env(EnvVar, FilePath) ->
    case {os:getenv(EnvVar), FilePath} of
        {false, {error, _} = Error} -> Error;
        {false, Path} -> Path;
        {EnvPath, {error, _}} -> check_path_exists(EnvPath);
        {_, Path} -> Path
    end.

-spec get_file_path(options()) -> {error, any()} | string().
get_file_path(Options) ->
  case maps:get(credential_path, Options, undefined) of
    undefined -> maybe_add_home("/.aws/");
    Path -> Path
  end.

-spec maybe_add_home(string()) -> string() | {error, any()}.
maybe_add_home(Path) ->
    case os:getenv("HOME") of
        false -> {error, could_not_get_home_directory_from_os_environment};
        Home -> Home ++ Path
    end.

-spec maybe_add_region(
        {error, any()} | {ok, aws_credentials:credentials(), 'infinity'},
        {error, any()} | string(),
        options()
      ) -> {ok, aws_credentials:credentials(), 'infinity'}.
maybe_add_region({error, _} = Error, _Config, _Options) -> Error;
maybe_add_region(Result, {error, _Error}, _Options) -> Result;
maybe_add_region({ok, Credentials, infinity}, ConfigPath, Options) ->
    case parse_config_file(ConfigPath, Options) of
        {ok, Config} ->
            {ok, maps:put(region, maps:get(<<"region">>, Config), Credentials), infinity};
        {error, _} ->
            {ok, Credentials, infinity}
    end.

-spec check_path_exists(string()) -> {error, 'file_not_found'} | string().
check_path_exists(Path) ->
    case filelib:is_regular(Path) of
      false -> {error, file_not_found};
      true -> Path
    end.

-spec parse_credentials_file(string(), options()) ->
        {error, any()} | {ok, aws_credentials:credentials(), 'infinity'}.
parse_credentials_file(Path, Options) ->
    {ok, F} = file:read_file(Path),
    {ok, Profiles} = eini:parse(F),
    Desired = desired_profile(Options),

    case read_from_profile(Profiles, Desired) of
        {error, _} = Error -> Error;
        {ok, Profile} ->
            case maps:is_key(<<"aws_session_token">>, Profile) of
              true ->
                  {ok, aws_credentials:make_map(?MODULE,
                                              maps:get(<<"aws_access_key_id">>, Profile),
                                              maps:get(<<"aws_secret_access_key">>, Profile),
                                              maps:get(<<"aws_session_token">>, Profile)),
                   infinity};
              false ->
                {ok, aws_credentials:make_map(?MODULE,
                                              maps:get(<<"aws_access_key_id">>, Profile),
                                              maps:get(<<"aws_secret_access_key">>, Profile)),
                 infinity}
            end
    end.

-spec parse_config_file(string(), options()) ->
        {error, any()} | {ok, map()}.
parse_config_file(Path, Options) ->
    {ok, F} = file:read_file(Path),
    {ok, Profiles} = eini:parse(F),
    Desired = desired_profile(Options),
    read_from_profile(Profiles, Desired).

-spec read_from_profile(map(), binary()) -> any().
read_from_profile(File, Profile) ->
    case maps:get(Profile, File, undefined) of
        undefined -> {error, {desired_profile_not_found, Profile}};
        Map -> {ok, Map}
    end.

-spec desired_profile(options()) -> binary().
desired_profile(Options) ->
    case {os:getenv("AWS_PROFILE"), maps:get(profile, Options, undefined)} of
        {false, undefined} -> <<"default">>;
        {false, Profile} -> Profile;
        {AwsProfile, undefined} -> list_to_binary(AwsProfile);
        {_, Profile} -> Profile
    end.
