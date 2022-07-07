%% @doc This provider reads the credentials file for the amazon
%% CLI tools. <a href="https://docs.aws.amazon.com/cli/latest/userguide/cli-config-files.html">
%% This format is documented here</a> but an example might look
%% like:
%%
%% <pre>
%% [default]
%% aws_access_key_id=AKIAIOSFODNN7EXAMPLE
%% aws_secret_access_key=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
%% </pre>
%%
%% Environment parameters:
%% <ul>
%%   <li> &lt;&lt;"credentials_path"&gt;&gt; - this is the path to the CLI configuration file.
%%   By default this is `~/.aws/credentials'</li>
%%   <li> &lt;&lt;"profile"&gt;&gt; - this is the desired profile to use in the credentials file.
%%   By default this is &lt;&lt;"default"&gt;&gt;</li>
%% </ul>
%% @end
-module(aws_credentials_file).
-behaviour(aws_credentials_provider).

-export([fetch/1]).

-spec fetch(aws_credentials_provider:options()) ->
        {error, any()} | {ok, aws_credentials:credentials(), 'infinity'}.
fetch(Options) ->
    FilePath = get_file_path(Options),
    ConfigPath = does_credentials_file_exist(FilePath, "config"),
    case does_credentials_file_exist(FilePath, "credentials") of
        {error, Error} ->
            {error, {credentials_file_does_not_exist, Error}};
        CredentialsPath ->
            CredFile = parse_credentials_file(CredentialsPath, Options),
            maybe_add_region(CredFile, ConfigPath, Options)
    end.

-spec does_credentials_file_exist(string(), string()) -> {error, any()} | string().
does_credentials_file_exist({error, _} = Error, _FileName) -> Error;
does_credentials_file_exist(Path, FileName) ->
    check_path_exists(Path ++ FileName).

-spec get_file_path(aws_credentials_provider:options()) -> {error, any()} | string().
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
        aws_credentials_provider:options()
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

-spec parse_credentials_file(string(), aws_credentials_provider:options()) ->
        {error, any()} | {ok, aws_credentials:credentials(), 'infinity'}.
parse_credentials_file(Path, Options) ->
    {ok, F} = file:read_file(Path),
    {ok, Profiles} = eini:parse(F),
    Desired = maps:get(profile, Options, <<"default">>),

    case maps:get(Desired, Profiles, false) of
        false -> {error, {desired_profile_not_found, Desired}};
        Profile ->
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

-spec parse_config_file(string(), aws_credentials_provider:options()) ->
        {error, any()} | {ok, aws_credentials:credentials()}.
parse_config_file(Path, Options) ->
    {ok, F} = file:read_file(Path),
    {ok, Profiles} = eini:parse(F),
    Desired = maps:get(profile, Options, <<"default">>),

    case maps:get(Desired, Profiles, false) of
        false -> {error, {desired_profile_not_found, Desired}};
        Profile -> {ok, Profile}
    end.
