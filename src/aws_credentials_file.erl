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
%%   <li><<"credentials_path">> - this is the path to the CLI configuration file. By default this is `~/.aws/credentials'</li>
%%   <li><<"profile">> - this is the desired profile to use in the credentials file. By default this is <<"default">></li>
%% </ul>
%% @end
-module(aws_credentials_file).
-behaviour(aws_credentials_provider).

-export([fetch/1]).

fetch(Options) ->
    case does_credentials_file_exist(Options) of
        {false, Error} ->
            {error, {credentials_file_does_not_exist, Error}};
        Path ->
            parse_file(Path, Options)
    end.

does_credentials_file_exist(Options) ->
    case maybe_add_home(proplists:get_value(<<"credential_path">>, Options, "/.aws/credentials")) of
        {error, _} = Error -> Error;
        Path -> check_path_exists(Path, Options)
    end.


maybe_add_home("/.aws/credentials") ->
    case os:getenv("HOME") of
        false -> {error, could_not_get_home_directory_from_os_environment};
        Home -> Home ++ "/.aws/credentials"
    end;
maybe_add_home(Other) -> Other.

check_path_exists(Path, _Options) ->
        case filelib:is_regular(Path) of
            false -> false;
            true -> Path
        end.

parse_file(Path, Options) ->
    {ok, F} = file:read_file(Path),
    {ok, Profiles} = eini:parse(F),
    Desired = proplists:get_value(<<"profile">>, Options, <<"default">>),

    case maps:get(Desired, Profiles, false) of
        false -> {error, {desired_profile_not_found, Desired}};
        Profile ->
            {ok, aws_credentials:make_map(?MODULE,
                                          maps:get(<<"aws_access_key_id">>, Profile),
                                          maps:get(<<"aws_secret_access_key">>, Profile)), infinity}
    end.
