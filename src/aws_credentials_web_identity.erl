%% @doc This provider fetches the credentials with
%% <a href="https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html">AssumeRoleWithWebIdentity</a>
%% API.
%% By default, this module uses the Amazon CLI tools to call the API.
%% This behavior can be changed by providing `assume_role_with_web_identity' callback by options.
%% For example, some module may resolve web identity token by aws-erlang.
%% (aws-erlang is much larger library than aws_credentials, so this module does not use it by default.)
%%
%% Environment parameters:
%% <ul>
%%   <li> &lt;&lt;"region"&gt;&gt; - this is the region to be used.
%%   The region must be provided via this option or os env "AWS_REGION" or "AWS_DEFAULT_REGION".</li>
%%   <li> &lt;&lt;"role_arn"&gt;&gt; - this is the RoleArn input for AssumeRoleWithWebIdentity API.
%%   The role ARN must be provided via this option or os env "AWS_ROLE_ARN".</li>
%%   <li> &lt;&lt;"role_session_name"&gt;&gt; - this is the RoleSessionName input for AssumeRoleWithWebIdentity API.
%%   The role session name can be optionally provided via this option or os env "AWS_ROLE_SESSION_NAME".</li>
%%   <li> &lt;&lt;"web_identity_token_file"&gt;&gt; - this is the file name which contains web identity token.
%%   The file name must be provided via this option or os env "AWS_WEB_IDENTITY_TOKEN_FILE".</li>
%%   <li> &lt;&lt;"web_identity_token_module"&gt;&gt; - this is the module which actually fetches credentials.
%%   If this option is absent, default callback is used, which uses aws cli command.</li>
%%   <li> &lt;&lt;"web_identity_token_module_option"&gt;&gt; - this is the option for web_identity_token_module.
%%   The option can be optionally provided via this option.
%%   For the default callback, &lt;&lt;"aws_cli_command"&gt;&gt; option is available for the aws cli command.</li>
%% </ul>
%% @end
-module(aws_credentials_web_identity).
-behaviour(aws_credentials_provider).

-export([fetch/1, assume_role_with_web_identity/5]).

-type region() :: binary().
-type role_arn() :: binary().
-type role_session_name() :: binary().
-type web_identity_token_file() :: binary() | string().
-type web_identity_token() :: binary().
-export_type([region/0, role_arn/0, role_session_name/0, web_identity_token/0]).

-callback assume_role_with_web_identity(
        region(), role_arn(), role_session_name(), web_identity_token(), map()) ->
    {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()} | {error, any()}.

-define(COMMAND_MAX_OUTPUT,
    1048576).
-define(COMMAND_TIMEOUT,
    5000).
-define(AWS_CLI_COMMAND,
    <<"aws">>).

-spec fetch(aws_credentials_provider:options()) ->
    {error, any()} | {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()}.
fetch(Options) ->
    try
        {ok, Region} = get_region(Options),
        {ok, RoleArn} = get_role_arn(Options),
        {ok, RoleSessionName} = get_role_session_name(Options),
        {ok, TokenFile} = get_token_file(Options),
        {ok, Token} = load_token_file(TokenFile),
        Module = maps:get(web_identity_token_module, Options, ?MODULE),
        ModuleOptions = maps:get(web_identity_token_module_options, Options, #{}),
        Module:assume_role_with_web_identity(Region, RoleArn, RoleSessionName, Token, ModuleOptions)
    catch
        error:{badmatch, {error, Reason}} -> {error, Reason}
    end.

-spec get_region(aws_credentials_provider:options()) -> {error, any()} | {ok, region()}.
get_region(Options) ->
    RegionByOptions = maps:get(region, Options, undefined),
    RegionByEnv = os:getenv("AWS_REGION"),
    DefaultRegionByEnv = os:getenv("AWS_DEFAULT_REGION"),
    case {RegionByOptions, RegionByEnv, DefaultRegionByEnv} of
        _ when is_binary(RegionByOptions) -> {ok, RegionByOptions};
        _ when is_list(RegionByEnv) -> {ok, list_to_binary(RegionByEnv)};
        _ when is_list(DefaultRegionByEnv) -> {ok, list_to_binary(DefaultRegionByEnv)};
        _ -> {error, no_region}
    end.

-spec get_role_arn(aws_credentials_provider:options()) -> {error, any()} | {ok, role_arn()}.
get_role_arn(Options) ->
    RoleArnByOptions = maps:get(role_arn, Options, undefined),
    RoleArnByEnv = os:getenv("AWS_ROLE_ARN"),
    case {RoleArnByOptions, RoleArnByEnv} of
        _ when is_binary(RoleArnByOptions) -> {ok, RoleArnByOptions};
        _ when is_list(RoleArnByEnv) -> {ok, list_to_binary(RoleArnByEnv)};
        _ -> {error, no_role_arn}
    end.

-spec get_role_session_name(aws_credentials_provider:options()) -> {ok, role_session_name()}.
get_role_session_name(Options) ->
    RoleSessionNameByOptions = maps:get(role_session_name, Options, undefined),
    RoleSessionNameByEnv = os:getenv("AWS_ROLE_SESSION_NAME"),
    case {RoleSessionNameByOptions, RoleSessionNameByEnv} of
        _ when is_binary(RoleSessionNameByOptions) -> {ok, RoleSessionNameByOptions};
        _ when is_list(RoleSessionNameByEnv) -> {ok, list_to_binary(RoleSessionNameByEnv)};
        _ ->
            %% session name is used to uniquely identify a session.
            %% So simply use unix time in nanoseconds.
            {ok, integer_to_binary(erlang:system_time(nanosecond))}
    end.

-spec get_token_file(aws_credentials_provider:options()) ->
    {error, any()} | {ok, web_identity_token_file()}.
get_token_file(Options) ->
    TokenFileByOptions = maps:get(web_identity_token_file, Options, undefined),
    TokenFileByEnv = os:getenv("AWS_WEB_IDENTITY_TOKEN_FILE"),
    case {TokenFileByOptions, TokenFileByEnv} of
        _ when is_binary(TokenFileByOptions) -> {ok, TokenFileByOptions};
        _ when is_list(TokenFileByEnv) -> {ok, TokenFileByEnv};
        _ -> {error, no_web_identity_token_file}
    end.

-spec load_token_file(web_identity_token_file()) -> {error, any()} | {ok, web_identity_token()}.
load_token_file(TokenFile) ->
    case file:read_file(TokenFile) of
        {ok, Data} ->
            [Token | _] = binary:split(Data, <<"\n">>),
            {ok, Token};
        {error, Reason} ->
            {error, {failed_to_read_web_identity_token_file, Reason}}
    end.

%% default implementation of assume_role_with_web_identity callback
-spec assume_role_with_web_identity(
        region(), role_arn(), role_session_name(), web_identity_token(), map()) ->
    {ok, aws_credentials:credentials(), aws_credentials_provider:expiration()} | {error, any()}.
assume_role_with_web_identity(Region, RoleArn, RoleSessionName, WebIdentityToken, Options) ->
    Result = do_aws_cli([<<"sts assume-role-with-web-identity">>,
                         <<" --region ">>, Region,
                         <<" --role-arn ">>, RoleArn,
                         <<" --role-session-name ">>, RoleSessionName,
                         <<" --web-identity-token ">>, WebIdentityToken
                        ], Options),
    case Result of
        {ok, 0, Output} ->
            OutputMap = jsx:decode(Output),
            CredentialsMap = maps:get(<<"Credentials">>, OutputMap),
            AccessKeyId = maps:get(<<"AccessKeyId">>, CredentialsMap),
            SecretAccessKey = maps:get(<<"SecretAccessKey">>, CredentialsMap),
            Token = maps:get(<<"SessionToken">>, CredentialsMap),
            Expiration = maps:get(<<"Expiration">>, CredentialsMap),
            C = aws_credentials:make_map(?MODULE, AccessKeyId, SecretAccessKey, Token, Region),
            {ok, C, Expiration};
        {ok, StatusCode, Output} ->
            {error, {aws_cli_failed, StatusCode, Output}};
        Error ->
            Error
    end.

-spec aws_cli_command(map()) -> binary().
aws_cli_command(Options) ->
    CommandByOptions = maps:get(aws_cli_command, Options, undefined),
    CommandByEnv = os:getenv("AWS_CLI_COMMAND"),
    case {CommandByOptions, CommandByEnv} of
        _ when is_binary(CommandByOptions) -> CommandByOptions;
        _ when is_list(CommandByEnv) -> list_to_binary(CommandByEnv);
        _ -> ?AWS_CLI_COMMAND
    end.

-spec do_aws_cli(iodata(), map()) ->
    {error, any()}
    | {ok, non_neg_integer(), binary()}.
do_aws_cli(Subcommand, Options) ->
    AwsCliCommand = aws_cli_command(Options),
    CommandLine = iolist_to_binary([AwsCliCommand, <<" ">>, Subcommand]),
    Port = open_port({spawn, CommandLine}, [stream, use_stdio, binary, exit_status]),
    do_aws_cli_loop(Port, []).

-spec do_aws_cli_loop(port(), [binary()]) -> {error, any()} | {ok, non_neg_integer(), binary()}.
do_aws_cli_loop(Port, Data) ->
    receive
        {Port, {data, NewData}} ->
            ConcatData = [Data, NewData],
            case erlang:external_size(ConcatData) > ?COMMAND_MAX_OUTPUT of
                true ->
                    do_aws_cli_close(Port),
                    {error, output_size_exceeded};
                false ->
                    do_aws_cli_loop(Port, NewData)
            end;
        {Port, {exit_status, Status}} ->
            do_aws_cli_close(Port),
            {ok, Status, iolist_to_binary(Data)}
    after ?COMMAND_TIMEOUT ->
        do_aws_cli_close(Port),
        {error, timeout}
    end.

-spec do_aws_cli_close(port()) -> ok.
do_aws_cli_close(Port) ->
    catch port_close(Port),
    ok.
