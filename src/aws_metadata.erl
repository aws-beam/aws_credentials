-module(aws_metadata).

-behaviour(gen_server).

-define(CREDENTIAL_URL,
        <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>).
%% As per
%% http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/iam-roles-for-amazon-ec2.html#instance-metadata-security-credentials
%% We make new credentials available at least five minutes prior to the
%% expiration of the old credentials.
-define(ALERT_BEFORE_EXPIRY, 4 * 60).

-export([init/1
        ,terminate/2
        ,code_change/3
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ]).

-export([start_link/0
        ,stop/0
        ,make_client/0
        ,make_client/1
        ,get_client/1
        ,delete_client/0
        ]).

-record(state, {client_ref = undefined :: reference() | undefined}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the server that holds the credentials.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the server that holds the credentials.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Make a new client from EC2 instance metadata returns a reference
%% that can be passed to get_client/1.  Region and endpoint default to
%% <<"us-east-1-a">> and <<"amazonaws.com">>, respectively.
make_client() ->
    make_client([]).

%% @doc Make a new client from EC2 instance metadata.  Options is a
%% proplist containing region and/or endpoint domain atom/binary pairs.
%% Returns a reference that can be passed to get_client.
make_client(Options) ->
    gen_server:call(?MODULE, {make_client, Options}).

%% @doc Get the client map associated with a reference returned from
%% make_client or undefined if one doesn't exist.
get_client(ClientRef) ->
    case ets:lookup(aws_metadata, ClientRef) of
        [{_Ref, _Type, Client}] -> Client;
        [] -> undefined
    end.

%% @doc Delete all stored client state.
delete_client() ->
    gen_server:call(?MODULE, {delete_client}).

%%====================================================================
%% Behaviour
%%====================================================================

init(_Args) ->
    ets:new(aws_metadata, [set, named_table]),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.

handle_call({make_client, Options}, _From,
            State=#state{client_ref=undefined}) ->
    {ok, Role} = get_role(),
    {ok, AccessKeyID, SecretAccessKey, Expiry} = get_metadata(Role),
    {Region, Endpoint} = parse_options(Options),
    ClientRef = make_ref(),
    true = ets:insert(aws_metadata,
                      {ClientRef, aws_metadata, #{access_key => AccessKeyID,
                                                  secret_key => SecretAccessKey,
                                                  region     => Region,
                                                  endpoint   => Endpoint}}),
    setup_update_callback(Expiry, ClientRef, Role),
    {reply, {ok, ClientRef}, State#state{client_ref=ClientRef}};
%% Fail if a reference already exists.
handle_call({make_client, _}, _From, State=#state{client_ref=ClientRef})
  when is_reference(ClientRef)->
    {reply, {error, already_exists}, State};
handle_call({delete_client}, _From, _State) ->
    ets:delete_all_objects(aws_metadata),
    {reply, ok, #state{}};
handle_call(Args, _From, State) ->
    error_logger:warning_msg("Unknown call: ~p~n", [Args]),
    {noreply, State}.

handle_cast(Message, State) ->
    error_logger:warning_msg("Unknown cast: ~p~n", [Message]),
    {noreply, State}.

handle_info({refresh_client, ClientRef, Role}, State) ->
    {ok, AccessKeyID, SecretAccessKey, Expiry} = get_metadata(Role),
    Client = get_client(ClientRef),
    true = ets:insert(aws_metadata,
                      {ClientRef, Client#{access_key => AccessKeyID,
                                          secret_key => SecretAccessKey}}),
    setup_update_callback(Expiry, ClientRef, Role),
    {noreply, State};
handle_info(Message, State) ->
    error_logger:warning_msg("Unknown message: ~p~n", [Message]),
    {noreply, State}.

code_change(_Prev, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

parse_options(Options) ->
    Region = proplists:get_value(region, Options, <<"us-east-1">>),
    Endpoint = proplists:get_value(endpoint, Options, <<"amazonaws.com">>),
    {Region, Endpoint}.

get_role() ->
    {ok, 200, _, ClientRef} = hackney:get(?CREDENTIAL_URL),
    hackney:body(ClientRef).

get_metadata(Role) ->
    {ok, 200, _, ClientRef} = hackney:get([?CREDENTIAL_URL, Role]),
    {ok, Body} = hackney:body(ClientRef),
    Map = jsx:decode(Body, [return_maps]),
    {ok, maps:get(<<"AccessKeyId">>, Map),
     maps:get(<<"SecretAccessKey">>, Map),
     maps:get(<<"Expiration">>, Map)}.

setup_update_callback(Timestamp, Ref, Role) ->
    AlertAt = seconds_until_timestamp(Timestamp) - ?ALERT_BEFORE_EXPIRY,
    erlang:send_after(AlertAt, ?MODULE, {refresh_client, Ref, Role}).

seconds_until_timestamp(Timestamp) ->
    calendar:datetime_to_gregorian_seconds(iso8601:parse(Timestamp))
    - (erlang_system_seconds()
       + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).

erlang_system_seconds() ->
    try
        erlang:system_time(seconds)
    catch
        error:undef -> % Pre 18.0
            {MegaSecs, Secs, MicroSecs} = os:timestamp(),
            round(((MegaSecs*1000000 + Secs)*1000000 + MicroSecs) / 1000000)
    end.
