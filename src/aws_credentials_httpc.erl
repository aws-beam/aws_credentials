%% @doc This implements an http client using the Erlang built in http client.
%% If desired, this could be abstracted to support arbitrary http clients
%% that return `{ok, Status :: non_neg_integer(), Body :: binary(), Headers :: map() }' or
%% `{error, Reason :: term()}'.
%% @end
-module(aws_credentials_httpc).
-export([start/0, get/1]).

-define(PROFILE, aws_credentials).
-define(TIMEOUT, 10000). % 10 sec
-define(CONNECT_TIMEOUT, 3000). % 3 sec

start() ->
    inets:start(httpc, [{profile, ?PROFILE}]).


-spec get( URL :: binary() ) -> {ok, { Status :: non_neg_integer(),
                                       Body :: binary(),
                                       Headers :: map() } } | {error, Reason :: term()}.
get(URL) ->
    {ok, {{_HttpVer, Status, _Reason}, Body, Headers}} =
         httpc:request(get, {URL, []},
                       [{timeout, ?TIMEOUT}, {connect_timeout, ?CONNECT_TIMEOUT}], % HTTP options
                       [{body_format, binary}], % options
                       ?PROFILE),
    {ok, Status, Body, maps:from_list(Headers)}.
