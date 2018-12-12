%% @doc This implements an http client using the Erlang built in http client.
%% If desired, this could be abstracted to support arbitrary http clients
%% that return `{ok, Status :: non_neg_integer(), Body :: binary(), Headers :: map() }' or
%% `{error, Reason :: term()}'.
%% @end
-module(aws_credentials_httpc).
-export([start/0, get/1, get/2, get/3]).

-define(PROFILE, aws_credentials).
-define(TIMEOUT, 10000). % 10 sec
-define(CONNECT_TIMEOUT, 3000). % 3 sec
-define(DEFAULT_RETRIES, 3).
-define(DELAY, 100). % 100 microseconds

start() ->
    inets:start(httpc, [{profile, ?PROFILE}]).

get(URL) ->
  get(URL, ?DEFAULT_RETRIES).

get(URL, Retries) ->
  get(URL, Retries + 1, Retries).

-spec get( URL :: binary(),
           Base :: pos_integer(),
           Retries :: pos_integer() ) -> {ok, { Status :: non_neg_integer(),
                                                Body :: binary(),
                                                Headers :: map() } } | {error, Reason :: term()}.
get(_URL, _Base, 0) -> {error, no_retries};
get(URL, Base, Retries) when is_integer(Retries)
                        andalso Retries > 0 ->
    case make_request(URL) of
      {ok, {{_HttpVer, Status, _Reason}, Body, Headers}} -> {ok, Status, Body, maps:from_list(Headers)};
      Error -> 
        error_logger:error_msg("Error fetching URL (retry attempts remaining: ~p) ~p: ~p.",
                               [Retries - 1, URL, Error]),
        timer:sleep((Base - Retries)*?DELAY),
        get(URL, Base, Retries - 1)
    end.

make_request(URL) ->
         httpc:request(get, {URL, []},
                       [{timeout, ?TIMEOUT}, {connect_timeout, ?CONNECT_TIMEOUT}], % HTTP options
                       [{body_format, binary}], % options
                       ?PROFILE).
