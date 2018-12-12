%% @doc This implements an http client using the Erlang built in http client.
%% If desired, this could be abstracted to support arbitrary http clients that
%% return `{ok, Status :: non_neg_integer(), Body :: binary(), Headers :: map()}'
%% or `{error, Reasons :: [term()]}'.
%%
%% On errors, there will be a number of retries with a delay equal to 100
%% milliseconds times the number of retries minus retries left. (So the first
%% delay would be 100 millseconds. The second delay would be 200 milliseconds,
%% and so on...)
%% @end

-module(aws_credentials_httpc).
-export([start/0, get/1, get/2]).

-define(PROFILE, aws_credentials).
-define(TIMEOUT, 10000). % 10 sec
-define(CONNECT_TIMEOUT, 3000). % 3 sec
-define(DEFAULT_RETRIES, 3).
-define(DELAY, 100). % 100 microseconds

start() ->
    inets:start(httpc, [{profile, ?PROFILE}]).

%% @doc Attempt to get a URL with the 3 retries. 3 is the default.
get(URL) ->
  get(URL, ?DEFAULT_RETRIES).

%% @doc Attempt to get a URL with a specified positive number of retries.
%% (Minimum of 1.)
%%
%% Note this function may return `{ok, Results}' even if it was unable to
%% successfully get the desired data. That is, it will return an
%% ok tuple with a status code of 500 or 404 or some other HTTP error
%% code and no data.
-spec get( URL :: binary(),
           Retries :: pos_integer() ) -> {ok, { Status :: non_neg_integer(),
                                                Body :: binary(),
                                                Headers :: map() } }
                                         | {error, Reasons :: [term()]}.
get(URL, Retries) when is_binary(URL)
                       andalso is_integer(Retries)
                       andalso Retries > 0 ->
  get(URL, Retries + 1, Retries, []).

get(_URL, _Base, 0, Errs) -> {error, Errors};
get(URL, Base, Retries, Errors) ->
    case make_request(URL) of
      {ok, {{_HttpVer, Status, _Reason}, Body, Headers}} ->
        {ok, Status, Body, maps:from_list(Headers)};

      Error ->
        error_logger:error_msg("Error fetching URL (retry attempts remaining: ~p) ~p: ~p.",
                               [Retries - 1, URL, Error]),
        timer:sleep((Base - Retries)*?DELAY),
        get(URL, Base, Retries - 1, [ Error | Errs ])
    end.

make_request(URL) ->
    httpc:request(get, {URL, []},
                  [{timeout, ?TIMEOUT},
                   {connect_timeout, ?CONNECT_TIMEOUT}], % HTTP options
                  [{body_format, binary}], % options
                  ?PROFILE).
