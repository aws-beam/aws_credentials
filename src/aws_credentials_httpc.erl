%% @doc This implements an http client using the Erlang built in http client.
%% If desired, this could be abstracted to support arbitrary http clients that
%% return `{ok, Status :: non_neg_integer(), Body :: binary(), Headers :: map()}'
%% or `{error, Reasons :: [term()]}'.
%%
%% On errors, there will be a number of attempts with a delay equal to 100
%% milliseconds times the number of tries minus tries left. (So the first
%% delay would be 100 milliseconds. The second delay would be 200 milliseconds,
%% and so on...)
%% @end

-module(aws_credentials_httpc).
-export([start/0, get/1, get/2]).

-define(PROFILE, aws_credentials).
-define(TIMEOUT, 10000). % 10 sec
-define(CONNECT_TIMEOUT, 3000). % 3 sec
-define(DEFAULT_TRIES, 3).
-define(DELAY, 100). % 100 milliseconds

-type httpc_result() :: {status_line(), [header()], body()}.
-type status_line() :: {http_version(), status_code(), reason_phrase()}.
-type header() :: {string(), string()}.
-type body() :: binary().
-type http_version() :: string().
-type status_code() :: non_neg_integer().
-type reason_phrase() :: string().
-type url() :: string().

-spec start() -> ok.

-include_lib("kernel/include/logger.hrl").

start() ->
    inets:start(httpc, [{profile, ?PROFILE}]).

%% @doc Attempt to get a URL with the 3 retries. 3 is the default.
-spec get(url()) -> {'error', any()} | {'ok', status_code(), body(), [header()]}.
get(URL) ->
  get(URL, ?DEFAULT_TRIES).

%% @doc Attempt to get a URL with a specified positive number of retries.
%% (Minimum of 1.)
%%
%% Note this function may return `{ok, Results}' even if it was unable to
%% successfully get the desired data. That is, it will return an
%% ok tuple with a status code of 500 or 404 or some other HTTP error
%% code and no data.
-spec get(url(), pos_integer() ) -> {ok, status_code(), body(), [header()]} | {error, any()}.
get(URL, Tries) when is_list(URL)
                     andalso is_integer(Tries)
                     andalso Tries > 0 ->
  get(URL, Tries, Tries, []).

-spec get(url(), pos_integer(), pos_integer(), any()) -> {ok, status_code(), body(), [header()]} | {error, any()}.
get(_URL, _Tries, 0, Errs) -> {error, lists:reverse(Errs)};
get(URL, Tries, Remaining, Errs) ->
    case make_request(URL) of
      {ok, {{_HttpVer, Status, _Reason}, Headers, Body}} ->
        {ok, Status, Body, Headers};

      Error ->
        NewRemaining = Remaining - 1,
        ?LOG_ERROR("Error fetching URL (attempts left: "
                   "~p of ~p) ~p: ~p.",
                   [NewRemaining, Tries, URL, Error]),
        timer:sleep((Tries - NewRemaining)*?DELAY),
        get(URL, Tries, NewRemaining, [ Error | Errs ])
    end.

-spec make_request(string()) -> {ok, httpc_result()} | {error, any()}.
make_request(URL) ->
    httpc:request(get, {URL, []},
                  [{timeout, ?TIMEOUT},
                   {connect_timeout, ?CONNECT_TIMEOUT}], % HTTP options
                  [{body_format, binary}], % options
                  ?PROFILE).
