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
-export([start/0, request/2, request/3, request/4]).

-define(PROFILE, aws_credentials).
-define(TIMEOUT, 10000). % 10 sec
-define(CONNECT_TIMEOUT, 3000). % 3 sec
-define(DEFAULT_TRIES, 3).
-define(DELAY, 100). % 100 milliseconds
-define(DEFAULT_HEADERS, []).

-type httpc_result() :: {status_line(), [header()], body()}.
-type status_line() :: {http_version(), status_code(), reason_phrase()}.
-type header() :: {string(), string()}.
-type body() :: binary().
-type http_version() :: string().
-type status_code() :: non_neg_integer().
-type reason_phrase() :: string().
-type url() :: string().
-type method() :: atom().

-spec start() -> ok.

-include_lib("kernel/include/logger.hrl").

start() ->
    inets:start(httpc, [{profile, ?PROFILE}]).

%% @doc Attempt to request a URL with the 3 retries. 3 is the default.
-spec request(method(), url()) -> {'error', any()} | {'ok', status_code(), body(), [header()]}.
request(Method, URL) ->
  request(Method, URL, ?DEFAULT_HEADERS, ?DEFAULT_TRIES).

-spec request(method(), url(), [header()]) -> {'error', any()} | {'ok', status_code(), body(), [header()]}.
request(Method, URL, RequestHeaders) ->
  request(Method, URL, RequestHeaders, ?DEFAULT_TRIES).


%% @doc Attempt to request a URL with a specified positive number of retries.
%% (Minimum of 1.)
%%
%% Note this function may return `{ok, Results}' even if it was unable to
%% successfully get the desired data. That is, it will return an
%% ok tuple with a status code of 500 or 404 or some other HTTP error
%% code and no data.
-spec request(method(), url(), [header()], pos_integer() ) -> {ok, status_code(), body(), [header()]} | {error, any()}.
request(Method, URL, RequestHeaders, Tries) when is_atom(Method)
                     andalso is_list(URL)
                     andalso is_integer(Tries)
                     andalso Tries > 0 ->
  request(Method, URL, RequestHeaders, Tries, Tries, []).

-spec request(method(), url(), [header()], pos_integer(), pos_integer(), any()) ->
        {ok, status_code(), body(), [header()]} | {error, any()}.
request(_Method, _URL, _RequestHeaders, _Tries, 0, Errs) -> {error, lists:reverse(Errs)};
request(Method, URL, RequestHeaders, Tries, Remaining, Errs) ->
    case make_request(Method, URL, RequestHeaders) of
      {ok, {{_HttpVer, Status, _Reason}, ResponseHeaders, Body}} ->
        {ok, Status, Body, ResponseHeaders};

      Error ->
        NewRemaining = Remaining - 1,
        ?LOG_ERROR("Error fetching URL (attempts left: "
                   "~p of ~p) ~p: ~p.",
                   [NewRemaining, Tries, URL, Error]),
        timer:sleep((Tries - NewRemaining) * ?DELAY),
        request(Method, URL, RequestHeaders, Tries, NewRemaining, [ Error | Errs ])
    end.

-spec make_request(method(), url(), [header()]) -> {ok, httpc_result()} | {error, any()}.
make_request(Method, URL, RequestHeaders) ->
    httpc:request(Method, {URL, RequestHeaders},
                  [{timeout, ?TIMEOUT},
                   {connect_timeout, ?CONNECT_TIMEOUT}], % HTTP options
                  [{body_format, binary}], % options
                  ?PROFILE).