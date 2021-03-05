-module(aws_credentials_app).

-behaviour(application).

%% Application callbacks
-export([ start/2
        , stop/1
        ]).

%%====================================================================
%% API
%%====================================================================

-spec start( 'normal'
           | {takeover, Node :: node()}
           | {failover, Node :: node()}, any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    aws_credentials_httpc:start(),
    aws_credentials_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(any()) -> 'ok'.
stop(_State) ->
    ok.
