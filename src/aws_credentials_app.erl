-module(aws_credentials_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    aws_credentials_httpc:start(),
    aws_credentials_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
