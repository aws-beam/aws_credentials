-module(aws_credentials_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => one_for_all
              , intensity => 0
              , period    => 1
              },
  ChildSpecs = [#{ id       => aws_credentials
                 , start    => {aws_credentials, start_link, []}
                 , restart  => permanent
                 , shutdown => 5000
                 , type     => worker
                 , modules  => [aws_credentials]
                 }],
  {ok, {SupFlags, ChildSpecs}}.
