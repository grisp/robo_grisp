% @private
% @doc robo_grisp top level supervisor.
-module(robo_grisp_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ROBOT = #{
        id => robo_grisp,
        start => {robo_grisp, start_link, []}
    },
    ChildSpecs = [ROBOT],
    {ok, {SupFlags, ChildSpecs}}.