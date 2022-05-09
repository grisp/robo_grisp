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
    Maxsonar = #{
        id => pmod_maxsonar,
        start => {pmod_maxsonar, start_link, [uart,[]]}
    },
    Nav = #{
        id => pmod_nav,
        start => {pmod_nav, start_link, [spi2, #{}]}
    },
    Wheels = #{
        id => robo_grisp_wheels,
        start => {robo_grisp_wheels, start_link, []}
    },
    Sensors = #{
        id => robo_grisp_sensors,
        start => {robo_grisp_sensors, start_link, []}
    },
    Robot = #{
        id => robo_grisp,
        start => {robo_grisp, start_link, []}
    },
    ChildSpecs = [Maxsonar, Nav, Wheels, Sensors, Robot],
    {ok, {SupFlags, ChildSpecs}}.