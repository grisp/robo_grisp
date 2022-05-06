-module(robo_grisp_app).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->

    {ok, Supervisor} = robo_grisp_sup:start_link(),

    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    timer:sleep(5000),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    grisp_led:pattern(1, [{100, Random}]),


    % M_front_right = pmod_hb5:open(gpio1, first),
    % M_front_left = pmod_hb5:open(gpio1, second),
    % M_back_right = pmod_hb5:open(uart, second),
    % M_back_left = pmod_hb5:open(i2c),

    % pmod_hb5:forward(M_front_right),
    % pmod_hb5:forward(M_front_left),

    % pmod_hb5:forward(M_back_right),
    % pmod_hb5:forward(M_back_left),


    {ok, Supervisor}.

stop(_State) -> ok.