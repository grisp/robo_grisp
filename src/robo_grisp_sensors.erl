-module(robo_grisp_sensors).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([range/0, acc/0, gyro/0, temp/0]).

-record(state, {
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

range() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME). 
acc() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME). 
gyro() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME). 
temp() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME). 
% CALLBACKS ------------------------------------------------

init(_) ->
    {ok, #state{}}.

handle_call( range, _, S) ->
    case pmod_maxsonar:get() of
        undefined -> {reply, undefined, S};
        Range -> {reply, float(Range), S}
    end;
handle_call( acc, _, S) ->
    Acc = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl], #{xl_unit => mg}),
    {reply, Acc, S};
handle_call( gyro, _, S) ->
    G = pmod_nav:read( acc, [out_x_g, out_y_g, out_z_g]),
    {reply, G, S};
handle_call( temp, _, S) ->
    T = pmod_nav:read( alt, [temp_out]),
    {reply, T, S}.

handle_cast( _, S) ->
    {noreply, S}.

handle_info( _, S) ->
    {noreply, S}.
