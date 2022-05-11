-module(robo_grisp_wheels).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
% API
-export([
    forward/0,
    backward/0,
    stop/0,
    rotate/1
]).

-record(state, {
    motor_front_right,
    motor_front_left,
    motor_rear_right,
    motor_rear_left
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

backward() -> 
    gen_server:cast(?MODULE, ?FUNCTION_NAME).
forward() -> 
    gen_server:cast(?MODULE, ?FUNCTION_NAME).
stop() -> 
    gen_server:cast(?MODULE, ?FUNCTION_NAME).
rotate(Dir) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Dir}).

init(_) ->
    {ok, #state{     
        motor_front_right = pmod_hb5:open({gpio1, top}),
        motor_front_left = pmod_hb5:open({gpio1, bottom}),
        motor_rear_right = pmod_hb5:open({uart, bottom}),
        motor_rear_left = pmod_hb5:open(i2c)
    }}.

handle_call( _, _, S) ->
    {reply, ok, S}.
% BUG
handle_cast( forward, S) ->
    pmod_hb5:forward(S#state.motor_front_right),
    pmod_hb5:forward(S#state.motor_front_left),
    pmod_hb5:forward(S#state.motor_rear_right),
    pmod_hb5:forward(S#state.motor_rear_left),
    {noreply, S};
handle_cast( backward, S) ->
    pmod_hb5:backward(S#state.motor_front_right),
    pmod_hb5:backward(S#state.motor_front_left),
    pmod_hb5:backward(S#state.motor_rear_right),
    pmod_hb5:backward(S#state.motor_rear_left),
    {noreply, S};
% FIX
% handle_cast( forward, S) ->
%     pmod_hb5:backward(S#state.motor_front_right),
%     pmod_hb5:forward(S#state.motor_front_left),
%     pmod_hb5:backward(S#state.motor_rear_right),
%     pmod_hb5:forward(S#state.motor_rear_left),
%     {noreply, S};
% handle_cast( backward, S) ->
%     pmod_hb5:forward(S#state.motor_front_right),
%     pmod_hb5:backward(S#state.motor_front_left),
%     pmod_hb5:forward(S#state.motor_rear_right),
%     pmod_hb5:backward(S#state.motor_rear_left),
%     {noreply, S};
handle_cast( stop, S) ->
    pmod_hb5:stop(S#state.motor_front_right),
    pmod_hb5:stop(S#state.motor_front_left),
    pmod_hb5:stop(S#state.motor_rear_right),
    pmod_hb5:stop(S#state.motor_rear_left),
    {noreply, S};
handle_cast( {rotate, right}, S) ->
    pmod_hb5:forward(S#state.motor_front_right),
    pmod_hb5:forward(S#state.motor_front_left),
    pmod_hb5:forward(S#state.motor_rear_right),
    pmod_hb5:forward(S#state.motor_rear_left),
{noreply, S};
handle_cast( {rotate, left}, S) ->
    pmod_hb5:backward(S#state.motor_front_right),
    pmod_hb5:backward(S#state.motor_front_left),
    pmod_hb5:backward(S#state.motor_rear_right),
    pmod_hb5:backward(S#state.motor_rear_left),
{noreply, S}.

handle_info( _, S) ->
    {noreply, S}.
