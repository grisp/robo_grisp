-module(robo_grisp).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("rosie_dds/include/dds_types.hrl").

-include_lib("geometry_msgs/src/_rosie/geometry_msgs_twist_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_pose_stamped_msg.hrl").

-include_lib("tf2_msgs/src/_rosie/tf2_msgs_t_f_message_msg.hrl").
-include_lib("visualization_msgs/src/_rosie/visualization_msgs_marker_msg.hrl").

-record(state, {
    ros_node
}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, #state{}, []).

init(_) ->
    Node = ros_context:create_node(atom_to_list(?MODULE)),

    {ok, #state{ ros_node = Node}}.

handle_call( _, _, S) ->
    {reply, ok, S}.

handle_cast( _, S) ->
    {noreply, S}.

handle_info( _, S) ->
    {noreply, S}.
