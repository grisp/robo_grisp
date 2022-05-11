-module(robo_grisp).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).



-include_lib("rosie_dds/include/dds_types.hrl").

% -include_lib("geometry_msgs/src/_rosie/geometry_msgs_twist_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_pose_stamped_msg.hrl").

-include_lib("sensor_msgs/src/_rosie/sensor_msgs_temperature_msg.hrl").
-include_lib("sensor_msgs/src/_rosie/sensor_msgs_range_msg.hrl").

-include_lib("tf2_msgs/src/_rosie/tf2_msgs_t_f_message_msg.hrl").

-record(state, {
    ros_node,
    % PUBLISHERS
    % % TF
    tf_static_Pub,
    tf_dynamic_Pub,
    % MARKERS
    static_markers_array,
    acc_pub,
    % SENSORS
    sonar_pub,
    temp_pub
}).

-define(POLLING_PERIOD, 1000).
-define(PUB_PERIOD, 50).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

init(_) ->
    Node = ros_context:create_node(atom_to_list(?MODULE)),

    TF_dinamic_Pub = ros_node:create_publisher(Node, tf2_msgs_t_f_message_msg, "tf", #qos_profile{
        durability = ?TRANSIENT_LOCAL_DURABILITY_QOS}),
    TF_static_Pub = ros_node:create_publisher(Node, tf2_msgs_t_f_message_msg, "tf_static", #qos_profile{
            durability = ?TRANSIENT_LOCAL_DURABILITY_QOS}),

    MarkerArray = ros_node:create_publisher(Node, visualization_msgs_marker_array_msg, "visualization_marker_array"),
    TempPub = ros_node:create_publisher(Node, sensor_msgs_temperature_msg, "temp"),
    SonarPub = ros_node:create_publisher(Node, sensor_msgs_range_msg, "sonar"),
    AccPub = ros_node:create_publisher(Node, visualization_msgs_marker_msg, "acceleration"),

    self() ! update_loop,

    self() ! wait_for_rviz,

    {ok, #state{ ros_node = Node, 
        tf_static_Pub = TF_static_Pub, 
        tf_dynamic_Pub = TF_dinamic_Pub, 
        static_markers_array = MarkerArray,
        sonar_pub = SonarPub,
        temp_pub = TempPub,
        acc_pub = AccPub}}.

handle_call( _, _, S) ->
    {reply, ok, S}.

handle_cast( _, S) ->
    {noreply, S}.

handle_info(wait_for_rviz, #state{tf_static_Pub = TF_static_Pub, acc_pub = AccPub} = S) ->
    case (ros_publisher:get_subscription_count(TF_static_Pub) >= 1) of
        true ->
            ros_publisher:publish(TF_static_Pub, robo_grisp_rviz2:tf_floor_to_map()),
            ros_publisher:publish(TF_static_Pub, robo_grisp_rviz2:tf_head()),

            ros_publisher:publish(AccPub, robo_grisp_rviz2:marker_info(acc));
        false ->
            io:format("Waiting for a remote display...\n"),
            erlang:send_after(?POLLING_PERIOD, self(), wait_for_rviz)
    end,
    {noreply, S};
handle_info( update_loop, #state{ 
    tf_dynamic_Pub = TF_dynamic, 
    sonar_pub = SonarPub,
    static_markers_array = MarkerArray,
    temp_pub = TempPub,
    acc_pub = AccPub}=S
) ->
    Range = robo_grisp_sensors:range(),
    [Ax,Ay,Az] = robo_grisp_sensors:acc(),
    [GRx,GRy,GRz] = robo_grisp_sensors:gyro(),
    [Temp] = robo_grisp_sensors:temp(),
    move_along_range(Range),
    % io:format("range is ~p\n",[Range]),
    % io:format("acc is ~p ~p ~p\n",[Ax,Ay,Az]),
    % io:format("gyro is ~p ~p ~p\n",[GRx,GRy,GRz] ),
    % io:format("temp is ~p\n",[Temp]),
    RangeMsg = #sensor_msgs_range{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_head"
        },
        radiation_type=0,
        field_of_view = 0.40,
        min_range = 6.0,
        max_range = 255.0,
        range = Range
    },
    case Range of
        undefined -> nothing;
        _ -> ros_publisher:publish(SonarPub, RangeMsg)
    end,

    TempMsg = #sensor_msgs_temperature{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_frame"
        },
        temperature = Temp
    },
    ros_publisher:publish(TempPub, TempMsg),

    TF = robo_grisp_rviz2:tf_robot_to_floor(
        {#geometry_msgs_vector3{x = 0, y = 0, z = 2.0}, 0, 
        robo_grisp_rviz2:quaternion_from_euler(-GRy/10,-GRx/10,GRz/10)}),
    ros_publisher:publish(TF_dynamic, TF),

    AccMsg = robo_grisp_rviz2:marker_update( 
        acc, 
        #geometry_msgs_point{x = Ax/500.0, y = Ay/500.0, z = Az / 500.0}, 
        #geometry_msgs_quaternion{}),
    ros_publisher:publish(AccPub, AccMsg),
    ros_publisher:publish(MarkerArray, robo_grisp_rviz2:marker_array([
        robo_grisp_rviz2:marker_info(base)])),
    ros_publisher:publish(MarkerArray, robo_grisp_rviz2:marker_array([
        robo_grisp_rviz2:marker_info(head)])),

    erlang:send_after(?PUB_PERIOD, self(), update_loop),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    grisp_led:off(1),
    grisp_led:off(2),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    robo_grisp_wheels:rotate(left),
    timer:sleep(1000),
    robo_grisp_wheels:rotate(right),
    timer:sleep(1000),
    robo_grisp_wheels:stop(),
    grisp_led:off(2),
    Random = fun() ->
        {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    end,
    grisp_led:pattern(1, [{100, Random}]),
    {ok, State}.

% Internals -------------------------------------


move_along_range(Range) when Range =< 20.0 ->
    robo_grisp_wheels:backward();
move_along_range(Range) when Range >= 60.0 ->
    robo_grisp_wheels:forward();
move_along_range(_) ->
    robo_grisp_wheels:stop().
