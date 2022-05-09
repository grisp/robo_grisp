-module(robo_grisp).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("rosie_dds/include/dds_types.hrl").

% -include_lib("geometry_msgs/src/_rosie/geometry_msgs_twist_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_pose_stamped_msg.hrl").

-include_lib("sensor_msgs/src/_rosie/sensor_msgs_temperature_msg.hrl").
-include_lib("sensor_msgs/src/_rosie/sensor_msgs_range_msg.hrl").

-include_lib("tf2_msgs/src/_rosie/tf2_msgs_t_f_message_msg.hrl").
-include_lib("visualization_msgs/src/_rosie/visualization_msgs_marker_msg.hrl").

-record(state, {
    ros_node,
    tf_static_Pub,
    tf_dynamic_Pub,
    head_pub,
    sonar_pub,
    temp_pub,
    acc_pub
}).

-define(POLLING_PERIOD, 1000).
-define(PUB_PERIOD, 100).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

init(_) ->
    Node = ros_context:create_node(atom_to_list(?MODULE)),

    TF_dinamic_Pub = ros_node:create_publisher(Node, tf2_msgs_t_f_message_msg, "tf", #qos_profile{
        durability = ?TRANSIENT_LOCAL_DURABILITY_QOS}),
    TF_static_Pub = ros_node:create_publisher(Node, tf2_msgs_t_f_message_msg, "tf_static", #qos_profile{
            durability = ?TRANSIENT_LOCAL_DURABILITY_QOS}),
    HeadMarkerPub = ros_node:create_publisher(Node, visualization_msgs_marker_msg, "visualization_marker"),
    TempPub = ros_node:create_publisher(Node, sensor_msgs_temperature_msg, "temp"),
    SonarPub = ros_node:create_publisher(Node, sensor_msgs_range_msg, "sonar"),

    AccPub = ros_node:create_publisher(Node, visualization_msgs_marker_msg, "acceleration"),

    self() ! update_loop,

    self() ! wait_for_rviz,

    {ok, #state{ ros_node = Node, 
        tf_static_Pub = TF_static_Pub, 
        tf_dynamic_Pub = TF_dinamic_Pub, 
        head_pub = HeadMarkerPub,
        sonar_pub = SonarPub,
        temp_pub = TempPub,
        acc_pub = AccPub}}.

handle_call( _, _, S) ->
    {reply, ok, S}.

handle_cast( _, S) ->
    {noreply, S}.

handle_info(wait_for_rviz, #state{tf_static_Pub = TF_static_Pub, head_pub = HeadMarkerPub, acc_pub = AccPub} = S) ->
    case (ros_publisher:get_subscription_count(TF_static_Pub) >= 1) of
        true ->
            ros_publisher:publish(TF_static_Pub, tf_floor_to_map()),
            ros_publisher:publish(TF_static_Pub, tf_head()),
            ros_publisher:publish(HeadMarkerPub, marker_info(head)),
            ros_publisher:publish(AccPub, marker_info(acc));
        false ->
            io:format("Waiting for a remote display...\n"),
            erlang:send_after(?POLLING_PERIOD, self(), wait_for_rviz)
    end,
    {noreply, S};
handle_info( update_loop, #state{ 
    tf_dynamic_Pub = TF_dynamic, 
    sonar_pub = SonarPub,
    temp_pub = TempPub,
    acc_pub = AccPub}=S
) ->
    Range = robo_grisp_sensors:range(),
    [Ax,Ay,Az] = robo_grisp_sensors:acc(),
    [GRx,GRy,GRz] = robo_grisp_sensors:gyro(),
    [Temp] = robo_grisp_sensors:temp(),
    move_along_range(Range),
    % io:format("range is ~p\n",[Range]),
    io:format("acc is ~p ~p ~p\n",[Ax,Ay,Az]),
    io:format("gyro is ~p ~p ~p\n",[GRx,GRy,GRz] ),
    io:format("temp is ~p\n",[Temp]),
    RangeMsg = #sensor_msgs_range{
        header = #std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = "robot_head"
        },
        radiation_type=0,
        field_of_view = 0.50, % ~60°
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

    TF = tf_robot_to_floor(
        {#geometry_msgs_vector3{x = 0, y = 0, z = 2.0}, 0, 
        quaternion_from_euler(-GRy,-GRx,GRz)}),
    ros_publisher:publish(TF_dynamic, TF),


    

    AccMsg = marker_info( {#geometry_msgs_point{x = Ax/500.0, y = Ay/500.0, z = Az / 500.0}, 0, #geometry_msgs_quaternion{w=1.0}}, "robot_frame", ?SPHERE, ?MODIFY),
    ros_publisher:publish(AccPub, AccMsg),

    erlang:send_after(100, self(), update_loop),
    {noreply, S}.

% Internals -------------------------------------



tf_head() ->
    #tf2_msgs_t_f_message{
        transforms = [
            #geometry_msgs_transform_stamped{
                header = #std_msgs_header{
                    stamp = #builtin_interfaces_time{},
                    frame_id = "robot_frame"
                },
                child_frame_id = "robot_head",
                transform = #geometry_msgs_transform{
                    translation = #geometry_msgs_vector3{x = 4.0, y = 0.0, z = 2.0},
                    rotation = quaternion_from_euler(0.0, -15.0, 0.0)
                }
            }
        ]
    }.
tf_robot_to_floor({#geometry_msgs_vector3{x = _X, y = _Y, z = _Z} = Pos, _Speed, Orientation}) ->
    #tf2_msgs_t_f_message{
        transforms = [
            #geometry_msgs_transform_stamped{
                header = #std_msgs_header{
                    stamp = #builtin_interfaces_time{},
                    frame_id = "floor"
                },
                child_frame_id = "robot_frame",
                transform = #geometry_msgs_transform{
                    translation = Pos,
                    rotation = Orientation
                }
            }
        ]
    }.
tf_floor_to_map() ->
    #tf2_msgs_t_f_message{
        transforms = [
            #geometry_msgs_transform_stamped{
                header = #std_msgs_header{
                    stamp = #builtin_interfaces_time{},
                    frame_id = "map"
                },
                child_frame_id = "floor"
            }
        ]
    }.
quaternion_from_euler(RollDeg, PitchDeg, YawDeg) ->
    Roll = RollDeg * math:pi() / 180.0,
    Pitch = PitchDeg * math:pi() / 180.0,
    Yaw = YawDeg * math:pi() / 180.0,
    % Convert an Euler angle to a Quaternion.
    %
    % Input
    %   :param roll: The roll (rotation around x-axis) angle in radians.
    %   :param pitch: The pitch (rotation around Y-axis) angle in radians.
    %   :param Yaw: The Yaw (rotation around z-axis) angle in radians.
    %
    % Output
    %   :return Qx, QY, Qz, Qw: The orientation in Quaternion [x,Y,z,w] format
    Qx =
        math:sin(Roll / 2) * math:cos(Pitch / 2) * math:cos(Yaw / 2) -
            math:cos(Roll / 2) * math:sin(Pitch / 2) * math:sin(Yaw / 2),
    Qy =
        math:cos(Roll / 2) * math:sin(Pitch / 2) * math:cos(Yaw / 2) +
            math:sin(Roll / 2) * math:cos(Pitch / 2) * math:sin(Yaw / 2),
    Qz =
        math:cos(Roll / 2) * math:cos(Pitch / 2) * math:sin(Yaw / 2) -
            math:sin(Roll / 2) * math:sin(Pitch / 2) * math:cos(Yaw / 2),
    Qw = math:cos(Roll / 2) * math:cos(Pitch / 2) * math:cos(Yaw / 2) +
            math:sin(Roll / 2) * math:sin(Pitch / 2) * math:sin(Yaw / 2),
    #geometry_msgs_quaternion{x = Qx, y = Qy, z = Qz, w = Qw}.


marker_info(head) ->
    marker_info({#geometry_msgs_point{},0.0,#geometry_msgs_quaternion{w = 1.0}}, "robot_head", ?CUBE, ?ADD);
marker_info(acc) ->
    marker_info({#geometry_msgs_point{},0.0,#geometry_msgs_quaternion{w=1.0}}, "robot_frame", ?SPHERE, ?ADD).

marker_info({Position, _Speed, Orientation}, Frame, Shape, Mode) ->
    #visualization_msgs_marker{
        header=#std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = Frame
        },
        type=Shape,
        action=Mode,
        pose=#geometry_msgs_pose{ position = Position,
                                    orientation = Orientation},
        scale=#geometry_msgs_vector3{x=2.0,y=2.0,z=0.5},
        color=#std_msgs_color_r_g_b_a{r=1.0,g=1.0,b=1.0,a=1.0}
    }.


move_along_range(Range) when Range =< 60.0 ->
    robo_grisp_wheels:backward();
move_along_range(Range) when Range >= 100.0 ->
    robo_grisp_wheels:forward();
move_along_range(_) ->
    robo_grisp_wheels:stop().
