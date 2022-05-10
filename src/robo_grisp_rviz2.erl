-module(robo_grisp_rviz2).

-export([
    tf_head/0,
    tf_floor_to_map/0,
    tf_robot_to_floor/1,
    quaternion_from_euler/3,
    marker_info/1,
    marker_update/3,
    marker_array/1]).


-include_lib("rosie_dds/include/dds_types.hrl").

% -include_lib("geometry_msgs/src/_rosie/geometry_msgs_twist_msg.hrl").
-include_lib("geometry_msgs/src/_rosie/geometry_msgs_pose_stamped_msg.hrl").

-include_lib("sensor_msgs/src/_rosie/sensor_msgs_temperature_msg.hrl").
-include_lib("sensor_msgs/src/_rosie/sensor_msgs_range_msg.hrl").

-include_lib("tf2_msgs/src/_rosie/tf2_msgs_t_f_message_msg.hrl").
-include_lib("visualization_msgs/src/_rosie/visualization_msgs_marker_msg.hrl").
-include_lib("visualization_msgs/src/_rosie/visualization_msgs_marker_array_msg.hrl").

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
                    translation = #geometry_msgs_vector3{x = 3.0, y = 0.0, z = 2.0},
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


marker_info(head) ->
    marker_info(
        #geometry_msgs_point{},
        quaternion_from_euler(0.0, 85.0, 0.0),
        #geometry_msgs_vector3{x=1.0,y=1.0,z=1.0},
        #std_msgs_color_r_g_b_a{r=0.1,g=0.1,b=0.1,a=1.0},
        0,
        "robot_head", ?CYLINDER, ?ADD);
marker_info(base) ->
    marker_info(
        #geometry_msgs_point{z = 1.0},
        #geometry_msgs_quaternion{},
        #geometry_msgs_vector3{x=6.0,y=4.0,z=1.5},
        #std_msgs_color_r_g_b_a{r=0.5,g=0.5,b=0.5,a=1.0},
        1,
        "robot_frame", ?CUBE, ?ADD);
marker_info(acc) ->
    marker_info(#geometry_msgs_point{},
        #geometry_msgs_quaternion{},
        #geometry_msgs_vector3{x=2.0,y=2.0,z=0.5},
        #std_msgs_color_r_g_b_a{r=1.0,g=0.0,b=0.0,a=1.0},
        2,
        "robot_frame", ?SPHERE, ?ADD).

marker_info(Position, Orientation, Scale, Color, ID, Frame, Shape, Mode) ->
    #visualization_msgs_marker{
        header=#std_msgs_header{
            stamp = #builtin_interfaces_time{},
            frame_id = Frame
        },
        type=Shape,
        action=Mode,
        id = ID,
        pose=#geometry_msgs_pose{ position = Position,
                                    orientation = Orientation},
        scale=Scale,
        color=Color,
        ns = "robo_grisp"
    }.

marker_update(acc, Position, Orientation ) ->
    Marker = marker_info(acc),
    Marker#visualization_msgs_marker{
        pose = #geometry_msgs_pose{position = Position, orientation = Orientation},
        action = ?MODIFY
    }.

marker_array(Markers) ->
    #visualization_msgs_marker_array{
        markers = Markers
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