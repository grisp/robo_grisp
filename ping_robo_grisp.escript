#!/usr/bin/env escript
%% -*- erlang -*-
main([]) ->
    net_adm:ping('robo_grisp@grisp-board').