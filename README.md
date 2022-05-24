robo_grisp
=====

A GRiSP and ROSiE application. This is meant to be used on a physical robot, presented at CODE BEAM Stockholm 2022.

Build
-----

    rebar3 compile

Deploy
------

    rebar3 grisp deploy

Generate the first SD image for robo_grisp 0.1.0
------

    git checkout 0.1.0
    git reset --hard
    rm -rf _build/grisp/rel
    rm -rf _build/grisp/lib/robo_grisp
    rebar3 grisp deploy

Serial
------

    ./serial.sh

Version 0.2.0, relup and tar ball
------

    git checkout 0.2.0
    git reset --hard
    rebar3 grisp deploy
    rebar3 as grisp appup generate
    rebar3 as grisp relup --relname robo_grisp --relvsn 0.2.0
    rebar3 as grisp tar

Release upgrade
------

In the same LAN of the grisp board running v 0.1.0 simply run:

    ./upgrade.sh

Remote shell
------

To spawn a remote shell run:

    ./remote_shell.sh

Connected rebar3 shell
------

Usefull to easely net-load modules, for example:

    rebar3 dev
    > r3:compile().
    > nl(robo_grisp).
