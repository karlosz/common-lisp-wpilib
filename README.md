## Installing the library onto the roboRIO

To install wpilib on the robot and keep the library on the robot in sync with an interactive development environment, use `roborio-install.sh`.

## On the roboRIO

Before executing sbcl, you have to change the kernel policy, since sbcl overcommits memory.

`echo 1 > /proc/sys/vm/overcommit_memory`

## Java persistence
Only one copy of HAL can be running at once, so in order to disable a running Java instance, run
`nirtcfg -s section=SYSTEMSETTINGS,token=NoApp.enabled,value=True`
