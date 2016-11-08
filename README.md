## Installing the library onto the roboRIO

Before doing interactive development, install the library onto the roboRIO firtst by running
`./roborio-install.sh lvuser:hostname port /path/to/common-lisp-wpilib`
where `hostname` is address of the robot, and `port` is the ssh port. Note that the path MUST be an absolute address (no ~, or ../) because I suck at schell scripting.

## On the roboRIO

Before executing sbcl, you have to change the kernel policy, since sbcl overcommits memory.

`echo 1 > /proc/sys/vm/overcommit_memory`

## Java persistence
Only one copy of HAL can be running at once, so in order to disable a running Java instance, run
`nirtcfg -s section=SYSTEMSETTINGS,token=NoApp.enabled,value=True`
