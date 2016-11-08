#!/bin/bash

## This install script installs this library onto the roboRIO.
## Run this script every time the version in source control changes
## on the host machine

userhostname="$1"
port="$2"
path="$3"

sftp -oPort=$2 $1 <<EOF
put -r $3
exit
EOF
