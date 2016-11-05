#!/bin/bash

## This install script installs this library onto the roboRIO.
## Run this script every time the version in source control changes
## on the host machine

userhostname="$1"

sftp $1 <<EOF
cd ~
put -r ../
quit
EOF
