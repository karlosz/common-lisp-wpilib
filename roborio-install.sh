#!/bin/bash

## This install script installs this library onto the roboRIO.
## Run this script every time the version in source control changes
## on the host machine

function gethelp
{
    echo "Usage: roborio-install.sh [-p port] [-d dir] [-l user@hostname]"
    echo "Default behaviour is to upload the working directory to lvuser@roboRIO-4795-FRC.local:22"
}

ARGS=$(getopt -o hp:d:l: --long --help -- "$@")

if [ $? -ne 0 ]; then
    gethelp
    exit 1
fi

PORT=22
DIR=$(pwd)
HOST="lvuser@roboRIO-4795-FRC.local"
eval set -- "$ARGS"
while true; do
    case "$1" in
	-h|--help) gethelp; exit 0 ;;
	-p) PORT="$2"; shift 2 ;;
	-d) DIR="$2"; shift 2 ;;
	-l) HOST="$2"; shift 2 ;;
	--) shift; break ;;
    esac
done

if [ "$@" ]; then
    gethelp
    exit 1
fi

sftp -oPort=$PORT $HOST << EOF
put -r $DIR
exit
EOF
