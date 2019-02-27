#!/usr/bin/env bash

# Check every ten seconds if the process identified as $1 is still
# running. After 5 checks (~60 seconds), kill it. Return non-zero to
# indicate something was killed.
monitor() {
    local pid=$1 i=0
    local max_checks=${2:-20}

    while ps $pid &>/dev/null; do
        if (( i++ > ${max_checks} )); then
            echo
            "Max checks reached. Sending SIGKILL to ${pid}..." >&2
            kill -9 $pid; return 1
        fi

        sleep 10
    done

    return 0
}

read -r pid < ~/data/.offlineimap/pid

if ps $pid &>/dev/null; then
    echo "Process $pid already running. Exiting..." >&2
    exit 1
fi

DBUS_SESSION_FILE=dbus_session.sh
if [ -f $HOME/bin/$DBUS_SESSION_FILE ]; then
    source $HOME/bin/$DBUS_SESSION_FILE
else
    echo "$DBUS_SESSION_FILE is not found"
    exit 1
fi

# FACE='-u quiet'
if [ "$1" == "full" ]; then
    offlineimap -o $FACE & monitor $! 200
else
    offlineimap -q -f 'INBOX','[Gmail]/Sent Mail'  -o $FACE & monitor $!
fi


