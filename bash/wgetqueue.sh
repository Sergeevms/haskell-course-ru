#!/bin/bash

shopt -s nullglob

QPATH=$HOME/.nyaqueue

mkdir -p "$QPATH"
mkdir -p "$QPATH/requests"
mkdir -p "$QPATH/requestinw"

for a in "$@"
do
    r=`mktemp --tmpdir="$QPATH/requestinw"`
    echo "$a" > "$r"
    mv -t "$QPATH/requests" "$r"
done

become_demon()
{
    cd /
    [[ -t 0 ]] && exec </dev/null
    [[ -t 1 ]] && exec >/dev/null
    [[ -t 2 ]] && exec 2>/dev/null
}

if [[ ! -f "$QPATH/running" ]]; then
    touch "$QPATH/running"
    trap "" SIGHUP

    become_demon

    (
    trap "rm '$QPATH/running'" 0
    while [ 1 ]
    do
        for a in "$QPATH/requests"/*
        do
            url=`cat "$a"`
            rm "$a"
            wget -c "$url"
        done

        sleep 1
    done
    )&
fi
