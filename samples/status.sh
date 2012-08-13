#!/bin/bash

STATUSPIPE="/tmp/xmobar_status_jrk"

function isMuted () {
    # retrieve mute status
    # return an arbitrary string for true or nothing at all
    echo
}

function getPercent () {
    # somehow retrieve the percent value as plain int (e.g. "66")
    echo "66"
}

function percentBar () {
    local i=1                   res=
          normal=47             high=80
          fgColor='#657b83'     mutedColor='#cb4b16'
          lowColor='#859900'    midColor='#b58900'
          highColor='#cb4b16'

          bar="$(echo -ne "\u2588")"
          percent="$( getPercent )"
          muted="$( isMuted )"

    if [ -n "$muted" ]; then
        res="<fc=$mutedColor>"
    else
        res="<fc=$lowColor>"
    fi

    while [ $i -lt $percent ]; do
        if   [ $i -eq $normal -a -z "$muted" ]; then
            res+="</fc><fc=$midColor>"
        elif [ $i -eq $high   -a -z "$muted" ]; then
            res+="</fc><fc=$highColor>"
        fi

        res+=$bar
        i=$((i+1))
    done

    res+="</fc><fc=$fgColor>"

    while [ $i -lt 100 ]; do
        res+='-'
        i=$((i+1))
    done

    echo "$res</fc>"
}

echo "$( percentBar )" > "$STATUSPIPE"
