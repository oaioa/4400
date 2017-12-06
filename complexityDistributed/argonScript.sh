#!/bin/bash

if [ $# -eq 0  ]
    then FOLDER="remote"
    else FOLDER=$1
fi

function myfunc() 
{ 
    local complex="$(argon $FOLDER  | awk -F '(' '{print $2}' | grep -Eo '[0-9]{1,4}' | awk '{sum+=$1} END {print sum}')"
    if [ -n "$complex" ]
        then echo 0
        else echo $complex
    fi
}

complex=$(myfunc)
echo $complex

