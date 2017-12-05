#!/bin/bash

FOLDER=$1
OUTPUT="$(argon $FOLDER  | awk -F '(' '{print $2}' | grep -Eo '[0-9]{1,4}' | awk '{sum+=$1} END {print sum}')"
if [ -n "$OUTPUT" ]
    then echo $OUTPUT
    else echo 0
fi
