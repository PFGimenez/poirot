#!/bin/sh
if [ "$#" -ne 3 ]; then
    echo "Usage:" $0 "grammar axiom sentence"
else
    python3 antlr4-oracle.py $1 $2 "" "" "$3"
    error=$?
    if [ $error -eq 0 ]; then
        echo "It is a word of the language"
    elif [ $error -eq 180 ]; then
        echo "It is NOT a word of the language"
    else
        echo "Error" $error
    fi
fi
