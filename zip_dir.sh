#!/bin/sh
set -eu

if [ -z "$1" ]; then
    echo Must provide path as argument
    exit 1
fi

olddir=$PWD
filename=$(basename "$1").zip

echo Creating $filename
cd "$1"
zip -r "$olddir/$filename" ./*
