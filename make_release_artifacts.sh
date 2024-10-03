#!/bin/sh
set -eu

my_dir=$(dirname "$0")
cd "$my_dir"
models=$(find . -name config.json -exec dirname "{}" \;)
for model in $models; do
    sh zip_dir.sh "$model"
done
