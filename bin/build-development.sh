#!/usr/bin/env bash

set -e

build_dir=.build
static_dir=static

tmp_dir=$(mktemp -d)

echo "[HTML]"
sed 's/{{ROOT}}//' $static_dir/index.html > $tmp_dir/index.html

echo "[CSS]"
cp $static_dir/index.css $tmp_dir/

echo "[ELM]"
elm make src/Main.elm --debug --output=$tmp_dir/app.js

# Update the build directory
rm -rf $build_dir
mv $tmp_dir $build_dir

echo "[DONE]"
