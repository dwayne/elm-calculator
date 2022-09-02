#!/usr/bin/env bash

set -e

build_dir=.build
root_dir=elm-calculator
static_dir=static

tmp_dir=$(mktemp -d)

echo "[HTML]"
sed "s/{{ROOT}}/\/$root_dir/" $static_dir/index.html > $tmp_dir/index.html

echo "[CSS]"
cp $static_dir/index.css $tmp_dir/

echo "[ELM]"

js_name="app.js"
min_name="app.min.js"
js="$tmp_dir/$js_name"
min="$tmp_dir/$min_name"

elm make src/Main.elm --optimize --output=$js

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $min

echo "Compiled size: $(cat $js | wc -c) bytes ($js_name)"
echo "Minified size: $(cat $min | wc -c) bytes ($min_name)"
echo "Gzipped size:  $(cat $min | gzip -c | wc -c) bytes"

mv $min $js

# Update the build directory
rm -rf $build_dir
mkdir $build_dir
mv $tmp_dir $build_dir/$root_dir

echo "[DONE]"
