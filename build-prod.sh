#!/bin/sh

set -e

js="main.js"
min="main.min.js"
html="elm-timelines.html"

elm make src/Main.elm --optimize --output=$js

uglifyjs $js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $min

echo "Initial size: $(cat $js | wc -c) bytes ($js)"
echo "Minified size:$(cat $min | wc -c) bytes ($min)"

./inline-script.js

echo "Standalone:   $(cat $html | wc -c) bytes ($html) (gzipped:$(cat $html | gzip -c | wc -c) bytes)\n"
