#!/usr/bin/env bash

echo "Browserifying"
pulp browserify --standalone Main --to html/index.tmp
echo "Minifying"
uglifyjs html/index.tmp > html/index.js
rm html/index.tmp
#psc-bundle-fast -i output -m Main --main Main -o html/index.js
