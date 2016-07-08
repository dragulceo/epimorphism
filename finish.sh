#!/usr/bin/env bash

#echo "Minifying"
#uglifyjs html/index.tmp > html/index.js
cp html/index.tmp html/index.js
rm html/index.tmp
#psc-bundle-fast -i output -m Main --main Main -o html/index.js
