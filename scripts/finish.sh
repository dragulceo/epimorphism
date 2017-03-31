#!/usr/bin/env bash

#echo "Minifying"
#uglifyjs html/index.tmp > html/javascripts/index.js
cp html/index.tmp html/javascript/index.js
rm html/index.tmp
#psc-bundle-fast -i output -m Main --main Main -o html/index.js
