#!/usr/bin/env bash

rm -r dist
mkdir dist

pulp build
pulp browserify --standalone Main --to html/index.js
