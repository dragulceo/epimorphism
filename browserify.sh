#!/usr/bin/env bash

rm -r dist
mkdir dist

pulp browserify --standalone Main --to html/index.js
