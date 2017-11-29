#!/usr/bin/env bash

pulp --watch --before 'clear;./scripts/bundle_lib.sh' --then './scripts/finish.sh' browserify --standalone Main --to html/index.tmp
