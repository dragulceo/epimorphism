#!/usr/bin/env bash

killall fswatch
fswatch -o lib/core/*.slib  | xargs -n1 ./scripts/bundle_lib.sh &
fswatch -o lib/sections/*.slib     | xargs -n1 ./scripts/bundle_lib.sh &
#fswatch -o lib/modules/*.lib      | xargs -n1 ./scripts/bundle_lib.sh &
#fswatch -o lib/modules/save/*.lib | xargs -n1 ./scripts/bundle_lib.sh &
pulp --watch --before 'clear;./scripts/bundle_lib.sh' --then './scripts/finish.sh' browserify --standalone Main --to html/index.tmp
