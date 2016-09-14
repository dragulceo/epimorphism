#!/usr/bin/env bash

killall fswatch
fswatch -o html/lib/components/*.slib  | xargs -n1 ./scripts/bundle_lib.sh &
fswatch -o html/lib/indexes/*.slib     | xargs -n1 ./scripts/bundle_lib.sh &
fswatch -o html/lib/modules/*.lib      | xargs -n1 ./scripts/bundle_lib.sh &
fswatch -o html/lib/modules/save/*.lib | xargs -n1 ./scripts/bundle_lib.sh &
pulp --watch --before 'clear;./scripts/bundle_lib.sh' --then './scripts/finish.sh' browserify --standalone Main --to html/index.tmp
