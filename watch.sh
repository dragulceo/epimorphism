#!/usr/bin/env bash

fswatch -o html/lib/components/*.slib | xargs -n1 ./bundle_lib.sh &
fswatch -o html/lib/indexes/*.slib | xargs -n1 ./bundle_lib.sh &
fswatch -o html/lib/modules/*.lib     | xargs -n1 ./bundle_lib.sh &
fswatch -o html/lib/modules/save/*.lib     | xargs -n1 ./bundle_lib.sh &
pulp --watch --before 'clear;./bundle_lib.sh' --then './finish.sh' browserify --standalone Main --to html/index.tmp
