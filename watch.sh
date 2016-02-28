#!/usr/bin/env bash

fswatch -o html/lib/components/*.slib | xargs -n1 ./bundle_lib.sh &
fswatch -o html/lib/modules/*.lib     | xargs -n1 ./bundle_lib.sh &
pulp --watch --before 'clear;./bundle_lib.sh' --then './browserify.sh' build
