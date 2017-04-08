#!/usr/bin/env bash
echo "Bundling Libraries"
for each in lib/components/*.slib; do cat $each; printf "\n\n\n"; done > lib/components.slib
for each in lib/indexes/*.slib; do cat $each; printf "\n\n\n"; done > lib/sections.slib
for each in lib/modules/*.lib; do cat $each; printf "\n\n\n"; done > lib/modules.lib
printf "\n\n\n" >> lib/modules.lib
for each in lib/modules/save/*.lib; do cat $each; printf "\n\n\n"; done >> lib/modules.lib
