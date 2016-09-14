#!/usr/bin/env bash
echo "Bundling Libraries"
for each in html/lib/components/*.slib; do cat $each; printf "\n\n\n"; done > html/lib/components.slib
for each in html/lib/indexes/*.slib; do cat $each; printf "\n\n\n"; done > html/lib/indexes.slib
for each in html/lib/modules/*.lib; do cat $each; printf "\n\n\n"; done > html/lib/modules.lib
printf "\n\n\n" >> html/lib/modules.lib
for each in html/lib/modules/save/*.lib; do cat $each; printf "\n\n\n"; done >> html/lib/modules.lib
