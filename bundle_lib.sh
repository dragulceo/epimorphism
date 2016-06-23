#!/usr/bin/env bash
echo "Bundling Libraries"
for each in html/lib/components/*.slib; do cat $each; echo ""; echo ""; echo ""; done > html/lib/components.slib
for each in html/lib/indexes/*.slib; do cat $each; echo ""; echo ""; echo ""; done > html/lib/indexes.slib
for each in html/lib/modules/*.lib; do cat $each; echo ""; echo ""; echo ""; done > html/lib/modules.lib
for each in html/lib/modules/save/*.lib; do cat $each; echo ""; echo ""; echo ""; done >> html/lib/modules.lib
