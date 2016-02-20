#!/usr/bin/env bash
echo "Bundling libraries"
for each in html/lib/components/*.slib; do cat $each; echo ""; echo ""; echo ""; done > html/lib/components.slib
for each in html/lib/modules/*.lib; do cat $each; echo ""; echo ""; echo ""; done > html/lib/modules.lib
echo "..Done Bundling libraries"
