#!/bin/bash
cd "$(dirname "$0")"

for d in $(find ./.stack-work -type f -iname "scotch-db*") ; do rm $d; done
for d in $(find ./.stack-work -type d -iname "scotch-db*") ; do rm -rf $d; done
stack build
exit
