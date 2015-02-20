#!/bin/bash

set -e

# for every hidden file in here that isn't a directory, copy the same file but in home
# directory
#
# ls -A        \ # all but . and ..
#    -p        \ # / after dis
#    -1 |      \ # one line per file
# grep "^\." | \ # leading periods ie hidden files
# grep -v "/$"   # everything that doesn't have a / at the end
for file in $(ls -Ap1 | grep  "^\." | grep -v "/$"); do
    echo ~/${file} .
done
