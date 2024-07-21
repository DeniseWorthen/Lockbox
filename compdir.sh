#!/bin/bash

set -x

src=c768_gw_case
dst=test

#src=test
#dst=c768_gw_case

# in dst but not src
echo "files in "$dst" and not in "$src
find "$src/" "$src/" "$dst/" -printf '%P\n' | sort | uniq -u
