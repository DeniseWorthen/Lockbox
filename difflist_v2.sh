#!/bin/bash

set -x

rt="/work2/noaa/stmp/dworthen/stmp/dworthen/maskchange/"

test=cpld_control_gfsv17_intel

#src=mask.86/${test}
#dst=mask/${test}

src=base/${test}
dst=mask/${test}

fname="ufs.cpld.cpl.hi.ice."
time="2021-03-22-22320"

#fname="RESTART/ufs.cpld.cpl.r."
#time="2021-03-22-43200"

cprnc -m ${rt}/${src}/${fname}${time}.nc  ${rt}/${dst}/${fname}${time}.nc
echo "${rt}/${src}/${fname}${time}.nc  ${rt}/${dst}/${fname}${time}.nc"

#for tile in "tile1" "tile2" "tile3" "tile4" "tile5" "tile6"; do
# $cmd -m ${rt}/base/${fname}${tile}.nc  ${rt}/rest/${fname}${tile}.nc | grep RMS
#done
