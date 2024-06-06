#!/bin/bash

cmd=~/cime/tools/cprnc/cprnc

set -x

rt="/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT"

src="devcice/datm_cdeps_gfs_intel"
#src="updcicePR/gfs.b14cedf"
dst="updcicePR/gfs.01ed4db7c"

#fname="ufs.cpld.cpl.hi.atm.2021-03-22-"
#time="22320"

fname="DATM_GFS.cpl.hi.ice.2021-03-22-"
time="25200"

#fname="DATM_CFSR.cpl.hi.ice.2011-10-02-"
#time="00000"

$cmd -m ${rt}/${src}/${fname}${time}.nc  ${rt}/${dst}/${fname}${time}.nc

#for tile in "tile1" "tile2" "tile3" "tile4" "tile5" "tile6"; do
# $cmd -m ${rt}/base/${fname}${tile}.nc  ${rt}/rest/${fname}${tile}.nc | grep RMS
#done
