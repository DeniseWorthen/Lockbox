#!/bin/bash

set -x

exename=fv3_s2s.uv3d_2nd.exe

#for dirname in "mem001"; do
#for dirname in "mem002" "mem003"; do
#for dirname in "mem001" "mem002" "mem003" "mem004" "mem005" "mem006" "mem007" "mem008" "mem009" "mem010"; do
for i in {1..10}; do
    ii=$(printf "%03d" "$i")
    dirname=mem$ii
    cd ${dirname}
    #rm atmf*nc sfcf*nc PET* out err log* ufs.cpld.cpl.hi*nc
    ln -s /ncrc/proj/drsa-hurr1/Denise.Worthen/ufs_dw/tests/${exename} fv3.exe
    sbatch job_card
    cd ../
done
