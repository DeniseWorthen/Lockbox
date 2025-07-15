#!/bin/bash

set -x

src=gefs.rundir

#for dirname in "mem001"; do
#for dirname in "mem002" "mem003"; do
#for dirname in "mem001" "mem002" "mem003" "mem004" "mem005" "mem006" "mem007" "mem008" "mem009" "mem010"; do
for i in {1..10}; do
    ii=$(printf "%03d" "$i")
    dirname=mem$ii
    cp -r ${src} ${dirname}
    cp perturbs/${dirname}/analysis/atmos/20201101.030000.fv3_perturbation.nc ${dirname}/INPUT/atminc.nc
    #echo "cp -r ${src} ${dirname}"
    #echo "cp perturbs/${dirname}/analysis/atmos/* ${dirname}/INPUT/atminc.nc"
done
