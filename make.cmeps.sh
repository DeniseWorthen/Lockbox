#!/bin/bash

set -x

src=tst2

#================
info=rhinit

outfile=${src}/${info}.dat
if [ -f "$outfile" ]; then
    rm "$outfile"
fi

for exp in run0 run1 run2 run3 run4; do
    outfile=${src}/${info}.dat
    #grep 'MED: (med_map_mod: RouteHandles_init)' ${src}/${exp}/ESMF_Profile*| awk '{print $6"  "$8"  "$9"  "$11}'
    grep 'MED: (med_map_mod: RouteHandles_init)' ${src}/${exp}/ESMF_Profile*| awk '{print $5"  "$7"  "$8"  "$10}'>>${outfile}
done

#================
info=post_ice
outfile=${src}/${info}.dat
if [ -f "$outfile" ]; then
    rm "$outfile"
fi

for exp in run0 run1 run2 run3 run4; do
    grep '\[MED] med_phases_post_ice' ${src}/${exp}/ESMF_Profile.summary |awk '{print $4"  "$6"  "$7"  "$9}'>>${outfile}
done

#================
info=post_ocn
outfile=${src}/${info}.dat
if [ -f "$outfile" ]; then
    rm "$outfile"
fi

for exp in run0 run1 run2 run3 run4; do
    grep '\[MED] med_phases_post_ocn' ${src}/${exp}/ESMF_Profile.summary |awk '{print $4"  "$6"  "$7"  "$9}'>>${outfile}
done

#================
info=post_atm
outfile=${src}/${info}.dat
if [ -f "$outfile" ]; then
    rm "$outfile"
fi

for exp in run0 run1 run2 run3 run4; do
    grep '\[MED] med_phases_post_atm' ${src}/${exp}/ESMF_Profile.summary |awk '{print $4"  "$6"  "$7"  "$9}'>>${outfile}
done

#================
info=post_wav
outfile=${src}/${info}.dat
if [ -f "$outfile" ]; then
    rm "$outfile"
fi

for exp in run0 run1 run2 run3 run4; do
    grep '\[MED] med_phases_post_wav' ${src}/${exp}/ESMF_Profile.summary |awk '{print $4"  "$6"  "$7"  "$9}'>>${outfile}
done
