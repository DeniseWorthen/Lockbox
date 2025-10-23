#!/bin/bash

set -x

#src=tst3

for src in tst4; do
#for src in tst1 tst2 tst3 tst4; do
    #================
    info=rhinit
    outfile=${src}/${info}.dat
    if [ -f "$outfile" ]; then
        rm "$outfile"
    fi

    for exp in run0 run1 run2 run3 run4; do
        outfile=${src}/${info}.dat
        grep 'MED: (med_map_mod: RouteHandles_init)' ${src}/${exp}/ESMF_Profile*| awk '{print $5"  "$7"  "$8"  "$10}'>>${outfile}
    done

    for info in post_ice post_ocn post_atm post_wav; do
        #================
        outfile=${src}/${info}.dat
        if [ -f "$outfile" ]; then
            rm "$outfile"
        fi

        for exp in run0 run1 run2 run3 run4; do
            grep '\[MED] med_phases_'${info} ${src}/${exp}/ESMF_Profile.summary |awk '{print $4"  "$6"  "$7"  "$9}'>>${outfile}
        done
    done
done
B
