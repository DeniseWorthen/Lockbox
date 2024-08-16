#!/bin/bash

set -x

src=atmw.base
dst=atmw.1cdaf9f1
#dst=atmw.ic4
#dst=atmw.1dfde570
#dst=test

#dst=atmw.1dfde570.fix
#dst=atmw.1dfde570.nofix

dt=720
t0=21600
#fsrc="ufs.atmw.cpl.hi.atm.2021-03-22-"
fsrc="ufs.atmw.cpl.hi.wav.2021-03-22-"
#fsrc="ufs.atmw.ww3.hi.2021-03-22-"
let time=t0+13*dt
fname=$fsrc$time".nc"

cprnc -m ${src}/${fname} ${dst}/${fname}

#fname="ufs.cpld.cpl.hi.44100."
#for tile in "tile1" "tile2" "tile3" "tile4" "tile5" "tile6"; do
# $cmd -m ${rt}/cpld_control_prod/RESTART/${fname}${tile}.nc  ${rt}/cpld_restart_prod/RESTART/${fname}${tile}.nc | grep RMS
#done

echo $src $dst $fname
