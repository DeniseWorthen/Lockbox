#!/bin/bash

set -x

rundir="bilin"
time=2020-11-01-21600

#dir=CICE_OUTPUT
#fsrc=iceh_inst.
#vlist=""aice_h" "Tsfc_h""

#dir=""
#fsrc=ufs.cpld.cpl.hi.ocn.
#vlist=""ocnImp_So_t" "ocnImp_So_s""
#vname=ocnImp_So_t
#vname=ocnExp_Foxx_sen

#fsrc=ufs.cpld.cpl.hi.ice.
#vlist=""iceExp_Faxa_lwdn" "iceExp_Sa_tbot" "iceExp_Sa_u" "iceExp_Sa_shum" "iceExp_Sa_pbot""

fsrc=ufs.cpld.cpl.hi.atm.
#vlist=""atmExp_Si_t""
vlist="atmExp_So_t"

f1=${rundir}/mem001/${dir}/${fsrc}${time}.nc
f2=${rundir}/mem002/${dir}/${fsrc}${time}.nc
f3=${rundir}/mem003/${dir}/${fsrc}${time}.nc
f4=${rundir}/mem004/${dir}/${fsrc}${time}.nc
f5=${rundir}/mem005/${dir}/${fsrc}${time}.nc
f6=${rundir}/mem006/${dir}/${fsrc}${time}.nc
f7=${rundir}/mem007/${dir}/${fsrc}${time}.nc
f8=${rundir}/mem008/${dir}/${fsrc}${time}.nc
f9=${rundir}/mem009/${dir}/${fsrc}${time}.nc
f10=${rundir}/mem010/${dir}/${fsrc}${time}.nc

for vname in ${vlist}; do
  ncrcat -O -v ${vname} $f1 $f2 $f3 $f4 $f5 $f6 $f7 $f8 $f9 $f10 foo1.nc
  ncwa -O -a time foo1.nc foo2.nc
  ncbo -O -v ${vname} foo1.nc foo2.nc foo3.nc
  ncra -O -y rmssdn foo3.nc ${rundir}.${vname}.${time}.stddev.nc
done
