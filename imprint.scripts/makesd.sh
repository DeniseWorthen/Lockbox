#!/bin/bash

set -x

#dir=""
dir=CICE_OUTPUT
time=2020-11-01-21600
fsrc=iceh_inst.
vname=aice_h

#fsrc=ufs.cpld.cpl.hi.ocn.
#vname=ocnImp_So_t
#vname=ocnExp_Foxx_sen

#fsrc=ufs.cpld.cpl.hi.ice.
#vname=iceExp_Faxa_lwdn
#vname=iceImp_Si_t

f1=mem001/${dir}/${fsrc}${time}.nc
f2=mem002/${dir}/${fsrc}${time}.nc
f3=mem003/${dir}/${fsrc}${time}.nc
f4=mem004/${dir}/${fsrc}${time}.nc
f5=mem005/${dir}/${fsrc}${time}.nc
f6=mem006/${dir}/${fsrc}${time}.nc
f7=mem007/${dir}/${fsrc}${time}.nc
f8=mem008/${dir}/${fsrc}${time}.nc
f9=mem009/${dir}/${fsrc}${time}.nc
f10=mem010/${dir}/${fsrc}${time}.nc

#ncrcat -O -v aice_h m1/iceh_inst.2020-11-01-43200.nc m2/iceh_inst.2020-11-01-43200.nc

ncrcat -O -v ${vname} $f1 $f2 $f3 $f4 $f5 $f6 $f7 $f8 $f9 $f10 foo1.nc
#ncrcat -O -v aice_h $f1 $f2 $f3 $f4 $f5 foo1.nc
ncwa -O -a time foo1.nc foo2.nc
ncbo -O -v ${vname} foo1.nc foo2.nc foo3.nc
ncra -O -y rmssdn foo3.nc ${vname}.${time}.stddev.nc
