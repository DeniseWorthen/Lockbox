#!/bin/bash
set -eux

# contains ocean_mosaic.nc
MOM6FIX=/scratch1/NCEPDEV/nems/emc.nemspara/RT/NEMSfv3gfs/input-data-20211210/MOM6_FIX
# contains CXX mosaics
ATMFIX=/scratch2/NCEPDEV/stmp3/Bing.Fu/fixtile
# make coupler executable
EXEDIR=/scratch2/NCEPDEV/ensemble/Bing.Fu/FRE-NCtools/tools/make_coupler_mosaic

#$EXEDIR/make_coupler_mosaic --verbose --atmos_mosaic $ATMFIX/C96_mosaic.nc --ocean_mosaic $MOM6FIX/025/ocean_mosaic.nc --mosaic_name mosaic --ocean_topog $MOM6FIX/025/topog.nc --mosaic_name aC96o025.grid_spec

$EXEDIR/make_coupler_mosaic --verbose --atmos_mosaic $ATMFIX/C384_mosaic.nc --ocean_mosaic $MOM6FIX/100/ocean_mosaic.nc --mosaic_name mosaic --ocean_topog $MOM6FIX/100/topog.nc --mosaic_name aC384o100.grid_spec


