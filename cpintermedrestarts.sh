#!/bin/bash

set -x
year="2021"
mon="03"
day="22"
hour="18"
secs="64800"

datetype1=${year}${mon}${day}.${hour}"0000"
datetype2=${year}-${mon}-${day}-${hour}
datetype3=${year}-${mon}-${day}-${secs}

sorc="/glade/derecho/scratch/worthen/FV3_RT/rt_41038/base"
dest="/glade/derecho/scratch/worthen/FV3_RT/rt_41038/rest"

#FV3 restarts
cp ${sorc}/RESTART/${datetype1}.coupler.res  ${dest}/INPUT/coupler.res
cp ${sorc}/RESTART/${datetype1}.fv_core.res.nc  ${dest}/INPUT/fv_core.res.nc
cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile1.nc  ${dest}/INPUT/fv_core.res.tile1.nc
cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile2.nc  ${dest}/INPUT/fv_core.res.tile2.nc
cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile3.nc  ${dest}/INPUT/fv_core.res.tile3.nc
cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile4.nc  ${dest}/INPUT/fv_core.res.tile4.nc
cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile5.nc  ${dest}/INPUT/fv_core.res.tile5.nc
cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile6.nc  ${dest}/INPUT/fv_core.res.tile6.nc
cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile1.nc  ${dest}/INPUT/fv_srf_wnd.res.tile1.nc
cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile2.nc  ${dest}/INPUT/fv_srf_wnd.res.tile2.nc
cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile3.nc  ${dest}/INPUT/fv_srf_wnd.res.tile3.nc
cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile4.nc  ${dest}/INPUT/fv_srf_wnd.res.tile4.nc
cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile5.nc  ${dest}/INPUT/fv_srf_wnd.res.tile5.nc
cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile6.nc  ${dest}/INPUT/fv_srf_wnd.res.tile6.nc
cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile1.nc  ${dest}/INPUT/fv_tracer.res.tile1.nc
cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile2.nc  ${dest}/INPUT/fv_tracer.res.tile2.nc
cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile3.nc  ${dest}/INPUT/fv_tracer.res.tile3.nc
cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile4.nc  ${dest}/INPUT/fv_tracer.res.tile4.nc
cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile5.nc  ${dest}/INPUT/fv_tracer.res.tile5.nc
cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile6.nc  ${dest}/INPUT/fv_tracer.res.tile6.nc
cp ${sorc}/RESTART/${datetype1}.phy_data.tile1.nc  ${dest}/INPUT/phy_data.tile1.nc
cp ${sorc}/RESTART/${datetype1}.phy_data.tile2.nc  ${dest}/INPUT/phy_data.tile2.nc
cp ${sorc}/RESTART/${datetype1}.phy_data.tile3.nc  ${dest}/INPUT/phy_data.tile3.nc
cp ${sorc}/RESTART/${datetype1}.phy_data.tile4.nc  ${dest}/INPUT/phy_data.tile4.nc
cp ${sorc}/RESTART/${datetype1}.phy_data.tile5.nc  ${dest}/INPUT/phy_data.tile5.nc
cp ${sorc}/RESTART/${datetype1}.phy_data.tile6.nc  ${dest}/INPUT/phy_data.tile6.nc
cp ${sorc}/RESTART/${datetype1}.sfc_data.tile1.nc  ${dest}/INPUT/sfc_data.tile1.nc
cp ${sorc}/RESTART/${datetype1}.sfc_data.tile2.nc  ${dest}/INPUT/sfc_data.tile2.nc
cp ${sorc}/RESTART/${datetype1}.sfc_data.tile3.nc  ${dest}/INPUT/sfc_data.tile3.nc
cp ${sorc}/RESTART/${datetype1}.sfc_data.tile4.nc  ${dest}/INPUT/sfc_data.tile4.nc
cp ${sorc}/RESTART/${datetype1}.sfc_data.tile5.nc  ${dest}/INPUT/sfc_data.tile5.nc
cp ${sorc}/RESTART/${datetype1}.sfc_data.tile6.nc  ${dest}/INPUT/sfc_data.tile6.nc
cp ${sorc}/RESTART/${datetype1}.ca_data.tile1.nc  ${dest}/INPUT/ca_data.tile1.nc
cp ${sorc}/RESTART/${datetype1}.ca_data.tile2.nc  ${dest}/INPUT/ca_data.tile2.nc
cp ${sorc}/RESTART/${datetype1}.ca_data.tile3.nc  ${dest}/INPUT/ca_data.tile3.nc
cp ${sorc}/RESTART/${datetype1}.ca_data.tile4.nc  ${dest}/INPUT/ca_data.tile4.nc
cp ${sorc}/RESTART/${datetype1}.ca_data.tile5.nc  ${dest}/INPUT/ca_data.tile5.nc
cp ${sorc}/RESTART/${datetype1}.ca_data.tile6.nc  ${dest}/INPUT/ca_data.tile6.nc
#
##MOM restarts
cp ${sorc}/RESTART/${datetype1}.MOM.res.nc  ${dest}/INPUT/MOM.res.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00.nc ${dest}/INPUT/MOM.res.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00_1.nc ${dest}/INPUT/MOM.res_1.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00_2.nc ${dest}/INPUT/MOM.res_2.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00_3.nc ${dest}/INPUT/MOM.res_3.nc

#CICE restarts
cp ${sorc}/RESTART/iced.${datetype3}.nc ${dest}/INPUT/iced.${datetype3}.nc
ls -1 ${dest}/INPUT/iced.${datetype3}.nc>${dest}/ice.restart_file

cp ${sorc}/RESTART/ufs.cpld.cpl.r.${datetype3}.nc ${dest}
ls -1 ${dest}/ufs.cpld.cpl.r.${datetype3}.nc>${dest}/rpointer.cpl

cp $sorc/ufs.cpld.ww3.r.${datetype3} ${dest}
