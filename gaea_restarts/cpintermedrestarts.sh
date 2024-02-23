#!/bin/bash

set -x

year="2021"
mon="03"
day="22"
hour="18"
secs="64800"
#hour="12"
#secs="43200"

sorc="/lustre/f2/scratch/Denise.Worthen/FV3_RT/rt_730/base"
dest="/lustre/f2/scratch/Denise.Worthen/FV3_RT/rt_730/rest"

cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.coupler.res  ${dest}/INPUT/coupler.res
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.nc  ${dest}/INPUT/fv_core.res.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.tile1.nc  ${dest}/INPUT/fv_core.res.tile1.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.tile2.nc  ${dest}/INPUT/fv_core.res.tile2.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.tile3.nc  ${dest}/INPUT/fv_core.res.tile3.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.tile4.nc  ${dest}/INPUT/fv_core.res.tile4.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.tile5.nc  ${dest}/INPUT/fv_core.res.tile5.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_core.res.tile6.nc  ${dest}/INPUT/fv_core.res.tile6.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_srf_wnd.res.tile1.nc  ${dest}/INPUT/fv_srf_wnd.res.tile1.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_srf_wnd.res.tile2.nc  ${dest}/INPUT/fv_srf_wnd.res.tile2.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_srf_wnd.res.tile3.nc  ${dest}/INPUT/fv_srf_wnd.res.tile3.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_srf_wnd.res.tile4.nc  ${dest}/INPUT/fv_srf_wnd.res.tile4.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_srf_wnd.res.tile5.nc  ${dest}/INPUT/fv_srf_wnd.res.tile5.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_srf_wnd.res.tile6.nc  ${dest}/INPUT/fv_srf_wnd.res.tile6.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_tracer.res.tile1.nc  ${dest}/INPUT/fv_tracer.res.tile1.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_tracer.res.tile2.nc  ${dest}/INPUT/fv_tracer.res.tile2.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_tracer.res.tile3.nc  ${dest}/INPUT/fv_tracer.res.tile3.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_tracer.res.tile4.nc  ${dest}/INPUT/fv_tracer.res.tile4.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_tracer.res.tile5.nc  ${dest}/INPUT/fv_tracer.res.tile5.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.fv_tracer.res.tile6.nc  ${dest}/INPUT/fv_tracer.res.tile6.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.phy_data.tile1.nc  ${dest}/INPUT/phy_data.tile1.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.phy_data.tile2.nc  ${dest}/INPUT/phy_data.tile2.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.phy_data.tile3.nc  ${dest}/INPUT/phy_data.tile3.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.phy_data.tile4.nc  ${dest}/INPUT/phy_data.tile4.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.phy_data.tile5.nc  ${dest}/INPUT/phy_data.tile5.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.phy_data.tile6.nc  ${dest}/INPUT/phy_data.tile6.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.sfc_data.tile1.nc  ${dest}/INPUT/sfc_data.tile1.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.sfc_data.tile2.nc  ${dest}/INPUT/sfc_data.tile2.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.sfc_data.tile3.nc  ${dest}/INPUT/sfc_data.tile3.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.sfc_data.tile4.nc  ${dest}/INPUT/sfc_data.tile4.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.sfc_data.tile5.nc  ${dest}/INPUT/sfc_data.tile5.nc
cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.sfc_data.tile6.nc  ${dest}/INPUT/sfc_data.tile6.nc

#cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.ca_data.tile1.nc  ${dest}/INPUT/ca_data.tile1.nc
#cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.ca_data.tile2.nc  ${dest}/INPUT/ca_data.tile2.nc
#cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.ca_data.tile3.nc  ${dest}/INPUT/ca_data.tile3.nc
#cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.ca_data.tile4.nc  ${dest}/INPUT/ca_data.tile4.nc
#cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.ca_data.tile5.nc  ${dest}/INPUT/ca_data.tile5.nc
#cp ${sorc}/RESTART/${year}${mon}${day}.${hour}0000.ca_data.tile6.nc  ${dest}/INPUT/ca_data.tile6.nc

cp ${sorc}/RESTART/MOM.res.${year}-${mon}-${day}-${hour}-00-00.nc ${dest}/INPUT/MOM.res.nc
#cp ${sorc}/RESTART/MOM.res.${year}-${mon}-${day}-${hour}-00-00_1.nc ${dest}/INPUT/MOM.res_1.nc
#cp ${sorc}/RESTART/MOM.res.${year}-${mon}-${day}-${hour}-00-00_2.nc ${dest}/INPUT/MOM.res_2.nc
#cp ${sorc}/RESTART/MOM.res.${year}-${mon}-${day}-${hour}-00-00_3.nc ${dest}/INPUT/MOM.res_3.nc

cp ${sorc}/RESTART/iced.${year}-${mon}-${day}-${secs}.nc ${dest}/INPUT/iced.${year}-${mon}-${day}-${secs}.nc

cp ${sorc}/RESTART/ufs.cpld.cpl.r.${year}-${mon}-${day}-${secs}.nc ${dest}
ls -1 ${dest}/ufs.cpld.cpl.r.${year}-${mon}-${day}-${secs}.nc>${dest}/rpointer.cpl

#sorc="/glade/work/worthen/cmeps_restart_1h"
#cp ${sorc}/rpointer.cpl ${dest}

#sorc="/glade/work/worthen/cmeps_restart_1h"
#cp ${sorc}/ice.restart_file ${dest}/RESTART
