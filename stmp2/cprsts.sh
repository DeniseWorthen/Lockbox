#!/bin/bash

set -x

day="03"
#hour="01"
#secs="03600"
hour="03"
secs="10800"
#hour="12"
#secs="43200"

#sorc="/work/noaa/stmp/dworthen/smtest/hour2"
#dest="/work/noaa/stmp/dworthen/smtest/restr"
sorc="/work/noaa/stmp/dworthen/smtest3_6/hour6"
dest="/work/noaa/stmp/dworthen/smtest3_6/restr3"

cp ${sorc}/RESTART/201610${day}.${hour}0000.coupler.res  ${dest}/INPUT/coupler.res
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.nc  ${dest}/INPUT/fv_core.res.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.tile1.nc  ${dest}/INPUT/fv_core.res.tile1.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.tile2.nc  ${dest}/INPUT/fv_core.res.tile2.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.tile3.nc  ${dest}/INPUT/fv_core.res.tile3.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.tile4.nc  ${dest}/INPUT/fv_core.res.tile4.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.tile5.nc  ${dest}/INPUT/fv_core.res.tile5.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_core.res.tile6.nc  ${dest}/INPUT/fv_core.res.tile6.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_srf_wnd.res.tile1.nc  ${dest}/INPUT/fv_srf_wnd.res.tile1.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_srf_wnd.res.tile2.nc  ${dest}/INPUT/fv_srf_wnd.res.tile2.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_srf_wnd.res.tile3.nc  ${dest}/INPUT/fv_srf_wnd.res.tile3.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_srf_wnd.res.tile4.nc  ${dest}/INPUT/fv_srf_wnd.res.tile4.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_srf_wnd.res.tile5.nc  ${dest}/INPUT/fv_srf_wnd.res.tile5.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_srf_wnd.res.tile6.nc  ${dest}/INPUT/fv_srf_wnd.res.tile6.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_tracer.res.tile1.nc  ${dest}/INPUT/fv_tracer.res.tile1.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_tracer.res.tile2.nc  ${dest}/INPUT/fv_tracer.res.tile2.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_tracer.res.tile3.nc  ${dest}/INPUT/fv_tracer.res.tile3.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_tracer.res.tile4.nc  ${dest}/INPUT/fv_tracer.res.tile4.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_tracer.res.tile5.nc  ${dest}/INPUT/fv_tracer.res.tile5.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.fv_tracer.res.tile6.nc  ${dest}/INPUT/fv_tracer.res.tile6.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.phy_data.tile1.nc  ${dest}/INPUT/phy_data.tile1.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.phy_data.tile2.nc  ${dest}/INPUT/phy_data.tile2.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.phy_data.tile3.nc  ${dest}/INPUT/phy_data.tile3.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.phy_data.tile4.nc  ${dest}/INPUT/phy_data.tile4.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.phy_data.tile5.nc  ${dest}/INPUT/phy_data.tile5.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.phy_data.tile6.nc  ${dest}/INPUT/phy_data.tile6.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.sfc_data.tile1.nc  ${dest}/INPUT/sfc_data.tile1.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.sfc_data.tile2.nc  ${dest}/INPUT/sfc_data.tile2.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.sfc_data.tile3.nc  ${dest}/INPUT/sfc_data.tile3.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.sfc_data.tile4.nc  ${dest}/INPUT/sfc_data.tile4.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.sfc_data.tile5.nc  ${dest}/INPUT/sfc_data.tile5.nc
cp ${sorc}/RESTART/201610${day}.${hour}0000.sfc_data.tile6.nc  ${dest}/INPUT/sfc_data.tile6.nc
