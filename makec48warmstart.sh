#!/bin/bash

set -x
year="2021"
mon="03"
day="23"
hour="06"
secs="64800"

datetype1=${year}${mon}${day}.${hour}"0000"
datetype2=${year}-${mon}-${day}-${hour}
datetype3=${year}-${mon}-${day}-${secs}

sorc="/scratch1/NCEPDEV/nems/Nick.Szapiro/tasks/warmStart_c48-5deg/IC"
dest="/scratch1/NCEPDEV/stmp4/Denise.Worthen/input-data_20240501/FV3_input_data48/INPUT_L127_mx500/2021032306"

#FV3 restarts
#cp ${sorc}/${datetype1}.coupler.res  ${dest}/coupler.res
cp ${sorc}/${datetype1}.fv_core.res.nc  ${dest}/fv_core.res.nc
cp ${sorc}/${datetype1}.fv_core.res.tile1.nc  ${dest}/fv_core.res.tile1.nc
cp ${sorc}/${datetype1}.fv_core.res.tile2.nc  ${dest}/fv_core.res.tile2.nc
cp ${sorc}/${datetype1}.fv_core.res.tile3.nc  ${dest}/fv_core.res.tile3.nc
cp ${sorc}/${datetype1}.fv_core.res.tile4.nc  ${dest}/fv_core.res.tile4.nc
cp ${sorc}/${datetype1}.fv_core.res.tile5.nc  ${dest}/fv_core.res.tile5.nc
cp ${sorc}/${datetype1}.fv_core.res.tile6.nc  ${dest}/fv_core.res.tile6.nc
cp ${sorc}/${datetype1}.fv_srf_wnd.res.tile1.nc  ${dest}/fv_srf_wnd.res.tile1.nc
cp ${sorc}/${datetype1}.fv_srf_wnd.res.tile2.nc  ${dest}/fv_srf_wnd.res.tile2.nc
cp ${sorc}/${datetype1}.fv_srf_wnd.res.tile3.nc  ${dest}/fv_srf_wnd.res.tile3.nc
cp ${sorc}/${datetype1}.fv_srf_wnd.res.tile4.nc  ${dest}/fv_srf_wnd.res.tile4.nc
cp ${sorc}/${datetype1}.fv_srf_wnd.res.tile5.nc  ${dest}/fv_srf_wnd.res.tile5.nc
cp ${sorc}/${datetype1}.fv_srf_wnd.res.tile6.nc  ${dest}/fv_srf_wnd.res.tile6.nc
cp ${sorc}/${datetype1}.fv_tracer.res.tile1.nc  ${dest}/fv_tracer.res.tile1.nc
cp ${sorc}/${datetype1}.fv_tracer.res.tile2.nc  ${dest}/fv_tracer.res.tile2.nc
cp ${sorc}/${datetype1}.fv_tracer.res.tile3.nc  ${dest}/fv_tracer.res.tile3.nc
cp ${sorc}/${datetype1}.fv_tracer.res.tile4.nc  ${dest}/fv_tracer.res.tile4.nc
cp ${sorc}/${datetype1}.fv_tracer.res.tile5.nc  ${dest}/fv_tracer.res.tile5.nc
cp ${sorc}/${datetype1}.fv_tracer.res.tile6.nc  ${dest}/fv_tracer.res.tile6.nc
cp ${sorc}/${datetype1}.phy_data.tile1.nc  ${dest}/phy_data.tile1.nc
cp ${sorc}/${datetype1}.phy_data.tile2.nc  ${dest}/phy_data.tile2.nc
cp ${sorc}/${datetype1}.phy_data.tile3.nc  ${dest}/phy_data.tile3.nc
cp ${sorc}/${datetype1}.phy_data.tile4.nc  ${dest}/phy_data.tile4.nc
cp ${sorc}/${datetype1}.phy_data.tile5.nc  ${dest}/phy_data.tile5.nc
cp ${sorc}/${datetype1}.phy_data.tile6.nc  ${dest}/phy_data.tile6.nc
cp ${sorc}/${datetype1}.sfc_data.tile1.nc  ${dest}/sfc_data.tile1.nc
cp ${sorc}/${datetype1}.sfc_data.tile2.nc  ${dest}/sfc_data.tile2.nc
cp ${sorc}/${datetype1}.sfc_data.tile3.nc  ${dest}/sfc_data.tile3.nc
cp ${sorc}/${datetype1}.sfc_data.tile4.nc  ${dest}/sfc_data.tile4.nc
cp ${sorc}/${datetype1}.sfc_data.tile5.nc  ${dest}/sfc_data.tile5.nc
cp ${sorc}/${datetype1}.sfc_data.tile6.nc  ${dest}/sfc_data.tile6.nc
cp ${sorc}/${datetype1}.ca_data.tile1.nc  ${dest}/ca_data.tile1.nc
cp ${sorc}/${datetype1}.ca_data.tile2.nc  ${dest}/ca_data.tile2.nc
cp ${sorc}/${datetype1}.ca_data.tile3.nc  ${dest}/ca_data.tile3.nc
cp ${sorc}/${datetype1}.ca_data.tile4.nc  ${dest}/ca_data.tile4.nc
cp ${sorc}/${datetype1}.ca_data.tile5.nc  ${dest}/ca_data.tile5.nc
cp ${sorc}/${datetype1}.ca_data.tile6.nc  ${dest}/ca_data.tile6.nc
#
##MOM restarts
#cp ${sorc}/RESTART/${datetype1}.MOM.res.nc  ${dest}/INPUT/MOM.res.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00.nc ${dest}/INPUT/MOM.res.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00_1.nc ${dest}/INPUT/MOM.res_1.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00_2.nc ${dest}/INPUT/MOM.res_2.nc
#cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00_3.nc ${dest}/INPUT/MOM.res_3.nc

#CICE restarts
#cp ${sorc}/RESTART/iced.${datetype3}.nc ${dest}/INPUT/iced.${datetype3}.nc
#ls -1 ${dest}/INPUT/iced.${datetype3}.nc>${dest}/ice.restart_file

#cp ${sorc}/RESTART/ufs.cpld.cpl.r.${datetype3}.nc ${dest}
#ls -1 ${dest}/ufs.cpld.cpl.r.${datetype3}.nc>${dest}/rpointer.cpl

#cp $sorc/ufs.cpld.ww3.r.${datetype3} ${dest}
