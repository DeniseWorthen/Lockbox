#!/bin/bash

set -x
year="2004"
mon="06"
day="15"
hour="00"
secs="00000"

datetype1=${year}${mon}${day}.${hour}"0000"
datetype2=${year}-${mon}-${day}-${hour}
datetype3=${year}-${mon}-${day}-${secs}

#sorc="/work2/noaa/stmp/dworthen/stmp/dworthen/negdvice/base.2d"
#dest="/work2/noaa/stmp/dworthen/stmp/dworthen/negdvice/rest.2d"

sorc="/work2/noaa/stmp/dworthen/stmp/dworthen/negdvice/base"
dest="/work2/noaa/stmp/dworthen/stmp/dworthen/negdvice/rest.test"

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
cp ${sorc}/RESTART/${datetype1}.atm_stoch.res.nc  ${dest}/INPUT/atm_stoch.res.nc
cp ${sorc}/RESTART/${datetype1}.ocn_stoch.res.nc  ${dest}/INPUT/ocn_stoch.res.nc

##MOM restarts
cp ${sorc}/MOM6_RESTART/${datetype1}.MOM.res.nc ${dest}/INPUT/MOM.res.nc
cp ${sorc}/MOM6_RESTART/${datetype1}.MOM.res_1.nc ${dest}/INPUT/MOM.res_1.nc
cp ${sorc}/MOM6_RESTART/${datetype1}.MOM.res_2.nc ${dest}/INPUT/MOM.res_2.nc
cp ${sorc}/MOM6_RESTART/${datetype1}.MOM.res_3.nc ${dest}/INPUT/MOM.res_3.nc

#CICE restarts
cp ${sorc}/CICE_RESTART/cice_model.res.${datetype3}.nc ${dest}/INPUT/iced.${datetype3}.nc
ls -1 ${dest}/INPUT/iced.${datetype3}.nc>${dest}/ice.restart_file

cp ${sorc}/CMEPS_RESTART/ufs.cpld.cpl.r.${datetype3}.nc ${dest}
ls -1 ${dest}/ufs.cpld.cpl.r.${datetype3}.nc>${dest}/rpointer.cpl
