#!/bin/bash

set -x

#tst1="/scratch1/NCEPDEV/nems/emc.nemspara/RT/FV3-MOM6-CICE5/develop-20200530/RT-Baselines_cold_bmrt_ccpp_cmeps/RESTART"
#tst1="/scratch1/NCEPDEV/stmp2/Jiande.Wang/MOM6-update-RT-run/RT-run-rt_28588/cpld_fv3_ccpp_384_mom6_cice_cmeps_cold_bmark_rt/RESTART"
#tst1="/scratch1/NCEPDEV/stmp2/Denise.Worthen/S2S_RT/TOD/cpld_fv3_ccpp_384_mom6_cice_cmeps_cold_bmark_rt/RESTART"

tst1="/scratch1/NCEPDEV/stmp2/Denise.Worthen/S2S_RT/TOD/cpld_fv3_ccpp_384_mom6_cice_cmeps_cold_bmark_rt"
tst2="/scratch1/NCEPDEV/stmp2/Denise.Worthen/S2S_RT/UPD/cpld_fv3_ccpp_384_mom6_cice_cmeps_cold_bmark_rt"
mtime="00450"

#tst1="/scratch1/NCEPDEV/nems/emc.nemspara/RT/FV3-MOM6-CICE5/develop-20200530/MEDIATOR_bmwav_ccpp_cmeps"
#tst2="/scratch1/NCEPDEV/stmp2/Denise.Worthen/S2S_RT/UPD/cpld_fv3_ccpp_384_mom6_cice_cmeps_cold_bmark_rt"
#mtime="03600"


#./cprnc -m ${tst1}/fv_core.res.nc  ${tst2}/fv_core.res.nc  | grep RMS
#./cprnc -m ${tst1}/fv_core.res.tile1.nc  ${tst2}/fv_core.res.tile1.nc  | grep RMS
#./cprnc -m ${tst1}/fv_core.res.tile2.nc  ${tst2}/fv_core.res.tile2.nc  | grep RMS
#./cprnc -m ${tst1}/fv_core.res.tile3.nc  ${tst2}/fv_core.res.tile3.nc  | grep RMS
#./cprnc -m ${tst1}/fv_core.res.tile4.nc  ${tst2}/fv_core.res.tile4.nc  | grep RMS
#./cprnc -m ${tst1}/fv_core.res.tile5.nc  ${tst2}/fv_core.res.tile5.nc  | grep RMS
#./cprnc -m ${tst1}/fv_core.res.tile6.nc  ${tst2}/fv_core.res.tile6.nc  | grep RMS

#./cprnc -m ${tst1}/fv_srf_wnd.res.tile1.nc  ${tst2}/fv_srf_wnd.res.tile1.nc  | grep RMS
#./cprnc -m ${tst1}/fv_srf_wnd.res.tile2.nc  ${tst2}/fv_srf_wnd.res.tile2.nc  | grep RMS
#./cprnc -m ${tst1}/fv_srf_wnd.res.tile3.nc  ${tst2}/fv_srf_wnd.res.tile3.nc  | grep RMS
#./cprnc -m ${tst1}/fv_srf_wnd.res.tile4.nc  ${tst2}/fv_srf_wnd.res.tile4.nc  | grep RMS
#./cprnc -m ${tst1}/fv_srf_wnd.res.tile5.nc  ${tst2}/fv_srf_wnd.res.tile5.nc  | grep RMS
#./cprnc -m ${tst1}/fv_srf_wnd.res.tile6.nc  ${tst2}/fv_srf_wnd.res.tile6.nc  | grep RMS

#./cprnc -m ${tst1}/fv_tracer.res.tile1.nc  ${tst2}/fv_tracer.res.tile1.nc  | grep RMS
#./cprnc -m ${tst1}/fv_tracer.res.tile2.nc  ${tst2}/fv_tracer.res.tile2.nc  | grep RMS
#./cprnc -m ${tst1}/fv_tracer.res.tile3.nc  ${tst2}/fv_tracer.res.tile3.nc  | grep RMS
#./cprnc -m ${tst1}/fv_tracer.res.tile4.nc  ${tst2}/fv_tracer.res.tile4.nc  | grep RMS
#./cprnc -m ${tst1}/fv_tracer.res.tile5.nc  ${tst2}/fv_tracer.res.tile5.nc  | grep RMS
#./cprnc -m ${tst1}/fv_tracer.res.tile6.nc  ${tst2}/fv_tracer.res.tile6.nc  | grep RMS

#./cprnc -m ${tst1}/phy_data.tile1.nc  ${tst2}/phy_data.tile1.nc  | grep RMS
#./cprnc -m ${tst1}/phy_data.tile2.nc  ${tst2}/phy_data.tile2.nc  | grep RMS
#./cprnc -m ${tst1}/phy_data.tile3.nc  ${tst2}/phy_data.tile3.nc  | grep RMS
#./cprnc -m ${tst1}/phy_data.tile4.nc  ${tst2}/phy_data.tile4.nc  | grep RMS
#./cprnc -m ${tst1}/phy_data.tile5.nc  ${tst2}/phy_data.tile5.nc  | grep RMS
#./cprnc -m ${tst1}/phy_data.tile6.nc  ${tst2}/phy_data.tile6.nc  | grep RMS

#./cprnc -m ${tst1}/sfc_data.tile1.nc  ${tst2}/sfc_data.tile1.nc  | grep RMS
#./cprnc -m ${tst1}/sfc_data.tile2.nc  ${tst2}/sfc_data.tile2.nc  | grep RMS
#./cprnc -m ${tst1}/sfc_data.tile3.nc  ${tst2}/sfc_data.tile3.nc  | grep RMS
#./cprnc -m ${tst1}/sfc_data.tile4.nc  ${tst2}/sfc_data.tile4.nc  | grep RMS
#./cprnc -m ${tst1}/sfc_data.tile5.nc  ${tst2}/sfc_data.tile5.nc  | grep RMS
#./cprnc -m ${tst1}/sfc_data.tile6.nc  ${tst2}/sfc_data.tile6.nc  | grep RMS

./cprnc -m ${tst1}/ufs.s2s.cold.cpl.hi.2013-04-01-${mtime}.nc  ${tst2}/ufs.s2s.cold.cpl.hi.2013-04-01-${mtime}.nc  
#./cprnc -m ${tst1}/ufs.s2s.cold.cpl.r.2013-04-01-${mtime}.nc  ${tst2}/ufs.s2s.cold.cpl.r.2013-04-01-${mtime}.nc  
