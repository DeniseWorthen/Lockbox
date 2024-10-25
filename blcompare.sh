#!/bin/bash

set -x

#cmd="cprnc -m "
cmd="nccmp -d -S -q -f -g -B --Attribute=checksum --warn=format"

src=/work/noaa/epic/hercules/UFS-WM_RT/NEMSfv3gfs/develop-20240904
dst=/work2/noaa/stmp/dworthen/stmp/dworthen/FV3_RT/rt_3449204

${cmd}   ${src}/atmwav_control_noaero_p8_intel/RESTART/ufs.atmw.cpl.r.2021-03-22-64800.nc	${dst}/atmwav_control_noaero_p8_intel/RESTART/ufs.atmw.cpl.r.2021-03-22-64800.nc
${cmd}   ${src}/cpld_bmark_p8_intel/RESTART/ufs.cpld.cpl.r.2013-04-01-21600.nc			${dst}/cpld_bmark_p8_intel/RESTART/ufs.cpld.cpl.r.2013-04-01-21600.nc
${cmd}   ${src}/cpld_control_c192_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-43200.nc		${dst}/cpld_control_c192_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-43200.nc
${cmd}   ${src}/cpld_control_c48_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_c48_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_c96_noaero_p8_gnu/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc	${dst}/cpld_control_c96_noaero_p8_gnu/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_c96_noaero_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc	${dst}/cpld_control_c96_noaero_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_ciceC_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_ciceC_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_gfsv17_iau_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-43200.nc	${dst}/cpld_control_gfsv17_iau_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-43200.nc
${cmd}   ${src}/cpld_control_gfsv17_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_gfsv17_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_noaero_p8_agrid_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc	${dst}/cpld_control_noaero_p8_agrid_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_noaero_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_noaero_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_p8.v2.sfc_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_p8.v2.sfc_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_p8_faster_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_p8_faster_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_p8_mixedmode_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc	${dst}/cpld_control_p8_mixedmode_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_pdlib_p8_gnu/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_pdlib_p8_gnu/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_control_pdlib_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc		${dst}/cpld_control_pdlib_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-21600.nc
${cmd}   ${src}/cpld_debug_gfsv17_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc		${dst}/cpld_debug_gfsv17_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc
${cmd}   ${src}/cpld_debug_noaero_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc		${dst}/cpld_debug_noaero_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc
${cmd}   ${src}/cpld_debug_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc			${dst}/cpld_debug_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc
${cmd}   ${src}/cpld_debug_pdlib_p8_gnu/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc		${dst}/cpld_debug_pdlib_p8_gnu/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc
${cmd}   ${src}/cpld_debug_pdlib_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc		${dst}/cpld_debug_pdlib_p8_intel/RESTART/ufs.cpld.cpl.r.2021-03-22-32400.nc
${cmd}   ${src}/cpld_warmstart_c48_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-43200.nc		${dst}/cpld_warmstart_c48_intel/RESTART/ufs.cpld.cpl.r.2021-03-23-43200.nc
