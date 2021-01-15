#!/bin/bash

set -x

SYEAR="2016"
SMONTH="10"
SDAY="03"

RESTART_INTERVAL=12
RESTART_INTERVAL_SECS=$(printf "%02d" $(( RESTART_INTERVAL*3600)))
RESTART_FILE_PREFIX="${SYEAR}${SMONTH}${SDAY}.$(printf "%02d" $(( RESTART_INTERVAL  )))0000"

sorc="/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/rt_270283/cpld_control_prod"
dest="/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/test_restart"

    cp -r ${sorc}/RESTART/${RESTART_FILE_PREFIX}.* ${dest}/INPUT
    #cp -r ${sorc}/RESTART/* ${dest}/INPUT
    rm -f INPUT/fv_core.res.*
    rm -f INPUT/fv_srf_wnd.res.*
    rm -f INPUT/fv_tracer.res.*
    rm -f INPUT/phy_data.*c
    rm -f INPUT/srf_data.*
    for RFILE in ${sorc}/RESTART/${RESTART_FILE_PREFIX}.*; do
      [ -e $RFILE ] || exit 1
      RFILE_OLD=$(basename $RFILE)
      RFILE_NEW="${RFILE_OLD//${RESTART_FILE_PREFIX}./}"
      cp $RFILE ${dest}/INPUT/${RFILE_NEW}
    done

    #if not mx025, then mom6 restart is a single file
    RESTART_FILE_SUFFIX="2016-10-03-"$(printf "%02d" $(( RESTART_INTERVAL )))
    cp -r ${sorc}/RESTART/MOM.res.${RESTART_FILE_SUFFIX}-00-00.nc ${dest}/INPUT/MOM.res.nc

    RESTART_FILE_SUFFIX="2016-10-03-"$(printf "%02d" $(( RESTART_INTERVAL_SECS )))
    # CMEPS restart and pointer files
    RFILE="ufs.cpld.cpl.r.${RESTART_FILE_SUFFIX}.nc"
    cp  ${sorc}/RESTART/${RFILE} .
    ls -1 ${RFILE}>${dest}/rpointer.cpl

    # CICE restart and pointer files
    RFILE="iced.${RESTART_FILE_SUFFIX}.nc"
    cp  ${sorc}/RESTART/${RFILE} ${dest}/INPUT
    ls -1 "${dest}/INPUT/"${RFILE}>ice.restart_file
