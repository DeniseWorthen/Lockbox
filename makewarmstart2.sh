#!/bin/bash

set -x
year="2021"
mon="03"
day="23"
hour="06"
secs="21600"


datetype1=${year}${mon}${day}.${hour}"0000"
datetype2=${year}-${mon}-${day}-${hour}
datetype3=${year}-${mon}-${day}-${secs}

inputroot=/gpfs/f6/infra-cpu/world-shared/Denise.Worthen/input-data-2025XXXX/

for exp in c48mx500 c24mx500 c24mx900 c12mx900; do

    case $exp in
        ##C48-5deg
        c48mx500)
            ares=48
            ores=500
            tres=5deg
            levels=127
            ;;
        c24mx500)
            ##C24-5deg
           ares=24
           ores=500
           tres=5deg
           levels=41
           ;;
        c24mx900)
            ##C24-9deg
            ares=24
            ores=900
            tres=9deg
            levels=41
            ;;
        c12mx900)
            ##C12-9deg
            ares=12
            ores=900
            tres=9deg
            levels=41
    esac

    # From control run
    #sorc=/gpfs/f6/infra-cpu/proj-shared/Denise.Worthen/bl.sfs.51689a69/cpld_control_c${ares}_${tres}_intel
    #sorc=/gpfs/f6/infra-cpu/proj-shared/Denise.Worthen/RT_BASELINE/Denise.Worthen/FV3_RT/REGRESSION_TEST/cpld_control_c${ares}_${tres}_intel
    sorc=/gpfs/f6/infra-cpu/proj-shared/Denise.Worthen/RT_RUNDIRS/Denise.Worthen/FV3_RT/rt_2670756/cpld_control_c${ares}_${tres}_intel

    # MOM6
    dest=${inputroot}/MOM6_IC/C${ares}mx${ores}/2021032306
    #mkdir -p ${dest}
    #cp ${sorc}/RESTART/${datetype1}.MOM.res.nc  ${dest}/MOM.res.nc

    # CICE6
    dest=${inputroot}/CICE_IC/C${ares}mx${ores}/2021032306
    #mkdir -p ${dest}
    #cp ${sorc}/RESTART/iced.${datetype3}.nc ${dest}/iced.${datetype3}.nc
    #echo INPUT/iced.${datetype3}.nc>${dest}/ice.restart_file

    # CMEPS
    dest=${inputroot}/CMEPS_IC/C${ares}mx${ores}/2021032306
    #mkdir -p ${dest}
    #cp ${sorc}/RESTART/ufs.cpld.cpl.r.${datetype3}.nc ${dest}
    #echo ufs.cpld.cpl.r.${datetype3}.nc>${dest}/rpointer.cpl

    #WW3
    dest=${inputroot}/WW3_IC/C${ares}mx${ores}/2021032306
    #mkdir -p ${dest}
    #cp ${sorc}/ufs.cpld.ww3.r.${datetype3}.nc ${dest}

    # #FV3 restarts
    dest=${inputroot}/FV3_input_data${ares}/INPUT_L${levels}_mx${ores}/2021032306
    #mkdir -p ${dest}

    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.nc  ${dest}/fv_core.res.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile1.nc  ${dest}/fv_core.res.tile1.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile2.nc  ${dest}/fv_core.res.tile2.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile3.nc  ${dest}/fv_core.res.tile3.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile4.nc  ${dest}/fv_core.res.tile4.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile5.nc  ${dest}/fv_core.res.tile5.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_core.res.tile6.nc  ${dest}/fv_core.res.tile6.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile1.nc  ${dest}/fv_srf_wnd.res.tile1.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile2.nc  ${dest}/fv_srf_wnd.res.tile2.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile3.nc  ${dest}/fv_srf_wnd.res.tile3.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile4.nc  ${dest}/fv_srf_wnd.res.tile4.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile5.nc  ${dest}/fv_srf_wnd.res.tile5.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_srf_wnd.res.tile6.nc  ${dest}/fv_srf_wnd.res.tile6.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile1.nc  ${dest}/fv_tracer.res.tile1.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile2.nc  ${dest}/fv_tracer.res.tile2.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile3.nc  ${dest}/fv_tracer.res.tile3.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile4.nc  ${dest}/fv_tracer.res.tile4.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile5.nc  ${dest}/fv_tracer.res.tile5.nc
    # cp ${sorc}/RESTART/${datetype1}.fv_tracer.res.tile6.nc  ${dest}/fv_tracer.res.tile6.nc
    # cp ${sorc}/RESTART/${datetype1}.phy_data.tile1.nc  ${dest}/phy_data.tile1.nc
    # cp ${sorc}/RESTART/${datetype1}.phy_data.tile2.nc  ${dest}/phy_data.tile2.nc
    # cp ${sorc}/RESTART/${datetype1}.phy_data.tile3.nc  ${dest}/phy_data.tile3.nc
    # cp ${sorc}/RESTART/${datetype1}.phy_data.tile4.nc  ${dest}/phy_data.tile4.nc
    # cp ${sorc}/RESTART/${datetype1}.phy_data.tile5.nc  ${dest}/phy_data.tile5.nc
    # cp ${sorc}/RESTART/${datetype1}.phy_data.tile6.nc  ${dest}/phy_data.tile6.nc
    # cp ${sorc}/RESTART/${datetype1}.sfc_data.tile1.nc  ${dest}/sfc_data.tile1.nc
    # cp ${sorc}/RESTART/${datetype1}.sfc_data.tile2.nc  ${dest}/sfc_data.tile2.nc
    # cp ${sorc}/RESTART/${datetype1}.sfc_data.tile3.nc  ${dest}/sfc_data.tile3.nc
    # cp ${sorc}/RESTART/${datetype1}.sfc_data.tile4.nc  ${dest}/sfc_data.tile4.nc
    # cp ${sorc}/RESTART/${datetype1}.sfc_data.tile5.nc  ${dest}/sfc_data.tile5.nc
    # cp ${sorc}/RESTART/${datetype1}.sfc_data.tile6.nc  ${dest}/sfc_data.tile6.nc

done
