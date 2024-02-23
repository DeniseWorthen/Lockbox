#!/bin/bash

cmd=~/cime/tools/cprnc/cprnc

set -x

rt="/lustre/f2/scratch/Denise.Worthen/FV3_RT/restarts/rm_caglobal"
#rt="/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT"

#fname="DATM_CFSR.cpl.hi.2011-10-01-25200.nc"
#fname="DATM_CFSR.cpl.hi.2011-10-01-28800.nc"
#fname="RESTART/DATM_CFSR.cpl.r.2011-10-01-43200.nc"

#fname="field_atm_exporta_2011-10-01T11:45:00.nc"

#fname="ufs.cpld.cpl.r.2016-10-04-00000.nc"
#fname="ufs.cpld.cpl.r.2013-04-02-00000.nc"
#fname="ufs.cpld.cpl.hi.2016-10-03-44100.nc"
#fname="ufs.cpld.cpl.hi.2016-10-03-11250.nc"
#fname="ufs.cpld.cpl.hi.2016-10-03-04500.nc"
#fname="ufs.cpld.cpl.hi.2016-10-03-05400.nc"

#dir="cpld_control_prod"
#dir="cpld_controlLAT_prod"
#dir="cpld_control_c192_prod"
#dir="cpld_control_c384_prod"
#dir="cpld_controlfrac_c384_prod"
#dir="cpld_bmark_wave_prod"
#dir="cpld_ca_prod"

#$cmd -m ${rt}/devtests/test/RESTART/${fname}  ${rt}/ciceupd/test/RESTART/${fname}
#$cmd -m ${rt}/updatecmeps/${dir}/RESTART/${fname}  ${rt}/perfcmeps/${dir}/RESTART/${fname}
#$cmd -m ${rt}/${dir}/RESTART/${fname}  ${rt}/${dir}/RESTART/${fname}

#$cmd -m ${rt}/cpld_control_prod/${fname}  ${rt}/cpld_restart_prod/${fname}
#$cmd -m ${rt}/cpld_controlLAT_prod/${fname}  ${rt}/cpld_restartLAT_prod/${fname}
#$cmd -m ${rt}/cpld_controlC_prod/${fname}  ${rt}/cpld_restartC_prod/${fname}

#ncdiff -O ${rt}/cpld_controlLAT_prod/${fname}  ${rt}/cpld_restartLAT_prod/${fname} diffLAT.nc
#ncdiff -O ${rt}/cpld_controlC_prod/${fname}  ${rt}/cpld_restartC_prod/${fname} diffC.nc

#$cmd -m ${rt}/cpld_control_c192_prod/${fname}  ${rt}/cpld_restart_c192_prod/${fname}
#$cmd -m ${rt}/cpld_control_c384_prod/${fname}  ${rt}/cpld_restart_c384_prod/${fname}

#$cmd -m ${rt}/restr/${fname}  ${rt}/hour12/${fname}

#$cmd -m ${rt}/datm_restart_cfsr/${fname}  ${rt}/datm_12h_cfsr/${fname}
#$cmd -m ${rt}/datm_restart_cfsr/RESTART/${fname}  ${rt}/datm_12h_cfsr/RESTART/${fname}
#ncdiff -O ${rt}/datm_restart_cfsr/RESTART/${fname}  ${rt}/datm_12h_cfsr/RESTART/${fname} diff.nc

#ncdiff ${rt}/devtests/test/RESTART/${fname}  ${rt}/ciceupd/test/RESTART/${fname} diff.nc

#ncdiff -O ${rt}/cpld_control_c384_prod/${fname}  ${rt}/cpld_restart_c384_prod/${fname} diff.nc

fname="ufs.cpld.cpl.hi.2021-03-22-65520."
for tile in "tile1" "tile2" "tile3" "tile4" "tile5" "tile6"; do
 $cmd -m ${rt}/base/${fname}${tile}.nc  ${rt}/rest/${fname}${tile}.nc | grep RMS
done
