#!/bin/bash

set -x

#cmd=mv
cmd=cp


#src="cpld_control_pdlib_p8"
#dst="nmlbase"

#src=/scratch4/NCEPDEV/nems/Daniel.Sarmiento/WORKING_c1152/cpld_control_c1152_v17_intel/
#dst=nml.dan
src=/scratch4/NCEPDEV/stmp/Denise.Worthen/RT_RUNDIRS/Denise.Worthen/FV3_RT/rt_3121544/cpld_debug_gfsv17_intel/
dst=rt.gfsv17
#src=rt1152
#dst=ursa.runs

mkdir -p $dst
$cmd $src/input.nml $dst
$cmd $src/model_configure $dst
$cmd $src/ice_in $dst
$cmd $src/ufs.configure $dst
$cmd $src/job_card $dst
$cmd $src/data_table $dst
$cmd $src/diag_table $dst
$cmd $src/field_table $dst
$cmd $src/noahmptable.tbl $dst
# src="cpld_restart_pdlib_p8"
# dst="nmlrest"

# mkdir -p $dst
# mv $src/input.nml $dst
# mv $src/model_configure $dst
# mv $src/ice_in $dst
# mv $src/ufs.configure $dst
