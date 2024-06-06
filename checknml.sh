#!/bin/bash

set -x

src=/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/devcice/cpld_control_nowave_noaero_p8_intel
#src=cpld.rundir
#dst=test.jday
#dst=base.nsst
#dst=cpld.rundir
dst=/scratch1/NCEPDEV/stmp2/Nick.Szapiro/FV3_RT/rt_190832/cpld_s2sa_p8_intel
#src=/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/use_inst/datm_cdeps_control_cfsr_intel
#dst=/scratch1/NCEPDEV/stmp2/Hae-Cheol.Kim/FV3_RT/GLBb0.08_061_gfs


diff $src/input.nml $dst/input.nml
diff $src/model_configure $dst/model_configure
diff $src/ufs.configure $dst/ufs.configure
diff $src/ice_in $dst/ice_in
#diff $src/ww3_shel.nml $dst/ww3_shel.nml
diff $src/job_card $dst/job_card
diff $src/INPUT/MOM_input $dst/INPUT/MOM_input
#diff $src/datm.streams $dst/datm.streams
diff $src/fd_ufs.yaml $dst/fd_ufs.yaml
