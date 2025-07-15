#!/bin/bash

set -x

src=gefs.rundir
dst=mem001
#src=testnml
#dst=test.db.2

diff $src/input.nml $dst/input.nml
diff $src/model_configure $dst/model_configure
diff $src/ufs.configure $dst/ufs.configure
diff $src/ice_in $dst/ice_in
#diff $src/ww3_shel.nml $dst/ww3_shel.nml
diff $src/job_card $dst/job_card
diff $src/INPUT/MOM_input $dst/INPUT/MOM_input
#diff $src/datm.streams $dst/datm.streams
diff $src/fd_ufs.yaml $dst/fd_ufs.yaml
