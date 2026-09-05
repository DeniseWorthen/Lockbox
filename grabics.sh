#!/bin/bash

set -x

src=/scratch4/NCEPDEV/global/Yangxing.Zheng/ICs/CPC_land/C192mx025
dst=/scratch3/NCEPDEV/stmp/Denise.Worthen/sfs_ics

#---------------
# 01-jan-2021
#---------------
# date1=20210101
# date2=20201231

# atmic=$src/sfs.$date1/00/mem000/model/atmos/input
# ocnic=$src/sfs.$date2/18/mem000/model/ocean/restart
# iceic=$src/sfs.$date2/18/mem000/model/ice/restart

# cp $atmic/* $dst/$date1
# cp $ocnic/* $dst/$date1
# cp $iceic/* $dst/$date1

#---------------
# 01-apr-2021
#---------------
date1=20210401
date2=20210331

atmic=$src/sfs.$date1/00/mem000/model/atmos/input
ocnic=$src/sfs.$date2/18/mem000/model/ocean/restart
iceic=$src/sfs.$date2/18/mem000/model/ice/restart

cp $atmic/* $dst/$date1
cp $ocnic/* $dst/$date1
cp $iceic/* $dst/$date1

#---------------
# 01-jul-2021
#---------------
date1=20210701
date2=20210630

atmic=$src/sfs.$date1/00/mem000/model/atmos/input
ocnic=$src/sfs.$date2/18/mem000/model/ocean/restart
iceic=$src/sfs.$date2/18/mem000/model/ice/restart

cp $atmic/* $dst/$date1
cp $ocnic/* $dst/$date1
cp $iceic/* $dst/$date1

#---------------
# 01-oct-2021
#---------------
date1=20211001
date2=20210930

atmic=$src/sfs.$date1/00/mem000/model/atmos/input
ocnic=$src/sfs.$date2/18/mem000/model/ocean/restart
iceic=$src/sfs.$date2/18/mem000/model/ice/restart

cp $atmic/* $dst/$date1
cp $ocnic/* $dst/$date1
cp $iceic/* $dst/$date1
