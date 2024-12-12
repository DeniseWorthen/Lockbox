#!/bin/bash

set -x
year="2021"
mon="11"
day="12"
hour="06"
secs="21600"

datetype1=${year}${mon}${day}.${hour}"0000"
datetype2=${year}-${mon}-${day}-${hour}
datetype3=${year}-${mon}-${day}-${secs}

sorc="/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/rt_24630/longrun1"
dest="/scratch1/NCEPDEV/stmp2/Denise.Worthen/freerun/RESTART/"

#FV3 restarts
cp ${sorc}/RESTART/${datetype1}.coupler.res  ${dest}
cp ${sorc}/RESTART/${datetype1}.* ${dest}

##MOM restarts
cp ${sorc}/RESTART/MOM.res.${datetype2}-00-00.nc ${dest}

#CICE restarts
cp ${sorc}/RESTART/iced.${datetype3}.nc ${dest}

#CMEPS restarts
cp ${sorc}/RESTART/ufs.cpld.cpl.r.${datetype3}.nc ${dest}
