#!/bin/bash

set -x

#src=dev
#grep 'arwt heat error'     resid.${src}/ice_diag.d | awk '{print $5"  "$6}'>heat.${src}.dat
#grep 'arwt salt flx error' resid.${src}/ice_diag.d | awk '{print $6"  "$7}'>salt.${src}.dat

#src=ec
#grep 'arwt heat error'     resid.${src}/ice_diag.d | awk '{print $5"  "$6}'>heat.${src}.dat
#grep 'arwt salt flx error' resid.${src}/ice_diag.d | awk '{print $6"  "$7}'>salt.${src}.dat

#src=ec.mod
src=zap
grep 'arwt heat error'     resid.${src}/ice_diag.d | awk '{print $5"  "$6}'>heat.${src}.dat
grep 'arwt salt flx error' resid.${src}/ice_diag.d | awk '{print $6"  "$7}'>salt.${src}.dat
