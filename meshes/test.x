#!/bin/bash

set -x

export CPLWAV=.true.

export OCNRES=100
#export WAVDOMAIN=mx${OCNRES}
export WAVDOMAIN=glo_30m
export MESH_WAV=mesh.${WAVDOMAIN}.nc

  #if [[ "${WAVDOMAIN:0:2}" == mx ]]; then
  if [[ ${WAVDOMAIN:0:2} != mx ]]; then
    test=mod_def.$WAVDOMAIN
  fi

export INPUTDATA_ROOT_WW3=testpath
#if [[ $CPLWAV == .true. ]]; then
#  cp  @[INPUTDATA_ROOT_WW3]/mod_def.points .
#  cp  @[INPUTDATA_ROOT_WW3]/@[MESH_WAV] .
#  if [[ $WAVDOMAIN == mx025 ]]; then
#     cp  @[INPUTDATA_ROOT_WW3]/mod_def.@[WAVDOMAIN]lite mod_def.ww3
#  else
#     cp  @[INPUTDATA_ROOT_WW3]/mod_def.@[WAVDOMAIN] mod_def.ww3
#  fi
#fi
