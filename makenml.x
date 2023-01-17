#!/bin/bash

set -x

src="cpld_control_pdlib_p8"
dst="nmlbase"

mkdir -p $dst
cp $src/input.nml $dst
cp $src/model_configure $dst
cp $src/ice_in $dst
cp $src/nems.configure $dst

src="cpld_restart_pdlib_p8"
dst="nmlrest"

mkdir -p $dst
cp $src/input.nml $dst
cp $src/model_configure $dst
cp $src/ice_in $dst
cp $src/nems.configure $dst
