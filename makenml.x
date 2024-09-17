#!/bin/bash

set -x


src="cpld_control_pdlib_p8"
dst="nmlbase"

mkdir -p $dst
mv $src/input.nml $dst
mv $src/model_configure $dst
mv $src/ice_in $dst
mv $src/ufs.configure $dst

src="cpld_restart_pdlib_p8"
dst="nmlrest"

mkdir -p $dst
mv $src/input.nml $dst
mv $src/model_configure $dst
mv $src/ice_in $dst
mv $src/ufs.configure $dst
