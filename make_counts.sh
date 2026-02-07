#!/bin/bash

set -x

file=test.dev.nc

ncks -v aice_h ${file} tmp.nc
ncap2 -s "count=0*aice_h;where(aice_h > 0.0 && aice_h <= 1.0e-6)count=1" tmp.nc tmp.nc
