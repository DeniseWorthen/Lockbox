#!/bin/bash

set -x

#src=base.dev
#dst=base

src=rest.dev
dst=rest

# after mod_def
cmp $src/fort.910 $dst/fort.910
cmp $src/fort.920 $dst/fort.920

# in restart read
cmp $src/fort.4010 $dst/fort.4010
cmp $src/fort.4020 $dst/fort.4020

# after restart read
cmp $src/fort.2010 $dst/fort.2010
cmp $src/fort.2020 $dst/fort.2020

# at end of w3init
cmp $src/fort.3010 $dst/fort.3010
cmp $src/fort.3020 $dst/fort.3020

# at history write
cmp $src/fort.1010 $dst/fort.1010
cmp $src/fort.1020 $dst/fort.1020

#history file
cmp $src/20210322.121200.out_grd.ww3 $dst/20210322.121200.out_grd.ww3
