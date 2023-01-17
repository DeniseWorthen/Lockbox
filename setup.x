#!/bin/bash

set -x

runtype="imp"
grdname=".trip1degmask"

base="base".${runtype}
rest="rest".${runtype}
moddef="mod_def."${runtype}${grdname}

cp -r rundir/ ${base}
cp moddefs/${moddef} ${base}/mod_def.ww3
cp "nmlbase/"* ${base}

cp -r rundir/ ${rest}
cp moddefs/${moddef} ${rest}/mod_def.ww3
cp "nmlrest/"* ${rest}
