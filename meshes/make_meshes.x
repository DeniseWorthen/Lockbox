#!/bin/bash

set -x

#hafs
 ncremap -g hafswav.SCRIP.nc -G latlon=441,901#snwe=1.45,45.55,-98.05,-7.95#lat_typ=uni#lat_drc=s2n
 #ESMF_Scrip2Unstruct hafswav.SCRIP.nc mesh.hafs.nc 0
 srun -A nems -n 1 ESMF_Scrip2Unstruct hafswav.SCRIP.nc mesh.hafs.nc 0
 
#glo_1deg rectilinear
#0:359, -85:85
 ncremap -g glo_1deg.SCRIP.nc -G latlon=171,360#snwe=-85.5,85.5,-0.5,359.5#lat_typ=uni#lat_drc=s2n
 #ESMF_Scrip2Unstruct glo_1deg.SCRIP.nc mesh.glo_1deg.nc 0
 srun -A nems -n 1 ESMF_Scrip2Unstruct glo_1deg.SCRIP.nc mesh.glo_1deg.nc 0

#glo_30m rectilinear
 0:359.5, -80:80
 ncremap -g glo_30m.SCRIP.nc -G latlon=321,720#snwe=-80.25,80.25,-0.25,359.75#lat_typ=uni#lat_drc=s2n
#ESMF_Scrip2Unstruct glo_30m.SCRIP.nc mesh.glo_30m.nc 0
 srun -A nems -n 1 ESMF_Scrip2Unstruct glo_30m.SCRIP.nc mesh.glo_30m.nc 0
 
#gwes_30m rectilinear
 0:359.5, -80:80
 ncremap -g gwes_30m.SCRIP.nc -G latlon=321,720#snwe=-80.25,80.25,-0.25,359.75#lat_typ=uni#lat_drc=s2n
#ESMF_Scrip2Unstruct gwes_30m.SCRIP.nc mesh.gwes_30m.nc 0
 srun -A nems -n 1 ESMF_Scrip2Unstruct gwes_30m.SCRIP.nc mesh.gwes_30m.nc 0
