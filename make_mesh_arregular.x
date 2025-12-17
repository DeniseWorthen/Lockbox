#!/bin/bash

set -x

# Define spacing value
dx=$(echo "scale=10; 1/12" | bc)
slon=-210
elon=-50

slat=-20
elat=70

# Calculate snwe bounds

ll_lat=$(echo "$slat - $dx/2" | bc -l)
ur_lat=$(echo "$elat + $dx/2" | bc -l)

ll_lon=$(echo "$slon - $dx/2" | bc -l)
ur_lon=$(echo "$elon + $dx/2" | bc -l)

#atmos river mom6 domain
ncremap -g ar_grid.SCRIP.nc -G latlon=1081,1920#snwe=$ll_lat,$ur_lat,$ll_lon,$ur_lon#lat_typ=uni
