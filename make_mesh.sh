#!/bin/bash
set -euo pipefail

# Center grid endpoints (from grid_xt/grid_yt)
slon=0
elon=359.0625
slat=89.2767128781058
elat=-89.2767128781058

# Grid size
nlon=384
nlat=190

# Compute grid spacing from center points
dx=$(echo "($elon-$slon)/($nlon-1)" | bc -l)
dy=$(echo "($elat-$slat)/($nlat-1)" | bc -l)

# Compute SNWE bounds (cell corners)
ll_lat=$(echo "$slat - ($dy/2)" | bc -l)
ur_lat=$(echo "$elat + ($dy/2)" | bc -l)
ll_lon=$(echo "$slon - ($dx/2)" | bc -l)
ur_lon=$(echo "$elon + ($dx/2)" | bc -l)

# Build SCRIP grid
ncremap -g output.scrip.nc -G latlon=${nlat},${nlon}#snwe=${ll_lat},${ur_lat},${ll_lon},${ur_lon}

# Convert SCRIP grid to unstructured ESMF mesh
OUTDIR_PATH=${OUTDIR_PATH:-.}
FSRC=${OUTDIR_PATH}/output.scrip.nc
FDST=${OUTDIR_PATH}/datm.mesh.${nlon}x${nlat}.nc
srun -n 1 ESMF_Scrip2Unstruct ${FSRC} ${FDST} 0
