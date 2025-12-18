#!/bin/bash

# Cutout a regional subset of the global ocean_hgrid.nc file
# Uses NetCDF subsetting to extract indices i1:i2, j1:j2

set -ex

i1=1898
i2=5897
j1=2500
j2=5403
i2p1=$((i2+1))
j2p1=$((j2+1))

SOURCE_DIR='/scratch3/NCEPDEV/global/role.glopara/fix/mom6/20250128/008'
OUTDIR=${OUTDIR:-.}

SOURCE_FILE=ocean_hgrid.nc
OUTPUT_FILE="ocean_hgrid_regional.nc"

# Verify source file exists
if [ ! -f "${SOURCE_DIR}/${SOURCE_FILE}" ]; then
    echo "Error: Source file not found: ${SOURCE_DIR}/${SOURCE_FILE}"
    exit 1
fi

# Extract regional subset
ncks -F -d nx,${i1},${i2} -d ny,${j1},${j2} -d nxp,${i1},${i2p1} -d nyp,${j1},${j2p1} "${SOURCE_DIR}/${SOURCE_FILE}" "${OUTDIR}/${OUTPUT_FILE}" || {
    echo "Error: ncks command failed"
    exit 1
}

echo "Successfully created ${OUTDIR}/${OUTPUT_FILE}"

npx=$(((i2-i1)+1)/2)
npy=$(((j2-j1)+1)/2)
# redefine i1 and j1
i1=$((i1/2))
j1=$((j1/2))

i2=(npx-i1)+1
j2=(npy-j1)+1

SOURCE_FILE=ocean_topog.nc
OUTPUT_FILE="ocean_topog_regional.nc"

# Extract regional subset
ncks -F -d nx,${i1},${i2} -d ny,${j1},${j2} "${SOURCE_DIR}/${SOURCE_FILE}" "${OUTDIR}/${OUTPUT_FILE}" || {
    echo "Error: ncks command failed"
    exit 1
}

echo "Successfully created ${OUTDIR}/${OUTPUT_FILE}"

SOURCE_FILE=ocean_mask.nc
OUTPUT_FILE="ocean_mask_regional.nc"

# Verify source file exists
if [ ! -f "${SOURCE_DIR}/${SOURCE_FILE}" ]; then
    echo "Error: Source file not found: ${SOURCE_DIR}/${SOURCE_FILE}"
    exit 1
fi

# Extract regional subset
ncks -F -d nx,${i1},${i2} -d ny,${j1},${j2} "${SOURCE_DIR}/${SOURCE_FILE}" "${OUTDIR}/${OUTPUT_FILE}" || {
    echo "Error: ncks command failed"
    exit 1
}

echo "Successfully created ${OUTDIR}/${OUTPUT_FILE}"
