
#!/bin/bash
#
# Script to extract specified variables from multiple 6-hourly forecast files
# and combine them into a single output file using NCO (netCDF Operators)
#

# Variables to extract
#VARS="slmsksfc,dswrf,dlwrf,vbdsf_ave,vddsf_ave,nbdsf_ave,nddsf_ave,u10m,v10m,hgt_hyblev1,pressfc,tmp_hyblev1,spfh_hyblev1,ugrd_hyblev1,vgrd_hyblev1,q2m,t2m,pres_hyblev1,precp,fprecp"
VARS="dswrf,dlwrf,vbdsf_ave,vddsf_ave,nbdsf_ave,nddsf_ave,ugrd10m,vgrd10m,hgt_hyblev1,pressfc,tmp_hyblev1,spfh_hyblev1,ugrd_hyblev1,vgrd_hyblev1,spfh2m,tmp2m,pressfc,prate_ave"

# Output file
OUTPUT_FILE="extracted_output.nc"

# Input file pattern (handles 3 and 4 digit forecast hours)
INPUT_PATTERN="sfcf*.nc"

# Directory containing the input files (modify as needed)
INPUT_DIR="."

# Change to input directory if specified
cd "${INPUT_DIR}" || exit 1

# Create list of input files (sorted by forecast hour)
# Handles both 3-digit (sfcf???.nc) and 4-digit (sfcf????.nc) forecast hours
INPUT_FILES=$(ls -1 sfcf[0-9][0-9][0-9].nc sfcf[0-9][0-9][0-9][0-9].nc 2>/dev/null | sort -V)

# Check if files exist
if [ -z "${INPUT_FILES}" ]; then
    echo "Error: No files matching pattern ${INPUT_PATTERN} found in ${INPUT_DIR}"
    exit 1
fi

echo "Found the following files:"
echo "${INPUT_FILES}"
echo ""

# Create temporary directory for intermediate files
TMPDIR="./tmp_extract_$$"
mkdir -p "${TMPDIR}"

echo "Extracting variables: ${VARS}"
echo ""

# Step 1: Extract variables from each file into temporary files
counter=0
for file in ${INPUT_FILES}; do
    if [ -f "${file}" ]; then
        counter=$((counter + 1))
        echo "Extracting variables from: ${file}"
        ncks -v ${VARS} "${file}" "${TMPDIR}/extracted_${counter}.nc"
    fi
done

echo ""
echo "Concatenating files into ${OUTPUT_FILE}..."

# Step 2: Concatenate all extracted files along the record (time) dimension
ncrcat "${TMPDIR}"/extracted_*.nc "${OUTPUT_FILE}"

# Clean up temporary files
echo "Cleaning up temporary files..."
rm -rf "${TMPDIR}"

echo ""
echo "Done! Output written to: ${OUTPUT_FILE}"
echo "Total files processed: ${counter}"
