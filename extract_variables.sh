
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
        # Convert time dimension to unlimited (record) dimension
        echo "Converting time to unlimited dimension..."
        ncks -O -4 --mk_rec_dmn time "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
        # Rename prate_ave to precp
        echo "Renaming prate_ave to precp..."
        ncrename -O -v prate_ave,precp "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
        # Create fprecp variable as copy of precp with all values set to 0.0
        echo "Creating fprecp variable from precp..."
        ncap2 -O -s 'fprecp=0.0*precp' "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
        # Rename pressfc to psurf
        echo "Renaming pressfc to psurf..."
        ncrename -O -v pressfc,psurf "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
        # Rename tmp2m to t2m
        echo "Renaming tmp2m to t2m..."
        ncrename -O -v tmp2m,t2m "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
        # Rename spfh2m to q2m
        echo "Renaming spfh2m to q2m..."
        ncrename -O -v spfh2m,q2m "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
        # Create pres_hyblev1 as copy of psurf
        echo "Creating pres_hyblev1 as copy of psurf..."
        ncap2 -O -s 'pres_hyblev1=psurf' "${TMPDIR}/extracted_${counter}.nc" "${TMPDIR}/extracted_${counter}.nc"
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
