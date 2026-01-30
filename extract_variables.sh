#!/bin/bash
#
# Script to extract specified variables from multiple 6-hourly forecast files
# and combine them into a single output file using NCO (netCDF Operators)
#

# Variables to extract
VARS="var1,var2,var3"

# Multiplication factors for each variable (in same order as VARS)
# -1 to change sign, 1 to keep as is
FACTORS="-1 1 1"

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
        #Apply multiplication factors to each variable
        IFS=',' read -ra VAR_ARRAY <<< "${VARS}"
        FACTOR_ARRAY=(${FACTORS})

        for i in "${!VAR_ARRAY[@]}"; do
            var="${VAR_ARRAY[$i]}"
            factor="${FACTOR_ARRAY[$i]}"

            if [ "${factor}" != "1" ]; then
                echo "  Applying factor ${factor} to ${var}..."
                ncap2 -O -s "${var}=${var}*${factor}" "${tmp_file}" "${tmp_file}"
            fi
        done

        counter=$((counter + 1))
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
