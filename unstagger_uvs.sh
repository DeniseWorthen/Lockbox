#!/bin/bash

# Ensure three input files are provided
#if [ "$#" -ne 3 ]; then
#    echo "Usage: $0 file1.nc file2.nc file3.nc"
#    exit 1
#fi

FILE1=hi.AB.ice.nc
FILE2=hi.AC.ice.nc
FILE3=hi.CC.ice.nc

# --- FILE 1: 4-Corner Stagger (uvel_h and vvel_h) ---
# Calculations use (time, nj, ni) indices
ncap2 -O -s '
  *u_w=uvel_h; *v_w=vvel_h;
  where(u_w.missing()) u_w=0.0;
  where(v_w.missing()) v_w=0.0;
  u_center = 0.25 * ( u_w(:, 0:-2, 0:-2) + u_w(:, 0:-2, 1:-1) +
                      u_w(:, 1:-1, 0:-2) + u_w(:, 1:-1, 1:-1) );
  v_center = 0.25 * ( v_w(:, 0:-2, 0:-2) + v_w(:, 0:-2, 1:-1) +
                      v_w(:, 1:-1, 0:-2) + v_w(:, 1:-1, 1:-1) );
' "$FILE1" "centered_4corner.nc"

# --- FILES 2 & 3: 1D Averages (uvelE_h along i, vvelN_h along j) ---
for FILE in "$FILE2" "$FILE3"; do
    OUT_NAME="centered_$(basename "$FILE")"
    ncap2 -O -s '
      u_center = 0.5 * ( uvelE_h(:, :, 0:-2) + uvelE_h(:, :, 1:-1) );
      v_center = 0.5 * ( vvelN_h(:, 0:-2, :) + vvelN_h(:, 1:-1, :) );
    ' "$FILE" "$OUT_NAME"
done

echo "Processing complete."
echo "File 1 used uvel_h/vvel_h -> centered_4corner.nc"
echo "Files 2 & 3 used uvelE_h/vvelN_h -> centered_$(basename "$FILE2") & centered_$(basename "$FILE3")"
