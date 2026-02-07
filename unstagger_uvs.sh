#!/bin/bash

# Ensure three input files are provided
#if [ "$#" -ne 3 ]; then
#    echo "Usage: $0 file1.nc file2.nc file3.nc"
#    exit 1
#fi

FILE1=/Users/toby/hi.AB.ice.nc
FILE2=/Users/toby/hi.AC.ice.nc
FILE3=/Users/toby/hi.CC.ice.nc

# --- FILE 1: 4-Corner Stagger (uvel_h and vvel_h) ---
# Calculations use (time, nj, ni) indices
ncap2 -O -s '
  *u_w=uvel_h; *v_w=vvel_h;
  u_w(u_w == uvel_h@_FillValue) = 0.0;
  v_w(v_w == vvel_h@_FillValue) = 0.0;
  u_center=float(0.25 * ( u_w(:, 0:-2, 0:-2) + u_w(:, 0:-2, 1:-1) +
                         u_w(:, 1:-1, 0:-2) + u_w(:, 1:-1, 1:-1) ));
  v_center=float(0.25 * ( v_w(:, 0:-2, 0:-2) + v_w(:, 0:-2, 1:-1) +
                         v_w(:, 1:-1, 0:-2) + v_w(:, 1:-1, 1:-1) ));
  u_center@_FillValue=1.e+30f;
  v_center@_FillValue=1.e+30f;
  u_center(tmask == 0) = u_center@_FillValue;
  v_center(tmask == 0) = v_center@_FillValue;
' "$FILE1" "$FILE1"

# --- FILES 2 & 3: 1D Averages (uvelE_h along i, vvelN_h along j) ---
for FILE in "$FILE2" "$FILE3"; do
    ncap2 -O -s '
      u_center=float(0.5 * ( uvelE_h(:, :, 0:-2) + uvelE_h(:, :, 1:-1) ));
      v_center=float(0.5 * ( vvelN_h(:, 0:-2, :) + vvelN_h(:, 1:-1, :) ));
      u_center@_FillValue=1.e+30f;
      v_center@_FillValue=1.e+30f;
      u_center(tmask == 0) = u_center@_FillValue;
      v_center(tmask == 0) = v_center@_FillValue;
    ' "$FILE" "$FILE"
done

echo "Processing complete."
echo "File 1 used uvel_h/vvel_h -> centered_4corner.nc"
echo "u_center and v_center variables appended to original files:"
echo "  $FILE1 (from uvel_h/vvel_h)"
echo "  $FILE2 (from uvelE_h/vvelN_h)"
echo "  $FILE3 (from uvelE_h/vvelN_h