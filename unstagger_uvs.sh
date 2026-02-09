#!/bin/bash

# Ensure three input files are provided
#if [ "$#" -ne 3 ]; then
#    echo "Usage: $0 file1.nc file2.nc file3.nc"
#    exit 1
#fi

ncrcat -F -d ni,200,265 -d nj,250,320 base.new.AB/history/iceh_06h*nc hi.AB.ice.nc
ncrcat -F -d ni,200,265 -d nj,250,320 base.new.AC/history/iceh_06h*nc hi.AC.ice.nc
ncrcat -F -d ni,200,265 -d nj,250,320 base.new.CC/history/iceh_06h*nc hi.CC.ice.nc

FILE1=hi.AB.ice.nc
FILE2=hi.AC.ice.nc
FILE3=hi.CC.ice.nc

# --- FILE 1: 4-Corner Stagger (uvel_h and vvel_h) ---
# Calculations use (time, nj, ni) indices
# u: average (i,j) and (i-1,j); v: average (i,j) and (i,j-1)
ncap2 -O -s '
  // 1. Capture dimension sizes
  *ni = $ni.size; *nj = $nj.size;

  // 2. Load input variables and immediately clean missing values to 0.0f
  // In NCO 5.2.4, float() creates a RAM variable that inherits the _FillValue.
  // We reference uvel_h@_FillValue directly to be safe.
  *u = float(uvel_h); where(u == uvel_h@_FillValue) u = 0.0f;
  *v = float(vvel_h); where(v == vvel_h@_FillValue) v = 0.0f;

  // 3. Create the output variables as clones of the input (to keep shape/metadata)
  u_center = uvel_h; v_center = vvel_h;

  // 4. Register _FillValue to ensure it is recognized as an attribute
  u_center@_FillValue = uvel_h@_FillValue; v_center@_FillValue = vvel_h@_FillValue;

  // 5. Initialize the entire output field to the FillValue
  u_center = u_center@_FillValue; v_center = v_center@_FillValue;

  // 6. Perform the NE-stagger average for the interior (i,j)
  // We use explicit ranges [1:nj-1, 1:ni-1] to match the output slice exactly.
  u_center(:, 1:nj-1, 1:ni-1) = 0.25f * (
    u(:, 1:nj-1, 1:ni-1) + u(:, 0:nj-2, 1:ni-1) +
    u(:, 0:nj-2, 0:ni-2) + u(:, 1:nj-1, 0:ni-2)
  );

  v_center(:, 1:nj-1, 1:ni-1) = 0.25f * (
    v(:, 1:nj-1, 1:ni-1) + v(:, 0:nj-2, 1:ni-1) +
    v(:, 0:nj-2, 0:ni-2) + v(:, 1:nj-1, 0:ni-2)
  );

  u_center@units = uvel_h@units; v_center@units = vvel_h@units;

  // 7. Apply the tmask
  where(tmask == tmask@_FillValue) {
      u_center = u_center@_FillValue; v_center = v_center@_FillValue;
  }
' "$FILE1" "$FILE1"

# # --- FILES 2 & 3: 1D Averages (uvelE_h along i, vvelN_h along j) ---
for FILE in "$FILE2" "$FILE3"; do
ncap2 -O -s '
  // 1. Capture dimension sizes
  *ni = $ni.size;
  *nj = $nj.size;

  // 2. Load and clean missing values (set to 0.0f for math)
  *u = float(uvel_h); where(u == uvel_h@_FillValue) u = 0.0f;
  *v = float(vvel_h); where(v == vvel_h@_FillValue) v = 0.0f;

  // 3. Create output variables and wipe to FillValue
  u_center = uvel_h; u_center@_FillValue = uvel_h@_FillValue; u_center = u_center@_FillValue;
  v_center = vvel_h; v_center@_FillValue = vvel_h@_FillValue; v_center = v_center@_FillValue;

  // 4. Perform 2-point average (Staggered Interior)

  // U_center: Average of (i,j) and (i-1,j) -> Average along the X-axis (ni)
  // We fill from 1:nj-1 (all rows except first) and 1:ni-1 (all cols except first)
  u_center(:, 1:nj-1, 1:ni-1) = 0.5f * (
    u(:, 1:nj-1, 1:ni-1) + u(:, 1:nj-1, 0:ni-2)
  );

  // V_center: Average of (i,j) and (i,j-1) -> Average along the Y-axis (nj)
  v_center(:, 1:nj-1, 1:ni-1) = 0.5f * (
    v(:, 1:nj-1, 1:ni-1) + v(:, 0:nj-2, 1:ni-1)
  );

  // 5. Transfer metadata
  u_center@units = uvel_h@units; v_center@units = vvel_h@units;

  // 6. Apply tmask
  where(tmask == tmask@_FillValue) {
      u_center = u_center@_FillValue; v_center = v_center@_FillValue;
  }
' "$FILE" "$FILE"
done

# # echo "Processing complete."
# # echo "File 1 used uvel_h/vvel_h -> centered_4corner.nc"
# # echo "u_center and v_center variables appended to original files:"
# # echo "  $FILE1 (from uvel_h/vvel_h)"
# # echo "  $FILE2 (from uvelE_h/vvelN_h)"
# # echo "  $FILE3 (from uvelE_h/vvelN_h
