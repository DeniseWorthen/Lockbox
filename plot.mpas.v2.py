import matplotlib
matplotlib.use('Agg') # Keep Agg to avoid Tcl/X11 errors

import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection
import cartopy.crs as ccrs
import io, imgcat  # New imports for terminal streaming

# 1. Load Files
ds_med  = xr.open_dataset("/scratch4/NCEPDEV/stmp/Denise.Worthen/RT_RUNDIRS/Denise.Worthen/FV3_RT/rt_3924270/cpld_control_mpasmm_gnu/ufs.cpld.cpl.hi.atm.2025-11-21-00000.nc") # CMEPS output
ds_esmf = xr.open_dataset("/scratch4/NCEPDEV/nems/Denise.Worthen/mpas/x1.40962/mesh.mpas.40962.nc")      # ESMF mesh mapping file

# 2. Extract Data from CMEPS
# Ensure the variable matches the length of the ESMF mesh elements
var_name = "atmImp_Sa_tbot"
data_values = ds_med[var_name].isel(time=0).values

# 3. Extract Geometry from ESMF Mesh
# ESMF usually stores coordinates in degrees
node_lons = ds_esmf['nodeCoords'].isel(coordDim=0).values
node_lats = ds_esmf['nodeCoords'].isel(coordDim=1).values
elem_conn = ds_esmf['elementConn'].values

# Force conversion to integer to eliminate floating-point indexing errors
# Any NaN values from the netCDF read will be converted to a safe default (-1)
elem_conn = np.nan_to_num(elem_conn, nan=-1).astype(int)

# Adjust for Fortran 1-based indexing if present
start_idx = ds_esmf['elementConn'].attrs.get('start_index', 1)
if start_idx == 1:
    elem_conn = elem_conn - 1

# 4. Build the Polygon Shapes
polygons = []
for i in range(len(elem_conn)):
    # Get node references for this element
    node_indices = elem_conn[i, :]

    # Filter out padding/fill values (e.g., -1, large negative ints, or repeating indices)
    # This strips the 6-padded array back down to a 3 or 4 sided shape if needed
    valid_indices = node_indices[node_indices >= 0]

    # Gather actual boundary coordinates for this specific polygon
    poly_lons = node_lons[valid_indices]
    poly_lats = node_lats[valid_indices]

    # Handle the map's Dateline wrapping (-180/180) to prevent stretched shapes
    if np.max(poly_lons) - np.min(poly_lons) > 180:
        poly_lons = np.where(poly_lons < 0, poly_lons + 360, poly_lons)

    polygons.append(np.column_stack((poly_lons, poly_lats)))

# 5. Render Map
fig = plt.figure(figsize=(10, 6))
ax = plt.axes(projection=ccrs.PlateCarree())
ax.coastlines()

# Use PolyCollection to fast-render the true shapes
coll = PolyCollection(polygons, array=data_values, cmap='turbo', edgecolors='none')
ax.add_collection(coll)

ax.autoscale_view()
plt.colorbar(coll, label=var_name, orientation='horizontal', pad=0.05)
#plt.show()

# Create an in-memory buffer
buf = io.BytesIO()
fig.savefig(buf, format='png', dpi=150, bbox_inches='tight')

# Send the buffer content to your terminal
imgcat.imgcat(buf.getvalue())
buf.close()
