import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cmaps
import numpy as np
import xarray as xr
import os

import geocat.viz as gv
import geocat.datafiles as gdf

# --- File Paths and Variables ---
RT = "/scratch4/NCEPDEV/stmp/Denise.Worthen/RT_RUNDIRS/Denise.Worthen/FV3_RT/rt_3924270/cpld_control_mpasmm_gnu/"
fname = os.path.join(RT, "ufs.cpld.cpl.hi.atm.2025-11-21-00000.nc")
vname = "atmImp_Sa_tbot"

# --- Data Loading ---
# Open the dataset using xarray
ds = xr.open_dataset(fname)

# Extract 1D data slice: equivalent to (0,0,:) in NCL
# Assumes dimensions are (time, lnd, cells) or similar 3D structure
pdat = ds[vname].isel(time=0, atmImp_ny=0).values
vmin, vmax = float(pdat.min()), float(pdat.max())
dimnames = ds[vname].dims
dim0=dimnames[0]
dim1=dimnames[1]
selectors = {dim0: 0, dim1: 0}

# Load coordinates and convert from radians to degrees
lon_rad = ds["atmImp_lon"].isel(selectors).values
lat_rad = ds["atmImp_lat"].isel(selectors).values

lon_deg = np.degrees(lon_rad)
lat_deg = np.degrees(lat_rad)

# --- 0/360 to -180/180 Longitude Mapping Function ---
# This normalizes longitudes to -180 to 180, resolving tricontourf wrap artifacts
#lon_deg = (lon_deg + 180) % 360 - 180

# 3. Use GeoCat-Viz to set up standard map features
#gv.util.add_cyclic = (
#    False  # False for unstructured data unless manually handled
#)
#gv.util.set_map_boundary(ax, projection=projection)

plt.figure(figsize=(12, 8))
# Generate axes, using Cartopy
projection = ccrs.PlateCarree()
ax = plt.axes(projection=projection)
# Use global map and draw coastlines
ax.set_global()
ax.coastlines()
# Import the default NCL colormap
newcmp = cmaps.ncl_default

# Contourf-plot data (for filled contours)
# Note, min-max contour levels are hard-coded. contourf's automatic contour value selector produces fractional values.
# p = pdat.plot.contourf(
#     ax=ax,
#     cmap=newcmp,
#     clim=(vmin,vmax),
#     levels=16,
#     add_colorbar=False,
#     transform=projection,
#     extend='neither',
# )
# 4. Plot the unstructured contour field
# If you have explicit connectivity, pass it as: ax.tricontourf(lons, lats, triangles, data, ...)
p = ax.tricontourf(
    lon_deg, lat_deg, pdat, transform=projection, cmap="viridis", levels=15
)

# Add horizontal colorbar
cbar = plt.colorbar(
    p, orientation='horizontal', shrink=0.75, drawedges=True, aspect=16, pad=0.075
)
cbar.ax.tick_params(labelsize=14)
cbar.set_ticks(np.linspace(vmin, vmax, 12))

# Use geocat.viz.util convenience function to set axes tick values
gv.set_axes_limits_and_ticks(
    ax, xticks=np.linspace(-180, 180, 13), yticks=np.linspace(-90, 90, 7)
)

# Use geocat.viz.util convenience function to make plots look like NCL plots by using latitude, longitude tick labels
gv.add_lat_lon_ticklabels(ax)

# Use geocat.viz.util convenience function to add minor and major tick lines
gv.add_major_minor_ticks(ax, labelsize=14)

# Use geocat.viz.util convenience function to add titles to left and right of the plot axis.
gv.set_titles_and_labels(
    ax,
    maintitle="NCL Default Colors",
    lefttitle=pdat.long_name,
    lefttitlefontsize=16,
    righttitle=pdat.units,
    righttitlefontsize=16,
    xlabel="",
    ylabel="",
)

# Show the plot
plt.show()













# # --- Plotting Configuration ---
# # Create figure and define a Cylindrical Equidistant projection (PlateCarree)
# fig, ax = plt.subplots(
#     figsize=(8, 8), subplot_kw={"projection": ccrs.PlateCarree()}
# )

# # Set map boundaries to global
# ax.set_global()

# # Draw coastlines using GeoCAT-Viz utility
# #gcv.util.add_coastlines(ax, linewidth=0.5, color="black")

# # --- Contour Plotting ---
# # Use matplotlib's tricontourf for unstructered/1D coordinate arrays
# contour = ax.tricontourf(
#     lon_deg,
#     lat_deg,
#     pdat,
#     transform=projection,
#     cmap="viridis",  # 'viridis' or 'gcm.cmap.gui_default' if registered
#     extend="both",
# )

# # Add a colorbar
# plt.colorbar(
#     contour, ax=ax, orientation="horizontal", pad=0.05, shrink=0.75
# )

# # --- GeoCAT-Viz Styling ---
# # Use GeoCAT-Viz to format ticks and labels nicely
# gcv.util.add_lat_lon_ticklabels(ax)
# gcv.util.set_titles_and_labels(
#     ax, maintitle="test", lefttitle=vname, righttitle=""
# )

# # Show plot (Interactive window equivalent to X11)
# plt.show()
