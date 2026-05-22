import xarray as xr
import uxarray as ux
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import geocat.viz as gv
import os

RT = "/scratch4/NCEPDEV/stmp/Denise.Worthen/RT_RUNDIRS/Denise.Worthen/FV3_RT/rt_3924270/cpld_control_mpasmm_gnu/"
fname = os.path.join(RT, "ufs.cpld.cpl.hi.atm.2025-11-21-00000.nc")
vname = "atmImp_Sa_tbot"
fmesh="/scratch4/NCEPDEV/nems/Denise.Worthen/mpas/x1.40962/mesh.mpas.40962.nc"

# 1. Load the data field and the ESMF mesh file
#data_ds = xr.open_dataset(fname)
#grid_ds = xr.open_dataset("/scratch4/NCEPDEV/nems/Denise.Worthen/mpas/x1.40962/mesh.mpas.40962.nc")

# Extract your specific variable (e.g., Temperature)
# Ensure the variable's dimension matches the number of nodes or elements
#data_field = data_ds["temperature"]
#data_field = data_ds[vname].isel(time=0, atmImp_ny=0)

# 2. Parse the ESMF topology with UXarray
# UXarray automatically recognizes ESMF unstructured grid formats
#ux_grid = ux.open_grid("/scratch4/NCEPDEV/nems/Denise.Worthen/mpas/x1.40962/mesh.mpas.40962.nc")

# 3. Create a UXarray Dataset linking your field to the mesh
#ux_ds = ux.UxDataset(uxdata=data_field, uxgrid=ux_grid)

ux_ds = ux.open_dataset(fmesh, fname)

# 4. Set up a map projection using Cartopy
fig = plt.figure(figsize=(10, 6))
ax = plt.axes(projection=ccrs.PlateCarree())

# Plot the color-filled map on the native unstructured grid
# 'tripcolor' creates a pseudocolor plot of an unstructured triangular grid
plot_mesh = ux_ds[vname].isel(time=0,atmImp_ny=0).plot.tripcolor(
    ax=ax,
    transform=ccrs.PlateCarree(),
    cmap="viridis",
    add_colorbar=False
)

# 5. Apply geocat.viz helper utilities for NCL-style aesthetics
gv.util.add_major_minor_ticks(ax)
gv.util.add_lat_lon_ticklabels(ax)
gv.util.set_titles_and_labels(
    ax,
    maintitle="Temperature on Unstructured Mesh",
    lefttitle="degC",
    righttitle="ESMF Mesh"
)

# Add coastlines and colorbar
ax.coastlines()
plt.colorbar(plot_mesh, ax=ax, orientation='horizontal', pad=0.08, label='Temperature')

plt.show()
