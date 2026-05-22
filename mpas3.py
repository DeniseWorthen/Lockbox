import xarray as xr
import uxarray as ux
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import geocat.viz as gv
import os
import numpy as np
import warnings

RT = "/scratch4/NCEPDEV/stmp/Denise.Worthen/RT_RUNDIRS/Denise.Worthen/FV3_RT/rt_3924270/cpld_control_mpasmm_gnu/"
fdata = os.path.join(RT, "ufs.cpld.cpl.hi.atm.2025-11-21-00000.nc")
vname = "atmImp_Sa_tbot"
fmesh="/scratch4/NCEPDEV/nems/Denise.Worthen/mpas/x1.40962/mesh.mpas.40962.nc"

#uxgrid = ux.open_grid(fmesh)

# 2. Suppress the internal numpy mesh-casting warning specifically for this line
with warnings.catch_warnings():
    warnings.filterwarnings("ignore", category=RuntimeWarning, message="invalid value encountered in cast")
    uxds = ux.open_dataset(fmesh, fdata, decode_times=False)
#uxds = ux.open_dataset(fmesh, fdata, decode_times=False)
#uxds[vname]
#uxds

pdat = uxds[vname].isel(time=0,atmImp_ny=0)

#uxds[vname].isel(time=0,atmImp_ny=0).plot()

projection = projection = ccrs.Robinson()

(
 pdat
 .plot.polygons()
 .opts(width=600, height=400, cmap="inferno", title="Vector Polygons")
 )

# pdat.plot.rasterize(
#     method="polygon",
#     projection=projection,
#     backend="matplotlib",
#     pixel_ratio=8.0,
#     fig_size=400,
# ) * features(projection)

# (
#     uxds[vname].isel(time=0,atmImp_ny=0)
#     .plot()
#     .opts(width=700, height=350, title="Default Plot (Excluding Antimeridian Faces)")
#    + uxds[vname].isel(time=0,atmImp_ny=0)
#     .plot(exclude_antimeridian=False)
#     .opts(width=700, height=350, title="Include Antimeridian Faces")
# ).cols(1)

# # Create a figure with a Cartopy Orthographic or Cylindrical Projection
# fig = plt.figure(figsize=(10, 8))
# ax = plt.axes(projection=ccrs.PlateCarree())

# # Add geographic anchors using your geocat.viz import
# #gv.util.add_coastlines_from_cartopy(ax, edgecolor="black", linewidth=0.8)
# gv.util.add_lat_lon_ticklabels(ax)

# # Generate a shaded unstructured polygon plot using UXarray's native engine
# # This maps the 1D pdat data vector onto its respective MPAS polygon faces
# pdat.plot.polygons(
#     ax=ax,
#     transform=ccrs.PlateCarree(),
#     cmap="coolwarm",
#     cbar_kwargs={"label": vname},
# )

# plt.title(f"MPAS Grid Output: {vname} (Time=0)")
# plt.show()

#nan_count = uxds[vname].isnull().sum().values
#print(f"Total NaNs: {nan_count}")
#inf_count = np.isinf(uxds[vname]).sum().values
#print(f"Total Infinities: {inf_count}")


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

# ux_ds = ux.open_dataset(fmesh, fname)

# # 4. Set up a map projection using Cartopy
# fig = plt.figure(figsize=(10, 6))
# projection = ccrs.PlateCarree()
# ax = plt.axes(projection=projection)

# pdat = ux_ds[vname].isel(time=0,atmImp_ny=0)

# # 4. Convert unstructured grid elements to a Matplotlib PolyCollection
# # This properly maps the MPAS cell faces into a 2D plot space
# plot_mesh = pdat.to_polycollection(
#     projection=projection,
#     periodic_elements='split'  # Splits cells wrapping around the antimeridian
# )

# # Apply colormap to the generated collection
# plot_mesh.set_cmap("coolwarm")

# # Clear polygon edge colors so the mesh lines don't obscure the field data
# plot_mesh.set_edgecolor("none")

# # Add the collection explicitly to your Cartopy axes
# ax.add_collection(plot_mesh)

# # 5. Apply NCL aesthetics via geocat.viz
# gv.util.add_major_minor_ticks(ax)
# gv.util.add_lat_lon_ticklabels(ax)
# gv.util.set_titles_and_labels(
#     ax,
#     maintitle="Atmosphere Import Bottom Temperature",
#     lefttitle="tbot",
#     righttitle="MPAS 40962 Mesh"
# )

# # Render map features and horizontal colorbar
# ax.coastlines(resolution='110m', color='black', linewidth=0.8)

# # Dynamically auto-scale the data limits for the color bar
# ax.autoscale_view()
# plt.colorbar(
#     plot_mesh,
#     ax=ax,
#     orientation='horizontal',
#     pad=0.06,
#     shrink=0.75,
#     label='Temperature'
# )

# plt.savefig("mpas_mesh_output.png", bbox_inches='tight', dpi=200)
# plt.show()
