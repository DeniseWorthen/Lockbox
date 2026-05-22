import matplotlib
matplotlib.use('Agg') # Keep Agg to avoid Tcl/X11 errors

import uxarray as ux
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from holoviews import render
import matplotlib.colors as mcolors
import numpy as np
import io, imgcat  # New imports for terminal streaming

#vname='atmImp_Sa_t2m'
vname='atmImp_Sa_u10m'

# 1. Load data
uxds = ux.open_dataset("x1.10242.grid.nc", "datm.gefs.cpl.hi.atm.2025-11-20-14400.nc")
pdat = uxds[vname].isel(time=0).squeeze()
vmin, vmax = float(pdat.min()), float(pdat.max())

# 2. Plot using HoloViz (Rasterized)
plot = pdat.plot(
    rasterize=True,
    projection=ccrs.PlateCarree(),
    cmap='RdYlBu_r',
    width=1000,
    height=500,
    global_extent=True,
    clim=(vmin, vmax)
)

# 3. Render and capture the axes
fig = render(plot, backend='matplotlib')
ax = fig.gca()
ax.set_global()

# # 4. ADD WIREFRAME (Directly using cell centers and triangulation)
# # We use the same masking trick as before to avoid lines across the date line
# lons = np.rad2deg(uxds.uxgrid.face_lon.values)
# lats = np.rad2deg(uxds.uxgrid.face_lat.values)
# import matplotlib.tri as tri
# triang = tri.Triangulation(lons, lats)
# tri_lons = triang.x[triang.triangles]
# is_bad = np.any(np.abs(np.diff(tri_lons, axis=1)) > 180, axis=1)
# triang.set_mask(is_bad)
# # Draw the wireframe (edges)
# ax.triplot(triang, color='black', linewidth=0.2, alpha=0.3, transform=ccrs.PlateCarree())

mappable = next((c for c in ax.get_children() if isinstance(c, matplotlib.cm.ScalarMappable)), None)
if mappable:
    mappable.set_norm(mcolors.Normalize(vmin=vmin, vmax=vmax))
    mappable.set_cmap('RdYlBu_r')
    cbar = fig.colorbar(mappable, ax=ax, orientation='horizontal', pad=0.1, extend='both')
    cbar.set_label('Temperature (K)')

# 4. FINAL CHANGE: Stream directly to iTerm2
fig.set_size_inches(12, 7)

# Create an in-memory buffer
buf = io.BytesIO()
fig.savefig(buf, format='png', dpi=150, bbox_inches='tight')

# Send the buffer content to your terminal
imgcat.imgcat(buf.getvalue())
buf.close()
