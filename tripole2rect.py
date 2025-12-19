import xarray as xr
import numpy as np
from scipy.sparse import coo_matrix

# Variables
var_name = 'grid_imask'
nx, ny = 1920, 1081
out_shape = (nx, ny)

# 1. Load datasets
ds_wgt = xr.open_dataset(wgt)
ds_src = xr.open_dataset(src)

# Capture the original data type (e.g., int32, float32, etc.)
original_dtype = ds_src[var_name].dtype

# 2. Extract weights (Force float64 for math)
col = ds_wgt['col'].values - 1
row = ds_wgt['row'].values - 1
S = ds_wgt['S'].values.astype(np.float64)
n_a, n_b = ds_wgt.dims['n_a'], ds_wgt.dims['n_b']

weight_matrix = coo_matrix((S, (row, col)), shape=(n_b, n_a), dtype=np.float64)

# 3. Process Source Field
# Flatten and cast to float64 to ensure high-precision multiplication
src_field_1d = ds_src[var_name].values.flatten(order='F').astype(np.float64)

# 4. Perform regridding (in double precision)
dst_field_1d = weight_matrix.dot(src_field_1d)

# 5. Convert back to original type
# If integer, it will truncate/round as per standard casting rules
dst_field_typed = dst_field_1d.astype(original_dtype)

# 6. Reshape and Create Output
dst_2d = dst_field_typed.reshape(out_shape, order='F')
lat_2d = ds_wgt['yc_b'].values.reshape(out_shape, order='F').astype(np.float64)
lon_2d = ds_wgt['xc_b'].values.reshape(out_shape, order='F').astype(np.float64)

ds_out = xr.Dataset(
    data_vars={
        var_name: (["grid_xt", "grid_yt"], dst_2d),
        "grid_xt": (["grid_xt", "grid_yt"], lon_2d),
        "grid_yt": (["grid_xt", "grid_yt"], lat_2d),
    }
)

# 7. Save to NetCDF
# Xarray will automatically use the appropriate NetCDF type based on the numpy dtype
ds_out.to_netcdf(f"{dirout}test_mask.nc")
