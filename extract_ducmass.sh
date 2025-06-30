#!/bin/bash
set -eux

# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230301.nc4 0301.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230302.nc4 0302.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230303.nc4 0303.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230304.nc4 0304.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230305.nc4 0305.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230306.nc4 0306.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230307.nc4 0307.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230308.nc4 0308.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230309.nc4 0309.nc
# ncks -v DUCMASS MERRA2_400.tavg1_2d_aer_Nx.20230310.nc4 0310.nc

ncrcat 03*.nc merra2.ducmass.20230301_10.nc
