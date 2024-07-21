!> @file w3smcomd.f90
!> @brief smc grid interpolation and regridding functionality
!>
!> @author chris bunney
!> @date 21-jul-2021
!/
!> @brief service module for support of smc regridding and interpolation
!>
!> @details
!>  for smc grids, four types of output are possible:
!>    1. flat grid (seapoint) output of smc cells with associated
!>       cell size variables (cx and cy). requires extra effort to
!>       plot as grid is not regular.
!>
!>    2. regular, uniformly gridded output to a specified output grid.
!>       this is achieved by area averaging of the smc cells. the output
!>       grid will be aligned with the smc grid cell edges which may
!>       result in the actual output grid being slightly different to
!>       the original user request. a land/sea mask is created by
!>       keeping track of the cell coverage and setting cells with
!>       total coverage of <50% undef.
!>
!>    3. [<em>experimental</em>] arbitrary regualar grid re-gridding
!>       using indices and weights generated from ww3_smcint*.
!>       uses the local gradient within the grid cell and distance
!>       between cell centres to interpolate.
!>
!>    4. [<em>experimental</em>] as type 3, but with no interpolation -
!>       value from surrounding smc cell only.
!>
!>  <em>* the ww3_smcint program is experimental and not yet included
!>  in the official ww3 distribution; it is currently part of the uk
!>  met office's suite of internal tools.</em>
!>
!>  @remark note - directional fields are expected to be passed to
!>    routines with units of <em>radians (cartesian convention)</em>.
!>    they will be output in units of <em>degrees (nautical convention)</em>.
!>
!> @author chris bunney
!> @date 21-jul-2021
!>
!> ### change log
!>   date      | ver  | comments
!> ------------|------|---------
!> 18-jan-2016 | 4.18 | initial version
!> 28-sep-2016 | 4.18 | bug fix exo/eyo calcs for whole domain output
!> 05-oct-2016 | 4.18 | bug fix regular grid indicies for type 2 output
!> 29-sep-2017 | 4.18 | revise calculation of indicies to ensure selected cells fall inside user selected areas.
!> 18-apr-2018 | 4.18 | added type 3 and 4 smc output
!> 20-jun-2018 | 4.18 | directional fields output as nautical convention (deg)
!> 27-jun-2018 | 6.05 | ported to v6
!> 06-jan-2021 | 7.12 | use arctc option for smc grid.
!> 20-jul-2021 | 7.12 | fix bug where edge cells in design grid may not be matched due where smc cell > base grid size.
!> 21-jul-2021 | 7.12 | elevated some grid variables to double precision, fixed exo/eyo bug
!>
module w3smcomd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |        chris bunney, ukmo         |
  !/                  |                        fortran 90 |
  !/                  | last update :         21-jul-2021 |
  !/                  +-----------------------------------+
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  use w3gdatmd
  use constants
  use w3odatmd, only: undef
  public
  ! output grid definition
  double precision     :: sxo  !< output grid longitude origin
  double precision     :: syo  !< output grid latitude origin
  double precision     :: exo  !< output grid final longitude
  double precision     :: eyo  !< output grid final latitude
  double precision     :: dxo  !< output grid cell longitude size
  double precision     :: dyo  !< output grid cell latitude size
  integer              :: nxo  !< output grid number of longitude cells
  integer              :: nyo  !< output grid number of latitude cells
  ! variables for smc regridding (type 2 output):
  !> type of smc output: 1=seapoint grid of smc cells; 2=regridding to regular grid;
  !> 3=interpolation to arbtrary grid; 4=nearest neighbour interpolation to
  !> arbitrary grid.
  integer              :: smcotype
  !> output grid cell scaling factor; should be an integer power of 2.
  integer              :: celfac
  integer, allocatable :: xidx(:)  !< x-indices of smc cells in regular grid
  integer, allocatable :: yidx(:)  !< y-indices of smc cells in regular grid
  integer, allocatable :: xspan(:) !< number of longitude cells smc cell spans
  integer, allocatable :: yspan(:) !< number of longitude cells smc cell spans
  real, allocatable    :: wts(:)   !< regridding weights
  real, allocatable    :: cov(:,:) !< wet fraction (coverage) of cell
  integer, allocatable :: mapsmc(:,:)  !< regridded mapsta
  logical, allocatable :: smcmask(:)   !< mask for type 1 output (flat array)
  integer, allocatable :: smcidx(:)    !< indices of smc cells within output grid domain
  !> smc grid definition
  integer, allocatable :: smccx(:)  !< longitude cell size factors
  integer, allocatable :: smccy(:)  !< latitude cell size factors
  real                 :: dlat      !< base longitude cell size
  real                 :: dlon      !< base latitude cell size
  integer              :: cfac      !< smc scaling factor (number of levels)
  real                 :: noval     !< fill value for seapoints with no value
  ! variables for smc nearest neighbour interpolation (type 3/4 output)
  integer, allocatable :: nnidx(:,:)  !< nearest neighbour smc point to regular grid
  real, allocatable    :: xdist(:,:)  !< lng. distance to nearest neighbour
  real, allocatable    :: ydist(:,:)  !< lat. distance to nearest neighbour
  integer              :: ndsmc       !< ww3_smcint file unit number
  ! counters:
  integer              :: smcnout  !< number of smc output cells
  integer              :: nsmc     !< number of smc cells used in regridding
contains
  !--------------------------------------------------------------------------
  !> @brief generate smc interpolation/output information
  !>
  !> @details
  !>  this subroutine generates index or mask values for extraction
  !>  of smc data to either a flat grid or regular lat/lon grid,
  !>  depending on the type of smc output grid selected:
  !>
  !>    type 1: generates a mask for extracting only points from
  !>            the user requested region.
  !>
  !>    type 2: calculate interpolation indices and weights for
  !>            regridding the irregular smc grid onto a regular,
  !>            uniformly spaced lat/lon grid.
  !>
  !> @author chris bunney
  !> @date 22-oct-2015
  !>
  subroutine smc_interp()
    implicit none
    ! locals
    real    :: cx0, cy0   ! sw corner of origin of grid
    real    :: s0chk, xsnap, ysnap
    integer :: isea, mx, my, ixx, iyy, j
    real    :: lat, lon
    j = 1
    nsmc = 0
    ! determine smallest cell size factor:
    cfac = 2**(nrlv - 1)
    ! get smallest smc grid cells step size:
    dlat = sy / cfac
    dlon = sx / cfac
    ! sw corner of grid origin cell:
    cx0 = x0 - sx / 2.
    cy0 = y0 - sy / 2.
    ! grid cell size to snap design grid to. will be regular grid
    ! resolution for cellsize <= cfac, or cellsize for cellsize > cfac
    xsnap = sx
    ysnap = sy
    if(celfac .gt. cfac) xsnap = celfac * dlon
    if(celfac .gt. cfac) ysnap = celfac * dlat
    ! get start lat,lon (must be aligned with smc grid edges). use
    ! regular grid origins if sxo or syo is -999.9 (use full grid):
    if(abs(sxo + 999.9) .lt. 1e-4) then
      sxo = cx0
    else
      s0chk = cx0 + floor((sxo - cx0) / xsnap) * xsnap
      ! ensure first grid value falls within specified range
      if (s0chk .lt. sxo) then
        sxo = s0chk + xsnap
      else
        sxo = s0chk
      endif
    endif
    if(abs(syo + 999.9) .lt. 1e-4) then
      syo = cy0
    else
      s0chk = cy0 + floor((syo - cy0) / ysnap) * ysnap
      ! ensure first grid value falls within specified range
      if (s0chk .lt. syo) then
        syo = s0chk + ysnap
      else
        syo = s0chk
      endif
    endif
    ! use regular grid extents for last lat/lon if user
    ! specifies -999.9 for exo/eyo (use full grid):
    if(abs(exo + 999.9) .lt. 1e-4) then
      exo = cx0 + sx * nx ! trhc of last cell
    endif
    if(abs(eyo + 999.9) .lt. 1e-4) then
      eyo = cy0 + sy * ny ! trhc of last cell
    endif
    ! ouput grid cell dx/dy will be integer factor of smallest
    ! smc grid cell size:
    dxo = dlon * celfac
    dyo = dlat * celfac
    ! determine number of cells in grid:
    nxo = nint((exo - sxo) / dxo)
    nyo = nint((eyo - syo) / dyo)
    if(smcotype .eq. 2) then
      ! initialise all indices to "missing":
      xidx(:) = -1
      yidx(:) = -1
    endif
    ! loop over cell array and calculate regidding factors:
    do isea=1, nsea
      !          ! for grids with arctic region: make sure we don't double count
      !          ! the overlapping boundary cells. also, don't process the arctic
      !          ! cell (which is always the last cell).
      !          ! note: narc contains all the boundary cells (global + arctic).
      !          ! whereas nglo contains only the global boundary cells.
      !          if(isea .gt. nglo-nbac .and. isea .lt. nsea-narc+1) cycle
      if( arctc .and. &
           isea .gt. nglo-nbac .and. isea .lt. nsea-narc+1) cycle
      ! get grid cell size:
      mx = ijkcel(3,isea)
      my = ijkcel(4,isea)
      ! determine cell lat/lon (bottom left corner of cell)
      lon = cx0 + ijkcel(1,isea) * dlon
      lat = cy0 + ijkcel(2,isea) * dlat
      ! for output type 1 (seapoint array), just check whether
      ! cell centre is within specified domain range, and update
      ! output mask accordingly:
      if( smcotype .eq. 1 ) then
        ! ensure longitude ranges are aligned
        lon = lon + 0.5 * mx * dlon
        lat = lat + 0.5 * my * dlat
        if(lon .lt. sxo) lon = lon + 360.0
        if(lon .gt. exo) lon = lon - 360.0
        ! now check if it is within range of requested domain:
        if(lon .ge. sxo .and. lon .le. exo .and.           &
             lat .ge. syo .and. lat .le. eyo ) then
          smcmask(isea) = .true.
          smcidx(j) = isea
          j = j + 1
        endif
        cycle
      endif ! smcotype == 1
      ! for output type 2 (area averaged regular grid), determine
      ! smc grid cell location and coverage in output grid:
      ! align lons
      if(lon .lt. sxo) then
        lon = lon + 360.
      endif
      if(lon .gt. exo) then
        lon = lon - 360.
      endif
      ! find first sw cell in design grid:
      ! we add on 1/2 of the smallest smc cell dlon/dlat values to ensure
      ! source grid cell ends up in the correct target grid cell (after
      ! integer trunction):
      ixx = floor((lon + 0.5*dlon - sxo) / dxo) + 1
      iyy = floor((lat + 0.5*dlat - syo) / dyo) + 1
      ! if we fall outside the left/bottom edge of the design grid,
      ! check for cases where the smc cell has a lon or lat
      ! scaling factor > cfac (design grid is assumed to align
      ! its origin with cells of size cfac). for such cells,
      ! keep moving the left/bottom edge up by cfac until
      ! the sw corner (possibly) matches a design grid cell.
      if(ixx .le. 0 .and. ixx + mx / celfac .gt. 0) then
        do while(mx .gt. cfac)
          mx = mx - cfac
          lon = lon + dlon * cfac
          ixx = floor((lon + 0.5*dlon - sxo) / dxo) + 1
          if(ixx .gt. 0) exit ! found cell lon-edge in design grid
        enddo
      endif
      if(iyy .le. 0 .and. iyy + my / celfac .gt. 0) then
        do while(my .gt. cfac)
          my = my - cfac
          lat = lat + dlat * cfac
          iyy = floor((lat + 0.5*dlat - syo) / dyo) + 1
          if(iyy .gt. 0) exit ! found cell lat-edge in design grid
        enddo
      endif
      ! if smc cell definitely out of design grid domain, then cycle.
      if(ixx .le. 0 .or. ixx .gt. nxo .or.                          &
           iyy .le. 0 .or. iyy .gt. nyo) then
        xidx(isea) = -1
        yidx(isea) = -1
        cycle
      endif
      xidx(isea) = ixx
      yidx(isea) = iyy
      nsmc = nsmc + 1
      smcidx(nsmc) = isea
      ! find out how many cells it covers in the x/y directions:
      xspan(isea) = max(1, int(mx / celfac))
      yspan(isea) = max(1, int(my / celfac))
      ! do a bit of error checking (non fatal - just produced warning):
      if(xspan(isea) .gt. 1) then
        if(abs((sxo+(ixx-1)*dxo) - lon) .gt. dxo/100.0) then
          print*, 'potential problem with smc grid cell span:'
          print*, xspan(isea), float(mx) / celfac
          print*, lon,lat
          print*, sxo+(ixx-1)*dxo,syo+iyy*dyo,dxo,dyo
          print*, "diff:", (sxo+(ixx-1)*dxo) - lon
        endif
      endif
      ! calc cell weight in relation to output grid:
      wts(isea) = min(1., dble(min(celfac, mx) * min(celfac, my)) / &
           (celfac**2))
    enddo
    ! reset sxo and syo to be the cell-centre (currently cell sw edge):
    sxo = sxo + 0.5 * dxo
    syo = syo + 0.5 * dyo
  end subroutine smc_interp
  !--------------------------------------------------------------------------
  !> @brief regrid smc data onto a regular grid
  !>
  !> @details regrids scalar data from the smc grid onto a regular grid.
  !>  uses pre-calculated grid indices and weights generated from the
  !>  smc_interp() subroutine.
  !>
  !> @remark if source field is directional data, use the w3s2xy_smcrg_dir()
  !>  subroutine instead.
  !>
  !> @param[in]  s   source field, on smc grid.
  !> @param[out] xy  storage for regridded field; must be 2d array with
  !>                 dimensions of (nxo,nyo).
  !>
  !> @author chris bunney
  !> @date 02-jul-2013
  !>
  subroutine w3s2xy_smcrg(s, xy)
    implicit none
    ! input parameters:
    real, intent(in)  :: s(:)
    real, intent(out) :: xy(nxo,nyo)
    ! local parameters
    integer           :: i, j, ix, iy, isea, ismc
    ! initialise coverage and output arrays:
    cov(:,:) = 0.0
    xy(:,:) = 0.0
    do ismc=1,nsmc
      isea = smcidx(ismc)
      if(s(isea) .eq. undef) cycle   ! mdi
      ! loop over number of spanned cells:
      do i=0, xspan(isea) - 1
        do j=0, yspan(isea) - 1
          ix = xidx(isea) + i
          iy = yidx(isea) + j
          ! spans outside of grid?
          if(ix .gt. nxo .or. iy .gt. nyo) cycle
          ! interpolate:
          xy(ix, iy) = xy(ix, iy) + s(isea) * wts(isea)
          ! keep track of how much of cell is (wet) covered:
          cov(ix, iy) = cov(ix, iy) + wts(isea)
        enddo
      enddo
    enddo
    ! create coastline by masking out areas with < 50% coverage:
    do ix=1,nxo
      do iy=1,nyo
        if(mapsmc(ix,iy) .eq. 0) then
          ! make land point
          xy(ix,iy) = undef
        else if(cov(ix,iy) .lt. 0.5) then
          ! more than half of cell has undef values - set to noval:
          xy(ix,iy) = noval
        else if(cov(ix,iy) .lt. 1.0) then
          ! if coverage < 1.0, scale values back to full cell coverage.
          ! without this step, points around coast could end up with lower
          ! waveheights due to weights not summing to 1.0:
          xy(ix,iy) = xy(ix,iy) * ( 1.0 / cov(ix,iy) )
        endif
      enddo
    enddo
    return
  end subroutine w3s2xy_smcrg
  !--------------------------------------------------------------------------
  !> @brief regrid directional smc data onto a regular grid
  !>
  !> @details regrids directioanl scalar data from the smc grid onto
  !>  a regular grid. uses pre-calculated grid indices and weights
  !>  generated from the smc_interp() subroutine.
  !>
  !> @remark functionality as per w3s2xy_smc(), but decomposes the field
  !>  into u/v components first to ensure proper area averaging of
  !>  directional data (handles cyclic transition between 359 -> 0 degrees).
  !>
  !> @param[in]  s   directional source field, on smc grid.
  !> @param[out] xy  storage for regridded field; must be 2d array with
  !>                 dimensions of (nxo,nyo).
  !>
  !> @author chris bunney
  !> @date 02-jul-2013
  !>
  subroutine w3s2xy_smcrg_dir(s, xy)
    implicit none
    ! input parameters:
    real, intent(in)  :: s(:)
    real, intent(out) :: xy(nxo,nyo)
    ! local parameters
    integer           :: i, j, ix, iy, isea, ismc
    real, allocatable :: aux1(:,:), aux2(:,:)
    real              :: coss, sins
    ! initialise coverage and output arrays:
    allocate(aux1(nxo,nyo),aux2(nxo,nyo))
    cov(:,:) = 0.0
    xy(:,:) = 0.0
    aux1(:,:) = 0.0
    aux2(:,:) = 0.0
    do ismc=1,nsmc
      isea = smcidx(ismc)
      if(s(isea) .eq. undef) cycle   ! mdi
      coss = cos(s(isea))
      sins = sin(s(isea))
      ! loop over number of spanned cells:
      do i=0, xspan(isea) - 1
        do j=0, yspan(isea) - 1
          ix = xidx(isea) + i
          iy = yidx(isea) + j
          ! spans outside of grid?
          if(ix .gt. nxo .or. iy .gt. nyo) cycle
          ! interpolate:
          !xy(ix, iy) = xy(ix, iy) + s(isea) * wts(isea)
          aux1(ix, iy) = aux1(ix, iy) + coss * wts(isea)
          aux2(ix, iy) = aux2(ix, iy) + sins * wts(isea)
          ! keep track of how much of cell is (wet) covered:
          cov(ix, iy) = cov(ix, iy) + wts(isea)
        enddo
      enddo
    enddo
    ! create coastline by masking out areas with < 50% coverage:
    do ix=1,nxo
      do iy=1,nyo
        if(mapsmc(ix,iy) .eq. 0) then
          ! make land point
          xy(ix,iy) = undef
        else if(cov(ix,iy) .lt. 0.5) then
          ! more than half of cell has undef values - set to noval
          xy(ix,iy) = noval
        else if(cov(ix,iy) .lt. 1.0) then
          ! if coverage < 1.0, scale values back to full cell coverage.
          ! without this step, points around coast could end up with lower
          ! waveheights due to weights not summing to 1.0:
          xy(ix,iy) = atan2(aux2(ix,iy), aux1(ix,iy))
          xy(ix,iy) = mod(630. - rade * xy(ix,iy), 360. )
        else
          xy(ix,iy) = atan2(aux2(ix,iy), aux1(ix,iy))
          xy(ix,iy) = mod(630. - rade * xy(ix,iy), 360. )
        endif
      enddo
    enddo
    return
  end subroutine w3s2xy_smcrg_dir
  !--------------------------------------------------------------------------
  !> @brief calculates a new mapsta using smc grid cell averaging.
  !>
  !> @author chris bunney
  !> @date 02-jul-2013
  !>
  subroutine mapsta_smc()
    implicit none
    ! local parameters
    integer           :: i, j, ix, iy, imx, imy, isea
    ! initialise coverage and output arrays:
    cov(:,:) = 0.0
    mapsmc(:,:) = 0
    do isea=1,nsea
      imx = mapsf(isea,1)
      imy = mapsf(isea,2)
      if(xidx(isea) .eq. -1) cycle   ! out of grid
      ! loop over number of spanned cells:
      do i=0, xspan(isea) - 1
        do j=0, yspan(isea) - 1
          ix = xidx(isea) + i
          iy = yidx(isea) + j
          ! spans outside of grid?
          if(ix .gt. nxo .or. iy .gt. nyo) cycle
          ! mapsta values: 0=excluded, (+-)1=sea, (+-2)=input boundary
          ! we will just keep track of sea and non-sea points:
          if(mapsta(imy, imx) .ne. 0) then
            ! keep track of how much of cell is (wet) covered:
            cov(ix, iy) = cov(ix, iy) + wts(isea)
          endif
        enddo
      enddo
    enddo
    ! create coastline by masking out areas with < 50% coverage:
    do ix=1,nxo
      do iy=1,nyo
        if(cov(ix,iy) .lt. 0.5) then
          mapsmc(ix, iy) = 0
        else
          mapsmc(ix, iy) = 1
        endif
      enddo
    enddo
    return
  end subroutine mapsta_smc
  !--------------------------------------------------------------------------
  !> @brief read interpolation information from smcint.ww3
  !>
  !> @details reads the interpolation indices and distance weights from the
  !>  smcint.ww3 file generated by ww3_smcint program.
  !>
  !> @author chris bunney
  !> @date 18-apr-2018
  !>
  subroutine read_smcint()
    use w3servmd, only: extcde
    implicit none
    ! locals
    integer :: ierr, i, j
    real :: plato, plono ! not used yet....future version might allow
    ! output to a rotated pole grid...
    ndsmc = 50
    open(ndsmc, file='smcint.ww3', status='old', form='unformatted', convert=file_endian, iostat=ierr)
    if(ierr .ne. 0) then
      write(*,*) "error! failed to open smcint.ww3 for reading"
      call extcde(1)
    endif
    ! header
    read(ndsmc) nxo, nyo, sxo, syo, dxo, dyo, plono, plato
    allocate(nnidx(nxo,nyo), xdist(nxo,nyo), ydist(nxo,nyo))
    ! indices and weights:
    read(ndsmc)((nnidx(i,j), xdist(i,j), ydist(i,j),i=1,nxo),j=1,nyo)
    close(ndsmc)
  end subroutine read_smcint
  !--------------------------------------------------------------------------
  !> @brief calculates weights for smc to arbitrary grid intepolation.
  !>
  !> @details
  !>  calculates the interpolation indices and weights for regridding
  !>  an smc grid to an arbitrary regular grid. calculated index is that of
  !>  the smc cell that contains output cell centre. weights are the distance
  !>  in metres between the output and smc cell centres.
  !>
  !>  a future version <em>may</em> allow for output grids to be on a
  !>  rotated pole.
  !>
  !> @author chris bunney
  !> @date 18-apr-2018
  !>
  subroutine calc_interp()
    use w3gdatmd, only: clats
    use constants, only : dera, radius
    implicit none
    integer :: ierr, i, j, isea, n, cfac
    real :: mlon(nsea), mlat(nsea), olon(nxo,nyo), olat(nxo,nyo),     &
         ang(nxo,nyo), lon, lat
    ! determine smallest cell size factor:
    cfac = 2**(nrlv - 1)
    ! get smallest smc grid cells step size:
    dlat = sy / cfac
    dlon = sx / cfac
    allocate(xdist(nx,ny), ydist(ny,nx))
    ! model lat/lons:
    do isea = 1,nsea
      mlon(isea) = (x0-0.5*sx) + (ijkcel(1,isea) + 0.5 * ijkcel(3,isea)) * dlon
      mlat(isea) = (y0-0.5*sy) + (ijkcel(2,isea) + 0.5 * ijkcel(4,isea)) * dlat
    enddo
    ! generate output grid cell centres:
    do i=1,nxo
      do j=1,nyo
        olon(i,j) = sxo + (i-1) * dxo
        olat(i,j) = syo + (j-1) * dyo
      enddo
    enddo
    ! cycle over output grid points and find containing smc cell:
    ! note : brute force!
    nnidx(:,:) = -1
    do i=1,nxo
      print*,i,' of ',nxo
      do j=1,nyo
        lon = olon(i,j)
        lat = olat(i,j)
        if(lon .lt. x0 - sx / 2) lon = lon + 360.0
        if(lon .gt. (x0 + (nx-1) * sx) + 0.5 * sx) lon = lon - 360.0
        do isea=1,nsea
          if(mlon(isea) - 0.5 * ijkcel(3,isea) * dlon .le. lon .and.    &
               mlon(isea) + 0.5 * ijkcel(3,isea) * dlon .ge. lon .and.    &
               mlat(isea) - 0.5 * ijkcel(4,isea) * dlat .le. lat .and.    &
               mlat(isea) + 0.5 * ijkcel(4,isea) * dlat .ge. lat ) then
            ! match!
            nnidx(i,j) = isea
            xdist(i,j) = (lon - mlon(isea)) * dera * radius * clats(isea)
            ydist(i,j) = (lat - mlat(isea)) * dera * radius
            exit
          endif
        enddo
      enddo
    enddo
  end subroutine calc_interp
  !--------------------------------------------------------------------------
  !> @brief fill regular grid using nearest smc point data
  !>
  !> @details directional fields (dirn=true) will be assumed to be in radians
  !>  and will be converted to degrees in nautical convention.
  !>
  !> @param[in]  s    input array on smc grid
  !> @param[out] xy   output array to store interpolated 2d field
  !> @param[in]  dirn set to .true. if s is a directional field
  !>
  !> @author chris bunney
  !> @date 18-apr-2018
  !>
  subroutine w3s2xy_smcnn(s, xy, dirn)
    implicit none
    ! input parameters:
    real, intent(in)    :: s(:)        ! inupt array
    real, intent(out)   :: xy(nxo,nyo) ! output data
    logical, intent(in) :: dirn        ! directional field?
    ! local parameters
    integer           :: i, j, ix, iy, isea, ismc
    do ix = 1,nxo
      do iy = 1,nyo
        isea = nnidx(ix,iy) ! nearest neighbour smc point
        if(isea .eq. -1) then
          ! land
          xy(ix,iy) = undef
        else
          if(s(isea) .eq. undef) then
            ! set undefined sea points to noval
            xy(ix,iy) = noval
          else
            xy(ix,iy) = s(isea)
            if(dirn) then
              ! convert direction fields to degrees nautical
              xy(ix,iy) = mod(630. - rade * xy(ix,iy), 360.0)
            endif
          endif
        endif
      enddo
    enddo
  end subroutine w3s2xy_smcnn
  !--------------------------------------------------------------------------
  !> @brief nearest neighbour interpolation
  !>
  !> @details fill regular grid using nearest smc point data and interpolate
  !>  output value based on local gradient and distance between grid
  !>  cell centres.
  !>
  !>  directional fields (dirn=true) will be assumed to be in radians
  !>  and will be converted to degrees in nautical convention.
  !>
  !> @param[in]  s    input array on smc grid
  !> @param[out] xy   output array to store interpolated 2d field
  !> @param[in]  dirn set to .true. if s is a directional field
  !>
  !> @author chris bunney
  !> @date 18-apr-2018
  !>
  subroutine w3s2xy_smcnn_int(s, xy, dirn)
    use w3psmcmd, only: smcgradn
    implicit none
    ! input parameters:
    real, intent(in)    :: s(:)        ! input array
    real, intent(out)   :: xy(nxo,nyo) ! output array
    logical, intent(in) :: dirn        ! directional field?
    ! locals
    integer           :: i, j, ix, iy, isea, ismc
    real              :: cvq(-9:nsea)
    real              :: grdx(nsea), grdy(nsea)
    ! calculate local gradients:
    cvq(1:nsea) = s ! need to copy s into array with bounds starting at -9
    call smcgradn(cvq, grdx, grdy, 0)
    ! interpolate:
    do ix = 1,nxo
      do iy = 1,nyo
        isea = nnidx(ix,iy) ! nearest neighbour smc point
        if(isea .eq. -1) then
          xy(ix,iy) = undef
        else
          ! interpolate using local gradient and distance from cell centre:
          xy(ix,iy) = s(isea) + grdx(isea) * xdist(ix,iy) + grdy(isea) * ydist(ix,iy)
          if(dirn) then
            ! convert direction fields to degrees nautical
            xy(ix,iy) = mod(630. - rade * xy(ix,iy), 360.0)
          endif
        endif
      enddo
    enddo
  end subroutine w3s2xy_smcnn_int
  !--------------------------------------------------------------------------
  !--------------------------------------------------------------------------
  !> @brief entry point for smc version of w3s2xy.
  !>
  !> @details dispatches to regridding subroutine based on smcotype.
  !>  optional dir logical specifies whether field is a directional
  !>  value; in which case it will be decomposed into u/v components
  !>  prior to any interpolation.
  !>
  !> @param[in]  s    input array on smc grid
  !> @param[out] xy   output array to store interpolated 2d field
  !> @param[in]  dir  (optional) set to .true. if s is a directional field
  !>
  !> @author chris bunney
  !> @date 18-apr-2018
  !>
  subroutine w3s2xy_smc(s, xy, dir)
    implicit none
    real, intent(in)  :: s(:)
    real, intent(out) :: xy(nxo,nyo)
    logical, optional :: dir
    logical           :: dirn
    integer           :: isea
    if(present(dir)) then
      dirn = dir
    else
      dirn = .false.
    endif
    if(smcotype .eq. 1) then
      ! flat sea point array
      xy(:,1) = pack(s, smcmask)
      if(dirn) then
        ! convert to nautical convention in degrees
        do isea=1,nxo
          if(xy(isea,1) .ne. undef) then
            xy(isea,1) = mod(630. - rade * xy(isea,1), 360.)
          endif
        enddo
      endif
    elseif(smcotype .eq. 2) then
      ! regular gridded smc cells
      if(dirn) then
        call w3s2xy_smcrg_dir(s, xy)
      else
        call w3s2xy_smcrg(s, xy)
      endif
    elseif(smcotype .eq. 3) then
      ! regridded to arbitrary regular grid with interpolation
      call w3s2xy_smcnn_int(s, xy, dirn)
    elseif(smcotype .eq. 4) then
      ! regridded to arbitrary regular grid - no interpolation
      call w3s2xy_smcnn(s, xy, dirn)
    else
      write(*,*) "uknonwn smc type!", smcotype
      ! unknown smc type!
      stop
    endif
  end subroutine w3s2xy_smc
  !--------------------------------------------------------------------------
end module w3smcomd
