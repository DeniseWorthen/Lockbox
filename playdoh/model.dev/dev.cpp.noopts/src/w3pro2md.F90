!> @file
!> @brief bundles routines for third order porpagation scheme in single
!>  module.
!>
!> @author h. l. tolman
!> @date   29-may-2014
!>
!/ ------------------------------------------------------------------- /
!/    preprocessing macros
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |        t. j. campbell, nrl        |
!/                  |                               cpp |
!/                  | last update :         26-oct-2015 |
!/                  +-----------------------------------+
!/
!/    10-dec-2014 : origination.                     ( version 5.04 )
!/    26-oct-2015 : replace c style comments with fortran
!/                  style comments.                  ( version 5.09 )
!/
!/ 1. purpose :
!/
!/    define preprocessor macros for ww3 ftn source code.
!/
!/ 2. method :
!/
!/ 3. parameters :
!/
!/ 4. subroutines used :
!/
!/ 5. called by :
!/
!/ 6. error messages :
!/
!/ 7. remarks :
!/
!/    this file uses fortran style comments, and hence, can only be
!/    included in the fortran (ftn) source files.  the fortran style
!/    comments are used because not all fortran pre-processors recognize
!/    the c style comments.
!/
!/    the "src/w3macros.h" and 36 macros are defined by cpp.
!/
!/ 8. structure :
!/
!/    see source code.
!/
!/ 9. source code :
!/
!/ ------------------------------------------------------------------- /
!/
!/ macros to wrap checking allocate/deallocate status
!/
!/
!/ end of w3macros.h ------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!>
!> @brief bundles routines for third order porpagation scheme in single
!>  module.
!>
!> @author h. l. tolman
!> @date   29-may-2014
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3pro2md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2014 |
  !/                  +-----------------------------------+
  !/
  !/    04-feb-2000 : origination.                        ( version 2.00 )
  !/    24-jan-2001 : flat grid version                   ( version 2.06 )
  !/    08-feb-2001 : uq routines moved to own module     ( version 2.08 )
  !/    09-feb-2001 : clean up of parameter lists         ( version 2.08 )
  !/    14-feb-2001 : unit numbers in uq routines         ( version 2.08 )
  !/    13-nov-2001 : sub-grid obstacles added.           ( version 2.14 )
  !/    26-dec-2002 : moving grid option.                 ( version 3.02 )
  !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    07-sep-2005 : improved xy boundary conditions.    ( version 3.08 )
  !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
  !/    05-mar-2008 : added nec sxf90 compiler directives.
  !/                  (chris bunney, uk met office)       ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    23-dec-2010 : fix hpfac and hqfac by including the cos(ygrd)
  !/                  factor with dxdp and dxdq terms.    ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    01-jul-2013 : adding uq and uno switches to chose between third
  !/                  and second order schemes.           ( version 4.12 )
  !/    29-may-2014 : adding omph switch.                 ( version 5.02 )
  !/
  !/    copyright 2009-2014 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     bundles routines for third order porpagation scheme in single
  !     module.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      trnmin    r.p.  private   minimum transparancy for local
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3map2    subr. public   set up auxiliary maps.
  !      w3xyp2    subr. public   third order spatial propagation.
  !      w3ktp2    subr. public   third order spectral propagation.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !      w3qck1    subr. w3uqckmd regular grid uq scheme.
  !      w3qck2    subr.   id.    irregular grid uq scheme.
  !      w3qck3    subr.   id.    regular grid uq scheme + obstructions.
  !      w3uno2    subr. w3uno2md uno2 scheme for irregular grid.
  !      w3uno2r   subr.   id.    uno2 scheme reduced to regular grid.
  !      w3uno2s   subr.   id.    uno2 regular grid with subgrid
  !                               obstruction.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/uq    3rd order uq propagation scheme.
  !       !/uno   2nd order uno propagation scheme.
  !
  !       !/mgp   correct for motion of grid.
  !
  !       !/omph  hybrid openmp directives.
  !
  !       !/tdyn, !/dss0, !/xw0, !/xw1
  !               diffusion options in w3xyp2
  !
  !       !/s     enable subroutine tracing.
  !       !/tn    enable test output.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ public variables
  !/
  public
  !/
  !/ private data
  !/
  real, private, parameter:: trnmin = 0.95
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief generate 'map' arrays for the ultimate quickest scheme.
  !>
  !> @author h. l. tolman
  !> @date   09-nov-2005
  !>
  subroutine w3map2
    !/
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         09-nov-2005 |
    !/                  +-----------------------------------+
    !/
    !/    19-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    15-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    09-feb-2001 : clean up of parameter lists         ( version 2.08 )
    !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
    !/
    !  1. purpose :
    !
    !     generate 'map' arrays for the ultimate quickest scheme.
    !
    !  2. method :
    !
    !     mapx2, mapy2, mapth2 and mapwn2 contain consecutive 1-d spatial
    !     grid counters (e.g., ixy = (ix-1)*my + iy). the arrays are
    !     devided in three parts. for mapx2, these ranges are :
    !
    !         1    - nmx0  counters ixy for which grid point (ix,iy) and
    !                      (ix+1,iy) both are active grid points.
    !       nmx0+1 - nmx1  id. only (ix,iy) active.
    !       nmx1+1 - nmx2  id. only (ix+1,iy) active.
    !
    !     the array mapy2 has a similar layout varying iy instead of ix.
    !     mapxy contains similar information for the cross term in the
    !     diffusion correction (counter nmxy only).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd wave model routine.
    !     ---------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     ------------------------------------------------------
    !      1.   map mapx2
    !        a  range 1 to nmx0
    !        b  range nmx0+1 to nmx1
    !        c  range nmx1+1 to nmx2
    !      2.   map mapy2
    !        a  range 1 to nmy0
    !        b  range nmy0+1 to nmy1
    !        c  range nmy1+1 to nmy2
    !      3.   map mapaxy and mapxy
    !      4.   maps for intra-spectral propagation
    !        a  mapth2, mapatk
    !        b  mapwn2
    !     ------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable test output.
    !
    ! 10. source code :
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk, nth, nspec, nx, ny, nsea, mapsta
    use w3adatmd, only: nmx0, nmx1, nmx2, nmy0, nmy1, nmy2, nact,   &
         nmxy, mapx2, mapy2, mapaxy, mapxy,          &
         mapth2, mapwn2
    use w3odatmd, only: ndst
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ix, iy, ixy0, ix2, iy2, ix0, iy0,    &
         ik, ith, isp, isp0, isp2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  map mapx2 ------------------------------------------------------ *
    ! 1.a range 1 to nmx0
    !
    !
    nmx0   = 0
    do ix=1, nx
      ixy0   = (ix-1)*ny
      ix2    = 1 + mod(ix,nx)
      do iy=2, ny-1
        if ( mapsta(iy,ix).eq.1 .and. mapsta(iy,ix2).eq.1 ) then
          nmx0   = nmx0 + 1
          mapx2(nmx0) = ixy0 + iy
        end if
      end do
    end do
    !
    ! 1.b range nmx0+1 to nmx1
    !
    nmx1   = nmx0
    do ix=1, nx
      ixy0   = (ix-1)*ny
      ix2    = 1 + mod(ix,nx)
      do iy=2, ny-1
        if ( mapsta(iy,ix).eq.1 .and. mapsta(iy,ix2).ne.1 ) then
          nmx1   = nmx1 + 1
          mapx2(nmx1) = ixy0 + iy
        end if
      end do
    end do
    !
    ! 1.c range nmx1+1 to nmx2
    !
    nmx2   = nmx1
    do ix=1, nx
      ixy0   = (ix-1)*ny
      ix2    = 1 + mod(ix,nx)
      do iy=2, ny-1
        if ( mapsta(iy,ix).ne.1 .and. mapsta(iy,ix2).eq.1 ) then
          nmx2   = nmx2 + 1
          mapx2(nmx2) = ixy0 + iy
        end if
      end do
    end do
    !
    !
    ! 2.  map mapy2 ------------------------------------------------------ *
    ! 2.a range 1 to nmy0
    !
    !
    nmy0   = 0
    do ix=1, nx
      ixy0   = (ix-1)*ny
      do iy=1, ny-1
        iy2    = iy + 1
        if ( mapsta(iy,ix).eq.1 .and. mapsta(iy2,ix).eq.1 ) then
          nmy0   = nmy0 + 1
          mapy2(nmy0) = ixy0 + iy
        end if
      end do
    end do
    !
    ! 2.b range nmy0+1 to nmy1
    !
    nmy1   = nmy0
    do ix=1, nx
      ixy0   = (ix-1)*ny
      do iy=1, ny-1
        iy2    = iy + 1
        if ( mapsta(iy,ix).eq.1 .and. mapsta(iy2,ix).ne.1 ) then
          nmy1   = nmy1 + 1
          mapy2(nmy1) = ixy0 + iy
        end if
      end do
    end do
    !
    ! 2.c range nmy1+1 to nmy2
    !
    nmy2   = nmy1
    do ix=1, nx
      ixy0   = (ix-1)*ny
      do iy=1, ny-1
        iy2    = iy + 1
        if ( mapsta(iy,ix).ne.1 .and. mapsta(iy2,ix).eq.1 ) then
          nmy2   = nmy2 + 1
          mapy2(nmy2) = ixy0 + iy
        end if
      end do
    end do
    !
    !
    ! 3.  map mapaxy and mapxy ------------------------------------------- *
    !
    nact   = 0
    do ix=1, nx
      iy0    = (ix-1)*ny
      do iy=2, ny-1
        if ( mapsta(iy,ix).eq.1 ) then
          nact         = nact + 1
          mapaxy(nact) = iy0 + iy
        end if
      end do
    end do
    !
    nmxy   = 0
    do ix=1, nx
      ixy0   = (ix-1)*ny
      ix2    = ix+1
      if ( ix .eq. nx ) ix2 =  1
      do iy=2, ny-1
        if ( mapsta( iy ,ix ).ge.1 .and.                            &
             mapsta( iy ,ix2).ge.1 .and.                            &
             mapsta(iy+1,ix ).ge.1 .and.                            &
             mapsta(iy+1,ix2).ge.1 ) then
          nmxy   = nmxy + 1
          mapxy(nmxy) = ixy0 + iy
        end if
      end do
    end do
    !
    ! 4.  maps for intra-spectral propagation ---------------------------- *
    !
    if ( mapth2(1) .ne. 0 ) return
    !
    !
    ! 4.a mapth2 and mapbtk
    !
    do ik=1, nk
      do ith=1, nth
        isp    = ith + (ik-1)*nth
        isp2   = (ik+1) + (ith-1)*(nk+2)
        mapth2(isp) = isp2
      end do
    end do
    !
    !
    ! 4.b mapwn2
    !
    isp0   = 0
    do ik=1, nk-1
      do ith=1, nth
        isp0   = isp0 + 1
        isp2   = (ik+1) + (ith-1)*(nk+2)
        mapwn2(isp0) = isp2
      end do
    end do
    !
    do ith=1, nth
      isp0   = isp0 + 1
      isp2   = nk+1 + (ith-1)*(nk+2)
      mapwn2(isp0) = isp2
    end do
    !
    do ith=1, nth
      isp0   = isp0 + 1
      isp2   = 1 + (ith-1)*(nk+2)
      mapwn2(isp0) = isp2
    end do
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3map2 ----------------------------------------------------- /
    !/
  end subroutine w3map2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief propagation in physical space for a given spectral component.
  !>
  !> @param[in]    isp     number of spectral bin (ik-1)*nth+ith.
  !> @param[in]    dtg     total time step.
  !> @param[in]    mapsta  grid point status map.
  !> @param[in]    mapfs   storage map.
  !> @param[inout] vq      field to propagate.
  !> @param[in]    vgx
  !> @param[in]    vgy
  !>
  !> @author h. l. tolman
  !> @date   29-may-2014
  !>
  subroutine w3xyp2 ( isp, dtg, mapsta, mapfs, vq, vgx, vgy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-may-2014 |
    !/                  +-----------------------------------+
    !/
    !/    07-jul-1998 : final fortran 77                    ( version 1.18 )
    !/    15-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    24-jan-2001 : flat grid version                   ( version 2.06 )
    !/    09-feb-2001 : clean up of parameter lists         ( version 2.08 )
    !/    14-feb-2001 : unit numbers in uq routines         ( version 2.08 )
    !/    13-nov-2001 : sub-grid obstructions.              ( version 2.14 )
    !/    26-dec-2002 : moving grid option,                 ( version 3.02 )
    !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    07-sep-2005 : improved boundary conditions.       ( version 3.08 )
    !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
    !/    05-mar-2008 : added nec sxf90 compiler directives.
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    23-dec-2010 : fix hpfac and hqfac by including the cos(ygrd)
    !/                  factor with dxdp and dxdq terms.    ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    01-jul-2013 : adding uq and uno switches to chose between third
    !/                  and second order schemes.           ( version 4.12 )
    !/    29-may-2014 : adding omph switch.                 ( version 5.02 )
    !/
    !  1. purpose :
    !
    !     propagation in physical space for a given spectral component.
    !
    !  2. method :
    !
    !     third-order ultimate quickest scheme and diffusion correction
    !     for linear dispersion (see manual).
    !     curvilinear grid implementation: fluxes are computed in index space
    !       and then transformed back into physical space.  the diffusion term
    !       is handled in physical space.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isp     int.   i   number of spectral bin (ik-1)*nth+ith
    !       dtg     real   i   total time step.
    !       mapsta  i.a.   i   grid point status map.
    !       mapfs   i.a.   i   storage map.
    !       vq      r.a.  i/o  field to propagate.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       ntloc   int   number of local time steps.
    !       dtloc   real  local propagation time step.
    !       cgd     real  deep water group velocity.
    !       dssd, dnnd    deep water diffusion coefficients.
    !       vlcflx  r.a.  local courant numbers in index space (norm. velocities)
    !       vlcfly  r.a.
    !       cxtot   r.a.  propagation velocities in physical space.
    !       cytot   r.a.
    !       cellp   real  cell reynolds/peclet number used to calculate
    !                     diffusion coefficient for growing spectral
    !                     components.
    !       dfrr    real  relative frequency increment.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd wave model routine.
    !     ---------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - note that the ultimate limiter does not guarantee non-zero
    !       energies.
    !     - the present scheme shows a strong distortion when propaga-
    !       ting a field under an angle with the grid in a truly 2-d
    !       fashion. propagation is therefore split along the two
    !       axes.
    !     - two boundary treatments are available. the first uses real
    !       boundaries in each space. in this case waves will not
    !       penetrate in narrow straights under an angle with the grid.
    !       this behavior is improved by using a 'soft' option, in
    !       which the 'x' or 'y' sweep allows for energy to go onto
    !       the land. this improves the above behavior, but implies
    !       that x-y connenctions are required in barriers for them
    !       to become inpenetrable.
    !     - if tdyn is set to zero, all diffusion is skipped. set tdyn
    !       to a small positive number to have growth diffusion only.
    !     - curvilinear grid implementation. variables facx, facy, ccos, csin,
    !       ccurx, ccury are not needed and have been removed.  facx is accounted
    !       for as approriate in this subroutine.  facx is also accounted for in
    !       the case of .not.flcx.  since facx is removed, there is now a check for
    !       .not.flcx in this subroutine.  in cfl calcs dx and dy are omitted,
    !       since dx=dy=1 in index space.  curvilinear grid derivatives
    !       (dpdy, dqdx, etc.) and metric (gsqrt) are brought in via w3gdatmd.
    !     - factors vfdifx_fac vfdify_fac vfdifc_fac are introduced so that results
    !       match for test case tp2.3.  use of these factors is optional and removal
    !       can significantly reduce size/cost of code. these variants are marked as
    !       curv1 or curv2. ncep will make final decision re: which version to adopt.
    !       curv1 is the shorter version and results do not match the original code
    !       for all test cases. curv2 is the longer version and results do match the
    !       original. detailed explanation: discrepancies occur at the boundaries.
    !       this is because, at the boundaries, the pre-curvilinear version zeros out
    !       some terms in the diffusion calculation. since they are zeroed out,
    !       they aren't there to *cancel* certain other terms: these "other terms"
    !       affect the result, so they have to be retained in the long vesion (curv2)
    !       to get an exact match. in the short version, the "canceling out" is
    !       performed prior to coding the scheme, so both the canceled and canceling
    !       terms are always omitted.
    !
    !  8. structure :
    !
    !     ---------------------------------------------
    !       1.  preparations
    !         a set constants
    !         b initialize arrays
    !       2.  prepare arrays
    !         a velocities and 'q'
    !         b diffusion coefficients
    !       3.  loop over sub-steps
    !       ----------------------------------------
    !         a propagate
    !         b update boundary conditions
    !         c diffusion correction
    !       ----------------------------------------
    !       4.  store q field in spectra
    !     ---------------------------------------------
    !
    !  9. switches :
    !
    !       !/mgp   correct for motion of grid.
    !
    !       !/tdyn  dynamic increase of dtme
    !       !/dss0  disable diffusion in propagation direction
    !       !/xw0   propagation diffusion only.
    !       !/xw1   growth diffusion only.
    !
    !       !/omph  hybrid openmp directives.
    !
    !       !/s     enable subroutine tracing.
    !
    !       !/t     enable general test output.
    !       !/t1    dump of input field and fluxes.
    !       !/t2    dump of output field.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nk, nth, dth, xfr, esin, ecos, sig, nx, ny, &
         nsea, sx, sy, mapsf, iclose, flcx, flcy,    &
         iclose_none, iclose_smpl, iclose_trpl,      &
         dtcfl, clats, dtme, clatmn, flagll,         &
         hpfac, hqfac, dpdx, dpdy, dqdx, dqdy, gsqrt
    use w3wdatmd, only: time
    use w3adatmd, only: cg, wn, u10, cx, cy, atrnx, atrny, itime,   &
         nmx0, nmx1, nmx2, nmy0, nmy1, nmy2, nact,   &
         nmxy, mapx2, mapy2, mapaxy, mapxy
    use w3idatmd, only: flcur
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin,       &
         isbpi, bbpi0, bbpin, iaproc, naperr
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isp, mapsta(ny*nx), mapfs(ny*nx)
    real, intent(in)        :: dtg, vgx, vgy
    real, intent(inout)     :: vq(1-ny:ny*(nx+2))
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, ntloc, itloc, isea, ixy,    &
         ix,iy, iy0, ip, ibi
    integer                 :: ttest(2),dttst
    real                    :: cg0, cga, cgn, cgx, cgy, cxc, cyc,  &
         cxmin, cxmax, cymin, cymax
    real                    :: dtloc, dtrad,               &
         dfrr, cellp,  cgd, dssd,    &
         dnnd, dcell, xwind, tfac, dss, dnn
    real                    :: rd1, rd2
    real                    :: rfac, dfac, dvq, qxx, qxy, qyy
    real                    :: cp, cq
    logical                 :: yfirst
    logical                 :: global
    !/
    !/ automatic work arrays
    !/
    real                    :: vlcflx((nx+1)*ny), vlcfly((nx+1)*ny),&
         vfdifx(1-ny:nx*ny), vfdify(nx*ny),   &
         vfdifc(1-ny:nx*ny), vdxx((nx+1)*ny), &
         vdyy(nx*ny), vdxy((nx+1)*ny)
    real                    :: cxtot((nx+1)*ny), cytot(nx*ny)
    real                    :: vq_old(1-ny:ny*(nx+2))
    !curv2: begin -----------------------------------------------------------------
    real                    :: vfdifx_fac(1-ny:nx*ny),              &
         vfdify_fac(1-ny:nx*ny),              &
         vfdifc_fac(1-ny:nx*ny)
    !curv2: end -------------------------------------------------------------------
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !     if ( maxval(vq) .eq. 0. ) then
    !         if ( nbi .eq. 0 ) then
    !             return
    !           else
    !             if ( maxval(bbpi0(isp,:)) .eq. 0. .and.            &
    !                  maxval(bbpin(isp,:)) .eq. 0. ) return
    !           end if
    !       end if
    !
    ! 1.  preparations --------------------------------------------------- *
    if ( iclose .eq. iclose_trpl ) then
      if (iaproc .eq. naperr) &
           write(ndse,*)'subroutine w3xyp2 is not yet adapted for '//    &
           'tripole grids. stopping now.'
      call extcde ( 1 )
    end if
    ! 1.a set constants
    !
    global = iclose.ne.iclose_none
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    !
    cg0    = 0.575 * grav / sig(1)
    cga    = 0.575 * grav / sig(ik)
    cgx    = cga * ecos(ith)
    cgy    = cga * esin(ith)
    !
    if ( flcur ) then
      cxmin  = minval ( cx(1:nsea) )
      cxmax  = maxval ( cx(1:nsea) )
      cymin  = minval ( cy(1:nsea) )
      cymax  = maxval ( cy(1:nsea) )
      if ( abs(cgx+cxmin) .gt. abs(cgx+cxmax) ) then
        cgx    = cgx + cxmin
      else
        cgx    = cgx + cxmax
      end if
      if ( abs(cgy+cymin) .gt. abs(cgy+cymax) ) then
        cgy    = cgy + cymin
      else
        cgy    = cgy + cymax
      end if
      cxc    = max ( abs(cxmin) , abs(cxmax) )
      cyc    = max ( abs(cymin) , abs(cymax) )
    else
      cxc    = 0.
      cyc    = 0.
    end if
    !
    cgn    = 0.9999 * max( abs(cgx) , abs(cgy) , cxc, cyc, 0.001*cg0 )
    !
    ntloc  = 1 + int(dtg/(dtcfl*cg0/cgn))
    dtloc  = dtg / real(ntloc)
    dtrad  = dtloc
    if ( flagll ) dtrad=dtrad/(dera*radius)
    !
    if ( flagll ) then
      rfac = dera * radius
      dfac = 1. / rfac**2
    else
      rfac = 1.
      dfac = 1.
    end if
    !
    ttest(1) = time(1)
    ttest(2) = 0
    dttst = dsec21(ttest,time)
    yfirst = mod(nint(dttst/dtg),2) .eq. 0
    !
    !
    !
    if ( dtme .ne. 0. ) then
      dfrr   = xfr - 1.
      cellp  = 10.
      cgd    = 0.5 * grav / sig(ik)
      dssd   = ( dfrr * cgd )**2 * dtme / 12.
      dnnd   = ( cgd * dth )**2 * dtme / 12.
    end if
    !
    ! 1.b initialize arrays
    !
    !
    vlcflx = 0.
    vlcfly = 0.
    vfdifx = 0.
    vfdify = 0.
    vfdifc = 0.
    vdxx   = 0.
    vdyy   = 0.
    vdxy   = 0.
    cxtot  = 0.
    cytot  = 0.
    !
    ! 2.  calculate velocities and diffusion coefficients ---------------- *
    ! 2.a velocities
    !
    !     q     = ( a / cg * clats )
    !     lcflx = ( cos*cg / clats ) * dt / dx
    !     lcfly = (     sin*cg )     * dt / dx
    !
    !
    !
    do isea=1, nsea
      ixy         = mapsf(isea,3)
      vq    (ixy) = vq(ixy) / cg(ik,isea) * clats(isea)
      cxtot(ixy) = ecos(ith) * cg(ik,isea) / clats(isea)
      cytot(ixy) = esin(ith) * cg(ik,isea)
    end do
    !
    !
    if ( flcur ) then
      do isea=1, nsea
        ixy         = mapsf(isea,3)
        cxtot(ixy) = cxtot(ixy) + cx(isea)/clats(isea)
        cytot(ixy) = cytot(ixy) + cy(isea)
      end do
    end if
    !
    !
    do isea=1, nsea
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      ixy    = mapsf(isea,3)
      cp=cxtot(ixy)*dpdx(iy,ix)+cytot(ixy)*dpdy(iy,ix)
      cq=cxtot(ixy)*dqdx(iy,ix)+cytot(ixy)*dqdy(iy,ix)
      vlcflx(ixy) = cp*dtrad
      vlcfly(ixy) = cq*dtrad
    end do
    !
    !
    ! 2.b diffusion coefficients
    !
    if ( dtme .ne. 0. ) then
      !
      !
      do isea=1, nsea
        ix        = mapsf(isea,1)
        iy        = mapsf(isea,2)
        ixy       = mapsf(isea,3)
        if ( min ( atrnx(ixy,1) , atrnx(ixy,-1) ,                 &
             atrny(ixy,1) , atrny(ixy,-1) ) .gt. trnmin ) then
          dcell     = cgd * min ( hpfac(iy,ix)*rfac, &
               hqfac(iy,ix)*rfac ) / cellp
          xwind     = 3.3 * u10(isea)*wn(ik,isea)/sig(ik) - 2.3
          xwind     = max ( 0. , min ( 1. , xwind ) )
          tfac      = min ( 1. , (clats(isea)/clatmn)**2 )
          dss       = xwind * dcell + (1.-xwind) * dssd * tfac
          dnn       = xwind * dcell + (1.-xwind) * dnnd * tfac
          vdxx(ixy) = dtloc * (dss*ecos(ith)**2+dnn*esin(ith)**2)
          vdyy(ixy) = dtloc * (dss*esin(ith)**2+dnn*ecos(ith)**2) &
               / clats(isea)**2
          vdxy(ixy) = dtloc * (dss-dnn) * esin(ith)*ecos(ith)     &
               / clats(isea)
        end if
      end do
      !
      !
    end if
    !
    ! 3.  loop over sub-steps -------------------------------------------- *
    !
    do itloc=1, ntloc
      !
      ! 3.a propagate fields
      !
      !
      do isea=1, nsea
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        ixy    = mapsf(isea,3)
        vq(ixy)= vq(ixy) * gsqrt(iy,ix)
      end do
      !
      !
      if ( yfirst ) then
        !
        !
        !
      else
        !
        !
        !
      end if
      !
      !
      do isea=1, nsea
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        ixy    = mapsf(isea,3)
        vq(ixy)= vq(ixy) / gsqrt(iy,ix)
      end do
      !
      !
      ! 3.b update boundaries
      !
      if ( flbpi ) then
        rd1    = dsec21 ( tbpi0, time ) - dtg *                   &
             real(ntloc-itloc)/real(ntloc)
        rd2    = dsec21 ( tbpi0, tbpin )
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        do ibi=1, nbi
          isea    = isbpi(ibi)
          ixy     = mapsf(isbpi(ibi),3)
          vq(ixy) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
               / cg(ik,isea) * clats(isea)
        end do
      end if
      !
      ! 3.c diffusion correction
      !
      if ( dtme .ne. 0. ) then
        if ( global ) then
          do iy=1, ny
            vq(iy+nx*ny) = vq(iy)
          end do
        end if
        !curv2: begin -----------------------------------------------------------------
        vfdifx_fac=0.0
        do ip=1, nmx0
          ixy         = mapx2(ip)
          vfdifx_fac(ixy) = 1.0
        end do
        vfdify_fac=0.0
        do ip=1, nmy0
          ixy         = mapy2(ip)
          vfdify_fac(ixy) = 1.0
        end do
        vfdifc_fac=0.0
        do ip=1, nmxy
          ixy         = mapxy(ip)
          vfdifc_fac(ixy) = 1.0
        end do
        if ( global ) then
          iy0     = (nx-1)*ny
          do iy=1, ny
            vfdifx_fac(iy-ny) = vfdifx_fac(iy+iy0)
            vfdifc_fac(iy-ny) = vfdifc_fac(iy+iy0)
          end do
        end if
        !curv2: end -------------------------------------------------------------------
        vq_old = vq
        !
        !
        do ip=1, nact
          ixy    = mapaxy(ip)
          isea   = mapfs(ixy)
          ix     = mapsf(isea,1)
          iy     = mapsf(isea,2)
          !curv1: does not give exact match to earlier ww3 version for test case tp2.3
          !curv1: near the boundary, note that this version uses non-active grid points
          !curv1: in its calcs. this is no problem, as long as the non-active grid points
          !curv1: exist in the array and are vq=0. also note that with the short version
          !curv1: of the code, vfdif?_fac variables can be removed and 3-4 do loops above
          !curv1: can be removed.
          !curv1: begin -----------------------------------------------------------------
          !                qxx = vq_old(ixy+ny) - 2.0*vq_old(ixy) + vq_old(ixy-ny)
          !                qyy = vq_old(ixy+1)  - 2.0*vq_old(ixy) + vq_old(ixy-1)
          !                qxy = vq_old(ixy+ny+1) - vq_old(ixy-ny+1) &
          !                    - vq_old(ixy+ny-1) + vq_old(ixy-ny-1)
          !curv1: end -------------------------------------------------------------------
          !curv2: does give exact match to earlier ww3 version for test case tp2.3. note
          !curv2: that if vfdifc_fac variables are all unity, many terms cancel out.
          !curv2: however, vfdifc_fac is zero when a related vq point is not an active
          !curv2: grid point
          !curv2: begin -----------------------------------------------------------------
          qxx = vfdifx_fac(ixy)   *vq_old(ixy+ny)     &
               - vfdifx_fac(ixy)   *vq_old(ixy)        &
               - vfdifx_fac(ixy-ny)*vq_old(ixy)        &
               + vfdifx_fac(ixy-ny)*vq_old(ixy-ny)
          qyy = vfdify_fac(ixy)   *vq_old(ixy+1)      &
               - vfdify_fac(ixy)   *vq_old(ixy)        &
               - vfdify_fac(ixy-1) *vq_old(ixy)        &
               + vfdify_fac(ixy-1) *vq_old(ixy-1)
          qxy = vfdifc_fac(ixy)     *vq_old(ixy)      &
               + vfdifc_fac(ixy-ny-1)*vq_old(ixy)      &
               - vfdifc_fac(ixy-1)   *vq_old(ixy)      &
               - vfdifc_fac(ixy-ny)  *vq_old(ixy)      &
               + vfdifc_fac(ixy-ny)  *vq_old(ixy+1)    &
               - vfdifc_fac(ixy)     *vq_old(ixy+1)    &
               + vfdifc_fac(ixy-1)   *vq_old(ixy-1)    &
               - vfdifc_fac(ixy-ny-1)*vq_old(ixy-1)    &
               + vfdifc_fac(ixy-1)   *vq_old(ixy+ny)   &
               - vfdifc_fac(ixy)     *vq_old(ixy+ny)   &
               + vfdifc_fac(ixy-ny)  *vq_old(ixy-ny)   &
               - vfdifc_fac(ixy-ny-1)*vq_old(ixy-ny)   &
               + vfdifc_fac(ixy)     *vq_old(ixy+ny+1) &
               - vfdifc_fac(ixy-1)   *vq_old(ixy+ny-1) &
               + vfdifc_fac(ixy-ny-1)*vq_old(ixy-ny-1) &
               - vfdifc_fac(ixy-ny)  *vq_old(ixy-ny+1)
          !curv2: end -------------------------------------------------------------------
          !
          qxy = 0.25*qxy
          !
          dvq = vdxx(ixy)*(  dpdx(iy,ix)*dpdx(iy,ix)*qxx          &
               + 2.0*dpdx(iy,ix)*dqdx(iy,ix)*qxy      &
               + dqdx(iy,ix)*dqdx(iy,ix)*qyy )        &
               + vdyy(ixy)*(  dpdy(iy,ix)*dpdy(iy,ix)*qxx          &
               + 2.0*dpdy(iy,ix)*dqdy(iy,ix)*qxy      &
               + dqdy(iy,ix)*dqdy(iy,ix)*qyy)         &
               + 2.0*vdxy(ixy)*(  dpdx(iy,ix)*dpdy(iy,ix)*qxx      &
               + dqdx(iy,ix)*dqdy(iy,ix)*qyy      &
               +      (  dpdx(iy,ix)*dqdy(iy,ix)  &
               + dqdx(iy,ix)*dpdy(iy,ix) )*qxy )
          !
          vq(ixy) = vq_old(ixy) + dvq * dfac
          !
        end do
        !
        !
      end if
      !
      yfirst = .not. yfirst
    end do
    !
    ! 4.  store results in vq in proper format --------------------------- *
    !
    !
    !
    do isea=1, nsea
      ixy    = mapsf(isea,3)
      if ( mapsta(ixy) .gt. 0 ) then
        vq(ixy) =  max ( 0. , cg(ik,isea) / clats(isea) * vq(ixy) )
        !         else
        !           vq(ixy) =  0.
      end if
    end do
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3xyp2 ----------------------------------------------------- /
    !/
  end subroutine w3xyp2
  !/
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief propagation in spectral space.
  !>
  !> @details third order quickest scheme with ultimate limiter.
  !>
  !>
  !>  as with the spatial propagation, the two spaces are considered
  !>  independently, but the propagation is performed in a 2-d space.
  !>  compared to the propagation in physical space, the directions
  !>  represent a closed space and are therefore comparable to the
  !>  longitudinal or 'x' propagation. the wavenumber space has to be
  !>  extended to allow for boundary treatment. using a simple first
  !>  order boundary treatment at both sided, two points need to
  !>  be added. this implies that the spectrum needs to be extended,
  !>  shifted and rotated, as is performed using mapth2 as set
  !>  in w3map3.
  !>
  !> @param[in]    isea   number of sea point.
  !> @param[in]    facth  factor in propagation velocity.
  !> @param[in]    fack   factor in propagation velocity.
  !> @param[in]    cthg0  factor in great circle refracftion term.
  !> @param[in]    cg     local group velocities.
  !> @param[in]    wn     local wavenumbers.
  !> @param[in]    depth  depth.
  !> @param[in]    dddx   depth gradient.
  !> @param[in]    dddy   depth gradient.
  !> @param[in]    cx     current component.
  !> @param[in]    cy     current component.
  !> @param[in]    dcxdx  current gradients.
  !> @param[in]    dcxdy  current gradients.
  !> @param[in]    dcydx  current gradients.
  !> @param[in]    dcydy  current gradients.
  !> @param[in]    dcdx   phase speed gradient.
  !> @param[in]    dcdy   phase speed gradient.
  !> @param[inout] va     spectrum.
  !>
  !> @author h. l. tolman
  !> @date   01-jul-2013
  !>
  subroutine w3ktp2 ( isea, facth, fack, cthg0, cg, wn, depth,    &
       dddx, dddy, cx, cy, dcxdx, dcxdy,           &
       dcydx, dcydy, dcdx, dcdy, va )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-jul-2013 |
    !/                  +-----------------------------------+
    !/
    !/    14-feb-2000 : origination.                        ( version 2.08 )
    !/    17-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    01-jul-2013 : adding uq and uno switches to chose between third
    !/                  and second order schemes.           ( version 4.12 )
    !/
    !  1. purpose :
    !
    !     propagation in spectral space.
    !
    !  2. method :
    !
    !     third order quickest scheme with ultimate limiter.
    !
    !     as with the spatial propagation, the two spaces are considered
    !     independently, but the propagation is performed in a 2-d space.
    !     compared to the propagation in physical space, the directions
    !     rerpesent a closed space and are therefore comparable to the
    !     longitudinal or 'x' propagation. the wavenumber space has to be
    !     extended to allow for boundary treatment. using a simple first
    !     order boundary treatment at both sided, two points need to
    !     be added. this implies that the spectrum needs to be extended,
    !     shifted and rotated, as is performed using mapth2 as set
    !     in w3map3.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isea    int.   i   number of sea point.
    !       facth/k real   i   factor in propagation velocity.
    !       cthg0   real   i   factor in great circle refracftion term.
    !       mapxx2  i.a.   i   propagation and storage maps.
    !       cg      r.a.   i   local group velocities.
    !       wn      r.a.   i   local wavenumbers.
    !       depth   r.a.   i   depth.
    !       dddx    real   i   depth gradients.
    !       cx/y    real   i   current components.
    !       dcxdx   real   i   current gradients.
    !       dcdx-y  real   i   phase speed gradients.
    !       va      r.a.  i/o  spectrum.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       dsdd    r.a.  partial derivative of sigma for depth.
    !       fdd, fdu, fdg, fcd, fcu
    !               r.a.  directionally varying part of depth, current and
    !                     great-circle refraction terms and of consit.
    !                     of ck term.
    !       cflt-k  r.a.  propagation velocities of local fluxes.
    !       db      r.a.  wavenumber band widths at cell centers.
    !       dm      r.a.  wavenumber band widths between cell centers and
    !                     next cell center.
    !       q       r.a.  extracted spectrum
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       w3qck1   actual propagation routine.
    !       w3qck2   actual propagation routine.
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3wave   wave model routine.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  8. structure :
    !
    !     -----------------------------------------------------------------
    !       1.  preparations
    !         a initialize arrays
    !         b set constants and counters
    !       2.  point  preparations
    !         a calculate dsdd
    !         b extract spectrum
    !       3.  refraction velocities
    !         a filter level depth reffraction.
    !         b depth refratcion velocity.
    !         c current refraction velocity.
    !       4.  wavenumber shift velocities
    !         a prepare directional arrays
    !         b calcuate velocity.
    !       5.  propagate.
    !       6.  store results.
    !     -----------------------------------------------------------------
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !       !/t     enable general test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nk2, nth, nspec, sig, dsip, ecos, esin, &
         ec2, esc, es2, fachfa, mapwn, flcth, flck,  &
         ctmax
    use w3adatmd, only: mapth2, mapwn2, itime
    use w3idatmd, only: flcur
    use w3odatmd, only: ndse, ndst
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isea
    real, intent(in)        :: facth, fack, cthg0, cg(0:nk+1),      &
         wn(0:nk+1), depth, dddx, dddy,       &
         cx, cy, dcxdx, dcxdy, dcydx, dcydy
    real, intent(in)        :: dcdx(0:nk+1), dcdy(0:nk+1)
    real, intent(inout)     :: va(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, isp
    real                    :: fddmax, fdg, fkd, fkd0, dcyx,        &
         dcxxyy, dcxy, dcxx, dcxyyx, dcyy
    real                    :: dsdd(0:nk+1), frk(nk), frg(nk),      &
         fkc(nth), vq(-nk-1:nk2*(nth+2)),     &
         db(nk2,nth+1), dm(nk2,0:nth+1),      &
         vcflt(nk2*(nth+1)), cflk(nk2,nth)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a initialize arrays
    !
    if ( flck ) vq = 0.
    !
    !
    ! 2.  preparation for point ------------------------------------------ *
    ! 2.a array with partial derivative of sigma versus depth
    !
    do ik=0, nk+1
      if ( depth*wn(ik) .lt. 5. ) then
        dsdd(ik) = max ( 0. ,                                     &
             cg(ik)*wn(ik)-0.5*sig(ik) ) / depth
      else
        dsdd(ik) = 0.
      end if
    end do
    !
    !
    ! 2.b extract spectrum
    !
    do isp=1, nspec
      vq(mapth2(isp)) = va(isp)
    end do
    !
    ! 3.  refraction velocities ------------------------------------------ *
    !
    if ( flcth ) then
      !
      ! 3.a set slope filter for depth refraction
      !
      fddmax = 0.
      fdg    = facth * cthg0
      !
      do ith=1, nth/2
        fddmax = max(fddmax,abs(esin(ith)*dddx-ecos(ith)*dddy))
      end do
      !
      do ik=1, nk
        frk(ik) = facth * dsdd(ik) / wn(ik)
        frk(ik) = frk(ik) / max ( 1. , frk(ik)*fddmax/ctmax )
        frg(ik) = fdg * cg(ik)
      end do
      !
      ! 3.b depth refraction and great-circle propagation
      !
      do isp=1, nspec
        vcflt(mapth2(isp)) = frg(mapwn(isp)) * ecos(isp)          &
             + frk(mapwn(isp)) * ( esin(isp)*dddx - ecos(isp)*dddy )
      end do
      !
      !
      ! 3.d current refraction
      !
      if ( flcur ) then
        !
        dcyx   = facth *   dcydx
        dcxxyy = facth * ( dcxdx - dcydy )
        dcxy   = facth *   dcxdy
        !
        do isp=1, nspec
          vcflt(mapth2(isp)) = vcflt(mapth2(isp)) +             &
               es2(isp)*dcyx  + esc(isp)*dcxxyy - ec2(isp)*dcxy
        end do
        !
      end if
      !
    end if
    !
    ! 4.  wavenumber shift velocities ------------------------------------ *
    !     fack is just the time step, which is accounted for in w3qck2
    !
    if ( flck ) then
      !
      ! 4.a directionally dependent part
      !
      dcxx   =  -   dcxdx
      dcxyyx =  - ( dcxdy + dcydx )
      dcyy   =  -   dcydy
      fkd    =    ( cx*dddx + cy*dddy )
      !
      do ith=1, nth
        fkc(ith) = ec2(ith)*dcxx +                                &
             esc(ith)*dcxyyx + es2(ith)*dcyy
      end do
      !
      ! 4.b velocities
      !
      do ik=0, nk+1
        fkd0   = fkd / cg(ik) * dsdd(ik)
        do ith=1, nth
          cflk(ik+1,ith) = fkd0 + wn(ik)*fkc(ith)
        end do
      end do
      !
      ! 4.c band widths
      !
      do ik=0, nk
        db(ik+1,1) = dsip(ik) / cg(ik)
        dm(ik+1,1) = wn(ik+1) - wn(ik)
      end do
      db(nk+2,1) = dsip(nk+1) / cg(nk+1)
      dm(nk+2,1) = 0.
      !
      do ith=2, nth
        do ik=1, nk+2
          db(ik,ith) = db(ik,1)
          dm(ik,ith) = dm(ik,1)
        end do
      end do
      !
    end if
    !
    ! 5.  propagate ------------------------------------------------------ *
    !
    if ( mod(itime,2) .eq. 0 ) then
      if ( flck ) then
        do ith=1, nth
          vq(nk+2+(ith-1)*nk2) = fachfa * vq(nk+1+(ith-1)*nk2)
        end do
        !
        !
      end if
      if ( flcth ) then
        !
        !
        !
      end if
    else
      if ( flcth ) then
        !
        !
        !
      end if
      if ( flck )  then
        do ith=1, nth
          vq(nk+2+(ith-1)*nk2) = fachfa * vq(nk+1+(ith-1)*nk2)
        end do
        !
        !
        !
      end if
    end if
    !
    ! 6.  store reults --------------------------------------------------- *
    !
    do isp=1, nspec
      va(isp) = vq(mapth2(isp))
    end do
    !
    return
    !
    ! formats
    !
    !
    !/
    !/ end of w3ktp2 ----------------------------------------------------- /
    !/
  end subroutine w3ktp2
  !/
  !/ end of module w3pro2md -------------------------------------------- /
  !/
end module w3pro2md
