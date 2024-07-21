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
module w3pro3md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         27-may-2014 |
  !/                  +-----------------------------------+
  !/
  !/    27-feb-2000 : origination.                        ( version 2.08 )
  !/    17-sep-2000 : clean-up.                           ( version 2.13 )
  !/    10-dec-2001 : sub-grid obstructions.              ( version 2.14 )
  !/    16-oct-2002 : change intent for atrn in w3xyp3.   ( version 3.00 )
  !/    26-dec-2002 : moving grid version.                ( version 3.02 )
  !/    01-aug-2003 : moving grid gse correction.         ( version 3.03 )
  !/    17-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    07-sep-2005 : upgrade xy boundary conditions.     ( version 3.08 )
  !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
  !/    05-mar-2008 : added nec sxf90 compiler directives.
  !/                  (chris bunney, uk met office)       ( version 3.13 )
  !/    01-apr-2008 : bug fix w3map3 mapsta range check.  ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    17-aug-2010 : add test output w3xyp3.           ( version 3.14.5 )
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    26-dec-2012 : more initializations.               ( version 4.11 )
  !/    01-jul-2013 : adding uq and uno switches to chose between third
  !/                  and second order schemes.           ( version 4.12 )
  !/    12-sep-2013 : add documentation for global clos.  ( version 4.12 )
  !/    27-may-2014 : adding omph switch.                 ( version 5.02 )
  !/
  !/    copyright 2009-2014 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     bundles routines for third order propagation scheme in single
  !     module.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      trnmin    r.p.  private   minimum transparancy for local
  !                                switching off of averaging.
  !     ----------------------------------------------------------------
  !
  !     also work arrays for w3ktp3 (private).
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3map3    subr. public   set up auxiliary maps.
  !      w3mapt    subr. public   set up transparency map for gse.
  !      w3xyp3    subr. public   third order spatial propagation.
  !      w3ktp3    subr. public   third order spectral propagation.
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
  !     - the averaging is not performed around semi-transparent grid
  !       points to avoid that leaking through barriers occurs.
  !
  !  6. switches :
  !
  !       !/uq    3rd order uq propagation scheme.
  !       !/uno   2nd order uno propagation scheme.
  !
  !       !/mgp   moving grid corrections.
  !       !/mgg   moving grid corrections.
  !
  !       !/omph  hybrid openmp directives.
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
  subroutine w3map3
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-apr-2008 |
    !/                  +-----------------------------------+
    !/
    !/    27-feb-2000 : origination.                        ( version 2.08 )
    !/    10-dec-2001 : sub-grid obstructions.              ( version 2.14 )
    !/                  (array allocation only.)
    !/    17-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
    !/    01-apr-2008 : bug fix sec. 4 mapsta range check.  ( version 3.13 )
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
    !     ----------------------------------------------------------------
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
    !      3.   map mapaxy
    !      4.   map mapcxy
    !      5.   maps for intra-spectral propagation
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
    use w3gdatmd, only: nk, nth, nspec, nx, ny, nsea, mapsta, mapsf,&
         gtype
    use w3adatmd, only: nmx0, nmx1, nmx2, nmy0, nmy1, nmy2, nact,   &
         ncent, mapx2, mapy2, mapaxy, mapcxy,        &
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
         isea, ik, ith, isp, isp0, isp2, ncentc
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    if (gtype .lt. 3) then
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
      ! 3.  map mapaxy ----------------------------------------------------- *
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
      ! 4.  map mapcxy ----------------------------------------------------- *
      !
      ncent  = 0
      ncentc = nsea
      mapcxy = 0
      !
      do isea=1,  nsea
        ix      = mapsf(isea,1)
        ix0    = ix-1
        ix2    = ix+1
        iy      = mapsf(isea,2)
        iy0    = iy-1
        iy2    = iy+1
        if ( ix .eq. nx ) ix2 = 1
        if ( ix .eq. 1 ) ix0 = nx
        if ( mapsta(iy,ix).eq.2 .or. mapsta(iy,ix).lt.0 ) then
          mapcxy(ncentc) = isea
          ncentc = ncentc - 1
        else if ( mapsta(iy0,ix0).ge.1 .and.                     &
             mapsta(iy0,ix ).ge.1 .and.                     &
             mapsta(iy0,ix2).ge.1 .and.                     &
             mapsta(iy ,ix0).ge.1 .and.                     &
             mapsta(iy ,ix2).ge.1 .and.                     &
             mapsta(iy2,ix0).ge.1 .and.                     &
             mapsta(iy2,ix ).ge.1 .and.                     &
             mapsta(iy2,ix2).ge.1 ) then
          ncent  = ncent + 1
          mapcxy(ncent) = isea
        else
          mapcxy(ncentc) = isea
          ncentc = ncentc - 1
        end if
      end do
    end if
    !
    ! 5.  maps for intra-spectral propagation ---------------------------- *
    !
    if ( mapth2(1) .ne. 0 ) return
    !
    !
    ! 5.a mapth2 and mapbtk
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
    ! 5.b mapwn2
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
    !/ end of w3map3 ----------------------------------------------------- /
    !/
  end subroutine w3map3
  !/ ------------------------------------------------------------------- /
  subroutine w3mapt
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    10-dec-2001 : origination.                        ( version 2.14 )
    !/    17-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     generate 'map' arrays for the ultimate quickest scheme to combine
    !     gse alleviation with obstructions.
    !
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !
    ! 10. source code :
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, mapsf
    use w3adatmd, only: atrnx, atrny, maptrn
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: isea, ixy
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  map maptrn ----------------------------------------------------- *
    !
    do isea=1, nsea
      ixy         = mapsf(isea,3)
      !notes: oct 22 2012: i changed this because it *looks* like a bug.
      !       i have not confirmed that it is a bug.
      !       old code is given (2 lines). only the first line is
      !       changed.
      !old    maptrn(ixy) = min( atrnx(ixy,1) ,atrny(ixy,-1) ,              &
      !old                       atrny(ixy,1), atrny(ixy,-1) ) .lt. trnmin
      maptrn(ixy) = min( atrnx(ixy,1) ,atrnx(ixy,-1) ,              &
           atrny(ixy,1), atrny(ixy,-1) ) .lt. trnmin
    end do
    !
    return
    !
    ! formats
    !/
    !/ end of w3mapt ----------------------------------------------------- /
    !/
  end subroutine w3mapt
  !/ ------------------------------------------------------------------- /
  subroutine w3xyp3 ( isp, dtg, mapsta, mapfs, vq, vgx, vgy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-may-2014 |
    !/                  +-----------------------------------+
    !/
    !/    27-feb-2000 : origination.                        ( version 2.08 )
    !/    17-sep-2000 : clean-up.                           ( version 2.13 )
    !/    10-dec-2001 : sub-grid obstructions.              ( version 2.14 )
    !/    16-oct-2002 : change intent for atrnrx/y.         ( version 3.00 )
    !/    26-dec-2002 : moving grid version.                ( version 3.02 )
    !/    01-aug-2003 : moving grid gse correction.         ( version 3.03 )
    !/    17-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    07-sep-2005 : upgrade xy boundary conditions.     ( version 3.08 )
    !/    09-nov-2005 : removing soft boundary option.      ( version 3.08 )
    !/    05-mar-2008 : added nec sxf90 compiler directives.
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    17-aug-2010 : add test output.                  ( version 3.14.5 )
    !/    30-oct-2010 : implement unstructured grid         ( version 3.14 )
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    01-jul-2013 : adding uq and uno switches to chose between third
    !/                  and second order schemes.           ( version 4.12 )
    !/    12-sep-2013 : add documentation for global clos.  ( version 4.12 )
    !/    27-may-2014 : adding omph switch.                 ( version 5.02 )
    !/
    !  1. purpose :
    !
    !     propagation in phyiscal space for a given spectral component.
    !
    !  2. method :
    !
    !     third-order ultimate quickest scheme with averaging.
    !     curvilinear grid implementation: fluxes are computed in index space
    !       and then transformed back into physical space.  the diffusion term
    !       is handled in physical space.  the averaging scheme is adapted for
    !       curvilinear grids by applying the appropriate local rotations and
    !       adjustments to interpolation weights which control the strength of
    !       the averaging in axial directions.
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
    !       vgx/y   real   i   speed of grid.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       ntloc   int   number of local time steps.
    !       dtloc   real  local propagation time step.
    !       vcfl0x  r.a.  local courant numbers for absolute group vel.
    !                     using local x-grid step.
    !       vcfl0y  r.a.  id. in y.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       w3qck3   actual propagation algorithm
    !
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
    !  7. remarks :
    !
    !     - note that the ultimate limiter does not guarantee non-zero
    !       energies.
    !     - the present scheme shows a strong distorsion when propaga-
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
    !     - note that unlike in w3xyp2, isotropic diffusion is never
    !       used for growth.
    !     - curvilinear grid implementation. variables facx, facy, ccos, csin,
    !       ccurx, ccury are not needed and have been removed.  facx is accounted
    !       for as approriate in this subroutine.  facx is also accounted for in
    !       the case of .not.flcx.  since facx is removed, there is now a check for
    !       .not.flcx in this subroutine.  in cfl calcs dx and dy are omitted,
    !       since dx=dy=1 in index space.  curvilinear grid derivatives
    !       (dpdy, dqdx, etc.) and metric (gsqrt) are brought in via w3gdatmd.
    !     - the strength of the averaging scheme is dependent on grid resolution.
    !       since grid resolution is non-uniform for curvilinear grids, this means
    !       that the strength of the averaging is also non-uniform. this may not be
    !       a desirable effect. a potential future upgrade would be to add an
    !       additional term/factor that balances the effect of the spatial
    !       variation of grid resolution.
    !
    !  8. structure :
    !
    !     ---------------------------------------------
    !       1.  preparations
    !         a set constants
    !         b initialize arrays
    !       2.  prepare arrays
    !         a velocities and 'q'
    !       3.  loop over sub-steps
    !       ----------------------------------------
    !         a average
    !         b propagate
    !         c update boundaries.
    !       ----------------------------------------
    !       4.  store q field in spectra
    !     ---------------------------------------------
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !
    !       !/omph  hybrid openmp directives.
    !
    !       !/mgp   moving grid corrections.
    !       !/mgg   moving grid corrections.
    !
    !       !/t     enable general test output.
    !       !/t0    dump of precalcaulted interpolation data.
    !       !/t1    dump of input field and fluxes.
    !       !/t2    dump of output field (before boundary update).
    !       !/t3    dump of output field (final).
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nx, ny, nsea, mapsf, dtcfl, clats,      &
         iclose, flcx, flcy, nk, nth, dth, xfr,  &
         iclose_none, iclose_smpl, iclose_trpl,  &
         ecos, esin, sig, wdcg, wdth, pfmove,    &
         flagll, dpdx, dpdy, dqdx, dqdy, gsqrt
    use w3wdatmd, only: time
    use w3adatmd, only: nmx0, nmx1, nmx2, nmy0, nmy1, nmy2, nact,   &
         ncent, mapx2, mapy2, mapaxy, mapcxy,        &
         maptrn, cg, cx, cy, atrnx, atrny, itime
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
    integer                 :: ith, ik, ntloc, itloc, isea, ixy, ip
    integer                 :: ix, iy, ixc, iyc, ibi
    integer                 :: iixy1(nsea), iixy2(nsea),            &
         iixy3(nsea), iixy4(nsea)
    integer                 :: ttest(2),dttst
    real                    :: cg0, cga, cgn, cgx, cgy, cxc, cyc,   &
         cxmin, cxmax, cymin, cymax
    real                    :: cgc, fgse = 1.
    real                    :: fth, fthx, fthy, fcg, fcgx, fcgy
    real                    :: dtloc, dtrad,                        &
         dxcgn, dycgn, dxcgs, dycgs, dxcgc,   &
         dycgc
    real                    :: rdi1(nsea), rdi2(nsea),              &
         rdi3(nsea), rdi4(nsea)
    real                    :: tmpx, tmpy, rd1, rd2, rd3, rd4
    logical                 :: yfirst
    logical                 :: global
    real                    :: cp, cq
    !/
    !/ automatic work arrays
    !/
    integer                 :: mapstx(1-2*ny:ny*(nx+2))
    real                    :: vlcflx((nx+1)*ny), vlcfly((nx+1)*ny),&
         aq(1-ny:ny*(nx+2))
    real                    :: cxtot((nx+1)*ny), cytot(nx*ny)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    if ( iclose .eq. iclose_trpl ) then
      !/ ------------------------------------------------------------------- /
      if (iaproc .eq. naperr) &
           write(ndse,*)'subroutine w3xyp3 is not yet adapted for '//    &
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
    cgc    = sqrt ( cgx**2 + cgy**2 )
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
    cgn    = max ( abs(cgx) , abs(cgy) , cxc, cyc, 0.001*cg0 )
    !
    ntloc  = 1 + int(dtg/(dtcfl*cg0/cgn))
    dtloc  = dtg / real(ntloc)
    dtrad  = dtloc
    if ( flagll ) dtrad=dtrad/(dera*radius)
    !
    ttest(1) = time(1)
    ttest(2) = 0
    dttst = dsec21(ttest,time)
    yfirst = mod(nint(dttst/dtg),2) .eq. 0
    !
    !
    ! 1.b initialize arrays
    !
    !
    vlcflx = 0.
    vlcfly = 0.
    cxtot  = 0.
    cytot  = 0.
    !
    mapstx(1:nx*ny) = mapsta(1:nx*ny)
    !
    if ( global ) then
      mapstx(1-2*ny:0)            = mapsta((nx-2)*ny+1:nx*ny)
      mapstx(nx*ny+1:nx*ny+2*ny)  = mapsta(1:2*ny)
    else
      mapstx(1-2*ny:0)            = 0
      mapstx(nx*ny+1:nx*ny+2*ny)  = 0
    end if
    !
    ! 1.c pre-calculate interpolation info
    !
    fth = fgse * wdth * dth * dtloc
    fcg = fgse * wdcg * 0.5 * (xfr-1./xfr) * dtloc
    if ( flagll ) then
      fth = fth / radius / dera
      fcg = fcg / radius / dera
    end if
    fcg = fcg / real(ntloc) !tjc: required to match original (is this correct?)
    fthx = - fth * esin(ith)
    fthy =   fth * ecos(ith)
    fcgx =   fcg * ecos(ith)
    fcgy =   fcg * esin(ith)
    !
    !
    !
    do isea=1, nsea
      !
      ix  = mapsf(isea,1)
      iy  = mapsf(isea,2)
      !
      ! 1.c.1 normal and parallel width ...
      !
      tmpx   = fthx / clats(isea)
      tmpy   = fthy
      dxcgn  = dpdx(iy,ix)*tmpx + dpdy(iy,ix)*tmpy
      dycgn  = dqdx(iy,ix)*tmpx + dqdy(iy,ix)*tmpy
      tmpx   = fcgx / clats(isea)
      tmpy   = fcgy
      dxcgs  = dpdx(iy,ix)*tmpx + dpdy(iy,ix)*tmpy
      dycgs  = dqdx(iy,ix)*tmpx + dqdy(iy,ix)*tmpy
      !
      ! 1.c.2 "sum" corner (and mirror image) ...
      !
      dxcgc  = dxcgn + dxcgs
      dycgc  = dycgn + dycgs
      !
      ixc    = ny
      if ( dxcgc .lt. 0. ) ixc = - ixc
      iyc    = 1
      if ( dycgc .lt. 0. ) iyc = - iyc
      !
      iixy1(isea) = ixc + iyc
      if ( abs(dxcgc) .gt. abs(dycgc) ) then
        iixy2(isea) = ixc
        rdi1 (isea) = abs(dycgc/dxcgc)
        rdi2 (isea) = abs(dxcgc)
      else
        iixy2(isea) = iyc
        if ( abs(dycgc) .gt. 1.e-5 ) then
          rdi1(isea) = abs(dxcgc/dycgc)
        else
          rdi1(isea) = 1.
        end if
        rdi2(isea) = abs(dycgc)
      end if
      !
      !
      ! 1.c.2 "difference" corner (and mirror image) ...
      !
      dxcgc  = dxcgn - dxcgs
      dycgc  = dycgn - dycgs
      !
      ixc    = ny
      if ( dxcgc .lt. 0. ) ixc = - ixc
      iyc    = 1
      if ( dycgc .lt. 0. ) iyc = - iyc
      !
      iixy3(isea) = ixc + iyc
      if ( abs(dxcgc) .gt. abs(dycgc) ) then
        iixy4(isea) = ixc
        rdi3 (isea) = abs(dycgc/dxcgc)
        rdi4 (isea) = abs(dxcgc)
      else
        iixy4(isea) = iyc
        if ( abs(dycgc) .gt. 1.e-5 ) then
          rdi3(isea) = abs(dxcgc/dycgc)
        else
          rdi3(isea) = 1.
        end if
        rdi4(isea) = abs(dycgc)
      end if
      !
      !
    end do
    !
    !
    ! 2.  calculate velocities and diffusion coefficients ---------------- *
    ! 2.a velocities
    !
    !     q     = ( a / cg * clats )
    !     lcflx = ( cos*cg / clats ) * dt / dx
    !     lcfly = (     sin*cg )     * dt / dy
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
      !
      !
      do isea=1, nsea
        ixy         = mapsf(isea,3)
        cxtot(ixy) = cxtot(ixy) + cx(isea)/clats(isea)
        cytot(ixy) = cytot(ixy) + cy(isea)
      end do
      !
      !
    end if
    !
    !
    do isea=1, nsea
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      ixy    = mapsf(isea,3)
      cp = cxtot(ixy)*dpdx(iy,ix) + cytot(ixy)*dpdy(iy,ix)
      cq = cxtot(ixy)*dqdx(iy,ix) + cytot(ixy)*dqdy(iy,ix)
      vlcflx(ixy) = cp*dtrad
      vlcfly(ixy) = cq*dtrad
    end do
    !
    !
    ! 3.  loop over sub-steps -------------------------------------------- *
    !
    do itloc=1, ntloc
      !
      ! 3.a average
      !
      aq     = vq
      vq     = 0.
      !
      ! 3.a.1 central points
      !
      do ip=1, ncent
        isea    = mapcxy(ip)
        ixy     = mapsf(isea,3)
        if ( maptrn(ixy) ) then
          vq(ixy) = aq(ixy)
        else
          rd1     = rdi1(isea)
          rd2     = min ( 1. , rdi2(isea) * cg(ik,isea) )
          rd3     = rdi3(isea)
          rd4     = min ( 1. , rdi4(isea) * cg(ik,isea) )
          vq(ixy          ) = vq(ixy          )                   &
               + aq(ixy) * (3.-rd2-rd4)/3.
          vq(ixy+iixy1(isea)) = vq(ixy+iixy1(isea))               &
               + aq(ixy) * rd2*rd1/6.
          vq(ixy+iixy2(isea)) = vq(ixy+iixy2(isea))               &
               + aq(ixy) * (1.-rd1)*rd2/6.
          vq(ixy+iixy3(isea)) = vq(ixy+iixy3(isea))               &
               + aq(ixy) * rd4*rd3/6.
          vq(ixy+iixy4(isea)) = vq(ixy+iixy4(isea))               &
               + aq(ixy) * (1.-rd3)*rd4/6.
          vq(ixy-iixy1(isea)) = vq(ixy-iixy1(isea))               &
               + aq(ixy) * rd2*rd1/6.
          vq(ixy-iixy2(isea)) = vq(ixy-iixy2(isea))               &
               + aq(ixy) * (1.-rd1)*rd2/6.
          vq(ixy-iixy3(isea)) = vq(ixy-iixy3(isea))               &
               + aq(ixy) * rd4*rd3/6.
          vq(ixy-iixy4(isea)) = vq(ixy-iixy4(isea))               &
               + aq(ixy) * (1.-rd3)*rd4/6.
        end if
      end do
      !
      ! 3.a.2 near-coast points
      !
      do ip=ncent+1, nsea
        isea    = mapcxy(ip)
        ix      = mapsf(isea,1)
        ixy     = mapsf(isea,3)
        if ( mapsta(ixy) .le. 0 ) cycle
        if ( maptrn(ixy) ) then
          vq(ixy) = aq(ixy)
        else
          rd1     = rdi1(isea)
          rd3     = rdi3(isea)
          rd2     = min ( 1. , rdi2(isea) * cg(ik,isea) )
          rd4     = min ( 1. , rdi4(isea) * cg(ik,isea) )
          vq(ixy          ) = vq(ixy          )                   &
               + aq(ixy) * (3.-rd2-rd4)/3.
          !
          ixc    = sign(ny,iixy1(isea))
          iyc    = iixy1(isea) - ixc
          if ( mapstx(ixy+iixy1(isea)) .ge. 1 .and.               &
               .not. ( mapstx(ixy+ixc).le.0 .and.                 &
               mapstx(ixy+iyc).le.0 ) ) then
            vq(ixy+iixy1(isea)) = vq(ixy+iixy1(isea))      &
                 + aq(ixy) * rd2*rd1/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * rd2*rd1/6.
          end if
          if ( mapstx(ixy-iixy1(isea)) .ge. 1 .and.               &
               .not. ( mapstx(ixy-ixc).le.0 .and.                 &
               mapstx(ixy-iyc).le.0 ) ) then
            vq(ixy-iixy1(isea)) = vq(ixy-iixy1(isea))      &
                 + aq(ixy) * rd2*rd1/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * rd2*rd1/6.
          end if
          if ( mapstx(ixy+iixy2(isea)) .ge. 1 ) then
            vq(ixy+iixy2(isea)) = vq(ixy+iixy2(isea))      &
                 + aq(ixy) * (1.-rd1)*rd2/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * (1.-rd1)*rd2/6.
          end if
          if ( mapstx(ixy-iixy2(isea)) .ge. 1 ) then
            vq(ixy-iixy2(isea)) = vq(ixy-iixy2(isea))      &
                 + aq(ixy) * (1.-rd1)*rd2/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * (1.-rd1)*rd2/6.
          end if
          !
          ixc    = sign(ny,iixy3(isea))
          iyc    = iixy3(isea) - ixc
          if ( mapstx(ixy+iixy3(isea)) .ge. 1 .and.               &
               .not. ( mapstx(ixy+ixc).le.0 .and.                 &
               mapstx(ixy+iyc).le.0 ) ) then
            vq(ixy+iixy3(isea)) = vq(ixy+iixy3(isea))      &
                 + aq(ixy) * rd4*rd3/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * rd4*rd3/6.
          end if
          if ( mapstx(ixy-iixy3(isea)) .ge. 1 .and.               &
               .not. ( mapstx(ixy-ixc).le.0 .and.                 &
               mapstx(ixy-iyc).le.0 ) ) then
            vq(ixy-iixy3(isea)) = vq(ixy-iixy3(isea))      &
                 + aq(ixy) * rd4*rd3/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * rd4*rd3/6.
          end if
          !
          if ( mapstx(ixy+iixy4(isea)) .ge. 1 ) then
            vq(ixy+iixy4(isea)) = vq(ixy+iixy4(isea))      &
                 + aq(ixy) * (1.-rd3)*rd4/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * (1.-rd3)*rd4/6.
          end if
          if ( mapstx(ixy-iixy4(isea)) .ge. 1 ) then
            vq(ixy-iixy4(isea)) = vq(ixy-iixy4(isea))      &
                 + aq(ixy) * (1.-rd3)*rd4/6.
          else
            vq(ixy          ) = vq(ixy          )          &
                 + aq(ixy) * (1.-rd3)*rd4/6.
          end if
          !
        end if
        !
      end do
      !
      ! 3.a.3 restore boundary data
      !
      do ixy=1, nx*ny
        if ( mapsta(ixy).eq.2 ) vq(ixy) = aq(ixy)
      end do
      !
      ! 3.a.4 global closure (averaging only, propagation is closed in w3qck3).
      !
      if ( global ) then
        do iy=1, ny
          vq(iy          ) = vq(iy          ) + vq(nx*ny+iy)
          vq((nx-1)*ny+iy) = vq((nx-1)*ny+iy) + vq(iy-ny)
        end do
      end if
      !
      ! 3.b propagate fields
      !
      !     transform vq to straightened space
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
      !     transform vq back to normal space
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
      ! 3.c update boundaries
      !
      !
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
          ixy     = mapsf(isbpi(ibi),3)
          vq(ixy) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
               / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
        end do
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
        vq(ixy) =  max ( 0. , cg(ik,isea)/clats(isea)*vq(ixy) )
      end if
    end do
    !
    !
    return
    !
    ! formats
    !
    !
    !
    !
    !/
    !/ end of w3xyp3 ----------------------------------------------------- /
    !/
  end subroutine w3xyp3
  !/ ------------------------------------------------------------------- /
  subroutine w3ktp3 ( isea, facth, fack, cthg0, cg, wn, dw,       &
       dddx, dddy, cx, cy, dcxdx, dcxdy,           &
       dcydx, dcydy, dcdx, dcdy, va, cflthmax, cflkmax )
    !/
    !/    *** this routine should be identical to w3ktp2 ***
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
    !/    06-mar-2011 : output of maximum cfl  (f. ardhuin) ( version 3.14 )
    !/    24-aug-2011 : limiter on k advection (f. ardhuin) ( version 4.04 )
    !/    25-aug-2011 : depth  = max ( dmin, dw(isea) )     ( version 4.04 )
    !/    26-dec-2012 : more initializations.               ( version 4.11 )
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
    !       dw      r.a.   i   depth.
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
         ctmax, dmin
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
         wn(0:nk+1), dw, dddx, dddy,       &
         cx, cy, dcxdx, dcxdy, dcydx, dcydy
    real, intent(in)        :: dcdx(0:nk+1), dcdy(0:nk+1)
    real, intent(inout)     :: va(nspec)
    real, intent(out)       :: cflthmax, cflkmax
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, isp
    real                    :: fddmax, fdg, fkd, fkd0, dcyx,        &
         dcxxyy, dcxy, dcxx, dcxyyx, dcyy,    &
         velnofilt, velfac, depth
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
    depth  = max ( dmin, dw )
    vq       = 0.
    if ( flcth ) vcflt    = 0.
    if ( flck  ) cflk     = 0.
    cflthmax = 0.
    cflkmax  = 0.
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
      ! n.b.:  facth = dtg / dth / real(ntloc)  (value set in w3wavemd)
      !        namely, facth*vc=1 corresponds to cfl=1
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
        !
        ! removes the filtering that was done at that stage (f. ardhuin 2011/03/06)
        !
        !            frk(ik) = frk(ik) / max ( 1. , frk(ik)*fddmax/ctmax )
        frg(ik) = fdg * cg(ik)
      end do
      !
      ! 3.b current refraction
      !
      if ( flcur ) then
        !
        dcyx   = facth *   dcydx
        dcxxyy = facth * ( dcxdx - dcydy )
        dcxy   = facth *   dcxdy
        !
        do isp=1, nspec
          vcflt(mapth2(isp)) = es2(isp)*dcyx  +     &
               esc(isp)*dcxxyy - ec2(isp)*dcxy
        end do
        !
      else
        vcflt(:)=0.
      end if
      !
      ! 3.c depth refraction and great-circle propagation
      !
      !
      do isp=1, nspec
        velnofilt = vcflt(mapth2(isp))       &
             + frg(mapwn(isp)) * ecos(isp)              &
             + frk(mapwn(isp)) * ( esin(isp)*dddx - ecos(isp)*dddy )
        !
        cflthmax = max(cflthmax, abs(velnofilt))
        !
        ! puts filtering on total velocity (including currents and great circle effects)
        ! the filtering limits vcflt to be less than ctmax
        ! this modification was proposed by f. ardhuin 2011/03/06
        !
        vcflt(mapth2(isp))=sign(min(abs(velnofilt),ctmax),velnofilt)
      end do
    end if
    !
    ! 4.  wavenumber shift velocities ------------------------------------ *
    ! n.b.:  fack = dtg / real(ntloc)  (value set in w3wavemd)
    !        namely, fack*vc/dk=1 corresponds to cfl=1
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
      ! 4.b band widths
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
      ! 4.c velocities
      !
      do ik=0, nk+1
        fkd0   = fkd / cg(ik) * dsdd(ik)
        velfac =  fack/db(ik+1,1)
        do ith=1, nth
          !
          ! puts filtering on velocity (needs the band widths)
          !
          velnofilt = ( fkd0 + wn(ik)*fkc(ith) ) * velfac   ! this is velocity * dt / dk
          cflkmax = max(cflkmax, abs(velnofilt))
          cflk(ik+1,ith) = sign(min(abs(velnofilt),ctmax),velnofilt)/velfac
          !cflk(ik+1,ith) = fkd0 + wn(ik)*fkc(ith)          ! this was without the limiter ...
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
      if ( flck ) then
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
    !/ end of w3ktp3 ----------------------------------------------------- /
    !/
  end subroutine w3ktp3
  !/ ------------------------------------------------------------------- /
  subroutine w3cflxy ( isea, dtg, mapsta, mapfs, cflxymax, vgx, vgy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           f. ardhuin            |
    !/                  |                        fortran 90 |
    !/                  | last update :         31-oct-2010 |
    !/                  +-----------------------------------+
    !/
    !/    07-mar-2011 : origination.                        ( version 3.14 )
    !/
    !  1. purpose :
    !
    !     computes the maximum cfl number for spatial advection. used for diagnostic
    !     purposes. (could be used to define a local time step ...)
    !
    !  2. method :
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isea    int.   i   index of grid point.
    !       dtg     real   i   total time step.
    !       mapsta  i.a.   i   grid point status map.
    !       mapfs   i.a.   i   storage map.
    !       cflxymax real  o   maximum cfl number for xy propagation.
    !       vgx/y   real   i   speed of grid.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       ntloc   int   number of local time steps.
    !       dtloc   real  local propagation time step.
    !       vcfl0x  r.a.  local courant numbers for absolute group vel.
    !                     using local x-grid step.
    !       vcfl0y  r.a.  id. in y.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
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
    !  7. remarks :
    !
    !     - curvilinear grid implementation. variables facx, facy, ccos, csin,
    !       ccurx, ccury are not needed and have been removed.  facx is accounted
    !       for as approriate in this subroutine.  facx is also accounted for in
    !       the case of .not.flcx.  since facx is removed, there is now a check for
    !       .not.flcx in this subroutine.  in cfl calcs dx and dy are omitted,
    !       since dx=dy=1 in index space.  curvilinear grid derivatives
    !       (dpdy, dqdx, etc.) and metric (gsqrt) are brought in via w3gdatmd.
    !
    !  8. structure :
    !
    !     ---------------------------------------------
    !     ---------------------------------------------
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !
    !       !/mgp   moving grid corrections.
    !       !/mgg   moving grid corrections.
    !
    !       !/t     enable general test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nx, ny, nsea, mapsf, dtcfl, clats,      &
         flcx, flcy, nk, nth, dth, xfr,          &
         ecos, esin, sig, wdcg, wdth, pfmove,    &
         flagll, dpdx, dpdy, dqdx, dqdy, gsqrt
    use w3wdatmd, only: time
    use w3adatmd, only: nmx0, nmx1, nmx2, nmy0, nmy1, nmy2, nact,   &
         ncent, mapx2, mapy2, mapaxy, mapcxy,        &
         maptrn, cg, cx, cy, atrnx, atrny, itime
    use w3idatmd, only: flcur
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin,       &
         isbpi, bbpi0, bbpin
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isea, mapsta(ny*nx), mapfs(ny*nx)
    real, intent(in)        :: dtg, vgx, vgy
    real, intent(inout)     :: cflxymax
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, ixy, ip
    integer                 :: ix, iy, ixc, iyc, ibi
    real                    :: cg0, cga, cgn, cgx, cgy, cxc, cyc,   &
         cxmin, cxmax, cymin, cymax
    real                    :: cgc, fgse = 1.
    real                    :: fth, fthx, fthy, fcg, fcgx, fcgy
    real                    :: cp, cq
    !/
    !/ automatic work arrays
    !/
    real                    :: vlcflx, vlcfly
    real                    :: cxtot, cytot
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a set constants
    !
    !
    cflxymax=0.
    ix  = mapsf(isea,1)
    iy  = mapsf(isea,2)
    ixy = mapsf(isea,3)
    do ik=1,nk
      do ith=1,nth
        cxtot = ecos(ith) * cg(ik,isea) / clats(isea)
        cytot = esin(ith) * cg(ik,isea)
        if ( flcur ) then
          cxtot = cxtot + cx(isea)/clats(isea)
          cytot = cytot + cy(isea)
        end if
        cp = cxtot*dpdx(iy,ix) + cytot*dpdy(iy,ix)
        cq = cxtot*dqdx(iy,ix) + cytot*dqdy(iy,ix)
        vlcflx = cp*dtg
        vlcfly = cq*dtg
        cflxymax = max(vlcflx,vlcfly,cflxymax)
      end do
    end do
    return
    !/
    !/ end of w3xycfl ----------------------------------------------------- /
    !/
  end subroutine w3cflxy
  !/
  !/ end of module w3pro3md -------------------------------------------- /
  !/
end module w3pro3md
