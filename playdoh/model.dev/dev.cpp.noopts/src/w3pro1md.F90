!> @file
!> @brief bundles routines for first order propagation scheme in single
!>        module.
!>
!> @author h. l. tolman
!> @date   05-jun-2018
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
!> @brief bundles routines for first order propagation scheme in single
!>        module.
!>
!> @author h. l. tolman
!> @date   05-jun-2018
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3pro1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         05-jun-2018 |
  !/                  +-----------------------------------+
  !/
  !/    04-feb-2000 : origination                         ( version 2.00 )
  !/    28-mar-2001 : partial time step bug fix (proper   ( version 2.10 )
  !/                  ingest of boundaries).
  !/    02-apr-2001 : sub-grid obstructions.              ( version 2.10 )
  !/    26-dec-2002 : moving grid version.                ( version 3.02 )
  !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    07-sep-2005 : improved xy boundary conditions.    ( version 3.08 )
  !/    10-jan-2007 : clean-up facvx/y compute.           ( version 3.10 )
  !/    05-mar-2008 : added nec sxf90 compiler directives
  !/                  (chris bunney, uk met office)       ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    29-may-2014 : adding omph switch.                 ( version 5.02 )
  !/    08-may-2014 : implement tripolar grid for first order propagation
  !/                  scheme                              ( version 5.03 )
  !/                  (w. e. rogers, nrl)
  !/    05-jun-2018 : add debug                           ( version 6.04 )
  !/
  !/    copyright 2009-2014 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     bundles routines for first order propagation scheme in single
  !     module.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3map1    subr. public   set up auxiliary maps.
  !      w3xyp1    subr. public   first order spatial propagation.
  !      w3ktp1    subr. public   first order spectral propagation.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      dsec21    func. w3timemd time difference.
  !      strace    subr. w3servmd subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/s     enable subroutine tracing.
  !       !/tn    enable test output.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief generate 'map' arrays for the first order upstream scheme.
  !>
  !> @param mapsta  status map
  !>
  !> @author h. l. tolman
  !> @date   06-dec-2010
  !>
  subroutine w3map1 ( mapsta )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-dec-2010 |
    !/                  +-----------------------------------+
    !/
    !/    19-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    14-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    10-jan-2007 : clean-up facvx/y compute.           ( version 3.10 )
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     generate 'map' arrays for the first order upstream scheme.
    !
    !  2. method :
    !
    !     see section 3.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       mapsta  i.a.   i   status map.
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
    !      1.   initialize arrays.
    !      2.   fill arrays.
    !      3.   invert arrays.
    !     ------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nth, nspec, nx, ny, iclose,                 &
         iclose_none, iclose_smpl, iclose_trpl
    use w3adatmd, only: is0, is2, facvx, facvy
    use w3odatmd, only: ndse, iaproc, naperr
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mapsta(ny*nx)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ix, iy, ixy, isp, ixnext
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  initialize x-y arrays ------------------------------------------ *
    !
    facvx = 0.
    facvy = 0.
    !
    ! 2.  fill x-y arrays ------------------------------------------------ *
    !
    !.....facvy
    do ix=1, nx
      do iy=1, ny-1
        ixy    = iy +(ix-1)*ny
        if ( mapsta( ixy ) .ne. 0 ) facvy(ixy) = facvy(ixy) + 1.
        !.........next point : j+1 : increment ixy by 1
        if ( mapsta(ixy+1) .ne. 0 ) facvy(ixy) = facvy(ixy) + 1.
      end do
    end do
    !
    !.....facvy for iy=ny
    if ( iclose.eq.iclose_trpl ) then
      iy=ny
      do ix=1, nx
        ixy    = iy +(ix-1)*ny
        if ( mapsta( ixy ) .ne. 0 ) facvy(ixy) = facvy(ixy) + 1.
        !...........next point: j+1: tripole: j==>j+1==>j and i==>ni-i+1
        ixnext=nx-ix+1
        ixy    = iy +(ixnext-1)*ny
        if ( mapsta( ixy ) .ne. 0 ) facvy(ixy) = facvy(ixy) + 1.
      end do
      !bgr: adding the following lines to compute facvx over all
      !      ix for iy=ny (this allows along-seam propagation).
      !      located here since already inside "trpl" if-block.
      !{
      do ix=1, nx-1
        ixy    = iy +(ix-1)*ny
        if ( mapsta( ixy ) .ne. 0 ) facvx(ixy) = facvx(ixy) + 1.
        if ( mapsta(ixy+ny) .ne. 0 ) facvx(ixy) = facvx(ixy) + 1.
      end do
      !}
    end if
    !
    !.....facvx
    do ix=1, nx-1
      do iy=2, ny-1
        ixy    = iy +(ix-1)*ny
        if ( mapsta( ixy  ) .ne. 0 ) facvx(ixy) = facvx(ixy) + 1.
        !.........next point : i+1 : increment ixy by ny
        if ( mapsta(ixy+ny) .ne. 0 ) facvx(ixy) = facvx(ixy) + 1.
      end do
    end do
    !
    !.....facvx for ix=nx
    if ( iclose.ne.iclose_none ) then
      do iy=2, ny-1
        ixy    = iy +(nx-1)*ny
        if ( mapsta(ixy) .ne. 0 ) facvx(ixy) = facvx(ixy) + 1.
        !...........next point : i+1 : increment ixy by ny
        !...........ixy+ny=iy+(ix-1)*ny+ny = iy+ix*ny = iy+nx*ny ==> wrap to iy
        if ( mapsta(iy ) .ne. 0 ) facvx(ixy) = facvx(ixy) + 1.
      end do
    end if
    !
    ! 3.  invert x-y arrays ---------------------------------------------- *
    !
    do ixy=1, nx*ny
      if ( facvx(ixy) .ne. 0. ) facvx(ixy) = 1. / facvx(ixy)
      if ( facvy(ixy) .ne. 0. ) facvy(ixy) = 1. / facvy(ixy)
    end do
    !
    ! 4.  fill theta arrays ---------------------------------------------- *
    !
    do isp=1, nspec
      is2  (isp) = isp + 1
      is0  (isp) = isp - 1
    end do
    !
    do isp=nth, nspec, nth
      is2(isp) = is2(isp) - nth
    end do
    !
    do isp=1, nspec, nth
      is0(isp) = is0(isp) + nth
    end do
    !
    return
    !/
    !/ end of w3map1 ----------------------------------------------------- /
    !/
  end subroutine w3map1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief propagation in physical space for a given spectral component.
  !>
  !> @param[in]      isp  number of spectral bin (ik-1)*nth+ith
  !> @param[in]      dtg  total time step.
  !> @param[in]   mapsta  grid point status map.
  !> @param[inout] field  wave action spectral densities on full grid.
  !> @param[in]      vgx  speed of grid.
  !> @param[in]      vgy  speed of grid.
  !>
  !> @author h. l. tolman
  !> @date   29-may-2014
  !>
  subroutine w3xyp1 ( isp, dtg, mapsta, field, vgx, vgy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-may-2014 |
    !/                  +-----------------------------------+
    !/
    !/    07-jul-1998 : final fortran 77                    ( version 1.18 )
    !/    14-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    28-mar-2001 : partial time step bug fix.          ( version 2.10 )
    !/    02-apr-2001 : sub-grid obstructions.              ( version 2.10 )
    !/    26-dec-2002 : moving grid version.                ( version 3.02 )
    !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    07-sep-2005 : improved xy boundary conditions.    ( version 3.08 )
    !/    05-mar-2008 : added nec sxf90 compiler directives
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    29-may-2014 : adding omph switch.                 ( version 5.02 )
    !/
    !  1. purpose :
    !
    !     propagation in physical space for a given spectral component.
    !
    !  2. method :
    !
    !     first order scheme with flux formulation.
    !     curvilinear grid implementation: fluxes are computed in index space
    !       and then transformed back into physical space.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isp     int.   i   number of spectral bin (ik-1)*nth+ith
    !       dtg     real   i   total time step.
    !       mapsta  i.a.   i   grid point status map.
    !       field   r.a.  i/o  wave action spectral densities on full
    !                          grid.
    !       vgx/y   real   i   speed of grid.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       ntloc   int.  number of local steps.
    !       dtloc   real  local propagation time step.
    !       vcx     r.a.  propagation velocities in index space.
    !       vcy     r.a.
    !       cxtot   r.a.  propagation velocities in physical space.
    !       cytot   r.a.
    !       vflx    r.a.  discrete fluxes between grid points in index space.
    !       vfly    r.a.
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
    !       none.
    !
    !  7. remarks :
    !
    !     - the local work arrays are initialized on the first entry to
    !       the routine.
    !     - curvilinear grid implementation. variables facx, facy, ccos, csin,
    !       ccurx, ccury are not needed and have been removed.  facx is accounted
    !       for as approriate in this subroutine.  facx is also accounted for in
    !       the case of .not.flcx.  since facx is removed, there is now a check for
    !       .not.flcx in this subroutine.  in cfl calcs dx and dy are omitted,
    !       since dx=dy=1 in index space.  curvilinear grid derivatives
    !       (dpdy, dqdx, etc.) and metric (gsqrt) are brought in via w3gdatmd.
    !     - standard vcb calculation for y is:
    !               vcb       = facvy(ixy) * ( vcy2d(iy,ix) + vcy2d(iy+1,ix) )
    !       this is to calculate the flux vcy(iy+0.5). for the tripole grid,
    !       we cannot do it this way, since the sign of vcy flips as we jump
    !       over the seam. if we were to do it this way, vcy(iy) and vcy(iy+1)
    !       are two numbers of similar magnitude and opposite sign, so the
    !       average of the two gives something close to zero, so energy does
    !       not leave via vcy(iy+0.5). one alternative is:
    !               vcb       = vcy2d(iy,ix)
    !       another alternative is :
    !               vcb       = facvy(ixy) * ( vcy2d(iy,ix) - vcy2d(iy+1,ix) )
    !       both appear to give correct results for ww3_tp2.13. we use the
    !       second alternative.
    !
    !  8. structure :
    !
    !     ---------------------------------------
    !       1.  preparations
    !         a set constants
    !         b initialize arrays
    !       2.  calculate local discrete fluxes
    !       3.  calculate propagation fluxes
    !       4.  propagate
    !       5.  update boundary conditions
    !     ---------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !
    !     !/omph  hybrid openmp directives.
    !
    !     !/t   enable general test output.
    !     !/t1  test output local fluxes (v)fx-yl.
    !     !/t2  test output propagation fluxes (v)flx-y.
    !     !/t3  test output propagation.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nk, nth, sig, ecos, esin, nx, ny, nsea,     &
         mapsf, dtcfl, iclose, clats, flcx, flcy,    &
         iclose_none, iclose_smpl, iclose_trpl,      &
         flagll, dpdx, dpdy, dqdx, dqdy, gsqrt
    use w3wdatmd, only: time
    use w3adatmd, only: cg, cx, cy, atrnx, atrny, facvx, facvy
    use w3idatmd, only: flcur
    use w3odatmd, only: ndst, flbpi, nbi, tbpi0, tbpin, isbpi,      &
         bbpi0, bbpin, ndse, iaproc, naperr
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isp, mapsta(ny*nx)
    real, intent(in)        :: dtg, vgx, vgy
    real, intent(inout)     :: field(1-ny:ny*(nx+2))
    !/
    !/ ------------------------------------------------------------------ /
    !/ local parameters
    !/
    integer                 :: ik, ith, ntloc, itloc, isea, ixy,    &
         iy0, ix, iy, jxn, jxp, jyn, jyp,     &
         ibi, nymax
    real                    :: cg0, cgl, cga, cc, cgn
    real                    :: dtloc,dtrad, vcb
    real                    :: rd1, rd2
    real                    :: cp, cq
    !/
    !/ automatic work arrays
    !/
    real                    :: cxtot2d(ny,nx)
    real                    :: cytot2d(ny,nx)
    real                    :: fld2d(ny+1,nx+1)
    real                    :: vcx2d(ny,nx+1)
    real                    :: vcy2d(ny+1,nx)
    real                    :: vflx2d(1:ny,0:nx)
    real                    :: vfly2d(ny,nx)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a set constants
    !
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    !
    cg0    = 0.575 * grav / sig(1)
    cgl    = 0.575 * grav / sig(ik)
    !
    if ( flcur ) then
      cga    = sqrt(maxval((cgl*ecos(ith)+cx(1:nsea))**2          &
           +(cgl*esin(ith)+cy(1:nsea))**2))
      cc     = sqrt(maxval(cx(1:nsea)**2+cy(1:nsea)**2))
    else
      cga    = cgl
      cc     = 0.
    end if
    !
    cgn    = 0.9999 * max ( cga, cc, 0.001*cg0 )
    !
    ntloc  = 1 + int(dtg/(dtcfl*cg0/cgn))
    dtloc  = dtg / real(ntloc)
    dtrad  = dtloc
    if ( flagll ) dtrad=dtrad/(dera*radius)
    !
    !
    ! ====================== loop partial ================================ *
    !
    do itloc=1, ntloc
      !
      ! 1.b initialize arrays
      !
      !
      vcx2d = 0.
      vcy2d = 0.
      cxtot2d  = 0.
      cytot2d  = 0.
      fld2d  = 0.
      vflx2d  = 0.
      vfly2d  = 0.
      !
      ! 2.  calculate field and velocities --------------------------------- *
      !
      !     field = a / cg * clats
      !     vcx   = cos*cg / clats
      !     vcy   = sin*cg
      !
      !
      !
      do isea=1, nsea
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        ixy    = mapsf(isea,3)
        fld2d(iy,ix) = field(ixy) / cg(ik,isea) * clats(isea)
        cxtot2d(iy,ix) = ecos(ith) * cg(ik,isea) / clats(isea)
        cytot2d(iy,ix) = esin(ith) * cg(ik,isea)
      end do
      !
      !
      if ( flcur ) then
        do isea=1, nsea
          ix     = mapsf(isea,1)
          iy     = mapsf(isea,2)
          cxtot2d(iy,ix) = cxtot2d(iy,ix) + cx(isea)/clats(isea)
          cytot2d(iy,ix) = cytot2d(iy,ix) + cy(isea)
        end do
      end if
      if ( flcx ) then
        do isea=1, nsea
          ix     = mapsf(isea,1)
          iy     = mapsf(isea,2)
          cp=cxtot2d(iy,ix)*dpdx(iy,ix)+cytot2d(iy,ix)*dpdy(iy,ix)
          vcx2d(iy,ix) = cp*dtrad
        end do
      else
        vcx2d=0.0
      endif
      if ( flcy ) then
        do isea=1, nsea
          ix     = mapsf(isea,1)
          iy     = mapsf(isea,2)
          cq=cxtot2d(iy,ix)*dqdx(iy,ix)+cytot2d(iy,ix)*dqdy(iy,ix)
          vcy2d(iy,ix) = cq*dtrad
        end do
      else
        vcy2d=0.0
      endif
      ! transform field to index space, i.e. straightened space
      ! bugfix: this is now done *before* adding the ghost row, so that ghost
      !   row will be in index space (bug applied only to global, irregular
      !   grids, so it did not apply to any test case that existed w/v4.18)
      fld2d(1:ny,1:nx)=fld2d(1:ny,1:nx)*gsqrt(1:ny,1:nx)
      !
      ! deal with longitude closure by duplicating one row *to the right*
      !   in field/fld2d, vcx
      if ( iclose.ne.iclose_none ) then
        do iy=1, ny
          fld2d(iy,nx+1)=fld2d(iy,1)
          vcx2d(iy,nx+1)=vcx2d(iy,1)
        end do
      end if
      ! deal with tripole closure by duplicating one row *at the top*
      !   in field/fld2d, vcy
      if ( iclose.eq.iclose_trpl ) then
        do ix=1,nx
          !...........next point: j+1: tripole: j==>j+1==>j and i==>ni-i+1
          fld2d(ny+1,ix)=fld2d(ny,nx-ix+1)
          vcy2d(ny+1,ix)=vcy2d(ny,nx-ix+1)
        end do
      end if
      !
      ! 3.  calculate propagation fluxes ----------------------------------- *
      !
      nymax=ny-1
      if ( iclose.eq.iclose_trpl ) nymax=ny
      !
      !
      do ix=1, nx
        do iy=1, nymax
          ixy    = iy +(ix-1)*ny
          vcb       = facvx(ixy) * ( vcx2d(iy,ix) + vcx2d(iy,ix+1) )
          vflx2d(iy,ix) = max ( vcb , 0. ) * fld2d(iy,ix)          &
               + min ( vcb , 0. ) * fld2d(iy,ix+1)
        end do
      end do
      !
      !
      ! deal with longitude closure by duplicating one row *to the left*
      !    in vflx. note that a similar action is not take for tripole grid,
      !    since tripole seam is only: iy=ny communicating with other points
      !    at iy=ny, not a case of iy=ny communicating with iy=1
      if ( iclose.ne.iclose_none ) then
        do iy=1, ny
          vflx2d(iy,0) = vflx2d(iy,nx)
        end do
      end if
      !
      !
      do ix=1, nx
        do iy=1, ny-1
          ixy    = iy +(ix-1)*ny
          vcb       = facvy(ixy) * ( vcy2d(iy,ix) + vcy2d(iy+1,ix) )
          vfly2d(iy,ix) = max ( vcb , 0. ) * fld2d(iy,ix)          &
               + min ( vcb , 0. ) * fld2d(iy+1,ix)
        end do
      end do
      !
      !
      ! for tripole grid, include iy=ny in calculation. vcb is handled
      !    differently. see notes in section "7. remarks" above.
      if ( iclose.eq.iclose_trpl ) then
        iy=ny
        !
        !
        do ix=1, nx
          ixy    = iy +(ix-1)*ny
          vcb       = facvy(ixy) * ( vcy2d(iy,ix) - vcy2d(iy+1,ix) )
          vfly2d(iy,ix) = max ( vcb , 0. ) * fld2d(iy,ix)          &
               + min ( vcb , 0. ) * fld2d(iy+1,ix)
        end do
        !
        !
      end if
      ! 4.  propagate ------------------------------------------------------ *
      !
      !
      !
      do isea=1, nsea
        !
        ix    = mapsf(isea,1)
        iy    = mapsf(isea,2)
        ixy   = mapsf(isea,3)
        !
        if (mapsta(ixy).eq.1) then
          !
          if ( vflx2d(iy,ix-1) .gt. 0. ) then
            jxn   = -1
          else
            jxn   =  0
          end if
          if ( vflx2d(iy,ix) .lt. 0. ) then
            jxp   =  1
          else
            jxp   =  0
          end if
          if ( vfly2d(iy-1,ix) .gt. 0. ) then
            jyn   = -1
          else
            jyn   =  0
          end if
          if ( vfly2d(iy,ix) .lt. 0. ) then
            jyp   =  1
          else
            jyp   =  0
          end if
          !
          fld2d(iy,ix) =   fld2d(iy,ix)                            &
               + atrnx(ixy,jxn) * vflx2d(iy,ix-1)   &
               - atrnx(ixy,jxp) * vflx2d(iy,ix)     &
               + atrny(ixy,jyn) * vfly2d(iy-1,ix)   &
               - atrny(ixy,jyp) * vfly2d(iy,ix)
          !
          !
          !
        end if !  if (mapsta(ixy).eq.1) then
        fld2d(iy,ix) = cg(ik,isea) / clats(isea) * fld2d(iy,ix)
      end do !  do isea=1, nsea
      !
      !
      ! transform field back to physical space, i.e. may be curvilinear
      fld2d(1:ny,1:nx)=fld2d(1:ny,1:nx)/gsqrt(1:ny,1:nx)
      !
      ! 5.  update boundary conditions ------------------------------------- *
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
          ix    = mapsf(isbpi(ibi),1)
          iy    = mapsf(isbpi(ibi),2)
          fld2d(iy,ix) = rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi)
        end do
      end if
      !
      ! 6.  put back in 1d shape ------------------------------------------- *
      !
      do isea=1, nsea
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        ixy    = mapsf(isea,3)
        field(ixy) = fld2d(iy,ix)
      end do
      !
      ! ... end of partial time step loop
      !
    end do !   do itloc=1, ntloc
    !
    return
    !
    ! formats
    !
    !
    !
    !
    !/
    !/ end of w3xyp1 ----------------------------------------------------- /
    !/
  end subroutine w3xyp1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief propagation in spectral space.
  !>
  !> @param[inout] isea    number of sea points.
  !> @param[inout] facth   factor in propagation velocity.
  !> @param[inout] fack    factor in propagation velocity.
  !> @param[inout] cthg0   factor in great circle refracftion term.
  !> @param[inout] cg      local group velocities.
  !> @param[inout] wn      local wavenumbers.
  !> @param[inout] depth   depth.
  !> @param[inout] dddx    depth gradients.
  !> @param[inout] dddy    depth gradients.
  !> @param[inout] cx      local group velocities.
  !> @param[inout] cy      local group velocities.
  !> @param[inout] dcxdx   current gradients.
  !> @param[inout] dcxdy   current gradients.
  !> @param[inout] dcydx   current gradients.
  !> @param[inout] dcydy   current gradients.
  !> @param[inout] dcdx    phase speed gradients.
  !> @param[inout] dcdy    phase speed gradients.
  !> @param[inout] va      spectrum.
  !>
  !> @author h. l. tolman
  !> @date   20-dec-2004
  !>
  subroutine w3ktp1 ( isea, facth, fack, cthg0, cg, wn, depth,    &
       dddx, dddy, cx, cy, dcxdx, dcxdy, dcydx,    &
       dcydy, dcdx, dcdy, va )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    29-aug-1997 : final fortran 77                    ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    20-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     propagation in spectral space.
    !
    !  2. method :
    !
    !     first order scheme.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isea    int.   i   number of sea point.
    !       facth/k real   i   factor in propagation velocity.
    !       cthg0   real   i   factor in great circle refracftion term.
    !       cg      r.a.   i   local group velocities.
    !       wn      r.a.   i   local wavenumbers.
    !       depth   real   i   depth.
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
    !       frk, frg, fkc
    !               r.a.  partial velocity terms.
    !       dwni    r.a.  inverse band width.
    !       cth-wn  r.a.  propagation velocities of local fluxes.
    !       flth-wn r.a.  propagation fluxes.
    !       aa      r.a.  extracted spectrum
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
    !       none.
    !
    !  8. structure :
    !
    !     -----------------------------------------------------------------
    !       1.  preparations
    !         a calculate dsdd
    !         b extract spectrum
    !       2.  refraction velocities
    !         a filter level depth reffraction.
    !         b depth refratcion velocity.
    !         c current refraction velocity.
    !       3.  wavenumber shift velocities
    !         a prepare directional arrays
    !         b calcuate velocity.
    !       4.  refraction
    !         a discrete fluxes.
    !         b propagation fluxes.
    !         c refraction.
    !       5.  wavenumber shifts.
    !         a discrete fluxes.
    !         b propagation fluxes.
    !         c refraction.
    !     -----------------------------------------------------------------
    !
    !  9. switches :
    !
    !     c/s   enable subroutine tracing.
    !     c/t   enable general test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, nspec, sig, dsip, ecos, esin, es2, &
         esc, ec2, fachfa, mapwn, flcth, flck, ctmax
    use w3adatmd, only: is0, is2
    use w3idatmd, only: flcur
    use w3odatmd, only: ndst
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
    integer                 :: ith, ik, isp, ith0
    real                    :: fddmax, fdg, dcyx, dcxxyy, dcxy,     &
         dcxx, dcxyyx, dcyy, fkd, fkd0, cthb, &
         cwnb
    real                    :: vcth(nspec), vcwn(1-nth:nspec+nth),  &
         vaa(1-nth:nspec+nth), vflth(nspec),  &
         vflwn(1-nth:nspec), dsdd(0:nk+1),    &
         frk(nk), frg(nk), fkc(nth), dwni(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a array with partial derivative of sigma versus depth
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
    ! 1.b extract spectrum
    !
    do isp=1, nspec
      vaa(isp) = va(isp)
    end do
    !
    ! 2.  refraction velocities ------------------------------------------ *
    !
    if ( flcth ) then
      !
      ! 2.a set slope filter for depth refraction
      !
      fddmax = 0.
      fdg    = facth * cthg0
      !
      do ith=1, nth
        fddmax = max ( fddmax , abs (                             &
             esin(ith)*dddx - ecos(ith)*dddy ) )
      end do
      !
      do ik=1, nk
        frk(ik) = facth * dsdd(ik) / wn(ik)
        frk(ik) = frk(ik) / max ( 1. , frk(ik)*fddmax/ctmax )
        frg(ik) = fdg * cg(ik)
      end do
      !
      ! 2.b depth refraction and great-circle propagation
      !
      do isp=1, nspec
        vcth(isp) = frg(mapwn(isp)) * ecos(isp)                   &
             + frk(mapwn(isp)) * ( esin(isp)*dddx - ecos(isp)*dddy )
      end do
      !
      !
      ! 2.d current refraction
      !
      if ( flcur ) then
        !
        dcyx   = facth *   dcydx
        dcxxyy = facth * ( dcxdx - dcydy )
        dcxy   = facth *   dcxdy
        !
        do isp=1, nspec
          vcth(isp) = vcth(isp) + es2(isp)*dcyx                 &
               + esc(isp)*dcxxyy - ec2(isp)*dcxy
        end do
        !
      end if
      !
    end if
    !
    ! 3.  wavenumber shift velocities ------------------------------------ *
    !
    if ( flck ) then
      !
      dcxx   =  - fack *   dcxdx
      dcxyyx =  - fack * ( dcxdy + dcydx )
      dcyy   =  - fack *   dcydy
      fkd    =    fack * ( cx*dddx + cy*dddy )
      !
      do ith=1, nth
        fkc(ith) = ec2(ith)*dcxx +                                &
             esc(ith)*dcxyyx + es2(ith)*dcyy
      end do
      !
      isp    = -nth
      do ik=0, nk+1
        fkd0   = fkd / cg(ik) * dsdd(ik)
        do ith=1, nth
          isp    = isp + 1
          vcwn(isp) = fkd0 + wn(ik)*fkc(ith)
        end do
      end do
      !
      ith0   = nspec - nth
      do ith=1, nth
        vaa(ith+nspec) = fachfa * vaa(ith+ith0)
        vaa(ith- nth ) = 0.
      end do
      !
      do ik=1, nk
        dwni(ik) = cg(ik) / dsip(ik)
      end do
      !
    end if
    !
    ! 4.  refraction ----------------------------------------------------- *
    !
    if ( flcth ) then
      !
      ! 4.a boundary velocity and fluxes
      !
      do isp=1, nspec
        cthb       = 0.5 * ( vcth(isp) + vcth(is2(isp)) )
        vflth(isp) = max ( cthb , 0. ) * vaa(isp)                 &
             + min ( cthb , 0. ) * vaa(is2(isp))
      end do
      !
      ! 4.b propagation
      !
      do isp=1, nspec
        va(isp) = va(isp) + vflth(is0(isp)) - vflth(isp )
      end do
      !
    end if
    !
    ! 5.  wavenumber shifts ---------------------------------------------- *
    !
    if ( flck ) then
      !
      ! 5.a boundary velocity and fluxes
      !
      do isp=1-nth, nspec
        cwnb       = 0.5 * ( vcwn(isp) + vcwn(isp+nth) )
        vflwn(isp) = max ( cwnb , 0. ) * vaa(  isp  )             &
             + min ( cwnb , 0. ) * vaa(isp+nth)
      end do
      !
      ! 5.c propagation
      !
      do isp=1, nspec
        va(isp) = va(isp) + dwni(mapwn(isp)) *                    &
             ( vflwn(isp-nth) - vflwn(isp) )
      end do
      !
    end if
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3ktp1 ----------------------------------------------------- /
    !/
  end subroutine w3ktp1
  !/
  !/ end of module w3pro1md -------------------------------------------- /
  !/
end module w3pro1md
