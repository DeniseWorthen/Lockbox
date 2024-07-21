!> @file
!> @brief bundles all input updating routines for wavewatch iii.
!>
!> @author h. l. tolman
!> @date   22-mar-2021
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
!> @brief bundles all input updating routines for wavewatch iii.
!>
!> @author h. l. tolman
!> @date   22-mar-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3updtmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    21-jan-2000 : origination.                        ( version 2.00 )
  !/    24-jan-2001 : flat grid version.                  ( version 2.06 )
  !/    02-apr-2001 : adding sub-grid obstacles.          ( version 2.10 )
  !/    18-may-2001 : clean up and bug fixes.             ( version 2.11 )
  !/    11-jan-2002 : sub-grid ice.                       ( version 2.15 )
  !/    30-apr-2002 : water level fixes.                  ( version 2.20 )
  !/    13-nov-2002 : add stress vector.                  ( version 3.00 )
  !/    26-dec-2002 : moving grid wind correction.        ( version 3.02 )
  !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    15-jul-2005 : adding mapst2.                      ( version 3.07 )
  !/    07-sep-2005 : upgrading w3ubpt.                   ( version 3.08 )
  !/    04-jul-2006 : consolidate stress arrays.          ( version 3.09 )
  !/    11-jan-2007 : clean-up w3utrn boundary points.    ( version 3.10 )
  !/    11-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    17-aug-2010 : abpi0-n(:,0) init. bug fix.         ( version 3.14 )
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    05-apr-2011 : place holder for xgr in ungtype     ( version 4.04 )
  !/                  (a. roland/f. ardhuin)
  !/    13-mar-2012 : add initialization of ust on re-    ( version 4.07 )
  !/                  activation of grid point.
  !/    06-jun-2012 : porting bugfixes from 3.14 to 4.07  ( version 4.07 )
  !/    12-jun-2012 : add /rtd option or rotated grid option.
  !/                  (jian-guo li)                       ( version 4.07 )
  !/    26-sep-2012 : adding update from tidal analysis   ( version 4.08 )
  !/                  (f. ardhuin)
  !/    16-sep-2013 : add arctic part for smc grid.       ( version 4.11 )
  !/    11-nov-2013 : smc and rotated grid incorporated in the main
  !/                  trunk                               ( version 4.13 )
  !/    13-nov-2013 : moved reflection from ww3_grid.ftn  ( version 4.13 )
  !/    27-may-2014 : ading ompg parallelizations dir,    ( version 5.02 )
  !/    08-may-2014 : implement tripolar grid for first order propagation
  !/                  scheme                              ( version 5.03 )
  !/                  (w. e. rogers, nrl)
  !/    27-aug-2015 : new function to update variables    ( version 5.08 )
  !/                  icef and icedmax at the first time step
  !/                  and add iceh initialization in w3uice.
  !/    13-jan-2016 : changed initial value of icedmax    ( version 5.08 )
  !/    26-mar-2018 : sea-point only wnd/cur input.  jgli ( version 6.04 )
  !/    07-oct-2019 : rtd option with standard lat-lon
  !/                  grid when nesting to rotated grid   ( version 7.11 )
  !/    22-mar-2021 : add w3utau, w3urho routines         ( version 7.13 )
  !/    06-may-2021 : use arctc option for smc grid. jgli ( version 7.13 )
  !/
  !/    copyright 2009-2014 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     bundles all input updating routines for wavewatch iii.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3ucur    subr. public   update current fields.
  !      w3uwnd    subr. public   update wind fields.
  !      w3utau    subr. public   update atmospheric momentum fields.
  !      w3uini    subr. public   update initial conditions.
  !      w3ubpt    subr. public   update boundary conditions.
  !      w3uice    subr. public   update ice concentrations.
  !      w3ulev    subr. public   update water levels.
  !      w3urho    subr. public   update air density.
  !      w3utrn    subr. public   update cell boundary transparancies.
  !      w3dzxy    subr. public   calculate derivatives of a field.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      dsec21    func. w3timemd difference in time.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd exit program with error code.
  !      prtblk    subr. w3arrymd print plot output.
  !      prt2ds    subr. w3arrymd print plot output.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/shrd   switch for shared / distributed memory architecture.
  !       !/dist   id.
  !
  !       !/ompg   openmp compiler directives.
  !
  !       !/crt0   no current interpolation.
  !       !/crt1   linear current interpolation.
  !       !/crt2   quasi-quadratic current interpolation.
  !
  !       !/wnt0   no wind/momentum interpolation.
  !       !/wnt1   linear wind/momentum interpolation.
  !       !/wnt2   energy conservation in wind/momentum interpolation.
  !
  !       !/rwnd   use wind speeds relative to currents.
  !
  !       !/stab2  calculate effective wind speed factor for stability
  !                to be used with !/st2.
  !
  !       !/s      enable subroutine tracing.
  !       !/tn     test output
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  use constants
  use w3odatmd, only: ndse, ndst, naproc, iaproc, naperr
  use w3timemd, only: dsec21
  !/
  !/ ------------------------------------------------------------------- /
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief interpolate the current field to the present time.
  !>
  !> @details linear interpolation of speed and direction, with optionally
  !>  a correction to get approximate quadratic interpolation of speed
  !>  only.
  !>
  !> @param[in] flfrst flag for first pass through routine.
  !>
  !> @author h. l. tolman
  !> @date   15-dec-2004
  !>
  subroutine w3ucur ( flfrst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    09-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    20-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    27-aug-2015 : rename dt0,dtt by dt0t,dt0n         ( version 5.10 )
    !/    23-mar-2016 : smc grid arctic part adjustment.    ( version 5.18 )
    !/    26-mar-2018 : sea-point only current on smc grid. ( version 6.02 )
    !/
    !  1. purpose :
    !
    !     interpolate the current field to the present time.
    !
    !  2. method :
    !
    !     linear interpolation of speed and direction, with optionally
    !     a correction to get approximate quadratic interpolation of speed
    !     only.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       flfrst  log.  i   flag for first pass through routine.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - only currents at sea points are considered.
    !     - time ranges checked in w3wave.
    !     - currents are stored by components to save on the use of
    !       sin and cos functions. the actual interpolations, however
    !       are by absolute value and direction.
    !
    !  8. structure :
    !
    !     --------------------------------------
    !      1.  prepare auxiliary arrays.
    !      2.  calculate interpolation factors.
    !      3.  get actual winds.
    !     --------------------------------------
    !
    !  9. switches :
    !
    !     !/crt0  no current interpolation.
    !     !/crt1  linear current interpolation.
    !     !/crt2  quasi-quadratic current interpolation.
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, mapsf
    use w3wdatmd, only: time
    use w3adatmd, only: cx, cy, ca0, cai, cd0, cdi
    use w3idatmd, only: tc0, cx0, cy0, tcn, cxn, cyn
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    logical, intent(in)     :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, ix, iy
    real                    :: d0, dn, dd, dt0n, dt0t, rd, cabs, cdir
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  prepare auxiliary arrays
    !
    if ( flfrst ) then
      do isea=1, nsea
          ix        = mapsf(isea,1)
          iy        = mapsf(isea,2)
        ca0(isea) = sqrt ( cx0(ix,iy)**2 + cy0(ix,iy)**2 )
        cai(isea) = sqrt ( cxn(ix,iy)**2 + cyn(ix,iy)**2 )
        if ( ca0(isea) .gt. 1.e-7) then
          d0     = mod ( tpi+atan2(cy0(ix,iy),cx0(ix,iy)) , tpi )
        else
          d0     = 0
        end if
        if ( cai(isea) .gt. 1.e-7) then
          dn     = mod ( tpi+atan2(cyn(ix,iy),cxn(ix,iy)) , tpi )
        else
          dn     = d0
        end if
        if ( ca0(isea) .gt. 1.e-7) then
          cd0(isea) = d0
        else
          cd0(isea) = dn
        end if
        dd     = dn - cd0(isea)
        if (abs(dd).gt.pi) dd = dd - tpi*sign(1.,dd)
        cdi(isea) = dd
        cai(isea) = cai(isea) - ca0(isea)
      end do
    end if
    !
    ! 2.  calculate interpolation factor
    !
    dt0n    = dsec21 ( tc0, tcn )
    dt0t    = dsec21 ( tc0, time )
    !
    !
    !
    ! 3.  actual currents for all grid points
    !
    do isea=1, nsea
        cabs    = ca0(isea) + rd * cai(isea)
        cdir    = cd0(isea) + rd * cdi(isea)
        cx(isea) = cabs * cos(cdir)
        cy(isea) = cabs * sin(cdir)
      !
    end do
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3ucur ----------------------------------------------------- /
    !/
  end subroutine w3ucur
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief interpolate wind fields to the given time.
  !>
  !> @details linear interpolation of wind speed and direction, with a
  !>  simple correction to obtain quasi-conservation of energy.
  !>
  !> @param[in] flfrst  flag for first pass through routine.
  !> @param[in] vgx     grid velocity
  !> @param[in] vgy     grid velocity
  !>
  !> @author h. l. tolman
  !> @date   27-may-2014
  !>
  subroutine w3uwnd ( flfrst, vgx, vgy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-may-2014 |
    !/                  +-----------------------------------+
    !/
    !/    03-dec-1998 : final fortran 77                    ( version 1.18 )
    !/    20-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    13-nov-2002 : add stress vector.                  ( version 3.00 )
    !/    26-dec-2002 : moving grid wind correction.        ( version 3.02 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    04-jul-2006 : consolidate stress arrays.          ( version 3.09 )
    !/    16-sep-2013 : rotating wind for arctic part.      ( version 4.11 )
    !/    27-may-2014 : adding ompg parallelizations dir.   ( version 5.02 )
    !/    27-aug-2015 : rename dt0,dtt by dt0t,dt0n         ( version 5.10 )
    !/    26-mar-2018 : sea-point only wind for smc grid.   ( version 6.07 )
    !/
    !  1. purpose :
    !
    !     interpolate wind fields to the given time.
    !
    !  2. method :
    !
    !     linear interpolation of wind speed and direction, with a simple
    !     correction to obtain quasi-conservation of energy.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       flfrst  log.  i   flag for first pass through routine.
    !       vgx/y   real  i   grid velocity                      (!/mgw)
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - only winds over sea points are considered.
    !     - time ranges checked in w3wave.
    !
    !  8. structure :
    !
    !     --------------------------------------
    !      1.  prepare auxiliary arrays.
    !      2.  calculate interpolation factors
    !      3.  get actual winds
    !      4.  correct for currents
    !      5.  convert to stresses
    !      6.  stability correction
    !     --------------------------------------
    !
    !  9. switches :
    !
    !     !/ompg   openmp compiler directives.
    !
    !     !/wnt0   no wind interpolation.
    !     !/wnt1   linear wind interpolation.
    !     !/wnt2   energy conservation in wind interpolation.
    !
    !     !/rwnd   use wind speeds relative to currents.
    !     !/mgw    moving grid wind correction.
    !
    !     !/stab2  calculate effective wind speed factor for stability
    !              to be used with !/st2.
    !
    !     !/s      enable subroutine tracing.
    !     !/t      test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, mapsf
    use w3wdatmd, only: time, asf
    use w3adatmd, only: dw, cx, cy, ua, ud, u10, u10d, as,          &
         ua0, uai, ud0, udi, as0, asi
    use w3idatmd, only: tw0, wx0, wy0, dt0, twn, wxn, wyn, dtn, flcur
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: vgx, vgy
    logical, intent(in)     :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, ix, iy
    real                    :: d0, dn, dd, dt0n, dt0t, rd, ui2,      &
         uxr, uyr
    real                    :: udarc
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  prepare auxiliary arrays
    !
    if ( flfrst ) then
      do isea=1, nsea
          ix        = mapsf(isea,1)
          iy        = mapsf(isea,2)
        ua0(isea) = sqrt ( wx0(ix,iy)**2 + wy0(ix,iy)**2 )
        uai(isea) = sqrt ( wxn(ix,iy)**2 + wyn(ix,iy)**2 )
        if ( ua0(isea) .gt. 1.e-7) then
          d0     = mod ( tpi+atan2(wy0(ix,iy),wx0(ix,iy)) , tpi )
        else
          d0     = 0
        end if
        if ( uai(isea) .gt. 1.e-7) then
          dn     = mod ( tpi+atan2(wyn(ix,iy),wxn(ix,iy)) , tpi )
        else
          dn     = d0
        end if
        if ( ua0(isea) .gt. 1.e-7) then
          ud0(isea) = d0
        else
          ud0(isea) = dn
        end if
        dd     = dn - ud0(isea)
        if (abs(dd).gt.pi) dd = dd - tpi*sign(1.,dd)
        udi(isea) = dd
        uai(isea) = uai(isea) - ua0(isea)
        as0(isea) = dt0(ix,iy)
        asi(isea) = dtn(ix,iy) - dt0(ix,iy)
      end do
    end if
    !
    ! 2.  calculate interpolation factor
    !
    dt0n    = dsec21 ( tw0, twn )
    dt0t   = dsec21 ( tw0, time )
    !
    !
    !
    ! 3.  actual wind for all grid points
    !
    !
    do isea=1, nsea
      !
      ua(isea) = ua0(isea) + rd * uai(isea)
      ud(isea) = ud0(isea) + rd * udi(isea)
      !
      as(isea) = as0(isea) + rd * asi(isea)
      !        if (ua(isea).ne.ua(isea)) write(6,*) 'bug wind:',isea,ua(isea),mapsf(isea,1), mapsf(isea,2),ua0(isea),rd,uai(isea)
      !        if (ud(isea).ne.ud(isea)) write(6,*) 'bug win2:',isea,ud(isea),mapsf(isea,1), mapsf(isea,2)
      !
    end do
    !
    !
    ! 3.b  bias correction ( !/wcor )
    !
    ! 4.  correct for currents and grid motion
    !
      !
      !
      !
      !
      do isea=1, nsea
        u10 (isea) = max ( ua(isea) , 0.001 )
        u10d(isea) = ud(isea)
      end do
      !
      !
    !
    ! 5.  stability correction ( !/stab2 )
    !     original settings :
    !
    !     shstab =    1.4
    !     ofstab =   -0.01
    !     ccng   =   -0.1
    !     ccps   =    0.1
    !     ffng   = -150.
    !     ffps   =  150.
    !
    !
      !
      !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3uwnd ----------------------------------------------------- /
    !/
  end subroutine w3uwnd
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief interpolate atmosphere momentum fields to the given time.
  !>
  !> @details linear interpolation of momentum module and direction, with
  !>  a simple correction to obtain quasi-conservation of energy.
  !>
  !> @param[in] flfrst  flag for first pass through routine.
  !>
  !> @author j. m. castillo
  !> @date   22-mar-2021
  !>
  subroutine w3utau ( flfrst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           j. m. castillo          |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    22-mar-2021 : first implementation                ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     interpolate atmosphere momentum fields to the given time.
    !
    !  2. method :
    !
    !     linear interpolation of momentum module and direction, with a simple
    !     correction to obtain quasi-conservation of energy.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       flfrst  log.  i   flag for first pass through routine.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - only momentum over sea points is considered.
    !     - time ranges checked in w3wave.
    !
    !  8. structure :
    !
    !     --------------------------------------
    !      1.  prepare auxiliary arrays.
    !      2.  calculate interpolation factors
    !      3.  get actual momentum
    !     --------------------------------------
    !
    !  9. switches :
    !
    !     !/ompg   openmp compiler directives.
    !
    !     !/wnt0   no momentum interpolation.
    !     !/wnt1   linear momentum interpolation.
    !     !/wnt2   energy conservation in momentum interpolation.
    !
    !     !/s      enable subroutine tracing.
    !     !/t      test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nsea, mapsf
    use w3wdatmd, only: time
    use w3adatmd, only: taua, tauadir, ma0, mai, md0, mdi
    use w3idatmd, only: tu0, ux0, uy0, tun, uxn, uyn
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    logical, intent(in)     :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, ix, iy
    real                    :: d0, dn, dd, dt0n, dt0t, rd, mi2,      &
         mxr, myr
    real                    :: mdarc
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  prepare auxiliary arrays
    !
    if ( flfrst ) then
      do isea=1, nsea
          ix        = mapsf(isea,1)
          iy        = mapsf(isea,2)
        ma0(isea) = sqrt ( ux0(ix,iy)**2 + uy0(ix,iy)**2 )
        mai(isea) = sqrt ( uxn(ix,iy)**2 + uyn(ix,iy)**2 )
        if ( ma0(isea) .gt. 1.e-7) then
          d0     = mod ( tpi+atan2(uy0(ix,iy),ux0(ix,iy)) , tpi )
        else
          d0     = 0
        end if
        if ( mai(isea) .gt. 1.e-7) then
          dn     = mod ( tpi+atan2(uyn(ix,iy),uxn(ix,iy)) , tpi )
        else
          dn     = d0
        end if
        if ( ma0(isea) .gt. 1.e-7) then
          md0(isea) = d0
        else
          md0(isea) = dn
        end if
        dd     = dn - md0(isea)
        if (abs(dd).gt.pi) dd = dd - tpi*sign(1.,dd)
        mdi(isea) = dd
        mai(isea) = mai(isea) - ma0(isea)
      end do
    end if
    !
    ! 2.  calculate interpolation factor
    !
    dt0n    = dsec21 ( tu0, tun )
    dt0t   = dsec21 ( tu0, time )
    !
    !
    !
    ! 3.  actual momentum for all grid points
    !
    !
    do isea=1, nsea
      !
      taua(isea) = ma0(isea) + rd * mai(isea)
      tauadir(isea) = md0(isea) + rd * mdi(isea)
      !
    end do
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3utau ----------------------------------------------------- /
    !/
  end subroutine w3utau
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialize the wave field with fetch-limited spectra before
  !>  the actual calculation start.
  !>
  !>
  !> @details named as an update routine due to placement in code.
  !>
  !>  fetch-limited jonswap spectra with a cosine^2 directional
  !>  distribution and a mean direction taken from the wind.
  !>
  !> @param[out] a  action density spectra.
  !>
  !> @author h. l. tolman
  !> @date   06-jun-2018
  !>
  subroutine w3uini ( a )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    19-oct-1998 : final fortran 77                    ( version 1.18 )
    !/    20-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    24-jan-2001 : flat grid version.                  ( version 2.06 )
    !/    18-may-2001 : fix cg declaration.                 ( version 2.11 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/    06-jun-2018 : use w3parall and init_get_isea      ( version 6.04 )
    !/
    !  1. purpose :
    !
    !     initialize the wave field with fetch-limited spectra before the
    !     actual calculation start. (named as an update routine due to
    !     placement in code.)
    !
    !  2. method :
    !
    !     fetch-limited jonswap spectra with a cosine^2 directional
    !     distribution and a mean direction taken from the wind.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   o   action density spectra.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - wind speeds filtered by u10min and u10max (data statements)
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/shrd  switch for shared / distributed memory architecture.
    !       !/dist  id.
    !
    !       !/s     enable subroutine tracing.
    !       !/t     general test output.
    !       !/t1    parameters at grid points.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, nseal, mapsf,                 &
         nk, nth, th, sig, dth, dsip, ungtype,       &
         rlgtype, clgtype, gtype, flagll,            &
         hpfac, hqfac
    use w3adatmd, only: u10, u10d, cg
    use w3parall, only : init_get_jsea_isproc, init_get_isea
    use w3parall, only : get_jsea_ibelong
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(out)       :: a(nth,nk,0:nseal)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local variables
    !/
    integer                 :: ix, iy, isea, jsea, ik, ith, isproc
    real                    :: alfa(nseal), fp(nseal), yln(nseal),  &
         aa, bb, cc
    real                    :: xgr, u10c, u10dir, xstar, fstar,     &
         gamma, fr, d1(nth), d1int, f1, f2
    real                    :: etot, e1i
    real                    :: u10min =  1.
    real                    :: u10max = 20.
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! pre-process jonswap data for all grid points ----------------------- *
    !
    !
    !  this is not clear what is going on betwen w3init and this ...
    a(:,:,:)=0
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      if (gtype.eq.ungtype) then
        xgr=1.  ! to be fixed later
      else
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        xgr    = 0.5 * sqrt(hpfac(iy,ix)**2+hqfac(iy,ix)**2)
      end if
      if ( flagll ) then
        xgr    = xgr * radius * dera
      end if
      !
      u10c   = max ( min(u10(isea),u10max) , u10min )
      !
      xstar  = grav * xgr / u10c**2
      fstar  = 3.5 / xstar**(0.33)
      gamma  = max ( 1. , 7.0 / xstar**(0.143) )
      !
      alfa(jsea) = 0.076 / xstar**(0.22)
      fp  (jsea) = fstar * grav / u10c
      yln (jsea) = log ( gamma )
      !
      !
    end do
    !
    ! 1-d spectrum at location ith = nth --------------------------------- *
    !
    do ik=1, nk
      fr     = sig(ik) * tpiinv
      do jsea=1, nseal
        !
        !/ ----- inlined ej5p (reduced) -------------------------------------- /
        !
        aa     = alfa(jsea) * 0.06175/fr**5
        bb     = max( -50. , -1.25*(fp(jsea)/fr)**4 )
        cc     = max( -50. , -0.5*((fr-fp(jsea))/(0.07*fp(jsea)))**2 )
        a(nth,ik,jsea)                                              &
             = aa * exp(bb + exp(cc) * yln(jsea))
        !
        !/ ----- inlined ej5p (end) ------------------------------------------ /
        !
      end do
    end do
    !
    ! apply directional distribution ------------------------------------- *
    !
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      u10dir = u10d(isea)
      d1int  = 0.
      !
      do ith=1, nth
        d1(ith) = ( max ( 0. , cos(th(ith)-u10dir) ) )**2
        d1int   = d1int + d1(ith)
      end do
      !
      d1int  = d1int * dth
      f1     = tpiinv / d1int
      !
      do ik=1, nk
        f2     = f1 * a(nth,ik,jsea) * cg(ik,isea) / sig(ik)
        do ith=1, nth
          a(ith,ik,jsea) = f2 * d1(ith)
        end do
      end do
      !
    end do
    !
    ! test output -------------------------------------------------------- *
    !
    !
    !
    !
    return
    !
    ! formats
    !
    !
    !/
    !/ end of w3uini ----------------------------------------------------- /
    !/
  end subroutine w3uini
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief update spectra at the active boundary points.
  !>
  !> @details spectra are read and interpolated in space and time from
  !> the data read by w3iobc.
  !>
  !> @author h. l. tolman
  !> @date   06-jun-2018
  !>
  subroutine w3ubpt
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    19-oct-1998 : final fortran 77                    ( version 1.18 )
    !/    20-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    07-sep-2005 : moving update to end of time step.  ( version 3.08 )
    !/    17-aug-2010 : add initialization abpi0-n(:,0).  ( version 3.14.5 )
    !/    12-jun-2012 : add /rtd option or rotated grid option.
    !/                  (jian-guo li)                       ( version 4.06 )
    !/    06-jun-2018 : add debugiobc/setup/debugw3ulev     ( version 6.04 )
    !/    13-jun-2019 : rotation only if polat<90 (c.hansen)( version 7.11 )
    !/
    !  1. purpose :
    !
    !     update spectra at the active boundary points.
    !
    !  2. method :
    !
    !     spectra are read and interpolated in space and time from the
    !     data read by w3iobc.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !       strace, dsec21
    !                service routines.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - the data arrays contain sigma spectra to assure conservation
    !       when changing grids.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s     enable subroutine tracing.
    !     !/t0    test output of wave heights.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nspec, mapwn, sig2, dden
    use w3adatmd, only: cg
    use w3odatmd, only: nbi, abpi0, abpin, isbpi, ipbpi, rdbpi,     &
         bbpi0, bbpin
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: ibi, isp, isea
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  process bbpi0 -------------------------------------------------- *
    ! 1.a first intialization
    !
    if ( bbpi0(1,0) .eq. -1. ) then
      !
      bbpi0(:,0) = 0.
      bbpin(:,0) = 0.
      abpi0(:,0) = 0.
      abpin(:,0) = 0.
      !
      do ibi=1, nbi
        isea   = isbpi(ibi)
        do isp=1, nspec
          bbpi0(isp,ibi) = cg(mapwn(isp),isea) / sig2(isp) *      &
               ( rdbpi(ibi,1) * abpi0(isp,ipbpi(ibi,1))   &
               + rdbpi(ibi,2) * abpi0(isp,ipbpi(ibi,2))   &
               + rdbpi(ibi,3) * abpi0(isp,ipbpi(ibi,3))   &
               + rdbpi(ibi,4) * abpi0(isp,ipbpi(ibi,4)) )
        end do
      end do
      !
      ! 1.b shift bbpin
      !
    else
      bbpi0 = bbpin
    end if
    !
    ! 2.  process bbpin -------------------------------------------------- *
    !
    do ibi=1, nbi
      isea   = isbpi(ibi)
      do isp=1, nspec
        bbpin(isp,ibi) = cg(mapwn(isp),isea) / sig2(isp) *          &
             ( rdbpi(ibi,1) * abpin(isp,ipbpi(ibi,1))       &
             + rdbpi(ibi,2) * abpin(isp,ipbpi(ibi,2))       &
             + rdbpi(ibi,3) * abpin(isp,ipbpi(ibi,3))       &
             + rdbpi(ibi,4) * abpin(isp,ipbpi(ibi,4)) )
      end do
      !
      !
    end do
    ! 3.  wave height test output ---------------------------------------- *
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3ubpt ----------------------------------------------------- /
    !/
  end subroutine w3ubpt
  !/ ------------------------------------------------------------------- /
  !> @attention flfrst not used.
  !> @brief update ice thickness in the wave model.
  !>
  !> @param[in] flfrst
  !>
  !> @author c. sevigny
  !> @date   27-aug-2015
  !>
  subroutine w3uic1( flfrst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. sevigny              |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-aug-2015 |
    !/                  +-----------------------------------+
    !/
    !/    27-aug-2015 : creation                            ( version 5.10 )
    !/
    !  1. purpose :
    !
    !     update ice thickness in the wave model.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      flfrst   l.    i     spectra in 1-d or 2-d representation
    !                           (points to same address).
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nsea, nsea, mapsf, iicehmin, iicehfac
    use w3wdatmd, only: time, tic1, iceh
    use w3idatmd, only: ti1, icep1, flic1
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    logical, intent(in)     :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local variables
    !/
    integer                 :: ix, iy, isea
    !/
    !/
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a update times
    !
    tic1(1) = ti1(1)
    tic1(2) = ti1(2)
    ! 2.  main loop over sea points -------------------------------------- *
    do isea=1, nsea
      !
      ix        = mapsf(isea,1)
      iy        = mapsf(isea,2)
      iceh(isea) = max(iicehmin,iicehfac*icep1(ix,iy))
    end do
    !
    return
    !/
    !/ end of w3uic1 ----------------------------------------------------- /
    !/
  end subroutine w3uic1
  !/ ------------------------------------------------------------------- /
  !> @attention flfrst not currently used.
  !> @brief update ice floe mean and max diameters in the wave model.
  !>
  !> @param[in] flfrst
  !>
  !> @author c. sevigny
  !> @author f. ardhuin
  !> @date   13-jan-2016
  !>
  subroutine w3uic5( flfrst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |  c. sevigny  & f. ardhuin         |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    27-aug-2015 : creation                            ( version 5.08 )
    !/    13-jan-2016 : changed initial value of icedmax    ( version 5.08 )
    !/
    !  1. purpose :
    !
    !     update ice floe mean and max diameters in the wave model.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      flfrst   l.    i     spectra in 1-d or 2-d representation
    !                           (points to same address).
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3idatmd, only: ti5, icep5
    use w3gdatmd, only: nsea, mapsf
    use w3wdatmd, only: time, tic5, ice, iceh, icef, icedmax
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    logical, intent(in)     :: flfrst
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local variables
    !/
    integer                 :: ix, iy, isea
    logical                 :: flfloe
    !/
    !/
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a update times
    !
    tic5(1) = ti5(1)
    tic5(2) = ti5(2)
    ! 2.  main loop over sea points -------------------------------------- *
    do isea=1, nsea
      !
      ix        = mapsf(isea,1)
      iy        = mapsf(isea,2)
      flfloe = ice(isea) .eq. 0 .or. iceh(isea) .eq. 0
      if ( flfloe) then
        icef(isea) = 0.0
        icedmax(isea) = 1000.0
      else
        icef(isea) = icep5(ix,iy)
        icedmax(isea) = icep5(ix,iy)
      end if
    end do
    !
    return
    !/
    !/
    !/ end of w3uic5 ----------------------------------------------------- /
    !/
  end subroutine w3uic5
  !/ ------------------------------------------------------------------- /
!>
!> @brief update ice map in the wave model.
!>
!> @details points with an ice concentration larger than ficen are
!>  removed from the sea map in the wave model. such points are
!>  identified by negative numbers is the grid status map mapsta. for
!>  ice points spectra are set to zero. points from which ice disappears
!>  are initialized with a "small" jonswap spectrum, based on the
!>  frequency sig(nk-1) and the local wind direction.
!>
!>  in the case of icebergs, the iceberg attenuation coefficient is
!>  added to the subgrid obstruction map.
!>
!> @param[inout] va  spectra in 1-d or 2-d representation.
!>
!> @author h. l. tolman
!> @date   28-mar-2014
!>
  subroutine w3uice ( va )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         28-mar-2014 |
    !/                  +-----------------------------------+
    !/
    !/    19-oct-1998 : final fortran 77                    ( version 1.18 )
    !/    20-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    11-jan-2002 : sub-grid ice.                       ( version 2.15 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    28-jun-2005 : adding mapst2.                      ( version 3.07 )
    !/                  taking out initilization.
    !/    11-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    15-may-2010 : adding second field for icebergs    ( version 3.14 )
    !/    13-mar-2012 : add initialization of ust on re-    ( version 4.07 )
    !/                  activation of grid point.
    !/    06-jun-2012 : porting bugfixes from 3.14 to 4.07  ( version 4.07 )
    !/    28-mar-2014 : adapting to icx source terms        ( version 4.18 )
    !/
    !  1. purpose :
    !
    !     update ice map in the wave model.
    !
    !  2. method :
    !
    !     points with an ice concentration larger than ficen are removed
    !     from the sea map in the wave model. such points are identified
    !     by negative numbers is the grid status map mapsta. for ice
    !     points spectra are set to zero. points from wich ice disappears
    !     are initialized with a "small" jonswap spectrum, based on the
    !     frequency sig(nk-1) and the local wind direction.
    !
    !     in the case of icebergs, the iceberg attenuation coefficient is
    !     added to the subgrid obstruction map.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      va       r.a.  i/o   spectra in 1-d or 2-d representation
    !                           (points to same address).
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, mapsf, mapsta, mapst2, &
         nspec, ficen
    use w3wdatmd, only: time, tice, ice, berg, ust
    use w3adatmd, only: nsealm, charn
    use w3idatmd, only: tin, icei, bergi
    use w3parall, only: init_get_jsea_isproc, init_get_isea
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(inout)     :: va(nspec,0:nsealm)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, jsea, ix, iy
    integer                 :: mapice(ny,nx), isproc
    logical                 :: local
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    local   = iaproc .le. naproc
    !
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a update times
    !
    tice(1) = tin(1)
    tice(2) = tin(2)
    !
    ! 1.b process maps
    !
    !
    ! 2.  main loop over sea points -------------------------------------- *
    !
    do isea=1, nsea
      !
      ! 2.a get grid counters
      !
      ix        = mapsf(isea,1)
      iy        = mapsf(isea,2)
      ice(isea) = icei(ix,iy)
      berg(isea)= bergi(ix,iy)
      !
      ! 2.b sea point to be de-activated..
      !
    end do
    !
    ! 3.  update mapst2 -------------------------------------------------- *
    !
    !
    return
    !
    !/
    !/ end of w3uice ----------------------------------------------------- /
    !/
  end subroutine w3uice
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief update the water level.
  !>
  !> @details the wavenumber grid is modified without modyfying the
  !>  spectrum (conservative linear interpolation to new grid).
  !>
  !> @param[inout] a   2-d represetation of the spectra.
  !> @param[inout] va  1-d represetation of the spectra.
  !>
  !> @author h. l. tolman
  !> @date   26-sep-2012
  !>
  subroutine w3ulev ( a, va )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-sep-2012 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-1998 : final fortran 77                    ( version 1.18 )
    !/    21-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    30-apr-2002 : water level fixes.                  ( version 2.20 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    15-jul-2005 : adding drying out of points.        ( version 3.07 )
    !/    11-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    23-aug-2011 : bug fix for ug grids : new boundary ( version 4.04 )
    !/    13-mar-2012 : add initialization of ust on re-    ( version 4.07 )
    !/                  activation of grid point.
    !/    06-jun-2012 : porting bugfixes from 3.14 to 4.07  ( version 4.07 )
    !/    26-sep-2012 : adding update from tidal analysis   ( version 4.08 )
    !/
    !  1. purpose :
    !
    !     update the water level.
    !
    !  2. method :
    !
    !     the wavenumber grid is modified without modyfying the spectrum
    !     (conservative linear interpolation to new grid).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      (v)a     r.a.  i/o  2-d and 1-d represetation of the spectra.
    !     ----------------------------------------------------------------
    !
    !     local variables
    !     ----------------------------------------------------------------
    !       kdmax   real   deep water cut-off for kd.
    !       wno     r.a.   old wavenumbers.
    !       cgo     r.a.   old group velocities.
    !       own     r.a.   old wavenumber band width.
    !       dwn     r.a.   new wavenumber band width.
    !       ta      r.a.   auxiliary spectrum.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - the grid is updated only if kdmin > kdmax.
    !     - the grid is updated for inactive points too.
    !     - the local wavenumber bandwidth is dsigma/cg.
    !     - the local spectrum is updated only if the grid is updated,
    !       the grid point is not disabled (mapst2) and if the change of
    !       the lowest wavenumber exceeds rdkmin times the band width.
    !     - no spectral initialization for newly wet points.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s     enable subroutine tracing.
    !     !/t     basic test output.
    !     !/t2    output of minimum relative depth per grid point.
    !     !/t3    spectra before and after
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, nseal, mapsf, mapsta, mapst2, &
         zb, dmin, nk, nth, nspec, sig, dsip,        &
         mapwn, mapth, fachfa, gtype, ungtype, w3setref
    use w3wdatmd, only: time, tlev, wlv, ust
    use w3adatmd, only: cg, wn, dw, hs
    use w3idatmd, only: tln, wlev
    use w3servmd, only: extcde
    use w3dispmd, only: wavnu1
    use w3timemd
    use w3parall, only : init_get_jsea_isproc, init_get_isea
    use w3parall, only : get_jsea_ibelong
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(inout)     :: a(nth,nk,0:nseal), va(nspec,0:nseal)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, jsea, ix, iy, ik, i1, i2,      &
         ispec, ik0, ith
    integer                 :: mapdry(ny,nx), isproc
    real                    :: dwo(nsea), kdchck, wno(0:nk+1),      &
         cgo(0:nk+1), depth,                  &
         rdk, rd1, rd2, ta(nth,nk),           &
         own(nk), dwn(nk)
    real                    :: kdmax = 4., rdkmin = 0.05
    real                    :: wlveff
    logical                 :: local
    integer                 :: ibelong
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    local   = iaproc .le. naproc
    !
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a check nk
    !
    if ( nk .lt. 2 ) then
      if ( iaproc .eq. naperr ) write (ndse,1000)
      call extcde ( 1 )
    end if
    !
    ! 1.b update times
    !
    tlev = tln
    !
    ! 1.c extract dry point map, and residual mapst2
    !
    mapdry = mod(mapst2/2,2)
    mapst2 = mapst2 - 2*mapdry
    !
    ! 1.d update water levels and save old
    !
    do isea=1, nsea
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      dwo(isea) = dw(isea)
      !
        !
        wlv(isea) = wlev(ix,iy)
        wlveff    = wlv(isea)
      dw (isea) = max ( 0. , wlveff-zb(isea) )
    end do ! nsea
    !
    ! 2.  loop over all sea points --------------------------------------- *
    !
    !
    do isea=1, nsea
      !
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      !
      ! 2.a check if deep water
      !
      kdchck = wn(1,isea) * min( dwo(isea) , dw(isea) )
      if ( kdchck .lt. kdmax ) then
        !
        ! 2.b update grid and save old grid
        !
        depth  = max ( dmin, dw(isea) )
        !
        do ik=0, nk+1
          wno(ik) = wn(ik,isea)
          cgo(ik) = cg(ik,isea)
          !
          !   calculate wavenumbers and group velocities.
              call wavnu1(sig(ik),depth,wn(ik,isea),cg(ik,isea))
        end do
        !
        do ik=1, nk
          own(ik) = dsip(ik) / cgo(ik)
          dwn(ik) = dsip(ik) / cg(ik,isea)
        end do
        !
        ! 2.c process dry points
        !
        if ( wlv(isea)-zb(isea) .le.0. ) then
          if ( mapdry(iy,ix) .eq. 0 ) then
            call get_jsea_ibelong(isea, jsea, ibelong)
            if ( local .and. (ibelong .eq. 1) ) then
              va(:,jsea) = 0.
            end if
            mapdry(iy,ix) = 1
            mapsta(iy,ix) = -abs(mapsta(iy,ix))
          endif
          cycle
        end if
        !
        ! 2.d process new wet point
        !
        if (wlv(isea)-zb(isea).gt.0. .and. mapdry(iy,ix).eq.1) then
          mapdry(iy,ix) = 0
          !
          ! resets the spectrum to zero
          !
          call get_jsea_ibelong(isea, jsea, ibelong)
          if ( local .and. (ibelong .eq. 1) ) then
            va(:,jsea) = 0.
          end if
          !
          ust(isea)     = 0.05
          if ( mapst2(iy,ix) .eq. 0 ) then
            mapsta(iy,ix) = abs(mapsta(iy,ix))
          end if
          cycle
        end if
        !
        ! 2.e check if ice on grid point, or if grid changes negligible
        !
        rdk    = abs(wno(1)-wn(1,isea)) / dwn(1)
        !
        !
        if ( rdk.lt.rdkmin .or. mapsta(iy,ix).lt.0 ) cycle
        call get_jsea_ibelong(isea, jsea, ibelong)
        if ( ibelong .eq. 0) cycle
        !
        if ( .not. local ) cycle
        !
        ! 2.d save discrete actions and clean spectrum
        !
        do ik=1, nk
          do ith=1, nth
            ta(ith,ik) = a(ith,ik,jsea) * own(ik)
          end do
        end do
        !
        va(:,jsea) = 0.
        !
        !
        ! 2.e redistribute discrete action density
        !
        if ( wno(1) .lt. wn(1,isea) ) then
          ik0    = 1
          i1     = 0
          i2     = 1
220       continue
          ik0    = ik0 + 1
          if ( ik0 .gt. nk+1 ) goto 251
          if ( wno(ik0) .ge. wn(1,isea) ) then
            ik0    = ik0 - 1
          else
            goto 220
          end if
        else
          ik0    = 1
          i1     = 1
          i2     = 2
        end if
        !
        do ik=ik0, nk
          !
230       continue
          if ( wno(ik) .gt. wn(i2,isea) ) then
            i1     = i1 + 1
            if ( i1 .gt. nk ) goto 250
            i2     = i1 + 1
            goto 230
          end if
          !
          if ( i1 .eq. 0 ) then
            rd1    = ( wn(1,isea) - wno(ik) ) / dwn(1)
            rd2    = 1. - rd1
          else
            rd1    = ( wn(i2,isea) - wno(ik) ) /                &
                 ( wn(i2,isea) - wn(i1,isea) )
            rd2    = 1. - rd1
          end if
          !
          if ( i1 .ge. 1 ) then
            do ith=1, nth
              a(ith,i1,jsea) = a(ith,i1,jsea) + rd1*ta(ith,ik)
            end do
          end if
          !
          if ( i2 .le. nk ) then
            do ith=1, nth
              a(ith,i2,jsea) = a(ith,i2,jsea) + rd2*ta(ith,ik)
            end do
          end if
          !
250       continue
        end do
251     continue
        !
        ! 2.f convert discrete action densities to spectrum
        !
        do ispec=1, nspec
          va(ispec,jsea) = va(ispec,jsea) / dwn(mapwn(ispec))
        end do
        !
        ! 2.f add tail if necessary
        !
        if ( i2.le.nk .and. rd2.le.0.95 ) then
          do ik=max(i2,2), nk
            do ith=1, nth
              a(ith,ik,jsea) = fachfa * a(ith,ik-1,jsea)
            end do
          end do
        end if
        !
        !
        !
      end if
      !
    end do ! nsea
    !
    ! 3.  reconstruct new mapst2 ----------------------------------------- *
    !
    mapst2 = mapst2 + 2*mapdry
    !
    ! 4. re-generates the boundary data ---------------------------------- *
    !
    if (gtype.eq.ungtype) then
    endif
    !
    return
    !
    ! formats
    !
1000 format (/' *** error w3ulev *** '/                              &
         '     this routine requires nk > 1 '/)
    !
    !
    !
    !/
    !/ end of w3ulev ----------------------------------------------------- /
    !/
  end subroutine w3ulev
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief interpolate air density field to the given time.
  !>
  !> @details linear interpolation.
  !>
  !> @param[in] flfrst  flag for first pass through routine.
  !>
  !> @author j. m. castillo
  !> @date   13-aug-2021
  !>
  subroutine w3urho ( flfrst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           j. m. castillo          |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-aug-2021 |
    !/                  +-----------------------------------+
    !/
    !/    22-mar-2021 : first implementation                ( version 7.13 )
    !/    13-aug-2021 : enable time interpolation           ( version 7.14 )
    !/
    !  1. purpose :
    !
    !     interpolate air density field to the given time.
    !
    !  2. method :
    !
    !     linear interpolation.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       flfrst  log.  i   flag for first pass through routine.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - only air density over sea points is considered.
    !     - time ranges checked in w3wave.
    !
    !  8. structure :
    !
    !     --------------------------------------
    !      1.  prepare auxiliary arrays
    !      2.  calculate interpolation factors
    !      3.  get actual air density
    !     --------------------------------------
    !
    !  9. switches :
    !
    !     !/ompg   openmp compiler directives
    !
    !     !/wnt0   no air density interpolation.
    !     !/wnt1   linear air density interpolation.
    !     !/wnt2   linear air density interpolation (and energy conservation for momentum).
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nsea, mapsf
    use w3wdatmd, only: time, trho, rhoair
    use w3idatmd, only: tr0, trn, rh0, rhn
    use w3adatmd, only: ra0, rai
    use w3odatmd, only: iaproc, naproc
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    logical, intent(in)     :: flfrst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, ix, iy
    real                    :: dt0n, dt0t, rd
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  prepare auxiliary arrays
    !
    if ( flfrst ) then
      do isea=1, nsea
          ix        = mapsf(isea,1)
          iy        = mapsf(isea,2)
        ra0(isea) = rh0(ix,iy)
        rai(isea) = rhn(ix,iy) - rh0(ix,iy)
      end do
    end if
    !
    ! 2.  calculate interpolation factor
    !
    dt0n   = dsec21 ( tr0, trn )
    dt0t   = dsec21 ( tr0, time )
    !
    !
    !
    ! 3.  actual momentum for all grid points
    !
    !
    do isea=1, nsea
      !
      rhoair(isea) = ra0(isea) + rd * rai(isea)
      !
    end do
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3urho ----------------------------------------------------- /
    !/
  end subroutine w3urho
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief update cell boundary transparencies for general use in
  !>  propagation routines.
  !>
  !> @details two arrays are generated with the size (ny*nx,-1:1). the value
  !>  at (ixy,-1) indicates the transparency to be used if the lower
  !>  or left boundary is an inflow boundary. (ixy,1) is used if the
  !>  upper or right boundary is an inflow boundary. (ixy,0) is used
  !>  for all other cases (by definition full transparency).
  !>
  !> @param[inout] trnx  transparencies from model definition file.
  !> @param[inout] trny  transparencies from model definition file.
  !>
  !> @author h. l. tolman
  !> @date   30-oct-2009
  !>
  subroutine w3utrn ( trnx, trny )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    02-apr-2001 : origination.                        ( version 2.10 )
    !/    11-jan-2002 : sub-grid ice.                       ( version 2.15 )
    !/    30-apr-2002 : change to ice on storage grid.      ( version 2.20 )
    !/    15-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    11-jan-2007 : clean-up for boundary points.       ( version 3.10 )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     update cell boundary transparencies for general use in propagation
    !     routines.
    !
    !  2. method :
    !
    !     two arrays are generated with the size (ny*nx,-1:1). the value
    !     at (ixy,-1) indicates the transparency to be used if the lower
    !     or left boundary is an inflow boundary. (ixy,1) is used if the
    !     upper or right boundary is an inflow boundary. (ixy,0) is used
    !     for all other cases (by definition full transparency).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       trnx/y  r.a.   i   transparencies from model defintion file.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !       !/t      basic test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, mapsta, mapsf,                &
         trflag, fice0, ficen, ficel,                &
         rlgtype, clgtype, gtype, flagll,            &
         hpfac, hqfac, ffacberg
    use w3wdatmd, only: ice, berg
    use w3adatmd, only: atrnx, atrny
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: trnx(ny*nx), trny(ny*nx)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer                 :: isea, ix, iy, ixy, ixn, ixp, iyn, iyp
    real                    :: trix(ny*nx), triy(ny*nx), dx, dy,    &
         lice0, licen
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    !
    atrnx = 1.
    atrny = 1.
    !
    ! 2.  filling arrays from trnx/y for obstructions -------------------- *
    ! 2.a trflag = 0, no action needed
    if ( trflag .eq. 0 ) then
      return
      !
      ! 2.b trflag = 1,3: trnx/y defined at boundaries
      !
    else if ( trflag.eq.1 .or. trflag.eq.3 .or. trflag.eq.5 ) then
      !
      do isea=1, nsea
        !
        ix            = mapsf(isea,1)
        iy            = mapsf(isea,2)
        ixy           = mapsf(isea,3)
        if ( ix .eq. 1 ) then
          atrnx(ixy,-1) = trnx(iy+(nx-1)*ny)
          atrnx(ixy, 1) = trnx(ixy)
        else if ( ix .eq. nx ) then
          atrnx(ixy,-1) = trnx(ixy-ny)
          atrnx(ixy, 1) = trnx(iy)
        else
          atrnx(ixy,-1) = trnx(ixy-ny)
          atrnx(ixy, 1) = trnx(ixy)
        end if
        atrny(ixy,-1) = trny(ixy-1)
        atrny(ixy, 1) = trny(ixy)
        !
        !
      end do
      !
      ! 2.c trflag = 2,4: trnx/y defined at cell centers
      !
    else
      !
      do isea=1, nsea
        !
        ix            = mapsf(isea,1)
        iy            = mapsf(isea,2)
        ixy           = mapsf(isea,3)
        !
        if ( ix .eq. 1 ) then
          ixn    = iy + (nx-1)*ny
          ixp    = iy +  ix   *ny
        else if ( ix .eq. nx ) then
          ixn    = iy + (ix-2)*ny
          ixp    = iy
        else
          ixn    = iy + (ix-2)*ny
          ixp    = iy +  ix   *ny
        end if
        !
        if ( iy .eq. 1 ) then
          iyn    = ixy
          iyp    = ixy + 1
        else if ( iy .eq. ny ) then
          iyn    = ixy - 1
          iyp    = ixy
        else
          iyn    = ixy - 1
          iyp    = ixy + 1
        end if
        !
        ! factors 0.5 in first term and 2. in second term cancel
        !
        atrnx(ixy,-1) = (1.+trnx(ixy)) * trnx(ixn)/(1.+trnx(ixn))
        atrnx(ixy, 1) = (1.+trnx(ixy)) * trnx(ixp)/(1.+trnx(ixp))
        atrny(ixy,-1) = (1.+trny(ixy)) * trny(iyn)/(1.+trny(iyn))
        atrny(ixy, 1) = (1.+trny(ixy)) * trny(iyp)/(1.+trny(iyp))
        !
        if ( mapsta(iy,ix) .eq. 2 ) then
          if ( ix .eq. 1  ) then
            atrnx(ixy,-1) = 1.
          else if ( mapsta( iy ,ix-1) .le. 0 ) then
            atrnx(ixy,-1) = 1.
          end if
          if ( ix .eq. nx ) then
            atrnx(ixy, 1) = 1.
          else if ( mapsta( iy ,ix+1) .le. 0 ) then
            atrnx(ixy, 1) = 1.
          end if
          if ( iy .eq. 1  ) then
            atrny(ixy,-1) = 1.
          else if ( mapsta(iy-1, ix ) .le. 0 ) then
            atrny(ixy,-1) = 1.
          end if
          if ( iy .eq. ny ) then
            atrny(ixy, 1) = 1.
          else if ( mapsta(iy+1, ix ) .le. 0 ) then
            atrny(ixy, 1) = 1.
          end if
        end if
        !
        !
      end do
    end if
    !
    !
    ! 3.  adding ice to obstructions ------------------------------------- *
    ! 3.a trflag < 3, no action needed
    !
    if ( trflag.lt.3 .or. ficen-fice0.lt.1.e-6 ) then
      return
      !
      ! 3.b trflag = 3,4: calculate ice transparencies
      !
    else
      trix   = 1.
      triy   = 1.
      !
      do isea=1, nsea
        !
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        ixy    = mapsf(isea,3)
        !
        dx     = hpfac(iy,ix)
        dy     = hqfac(iy,ix)
        if ( flagll ) then
          dx     = dx * radius * dera
          dy     = dy * radius * dera
        end if
        !
            ! otherwise: original tolman expression (tolman 2003)
            ! begin temporary notes
            !                trix = (   licen    - ice(isea)*dx ) / (   licen -      lice0 )
            !    thus, it is trix=  ( (ficen*dx) - ice(isea)*dx ) / ( (ficen*dx) - (fice0*dx) )
            !    thus, it is trix=  (   ficen -    ice(isea) )   /  (  ficen     -   fice0 )
            !    in other words, the variables dx dy are not used
            !                    and the variables lice0 licen are not necessary.
            ! end temporary notes
          !
        !
        !  adding iceberg attenuation
        !
        if (berg(isea).gt.0) then
          trix(ixy) = trix(ixy)*exp(-berg(isea)*ffacberg  *dx*0.0001)
          triy(ixy) = triy(ixy)*exp(-berg(isea)*ffacberg  *dy*0.0001)
        end if
        !
        !
      end do
      !
      !
      ! 3.c combine transparencies, ice always defined at cell center !
      !
      do isea=1, nsea
        !
        ix            = mapsf(isea,1)
        iy            = mapsf(isea,2)
        ixy           = mapsf(isea,3)
        !
        if ( ix .eq. 1 ) then
          ixn    = iy + (nx-1)*ny
          ixp    = iy +  ix   *ny
        else if ( ix .eq. nx ) then
          ixn    = iy + (ix-2)*ny
          ixp    = iy
        else
          ixn    = iy + (ix-2)*ny
          ixp    = iy +  ix   *ny
        end if
        !
        if ( iy .eq. 1 ) then
          iyn    = ixy
          iyp    = ixy + 1
        else if ( iy .eq. ny ) then
          iyn    = ixy - 1
          iyp    = ixy
        else
          iyn    = ixy - 1
          iyp    = ixy + 1
        end if
        !
        atrnx(ixy,-1) = atrnx(ixy,-1)                             &
             * (1.+trix(ixy)) * trix(ixn)/(1.+trix(ixn))
        atrnx(ixy, 1) = atrnx(ixy, 1)                             &
             * (1.+trix(ixy)) * trix(ixp)/(1.+trix(ixp))
        atrny(ixy,-1) = atrny(ixy,-1)                             &
             * (1.+triy(ixy)) * triy(iyn)/(1.+triy(iyn))
        atrny(ixy, 1) = atrny(ixy, 1)                             &
             * (1.+triy(ixy)) * triy(iyp)/(1.+triy(iyp))
        !
      end do
      !
    end if
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3utrn ----------------------------------------------------- /
    !/
  end subroutine w3utrn
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate derivatives of a field.
  !>
  !> @details derivatives are calculated in m/m from the longitude/latitude
  !>  grid, central in space for iternal points, one-sided for coastal
  !>  points.
  !>
  !> @param[in]  zz     field to calculate derivatives of.
  !> @param[in]  zunit  units of zz (used for test output).
  !> @param[out] dzzdx  derivative in x-direction (w-e).
  !> @param[out] dzzdy  derivative in y-direction (s-n).
  !>
  !> @author w. e. rogers, nrl
  !> @date   06-dec-2010
  !>
  subroutine w3dzxy( zz, zunit, dzzdx, dzzdy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |         w. e. rogers, nrl         |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-dec-2010 |
    !/                  +-----------------------------------+
    !/
    !/    30-oct-2009 : origination.                        ( version 3.14 )
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     calculate derivatives of a field.
    !
    !  2. method :
    !
    !     derivatives are calculated in m/m from the longitude/latitude
    !     grid, central in space for iternal points, one-sided for
    !     coastal points.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       zz      r.a.  i   field to calculate derivatives of.
    !       zunit   r.a.  i   units of zz (used for test output).
    !       dzzdx   r.a.  o   derivative in x-direction (w-e).
    !       dzzdy   r.a.  o   derivative in y-direction (s-n).
    !       ixp: ix plus 1 (with branch cut incorporated)
    !       iyp, ixm, iym: ditto
    !       ixps: value to use for ixp if ixps is not masked.
    !             (use ix if masked)
    !       iyps, ixms, iyms : ditto
    !       ixtrpl : in case of needing iy+1 for iy=ny, ix needs to be
    !                modified (tripole grid only)
    !       ixtrpls : value to use for ixtrpl if ixtrpls is not masked
    !             (use ix if masked)
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     this routine replaces the functionality of w3ddxy and w3dcxy.
    !     nb: subroutine "w3cgdm" has a similar purpose.
    !     output arrays are always initialized to zero.
    !
    !  8. structure :
    !
    !     ----------------------------------------
    !      1.  preparations
    !        a initialize arrays
    !        b set constants
    !      2.  derivatives in x-direction (w-e).
    !      3.  derivatives in y-direction (s-n).
    !     ----------------------------------------
    !
    !  9. switches :
    !
    !       !/s   enable subroutine tracing.
    !       !/t   enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nx, ny, nsea, mapsta, mapfs, mapfs, &
         dpdx, dpdy, dqdx, dqdy, flagll, iclose,          &
         iclose_none, iclose_smpl, iclose_trpl
    use w3odatmd, only: ndse, iaproc, naperr, naproc
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real, intent(in)        :: zz(nsea)
    character, intent(in)   :: zunit*(*)
    real, intent(out)       :: dzzdx(ny,nx), dzzdy(ny,nx)
    integer                 :: isea, ix, iy, ixp, ixm, iyp, iym
    real                    :: dfac , stx, sty
    integer                 :: ixps,iyps,ixms,iyms,ixtrpl,ixtrpls
    integer                 :: ixstart,ixend
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a initialize arrays
    !
    dzzdx = 0.
    dzzdy = 0.
    !
    ! 1.b set constants
    !
    if ( flagll ) then
      dfac   = 1. / ( dera * radius )
    else
      dfac   = 1.
    end if
    !
    ! 2.  derivatives in x-direction (w-e) and y-direction (s-n) ----- *
    !
    ! 2a. all points previously done in 2a,2b,2c of v4.18 done in 2a now:
    if ( iclose.eq.iclose_none ) then
      ixstart=2
      ixend=nx-1
    else
      ixstart=1
      ixend=nx
    endif
    do iy=2, ny-1
      do ix=ixstart,ixend
        if ( mapsta(iy,ix) .ne. 0 ) then
          stx    = 0.5
          if (ix.eq.nx)then
            ixps=1
          else
            ixps=ix+1
          endif
          if (mapsta(iy,ixps).eq.0) then
            ixp    = ix
            stx    = 1.0
          else
            ixp    = ixps
          end if
          if(ix.eq.1)then
            ixms=nx
          else
            ixms=ix-1
          endif
          if (mapsta(iy,ixms).eq.0) then
            ixm    = ix
            stx    = 1.0
          else
            ixm    = ixms
          end if
          sty    = 0.5
          iyps=iy+1
          if (mapsta(iyps,ix).eq.0) then
            iyp    = iy
            sty    = 1.0
          else
            iyp    = iyps
          end if
          iyms=iy-1
          if (mapsta(iyms,ix).eq.0) then
            iym    = iy
            sty    = 1.0
          else
            iym    = iyms
          end if
          dzzdx(iy,ix) = (zz(mapfs(iy ,ixp))-zz(mapfs(iy ,ixm))) * stx * dpdx(iy,ix) &
               + (zz(mapfs(iyp,ix ))-zz(mapfs(iym,ix ))) * sty * dqdx(iy,ix)
          dzzdy(iy,ix) = (zz(mapfs(iy ,ixp))-zz(mapfs(iy ,ixm))) * stx * dpdy(iy,ix) &
               + (zz(mapfs(iyp,ix ))-zz(mapfs(iym,ix ))) * sty * dqdy(iy,ix)
          dzzdx(iy,ix) = dzzdx(iy,ix) * dfac
          dzzdy(iy,ix) = dzzdy(iy,ix) * dfac
        end if
      end do
    end do
    ! 2b. column iy=ny for tripole case
    ! this is more complex, since for these two points: (iyp,ix) (iym,ix),
    !     not only is the first index different (iyp.ne.iym), but also the
    !     second index is different (ix.ne.ix)!
    if ( iclose.eq.iclose_trpl ) then
      iy=ny
      do ix=1, nx
        if ( mapsta(iy,ix) .ne. 0 ) then
          stx    = 0.5
          if (ix.eq.nx)then
            ixps=1
          else
            ixps=ix+1
          endif
          if (mapsta(iy,ixps).eq.0) then
            ixp    = ix
            stx    = 1.0
          else
            ixp    = ixps
          end if
          if(ix.eq.1)then
            ixms=nx
          else
            ixms=ix-1
          endif
          if (mapsta(iy,ixms).eq.0) then
            ixm    = ix
            stx    = 1.0
          else
            ixm    = ixms
          end if
          sty    = 0.5
          !..............next point: j+1: tripole: j==>j+1==>j and i==>ni-i+1
          !..............i.e. target point is mapfs(iy,(nx-ix+1))
          ixtrpls=nx-ix+1
          if (mapsta(iy,ixtrpls).eq.0) then
            ixtrpl = ix
            sty    = 1.0
          else
            ixtrpl=ixtrpls
          end if
          iyms=iy-1
          if (mapsta(iyms,ix).eq.0) then
            iym    = iy
            sty    = 1.0
          else
            iym    = iyms
          end if
          ! tripole grid: (iyp,ix) is replaced with (iy,ixtrpl)
          dzzdx(iy,ix) = (zz(mapfs(iy ,ixp))-zz(mapfs(iy ,ixm))) * stx * dpdx(iy,ix) &
               + (zz(mapfs(iy,ixtrpl))-zz(mapfs(iym,ix ))) * sty * dqdx(iy,ix)
          dzzdy(iy,ix) = (zz(mapfs(iy ,ixp))-zz(mapfs(iy ,ixm))) * stx * dpdy(iy,ix) &
               + (zz(mapfs(iy,ixtrpl))-zz(mapfs(iym,ix ))) * sty * dqdy(iy,ix)
          dzzdx(iy,ix) = dzzdx(iy,ix) * dfac
          dzzdy(iy,ix) = dzzdy(iy,ix) * dfac
        end if
      end do
    end if ! if ( iclose.eq.iclose_trpl ) then
    !
    ! 3.  test output of fields ------------------------------------------ *
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3dzxy ----------------------------------------------------- /
    !/
  end subroutine w3dzxy
  !/ ------------------------------------------------------------------- /
  !/ end of module w3updtmd -------------------------------------------- /
  !/
end module w3updtmd
