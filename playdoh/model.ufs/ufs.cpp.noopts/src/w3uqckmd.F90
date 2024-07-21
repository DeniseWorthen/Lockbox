!> @file
!> @brief contains module w3uqckmd.
!>
!> @author h. l. tolman  @date 27-may-2014
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
!> @brief portable ultimate quickest schemes.
!>
!> @author h. l. tolman  @date 27-may-2014
!>
module w3uqckmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         27-may-2014 |
  !/                  +-----------------------------------+
  !/
  !/    08-feb-2001 : origination of module. routines     ( version 2.08 )
  !/                  taken out of w3pro2md.ftn
  !/    13-nov-2001 : version with obstacles added.       ( version 2.14 )
  !/    16-oct-2002 : fix par list w3qck3.                ( version 3.00 )
  !/    05-mar-2008 : added nec sxf90 compiler directives.
  !/                  (chris bunney, uk met office)       ( version 3.13 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : fixed a couple of doc lines.        ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    27-may-2014 : added omph switches in w3qck3.      ( version 5.02 )
  !/
  !/    copyright 2009-2014 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     portable ultimate quickest schemes.
  !
  !  2. variables and types :
  !
  !     none.
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3qck1    subr. public   original ultimate quickest scheme.
  !      w3qck2    subr. public   uq scheme for irregular grid.
  !      w3qck3    subr. public   original ultimate quickest with obst.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - strace and !/s irrelevant for running code. the module is
  !       therefore fully portable to any other model.
  !
  !  6. switches :
  !
  !       !/omph  ading omp directves for hybrid paralellization.
  !
  !       !/s     enable subroutine tracing.
  !       !/tn    enable test output.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief preform one-dimensional propagation in a two-dimensional space
  !>  with irregular boundaries and regular grid.
  !>
  !> @details ultimate quickest scheme (see manual).
  !>
  !>  note that the check on monotonous behavior of qcn is performed
  !>  using weights cfac, to avoid the need for if statements.
  !>
  !>  called by:  w3ktp2   propagation in spectral space.
  !>
  !>  this routine can be used independently from wavewatch iii.
  !>
  !>
  !> @param[in]    mx  field dimensions, if grid is 'closed' or circular, mx is the closed dimension.
  !> @param[in]    my  field dimension (see mx)
  !> @param[in]    nx  part of field actually used
  !> @param[in]    ny  part of field actually used
  !> @param[in]    inc     increment in 1-d array corresponding to increment in 2-d space.
  !> @param[in]    mapact  list of active grid points.
  !> @param[in]    nact    size of mapact.
  !> @param[in]    mapbou  map with boundary information (see w3map2).
  !> @param[in]    nb0     counter in mapbou
  !> @param[in]    nb1     counter in mapbou
  !> @param[in]    nb2     counter in mapbou
  !> @param[in]    ndse    error output unit number.
  !> @param[in]    ndst    test output unit number.
  !> @param[inout] cfll    local courant numbers (my,  mx+1)
  !> @param[inout] q       propagated quantity   (my,0:mx+2)
  !> @param[in]    close   flag for closed 'x' dimension.
  !>
  !> @author  h. l. tolman  @date 30-oct-2009
  !>
  subroutine w3qck1 (mx, my, nx, ny, cfll, q, close, inc,         &
       mapact, nact, mapbou, nb0, nb1, nb2,         &
       ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    11-mar-1997 : final fortran 77                    ( version 1.18 )
    !/    15-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    15-feb-2001 : unit numbers added to par list.     ( version 2.08 )
    !/    05-mar-2008 : added nec sxf90 compiler directives.
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    30-oct-2009 : fixed "called by" doc line.         ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     preform one-dimensional propagation in a two-dimensional space
    !     with irregular boundaries and regular grid.
    !
    !  2. method :
    !
    !     ultimate quickest scheme (see manual).
    !
    !     note that the check on monotonous behavior of qcn is performed
    !     using weights cfac, to avoid the need for if statements.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       mx,my   int.   i   field dimensions, if grid is 'closed' or
    !                          circular, mx is the closed dimension.
    !       nx,ny   int.   i   part of field actually used.
    !       cfll    r.a.   i   local courant numbers.         (my,  mx+1)
    !       q       r.a.  i/o  propagated quantity.           (my,0:mx+2)
    !       close   log.   i   flag for closed 'x' dimension'
    !       inc     int.   i   increment in 1-d array corresponding to
    !                          increment in 2-d space.
    !       mapact  i.a.   i   list of active grid points.
    !       nact    int.   i   size of mapact.
    !       mapbou  i.a.   i   map with boundary information (see w3map2).
    !       nbn     int.   i   counters in mapbou.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !           - cfll amd q need only bee filled in the (my,mx) range,
    !             extension is used internally for closure.
    !           - cfll and q are defined as 1-d arrays internally.
    !
    !  4. subroutines used :
    !
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3ktp2   propagation in spectral space
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - this routine can be used independently from wavewatch iii.
    !
    !  8. structure :
    !
    !     ------------------------------------------------------
    !       1. initialize aux. array fla.
    !       2. fluxes for central points (3rd order + limiter).
    !       3. fluxes boundary point above (1st order).
    !       4. fluxes boundary point below (1st order).
    !       5. closure of 'x' if required
    !       6. propagate.
    !     ------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable test output.
    !     !/t0  test output input/output fields.
    !     !/t1  test output fluxes.
    !     !/t2  test output integration.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, nx, ny, inc, mapact(my*mx),  &
         nact, mapbou(my*mx), nb0, nb1, nb2,  &
         ndse, ndst
    real, intent(inout)     :: cfll(my*(mx+1)), q(1-my:my*(mx+2))
    logical, intent(in)     :: close
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ixy, ip, ixyc, ixyu, ixyd, iy, ix,   &
         iad00, iad02, iadn0, iadn1, iadn2
    real                    :: cfl, qb, dq, dqnz, qcn, qbn, qbr, cfac
    real                    :: fla(1-my:my*mx)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    !
    !
    ! 1.  initialize aux. array fla and closure ------------------------- *
    !
    fla = 0.
    !
    if ( close ) then
      iad00  = -my
      iad02  =  my
      iadn0  = iad00 + my*nx
      iadn1  =         my*nx
      iadn2  = iad02 + my*nx
      do iy=1, ny
        q   (iy+iad00) = q   (iy+iadn0)
        q   (iy+iadn1) = q   (   iy   )
        q   (iy+iadn2) = q   (iy+iad02)
        cfll(iy+iadn1) = cfll(   iy   )
      end do
    end if
    !
    ! 2.  fluxes for central points ------------------------------------- *
    !     ( 3rd order + limiter )
    !
    !
    do ip=1, nb0
      !
      ixy    = mapbou(ip)
      cfl    = 0.5 * ( cfll(ixy) + cfll(ixy+inc) )
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      qb     = 0.5 * ( (1.-cfl)*q(ixy+inc) + (1.+cfl)*q(ixy) )      &
           - (1.-cfl**2)/6. * (q(ixyc-inc)-2.*q(ixyc)+q(ixyc+inc))
      !
      ixyu   = ixyc - inc * int ( sign (1.1,cfl) )
      ixyd   = 2*ixyc - ixyu
      dq     = q(ixyd) - q(ixyu)
      dqnz   = sign ( max(1.e-15,abs(dq)) , dq )
      qcn    = ( q(ixyc) - q(ixyu) ) / dqnz
      qcn    = min ( 1.1, max ( -0.1 , qcn ) )
      !
      qbn    = max ( (qb-q(ixyu))/dqnz , qcn )
      qbn    = min ( qbn , 1. , qcn/max(1.e-10,abs(cfl)) )
      qbr    = q(ixyu) + qbn*dq
      cfac   = real ( int( 2. * abs(qcn-0.5) ) )
      qb     = (1.-cfac)*qbr + cfac*q(ixyc)
      !
      fla(ixy) = cfl * qb
      !
      !
    end do
    !
    ! 3.  fluxes for points with boundary above ------------------------- *
    !     ( 1st order without limiter )
    !
    !
    do ip=nb0+1, nb1
      ixy    = mapbou(ip)
      cfl    = cfll(ixy)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      fla(ixy) = cfl * q(ixyc)
    end do
    !
    ! 4.  fluxes for points with boundary below ------------------------- *
    !     ( 1st order without limiter )
    !
    !
    do ip=nb1+1, nb2
      ixy    = mapbou(ip)
      cfl    = cfll(ixy+inc)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      fla(ixy) = cfl * q(ixyc)
    end do
    !
    ! 5.  global closure ----------------------------------------------- *
    !
    if ( close ) then
      do iy=1, ny
        fla (iy+iad00) = fla (iy+iadn0)
      end do
    end if
    !
    ! 6.  propagation -------------------------------------------------- *
    !
    do ip=1, nact
      ixy    = mapact(ip)
      q(ixy) = max ( 0. , q(ixy) + fla(ixy-inc) - fla(ixy) )
    end do
    !
    !
    return
    !
    ! formats
    !
    !
    !
    !/
    !/ end of w3qck1 ----------------------------------------------------- /
    !/
  end subroutine w3qck1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief like w3qck1 with variable grid spacing.
  !>
  !> @details velo amd q need only bee filled in the (my,mx) range,
  !>  extension is used internally for closure.
  !>  velo and q are defined as 1-d arrays internally.
  !>
  !>  called by:  w3ktp2   propagation in spectral space.
  !>
  !>  this routine can be used independently from wavewatch iii.
  !>
  !>
  !> @param[in]    mx      field dimensions, if grid is 'closed' or circular, mx is the closed dimension.
  !> @param[in]    my      field dimension (see mx).
  !> @param[in]    nx      part of field actually used.
  !> @param[in]    ny      part of field actually used.
  !> @param[in]    mapact  list of active grid points.
  !> @param[in]    nact    size of mapact.
  !> @param[in]    mapbou  map with boundary information (see w3map2).
  !> @param[in]    nb0     counter in mapbou.
  !> @param[in]    nb1     counter in mapbou.
  !> @param[in]    nb2     counter in mapbou.
  !> @param[inout] velo    local velocities          (my,  mx+1).
  !> @param[in]    dt      time step.
  !> @param[inout] dx1     band width at points      (my,  mx+1).
  !> @param[inout] dx2     band width between points (my,0:mx+1)
  !> @param[in]    ndse    error output unit number.
  !> @param[in]    ndst    test output unit number.
  !> @param[inout] q       propagated quantity       (my,0:mx+2).
  !> @param[in]    close   flag for closed 'x' dimension.
  !> @param[in]    inc     increment in 1-d array corresponding to
  !>                       increment in 2-d space.
  !>
  !> @author  h. l. tolman  @date 30-oct-2009
  !>
  subroutine w3qck2 (mx, my, nx, ny, velo, dt, dx1, dx2, q, close,&
       inc,  mapact, nact, mapbou, nb0, nb1, nb2,    &
       ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    07-sep-1997 : final fortran 77                    ( version 1.18 )
    !/    16-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    14-feb-2001 : unit numbers added to par list.     ( version 2.08 )
    !/    05-mar-2008 : added nec sxf90 compiler directives.
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    30-oct-2009 : fixed "called by" doc line.         ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     like w3qck1 with variable grid spacing.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       mx,my   int.   i   field dimensions, if grid is 'closed' or
    !                          circular, mx is the closed dimension.
    !       nx,ny   int.   i   part of field actually used.
    !       velo    r.a.   i   local velocities.              (my,  mx+1)
    !       dt      real   i   time step.
    !       dx1     r.a.  i/o  band width at points.          (my,  mx+1)
    !       dx2     r.a.  i/o  band width between points.     (my,0:mx+1)
    !                          (local counter and counter+inc)
    !       q       r.a.  i/o  propagated quantity.           (my,0:mx+2)
    !       close   log.   i   flag for closed 'x' dimension'
    !       inc     int.   i   increment in 1-d array corresponding to
    !                          increment in 2-d space.
    !       mapact  i.a.   i   list of active grid points.
    !       nact    int.   i   size of mapact.
    !       mapbou  i.a.   i   map with boundary information (see w3map2).
    !       nbn     int.   i   counters in mapbou.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !           - velo amd q need only bee filled in the (my,mx) range,
    !             extension is used internally for closure.
    !           - velo and q are defined as 1-d arrays internally.
    !
    !  4. subroutines used :
    !
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3ktp2   propagation in spectral space
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - this routine can be used independently from wavewatch iii.
    !
    !  8. structure :
    !
    !     ------------------------------------------------------
    !       1. initialize aux. array fla.
    !       2. fluxes for central points (3rd order + limiter).
    !       3. fluxes boundary point above (1st order).
    !       4. fluxes boundary point below (1st order).
    !       5. closure of 'x' if required
    !       6. propagate.
    !     ------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable test output.
    !     !/t0  test output input/output fields.
    !     !/t1  test output fluxes.
    !     !/t2  test output integration.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, nx, ny, inc, mapact(my*mx),  &
         nact, mapbou(my*mx), nb0, nb1, nb2,  &
         ndse, ndst
    real, intent(in)        :: dt
    real, intent(inout)     :: velo(my*(mx+1)), dx1(my*(mx+1)),     &
         dx2(1-my:my*(mx+1)), q(1-my:my*(mx+2))
    logical, intent(in)     :: close
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ixy, ip, ixyc, ixyu, ixyd, iy, ix,   &
         iad00, iad02, iadn0, iadn1, iadn2
    real                    :: cfl, vel, qb, dq, dqnz, qcn, qbn,    &
         qbr, cfac, fla(1-my:my*mx)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    !
    !
    ! 1.  initialize aux. array fla and closure ------------------------- *
    !
    fla = 0.
    !
    if ( close ) then
      iad00  = -my
      iad02  =  my
      iadn0  = iad00 + my*nx
      iadn1  =         my*nx
      iadn2  = iad02 + my*nx
      do iy=1, ny
        q   (iy+iad00) = q   (iy+iadn0)
        q   (iy+iadn1) = q   (   iy   )
        q   (iy+iadn2) = q   (iy+iad02)
        velo(iy+iadn1) = velo(   iy   )
        dx1 (iy+iadn1) = dx1 (   iy   )
        dx2 (iy+iad00) = dx1 (iy+iadn0)
        dx2 (iy+iadn1) = dx1 (   iy   )
      end do
    end if
    !
    ! 2.  fluxes for central points ------------------------------------- *
    !     ( 3rd order + limiter )
    !
    !
    do ip=1, nb0
      !
      ixy    = mapbou(ip)
      vel    = 0.5 * ( velo(ixy) + velo(ixy+inc) )
      cfl    = dt *  vel / dx2(ixy)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      qb     = 0.5 * ( (1.-cfl)*q(ixy+inc) + (1.+cfl)*q(ixy) )      &
           - dx2(ixy)**2 / dx1(ixyc) * (1.-cfl**2) / 6.      &
           * ( (q(ixyc+inc)-q(ixyc))/dx2(ixyc)           &
           - (q(ixyc)-q(ixyc-inc))/dx2(ixyc-inc) )
      !
      ixyu   = ixyc - inc * int ( sign (1.1,cfl) )
      ixyd   = 2*ixyc - ixyu
      dq     = q(ixyd) - q(ixyu)
      dqnz   = sign ( max(1.e-15,abs(dq)) , dq )
      qcn    = ( q(ixyc) - q(ixyu) ) / dqnz
      qcn    = min ( 1.1, max ( -0.1 , qcn ) )
      !
      qbn    = max ( (qb-q(ixyu))/dqnz , qcn )
      qbn    = min ( qbn , 1. , qcn/max(1.e-10,abs(cfl)) )
      qbr    = q(ixyu) + qbn*dq
      cfac   = real ( int( 2. * abs(qcn-0.5) ) )
      qb     = (1.-cfac)*qbr + cfac*q(ixyc)
      !
      fla(ixy) = vel * qb
      !
      !
    end do
    !
    ! 3.  fluxes for points with boundary above ------------------------- *
    !     ( 1st order without limiter )
    !
    !
    do ip=nb0+1, nb1
      ixy    = mapbou(ip)
      vel    = velo(ixy)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,vel) ) )
      fla(ixy) = vel * q(ixyc)
    end do
    !
    ! 4.  fluxes for points with boundary below ------------------------- *
    !     ( 1st order without limiter )
    !
    !
    do ip=nb1+1, nb2
      ixy    = mapbou(ip)
      vel    = velo(ixy+inc)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,vel) ) )
      fla(ixy) = vel * q(ixyc)
    end do
    !
    ! 5.  global closure ----------------------------------------------- *
    !
    if ( close ) then
      do iy=1, ny
        fla (iy+iad00) = fla (iy+iadn0)
      end do
    end if
    !
    ! 6.  propagation -------------------------------------------------- *
    !
    do ip=1, nact
      ixy    = mapact(ip)
      q(ixy) = max ( 0. , q(ixy) + dt/dx1(ixy) *                    &
           (fla(ixy-inc)-fla(ixy)) )
    end do
    !
    !
    return
    !
    ! formats
    !
    !
    !
    !/
    !/ end of w3qck2 ----------------------------------------------------- /
    !/
  end subroutine w3qck2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief like w3qck1 with cell transparencies added.
  !>
  !> @details cfll amd q need only bee filled in the (my,mx) range,
  !>  extension is used internally for closure.
  !>  cfll and q are defined as 1-d arrays internally.
  !>
  !>  called by:  w3xyp2   propagation in physical space.
  !>
  !>  this routine can be used independently from wavewatch iii.
  !>
  !>
  !> @param[in]    mx    field dimensions, if grid is 'closed' or circular, mx is the closed dimension.
  !> @param[in]    my    field dimension (see mx)
  !> @param[in]    nx    part of field actually used
  !> @param[in]    ny    part of field actually used
  !> @param[inout] cfll  local courant numbers      (my,  mx+1).
  !> @param[in]    trans
  !> @param[in]    inc     increment in 1-d array corresponding to increment in 2-d space.
  !> @param[in]    mapact  list of active grid points.
  !> @param[in]    nact    size of mapact.
  !> @param[in]    mapbou  map with boundary information (see w3map2).
  !> @param[in]    nb0     counter in mapbou
  !> @param[in]    nb1     counter in mapbou
  !> @param[in]    nb2     counter in mapbou
  !> @param[in]    ndse    error output unit number.
  !> @param[in]    ndst    test output unit number.
  !> @param[inout] q       propagated quantity       (my,0:mx+2)
  !> @param[in]    close   flag for closed 'x' dimension.
  !>
  !> @author  h. l. tolman  @date 30-oct-2009
  !>
  subroutine w3qck3 (mx, my, nx, ny, cfll, trans, q, close,       &
       inc, mapact, nact, mapbou, nb0, nb1, nb2,    &
       ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-may-2014 |
    !/                  +-----------------------------------+
    !/
    !/    13_nov-2001 : origination.                        ( version 2.14 )
    !/    16-oct-2002 : fix intent for trans.               ( version 3.00 )
    !/    05-mar-2008 : added nec sxf90 compiler directives.
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    27-may-2014 : added omph switches in w3qck3.      ( version 5.02 )
    !/
    !  1. purpose :
    !
    !     like w3qck1 with cell transparencies added.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       mx,my   int.   i   field dimensions, if grid is 'closed' or
    !                          circular, mx is the closed dimension.
    !       nx,ny   int.   i   part of field actually used.
    !       cfll    r.a.   i   local courant numbers.         (my,  mx+1)
    !       q       r.a.  i/o  propagated quantity.           (my,0:mx+2)
    !       close   log.   i   flag for closed 'x' dimension'
    !       inc     int.   i   increment in 1-d array corresponding to
    !                          increment in 2-d space.
    !       mapact  i.a.   i   list of active grid points.
    !       nact    int.   i   size of mapact.
    !       mapbou  i.a.   i   map with boundary information (see w3map2).
    !       nbn     int.   i   counters in mapbou.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !           - cfll amd q need only bee filled in the (my,mx) range,
    !             extension is used internally for closure.
    !           - cfll and q are defined as 1-d arrays internally.
    !
    !  4. subroutines used :
    !
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3xyp2   propagation in physical space
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - this routine can be used independently from wavewatch iii.
    !
    !  8. structure :
    !
    !     ------------------------------------------------------
    !       1. initialize aux. array fla.
    !       2. fluxes for central points (3rd order + limiter).
    !       3. fluxes boundary point above (1st order).
    !       4. fluxes boundary point below (1st order).
    !       5. closure of 'x' if required
    !       6. propagate.
    !     ------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/omph  ading omp directves for hybrid paralellization.
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable test output.
    !     !/t0  test output input/output fields.
    !     !/t1  test output fluxes.
    !     !/t2  test output integration.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: mx, my, nx, ny, inc, mapact(my*mx),  &
         nact, mapbou(my*mx), nb0, nb1, nb2,  &
         ndse, ndst
    real, intent(in)        :: trans(my*mx,-1:1)
    real, intent(inout)     :: cfll(my*(mx+1)), q(1-my:my*(mx+2))
    logical, intent(in)     :: close
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ixy, ip, ixyc, ixyu, ixyd, iy, ix,   &
         iad00, iad02, iadn0, iadn1, iadn2,   &
         jn, jp
    real                    :: cfl, qb, dq, dqnz, qcn, qbn, qbr, cfac
    real                    :: fla(1-my:my*mx)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    !
    !
    ! 1.  initialize aux. array fla and closure ------------------------- *
    !
    fla = 0.
    !
    if ( close ) then
      iad00  = -my
      iad02  =  my
      iadn0  = iad00 + my*nx
      iadn1  =         my*nx
      iadn2  = iad02 + my*nx
      !
      !
      do iy=1, ny
        q   (iy+iad00) = q   (iy+iadn0) ! 1 ghost column to left
        q   (iy+iadn1) = q   (   iy   ) ! 1st ghost column to right
        q   (iy+iadn2) = q   (iy+iad02) ! 2nd ghost column to right
        cfll(iy+iadn1) = cfll(   iy   ) ! as for q above, 1st to rt
      end do
      !
      !
    end if
    !
    ! 2.  fluxes for central points ------------------------------------- *
    !     ( 3rd order + limiter )
    !
    !
    !
    do ip=1, nb0
      !
      ixy    = mapbou(ip)
      cfl    = 0.5 * ( cfll(ixy) + cfll(ixy+inc) )
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      qb     = 0.5 * ( (1.-cfl)*q(ixy+inc) + (1.+cfl)*q(ixy) )      &
           - (1.-cfl**2)/6. * (q(ixyc-inc)-2.*q(ixyc)+q(ixyc+inc))
      !
      ixyu   = ixyc - inc * int ( sign (1.1,cfl) )
      ixyd   = 2*ixyc - ixyu
      dq     = q(ixyd) - q(ixyu)
      dqnz   = sign ( max(1.e-15,abs(dq)) , dq )
      qcn    = ( q(ixyc) - q(ixyu) ) / dqnz
      qcn    = min ( 1.1, max ( -0.1 , qcn ) )
      !
      qbn    = max ( (qb-q(ixyu))/dqnz , qcn )
      qbn    = min ( qbn , 1. , qcn/max(1.e-10,abs(cfl)) )
      qbr    = q(ixyu) + qbn*dq
      cfac   = real ( int( 2. * abs(qcn-0.5) ) )
      qb     = (1.-cfac)*qbr + cfac*q(ixyc)
      !
      fla(ixy) = cfl * qb
      !
      !
    end do
    !
    !
    ! 3.  fluxes for points with boundary above ------------------------- *
    !     ( 1st order without limiter )
    !
    !
!!!/omph/!$omp parallel do private (ip, ixy, cfl, ixyc)
!!!
    do ip=nb0+1, nb1
      ixy    = mapbou(ip)
      cfl    = cfll(ixy)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      fla(ixy) = cfl * q(ixyc)
    end do
!!!
!!!/omph/!$omp end parallel do
    !
    ! 4.  fluxes for points with boundary below ------------------------- *
    !     ( 1st order without limiter )
    !
    !
!!!/omph/!$omp parallel do private (ip, ixy, cfl, ixyc)
!!!
    do ip=nb1+1, nb2
      ixy    = mapbou(ip)
      cfl    = cfll(ixy+inc)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      fla(ixy) = cfl * q(ixyc)
    end do
    !
!!!/omph/!$omp end parallel do
    !
    ! 5.  global closure ----------------------------------------------- *
    !
    if ( close ) then
      do iy=1, ny
        fla (iy+iad00) = fla (iy+iadn0)
      end do
    end if
    !
    ! 6.  propagation -------------------------------------------------- *
    !
    !
    do ip=1, nact
      !
      ixy    = mapact(ip)
      if ( fla(ixy-inc) .gt. 0. ) then
        jn    = -1
      else
        jn    =  0
      end if
      if ( fla(ixy    ) .lt. 0. ) then
        jp    =  1
      else
        jp    =  0
      end if
      !
      q(ixy) = max ( 0. , q(ixy) + trans(ixy,jn) * fla(ixy-inc)     &
           - trans(ixy,jp) * fla(ixy) )
    end do
    !
    !
    !
    return
    !
    ! formats
    !
    !
    !
    !/
    !/ end of w3qck3 ----------------------------------------------------- /
    !/
  end subroutine w3qck3
  !/
  !/ end of module w3uqckmd -------------------------------------------- /
  !/
end module w3uqckmd
