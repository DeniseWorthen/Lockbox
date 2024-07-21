!> @file
!> @brief contains module w3uno2md, with uno2 scheme.
!>
!> @author jain-guo li  @date 1-jul-2013
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
!> @brief portable uno2 scheme on irregular grid.
!>
!> @author jain-guo li  @date 1-jul-2013
!>
module w3uno2md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           metoffice |
  !/                  |           jian-guo li             |
  !/                  |                        fortran 90 |
  !/                  | last update :         01-jul-2013 |
  !/                  +-----------------------------------+
  !/
  !/    adapted from   wavewatch-iii  w3uqckmd
  !/            for    uno2 advection scheme.
  !/
  !/    18-mar-2008 : origination.                        ( version 3.14 )
  !/    ..-...-...  : .....                               ( version 3.14 )
  !/    19-mar-2008 : last modified by jian-guo           ( version 3.14 )
  !/    01-jul-2013 : put in ncep branch (tolman).        ( version 4.12 )
  !/    08-jan-2018 : added omph switches in w3uno2.      ( version 6.02 )
  !/
  !  1. purpose :
  !
  !     portable uno2 scheme on irregular grid.
  !
  !  2. variables and types :
  !
  !     none.
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3uno2    subr. public   uno2 scheme for irregular grid.
  !      w3uno2r   subr. public   uno2 scheme reduced to regular grid.
  !      w3uno2s   subr. public   uno2 regular grid with subgrid obstruction.
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
  !> @brief uno2 scheme for irregular grid.
  !>
  !> @param[in] mx field dimensions, if grid is 'closed' or circular, mx is the closed dimension.
  !> @param[in] my       field dimensions
  !> @param[in] nx       part of field actually used
  !> @param[in] ny       part of field actually used
  !> @param[inout] velo  local velocities            (my,  mx+1).
  !> @param[in] dt       time step.
  !> @param[inout] dx1   band width at points        (my,  mx+1).
  !> @param[inout] dx2   band width between points   (my,0:mx+1).
  !> @param[inout] q     propagated quantity.
  !> @param[in] bclose   flag for closed 'x' dimension.
  !> @param[in] inc      increment in 1-d array corresponding to increment in 2-d space.
  !> @param[in] mapact   list of active grid points.
  !> @param[in] nact     size of mapact.
  !> @param[in] mapbou   map with boundary information (see w3map2).
  !> @param[in] nb0      counter in mapbou
  !> @param[in] nb1      counter in mapbou
  !> @param[in] nb2      counter in mapbou
  !> @param[in] ndse     error output unit number.
  !> @param[in] ndst     test output unit number.
  !>
  !> @author jain-guo li  @date 1-jul-2013
  !>
  subroutine w3uno2 (mx, my, nx, ny, velo, dt, dx1, dx2, q,bclose,&
       inc,  mapact, nact, mapbou, nb0, nb1, nb2,    &
       ndse, ndst )
    !/
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
    !       bclose  log.   i   flag for closed 'x' dimension'
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
    !       w3xyp2   propagation in physical space
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - this routine can be used independently from wavewatch-iii.
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
    logical, intent(in)     :: bclose
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
    if ( bclose ) then
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
    !     ( 2rd order uno2 scheme )
    !
    !
    do ip=1, nb0
      !
      ixy  = mapbou(ip)
      vel  = 0.5 * ( velo(ixy) + velo(ixy+inc) )
      !  assuming velocity is at cell centre, so face velocity is an average.
      cfl  = dt *  vel
      !  courant number without gradient distance (between ixy and ixy+inc cells)
      ixyc = ixy - inc * int( min ( 0. , sign(1.1,cfl) ) )
      !  central cell index, depending on flow direction.
      !          ixy for positive cfl, ixy+inc for negative cfl
      !  upstream and downstream cell numbers
      ixyd = ixyc + inc * int ( sign (1.1,cfl) )
      !  minimum gradient is derived from the two sides of the central cell
      !
      qb   = q(ixyc)+sign(0.5, q(ixyd)-q(ixyc))*(dx1(ixyc)-abs(cfl)) &
           *min(abs(q(ixyc+inc)-q(ixyc))/dx2(ixyc),         &
           abs(q(ixyc)-q(ixyc-inc))/dx2(ixyc-inc) )
      !
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
      vel    = velo(ixy)
      ixyc   = ixy - inc * int( min ( 0. , sign(1.1,vel) ) )
      fla(ixy) = vel * dt * q(ixyc)
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
      fla(ixy) = vel * dt * q(ixyc)
    end do
    !
    ! 5.  global closure ----------------------------------------------- *
    !
    if ( bclose ) then
      do iy=1, ny
        fla (iy+iad00) = fla (iy+iadn0)
      end do
    end if
    !
    ! 6.  propagation -------------------------------------------------- *
    !
    do ip=1, nact
      ixy    = mapact(ip)
      ! li    update transported quantity with fluxes
      q(ixy) = max( 0., q(ixy)+( fla(ixy-inc)-fla(ixy) )/dx1(ixy) )
      ! li    this positive filter is not necessary for uno2 scheme but kept here.
    end do
    !
    !
    return
    !
    ! formats
    !
    !
    !
  end subroutine w3uno2
  !/
  !/ end of w3uno2 ----------------------------------------------------- /
  !>
  !> @brief preform one-dimensional propagation in a two-dimensional space
  !>  with irregular boundaries and regular grid.
  !>
  !> @param[in] mx field dimensions, if grid is 'closed' or circular, mx is the closed dimension.
  !> @param[in] my       field dimensions
  !> @param[in] nx       part of field actually used
  !> @param[in] ny       part of field actually used
  !> @param[inout] cfll  local courant numbers         (my,  mx+1).
  !> @param[inout] q     propagated quantity           (my,0:mx+2).
  !> @param[in] bclose   flag for closed 'x' dimension.
  !> @param[in] inc      increment in 1-d array corresponding to increment in 2-d space.
  !> @param[in] mapact   list of active grid points.
  !> @param[in] nact     size of mapact.
  !> @param[in] mapbou   map with boundary information (see w3map2).
  !> @param[in] nb0      counter in mapbou
  !> @param[in] nb1      counter in mapbou
  !> @param[in] nb2      counter in mapbou
  !> @param[in] ndse     error output unit number.
  !> @param[in] ndst     test output unit number.
  !>
  !> @author jain-guo li  @date 8-jan-2018
  !>
  subroutine w3uno2r (mx, my, nx, ny, cfll, q, bclose, inc,       &
       mapact, nact, mapbou, nb0, nb1, nb2,         &
       ndse, ndst )
    !/
    !/    adapted from w3qck1 for uno2 regular grid scheme.
    !/            first created:    19 mar 2008   jian-guo li
    !/            last modified:     8 jan 2018   jian-guo li
    !/
    !  1. purpose :
    !
    !     preform one-dimensional propagation in a two-dimensional space
    !     with irregular boundaries and regular grid.
    !
    !  2. method :
    !
    !     uno2 regular grid scheme
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
    !       bclose  log.   i   flag for closed 'x' dimension'
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
    !     - this routine can be used independently from wavewatch-iii.
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
    logical, intent(in)     :: bclose
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
    if ( bclose ) then
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
      ixy  = mapbou(ip)
      cfl  = 0.5 * ( cfll(ixy) + cfll(ixy+inc) )
      ixyc = ixy  - inc * int( min ( 0. , sign(1.1,cfl) ) )
      ixyd = ixyc + inc * int( sign (1.1,cfl) )
      qb   = q(ixyc)+sign(0.5, q(ixyd)-q(ixyc))*(1.0-abs(cfl)) &
           *min(abs(q(ixyc+inc)-q(ixyc)),             &
           abs(q(ixyc)-q(ixyc-inc)) )
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
    if ( bclose ) then
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
  end subroutine w3uno2r
  !/
  !/ end of w3uno2r ---------------------------------------------------- /
  !/
  !/
  !>
  !> @brief like w3uno2r with cell transparencies added.
  !>
  !> @details adapted from w3qck3 for uno2 regular grid scheme with subgrid obstruction.
  !>
  !> @param[in] mx field dimensions, if grid is 'closed' or circular, mx is the closed dimension.
  !> @param[in] my       field dimensions
  !> @param[in] nx       part of field actually used
  !> @param[in] ny       part of field actually used
  !> @param[in] trans
  !> @param[inout] cfll  local courant numbers         (my,  mx+1).
  !> @param[inout] q     propagated quantity           (my,0:mx+2).
  !> @param[in] bclose   flag for closed 'x' dimension.
  !> @param[in] inc      increment in 1-d array corresponding to increment in 2-d space.
  !> @param[in] mapact   list of active grid points.
  !> @param[in] nact     size of mapact.
  !> @param[in] mapbou   map with boundary information (see w3map2).
  !> @param[in] nb0      counter in mapbou
  !> @param[in] nb1      counter in mapbou
  !> @param[in] nb2      counter in mapbou
  !> @param[in] ndse     error output unit number.
  !> @param[in] ndst     test output unit number.
  !>
  !> @author jain-guo li  @date 8-jan-2018
  !>
  subroutine w3uno2s (mx, my, nx, ny, cfll, trans, q, bclose,     &
       inc, mapact, nact, mapbou, nb0, nb1, nb2,    &
       ndse, ndst )
    !/
    !/
    !/    adapted from w3qck3 for uno2 regular grid scheme with
    !/            subgrid obstruction.
    !/            first created:    19 mar 2008   jian-guo li
    !/            last modified:     8 jan 2018   jian-guo li
    !/
    !  1. purpose :
    !
    !     like w3uno2r with cell transparencies added.
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
    !       bclose  log.   i   flag for closed 'x' dimension'
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
    !     - this routine can be used independently from wavewatch-iii.
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
    logical, intent(in)     :: bclose
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
    if ( bclose ) then
      iad00  = -my
      iad02  =  my
      iadn0  = iad00 + my*nx
      iadn1  =         my*nx
      iadn2  = iad02 + my*nx
      !
      !
      do iy=1, ny
        q   (iy+iad00) = q   (iy+iadn0)
        q   (iy+iadn1) = q   (   iy   )
        q   (iy+iadn2) = q   (iy+iad02)
        cfll(iy+iadn1) = cfll(   iy   )
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
      ixy  = mapbou(ip)
      cfl  = 0.5 * ( cfll(ixy) + cfll(ixy+inc) )
      ixyc = ixy  - inc * int( min ( 0. , sign(1.1,cfl) ) )
      ixyd = ixyc + inc * int( sign (1.1,cfl) )
      qb   = q(ixyc)+sign(0.5, q(ixyd)-q(ixyc))*(1.0-abs(cfl)) &
           *min(abs(q(ixyc+inc)-q(ixyc)),             &
           abs(q(ixyc)-q(ixyc-inc)) )
      !
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
    if ( bclose ) then
      do iy=1, ny
        fla (iy+iad00) = fla (iy+iadn0)
      end do
    end if
    !
    ! 6.  propagation -------------------------------------------------- *
    !
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
    !/ end of w3uno2s ---------------------------------------------------- /
    !/
  end subroutine w3uno2s
  !/
  !/ end of module w3uno2md -------------------------------------------- /
  !/
end module w3uno2md
!/
