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
module w3dispmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    30-nov-1999 : fortran 90 version.                 ( version 2.00 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    10-mar-2016 : added liu & mollo-christensen
  !/                  dispersion with ice (e. rogers)     ( version 5.10 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     a set of routines for solving the dispersion relation.
  !
  !  2. variables and types :
  !
  !     all variables are retated to the interpolation tables. see
  !     distab for a more comprehensive description.
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nar1d     i.p.  public   nmmer of elements in interpolation
  !                               array.
  !      dfac      r.p.  public   value of kh at deep boundary.
  !      ewn1      r.a.  public   wavenumber array.
  !      ecg1      r.a.  public   group velocity array.
  !      n1max     int.  public   actual maximum position in array.
  !      dsie      real  public   si step.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      wavnu1    subr. public   solve dispersion using lookup table.
  !      wavnu2    subr. public   solve dispersion relation itteratively.
  !      distab    subr. public   fill interpolation tables.
  !      liu_forward_dispersion subr. public  dispersion with ice
  !      liu_reverse_dispersion subr. public  dispersion with ice
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing            ( !/s )
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/s   enable subroutine tracing.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
  !/ set up of public interpolation table ------------------------------ /
  !/
  integer, parameter      :: nar1d  =  121
  real, parameter         :: dfac   =    6.
  !/
  integer                 :: n1max
  real                    :: ecg1(0:nar1d), ewn1(0:nar1d), dsie
  !/
  !/ set up of public subroutines -------------------------------------- /
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine wavnu1 (si,h,k,cg)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    04-nov-1990 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     calculate wavenumber and group velocity from the interpolation
    !     array filled by distab from a given intrinsic frequency and the
    !     waterdepth.
    !
    !  2. method :
    !
    !     linear interpolation from one-dimensional array.
    !
    !  3. parameters used :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       si      real   i   intrinsic frequency (moving frame)  (rad/s)
    !       h       real   i   waterdepth                            (m)
    !       k       real   o   wavenumber                          (rad/m)
    !       cg      real   o   group velocity                       (m/s)
    !     ----------------------------------------------------------------
    !
    !  4. error messages :
    !
    !     - none.
    !
    !  5. called by :
    !
    !     - any main program
    !
    !  6. subroutines used :
    !
    !     - none
    !
    !  7. remarks :
    !
    !     - calculated si* is always made positive without checks : check in
    !       main program assumed !
    !     - depth is unlimited.
    !
    !  8. structure :
    !
    !     +---------------------------------------------+
    !     | calculate non-dimensional frequency         |
    !     |---------------------------------------------|
    !     | t            si* in range ?               f |
    !     |----------------------|----------------------|
    !     | calculate k* and cg* | deep water approx.   |
    !     | calculate output     |                      |
    !     |      parameters      |                      |
    !     +---------------------------------------------+
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants, only : grav
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: si, h
    real, intent(out)       :: k, cg
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i1, i2
    real                    :: sqrth, six, r1, r2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    sqrth  = sqrt(h)
    six    = si * sqrth
    i1     = int(six/dsie)
    !
    if (i1.le.n1max.and.i1.ge.1) then
      i2 = i1 + 1
      r1 = six/dsie - real(i1)
      r2 = 1. - r1
      k  = ( r2*ewn1(i1) + r1*ewn1(i2) ) / h
      cg = ( r2*ecg1(i1) + r1*ecg1(i2) ) * sqrth
    else
      k  = si*si/grav
      cg = 0.5 * grav / si
    end if
    !
    return
    !/
    !/ end of wavnu1 ----------------------------------------------------- /
    !/
  end subroutine wavnu1
  !/ ------------------------------------------------------------------- /
  subroutine wavnu2 (w,h,k,cg,eps,nmax,icon)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-nov-1999 |
    !/                  +-----------------------------------+
    !/
    !/    17-jul-1990 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     calculation of wavenumber k from a given angular
    !     frequency w and waterdepth h.
    !
    !  2. method :
    !
    !     used equation :
    !                        2
    !                       w  = g*k*tanh(k*h)
    !
    !     because of the nature of the equation, k is calculated
    !     with an itterative procedure.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       w       real   i   angular frequency
    !       h       real   i   waterdepth
    !       k       real   o   wavenumber ( same sign as w )
    !       cg      real   o   group velocity (same sign as w)
    !       eps     real   i   wanted max. difference between k and kold
    !       nmax    int.   i   max number of repetitions in calculation
    !       icon    int.   o   contol counter ( see error messages )
    !     ----------------------------------------------------------------
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants, only : grav
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: nmax
    integer, intent(out)    :: icon
    real, intent(in)        :: w, h, eps
    real, intent(out)       :: cg, k
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i
    real                    :: f, w0, fd, dif, rdif, kold
    !real                    :: ktest1, cgtest1, ktest2, cgtest2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !     initialisations :
    !
    !call wavnu1(abs(w),h,ktest1,cgtest1)
    !call wavnu3(abs(w),h,ktest2,cgtest2)
    cg   = 0
    kold = 0
    icon = 0
    w0   = abs(w)
    !
    !     1st approach :
    !
    if (w0.lt.sqrt(grav/h)) then
      k = w0/sqrt(grav*h)
    else
      k = w0*w0/grav
    end if
    !
    !     refinement :
    !
    do i=1, nmax
      dif = abs(k-kold)
      if (k.ne.0) then
        rdif = dif/k
      else
        rdif = 0
      end if
      if (dif .lt. eps .and. rdif .lt. eps) then
        icon = 1
        goto 100
      else
        kold = k
        f    = grav*kold*tanh(kold*h)-w0**2
        if (kold*h.gt.25) then
          fd = grav*tanh(kold*h)
        else
          fd = grav*tanh(kold*h) + grav*kold*h/((cosh(kold*h))**2)
        end if
        k    = kold - f/fd
      end if
    end do
    !
    dif   = abs(k-kold)
    rdif  = dif/k
    if (dif .lt. eps .and. rdif .lt. eps) icon = 1
100 continue
    if (2*k*h.gt.25) then
      cg = w0/k * 0.5
    else
      cg = w0/k * 0.5*(1+(2*k*h/sinh(2*k*h)))
    end if
    if (w.lt.0.0) then
      k  = (-1)*k
      cg = cg*(-1)
    end if
    !write(*,'(20f20.10)') w, h, (k-ktest2)/k*100., (cg-cgtest2)/cg*100.
    !
    return
    !/
    !/ end of wavnu2 ----------------------------------------------------- /
    !/
  end subroutine wavnu2
  !/
  pure subroutine wavnu3 (si,h,k,cg)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           aron roland             |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-05-17    |
    !/                  +-----------------------------------+
    !/
    !/    20.05.17 : initial version, aron roland based on wavnu1
    !/
    !  1. purpose :
    !
    !     calculate wavenumber and group velocity from the improved
    !     eckard's formula by beji (2003)
    !
    !  2. method :
    !
    !     direct computation by approximation
    !
    !  3. parameters used :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       si      real   i   intrinsic frequency (moving frame)  (rad/s)
    !       h       real   i   waterdepth                            (m)
    !       k       real   o   wavenumber                          (rad/m)
    !       cg      real   o   group velocity                       (m/s)
    !     ----------------------------------------------------------------
    !
    !  4. error messages :
    !
    !     - none.
    !
    !  5. called by :
    !
    !     - any main program
    !
    !  6. subroutines used :
    !
    !     - none
    !
    !  7. remarks :
    !
    !     - calculated si* is always made positive without checks : check in
    !       main program assumed !
    !     - depth is unlimited.
    !
    !  8. structure :
    !
    !     +---------------------------------------------+
    !     | calculate non-dimensional frequency         |
    !     |---------------------------------------------|
    !     | t            si* in range ?               f |
    !     |----------------------|----------------------|
    !     | calculate k* and cg* | deep water approx.   |
    !     | calculate output     |                      |
    !     |      parameters      |                      |
    !     +---------------------------------------------+
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants, only : grav, pi
    !!/s      use w3servmd, only: strace
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: si, h
    real, intent(out)       :: k, cg
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i1, i2
    !!/s      integer, save           :: ient = 0
    real                    :: kh0, kh, tmp, tp, cp, l
    real, parameter         :: beta1 = 1.55
    real, parameter         :: beta2 = 1.3
    real, parameter         :: beta3 = 0.216
    real, parameter         :: zpi   = 2 * pi
    real, parameter         :: kdmax = 20.
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ! ient does not work with pure subroutines
    !!/s      call strace (ient, 'wavnu1')
    !
    tp  = si/zpi
    kh0 = zpi*zpi*h/grav*tp*tp
    tmp = 1.55 + 1.3*kh0 + 0.216*kh0*kh0
    kh  = kh0 * (1 + kh0**1.09 * 1./exp(min(kdmax,tmp))) / sqrt(tanh(min(kdmax,kh0)))
    k   = kh/h
    cg  = 0.5*(1+(2*kh/sinh(min(kdmax,2*kh))))*si/k
    !
    return
    !/
    !/ end of wavnu3 ----------------------------------------------------- /
    !/
  end subroutine wavnu3
  pure subroutine wavnu_local (sig,dw,wnl,cgl)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           aron roland             |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-05-17    |
    !/                  +-----------------------------------+
    !/
    !/    20.05.17 : initial version, aron roland based on wavnu1
    !/
    !  1. purpose :
    !
    !     calculate wavenumber and group velocity from the improved
    !     eckard's formula by beji (2003)
    !
    !  2. method :
    !
    !     linear interpolation from one-dimensional array.
    !
    !  3. parameters used :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       si      real   i   intrinsic frequency (moving frame)  (rad/s)
    !       h       real   i   waterdepth                            (m)
    !       k       real   o   wavenumber                          (rad/m)
    !       cg      real   o   group velocity                       (m/s)
    !     ----------------------------------------------------------------
    !
    !  4. error messages :
    !
    !     - none.
    !
    !  5. called by :
    !
    !     - any main program
    !
    !  6. subroutines used :
    !
    !     - none
    !
    !  7. remarks :
    !
    !     - calculated si* is always made positive without checks : check in
    !       main program assumed !
    !     - depth is unlimited.
    !
    !  8. structure :
    !
    !     +---------------------------------------------+
    !     | calculate non-dimensional frequency         |
    !     |---------------------------------------------|
    !     | t            si* in range ?               f |
    !     |----------------------|----------------------|
    !     | calculate k* and cg* | deep water approx.   |
    !     | calculate output     |                      |
    !     |      parameters      |                      |
    !     +---------------------------------------------+
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    use w3gdatmd, only: dmin
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: sig, dw
    real, intent(out)       :: wnl, cgl
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    real                    :: depth
    !
    !/
    !/ end of wavnu_local------------------------------------------------- /
    !/
    depth  = max ( dmin , dw)
    !
    call wavnu3(sig,depth,wnl,cgl)
  end subroutine wavnu_local
  !/
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine distab
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-nov-1990 |
    !/                  +-----------------------------------+
    !/
    !/    04-nov-1990 : final fortran 77                    ( version 1.18 )
    !/    30-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !     fill interpolation arrays for the calculation of wave parameters
    !     according to the linear (airy) wave theory given the intrinsic
    !     frequency.
    !
    !  2. method :
    !
    !     for a given set of non-dimensional frequencies the interpolation
    !     arrays with non-dimensional depths and group velocity are filled.
    !     the following non-dimensional parameters are used :
    !
    !       frequency   f*sqrt(h/g) = f*
    !       depth       kh          = k*
    !       group vel.  c/sqrt(gh)  = c*
    !
    !     where k is the wavenumber, h the depth f the intrinsic frequency,
    !     g the acceleration of gravity and c the group velocity.
    !
    !  3. parameters :
    !
    !     see module documentation.
    !
    !  4. error messages :
    !
    !     - none.
    !
    !  5. called by :
    !
    !     - w3grid
    !     - any main program.
    !
    !  6. subroutines used :
    !
    !     - wavnu2 (solve dispersion relation)
    !
    !  7. remarks :
    !
    !     - in the filling of the arrays h = 1. is assumed and the factor
    !       sqrt (g) is moved from the interpolation to the filling
    !       procedure thus :
    !
    !         k* = k
    !
    !         c* = cg/sqrt(g)
    !
    !  8. structure
    !
    !     -----------------------------------
    !       include common block
    !       calculate parameters
    !       fill zero-th position of arrays
    !       fill middle positions of arrays
    !       fill last positions of arrays
    !     -----------------------------------
    !
    !  9. switches :
    !
    !       !/s   enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants, only : grav
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, icon
    real                    :: depth, cg, simax, si, k
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! calculate parameters ----------------------------------------------- *
    !
    n1max  = nar1d - 1
    depth  = 1.
    simax  = sqrt (grav * dfac)
    dsie   = simax / real(n1max)
    !
    ! fill zero-th position of arrays ------------------------------------ *
    !
    ewn1(0) = 0.
    ecg1(0) = sqrt(grav)
    !
    ! fill middle positions of arrays ------------------------------------ *
    !
    do i=1, n1max
      si = real(i)*dsie
      call wavnu2 (si,depth,k,cg,1e-7,15,icon)
      ewn1(i) = k
      ecg1(i) = cg
    end do
    !
    ! fill last positions of arrays -------------------------------------- *
    !
    i      = n1max+1
    si     = real(i)*dsie
    call wavnu2 (si,depth,k,cg,1e-7,15,icon)
    ewn1(i) = k
    ecg1(i) = cg
    !
    return
    !/
    !/ end of distab ----------------------------------------------------- /
    !/
  end subroutine distab
  !/ ------------------------------------------------------------------- /
  !/
  subroutine liu_forward_dispersion (h_ice,visc,h_wdepth,sigma &
       ,k_solution,cg,alpha)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |        w. e. rogers (nrl-ssc)     |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    16-oct-2012 : origination.                        ( version 4.04 )
    !/                                                        (e. rogers)
    !/
    !  1. purpose :
    !
    !     dispersion relation calculation: given frequency, find k
    !     this is for dispersion in ice, so it requires the ice thickness
    !     and viscosity also. (the latter is the "eddy viscosity in the
    !     turbulent boundary layer beneath the ice.").
    !     please note that this is for a continuous ice cover (not broken in floes)
    !
    !     this subroutine also calculates cg and alpha.
    !     alpha is the exponential decay rate of *energy* (not to be
    !     confused with k_i which is the exponential decay rate of
    !     amplitude)
    !
    !     both alpha and k_i are for spatial decay rate, units (1/m)
    !     neither is for temporal decay rate.
    !
    !     references:
    !     n/a here, but see subroutine "liu_reverse_dispersion"
    !
    !  2. method :
    !
    !     newton-raphson.
    !     for actual dispersion relation, see documentation of subroutine
    !     "liu_reverse_dispersion"
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      h_ice      real    i  ice thickness
    !      visc       real    i  eddy viscosity (m2/sec)
    !      h_wdepth   real    i  water depth
    !      sigma      r.a.    i  radian wave frequency
    !      k_solution r.a.    o  wave number
    !      cg         r.a.    o  group velocity
    !      alpha      r.a.    o  exponential decay rate of energy
    !      nk         int.    i  number of frequencies
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name                   | type |  module | description
    !     ----------------------------------------------------------------
    !      liu_reverse_dispersion | subr.| w3sic2md| as name implies.
    !      strace                 | subr.| w3servmd| subroutine tracing.
    !      wavnu1                 | subr.| w3dispmd| wavenumber for waves
    !                                                in open water.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name                   | type |  module | description
    !     ----------------------------------------------------------------
    !      w3sic2                 | subr.| w3sic2md| s_ice source term
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     fails if solution is not found in a given number of iterations
    !
    !  7. remarks :
    !
    !     eventually, k and cg should be used for propagation. this is not
    !     implemented yet. for now, it is only used to calculate the source
    !     term.
    !
    !     for discussion of the eddy viscosity term, see documentation of
    !     subroutine "liu_reverse_dispersion"
    !
    !     this subroutine expects eddy viscosity in units of m2/sec even
    !     though values are given in units of cm2/sec in the liu paper.
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
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpi
    use w3odatmd,  only: ndse
    use w3servmd,  only: extcde
    use w3gdatmd, only: nk, iicehdisp, iiceddisp, iicefdisp, iicehmin
    ! use w3dispmd,  only: wavnu1
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real   , intent(in)  :: h_ice, h_wdepth, sigma(nk)
    real   , intent(in)  :: visc    ! in m2/sec
    real   , intent(out) :: k_solution(nk) ,cg(nk) ,alpha(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    integer            :: ik
    real, parameter    :: ferrormax=1.0e-5  ! maximum acceptable error
    integer, parameter :: n_iter=20  ! number of iterations prior to
    ! failure
    logical            :: get_cg     ! indicates whether to get cg
    ! and alpha
    ! from "liu_reverse_dispersion"
    real :: freq(20)    ! wave frequency at current
    ! iteration
    real  :: kwn(20)     ! wavenumber at current
    ! iteration
    integer            :: iter       ! iteration number
    real               :: dk,df,dfdk ! as name implies
    real               :: fdummy     ! as name implies
    !real               :: sigma      ! 2*pi/t
    real               :: k_open     ! open-water value of k
    real               :: cg_open    ! open-water value of cg
    real               :: fwanted    ! freq. corresponding to sigma
    real               :: ferror     ! max acceptable error after test to avoid crash
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/ 0) --- initialize/allocate variables ------------------------------ /
    do ik = 1, nk
      get_cg  = .false.
      !/t38      write(*,*)'forward in: h_ice,visc,h_wdepth,fwanted = ', &
      !/t38                          h_ice,visc,h_wdepth,fwanted
      fwanted=sigma(ik)/tpi
      ! first guess for k :
      call wavnu1(sigma(ik),h_wdepth,k_open,cg_open)
      !     kwn(1)  = 0.2 ! (old method)
      kwn(1)  =k_open ! new method, mar 10 2014
      !
      !/ 1) ----- iteration loop to find k --------------------------------- /
      iter = 0
      df   = 999.
      if ( (h_ice.lt.iicehdisp).or.(h_wdepth.lt.iiceddisp) ) then
        ferror=iicefdisp*ferrormax
      else
        ferror=ferrormax
      endif
      do while ( abs(df).ge.ferror .and. iter.le.n_iter )
        iter = iter + 1
        ! compute freq for this iteration
        call liu_reverse_dispersion(h_ice,visc,h_wdepth,kwn(iter), &
             get_cg,freq(iter),cg(ik),alpha(ik))
        ! calculate dk
        if (iter == 1)then
          ! we do not have slope yet, so pick a number...
          dk = 0.01
        elseif (iter.eq.n_iter+1) then
          write(ndse,800) n_iter
          call extcde(2)
        else
          ! use slope
          dfdk = (freq(iter)-freq(iter-1)) / (kwn(iter)-kwn(iter-1))
          df   = fwanted - freq(iter)
          !/t38       write(*,*)'iter = ',iter,' ;  k = ',kwn(iter),' ; f = ', &
          !/t38                  freq(iter),' ; df = ',df
          dk   = df / dfdk
        endif
        ! decide on next k to try
        kwn(iter+1) = kwn(iter) + dk
        ! if we end up with a negative k for the next iteration, don't
        !   allow this.
        if(kwn(iter+1) < 0.0)then
          kwn(iter+1) = tpi / 1000.0
        endif
      end do
      !/ 2) -------- finish up. -------------------------------------------- /
      !     success, so return k_solution, and call liu_reverse_dispersion one
      !     last time, to get cg and alpha
      k_solution(ik) = kwn(iter)
      get_cg     = .true.
      call liu_reverse_dispersion(h_ice,visc,h_wdepth,k_solution(ik), &
           get_cg,fdummy,cg(ik),alpha(ik))
    end do
    !
    !
800 format (/' *** wavewatch iii error in '           &
         'w3sic2_liu_forward_dispersion : ' /     &
         '     no solution found after ',i4,' iterations.')
    !/
    !/ end of liu_forward_dispersion ------------------------------------- /
    !/
  end subroutine liu_forward_dispersion
  !/ ------------------------------------------------------------------- /
  !/
  subroutine liu_reverse_dispersion (h_ice,visc,h_wdepth,kwn &
       ,get_cg,freq,cg,alpha)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |        w. e. rogers (nrl-ssc)     |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    12-oct-2012 : origination.                        ( version 4.04 )
    !/                                                        (e. rogers)
    !/
    !  1. purpose :
    !
    !     dispersion relation calculation: given k, find frequency.
    !     this is for dispersion in ice, so it requires the ice thickness
    !     and viscosity also. (the latter is the "eddy viscosity in the
    !     turbulent boundary layer beneath the ice.").
    !
    !     this subroutine also (optionally) calculates cg and alpha.
    !     alpha is the exponential decay rate of *energy* (not to be
    !     confused with k_i which is the exponential decay rate of
    !     amplitude)
    !
    !     both alpha and k_i are for spatial decay rate, units (1/m)
    !     neither is for temporal decay rate.
    !     this calculation is optional for reasons of computational
    !      efficiency (don't calculate if it will not be used). note that
    !      if cg and alpha are not calculated, the value of input viscosity
    !      is irrelevant.
    !
    !     references:
    !       liu et al.    1991: jgr 96 (c3), 4605-4621
    !       liu and mollo 1988: jpo 18       1720-1712
    !
    !  2. method :
    !
    !     in 1991 paper, see equations on page 4606. the key equations are:
    !     sigma2=(grav*k+b*k^5)/((coth(k*h_wdepth))+k*m);
    !     cg=(grav+(5+4*k*m)*(b*k^4))/((2*sigma)*((1+k*m)^2));
    !     alpha=(sqrt(visc)*k*sqrt(sigma))/(cg*sqrt(2)*(1+k*m));
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      h_ice     real    i  ice thickness
    !      visc      real    i  eddy viscosity (if get_cg) (m2/sec)
    !      h_wdepth  real    i  water depth
    !      kwn       real    i  wavenumber
    !      get_cg    logical i  indicates whether to calculate cg and alpha
    !      freq      real    o  frequency
    !      cg        real    o  group velocity (if get_cg)
    !      alpha     real    o  exponential decay rate of energy (if get_cg)
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name                  | type |  module | description
    !     ----------------------------------------------------------------
    !      liu_forward_dispersion| subr.| w3sic2md| as name implies.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     eventually, k and cg should be used for propagation. this is not
    !     implemented yet. for now, it is only used to calculate the source
    !     term.
    !
    !     the eddy viscosity term given by liu is unfortunately highly
    !     variable, and "not a physical parameter", which suggests that it
    !     is difficult to specify in practice. in this paper, we see values
    !     of:
    !     nu= 160.0e-4 m2/sec (brennecke (1921)
    !     nu=  24.0e-4 m2/sec (hunkins 1966)
    !     nu=3450.0e-4 m2/sec (fig 11)
    !     nu=   4.0e-4 m2/sec (fig 12)
    !     nu= 150.0e-4 m2/sec (fig 13)
    !     nu=  54.0e-4 m2/sec (fig 14)
    !     nu= 384.0e-4 m2/sec (fig 15)
    !     nu=1536.0e-4 m2/sec (fig 16)
    !
    !     the paper states: "the only tuning parameter is the turbulent eddy
    !     viscosity, and it is a function of the flow conditions in the
    !     turbulent boundary layer which are determined by the ice
    !     thickness, floe sizes, ice concentration, and wavelength."
    !
    !     another criticism of this source term is that it does not use the
    !     ice concentration in actual calculations. the method appears to
    !     simply rely on concentration being high, "when the ice is highly
    !     compact with high concentration, the flexural waves obey the
    !     dispersion relation (1) as similar waves in a continuous ice
    !     sheet." later, "five of these  cases with high ice conentration
    !     (larger than 60%) in the miz have been selected"
    !
    !     this subroutine expects eddy viscosity in units of m2/sec even
    !     though values are given in units of cm2/sec in the liu paper.
    !
    !     cg used here is correct only for deep water. it is taken from
    !     liu et al. (1991) equation 2. if we want to calculate for finite
    !     depths accurately, we need to use d_sigma/d_k. however, be warned
    !     that this calculation is sensitive to numerical error and so the
    !     (potentially too coarse) computational grid for sigma and k should
    !     *not* be used.
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
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: dwat, tpi, grav
    use w3gdatmd, only: nk
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real   , intent(in)  :: h_ice,h_wdepth,kwn
    real   , intent(in)  :: visc    ! in m2/sec
    logical, intent(in)  :: get_cg
    real   , intent(out) :: freq,cg,alpha
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    real, parameter   :: e = 6.0e+9 ! young's modulus of elasticity
    real, parameter   :: s = 0.3    ! "s", poisson's ratio
    real              :: dice       ! "dice", density of ice
    real              :: b          ! quantifies effect of bending
    ! of ice
    real              :: m          ! quantifies effect of inertia
    ! of ice
    real              :: cothterm   ! temporary variable
    real              :: sigma      ! 2*pi/t
    real              :: kh         ! k*h
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/ 0) --- initialize essential parameters ---------------------------- /
    cg    = 0.
    alpha = 0.
    freq  = 0.
    dice = dwat * 0.9 ! from liu 1991 pg 4606
    !
    !/ 1) --- calculate frequency ---------------------------------------- /
    ! note: liu et al 1991 have "kwn*h_ice" in coth(_) but i believe they
    ! meant to write "kwn*h_wdepth"
    b  = (e * h_ice**3) / (12. * (1. - s**2) * dwat)
    m  = dice * h_ice / dwat
    kh = kwn * h_wdepth
    if ( kh>5.0 ) then
      cothterm = 1.0
    elseif ( kh<1.0e-4 ) then
      cothterm = 1.0 / kh
    else
      cothterm = cosh(kh) / sinh(kh)
    endif
    sigma = sqrt((grav * kwn + b * kwn**5) / (cothterm + kwn * m))
    freq  = sigma/(tpi)
    !/ 2) --- calculate cg and alpha if requested ------------------------ /
    !     note: cg is correct only for deep water
    if (get_cg) then
      cg    = (grav + (5.0+4.0 * kwn * m) * (b * kwn**4)) &
           / (2.0 * sigma * ((1.0 + kwn * m)**2))
      alpha = (sqrt(visc) * kwn * sqrt(sigma)) &
           / (cg * sqrt(2.0) * (1 + kwn * m))
    endif
    !/
    !/ end of liu_reverse_dispersion ------------------------------------- /
    !/
  end subroutine liu_reverse_dispersion
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3dispmd -------------------------------------------- /
  !/
end module w3dispmd
