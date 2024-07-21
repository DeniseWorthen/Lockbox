!> @file
!> @brief contains module w3swlmd, for swell dissipation source term.
!>
!> @author h. l. tolman  @date 21-nov-2011
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
!> @brief source term module for swell dissipation.
!>
!> @details source term for swell dissipation based on different
!>  physics that can be independently selected from the input
!>  and whitecapping dissipation terms in the model setup.
!>
!> @author h. l. tolman  @date 21-nov-2011
!>
module w3swldmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  +-----------------------------------+
  !/
  !/    21-nov-2011 : origination.                        ( version 4.07 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     source term module for swell dissipation based on different
  !     physics that can be independently selected form the input
  !     and whitecapping dissipation terms in the model setup.
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
  !      w3swl4    subr. public   ardhuin et al (2010+) swell dissipation
  !      w3swl6    subr. public   babanin (2011) swell dissipation
  !
  !      irange    func. private  generate a sequence of integer values
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
  !
  !  6. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public  :: w3swl4, w3swl6
  private :: irange
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief fixme
  !>
  !> @details a, s, d all stored as 1-d arrays with dimension nth*nk
  !>  (column by column).
  !>
  !> @param[in]  a     action density spectrum.
  !> @param[in]  cg    group velocities.
  !> @param[in]  wn    wavenumbers.
  !> @param[in]  dair  air density.
  !> @param[out] s     source term.
  !> @param[out] d     diagonal term of the derivative.
  !>
  !> @author h. l. tolman  @date 13-aug-2021
  !>
  subroutine w3swl4 (a, cg, wn, dair, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-aug-2021 |
    !/                  +-----------------------------------+
    !/
    !/    29-may-2009 : origination (w3srcxmd.ftn)          ( version 3.14 )
    !/    06-jan-2012 : implementation                         (s. zieger)
    !/    13-aug-2021 : consider dair a variable           ( version x.xx )
    !/
    !  1. purpose :
    !
    !     fixme
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      a¹      r.a. i  action density spectrum
    !      cg      r.a. i  group velocities
    !      wn      r.a. i  wavenumbers
    !      dair    r.a. i   air density
    !      s¹      r.a. o  source term
    !      d¹      r.a. o  diagonal term of derivative
    !      ¹ stored as 1-d array with dimension nth*nk (column by column).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      irange    func. w3swldmd
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see comments in source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, dwat
    use w3gdatmd,  only: nk, nth, nspec, sig2, dden, fte, swl6b1
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)  :: a(nspec), cg(nk), wn(nk), dair
    real, intent(out) :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    integer           :: ikn(nk), ith
    real, parameter   :: va = 1.4e-5 ! air kinematic viscosity (used in wam).
    real              :: eb(nk), wn2(nspec), emean
    real              :: fe, aorb, re, recrit, uosig, cdsv
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ikn = irange(1,nspec,nth)
    d   = 0.
    wn2 = 0.
    !
    do ith = 1, nth
      wn2(ikn+(ith-1)) = wn        ! wavenumbers to all directions.
    end do
    !
    eb     = sum(reshape(a,(/ nth,nk /)),1) * dden(1:nk) / cg
    emean  = sum(eb) + (eb(nk) / dden(nk)) * fte
    !
    aorb   = 2.0*sqrt(emean)
    !
    eb     = sum(reshape(a*sig2**2,(/ nth,nk /)),1) * dden(1:nk) / cg
    uosig  = 2.0*sqrt(sum(eb))
    fe     = swl6b1    ! (from namelist)
    !     fe     = 0.001     ! (from namelist)
    !/             0.001 - 0.019 with median value 0.007 (ardhuin et al 2009, babanin 2011)
    cdsv   = 1.2
    !
    recrit = 1.0e5
    re     = 4.0 * uosig * aorb / va
    !
    if (re .gt. recrit) then
      d = -(16.0/grav) * (dair/dwat) * fe * (sig2**2) *uosig
    else
      d = -2.0 * (dair/dwat) * cdsv * wn2 * sqrt(2.0 * va * sig2)
    end if
    !
    s = d * a
    !
    !  write(*,*) ' fe       =',fe
    !  write(*,*) ' hs       =',4.*sqrt(emean)
    !  write(*,*) ' uosig    =',uosig
    !  write(*,*) ' aorb     =',aorb
    !  write(*,*) ' re/recrit=',re/recrit
    !  write(*,*) ' swl4_tot =',sum(sum(reshape(s,(/ nth,nk /)),1)*dden/cg)
    !/
    !/ end of w3swl4 ----------------------------------------------------- /
    !/
  end subroutine w3swl4
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief turbulent dissipation of narrow-banded swell.
  !>
  !>
  !> @details a, s, d all stored as 1-d arrays with dimension nth*nk
  !>  (column by column).
  !>
  !> described in babanin (2011, section 7.5).
  !> babanin 2011: cambridge press, 295-321, 463pp.
  !>
  !> s = d * a
  !>
  !> @param[in]  a     action density spectrum.
  !> @param[in]  cg    group velocities.
  !> @param[in]  wn    wavenumbers.
  !> @param[out] s     source term.
  !> @param[out] d     diagonal term of the derivative.
  !>
  !> @author h. l. tolman  @date 16-feb-2012
  !>
  subroutine w3swl6 (a, cg, wn, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         16-feb-2012 |
    !/                  +-----------------------------------+
    !/
    !/    29-may-2009 : origination (w3srcxmd.ftn)          ( version 3.14 )
    !/    16-feb-2012 : implementation                      ( version 4.07 )
    !/                                                         (s. zieger)
    !/
    !  1. purpose :
    !
    !     turbulent dissipation of narrow-banded swell as described in
    !     babanin (2011, section 7.5).
    !
    !     babanin 2011: cambridge press, 295-321, 463pp.
    !
    !  2. method :
    !
    !     s = d * a
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      a¹      r.a. i  action density spectrum
    !      cg      r.a. i  group velocities
    !      wn      r.a. i  wavenumbers
    !      s¹      r.a. o  source term
    !      d¹      r.a. o  diagonal term of derivative
    !      ¹ stored as 1-d array with dimension nth*nk (column by column).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      irange    func. w3swldmd
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see comments in source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: grav
    use w3gdatmd,  only: nk, nth, nspec, sig, dden, dth
    use w3gdatmd,  only: swl6cstb1, swl6b1, fte, ftwn
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)    :: a(nspec), cg(nk), wn(nk)
    real, intent(out)   :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    integer             :: ik, ith, ikn(nk)
    real, dimension(nk) :: aband, kmax, anar, bn, aorb, ddis
    real                :: k(nth,nk), b1
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/ 0) --- initialize parameters -------------------------------------- /
    ikn   = irange(1,nspec,nth)            ! index vector for array access, e.g.
    !                                            ! in form of wn(1:nk) == wn2(ikn).
    aband = sum(reshape(a,(/ nth,nk /)),1) ! action density as function of wavenumber
    ddis  = 0.
    d     = 0.
    b1    = swl6b1                         ! empirical constant from namelist
    !
    !/ 1) --- choose calculation of steepness a*k ------------------------ /
    !/        replace the measure of steepness with the spectral
    !         saturation after banner et al. (2002) ---------------------- /
    k     = reshape(a,(/ nth,nk /))
    kmax  = maxval(k,1)
    do ik = 1,nk
      if (kmax(ik).lt.1.0e-34) then
        k(1:nth,ik) = 1.
      else
        k(1:nth,ik) = k(1:nth,ik)/kmax(ik)
      end if
    end do
    anar  = 1.0/( sum(k,1) * dth )
    bn    = anar * ( aband * sig(1:nk) * dth ) * wn**3
    !
    if (.not.swl6cstb1) then
      !
      !/    --- a constant value for b1 attenuates swell too strong in the
      !/        western central pacific (i.e. cross swell less than 1.0m).
      !/        workaround is to scale b1 with steepness a*kp, where kp is
      !/        the peak wavenumber. swl6b1 remains a scaling constant, but
      !/        with different magnitude.  --------------------------------- /
      ik    = maxloc(aband,1)         ! index for peak
      !         emean = sum(aband * dden / cg)  ! total sea surface variance
      b1    = swl6b1 * ( 2. * sqrt(sum(aband*dden/cg)) * wn(ik) )
      !
    end if
    !
    !/ 2) --- calculate the derivative term only (in units of 1/s) ------- /
    do ik = 1,nk
      if (aband(ik) .gt. 1.e-30) then
        ddis(ik) = -(2./3.) * b1 * sig(ik) * sqrt(bn(ik))
      end if
    end do
    !
    !/ 3) --- apply dissipation term of derivative to all directions ----- /
    do ith = 1, nth
      d(ikn+(ith-1)) = ddis
    end do
    !
    s = d * a
    !
    !  write(*,*) ' b1       =',b1
    !  write(*,*) ' ddis_tot =',sum(ddis*aband*dden/cg)
    !  write(*,*) ' edens_tot=',sum(aband*dden/cg)
    !  write(*,*) ' edens_tot=',sum(aband*sig*dth*dsii/cg)
    !  write(*,*) ' '
    !  write(*,*) ' swl6_tot =',sum(sum(reshape(s,(/ nth,nk /)),1)*dden/cg)
    !
    !/
    !/ end of w3swl6 ----------------------------------------------------- /
    !/
  end subroutine w3swl6
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief generate a linear-spaced sequence of integer numbers.
  !>
  !> @details used for array addressing (indexing).
  !>
  !> @param   x0
  !> @param   x1
  !> @param   dx
  !> @returns ix
  !>
  !> @author h. l. tolman
  !> @author s. zieger
  !> @date 15-feb-2011
  !>
  function irange(x0,x1,dx) result(ix)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-feb-2011 |
    !/                  +-----------------------------------+
    !/
    !/    15-feb-2011 : origination from w3src6md          ( version 4.07 )
    !/                                                        (s. zieger)
    !/
    !  1. purpose :
    !         generate a linear-spaced sequence of integer
    !         numbers. used for array addressing (indexing).
    !
    !/
    implicit none
    integer, intent(in)  :: x0, x1, dx
    integer, allocatable :: ix(:)
    integer              :: n
    integer              :: i
    !
    n = int(real(x1-x0)/real(dx))+1
    allocate(ix(n))
    do i = 1, n
      ix(i) = x0+ (i-1)*dx
    end do
    !/
  end function irange
  !/ ------------------------------------------------------------------- /
  !/
end module w3swldmd
