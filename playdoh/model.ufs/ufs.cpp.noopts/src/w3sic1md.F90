!> @file
!> @brief calculate ice source term s_{ice} according to simple methods.
!>
!> @author e. rogers
!> @author s. zieger
!> @date   11-oct-2013
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
!> @brief calculate ice source term s_{ice} according to simple methods.
!>
!> @author e. rogers
!> @author s. zieger
!> @date   11-oct-2013
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3sic1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           e. rogers               |
  !/                  |           s. zieger               |
  !/                  |                        fortran 90 |
  !/                  | last update :         11-oct-2013 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see w3sic1 documentation.
  !/
  !  1. purpose :
  !
  !     calculate ice source term s_{ice} according to simple methods.
  !          exponential decay rate is uniform in frequency, and
  !          specified directly by the user.  this method is, in effect,
  !          not sustantially different from handling sea ice via the
  !          "sub-grid" blocking approach, after improvements by
  !          fabrice ardhuin (in v4.00).
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sic1    subr. public   ice source term.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine documentation.
  !
  !  5. remarks :
  !
  !     reference:rogers, w.e. and m.d. orzech, 2013: implementation and
  !        testing of ice and mud source functions in wavewatch iii(r),
  !        nrl/mr/7320--13-9462, 31pp.
  !        available from http://www7320.nrlssc.navy.mil/pubs.php
  !        direct link:
  !        http://www7320.nrlssc.navy.mil/pubs/2013/rogers2-2013.pdf
  !
  !  6. switches :
  !
  !     see subroutine documentation.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public :: w3sic1
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief s_{ice} source term using 5 parameters read from input files.
  !>
  !> @param[in]  a      action density spectrum (1-d).
  !> @param[in]  depth  local water depth.
  !> @param[in]  cg     group velocities.
  !> @param[in]  ix     grid index.
  !> @param[in]  iy     grid index.
  !> @param[out] s      source term (1-d version).
  !> @param[out] d      diagonal term of derivative (1-d version).
  !>
  !> @author e. rogers
  !> @author s. zieger
  !> @date   11-oct-2013
  !>
  subroutine w3sic1 (a, depth, cg, ix, iy, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           e. rogers               |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-oct-2013 |
    !/                  +-----------------------------------+
    !/
    !/    16-oct-2012 : origination.                        ( version 4.04 )
    !/                                                        (e. rogers)
    !/    09-oct-2013 : w3sic1 subtype=2 outsourced to w3sic2 (s. zieger)
    !/
    !/        fixme   : move field input to w3srce and provide
    !/     (s.zieger)   input parameter to w3sic1 to make the subroutine
    !/                : versatile for point output processors ww3_outp
    !/                  and ww3_ounp.
    !/
    !/    copyright 2009 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !  1. purpose :
    !
    !     s_{ice} source term using 5 parameters read from input files.
    !     these parameters are allowed to vary in space and time.
    !     the parameters control the exponential decay rate k_i
    !     since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !/ ------------------------------------------------------------------- /
    !
    !  2. method :
    !
    !     regarding i/o (general to all sice modules): s_{ice} source term
    !     is calculated using up to 5 parameters read from input files.
    !     these parameters are allowed to vary in space and time.
    !     the parameters control the exponential decay rate k_i
    !     since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !     sea ice affects the wavenumber k of wind-generated ocean waves.
    !     the ice-modified wavenumber can be expressed as a complex number
    !     k = k_r + i*k_i, with the real part k_r representing impact of
    !     the sea ice on the physical wavelength and propagation speeds,
    !     producing something analogous to shoaling and refraction by
    !     bathymetry, whereas the imaginary part of the complex
    !     wavenumber, k_i, is an exponential decay coefficient
    !     k_i(x,y,t,sigma) (depending on location, time and frequency,
    !     respectively), representing wave attenuation, and can be
    !     introduced in a wave model such as ww3 as s_ice/e=-2*cg*k_i,
    !     where s_ice is one of several dissipation mechanisms, along
    !     with whitecapping, for example, s_ds=s_wc+s_ice+⋯. the k_r -
    !     modified by ice would enter the model via the c calculations
    !     on the left-hand side of the governing equation.the fundamentals
    !     are straightforward, e.g. rogers and holland (2009 and
    !     subsequent unpublished work) modified a similar model, swan
    !     (booij et al. 1999) to include the effects of a viscous mud
    !     layer using the same approach (k = k_r + i*k_i) previously.
    !
    !     general approach is analogous to rogers and holland (2009)
    !         approach for mud.
    !     see text near their eq. 1 :
    !       k        = k_r  +  i * k_i
    !       eta(x,t) = real( a * exp( i * ( k * x - sigma * t ) ) )
    !       a        = a0 * exp( -k_i * x )
    !       s / e    = -2 * cg * k_i (see also komen et al. (1994, pg. 170)
    !
    !     following w3sbt1 as a guide, equation 1 of w3sbt1 says:
    !         s = d * e
    !     however, the code of w3sbt1 has
    !         s = d * a
    !     this leads me to believe that the calling routine is
    !         expecting "s/sigma" not "s"
    !     thus we will use d = s/e = -2 * cg * k_i
    !
    !     notes regarding numerics:
    !     experiments with constant k_i values suggest that :
    !       for dx=20.0 km, k_i should not exceed 3.5e-6
    !      (assumes 2.7% hs error in my particular test case is intolerable)
    !       for dx=5.0 km,  k_i should not exceed 2.0e-5
    !       for dx=2.5 km,  k_i should not exceed 5.0e-5
    !       for dx=1.0 km,  k_i should not exceed 2.0e-4
    !       for dx=0.35 km, error is less than 2.1% for all k_i tested
    !       for dx=0.10 km, error is less than 1.3% for all k_i tested
    !     "ground truth" used for this is an exponential decay profile.
    !
    !      for reference, acnfs is 1/12th deg, so delta_latitude=9.25 km.
    !
    !     {put more equations here}
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d)
    !       depth   real  i   local water depth
    !       cg      r.a.  i   group velocities.
    !       ix,iy   i.s.  i   grid indices.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing (!/s switch).
    !      prt2ds    subr. w3arrymd print plot output (!/t1 switch).
    !      outmat    subr. w3arrymd matrix output (!/t2 switch).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    ascii point output post-processor.
    !      w3exnc    subr.   n/a    netcdf point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     if ice parameter 1 is zero, no calculations are made.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpi
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use w3gdatmd, only: nk, nth, nspec, sig, mapwn
    use w3idatmd, only: icep1, icep2, icep3, icep4, icep5, inflags2
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)        :: cg(nk),   a(nspec), depth
    real, intent(out)       :: s(nspec), d(nspec)
    integer, intent(in)     :: ix, iy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ikth, ik
    real                    :: d1d(nk) !in sbt1: d1d was named "cbeta"
    real                    :: icecoef1, icecoef2, icecoef3, &
         icecoef4, icecoef5
    real, allocatable       :: wn_i(:)  ! exponential decay rate for amplitude
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    d        = 0.0
    !
    allocate(wn_i(nk))
    wn_i     = 0.0
    icecoef1 = 0.0
    icecoef2 = 0.0
    icecoef3 = 0.0
    icecoef4 = 0.0
    icecoef5 = 0.0
    !
    if (.not.inflags2(-7))then
      write (ndse,1001) 'ice parameter 1'
      call extcde(2)
    endif
    !
    icecoef1 = icep1(ix,iy)
    !
    ! 1.  no ice --------------------------------------------------------- /
    !
    if ( icecoef1==0. ) then
      d = 0.
      !
      ! 2.  ice ------------------------------------------------------------ /
    else
      !
      ! 2.a set constant(s) and write test output -------------------------- /
      !
      !         (none)
      !
      !
      ! 2.b make calculations ---------------------------------------------- /
      wn_i = icecoef1 ! uniform in k
      do ik=1, nk
        !   sbt1 has: d1d(ik) = factor *  max(0., (cg(ik)*wn(ik)/sig(ik)-0.5) )
        !             recall that d=s/e=-2*cg*k_i
        d1d(ik) = -2. * cg(ik) * wn_i(ik)
      end do
      !
      ! 2.c fill diagional matrix
      !
      do ikth=1, nspec
        d(ikth) = d1d(mapwn(ikth))
      end do
      !
    end if
    !
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3sic1 : '/               &
         '     ',a,' required but not selected'/)
    !
    !/
    !/ end of w3sic1 ----------------------------------------------------- /
    !/
  end subroutine w3sic1
  !/
  !/ end of module w3sic1md -------------------------------------------- /
  !/
end module w3sic1md
