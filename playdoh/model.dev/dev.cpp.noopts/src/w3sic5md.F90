!> @file
!> @brief calculate ice source term s_{ice} according to different ice
!>  models.
!>
!> @author q. liu
!> @author e. rogers
!> @date   19-may-2021
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
!> @brief calculate ice source term s_{ice} according to different ice
!>     models:
!>
!> @author q. liu
!> @author e. rogers
!> @date   19-may-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3sic5md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           q. liu                  |
  !/                  |           e. rogers               |
  !/                  |                        fortran 90 |
  !/                  | last update :         19-may-2021 |
  !/                  +-----------------------------------+
  !/
  !/    15-mar-2016 : origination.                        ( version 5.10 )
  !/                                                      ( q. liu )
  !/    15-mar-2016 : started from w3sic1/2/3/4 module    ( q. liu )
  !/
  !/    24-apr-2017 : adding more filters                 ( q. liu )
  !/
  !/    29-apr-2017 : introducing cmplx_tanh2             ( q. liu )
  !/
  !/    02-jun-2017 : update to version 5.16              ( q. liu )
  !/
  !/    17-jun-2017 : remove some unnecessary lines       ( q. liu )
  !/                 (cg_ice, detla function, complx_tanh etc.)
  !/
  !/    20-aug-2018 : ready to be merged to master (v6.06)( q. liu)
  !/
  !/    19-may-2021 : incl. the rp and m2 model           ( q. liu)
  !/
  !/ 1. purpose :
  !     calculate ice source term s_{ice} according to different ice
  !     models:
  !     * 'fs': the viscoelastic, extended fox and squire sea ice model
  !             (mosig et al. 2015)
  !     * 'rp': the viscoelastic, robinson and palmer model (mosig et al.
  !             2015)
  !     * 'm2': the order 3 power law model proposed by meylan et al.
  !             (2018)
  !
  !     reference:
  !     mosig, j.e.m., f. montiel, and v. a. squire (2015):
  !     comparison of viscoelastic-type models for ocean wave attenuation
  !     in ice-covered seas, j. geophys. res. oceans, 120, 6072–6090,
  !     doi:10.1002/2015jc010881.
  !
  !     meylan, m.h., l. bennetts, j. mosig, w. rogers, m. doble, and
  !     m. peter (2018): dispersion relations, power laws, and energy loss
  !     for waves in the marginal ice zone. j. geophys. res. oceans, 123,
  !     3322–3335, https://doi.org/10.1002/2018jc013776.
  !
  !     liu, q., w. e. rogers, a. babanin, j. li, and c. guan (2020):
  !     spectral modeling of ice-induced wave decay. j. phys. oceanogr.,
  !     50 (6), 1583–1604.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      ksp       int.  private  the kind parameter for single precision
  !                               real variables
  !      kdp       int.  private  same as ksp but for double precision
  !      kspc      int.  private  the kind parameter for single precision
  !                               complex variables
  !      kdpc      int.  private  same as kspc but for double precision
  !      errtol    real  private  a real parameter used for "==" test
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sic5    subr. public   ice source term
  !      w3ic5wncg subr. public   wavenumber and group velocity of ice-
  !                               coupled waves
  !      fsdisp    subr. public   solving the ice-coupled wave dispersion
  !      balancing_matrix
  !                subr. private  balancing the matrix before we try to
  !                               find its eigenvalues
  !      eig_hqr   subr. private  qr algorithm for real hessenberg matrix
  !                               (eigenvalues-finding algorithm)
  !      polyroots subr. private  finding roots of a general polynomial
  !      nr_corr   func. private  get the newton-raphson correction term
  !                               for iteration
  !      nr_root   func. private  newton-raphson algorithm for solving
  !                               the ice-coupled wave dispersion
  !      cmplx_sinh, cmplx_cosh, cmplx_tanh2
  !                func. private  sinh, cosh, tanh for complex inputs
  !      init_random_seed
  !                subr. private  initialize the random seed based on
  !                               the system's time
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !      see subroutine documentation
  !
  !  5. remarks :
  !
  !  6. switches :
  !      see subroutine documentation
  !
  !  7. source code:
  !/
  !/ ------------------------------------------------------------------- /
  implicit none
  !/
  public  :: w3sic5, w3ic5wncg, fsdisp
  private :: balancing_matrix, eig_hqr, polyroots
  private :: nr_corr, nr_root
  private :: cmplx_sinh, cmplx_cosh, cmplx_tanh2
  private :: init_random_seed
  !/
  private :: ksp, kdp, kspc, kdpc, errtol
  !/ ------------------------------------------------------------------- /
  !/ parameter list
  !     kind for single- and double-precision real type
  integer, parameter :: ksp    = kind(1.0)
  integer, parameter :: kdp    = kind(1.0d0)
  !
  !     kind for single- and double-precision complex type
  integer, parameter :: kspc   = kind((1.0, 1.0))
  integer, parameter :: kdpc   = kind((1.0d0, 1.0d0))
  real, parameter    :: errtol = 1.e-12
  !/
contains
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief calculate ice source term s_{ice} according to 3 different sea ice
  !>  models.
  !>
  !> @details (mosig et al. 2015, meylan et al. 2018, liu et al. 2020).
  !>
  !> @param[in]  a      action density spectrum (1-d).
  !> @param[in]  depth  local water depth.
  !> @param[in]  cg     group velocities.
  !> @param[in]  wn     wavenumbers.
  !> @param[in]  ix     grid index.
  !> @param[in]  iy     grid index.
  !> @param[out] s      source term (1-d version).
  !> @param[out] d      diagonal term of derivative (1-d version).
  !>
  !> @author q. liu
  !> @author e. rogers
  !> @date   19-may-2021
  !>
  subroutine w3sic5 (a, depth, cg, wn, ix, iy, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |           e. rogers               |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-may-2021 |
    !/                  +-----------------------------------+
    !/
    !/    23-mar-2016 : origination                         ( version 5.10 )
    !/                                                      ( q. liu)
    !/    23-mar-2016 : started from w3sic1/2/3/4 subr.     ( q. liu)
    !/    05-apr-2016 : options for cg_{ice} or cg          ( q. liu)
    !/    25-apr-2017 : add more filters                    ( q. liu)
    !/    20-aug-2018 : ready to be merged to master (v6.06)( q. liu)
    !/
    !/    copyright 2009 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !/ 1. purpose :
    !     calculate ice source term s_{ice} according to 3 different sea ice
    !     models (mosig et al. 2015, meylan et al. 2018, liu et al. 2020)
    !
    !  2. method :
    !     regarding i/o (general to all sice modules): s_{ice} source term
    !     is calculated using up to 5 parameters read from input files.
    !     these parameters are allowed to vary in space and time.
    !     the parameters control the exponential decay rate k_i.
    !     since there are 5 parameters, this permits description of
    !     dependence of k_i on frequency or wavenumber.
    !
    !     sea ice affects the wavenumber k of wind-generated ocean waves.
    !     the ice-modified wavenumber can be expressed as a complex number
    !     k = k_r + i * k_i, with the real part k_r representing impact of
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
    !        (see also documentation of w3sic1)
    !
    !     notes regarding numerics:
    !     -------------------------
    !     experiments with constant k_i values suggest that results may be
    !        dependent on resolution if insufficient resolution is used.
    !        for detailed information, see documentation of w3sic1.
    !
    !     note regarding applicability/validity:
    !     --------------------------------------
    !     similar to the wang and shen model used in w3sic3md, the 3 models
    !     used here are empirical medium models as well which treat the sea
    !     ice cover as a continuum and use 1/2 empirical rheological para-
    !     meters, i.e., the effective shear modulus of ice g and the effec-
    !     tive viscosity η to characterize sea ices of various type. please
    !     see the documentation of w3sic3md for a detailed discussion of
    !     this kind of model.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       depth   real  i   local water depth.
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers
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
    !      prt2ds    subr. w3arrymd print plot output (!/t0 switch).
    !      outmat    subr. w3arrymd matrix output (!/t1 switch).
    !      w3ic5wncg subr. /        wavenumber and group velocity of ice-
    !                               coupled waves
    !     ----------------------------------------------------------------
    !      * / means this module
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
    !/ ------------------------------------------------------------------- /
    !/
    !/
    use constants, only: tpi
    use w3servmd,  only: extcde
    use w3odatmd,  only: ndse, iaproc, naproc, naperr
    use w3gdatmd,  only: nk, nth, nspec, sig, mapwn, ic5pars
    use w3idatmd,  only: inflags2, icep1, icep2, icep3, icep4, icei
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: cg(nk), wn(nk), a(nspec), depth
    real, intent(out)       :: s(nspec), d(nspec)
    integer, intent(in)     :: ix, iy
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    real                    :: icecoef1, icecoef2, icecoef3, &
         icecoef4, iceconc
    real, dimension(nk)     :: d1d, wn_r, wn_i
    !     real                    :: twn_r, twn_i
    integer                 :: ik, ikth
    logical                 :: noice
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ /
    d        = 0.
    d1d      = 0.
    wn_r     = 0.
    wn_i     = 0.
    !
    icecoef1 = 0.
    icecoef2 = 0.
    icecoef3 = 0.
    icecoef4 = 0.
    iceconc  = 0.
    !
    ! set the ice parameters from input
    if (inflags2(-7)) then
      icecoef1 = icep1(ix, iy) ! ice thickness h_i
    else
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 1 (hice)'
      call extcde(2)
    endif
    !
    if (inflags2(-6)) then
      icecoef2 = icep2(ix, iy) ! effective viscosity of ice η
    else
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 2 (visc)'
      call extcde(2)
    endif
    !
    if (inflags2(-5)) then
      icecoef3 = icep3(ix, iy) ! density of ice ρ_i
    else
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 3 (dens)'
      call extcde(2)
    endif
    !
    if (inflags2(-4)) then
      icecoef4 = icep4(ix, iy) ! effective shear modulus of ice g
    else
      if ( iaproc .eq. naperr )                       &
           write (ndse,1001) 'ice parameter 4 (elas)'
      call extcde(2)
    endif
    !
    if (inflags2(4)) iceconc = icei(ix, iy) ! ice concentration
    !
    ! 1. no ice --------------------------------------------------------- /
    noice = .false.
    !     zero ice thickness
    !     very small ice thickness may cause problems in polyroots because
    !     the first coefficient c1 may be very close to zero. so we regard
    !     cases where hice is less than 0.0001 as no ice.
    !     if (icecoef1 < errtol) noice = .true.
    if (icecoef1 < 0.0001) noice = .true.
    !     zero ice concentration
    if (inflags2(4) .and. iceconc < errtol) noice = .true.
    !
    ! calculate the decay rate k_i
    if ( noice ) then
      d1d = 0.
      !
      ! 2. ice ------------------------------------------------------------- /
    else
      !         w3ic5wncg(wn_r, wn_i, cg, hice, ivisc, rhoi, ismodg, hwat)
      call w3ic5wncg(wn_r, wn_i, cg, icecoef1, icecoef2, &
           icecoef3, icecoef4, depth)
      ! recall that d=s/e=-2*cg_{ice}*k_i
      ! in some cases, the fs model yields very large cg_{ice}, which
      ! subquently may result in numerical failure due to the violation of cfl
      ! conditions, therefore we still use ice-free group velocity to advect
      ! wave packets.
      !
      do ik = 1, nk
        d1d(ik) = -2.0 * cg(ik) * wn_i(ik)
      end do
    end if
    !
    ! 2.1 fill diagonal matrix
    do ikth = 1, nspec
      d(ikth) = d1d(mapwn(ikth))
    end do
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    ! formats
    !
1001 format(/' *** wavewatch iii error in w3sic5md : '/   &
         '     ',a,' is not defined in ww3_shel.inp.')
    !/
    !/ end of w3sic5------------------------------------------------------ /
    !/
  end subroutine w3sic5
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief calculation of complex wavenumber arrays for ice-coupled waves.
  !>
  !> @details using the  fox-squire dispersion relations to get (kr, ki) and
  !>  then get cg by cg = dσ / dk (here dk uses kr).
  !>
  !> @param[inout] wn_r    the real part of the wave number.
  !> @param[inout] wn_i    the imaginary part of the wave number.
  !> @param[inout] cg      group velocity (m s^{-1}).
  !> @param[inout] hice    thickness of ice (m).
  !> @param[inout] ivisc   viscosity parameter of ice (m^2 s^{-1}).
  !> @param[inout] rhoi    the density of ice (kg m^{-3}).
  !> @param[inout] ismodg  effecitive shear modulus g of ice (pa).
  !> @param[inout] hwat    water depth.
  !>
  !> @author q. liu
  !> @author e. rogers
  !> @date   25-apr-2017
  !>
  subroutine w3ic5wncg(wn_r, wn_i, cg, hice, ivisc, rhoi, ismodg, &
       hwat)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |           e. rogers               |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-apr-2017 |
    !/                  +-----------------------------------+
    !/
    !/    17-apr-2016 : origination                         ( version 5.10)
    !/                                                      ( q. liu )
    !/    17-apr-2016 : start from w3ic3wncg_cheng          ( q. liu )
    !/
    !/ 1. purpose:
    !     calculation of complex wavenumber arrays for ice-coupled waves.
    !
    !     this also allows us to use cg_ice in the advection part of the
    !     radiative transfer energy equation (rte). --- abandoned in the end
    !
    !  2. method:
    !     using the  fox-squire dispersion relations to get (kr, ki) and
    !     then get cg by cg = dσ / dk (here dk uses kr)
    !
    !  3. parameters:
    !
    !     parameter list:
    !     ----------------------------------------------------------------
    !     name     type   intent   description
    !     ----------------------------------------------------------------
    !     wn_r     r.a.   i/o      the real. part of the wave number
    !     wn_i     r.a.   i/o      the imag. part of the wave number
    !     cg       r.a.   i        group velocity (m s^{-1})
    !     hice     real.  i        thickness of ice (m)
    !     ivisc    real.  i        viscosity parameter of ice (m^2 s^{-1})
    !     rhoi     real.  i        the density of ice (kg m^{-3})
    !     ismodg   real.  i        effecitive shear modulus g of ice (pa)
    !     hwat     real.  i        water depth
    !     ----------------------------------------------------------------
    !     * the intent of wn_r/i must be inout
    !     * cg is unchanged but still kept here because some legacy reasons.
    !
    !  4. subroutines used:
    !
    !     name      type  module   description
    !     ----------------------------------------------------------------
    !     fsdisp    subr. /        dispersion relations for ice-coupled waves
    !     cginice5  subr. /        group velocity for given (σ, kr) array
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     name      type  module   description
    !     ----------------------------------------------------------------
    !     w3sic5    subr. public   ice source term
    !     w3wave    subr. w3wavemd ww3 integration
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
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpi
    use w3gdatmd,  only: nk, sig
    use w3odatmd,  only: ndse, iaproc, naproc, naperr
    use w3servmd,  only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(inout)   :: wn_r(:), wn_i(:)
    real, intent(in)      :: cg(:)
    real, intent(in)      :: hice, ivisc, rhoi, ismodg, hwat
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    real, allocatable     :: sigma(:)
    integer               :: kl, ku, ik
    real                  :: twn_r, twn_i
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    ! initialize sigma {in w3gdatmd: sig (0: nk+1)}
    if (allocated(sigma)) deallocate(sigma); allocate(sigma(size(cg)))
    sigma = 0.
    if (size(wn_r, 1) .eq. nk) then
      kl    = 1
      ku    = nk
      sigma = sig(1:nk)
    else if (size(wn_r,1) .eq. nk+2) then
      kl    = 1
      ku    = nk+2
      sigma = sig(0:nk+1)
    else
      if ( iaproc .eq. naperr ) write(ndse,900) 'w3ic5wncg'
      call extcde(3)
    end if
    !
    ! fox-squire dispersion
    do ik = kl, ku
      !         fsdisp(hice, ivisc, rhoi, ismodg, hwat, wt, wnr, wni)
      call fsdisp(hice, ivisc, rhoi, ismodg, hwat, tpi/sigma(ik), &
           twn_r, twn_i)
      wn_r(ik) = twn_r
      wn_i(ik) = twn_i
    end do
    !
    deallocate(sigma)
    !
900 format(/' *** wavewatch iii error in w3sic5md : '/   &
         '     subr. ', a, ': cannot determine bounds of&
         & wavenumber array.'/)
    !/
    !/ end of w3ic5wncg -------------------------------------------------- /
    !/
  end subroutine w3ic5wncg
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief calculate the complex wavenumber for waves in ice according to
  !>  three different sea ice models.
  !>
  !> @param[in]  hice     thickness of ice (m).
  !> @param[in]  ivisc    viscosity parameter of ice (m^2 s^{-1}).
  !> @param[in]  rhoi     the density of ice (kg m^{-3}).
  !> @param[in]  ismodg   effecitive shear modulus g of ice (pa).
  !> @param[in]  hwat     water depth.
  !> @param[in]  wt       wave period (s; 1/freq).
  !> @param[out] wnr      the real part of the wave number.
  !> @param[out] wni      the imaginary part of the wave number.
  !>
  !> @author q. liu
  !> @date   19-may-2021
  !>
  subroutine fsdisp(hice, ivisc, rhoi, ismodg, hwat, wt, wnr, wni)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-may-2021 |
    !/                  +-----------------------------------+
    !/
    !/    17-mar-2016 : origination                         ( version 5.10)
    !/                                                      ( q. liu)
    !/    17-mar-2016 : start from the matlab code `foxsquire.m` (provided
    !/                  by prof. vernon squire from university of otago)
    !/                                                      ( q. liu)
    !/    25-apr-2017 : add more filters                    ( q. liu)
    !/
    !/    19-may-2021 : incl. rp and m2 ice models          ( q. liu)
    !/
    !  1. purpose :
    !
    !     calculate the complex wavenumber for waves in ice according to
    !     three different sea ice models, i.e., fs, rp and m2 (see liu et
    !     al. 2020)
    !
    !  2. method :
    !     mainly solving the dispersion relations of fs and rp models (
    !     eqs. (20, 24, 25)) in mosig et al. (2015))
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type   intent   description
    !     ----------------------------------------------------------------
    !     hice     real.  in       thickness of ice (m)
    !     ivisc    real.  in       viscosity parameter of ice (m^2 s^{-1})
    !     rhoi     real.  in       the density of ice (kg m^{-3})
    !     ismodg   real.  in       effecitive shear modulus g of ice (pa)
    !     hwat     real.  in       water depth
    !     wt       real.  in       wave period (s; 1/freq)
    !     wnr      real.  out      the real. part of the wave number
    !     wni      real.  out      the imag. part of the wave number
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      polyroots subr. /        find the roots of a general polynomial
    !      nr_root   func. /        newton-raphson root finding
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3ic5wncg subr. /        wavenumber and group velocity of ice-
    !                               coupled waves
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see format 1000, 1001, 1002
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants, only: grav, tpi
    use w3dispmd,  only: wavnu1
    use w3servmd,  only: extcde
    use w3odatmd,  only: ndse, iaproc, naproc, naperr
    use w3gdatmd,  only: ic5pars
    use w3gsrumd,  only: w3inan
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)      :: hice, ivisc, rhoi, ismodg, hwat, wt
    real, intent(out)     :: wnr, wni
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !
    real                  :: ic5minig, ic5minwt, ic5maxkratio, &
         ic5maxki, ic5minhw, ic5vemod
    real                  :: tismodg, twt, tratio, thw
    real, parameter       :: nu = 0.3, rhow = 1025.
    !     complex               :: gv, c1
    !     real                  :: sigma, c2, wno, cgo, thkh,  &
    complex               :: gv, c1, c2
    real                  :: sigma, wno, cgo, thkh,  &
         rtrl(5), rtim(5), rtang(5)
    integer               :: ireal
    !     complex(kdpc)         :: guess, croot, c1d
    !     real(kdp)             :: c2d, hwatd
    complex(kdpc)         :: guess, croot, c1d, c2d
    real(kdp)             :: hwatd
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ! note, same as w3ic3wncg_xx in w3sic3md :
    !     hice   →   ice1
    !     ivisc  →   ice2
    !     rhoi   →   ice3
    !     ismodg →   ice4
    ! 0.  initializations ------------------------------------------------ *
    ! set limiters
    !
    ! when g = 0, the fs method does not provide a solution. it is not
    ! unexpected because the fs model is originally devised as a
    ! thin elastic plate model in which elasticity is necessary.
    !
    ! the fs algorithm may also have issues for very short wave periods,
    ! shallow waters and low g (e.g., t~3 s, d~10 m, hi~0.5 m, g<10^6 pa)
    !
    ic5minig     = ic5pars(1) ! minimum g
    ic5minwt     = ic5pars(2) ! minimum t
    ic5maxkratio = ic5pars(3) ! maximum k_{ow}/k_r
    ic5maxki     = ic5pars(4) ! maximum k_i
    ic5minhw     = ic5pars(5) ! minimum d
    ic5vemod     = ic5pars(9) ! model selected 1: efs, 2: rp, 3: m2
    !
    tismodg  = max(ic5minig, ismodg)
    twt      = max(ic5minwt, wt)
    thw      = max(ic5minhw, hwat)
    !
    ! g <= 0. is not allowed
    if (abs(tismodg) < errtol) then
      if ( iaproc .eq. naperr ) write(ndse, 1000) 'fsdisp'
      call extcde(1)
    end if
    !
    ! σ = 2π / t
    sigma = tpi / twt
    !
    if (abs(ic5vemod - 1.) < errtol) then
      ! complex shear modulus gv = g - i σ ρ η (efs model)
      gv = cmplx(tismodg, -1. * sigma * rhoi * ivisc)
      !
      ! -------------------------------------------------------------------- *
      ! note that eq. (24) in mosig et al. (2015) can be written like below:
      ! (c1 * k^5 + c2 * k) * tanh(hwat*k) - 1 = 0
      ! most important part of this module --------------------------------- *
      c1 = gv * hice**3. / (6. * rhow * sigma**2.)
      !
      ! to be divided by (1-nu) or multiplied by (1+nu) ??
      ! beam model: then multiplied by (1+ν)
      ! plate model: then divided by (1-ν)
      ! the beam version is more theoretically (j.e.m. mosig, personal
      ! communication, 2016), although there is only very marginal difference
      ! between this two version as (1+nu = 1.3 and 1/(1-nu) ~ 1.4)
      c1 = c1 * (1+nu)
      !         c1 = c1 / (1-nu)
      !
      ! c2
      !         c2 = grav / sigma**2. - rhoi * hice / rhow
      c2 = cmplx(grav / sigma**2. - rhoi * hice / rhow, 0.)
      !
    else if (abs(ic5vemod - 2.) < errtol) then
      ! see appendix of liu et al. (2020) - rp model
      c1 = cmplx(tismodg * hice**3. * (1+nu) / (6. * rhow * sigma**2.), 0.)
      c2 = cmplx(grav/sigma**2. - rhoi * hice / rhow,              &
           -1. * ivisc / (rhow * sigma))
      !
    else if (abs(ic5vemod - 3.) > errtol) then
      write(ndse, 1003) 'fsdisp', ic5vemod
      call extcde(4)
    end if
    ! use the dispersion in open water to get an approximation of
    ! tanh(hwat * k). we can also roughly use the dispersion in deep
    ! water case, that is tanh(hwat*k) ~ 1.
    ! wavenumber in the open water
    !     wavnu1(si, h, k, cg)
    call wavnu1(sigma, thw, wno, cgo)
    thkh = tanh(wno * thw)
    !
    if (abs(ic5vemod - 1.) < errtol .or. abs(ic5vemod - 2.) < errtol) then
      ! get the first guess of the complex wavenumber
      call polyroots(6, &
           (/real(real(c1))*thkh, 0., 0., 0., real(real(c2))*thkh, -1./),&
           rtrl, rtim)
      rtang = atan2(rtim, rtrl)
      !
      ! there should only be one real root in rtrl + i * rtim because in
      ! this case (ivisc=0) the original viscoelastic-type model reduced to
      ! the thin elastic plate model which has only one real solution.
      ! find its index ...
      !
      ireal = minloc(abs(rtang), dim=1)
      if (rtrl(ireal) <= 0. .or. abs(rtim(ireal)) > errtol) then
        if ( iaproc .eq. naperr ) write(ndse, 1001) 'fsdisp'
        call extcde(2)
      end if
      !
      ! get the first guess for iteration
      guess = rtrl(ireal) * exp(cmplx(0., 1e-6))
      !
      ! newton-raphson method
      ! turn c1, c2, hwat to be double
      c1d = c1; c2d = c2; hwatd = thw
      croot = nr_root(c1d, c2d, hwatd, guess)
      wnr = real(real(croot))
      wni = real(aimag(croot))
      !
    else if (abs(ic5vemod - 3.) < errtol) then ! m2
      ! model with order 3 power law (section 6.2 in meylan et al. (2018, jgr-ocean))
      ! based on my understanding, the wavelength does not change because
      ! the elasticity is not considered in this model
      wnr = wno ! open-water wavenumber
      ! eq. (53) in meylan et al. (2018)
      wni = hice * ivisc * sigma**3. / (rhow * grav**2.)
    end if
    !
    ! ratio check
    ! using the ratio k0 / kr as a basic check for the reliability of
    ! fsdisp. the fs dispersion relation can give a very different kr from
    ! k0, especially for small wave periods (k0/kr is as high as 100).
    ! from my tests, using ic5maxkratio = 1000. can basically detect most
    ! spurious solutions (although not all of them)
    !
    ! isnan check
    ! common ways used are:
    ! nan = sqrt(-1.) or
    ! a /= a then a is nan or
    ! isnan func (supported by gfortran & ifort)
    !        --- isnan -> w3inan because isnan is not supported by pgi
    ! for very few cases, we can get nan | negative ki | kr
    !
    ! (n.b.) nan problem solved by using cmplx_tanh2
    !
    tratio = wno / wnr
    if (w3inan(wnr) .or. w3inan(wni) .or. wnr <= 0 .or. wni <= 0. &
         .or. tratio >= ic5maxkratio) then
      if ( iaproc .eq. naperr )                       &
           write(ndse, 1002) 'fsdisp', hice, ivisc, tismodg, hwat, twt, &
           wno, wnr, wni
      call extcde(3)
    end if
    !
    ! filter high ki
    wni = min(ic5maxki, wni)
    !
    ! format
1000 format(/' *** wavewatch iii error in w3sic5md : '/   &
         '     subr. ', a, ': zero shear modulus g is not allowed&
         & in the fs viscoelastic model'/)
    !
1001 format(/' *** wavewatch iii error in w3sic5md : '/   &
         '     subr. ', a, ': get a bad first guess'/)
    !
1002 format(/' *** wavewatch iii error in w3sic5md : '/   &
         ' -----------------------------------------------------'/&
         '     subr. ', a,'   : get nan/neg/huge kr or ki for'   /&
         ' -----------------------------------------------------'/&
         '  ice thickness     : ', f9.1, ' m'/         &
         '  ice viscosity     : ', e9.2, ' m2/s'/      &
         '  ice shear modulus : ', e9.2, ' pa' /       &
         '  water depth       : ', f9.1, ' m'/         &
         '  wave period       : ', f10.2, ' s'/        &
         '  wave number (ko)  : ', f11.3, ' rad/m'/    &
         '  wave number (kr)  : ', f11.3, ' rad/m'/    &
         '  attenu. rate (ki) : ', e9.2, ' /m'/)
    !
1003 format(/' *** wavewatch iii error in w3sic5md : '/   &
         '     subr. ', a, ': unknown ve model (', f5.0, ')'/)
    !/
    !/ end of fsdisp ----------------------------------------------------- /
    !/
  end subroutine fsdisp
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief reducing the sensitivity of eigenvalues to rounding errors during
  !>  the execution of some algorithms.
  !>
  !> @param[in]    nmat    the size of one dimension of matrix.
  !> @param[inout] matrix  a matrix with the shape (nmat, nmat).
  !>
  !> @author q. liu
  !> @date   15-mar-2016
  !>
  subroutine balancing_matrix(nmat, matrix)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    15-mar-2016 : origination                         ( version 5.10)
    !/                                                      ( q. liu )
    !/    15-mar-2016 : borrowed from numerical recipes in fortran
    !/                                                      ( q. liu )
    !  1. purpose :
    !     reducing the sensitivity of eigenvalues to rounding errors during
    !     the execution of some algorithms.
    !
    !  2. method :
    !     the errors in the eigensystem found by a numerical procedure are
    !     generally proportional to the euclidean norm of the matrix, that
    !     is, to the square root of the sum of the squares of the elements
    !     (sqrt(sum(a_{i, j} ** 2.)). the idea of balancing is to use
    !     similarity transformations to make corresponding rows and columns
    !     of the matrix have comparable norms, thus reducing the overall
    !     norm of the matrix while leaving the eigenvalues unchanged. note
    !     that the symmetric matrix is already balanced.
    !
    !     the output is matrix that is balanced in the norm given by
    !     summing the absolute magnitudes of the matrix elements(
    !     sum(abs(a_{i, j})) ). this is more efficient than using the
    !     euclidean norm, and equally effective: a large reduction in
    !     one norm implies a large reduction in the other.
    !
    !     for the details of this method, please refer to
    !     1) numerical recipes in fortran 77 (volume 1, 2nd edition)
    !        [chapter 11.5 / subroutine balanc]
    !     2) numerical recipes in fortran 90 (volume 2)
    !        [chapter b11 / subroutine balanc]
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type   intent   description
    !     ----------------------------------------------------------------
    !     nmat     int.   i        the size of one dimension of matrix
    !     matrix   r.a.   i/o      a matrix with the shape (nmat, nmat)
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing (!/s switch).
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      polyroots subr. /        find the roots of polynomials
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks:
    !     balancing only needs marginal computational efforts but can
    !     substantially improve the accuracy of the eigenvalues computed
    !     for a badly balanced matrix. it is therefore recommended that
    !     you always balance nonsymmetric matrices.
    !
    !     given a (nmat, nmat) matrix, this routine replaces it by a
    !     balanced matrix with identical eigenvalues. a symmetric matrix is
    !     already balanced and is unaffected by this procedure.
    !
    !  8. structure :
    !
    !     see the source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    integer, intent(in)    :: nmat
    real, intent(inout)    :: matrix(nmat, nmat)
    !/ ------------------------------------------------------------------- /
    !/ local parameter
    ! the parameter radx is the machine's floating-point radix
    real, parameter        :: radx = radix(matrix), &
         sqradx = radx ** 2
    integer                :: i, last
    real                   :: c, f, g, r, s
    !/ ------------------------------------------------------------------- /
    !
    do
      last = 1
      do i = 1, nmat
        ! calculate row and column norms
        c = sum( abs(matrix(:, i)) ) - matrix(i, i)
        r = sum( abs(matrix(i, :)) ) - matrix(i, i)
        ! if both are non-zero
        if (c /= 0.0 .and. r /= 0.0) then
          ! find the integer power of the machine radix that comes closest to
          ! balancing the matrix (get g, f from c, r)
          g = r / radx
          f = 1.0
          s = c + r
          do
            if (c >= g) exit
            f = f * radx
            c = c * sqradx
          end do
          !
          g = r * radx
          do
            if (c <= g) exit
            f = f / radx
            c = c / sqradx
          end do
          !
          if ( (c+r)/f < 0.95*s) then
            last = 0
            g = 1.0 / f
            ! apply similarity tranformation
            matrix(i, :) = matrix(i, :) * g
            matrix(:, i) = matrix(:, i) * f
          end if
        end if
      end do
      if (last /= 0) exit
    end do
    !/
    !/ end of subroutine balancing_matrix -------------------------------- /
    !/
  end subroutine balancing_matrix
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief calculate the eigenvalues of a general matrix.
  !>
  !> @param[in]    nmat  the size of one dimension of hmat.
  !> @param[inout] hmat  the hessenberg-type matrix (nmat, nmat).
  !> @param[out]   eigr  the real part of the n eigenvalues.
  !> @param[out]   eigi  the imaginary part of the n eigenvalues.
  !>
  !> @author q. liu
  !> @date   17-mar-2016
  !>
  subroutine eig_hqr (nmat, hmat, eigr, eigi)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    16-mar-2016 : origination                         ( version 5.10)
    !/                                                      ( q. liu )
    !/    16-mar-2016 : borrowed from numerical recipes in fortran
    !/                                                      ( q. liu )
    !/    17-mar-2016 : update the nr code v2.08 to v2.10   ( q. liu )
    !/
    !  1. purpose :
    !
    !     when we calculate the eigenvalues of a general matrix, we first
    !     reduce the matrix to a simpler form (e.g., hessenberg form) and
    !     then we perform the iterative procedures.
    !
    !     a upper hessenberg matrix has zeros everywhere below the diagnal
    !     except for the first subdiagonal row. for example, in the 6x6
    !     case, the non-zero elements are:
    !                   |x x x x x x|
    !                   |x x x x x x|
    !                   |  x x x x x|
    !                   |    x x x x|
    !                   |      x x x|
    !                   |        x x|
    !
    !     this subroutine uses qr algorithm to get the eigenvalues of a
    !     hessenberg matrix. so make sure the input array hmat is a
    !     hessenberg-type matrix.
    !
    !  2. method :
    !     qr algorithm for real hessenberg matrices.
    !     (i did not understand this algorithm well, so i could not give
    !      any detailed explanations)
    !
    !     for the details of this hqr method, please refer to
    !     1) numerical recipes in fortran 77 (volume 1, 2nd edition)
    !        [chapter 11.6 / subroutine hqr]
    !     2) numerical recipes in fortran 90 (volume 2)
    !        [chapter b11 / subroutine hqr]
    !
    !     note that there is a bug in the `hqr` subroutine in nr v2.08.
    !     see http://numerical.recipes/latest-known-bugs.html. please use
    !     the updated code in nr v2.10.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type   intent   description
    !     ----------------------------------------------------------------
    !     nmat     int.   i        the size of one dimension of hmat
    !     hmat     r.a.   i/o      the hessenberg-type matrix (nmat, nmat)
    !     eigr     r.a.   o        the real part of the n eigenvalues
    !     eigi     r.a.   o        the imag part of the n eigenvalues
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
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      polyroots subr. /        find the roots of polynomials
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see format 1001
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    use w3odatmd, only: ndse, iaproc, naproc, naperr
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)  :: nmat
    real, intent(inout)  :: hmat(nmat, nmat)
    real, intent(out)    :: eigr(nmat), eigi(nmat)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    integer              :: i, its, k, l, m, nn, mnnk, idiag
    real                 :: anorm, p, q, r, s, t, u, v, w, x, y, z
    real                 :: pp(nmat)
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! compute matrix norm for possible use in locating single small
    ! subdiagonal element.
    !
    ! note the speciality of hessenberg matrix :
    ! elements below the diagonal are zeros except for the first
    ! subdiagonal row. it might be more accurate if we use a mask array
    ! to mask all zero elments.
    !
    anorm = sum(abs(hmat))
    nn = nmat
    ! gets changed only by an exceptional shift.
    t = 0.0
    ! begin search for next eigenvalue: "do while nn >= 1"
    do
      if (nn < 1) exit
      its=0
      ! begin iteration
      iterate:do
        ! look for single small subdiagonal element.
        small: do l=nn, 2, -1
          s = abs( hmat(l-1, l-1) ) + abs( hmat(l, l) )
          !                  if (s == 0.0) s = anorm
          if (abs(s) < errtol) s = anorm
          !                  if ( abs(hmat(l, l-1)) + s == s ) then
          if ( abs(hmat(l, l-1)) < errtol ) then
            hmat(l, l-1) = 0.0
            exit small
          end if
        end do small
        x = hmat(nn, nn)
        ! one root found
        if (l == nn) then
          eigr(nn) = x + t
          eigi(nn) = 0.0
          nn=nn-1
          ! go back for next eigenvalue
          exit iterate
        end if
        y = hmat(nn-1, nn-1)
        w = hmat(nn, nn-1) * hmat(nn-1, nn)
        ! two roots found . . .
        if (l == nn-1) then
          p = 0.5 * (y - x)
          q = p**2 + w
          z = sqrt( abs(q) )
          x = x + t
          ! . . . a real pair . . .
          if (q >= 0.0) then
            z = p + sign(z, p)
            eigr(nn) = x + z
            eigr(nn-1) = eigr(nn)
            if (z /= 0.0) eigr(nn) = x - w/z
            eigi(nn) = 0.0
            eigi(nn-1) = 0.0
            ! . . . a complex pair
          else
            eigr(nn) = x + p
            eigr(nn-1) = eigr(nn)
            eigi(nn) = z
            eigi(nn-1) = -z
          end if
          nn=nn-2
          ! go back for next eigenvalue.
          exit iterate
        end if
        ! no roots found. continue iteration.
        if (its == 30) then
          if ( iaproc .eq. naperr ) write(ndse, 1001) 'eig_hqr'
          call extcde(2)
        end if
        ! form exceptional shift.
        if (its == 10 .or. its == 20) then
          t = t + x
          ! add -x to the diagonal of hmat
          do idiag = 1, nn
            hmat(idiag, idiag) = hmat(idiag, idiag) + (-x)
          end do
          s = abs(hmat(nn, nn-1)) + abs(hmat(nn-1, nn-2))
          x = 0.75 * s
          y = x
          w = -0.4375 * s**2
        end if
        its = its + 1
        ! form shift and then look for 2 consecutive small subdiagonal elements.
        do m = nn-2, l, -1
          z = hmat(m, m)
          r = x - z
          s = y - z
          ! equation (11.6.23).
          p = (r * s - w) / hmat(m+1, m) + hmat(m, m+1)
          q = hmat(m+1, m+1) - z - r - s
          r = hmat(m+2, m+1)
          ! scale to prevent overflow or underflow
          s = abs(p) + abs(q) + abs(r)
          p = p / s
          q = q / s
          r = r / s
          if (m == l) exit
          u = abs( hmat(m, m-1) ) * ( abs(q) + abs(r) )
          v = abs(p) * ( abs(hmat(m-1, m-1)) + abs(z) + &
               abs( hmat(m+1, m+1) ))
          ! equation (11.6.26)
          if (u+v == v) exit
        end do
        do i= m+2, nn
          hmat(i, i-2) = 0.0
          if (i /= m+2) hmat(i, i-3)=0.0
        end do
        ! double qr step on rows l to nn and columns m to nn
        do k=m, nn-1
          if (k /= m) then
            ! begin setup of householder vector
            p = hmat(k, k-1)
            q = hmat(k+1, k-1)
            r = 0.0
            if (k /= nn-1) r = hmat(k+2, k-1)
            x = abs(p) + abs(q) + abs(r)
            if (x /= 0.0) then
              ! scale to prevent overflow or underflow
              p = p / x
              q = q / x
              r = r / x
            end if
          end if
          s = sign(sqrt(p**2 + q**2 + r**2), p)
          if (s /= 0.0) then
            if (k == m) then
              if (l /= m) hmat(k, k-1) = -hmat(k, k-1)
            else
              hmat(k, k-1) = -s * x
            end if
            ! equations (11.6.24).
            p = p + s
            x = p / s
            y = q / s
            z = r / s
            q = q / p
            ! ready for row modification.
            r = r / p
            pp(k:nn) = hmat(k, k:nn) + q * hmat(k+1, k:nn)
            if (k /= nn-1) then
              pp(k:nn) = pp(k:nn) + r * hmat(k+2, k:nn)
              hmat(k+2, k:nn) = hmat(k+2, k:nn) - &
                   pp(k:nn)*z
            end if
            hmat(k+1, k:nn) = hmat(k+1, k:nn) - pp(k:nn) * y
            hmat(k, k:nn) = hmat(k, k:nn) - pp(k:nn) * x
            ! column modification.
            mnnk = min(nn, k+3)
            pp(l:mnnk) = x * hmat(l:mnnk, k) + y * &
                 hmat(l:mnnk, k+1)
            if (k /= nn-1) then
              pp(l:mnnk) = pp(l:mnnk) + z*hmat(l:mnnk, k+2)
              hmat(l:mnnk, k+2) = hmat(l:mnnk, k+2) - &
                   pp(l:mnnk) * r
            end if
            hmat(l:mnnk, k+1) = hmat(l:mnnk, k+1) - &
                 pp(l:mnnk) * q
            hmat(l:mnnk, k) = hmat(l:mnnk, k) - pp(l:mnnk)
          end if
        end do
        ! go back for next iteration on current eigenend do value.
      end do iterate
    end do
    !
    ! formats
1001 format(/' *** wavewatch iii error in w3sic5md : '/  &
         '     subr. ', a, ': too many iterations'/)
    !/ ------------------------------------------------------------------- /
    !/
    !/ end of eig_hqr ---------------------------------------------------- /
    !/
  end subroutine eig_hqr
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief find the roots of arbitrary polynomials through finding the
  !>  eigenvalues of companion matrix.
  !>
  !> @param[in]  npc    the # of the polynomial coefficients.
  !> @param[in]  pcvec  the 1d vector for the polynomial coefficients.
  !> @param[out] rtrl   the real part of all of the roots shape: [npc-1].
  !> @param[out] rtim   the real part of all of the roots shape: [npc-1].
  !>
  !> @author q. liu
  !> @date   16-mar-2016
  !>
  subroutine polyroots(npc, pcvec, rtrl, rtim)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         16-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    16-mar-2016 : origination                         ( version 5.10)
    !/                                                      ( q. liu )
    !/    16-mar-2016 : started from numerical recipes in fortran
    !/                                                      ( q. liu )
    !/
    !  1. purpose :
    !
    !     find the roots of arbitrary polynomials through finding the
    !     eigenvalues of companion matrix.
    !
    !  2. method :
    !     suppose we have a general polynomial, which reads
    !     p(x) = c_n * x^n + c_{n-1} * x^{n-1} + ... + c_1 * x + c_0
    !
    !     then finding the roots of p(x) is equivalent to find the eigen-
    !     values of the special n x n companion matrix a
    !         |  -c_{n-1}/c_n   -c_{n-2}/c_n   ...   -c_1/c_n   -c_0/c_n |
    !         |  1              0              ...   0          0        |
    !     a = |  0              1              ...   0          0        |
    !         |  :              :                    :          :        |
    !         |  0              0                    1          0        |
    !
    !     in fact, p(x) is the characteristic polynomial of matrix a, i.e.,
    !     p(x) = det|a-xi| and x is the eigenvalues of a (this is a
    !     hessenberg matrix).
    !
    !     in this subrountine, we will use the two subroutines above
    !     (balancing_matrix & eig_hqr) to get the complex eigenvalues of
    !     an abitrary hessenberg matrix
    !
    !     for the details of this method, please refer to
    !     1) numerical recipes in fortran 77 (volume 1, 2nd edition)
    !        [chapter 9.5 / eigenvalue methods / subroutine zrhqr]
    !     2) numerical recipes in fortran 90 (volume 2)
    !        [chapter b9 / subroutine zrhqr]
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type   intent   description
    !     ----------------------------------------------------------------
    !     npc      int.   i        the # of the polynomial coefficients
    !                              (from c_n to c_0)
    !     pcvec    r.a.   i        the 1d vector for the polynomial
    !                              coefficients  [c_n, c_{n-1}, ..., c_0]
    !     rtrl     r.a.   o        the real part of all of the roots
    !                              shape: [npc-1]
    !     rtim     r.a.   o        the real part of all of the roots
    !                              shape: [npc-1]
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name                type    module    description
    !     ------------------------------------- ---------------------------
    !      strace              subr.   w3servmd  subroutine tracing.
    !      balancing_matrix    subr.   /         balancing matrix
    !      eig_hqr             subr.   /         finding eigenvalues
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      fsdisp    subr. /        solving the dispersion relations
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see format 1001
    !
    !  7. remarks :
    !     the built-in matlab function <roots> uses the same method to
    !     find roots of a general polynomial. but perhaps matlab uses
    !     different methods to find eigenvalues of the companion matrix.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    use w3odatmd, only: ndse, iaproc, naproc, naperr
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    integer, intent(in)   :: npc
    real, intent(in)      :: pcvec(npc)
    real, intent(out)     :: rtrl(npc-1), rtim(npc-1)
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real                  :: hess(npc-1, npc-1)
    integer               :: j
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    if (abs(pcvec(1)) < errtol) then
      if ( iaproc .eq. naperr ) write(ndse, 1001) 'polyroots'
      call extcde(2)
    end if
    !
    ! generate the hessenberg matrix
    hess = 0.
    hess(1, :) = -1 * pcvec(2:) / pcvec(1)
    do j = 1, npc-2
      hess(j+1, j) = 1.
    end do
    ! balancing the matrix hess
    call balancing_matrix(npc-1, hess)
    ! eigenvalues of the matrix hess
    call eig_hqr(npc-1, hess, rtrl, rtim)
    ! formats
1001 format(/' *** wavewatch iii error in w3sic5md : '/  &
         '     subr. ', a, ': the coeff. of x^n must not be 0'/)
    !/
    !/ end of polyroots -------------------------------------------------- /
    !/
  end subroutine polyroots
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief calculate the corrected term in the newton-raphson root-finding
  !>  method (must use double precision).
  !>
  !> @param   k        complex wave number.
  !> @param   c1       c1 in fsdisp.
  !> @param   c2       c2 in fsdisp.
  !> @param   h        water depth.
  !> @returns nr_corr  newton-raphson corrected term (dk).
  !>
  !> @author q. liu
  !> @date   19-may-2021
  !>
  function nr_corr(k, c1, c2, h)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-may-2021 |
    !/                  +-----------------------------------+
    !/
    !/    18-mar-2016 : origination.                        ( version 5.10 )
    !/                                                      ( q. liu )
    !/    18-mar-2016 : start from the matlab code `foxsquire.m` (provided
    !/                  by prof. vernon squire from university of otago)
    !/                                                      ( q. liu )
    !/    24-mar-2016 : adding the cmplx_sinh/cosh/tanh     ( q. liu )
    !/
    !/    19-may-2021 : change types of few input arguments ( q. liu )
    !/
    !  1. purpose :
    !
    !     calculate the corrected term in the newton-raphson root-finding
    !     method (must use double precision)
    !
    !  2. method :
    !     suppose we want to find the root of f(x) = 0, then according to
    !     the newton-raphson method, the root is iteratively updated by the
    !     formula below:
    !
    !         x_{i+1} = x_{i} - f(x_{i}) / f'(x_{i}),
    !
    !     where f'(x) denotes the derivative of f(x). in this function,
    !     our f(x) reads (see also subr. fsdisp)
    !
    !         f(x) = (c1 * k**4 + c2) * k * tanh(kh) -1
    !
    !     we finally will get the newton-raphson correted term, i.e.,
    !
    !         dx = f(x_{i}) / f'(x_{i})
    !
    !     for the details of this method, please refer to
    !     1) numerical recipes in fortran 77 (volume 1, 2nd edition)
    !        chapter 9.4
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type      intent   description
    !     ----------------------------------------------------------------
    !     k        cmpl.(d)  i        complex wave number
    !     c1       cmpl.(d)  i        c1 in fsdisp
    !     c2       real.(d)  i        c2 in fsdisp
    !     h        real.(d)  i        water depth
    !     nr_corr  cmpl.(d)  o        newton-raphson corrected term (dk)
    !     ----------------------------------------------------------------
    !     * (d) means double precision
    !
    !  4. subroutines used :
    !
    !      name        type  module   description
    !     ----------------------------------------------------------------
    !      strace      subr. w3servmd subroutine tracing.
    !      cmplx_sinh  func. /         sinh for complex var.
    !      cmplx_cosh  func. /         cosh for complex var.
    !      cmplx_tanh2 func. /         tanh for complex var.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      nr_root   func. /        newton-raphson root finding.
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !     complex(kdpc), intent(in)     :: k, c1
    !     real(kdp), intent(in)         :: c2, h
    complex(kdpc), intent(in)     :: k, c1, c2
    real(kdp), intent(in)         :: h
    complex(kdpc)                 :: nr_corr
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    ! a rough value to differentiate deep water case from finite water case
    real(kdp), parameter          :: kh_lim = 7.5
    complex(kdpc)                 :: lam, lampr, fv, df, tkh
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ! f(k) = (c1 * k**4 + c2) * k * tanh(k*h) - 1
    !      = lam * k * tanh(k*h) - 1
    !
    tkh   = k * h
    lam   = c1 * k**4 + c2
    ! the derivative of (lam * k)
    lampr = 5 * c1 * k**4 + c2
    !
    if (real(real(tkh)) <= kh_lim) then
      !         kh is small enough
      !         fv = lam * k * sinh(k*h) - cosh(k*h)
      !         df = lam * (k*h) * cosh(k*h) + (lampr - h) * sinh(k*h)
      fv = lam * k * cmplx_sinh(tkh) - cmplx_cosh(tkh)
      df = lam * tkh * cmplx_cosh(tkh) + (lampr-h) * cmplx_sinh(tkh)
    else
      !         fv = lam * k * tanh(k*h) - 1
      !         df = lam * k * h + (lampr - h) * tanh(k*h)
      !         df = lampr * tanh(k*h) + lam * k * h / (cosh(k*h) **2)
      fv = lam * k * cmplx_tanh2(tkh) - 1
      df = lampr * cmplx_tanh2(tkh) + lam * tkh * &
           (1 - cmplx_tanh2(tkh) ** 2.)
    end if
    !
    nr_corr = fv / df
    !/
    !/ end of nr_corr ---------------------------------------------------- /
    !/
  end function nr_corr
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief the iterative procedure of the newton-raphson method.
  !>
  !> @param    c1        c1 in fs dipsersion relations.
  !> @param    c2        c2 in fs dipsersion relations.
  !> @param    h         water depth.
  !> @param    guess     the first guess obtained from polyroots.
  !> @returns  nr_root   the calculated complex wave number.
  !>
  !> @author q. liu
  !> @date   19-may-2021
  !>
  function nr_root(c1, c2, h, guess)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-may-2021 |
    !/                  +-----------------------------------+
    !/
    !/    18-mar-2016 : origination.                        ( version 5.10 )
    !/                                                      ( q. liu )
    !/    18-mar-2016 : start from the matlab code `foxsquire.m` (provided
    !/                  by prof. vernon squire from university of otago)
    !/                                                      ( q. liu )
    !/
    !/    19-may-2021 : change types of few input arguments ( q. liu )
    !/
    !  1. purpose :
    !
    !     the iterative procedure of the newton-raphson method
    !
    !  2. method :
    !     see the document of subr. nr_corr (must use double precision)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type      intent  description
    !     ----------------------------------------------------------------
    !     c1       cmpl.(d)  i       c1 in fs dipsersion relations
    !                                see the doc. of subr. nr_corr
    !     c2       real (d)  i       c2 in fs dipsersion relations
    !     h        real (d)  i       water depth
    !     guess    cmpl.(d)  i       the first guess obtained from polyroots
    !     nr_root  cmpl.(d)  o       the calculated complex wave number.
    !     ----------------------------------------------------------------
    !     * (d) means double precision
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      nr_corr   func. /        newton-raphson correction term
    !      init_random_seed
    !                subr. /        initialize the random seed based on
    !                               the system's time
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      fsdisp    subr. /        solve  fs dispersion relations
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3servmd, only: extcde
    use w3odatmd, only: ndse, iaproc, naproc, naperr
    use w3gdatmd, only: ic5pars
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !     complex(kdpc), intent(in)     :: c1, guess
    !     real(kdp), intent(in)         :: c2, h
    complex(kdpc), intent(in)     :: c1, guess, c2
    real(kdp), intent(in)         :: h
    complex(kdpc)                 :: nr_root
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    complex(kdpc)                 :: k0, k1, dk
    integer                       :: iter
    real                          :: tranval
    real                          :: ic5maxiter, ic5rkick, ic5kfilter
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/ set parameters
    ic5maxiter = ic5pars(6)
    ic5rkick   = ic5pars(7) ! 0: false, 1: true
    ic5kfilter = ic5pars(8)
    !
    k0 = guess
    dk = nr_corr(k0, c1, c2, h)
    k1 = k0 - dk
    iter = 0
    if (ic5rkick > 0.5) call init_random_seed()
    !
    do while (abs(dk) > errtol)
      k0 = k1
      dk = nr_corr(k0, c1, c2, h)
      k1 = k0 - dk
      iter = iter + 1
      !
      ! random kick to avoid converging to evanescent modes
      ! note: do not use rand(1) because it alway gives a same random no.
      ! the built in function of rand is not available in <ifort>, use
      ! random_seed/number instead.
      !
      ! based on many tests, i found the random kick & the corridor excluded
      ! from imaginary axis are kind of helpful to avoid spurious solutions.
      ! however, it may also lead to no solutions returned, especially for
      ! high g and high t.
      !
      if (ic5rkick > 0.5 .and. abs(real(k1)) < ic5kfilter) then
        !             k1 = k1 + 2*rand(0)
        call random_number(tranval)
        k1 = k1 + 2 * tranval
      end if
      !
      if (iter >= ic5maxiter) then
        if ( iaproc .eq. naperr ) write(ndse, 1001) 'nr_root'
        call extcde(1)
      end if
      !
    end do
    !
    nr_root = k1
    !
    ! formats
1001 format(/' *** wavewatch iii error in w3sic5md : '/  &
         '     subr. ', a, ': too many iterations'/)
    !
    !/
    !/ end of nr_root ---------------------------------------------------- /
    !/
  end function nr_root
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief custom written sinh function for complex inputs.
  !>
  !> @details for a number of compilers, the built-in function sinh,
  !>  cosh and tanh do not support the complex inputs. so here i write an
  !>  external one.
  !>
  !> @param    x            a double-precision complex variable.
  !> @returns  cmplx_sinh   complex sinh(x).
  !>
  !> @author q. liu
  !> @date   24-mar-2016
  !>
  function cmplx_sinh(x)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    24-mar-2016 : origination.                        ( version 5.10 )
    !/                                                      ( q. liu )
    !/
    !  1. purpose :
    !
    !     for a number of compilers, the built-in function sinh, cosh and
    !     tanh do not support the complex inputs. so here i write an
    !     external one.
    !
    !  2. method :
    !
    !     sinh(x) = (e**x - e**(-x)) / 2 (the built in function exp supports
    !     complex input)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type      intent  description
    !     ----------------------------------------------------------------
    !      x       cmpl(d)     i     a double-precision complex var.
    !     ----------------------------------------------------------------
    !      * note, this subr. will be only called by nr_corr,
    !      so for simplicity, i only use double-precision complex var.
    !      as input.
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
    !      name       type  module   description
    !     ----------------------------------------------------------------
    !      nr_corr    subr. /        newton-raphson correction term.
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    complex(kdpc), intent(in)  :: x
    complex(kdpc)              :: cmplx_sinh
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    cmplx_sinh = (exp(x) - exp(-x)) * 0.5
    !/
    !/ end of cmplx_sinh ------------------------------------------------- /
    !/
  end function cmplx_sinh
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief hand written cosh function for complex inputs.
  !>
  !> @details for a number of compilers, the built-in function sinh,
  !>  cosh and tanh do not support the complex inputs. so here i write an
  !>  external one.
  !>
  !> @param   x           a double-precision complex variable.
  !> @returns cmplx_cosh  complex cosh(x).
  !>
  !> @author q. liu
  !> @date   24-mar-2016
  !>
  function cmplx_cosh(x)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    24-mar-2016 : origination.                        ( version 5.10 )
    !/                                                      ( q. liu )
    !/
    !  1. purpose :
    !
    !     for a number of compilers, the built-in function sinh, cosh and
    !     tanh do not support the complex inputs. so here i write an
    !     external one.
    !
    !  2. method :
    !
    !     cosh(x) = (e**x + e**(-x)) / 2 (the built in function exp supports
    !     complex input)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type      intent  description
    !     ----------------------------------------------------------------
    !      x       cmpl(d)     i     a double-precision complex var.
    !     ----------------------------------------------------------------
    !      * note, this subr. will be only called by nr_corr,
    !      so for simplicity, i only use double-precision complex var.
    !      as input.
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
    !      name       type  module   description
    !     ----------------------------------------------------------------
    !      nr_corr    subr. /        newton-raphson correction term.
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    complex(kdpc), intent(in)  :: x
    complex(kdpc)              :: cmplx_cosh
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    cmplx_cosh = (exp(x) + exp(-x)) * 0.5
    !/
    !/ end of cmplx_cosh ------------------------------------------------- /
    !/
  end function cmplx_cosh
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief an alternate version of tanh to avoid overflow error.
  !>
  !> @details we may encounter overflow error for the above tanh
  !>  function as kh becomes huge. this is another version of tanh
  !>  function.
  !>
  !> @param   x            a double-precision complex variable.
  !> @returns cmplx_tanh2  complex tanh(x).
  !>
  !> @author q. liu
  !> @date   24-mar-2016
  !>
  function cmplx_tanh2(x)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    24-mar-2016 : origination.                        ( version 5.10 )
    !/                                                      ( q. liu )
    !/
    !  1. purpose :
    !     we may encounter overflow error for the above tanh function as kh
    !     becomes huge. this is another version of tanh function
    !
    !  2. method :
    !
    !     see https://en.wikipedia.org/wiki/hyperbolic_function
    !     tanh(x) = (exp(x) - exp(-x)) / (exp(x) + exp(-x))
    !             = (1 - exp(-2x)) / (1 + exp(-2x))
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     name     type      intent  description
    !     ----------------------------------------------------------------
    !      x       cmpl(d)     i     a double-precision complex var.
    !     ----------------------------------------------------------------
    !      * note, this subr. will be only called by nr_corr, so for
    !        simplicity, i only use double-precision complex var. as input.
    !
    !  4. subroutines used :
    !      name        type  module   description
    !     ----------------------------------------------------------------
    !      strace      subr. w3servmd subroutine tracing.
    !      cmplx_sinh  func. /        sinh for complex var.
    !      cmplx_cosh  func. /        cosh for complex var.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      nr_corr   subr. /        newton-raphson correction term.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !       calculating tanh in this way may have problems when x ->
    !       -inf. but in our cases x is alway >0.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    complex(kdpc), intent(in)  :: x
    complex(kdpc)              :: cmplx_tanh2
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    cmplx_tanh2 = (1 - exp(-2*x)) / (1 + exp(-2*x))
    !/
    !/ end of cmplx_tanh2 ------------------------------------------------ /
    !/
  end function cmplx_tanh2
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief initialize the random seed based on the system's time.
  !>
  !> @author q. liu
  !> @date   24-mar-2016
  !>
  subroutine init_random_seed()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-mar-2016 |
    !/                  +-----------------------------------+
    !/
    !/    24-mar-2016 : origination.                        ( version 5.10 )
    !/                                                      ( q. liu )
    !/    24-mar-2016 : borrowed from fortran wiki          ( q. liu )
    !
    !  1. purpose :
    !
    !     initialize the random seed based on the system's time.
    !
    !  2. method :
    !
    !     see http://fortranwiki.org/fortran/show/random_seed
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      nr_root   func. /        newton-raphson root finding.
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    integer                            :: i, n, clock
    integer, dimension(:), allocatable :: seed
    !/ ------------------------------------------------------------------- /
    !/
    call random_seed(size = n)
    allocate(seed(n))
    !
    call system_clock(count=clock)
    !
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)
    !
    deallocate(seed)
    !/
    !/ end of init_random_seed ------------------------------------------- /
    !/
  end subroutine init_random_seed
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3sic5md -------------------------------------------- /
  !/
end module w3sic5md
!/ ------------------------------------------------------------------- /
