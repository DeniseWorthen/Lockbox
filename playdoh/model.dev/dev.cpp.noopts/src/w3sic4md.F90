!> @file
!> @brief calculate ice source term s_{ice} according to simple methods.
!>
!> @author c. collins
!> @author e. rogers
!> @date   21-jan-2015
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
!> @details attenuation is a function of frequency and specified directly
!>  by the user. example: a function is based on an exponential fit to
!>  the empirical data of wadhams et al. (1988).
!>
!> @author c. collins
!> @author e. rogers
!> @date   21-jan-2015
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3sic4md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           c. collins              |
  !/                  |           e. rogers               |
  !/                  |                        fortran 90 |
  !/                  | last update :         21-jan-2015 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see w3sic4 documentation.
  !/
  !  1. purpose :
  !
  !     calculate ice source term s_{ice} according to simple methods.
  !          attenuation is a function of frequency and specified directly
  !          by the user. example: a function is based on an exponential fit to
  !          the empirical data of wadhams et al. (1988).
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sic4    subr. public   ice source term.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine documentation.
  !
  !  5. remarks :
  !
  !     documentation of ic4:
  !         1) collins and rogers, nrl memorandum report 2017
  !         ---> "a source term for wave attenuation by sea
  !               ice in wavewatch iii® : ic4"
  !         ---> describes original ic4 methods, 1 to 6
  !         2) rogers et al., nrl memorandum report 2018a
  !         ---> "forecasting and hindcasting waves in and near the
  !              marginal ice zone: wave modeling and the onr “sea
  !              state” field experiment"
  !         ---> ic4 method 7 added
  !         2) rogers et al., nrl memorandum report 2018b
  !         ---> "frequency distribution of dissipation of energy of
  !               ocean waves by sea ice using data from wave array 3 of
  !               the onr “sea state” field experiment"
  !         ---> new recommendations for ic4 method 2 (polynomial fit)
  !              and ic4 method 6 (step function via namelist)
  !
  !     other source material :
  !        *** wadhams et al. jgr 1988
  !        *** meylan et al. grl 2014
  !        *** kohout & meylan jgr 2008 in horvat & tziperman cryo. 2015
  !        *** kohout et al. nature 2014
  !        *** doble et al. grl 2015
  !        *** rogers et al. jgr 2016
  !        *** meylan et al. jgr 2018
  !        *** yu et al. jgr 2019
  !        *** liu et al. jpo 2020
  !        *** rogers et al. crst 2021 (rmk2021)
  !        *** rogers et al. tech. rep. 2021 (ryw2021)
  !        *** yu et al. crst 2022
  !        *** yu jmse 2022
  !
  !  6. switches :
  !
  !     see subroutine documentation.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public :: w3sic4
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
  !> @author c. collins
  !> @author e. rogers
  !> @date   24-feb-2017
  !>
  subroutine w3sic4 (a, depth, cg, ix, iy, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           c. collins              |
    !/                  |           e. rogers               |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-feb-2017 |
    !/                  +-----------------------------------+
    !/
    !/    03-dec-2015 : origination                         ( version 5.09 )
    !/                     (starting from ic1)                (c. collins)
    !/    03-dec-2015 : w3sic4 created, methods 1,2,3,4       (c. collins)
    !/    21-jan-2016 : ic4 added to ncep repository          (e. rogers)
    !/    27-jan-2016 : method 5 added (step function)        (e. rogers)
    !/    08-apr-2016 : method 6 added (namelist step funct.) (e. rogers)
    !/    24-feb-2017 : corrections to methods 1,2,3,4        (e. rogers)
    !/    13-apr-2017 : method 7 added (doble et al. 2015)    (e. rogers)
    !/    11-jan-2024 : method 8 added (meylan et al. 2018)   (e. rogers)
    !/    11-jan-2024 : method 9 added (rogers et al., 2021)
    !/                                      denoted "ryw2021" (e. rogers)
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
    !     apply parametric/empirical functions, e.g. from the literature.
    !     1) exponential fit to wadhams et al. 1988, table 2
    !     2) polynomial fit, eq. 3 from meylan et al. 2014
    !     3) quadratic fit to kohout & meylan'08 in horvat & tziperman'15
    !        here, note that their eqn is given as ln(alpha)=blah, so we
    !        have alpha=exp(blah).
    !        note from er:
    !        this implementation has two things to keep in mind:
    !          1) this is a scattering model, applied as dissipation,
    !             which is not correct.
    !          2) this is not actually ht15! the alpha of ht15 has
    !             different meaning from alpha of cr17, as follows:
    !             ht15: decay is exp(-alpha*lambda) where lambda
    !                   is the number of floes encountered.
    !             cr17: decay is exp(-alpha*x)
    !             thus, cr17's implementation of ht15 is equivalent to
    !             the actual ht15 only if one assumes one floe encountered
    !             per meter. this is very strong attenuation, as shown in
    !             figure 3 of cr17! this problem might be fixed by computing
    !             an encounter interval length scale from an a_ice and d_ice
    !             provided by the user...or a length scale provided by the 
    !             user.
    !             see also: page 3 of rogers et al. (ryw2021).
    !     4) eq. 1 from kohout et al. 2014
    !
    !     5) simple step function for ki as a function of frequency
    !          with up to 4 "steps". controlling parameters kix and fcx are
    !          read in as input fields, so they may be nonstationary and
    !          non-uniform in the same manner that ice concentration and
    !          water levels may be nonstationary and non-uniform.
    !                                          444444444444
    !                               33333333333
    !                   222222222222
    !      1111111111111
    !                  ^            ^          ^
    !                  |            |          |
    !                  5            6          7
    !      here, 1 indicates ki=ki1=icecoef1 (icep1)
    !            2 indicates ki=ki2=icecoef2 (icep2)
    !            3 indicates ki=ki3=icecoef3 (icep3)
    !            4 indicates ki=ki4=icecoef4 (icep4)
    !            5 indicates freq cutoff #1 =fc5=icecoef5 (icep5)
    !            6 indicates freq cutoff #2 =fc6=icecoef6 (mudd)
    !            7 indicates freq cutoff #3 =fc7=icecoef7 (mudt)
    !     freq cutoff is given in hz, freq=1/t (not sigma)
    !     examples using hindcast, inversion with uniform ki:
    !        5.1) beaufort sea, awac mooring, 2012, aug 17 to 20
    !             0.0418 hz to 0.15 hz : ki=10e-6
    !             0.15 hz to 0.175 hz : ki=11e-6
    !             0.175 hz to 0.25 hz : ki=15e-6
    !             0.25 hz to 0.5 hz : ki=25e-6
    !        5.2) beaufort sea, awac mooring, 2012, oct 27 to 30
    !             0.0418 hz to 0.1 hz : ki=5e-6
    !             0.1 hz to 0.12 hz : ki=7e-6
    !             0.12 hz to 0.16 hz : ki=15e-6
    !             0.16 hz to 0.5 hz : ki=100e-6
    !             icep1=ki1=5.0e-6
    !             icep2=ki2=7.0e-6
    !             icep3=ki3=15.0e-6
    !             icep4=ki4=100.0e-6
    !             icep5=fc5=0.10
    !             mudd=fc6=0.12
    !             mudt=fc7=0.16
    !             in terms of the 3-character ids for "homogeneous field
    !             data" in ww3_shel.inp, these are, respectively, ic1, ic2,
    !             ic3, ic4, ic5, mdn, mth, and so this might look like:
    !                'ic1' 19680606 000000   5.0e-6
    !                'ic2' 19680606 000000   7.0e-6
    !                'ic3' 19680606 000000   15.0e-6
    !                'ic4' 19680606 000000   100.0e-6
    !                'ic5' 19680606 000000   0.10
    !                'mdn' 19680606 000000   0.12
    !                'mth' 19680606 000000   0.16
    !
    !     6) simple step function for ki as a function of frequency
    !          with up to 16 "steps". controlling parameters kix and fcx are
    !          read in as namelist parameters, so they are stationary and
    !          uniform. (if 16 steps is not enough, the number of steps can be
    !          increased at compile time by changing nic4 in w3gdatmd.ftn.)
    !          the last non-zero fcx value should be a large number, e.g. 99 hz
    !
    !                                          4444444444  <--- ki=ic4_ki(4)
    !                               3333333333             <--- ki=ic4_ki(3)
    !                   2222222222                         <--- ki=ic4_ki(2)
    !      11111111111                                     <--- ki=ic4_ki(1)
    !                 ^           ^           ^         ^
    !                 |           |           |         |
    !          ic4_fc(1)   ic4_fc(2)   ic4_fc(3) ic4_fc(4)=large number
    !       example: beaufort sea, awac mooring, 2012, oct 27 to 30
    !           &sic4  ic4method = 6,
    !                  ic4ki =    0.50e-05,   0.70e-05,   0.15e-04,
    !                             0.10e+00,   0.00e+00,   0.00e+00,
    !                             0.00e+00,   0.00e+00,   0.00e+00,
    !                             0.00e+00,
    !                  ic4fc =    0.100,      0.120,      0.160,
    !                             99.00,      0.000,      0.000,
    !                             0.000,      0.000,      0.000,
    !                             0.000
    !                             /
    !
    !     7) doble et al. (grl 2015), eq. 3. this is a function of ice
    !        thickness and wave period.
    !        alpha  = 0.2*(t^(-2.13)*hice or
    !        alpha  = 0.2*(freq^2.13)*hice
    !
    !     8) meylan et al. (jgr 2018), eq. 48. "model with order 3 power
    !        law". the is denoted as the "m2" model by liu et al. (jpo 2020)
    !        it is a function of ice thickness and wave period.
    !        ki  = chfm2*h_ice*freq^3
    !        where chfm2 is a coefficient of proportionality which formally
    !        includes viscosity, density, and gravity parameters, see
    !        meylan et al. (jgr 2018) for details.
    !        chfm2 has units of s3/m2
    !        it is equation 53 in meylan et al. (2018) and equation 16 in
    !        liu et al. (2020).
    !        this method is functionally the same as the "m2" model in ic5
    !        in ww3 (ic5 w/ic5vemod=3) and is redundantly included here as
    !        ic4m8 because it is in the same "family" as ic4m7 and ic4m9,
    !        being in the form of:
    !        ki=chf * h_ice^m * freq^n .
    !        calibrations:
    !        * liu et al. has chfm2=eta*(2*pi)^3/(1025*9.81^2)
    !        ** eta=14.0 for "sikuliaq" case of liu et al., so chfm2=0.035
    !        ** eta=3.0 for "sipex" case of liu et al., so chfm2=0.0075
    !        * rogers et al. (tech rep. 2021, "ryw2021") :
    !        ** fit to rogers et al. (crst 2021 "rmk2021") chfm2=0.059 (*sd*)
    !        suggested default is marked with "(*sd*)", for consistency
    !          with swan (v41.31ab or later)
    !
    !     9) rogers et al. (tech. rep. 2021, "ryw2021"): the "monomial power
    !        fit" described in section 2.2.3. it is the general form above,
    !        ki=chf * h_ice^m * freq^n but is constrained such that m=n/2-1.
    !        this constraint is derived by ryw2021 by invoking the scaling from
    !        yu et al. (2019), which is based on reynolds number with ice
    !        thickness as the relevant length scale.
    !        this is also given as equation 2 in yu et al. (crst 2022).
    !        some calibrations are as follows:
    !        * ryw2021, calibration to rmk2021: chf=2.9 and n=4.5  (*sd*)
    !        * yu et al. (2022) calibration to rmk2021 : chf=2.4 and n=4.46
    !          (noting that c_n=0.108 and chf=c_n*(2*pi/sqrt(g))^n)
    !        * yu (2022) adjusted the prior calibration to get better fit
    !          to higher frequency lab measurements and got:
    !          chf=7.89 and n=4.8
    !        suggested default is marked with "(*sd*)", for consistency
    !          with swan (v41.31ab or later)
    !
    !     ------------------------------------------------------------------
    !
    !     for all methods, the user can specify namelist
    !     variables ic4fmin and ic4kibk such as:
    !     &sic4 ic4method = [...], ic4fmin=0.08, ic4kibk=1.0e-7, [...]
    !     this accomodates the situation where the empirically-derived
    !     dissipation is uncertain for the lowest frequencies, which can be
    !     the case if estimated dissipation rate is so small that it falls
    !     in the noise level for the estimation method. (this is common,
    !     since some ice types cause only very weak dissipation
    !     to low frequencies.) in the example above, the amplitude
    !     dissipation rate ki is set to some low background level
    !     dissipation ic4kibk=1.0e-7 1/m when model frequency is less than
    !     0.08 hz.
    !
    !     more verbose description of implementation of sice in ww3:
    !      see documentation for ic1
    !
    !     notes regarding numerics:
    !      see documentation for ic1
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
    !     for questions, comments and/or corrections, please refer to:
    !        method 1 : c. collins
    !        method 2 : c. collins
    !        method 3 : c. collins
    !        method 4 : c. collins
    !        method 5 : e. rogers
    !        method 6 : e. rogers
    !        method 7 : e. rogers
    !
    !     alpha = 2 * wn_i
    !     though it may seem redundant/unnecessary to have *both* in the
    !       code, we do it this way to make the code easier to read and
    !       relate to other codes and source material, and hopefully avoid
    !       mistakes.
    !/ ------------------------------------------------------------------- /
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
    use w3gdatmd, only: nk, nth, nspec, sig, mapwn, ic4pars, dden, &
                        ic4_ki, ic4_fc, ic4_cn, nic4, ic4_fmin,    &
                        ic4_kibk
    use w3idatmd, only: icep1, icep2, icep3, icep4, icep5, &
                        mudt, mudv, mudd, inflags2
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
    integer                 :: ikth, ik, ith, ic4method, ifc
    real                    :: d1d(nk), eb(nk)
    real                    :: icecoef1, icecoef2, icecoef3, &
                               icecoef4, icecoef5, icecoef6, &
                               icecoef7, icecoef8
    real                    :: cice1,cice2,cice3,cice4,cice5 ! temporary variables
    real                    :: ki1,ki2,ki3,ki4,fc5,fc6,fc7
    real                    :: hs, emean, hice
    real                    :: chf,mpow,npow
    real, allocatable       :: wn_i(:)  ! exponential decay rate for amplitude
    real, allocatable       :: alpha(:) ! exponential decay rate for energy
    real, allocatable       :: freq(:) ! wave frequency
    real, allocatable       :: marg1(:), marg2(:) ! arguments for m2
    real, allocatable       :: karg1(:), karg2(:), karg3(:) !arguments for m3
    logical                 :: nml_input ! if using namelist input for m2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    d        = 0.0
    !
    allocate(wn_i(0:nk+1))
    allocate(alpha(0:nk+1))
    allocate(marg1(0:nk+1))
    allocate(marg2(0:nk+1))
    allocate(karg1(0:nk+1))
    allocate(karg2(0:nk+1))
    allocate(karg3(0:nk+1))
    allocate(freq(0:nk+1))
    marg1    = 0.0
    marg2    = 0.0
    karg1    = 0.0
    karg2    = 0.0
    karg3    = 0.0
    wn_i     = 0.0
    alpha    = 0.0
    icecoef1 = 0.0
    icecoef2 = 0.0
    icecoef3 = 0.0
    icecoef4 = 0.0
    icecoef5 = 0.0
    icecoef6 = 0.0
    icecoef7 = 0.0
    icecoef8 = 0.0
    hs       = 0.0
    hice     = 0.0
    emean    = 0.0
    freq=sig/tpi
    !
    !     if (.not.inflags2(-7))then
    !        write (ndse,1001) 'ice parameter 1'
    !        call extcde(201)
    !     endif
    !
    !   we cannot remove the other use of inflags below,
    !   because we would get 'array not allocated' error for the methods
    !   that don't use mudv, etc. and don't have mudv allocated.
    if (inflags2(-7)) icecoef1 = icep1(ix,iy) ! a.k.a. ic1
    if (inflags2(-6)) icecoef2 = icep2(ix,iy) ! etc.
    if (inflags2(-5)) icecoef3 = icep3(ix,iy)
    if (inflags2(-4)) icecoef4 = icep4(ix,iy)
    if (inflags2(-3)) icecoef5 = icep5(ix,iy)
    ! borrow from smud (error if bt8 or bt9)
    if (inflags2(-2)) icecoef6 = mudd(ix,iy) ! a.k.a. mdn
    if (inflags2(-1)) icecoef7 = mudt(ix,iy) ! a.k.a. mth
    if (inflags2(0 )) icecoef8 = mudv(ix,iy) ! a.k.a. mvs
    ic4method = ic4pars(1)
    !
    !
    ! 1.  make calculations ---------------------------------------------- /
    !
    ! 1.a calculate wn_i
    select case (ic4method)
    case (1) ! ic4m1 : exponential fit to wadhams et al. 1988
      alpha = exp(-icecoef1 * tpi / sig - icecoef2)
      wn_i = 0.5 * alpha
    case (2) ! ic4m2 : polynomial fit, eq. 3 from meylan et al. 2014
      !nb: eq. 3 only includes t^2 and t^4 terms,
      !  which correspond to icecoef3, icecoef5, so in
      !  regtest: icecoef1=icecoef2=icecoef4=0
      nml_input=.true.
      if (inflags2(-7).or.inflags2(-6).or.inflags2(-5).or. &
          inflags2(-4).or.inflags2(-3)) nml_input=.false.
      if(nml_input)then ! get from namelist array
         cice1=ic4_cn(1)
         cice2=ic4_cn(2)
         cice3=ic4_cn(3)
         cice4=ic4_cn(4)
         cice5=ic4_cn(5)
      else ! get from input-field array (icep1 etc.)
         cice1=icecoef1
         cice2=icecoef2
         cice3=icecoef3
         cice4=icecoef4
         cice5=icecoef5
      endif
      ! cice1 is c_{ice,1} in collins and rogers (2017), for example.
      marg1 = cice1 + cice2*freq + cice3*freq**2
      marg2 = cice4*freq**3 + cice5*freq**4
      alpha = marg1 + marg2
      wn_i = 0.5 * alpha
    case (3) ! ic4m3 : quadratic fit to kohout & meylan'08 in horvat & tziperman'15
      hice=icecoef1 ! for this method, icecoef1=ice thickness
      karg1 = -0.3203 + 2.058*hice - 0.9375*(tpi/sig)
      karg2 = -0.4269*hice**2 + 0.1566*hice*(tpi/sig)
      karg3 =  0.0006 * (tpi/sig)**2
      alpha  = exp(karg1 + karg2 + karg3)
      wn_i = 0.5 * alpha
    case (4) !eq. 1 from kohout et al. 2014
      !calculate hs
      do ik=1, nk
        eb(ik) = 0.
        do ith=1, nth
          eb(ik) = eb(ik) + a(ith+(ik-1)*nth)
        end do
      end do
      do ik=1, nk
        eb(ik) = eb(ik) * dden(ik) / cg(ik)
        emean  = emean + eb(ik)
      end do
      hs = 4.*sqrt( max(0.,emean) )
      ! if hs < 3 m then do hs dependent calc, otherwise dh/dx is a constant
      if (hs <= 3) then
        wn_i=icecoef1 ! from: dhdx=icecoef1*hs and wn_i=dhdx/hs
      else if (hs > 3) then
        wn_i=icecoef2/hs ! from: dhdx=icecoef2 and wn_i=dhdx/hs
      end if
    case (5) ! simple step function (time- and/or space-varying)
      ! rename variables for clarity
      ki1=icecoef1
      ki2=icecoef2
      ki3=icecoef3
      ki4=icecoef4
      fc5=icecoef5
      fc6=icecoef6
      fc7=icecoef7
      if((ki1.eq.0.0).or.(ki2.eq.0.0).or.(ki3.eq.0.0).or. &
           (ki4.eq.0.0).or.(fc5.eq.0.0).or.(fc6.eq.0.0).or. &
           (fc7.eq.0.0))then
        write (ndse,1001)'ice parameters'
        call extcde(201)
      end if
      do ik=1, nk
        ! select ki
        if(freq(ik).lt.fc5)then
          wn_i(ik)=ki1
        elseif(freq(ik).lt.fc6)then
          wn_i(ik)=ki2
        elseif(freq(ik).lt.fc7)then
          wn_i(ik)=ki3
        else
          wn_i(ik)=ki4
        endif
      end do
    case (6) ! simple step function (from namelist)
      ! error checking: require at least 3 steps
      if((ic4_ki(1).eq.0.0).or.(ic4_ki(2).eq.0.0).or. &
           (ic4_ki(3).eq.0.0).or.(ic4_fc(1).eq.0.0).or. &
           (ic4_fc(2).eq.0.0) )then
        write (ndse,1001)'ice parameters'
        call extcde(201)
      end if
      do ik=1, nk
        ! select ki
        do ifc=1,nic4
          if(freq(ik).lt.ic4_fc(ifc))then
            wn_i(ik)=ic4_ki(ifc)
            exit
          end if
        end do
      end do
    case (7) ! doble et al. (grl 2015)
      hice=icecoef1 ! for this method, icecoef1=ice thickness
      do ik=1,nk
        alpha(ik)  = 0.2*(freq(ik)**2.13)*hice
      end do
      wn_i= 0.5 * alpha
    case (8) ! meylan et al. (jgr 2018), liu et al. (jpo 2020)
      nml_input=.true.
      if (inflags2(-6)) nml_input=.false.
      if(nml_input)then ! get from namelist array
        chf=ic4_cn(1) ! denoted "chfm2" in documentation
      else ! get from input-field array (icep1 etc.)
        chf=icecoef2 ! denoted "chfm2" in documentation
      endif
      ! rename variable, for clarity
      hice=icecoef1 ! for this method, icecoef1 is ice thickness
      do ik=1,nk
        wn_i(ik)  = chf*hice*(freq(ik)**3)
      end do
    case (9) ! rogers et al. (2021) (ryw2021), yu et al. (jgr 2022)
      nml_input=.true.
      if (inflags2(-6).or.inflags2(-5)) nml_input=.false.
      if(nml_input)then ! get from namelist array
        chf=ic4_cn(1) ! denoted as same in documentation
        npow=ic4_cn(2) ! denoted "n" in documentation
      else ! get from input-field array (icep1 etc.)
        chf=icecoef2 ! denoted as same in documentation
        npow=icecoef3 ! denoted "n" in documentation
      endif
      ! rename variable, for clarity
      hice=icecoef1 ! for this method, icecoef1 is ice thickness
      ! compute
      mpow=0.5*npow-1.0 ! denoted "m" in documentation
      do ik=1,nk
        wn_i(ik)  = chf*(hice**mpow)*(freq(ik)**npow)
      end do
    case default
      wn_i = icecoef1 !default to ic1: uniform in k
    end select
    !
    ! 1.b calculate did
    !
    do ik=1, nk
      !   sbt1 has: d1d(ik) = factor *  max(0., (cg(ik)*wn(ik)/sig(ik)-0.5) )
      !             recall that d=s/e=-2*cg*k_i
      if(freq(ik).lt.ic4_fmin)wn_i(ik)=ic4_kibk
      !           write(*,*)freq(ik),wn_i(ik),icecoef1,' % :: freq,ki,hice' ! temporary code: do not commit to repo uncommented
      d1d(ik) = -2. * cg(ik) * wn_i(ik)
    end do
    !
    ! 1.c fill diagional matrix
    !
    do ikth=1, nspec
      d(ikth) = d1d(mapwn(ikth))
    end do
    !
    !      end if
    !
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    ! formats
    !
1001 format (/' *** wavewatch iii error in w3sic4 : '/               &
        '     ',a,' required but not selected'/)
    !
    !/
    !/ end of w3sic4 --------------------------------------------------- /
    !/
  end subroutine w3sic4
  !/
  !/ end of module w3sic4md ------------------------------------------ /
  !/
end module w3sic4md
