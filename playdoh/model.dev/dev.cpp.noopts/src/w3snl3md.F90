!> @file
!> @brief generalized and optimized multiple dia implementation.
!>
!> @author h. l. tolman
!> @date   13-jul-2012
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
!> @brief generalized and optimized multiple dia implementation.
!>
!> @details expressions in terms of original f(f,theta) spectrum.
!>
!> @author h. l. tolman
!> @date   13-jul-2012
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3snl3md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch-iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         13-jul-2012 |
  !/                  +-----------------------------------+
  !/
  !/    21-jul-2008 : origination as nlx option.          ( version 3.13 )
  !/    03-jan-2009 : bug fixes insnlx.                   ( version 3.13 )
  !/                  see remarks section for module.
  !/    25-aug-2009 : conversion to f(f,theta) form.      ( version 3.13 )
  !/    13-nov-2009 : bug fix delth in initialization.    ( version 3.13 )
  !/    01-dec-2009 : bug fix frequency filtering.        ( version 3.13 )
  !/    13-aug-2010 : move to nl3.                        ( version 3.15 )
  !/    13-jul-2012 : moved from version 3.15 to 4.08.    ( version 4.08 )
  !/
  !/    copyright 2008-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !
  !  1. purpose :
  !
  !     generalized and optimized multiple dia implementation.
  !     expressions in terms of original f(f,theta) spectrum.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nkd       i.p.  private  number of nondimensional depths in
  !                               storage array.
  !      kdmin     r.p.  private  minimum relative depth in table.
  !      kdmax     r.p.  private  maximum relative depth in table.
  !      lammax    r.p.  public   maximum value for lambda or mu.
  !      delthm    r.p.  public   maximum angle gap (degree).
  !      sitmin    real  private  minimum nondimensional radian
  !                               frequency in table.
  !      xsit      real  private  corresponding increment factor.
  !     ----------------------------------------------------------------
  !
  !     see w3snl3 and insnl3 for documentation of variables in w3gdatmd
  !     as used here.
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3snl3    subr. public   multiple dia for arbitrary depth.
  !      expand    subr. w3snl3   expand spectrum for indirect address.
  !      expnd2    subr. w3snl3   expand snl and d contributions.
  !      insnl3    subr. public   corresponding initialization routine.
  !      minlam    r.f.  insnl3   minimum lambda for quadruplet.
  !      maxlam    r.f.  insnl3   maximum lambda for quadruplet.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w2servmd program abort.
  !      wavnu1    subr. w3dispmd solve dispersion relation.
  !      wavnu2    subr. w3dispmd solve dispersion relation.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - filtering techniques for computation of quadruplet spectral
  !       values and distribution in spectral space have been tested
  !       but were not found worth the large coding effort involved.
  !     - wavnu1 is used in w3snl3  for consistency with spectral grid
  !       description.
  !     - wavnu2 is used in insnl3  for accuracy in the computation of
  !       the layout of the quadruplets (higher computational cost is
  !       not an issue with initialization routine).
  !     - for large lambda or mu the original maximum kd = 10. still
  !       leads to significantly different quadruplet layout in
  !       secion 3. to remedy this, the orriginal settings of the
  !       lookup tables
  !
  !     integer, private, parameter :: nkd = 250
  !     real, private, parameter    :: kdmin = 0.025 ,  kdmax = 10.
  !
  !       was reset to
  !
  !     integer, private, parameter :: nkd = 275
  !     real, private, parameter    :: kdmin = 0.025 ,  kdmax = 20.
  !
  !       for the bug fix of 03-jan-2009. note that with this, the
  !       estimate of nthmax in insnl3 also is needed to guarantee
  !       consistent nthmax and nthm2 for any lambda and mu.
  !
  !  6. switches :
  !
  !     !/s    enable subroutine tracing.
  !     !/tn   test output (see main subroutines).
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  integer, private, parameter :: nkd = 275
  real, private, parameter    :: kdmin = 0.025 ,  kdmax = 20.
  real, public, parameter     :: lammax = 0.49999
  real, public, parameter     :: delthm = 90.
  !
  real, private               :: sitmin, xsit
  !
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief multiple discrete interaction parameterization for arbitrary
  !>  depths with generalized quadruplet layout.
  !>
  !> @details this is a direct implementation of the discrete interaction
  !>  paramterization (dia) with multiple representative quadruplets
  !>  (mdia) for arbitrary water depths.
  !>
  !>  the outer loop of the code is over quadruplet realizations,
  !>  which implies two realizations for a conventional quadruplet
  !>  definitions and four for extended definitions (with rescaling
  !>  of the contants for consistency). within this loop the compu-
  !>  tations are performed in two stages. first, interactions
  !>  contributions are computed for the entire spectral space,
  !>  second all contributions are combined into the actual inter-
  !>  actions and diagonal contributions.
  !>
  !>  arbitrary depths are addressed by generating a lookup table
  !>  for the relative depth. these tables are used for each discrete
  !>  frequency separately. efficient memory usages requires relative
  !>  addressing to reduce the size of the lookup tables. to use this
  !>  the spectral space is expanded to higher and lower frequencies
  !>  as well as directional space is expanded/volded. this is done
  !>  for the input (pseudo-) spectrum (action spectrum devided by the
  !>  wavenumber) to determine spectral densities at the quadruplet
  !>  components, and the spectral space describing individual contri-
  !>  butions before they are combined into the actual interactions.
  !>
  !> @param[in]  a       action spectrum a(ith,ik) as a function of
  !>                     direction (rad)  and wavenumber.
  !> @param[in]  cg      group velocities (dimension nk).
  !> @param[in]  wn      wavenumbers (dimension nk).
  !> @param[in]  depth   water depth in meters.
  !> @param[out] s       source term.
  !> @param[out] d       diagonal term of derivative.
  !>
  !> @author h. l. tolman
  !> @date   01-dec-2009
  !>
  subroutine w3snl3 (  a, cg, wn, depth, s, d )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-dec-2009 |
    !/                  +-----------------------------------+
    !/
    !/    21-jul-2008 : origination as nlx option.          ( version 3.13 )
    !/    25-aug-2009 : conversion to f(f,theta) form.      ( version 3.13 )
    !/    01-dec-2009 : bug fix frequency filtering.        ( version 3.13 )
    !/
    !  1. purpose :
    !
    !     multiple discrete interaction parameterization for arbitrary
    !     depths with generalized quadruplet layout.
    !
    !  2. method :
    !
    !     this is a direct implementation of the discrete interaction
    !     paramterization (dia) with multiple representative quadruplets
    !     (mdia) for arbitrary water depths.
    !
    !     the outer loop of the code is over quadruplet realizations,
    !     which implies two realizations for a conventional quadruplet
    !     definitions and four for extended definitions (with rescaling
    !     of the contants for consistency). within this loop the compu-
    !     tations are performed in two stages. first, interactions
    !     contributions are computed for the entire spectral space,
    !     second all contributions are combined into the actual inter-
    !     actions and diagonal contributions.
    !
    !     arbitrary depths are addressed by generating a lookup table
    !     for the relative depth. these tables are used for each discrete
    !     frequency separately. efficient memory usages requires relative
    !     addressing to reduce the size of the lookup tables. to use this
    !     the spectral space is expanded to higher and lower frequencies
    !     as well as directional space is expanded/volded. this is done
    !     for the input (pseudo-) spectrum (action spectrum devided by the
    !     wavenumber) to determine spectral densities at the quadruplet
    !     components, and the spectral space describing individual contri-
    !     butions before they are combined into the actual interactions.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action spectrum a(ith,ik) as a function of
    !                         direction (rad)  and wavenumber.
    !       cg      r.a.  i   group velocities (dimension nk).
    !       wn      r.a.  i   wavenumbers (dimension nk).
    !       depth   real  i   water depth in meters.
    !       s       r.a.  o   source term.
    !       d       r.a.  o   diagonal term of derivative.
    !     ----------------------------------------------------------------
    !
    !     variables describing the expanded frequency space from the
    !     dynamic storage in w3gdatmd.
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      nfr       int.  public   number of frequencies or wavenumbers
    !                               in discrete spectral space (nfr=>nk).
    !      nfrmin    int.  public   minimum discrete frequency in the
    !                               expanded frequency space.
    !      nfrmax    int.  public   idem maximum for first part.
    !      nfrcut    int.  public   idem maximum for second part.
    !      nthmax    int.  public   extension of directional space.
    !      nthexp    int   public   number of bins in extended dir. space.
    !      nspmin, nspmax, nspmx2
    !                int.  public   1d spectral space range.
    !      frq       r.a.  public   expanded frequency range (hz).
    !      xsi       r.a.  public   expanded frequency range (rad/s).
    !     ----------------------------------------------------------------
    !
    !     variables describing lookup tables.
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      nqa       int.  public   number of actual quadruplets.
    !      qst1      i.a.  public   spectral offsets for compuation of
    !                               quadruplet spectral desnities.
    !      qst2      r.a.  public   idem weights.
    !      qst3      r.a.  public   norm. factors in product term and
    !                               in diagonal strength.
    !      qst4      i.a.  public   spectral offsets for combining of
    !                               interactions and diagonal.
    !      qst5      r.a.  public   idem weights for interactions.
    !      qst6      r.a.  public   idem weights for diagonal.
    !     ----------------------------------------------------------------
    !
    !     variables describing model setup.
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      snlmsc    real  public   tuning power 'deep' scaling.
    !      snlnsc    real  public   tuning power 'shallow' scaling.
    !      snlsfd    real  public   'deep' nondimensional filer freq.
    !      snlsfs    real  public   'shallow' nondimensional filer freq.
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
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     - note that this code uses explicit unroling of potential loop
    !       structures for optimization purposes.
    !     - normalization with respect to the number of quadruplets is
    !       included in the proportionality constant.
    !     - note that the outer loop in the routine considers one actual
    !       quadruplet realization per loop cycle. for the traditional
    !       quadruplet layout two realizations occure, for the expanded
    !       four realizations occur. for consistency, strength of a
    !       traditional layout is therefore doubled.
    !     - 1d representation is used of 2d spectral space for optimization
    !       purposes.
    !     - contributions are first computed in the convetional spectral
    !       space and are then expancded "in place" into the expanded
    !       spectral space in expnd2.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nfr => nk, nth, sig, fachfe, facti1, facti2,&
         nfrmin, nfrmax, nfrcut, nthmax, nthexp,     &
         nspmin, nspmax, nspmx2, frq, xsi, nqa,      &
         qst1, qst2, qst3, qst4, qst5, qst6, snlmsc, &
         snlnsc, snlsfd, snlsfs
    use w3odatmd, only: ndse, ndst
    !
    use w3servmd, only: extcde
    use w3dispmd, only: wavnu1, wavnu3
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)   :: a(nth,nfr), cg(nfr), wn(nfr), depth
    real, intent(out)  :: s(nth,nfr), d(nth,nfr)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer            :: ifr, ierr, ikd, jkd(nfrcut), iqa, if1min, &
         if1max, if2min, if2max, isp0, ispx0, ith, &
         isp, ispx
    integer            :: lqst1(16), lqst4(16)
    real               :: xsitln, sit, fprop, fq1, fq2, fq3, fq4,   &
         aux1, aux2
    real               :: xwn(nfrmax), xcg(nfrmax), scale1(nfrcut), &
         scale2(nfrcut), lqst2(16), fact(6),       &
         lqst5(16), lqst6(16)
    real               :: ue(nspmin:nspmax), dsb(nspmin:nspmx2),    &
         dd1(nspmin:nspmx2), dd2(nspmin:nspmx2),   &
         dd3(nspmin:nspmx2), dd4(nspmin:nspmx2)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  initialization ------------------------------------------------- *
    ! 1.a constants and arrays
    !
    xsitln = log(xsit)
    !
    s      = 0.
    d      = 0.
    !     dsb    = 0.
    !     dd1    = 0.
    !     dd2    = 0.
    !     dd3    = 0.
    !     dd4    = 0.
    !
    ! 1.a extended frequency range
    !
    xwn(1:nfr) = wn
    xcg(1:nfr) = cg
    !
    do ifr = nfr+1, nfrmax
      call wavnu1(xsi(ifr), depth, xwn(ifr), xcg(ifr))
    end do
    !
    ! 1.b expanded pseudo spetrum
    !
    call expand ( ue )
    !
    ! 1.c set up scaling functions
    !
    aux1   = 1. / ( tpi**11 * grav**(4.-snlmsc) )
    aux2   = grav**2 / tpi**11
    !
    do ifr=1, nfrcut
      scale1(ifr) = aux1 * xwn(ifr)**(4.+snlmsc) *                 &
           xsi(ifr)**(13.-2.*snlmsc) / xcg(ifr)**2
      scale2(ifr) = aux2 * xwn(ifr)**11 *                          &
           (xwn(ifr)*depth)**snlnsc / xcg(ifr)
    end do
    !
    ! 1.d set up depth scaling counters
    !
    do ifr=1, nfrcut
      sit      = xsi(ifr) * sqrt(depth/grav)
      ikd      = 1 + nint ( ( log(sit) - log(sitmin) ) / xsitln )
      jkd(ifr) = max ( 1 , min(ikd,nkd) )
    end do
    !
    ! 2.  base loop over quadruplet realizations ------------------------- *
    !
    do iqa=1 , nqa
      !
      ! 3.  obtain quadruplet energies for all spectral bins --------------- *
      ! 3.a set frequency ranges
      !
      aux1   = qst3(5,iqa,1)
      aux2   = qst3(6,iqa,1)
      !
      if1min = 1
      if1max = nfrcut
      if2min = 1
      if2max = nfr
      !
      if ( aux1 .le. 0. .and. aux2 .le. 0. ) then
        !
        cycle
        !
      else if ( aux2 .le. 0. ) then
        !
        sit    = snlsfd * sqrt(grav/depth)
        ifr    = nint ( facti2 + facti1*log(sit) )
        if ( ifr .gt. nfr ) cycle
        !
        if ( ifr .gt. 1 ) then
          if1min = max ( 1 , ifr )
          if2min = max ( 1 , if1min + nfrmin )
          dsb(1:(if1min-1)*nth) = 0.
          dd1(1:(if1min-1)*nth) = 0.
          dd2(1:(if1min-1)*nth) = 0.
          dd3(1:(if1min-1)*nth) = 0.
          dd4(1:(if1min-1)*nth) = 0.
        end if
        !
      else if ( aux1 .le. 0. ) then
        !
        sit    = snlsfs * sqrt(grav/depth)
        ifr    = nint ( facti2 + facti1*log(sit) )
        if ( ifr .lt. 1 ) cycle
        !
        if ( ifr .lt. nfrcut ) then
          if1max = min ( nfrcut, ifr )
          !               if2max = nfr
          dsb(if1max*nth+1:nfrcut*nth) = 0.
          dd1(if1max*nth+1:nfrcut*nth) = 0.
          dd2(if1max*nth+1:nfrcut*nth) = 0.
          dd3(if1max*nth+1:nfrcut*nth) = 0.
          dd4(if1max*nth+1:nfrcut*nth) = 0.
        end if
        !
      end if
      !
      ! 3.b loop over frequencies
      !
      do ifr=if1min, if1max
        !
        ! 3.c find discrete depths
        !
        ikd    = jkd(ifr)
        !
        ! 3.d get offsets and weights
        !
        lqst1  = qst1(:,iqa,ikd)
        lqst2  = qst2(:,iqa,ikd)
        fact   = qst3(:,iqa,ikd)
        fact(1:4) = fact(1:4) * xcg(ifr) / ( xwn(ifr) *xsi(ifr) )
        fprop  = scale1(ifr)*fact(5) + scale2(ifr)*fact(6)
        !
        ! 3.e loop over directions
        !
        isp0   = (ifr-1)*nth
        ispx0  = (ifr-1)*nthexp
        !
        do ith=1, nth
          !
          isp    = isp0 + ith
          ispx   = ispx0 + ith
          !
          fq1    = ( ue(ispx+lqst1( 1)) * lqst2( 1) +               &
               ue(ispx+lqst1( 2)) * lqst2( 2) +               &
               ue(ispx+lqst1( 3)) * lqst2( 3) +               &
               ue(ispx+lqst1( 4)) * lqst2( 4) ) * fact(1)
          fq2    = ( ue(ispx+lqst1( 5)) * lqst2( 5) +               &
               ue(ispx+lqst1( 6)) * lqst2( 6) +               &
               ue(ispx+lqst1( 7)) * lqst2( 7) +               &
               ue(ispx+lqst1( 8)) * lqst2( 8) ) * fact(2)
          fq3    = ( ue(ispx+lqst1( 9)) * lqst2( 9) +               &
               ue(ispx+lqst1(10)) * lqst2(10) +               &
               ue(ispx+lqst1(11)) * lqst2(11) +               &
               ue(ispx+lqst1(12)) * lqst2(12) ) * fact(3)
          fq4    = ( ue(ispx+lqst1(13)) * lqst2(13) +               &
               ue(ispx+lqst1(14)) * lqst2(14) +               &
               ue(ispx+lqst1(15)) * lqst2(15) +               &
               ue(ispx+lqst1(16)) * lqst2(16) ) * fact(4)
          !
          aux1   = fq1 * fq2 * ( fq3 + fq4 )
          aux2   = fq3 * fq4 * ( fq1 + fq2 )
          dsb(isp) = fprop * ( aux1 - aux2 )
          !
          aux1   = fq3 + fq4
          aux2   = fq3 * fq4
          dd1(isp) = fprop * fact(1) * ( fq2 * aux1 - aux2 )
          dd2(isp) = fprop * fact(2) * ( fq1 * aux1 - aux2 )
          !
          aux1   = fq1 + fq2
          aux2   = fq1 * fq2
          dd3(isp) = fprop * fact(3) * ( aux2 - fq4*aux1 )
          dd4(isp) = fprop * fact(4) * ( aux2 - fq3*aux1 )
          !
          ! ... end loop 3.e
          !
        end do
        !
        ! ... end loop 3.b
        !
      end do
      !
      ! 3.e expand arrays
      !
      call expnd2 ( dsb(1:nth*nfrcut), dsb )
      call expnd2 ( dd1(1:nth*nfrcut), dd1 )
      call expnd2 ( dd2(1:nth*nfrcut), dd2 )
      call expnd2 ( dd3(1:nth*nfrcut), dd3 )
      call expnd2 ( dd4(1:nth*nfrcut), dd4 )
      !
      ! 4.  put it all together -------------------------------------------- *
      ! 4.a loop over frequencies
      !
      do ifr=if2min, if2max
        !
        ! 4.b find discrete depths and storage
        !
        ikd    = jkd(ifr)
        !
        ! 4.c get offsets and weights
        !
        lqst4  = qst4(:,iqa,ikd)
        lqst5  = qst5(:,iqa,ikd)
        lqst6  = qst6(:,iqa,ikd)
        !
        ! 4.d loop over directions
        !
        ispx0  = (ifr-1)*nthexp
        !
        do ith=1, nth
          !
          ispx   = ispx0 + ith
          !
          s(ith,ifr) = s(ith,ifr) + dsb(ispx+lqst4( 1)) * lqst5( 1) &
               + dsb(ispx+lqst4( 2)) * lqst5( 2) &
               + dsb(ispx+lqst4( 3)) * lqst5( 3) &
               + dsb(ispx+lqst4( 4)) * lqst5( 4) &
               + dsb(ispx+lqst4( 5)) * lqst5( 5) &
               + dsb(ispx+lqst4( 6)) * lqst5( 6) &
               + dsb(ispx+lqst4( 7)) * lqst5( 7) &
               + dsb(ispx+lqst4( 8)) * lqst5( 8) &
               + dsb(ispx+lqst4( 9)) * lqst5( 9) &
               + dsb(ispx+lqst4(10)) * lqst5(10) &
               + dsb(ispx+lqst4(11)) * lqst5(11) &
               + dsb(ispx+lqst4(12)) * lqst5(12) &
               + dsb(ispx+lqst4(13)) * lqst5(13) &
               + dsb(ispx+lqst4(14)) * lqst5(14) &
               + dsb(ispx+lqst4(15)) * lqst5(15) &
               + dsb(ispx+lqst4(16)) * lqst5(16)
          !
          d(ith,ifr) = d(ith,ifr) + dd1(ispx+lqst4( 1)) * lqst6( 1) &
               + dd1(ispx+lqst4( 2)) * lqst6( 2) &
               + dd1(ispx+lqst4( 3)) * lqst6( 3) &
               + dd1(ispx+lqst4( 4)) * lqst6( 4) &
               + dd2(ispx+lqst4( 5)) * lqst6( 5) &
               + dd2(ispx+lqst4( 6)) * lqst6( 6) &
               + dd2(ispx+lqst4( 7)) * lqst6( 7) &
               + dd2(ispx+lqst4( 8)) * lqst6( 8) &
               + dd3(ispx+lqst4( 9)) * lqst6( 9) &
               + dd3(ispx+lqst4(10)) * lqst6(10) &
               + dd3(ispx+lqst4(11)) * lqst6(11) &
               + dd3(ispx+lqst4(12)) * lqst6(12) &
               + dd4(ispx+lqst4(13)) * lqst6(13) &
               + dd4(ispx+lqst4(14)) * lqst6(14) &
               + dd4(ispx+lqst4(15)) * lqst6(15) &
               + dd4(ispx+lqst4(16)) * lqst6(16)
          !
          ! ... end loop 4.d
          !
        end do
        !
        ! ... end loop 4.a
        !
      end do
      !
      ! ... end of loop 2.
      !
    end do
    !
    ! 5.  convert back to wave action ------------------------------------ *
    !
    do ifr=if2min, if2max
      s(:,ifr) = s(:,ifr) / xsi(ifr) * xcg(ifr) * tpiinv
    end do
    !
    return
    !/
    !/ embedded subroutines
    !/
  contains
    !/ ------------------------------------------------------------------- /
    !>
    !> @brief expand spectrum, subroutine used to simplify addressing.
    !>
    !> @param[out] spec  expanded spectrum.
    !>
    !> @author h. l. tolman
    !> @date   21-aug-2009
    !>
    subroutine expand ( spec )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch-iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         21-aug-2009 |
      !/                  +-----------------------------------+
      !/
      !/    03-jul-2008 : origination.                        ( version 3.13 )
      !/    21-aug-2009 : conversion to f(f,theta) form.      ( version 3.13 )
      !/
      !  1. purpose :
      !
      !     expand spectrum, subroutine used to simplify addressing.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       spec    r.a.  o   expanded spectrum.
      !     ----------------------------------------------------------------
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      implicit none
      !/
      !/ parameter list
      !/
      real, intent(out)       :: spec(1-nthmax:nth+nthmax,nfrmin:nfrmax)
      !/
      !/ local parameters
      !/
      integer                 :: ifr, ith
      !/
      !/ ------------------------------------------------------------------- /
      !
      spec(:,nfrmin:0) = 0.
      !
      spec(1:nth,1:nfr) = a * tpi
      !
      do ifr=1, nfr
        spec(1:nth,ifr) = spec(1:nth,ifr) * xsi(ifr) / xcg(ifr)
      end do
      !
      do ifr=nfr+1, nfrmax
        spec(1:nth,ifr) = spec(1:nth,ifr-1) * fachfe
      end do
      !
      do ith=1, nthmax
        spec(nth+ith,1:nfrmax) = spec(   ith   ,1:nfrmax)
        spec( 1 -ith,1:nfrmax) = spec(nth+1-ith,1:nfrmax)
      end do
      !
      return
      !/
      !/ end of expand ----------------------------------------------------- /
      !/
    end subroutine expand
    !/ ------------------------------------------------------------------- /
    !>
    !> @brief expand spectrum to simplify indirect addressing.
    !>
    !> @details done 'in place' with temporary array ( arin = arout ).
    !>
    !> @param[in]  arin   input array.
    !> @param[out] arout  output array.
    !>
    !> @author h. l. tolman
    !> @date   16-jul-2008
    !>
    subroutine expnd2 ( arin, arout )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch-iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         16-jul-2008 |
      !/                  +-----------------------------------+
      !/
      !/    16-jul-2008 : origination.                        ( version 3.13 )
      !/
      !  1. purpose :
      !
      !     expand spectrum to simplify indirect addressing.
      !     done 'in place' with temporary array ( arin = arout )
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       spin    r.a.  i   input array.
      !       spout   r.a.  i   output array.
      !     ----------------------------------------------------------------
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      implicit none
      !/
      !/ parameter list
      !/
      real, intent(in)      :: arin(nth,nfrcut)
      real, intent(out)     :: arout(1-nthmax:nth+nthmax,nfrmin:nfrcut)
      !/
      !/ local parameters
      !/
      integer               :: ifr, ith
      real                  :: temp(nth,nfrcut)
      !/
      !/ ------------------------------------------------------------------- /
      !
      temp   = arin
      !
      arout(:,nfrmin:0) = 0.
      !
      arout(1:nth,1:nfrcut) = temp
      !
      do ith=1, nthmax
        arout(nth+ith,1:nfrcut) = arout(   ith   ,1:nfrcut)
        arout( 1 -ith,1:nfrcut) = arout(nth+1-ith,1:nfrcut)
      end do
      !
      return
      !/
      !/ end of expnd2 ----------------------------------------------------- /
      !/
    end subroutine expnd2
    !/
    !/ end of w3snl3 ----------------------------------------------------- /
    !/
  end subroutine w3snl3
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialization for generalized multiple dia routine.
  !>
  !> @details fill storage arrays as described in the main subroutine
  !>  with interpolation, model and distribution data.
  !>
  !> @author h. l. tolman
  !> @date   13-nov-2009
  !>
  subroutine insnl3
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-nov-2009 |
    !/                  +-----------------------------------+
    !/
    !/    21-jul-2008 : origination as nlx option.          ( version 3.13 )
    !/    03-jan-2009 : bug fixes nthmax and nthmx2.        ( version 3.13 )
    !/    21-aug-2009 : conversion to f(f,theta) form.      ( version 3.13 )
    !/    13-nov-2009 : harden delth computation.           ( version 3.13 )
    !/
    !  1. purpose :
    !
    !     initialization for generalized multiple dia routine.
    !
    !  2. method :
    !
    !     fill storage aryays as described in the main subroutine with
    !     interpolation, model and distribution data.
    !
    !  3. parameters :
    !
    !     variables in w3gdatmd describing model setup.
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      snlnq     int.  public   number of quadruplet definitions.
    !      snll      r.a.  public   array with lambda for quadruplet.
    !      snlm      r.a.  public   array with mu for quadruplet.
    !      snlt      r.a.  public   array with dtheta for quadruplet.
    !      snlcd     r.a.  public   array with cd for quadruplet.
    !      snlcs     r.a.  public   array with cs for quadruplet.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      extcde    subr. w3servmd program abort.
    !      wavnu2    subr. w3dispmd solve dispersion relation.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd process model definiton file.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     see error escape location.
    !
    !  8. remarks :
    !
    !     - allocation of arrays directly done in data structure, using
    !       igrid and resetting pointer of aliaases.
    !     - in the 03-jan-2009 bug fix !/t3 error output was fixed, and
    !       nthmax is increased by 1 to assure that nthmx2 .le. nthmax
    !       for any lambda and mu. with this, the label 810 test is
    !       changed from equality testing to .le. testing.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    general test output.
    !     !/t1   filling of lookup table for quadruplet and interaction
    !            strength.
    !     !/t2   filling of lookup table for combining interactions.
    !     !/t3   display raw lookup table of second type.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3odatmd, only: ndse, ndst
    use w3gdatmd,       nfr => nk
    !
    use w3dispmd, only: wavnu2
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
    integer                 :: ifrmin, ifrmax, ikd, ierr, iq, nqd,  &
         nqs, j, ifr, iqa, jj, jf, nthmx2,    &
         jiq, jof, jqr, ist
    integer                 :: jfr(4), jfr1(4), jth(4), jth1(4)
    integer, allocatable    :: ast1(:,:,:), ast2(:,:,:)
    real                    :: sitmax, xfrln
    real                    :: off12, off34, th12, depth,           &
         s0, s1, s2, s3, s4, auxfr(4),        &
         wn0, wn1, wn2, wn3, wn4,             &
         cg0, cg1, cg2, cg3, cg4, auxf,       &
         aa, bb, cc, delth(4), aux1, aux2,    &
         wfr(4), wfr1(4), wth(4), wth1(4),    &
         wfroff, sioff, wf
    !
    type qst
      integer               :: ofr(4), ofr1(4), oth(4), oth1(4)
      real                  :: hfr(4), hfr1(4), hth(4), hth1(4)
      real                  :: f1, f2, f3, f4, cqd, cqs
    end type qst
    !
    type(qst), allocatable  :: tstore(:,:)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  initialization ------------------------------------------------- *
    ! 1.a checks
    !
    xfrln  = log(xfr)
    !
    if ( lammax.le.0. .or. lammax.gt.0.5 .or. delthm.lt.0. ) goto 800
    !
    ! 1.b set up relative depths
    !
    allocate ( tstore(snlnq*4,1:nkd) )
    !
    depth  = 1.
    sitmin = sqrt ( kdmin * tanh(kdmin) )
    sitmax = sqrt ( kdmax * tanh(kdmax) )
    xsit   = (sitmax/sitmin)**(1./real(nkd-1))
    !
    !
    ! 2.  building quadruplet data base ---------------------------------- *
    !     for quadruplet and interaction strength evaluation
    !
    ifrmin = 0
    ifrmax = 0
    nthmax = 0
    !
    ! 2.a loop over relative depths
    !
    s0     = sitmin * sqrt ( grav / depth )  / xsit
    !
    do ikd=1, nkd
      !
      s0     = s0 * xsit
      call wavnu2 ( s0, depth, wn0, cg0, 1.e-6, 25, ierr)
      !
      ! 2.b loop over representative quadruplets
      !
      nqa    = 0
      nqd    = 0
      nqs    = 0
      !
      do iq=1, snlnq
        !
        !
        off12  = snlm(iq)
        off34  = snll(iq)
        th12   = snlt(iq) * dera
        if ( snlcd(iq) .gt. 0. ) nqd = nqd + 1
        if ( snlcs(iq) .gt. 0. ) nqs = nqs + 1
        !
        if ( th12 .lt. 0. ) then
          if ( off12.lt.0. .or. off12.gt.0.5 .or.                 &
               off34.lt.0. .or. off34.gt.0.5 ) goto 801
        else
          if ( snlt(iq).gt.delthm .or. off12.lt.0. .or.           &
               off12.ge.1.                                        &
               .or.  off34.lt.minlam(off12,snlt(iq)) .or.         &
               off34.gt.maxlam(off12,snlt(iq)) ) goto 802
        end if
        !
        !
        ! 2.c offset angles
        !
        s1     = s0 * ( 1. + off12 )
        call wavnu2 ( s1, depth, wn1, cg1, 1.e-6, 25, ierr)
        s2     = s0 * ( 1. - off12 )
        call wavnu2 ( s2, depth, wn2, cg2, 1.e-6, 25, ierr)
        s3     = s0 * ( 1. + off34 )
        call wavnu2 ( s3, depth, wn3, cg3, 1.e-6, 25, ierr)
        s4     = s0 * ( 1. - off34 )
        call wavnu2 ( s4, depth, wn4, cg4, 1.e-6, 25, ierr)
        !
        auxfr(1) = s1 / s0
        auxfr(2) = s2 / s0
        auxfr(3) = s3 / s0
        auxfr(4) = s4 / s0
        !
        if ( th12 .lt. 0. ) then
          bb = 2. * wn0
        else
          bb = wn1**2 + wn2**2 + 2.*wn1*wn2*cos(th12)
          bb = sqrt ( max ( bb , 0. ) )
        end if
        !
        if ( th12.lt.0. .and. abs(off12).le.1.e-4 ) then
          delth(1) = 0.
          delth(2) = 0.
        else
          cc       = wn1
          aa       = wn2
          aux1     = (cc**2+bb**2-aa**2) / (2.*bb*cc)
          aux2     = (aa**2+bb**2-cc**2) / (2.*bb*aa)
          delth(1) = - acos( max ( 0. , min ( 1. , aux1 ) ) )
          delth(2) =   acos( max ( 0. , min ( 1. , aux2 ) ) )
        end if
        cc       = wn3
        aa       = wn4
        aux1     = (cc**2+bb**2-aa**2) / (2.*bb*cc)
        aux2     = (aa**2+bb**2-cc**2) / (2.*bb*aa)
        delth(3) = - acos( max ( 0. , min ( 1. , aux1 ) ) )
        delth(4) =   acos( max ( 0. , min ( 1. , aux2 ) ) )
        !
        !
        ! 2.d frequency indices
        !
        do j=1, 4
          jfr (j) = int( log(auxfr(j)) / xfrln )
          jfr1(j) = jfr(j) + 1 * sign(1.,auxfr(j)-1.)
          wfr (j) = (xfr**jfr1(j)-auxfr(j))/(xfr**jfr1(j)-xfr**jfr(j))
          wfr1(j) = 1. - wfr(j)
        end do
        !
        ifrmin = min ( ifrmin , minval(jfr1) )
        ifrmax = max ( ifrmax , maxval(jfr1) )
        !
        !
        ! 2.e directional indices
        !
        do j=1, 4
          aux1    = delth(j) / dth
          jth (j) = int(aux1)
          jth1(j) = jth(j) + 1 * sign(1.,delth(j))
          wth1(j) = abs(aux1) - real(abs(jth(j)))
          wth (j) = 1. - wth1(j)
        end do
        !
        nthmax = max ( nthmax , maxval(abs(jth1)) )
        !
        !
        ! 2.f temp storage of data
        !
        if ( snlm(iq).eq.0. .and. snlt(iq).lt.0. ) then
          jj     = 2
        else
          jj     = 4
        end if
        !
        do j=1, jj
          select case (j)
          case (2)
            jth (3) = -jth (3)
            jth (4) = -jth (4)
            jth1(3) = -jth1(3)
            jth1(4) = -jth1(4)
          case (3)
            jth     = -jth
            jth1    = -jth1
          case (4)
            jth (3) = -jth (3)
            jth (4) = -jth (4)
            jth1(3) = -jth1(3)
            jth1(4) = -jth1(4)
          case default
          end select
          !
          nqa    = nqa + 1
          tstore(nqa,ikd)%ofr  = jfr
          tstore(nqa,ikd)%ofr1 = jfr1
          tstore(nqa,ikd)%hfr  = wfr
          tstore(nqa,ikd)%hfr1 = wfr1
          tstore(nqa,ikd)%oth  = jth
          tstore(nqa,ikd)%oth1 = jth1
          tstore(nqa,ikd)%hth  = wth
          tstore(nqa,ikd)%hth1 = wth1
          if ( jj .eq. 2 ) then
            tstore(nqa,ikd)%cqd  = snlcd(iq) * 2.
            tstore(nqa,ikd)%cqs  = snlcs(iq) * 2.
          else
            tstore(nqa,ikd)%cqd  = snlcd(iq)
            tstore(nqa,ikd)%cqs  = snlcs(iq)
          end if
          auxf                 = ( wn0 * s0 ) / cg0
          tstore(nqa,ikd)%f1   = auxf * cg1 / ( wn1 * s1 )
          tstore(nqa,ikd)%f2   = auxf * cg2 / ( wn2 * s2 )
          tstore(nqa,ikd)%f3   = auxf * cg3 / ( wn3 * s3 )
          tstore(nqa,ikd)%f4   = auxf * cg4 / ( wn4 * s4 )
          !
        end do
        !
        ! ... end loop 2.b
        !
      end do
      !
      ! ... end loop 2.a
      !
    end do
    !
    !
    ! 2.g expanded spectral range
    !
    nthmax = nthmax + 1
    !
    nfrmin =  1  + ifrmin
    nfrmax = nfr + ifrmax - ifrmin
    nfrcut = nfr          - ifrmin
    nthexp = nth + 2*nthmax
    !
    nspmin = 1 + (nfrmin-1)*nthexp - nthmax
    nspmax = nfrmax * nthexp - nthmax
    nspmx2 = nfrcut * nthexp - nthmax
    !
    !
    allocate ( mpars(igrid)%snlps%frq(nfrmax),                      &
         mpars(igrid)%snlps%xsi(nfrmax) )
    frq     => mpars(igrid)%snlps%frq
    xsi     => mpars(igrid)%snlps%xsi
    !
    xsi(1:nfr) = sig(1:nfr)
    do ifr=nfr+1, nfrmax
      xsi(ifr) = xsi(ifr-1) * xfr
    end do
    frq    = xsi * tpiinv
    !
    ! 2.h final storage
    !
    allocate ( mpars(igrid)%snlps%qst1(16,nqa,nkd),                 &
         mpars(igrid)%snlps%qst3(6,nqa,nkd),                  &
         mpars(igrid)%snlps%qst2(16,nqa,nkd) )
    qst1   => mpars(igrid)%snlps%qst1
    qst2   => mpars(igrid)%snlps%qst2
    qst3   => mpars(igrid)%snlps%qst3
    !
    ! 2.h.1 basic data
    !
    do ikd=1, nkd
      do iqa=1, nqa
        !
        do j=1, 4
          !
          qst1((j-1)*4+1,iqa,ikd) = tstore(iqa,ikd)%oth (j) +       &
               tstore(iqa,ikd)%ofr (j) * nthexp
          qst1((j-1)*4+2,iqa,ikd) = tstore(iqa,ikd)%oth1(j) +       &
               tstore(iqa,ikd)%ofr (j) * nthexp
          qst1((j-1)*4+3,iqa,ikd) = tstore(iqa,ikd)%oth (j) +       &
               tstore(iqa,ikd)%ofr1(j) * nthexp
          qst1((j-1)*4+4,iqa,ikd) = tstore(iqa,ikd)%oth1(j) +       &
               tstore(iqa,ikd)%ofr1(j) * nthexp
          !
          qst2((j-1)*4+1,iqa,ikd) = tstore(iqa,ikd)%hfr (j) *       &
               tstore(iqa,ikd)%hth (j)
          qst2((j-1)*4+2,iqa,ikd) = tstore(iqa,ikd)%hfr (j) *       &
               tstore(iqa,ikd)%hth1(j)
          qst2((j-1)*4+3,iqa,ikd) = tstore(iqa,ikd)%hfr1(j) *       &
               tstore(iqa,ikd)%hth (j)
          qst2((j-1)*4+4,iqa,ikd) = tstore(iqa,ikd)%hfr1(j) *       &
               tstore(iqa,ikd)%hth1(j)
          !
        end do
        !
        qst3(1,iqa,ikd) = tstore(iqa,ikd)%f1
        qst3(2,iqa,ikd) = tstore(iqa,ikd)%f2
        qst3(3,iqa,ikd) = tstore(iqa,ikd)%f3
        qst3(4,iqa,ikd) = tstore(iqa,ikd)%f4
        qst3(5,iqa,ikd) = tstore(iqa,ikd)%cqd
        qst3(6,iqa,ikd) = tstore(iqa,ikd)%cqs
        !
      end do
    end do
    !
    if ( nqd .gt. 0 ) qst3(5,:,:) = qst3(5,:,:) / real(nqd)
    if ( nqs .gt. 0 ) qst3(6,:,:) = qst3(6,:,:) / real(nqs)
    !
    deallocate ( tstore )
    !
    ! 3.  building quadruplet data base ---------------------------------- *
    !     for constructing interactions and diagonal from contributions
    !
    nthmx2 = 0
    allocate ( mpars(igrid)%snlps%qst4(16,nqa,nkd),                 &
         mpars(igrid)%snlps%qst5(16,nqa,nkd),                 &
         mpars(igrid)%snlps%qst6(16,nqa,nkd) )
    qst4   => mpars(igrid)%snlps%qst4
    qst5   => mpars(igrid)%snlps%qst5
    qst6   => mpars(igrid)%snlps%qst6
    allocate ( ast1(16,nqa,nkd), ast2(16,nqa,nkd) )
    !
    ! 3.a loop over relative depths
    !
    s0     = sitmin * sqrt ( grav / depth )  / xsit
    !
    do ikd=1, nkd
      !
      s0     = s0 * xsit
      call wavnu2 ( s0, depth, wn0, cg0, 1.e-6, 25, ierr)
      !
      ! 3.b loop over representative quadruplets
      !
      nqa    = 0
      !
      do iq=1, snlnq
        !
        !
        off12  = snlm(iq)
        off34  = snll(iq)
        th12   = snlt(iq) * dera
        !
        !
        ! 3.c frequency indices
        !
        auxfr(1) = ( 1. + off12 )
        auxfr(2) = ( 1. - off12 )
        auxfr(3) = ( 1. + off34 )
        auxfr(4) = ( 1. - off34 )
        !
        do j=1, 4
          jfr (j) = int( log(auxfr(j)) / xfrln )
          jfr1(j) = jfr(j) + 1 * sign(1.,auxfr(j)-1.)
          wfr (j) = (xfr**jfr1(j)-auxfr(j))/(xfr**jfr1(j)-xfr**jfr(j))
          wfr1(j) = 1. - wfr(j)
        end do
        !
        !
        ! 3.d loop over quadruplet components
        !
        do jiq=1, 4
          !
          if ( jiq .le. 2 ) then
            wf     = -1.
          else
            wf     =  1.
          end if
          !
          ! 3.e loop over frequency offsets, get directional offsets
          !
          do jof=1, 2
            !
            if ( jof .eq. 1 ) then
              ifr    = -jfr(jiq)
              wfroff =  wfr(jiq)
            else
              ifr    = -jfr1(jiq)
              wfroff =  wfr1(jiq)
            end if
            !
            sioff  = s0 * xfr**ifr
            call wavnu2 ( sioff, depth, wn0, cg0, 1.e-6, 25, ierr)
            s1     = sioff * ( 1. + off12 )
            call wavnu2 ( s1, depth, wn1, cg1, 1.e-6, 25, ierr)
            s2     = sioff * ( 1. - off12 )
            call wavnu2 ( s2, depth, wn2, cg2, 1.e-6, 25, ierr)
            s3     = sioff * ( 1. + off34 )
            call wavnu2 ( s3, depth, wn3, cg3, 1.e-6, 25, ierr)
            s4     = sioff * ( 1. - off34 )
            call wavnu2 ( s4, depth, wn4, cg4, 1.e-6, 25, ierr)
            !
            !
            if ( th12 .lt. 0. ) then
              bb = 2. * wn0
            else
              bb = wn1**2 + wn2**2 + 2.*wn1*wn2*cos(th12)
              bb = sqrt ( max ( bb , 0. ) )
            end if
            !
            if ( th12.lt.0. .and. abs(off12).le.1.e-4 ) then
              delth(1) = 0.
              delth(2) = 0.
            else
              cc       = wn1
              aa       = wn2
              aux1     = (cc**2+bb**2-aa**2) / (2.*bb*cc)
              aux2     = (aa**2+bb**2-cc**2) / (2.*bb*aa)
              delth(1) = - acos( max ( 0. , min ( 1. , aux1 ) ) )
              delth(2) =   acos( max ( 0. , min ( 1. , aux2 ) ) )
            end if
            cc       = wn3
            aa       = wn4
            aux1     = (cc**2+bb**2-aa**2) / (2.*bb*cc)
            aux2     = (aa**2+bb**2-cc**2) / (2.*bb*aa)
            delth(3) = - acos( max ( 0. , min ( 1. , aux1 ) ) )
            delth(4) =   acos( max ( 0. , min ( 1. , aux2 ) ) )
            !
            !
            aux1    = delth(jiq) / dth
            jth (jiq) = int(aux1)
            jth1(jiq) = jth(jiq) + 1 * sign(1.,delth(jiq))
            wth1(jiq) = abs(aux1) - real(abs(jth(jiq)))
            wth (jiq) = 1. - wth1(jiq)
            !
            nthmx2 = max ( nthmx2 , abs(jth1(jiq)) )
            !
            !
            ! 3.f loop over quadruplet realizations
            !
            if ( snlm(iq).eq.0. .and. snlt(iq).lt.0. ) then
              jj     = 2
            else
              jj     = 4
            end if
            !
            do jqr=1, jj
              !
              select case (jqr)
              case (2)
                jth (3) = -jth (3)
                jth (4) = -jth (4)
                jth1(3) = -jth1(3)
                jth1(4) = -jth1(4)
              case (3)
                jth     = -jth
                jth1    = -jth1
              case (4)
                jth (3) = -jth (3)
                jth (4) = -jth (4)
                jth1(3) = -jth1(3)
                jth1(4) = -jth1(4)
              case default
                jth     = -jth
                jth1    = -jth1
              end select
              !
              ist    = (jiq-1)*4 + (jof-1)*2 + 1
              ast1(ist,nqa+jqr,ikd) = ifr
              ast2(ist,nqa+jqr,ikd) = jth(jiq)
              qst5(ist,nqa+jqr,ikd) = wf * ( wfroff * wth(jiq) )
              qst6(ist,nqa+jqr,ikd) = wf * ( wfroff * wth(jiq) )**2
              ist    = ist + 1
              ast1(ist,nqa+jqr,ikd) = ifr
              ast2(ist,nqa+jqr,ikd) = jth1(jiq)
              qst5(ist,nqa+jqr,ikd) = wf * ( wfroff * wth1(jiq) )
              qst6(ist,nqa+jqr,ikd) = wf * ( wfroff * wth1(jiq) )**2
              !
              ! ... end loop 3.f
              !
            end do
            !
            ! ... end loop 3.e
            !
          end do
          !
          ! ... end loop 3.d
          !
        end do
        !
        !
        ! ... end loop 3.b
        !
        nqa    = nqa + jj
        !
      end do
      !
      ! ... end loop 3.a
      !
    end do
    !
    ! 3.g finalize storage
    !
    qst4 = ast1*nthexp + ast2
    !
    if ( nthmax .lt. nthmx2 ) goto 810
    if ( nqa .ne. size(ast1(1,:,1)) ) goto 811
    !
    deallocate ( ast1, ast2 )
    !
    return
    !
    ! error escape locations
    !
800 continue
    write (ndse,1000) lammax, delthm
    call extcde ( 1000 )
    !
801 continue
    write (ndse,1001) off12, off34
    call extcde ( 1001 )
    !
802 continue
    write (ndse,1002) off12, off34, snlt(iq),                       &
         minlam(off12,snlt(iq)), maxlam(off12,snlt(iq))
    call extcde ( 1002 )
    !
810 continue
    write (ndse,1010) nthmax, nthmx2
    call extcde ( 1010 )
    !
811 continue
    write (ndse,1011) nqa, size(ast1(1,:,1))
    call extcde ( 1011 )
    !
    return
    !
    ! formats
    !
1000 format (/' *** wavewatch-iii error in insnl3 :'/                &
         '     parameter out of range '/                        &
         '     lammax, delthm :', 2e12.4/)
1001 format (/' *** wavewatch-iii error in insnl3 :'/                &
         '     parameter out of range '/                        &
         '     mu, lambda :', 2e12.4/)
1002 format (/' *** wavewatch-iii error in insnl3 :'/                &
         '     parameter out of range '/                        &
         '     mu, lambda, th12 :',3e12.4/                      &
         '     lambda range     :',2e12.4)
1010 format (/' *** wavewatch-iii error in insnl3 :'/                &
         '     nthmax less than nthmx2 :', 2i8/)
1011 format (/' *** wavewatch-iii error in insnl3 :'/                &
         '     nqa inconsistent :', 2i8/)
    !
    !
    !
    !/
    !/ embedded subroutines
    !/
  contains
    !/ ------------------------------------------------------------------- /
    !>
    !> @brief calculate minimum allowed lambda for quadruplet configuration.
    !>
    !> @param    mu       quadruplet parameters.
    !> @param    theta    theta in degrees.
    !> @returns  minlam   minimum allowed lambda.
    !>
    !> @author h. l. tolman
    !> @date   28-jan-2004
    !>
    real function minlam ( mu, theta )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch-iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         28-jan-2004 |
      !/                  +-----------------------------------+
      !/
      !/    28-jan-2009 : origination.
      !/
      !  1. purpose :
      !
      !     calculate minimum allowed lambda for quadruplet configuration.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       mu, theta  real   quadruplet parameters, theta in degree.
      !     ----------------------------------------------------------------
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      implicit none
      !/
      !/ parameter list
      !/
      real, intent(in)        :: mu, theta
      !/
      !/ local parameters
      !/
      real                    :: muloc, thetar, bb, aux
      !/
      !/ ------------------------------------------------------------------- /
      !/
      if ( theta .lt. 0. ) then
        minlam = 0.
      else
        muloc  = max ( 0. , min ( 1., mu ) )
        thetar = theta * atan(1.) / 45.
        bb     = (1.+muloc)**4 + (1.-muloc)**4 +                    &
             2. * (1.+muloc)**2 * (1.-muloc)**2 * cos(thetar)
        bb     = sqrt ( max ( bb , 0. ) )
        aux    = max ( 0. , 0.5*bb-1. )
        minlam = sqrt ( aux )
      end if
      !
      return
      !/
      !/ end of minlam ----------------------------------------------------- /
      !/
    end function minlam
    !/ ------------------------------------------------------------------- /
    !>
    !> @attention replaced (likely typo) 'minimum' from original header here
    !>  with 'maximum'.
    !> @brief calculate maximum allowed lambda for quadruplet configuration.
    !>
    !> @param    mu       quadruplet parameters.
    !> @param    theta    theta in degrees.
    !> @returns  maxlam   maximum allowed lambda.
    !>
    !> @author h. l. tolman
    !> @date   28-jan-2004
    !>
    real function maxlam ( mu, theta )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch-iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         28-jan-2004 |
      !/                  +-----------------------------------+
      !/
      !/    28-jan-2009 : origination.
      !/
      !  1. purpose :
      !
      !     calculate minimum allowed lambda for quadruplet configuration.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       mu, theta  real   quadruplet parameters, theta in degree.
      !     ----------------------------------------------------------------
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      implicit none
      !/
      !/ parameter list
      !/
      real, intent(in)        :: mu, theta
      !/
      !/ local parameters
      !/
      real                    :: muloc, thetar, bb, aux
      !/
      !/ ------------------------------------------------------------------- /
      !/
      if ( theta .lt. 0. ) then
        maxlam = 0.5
      else
        muloc  = max ( 0. , min ( 1., mu ) )
        thetar = theta * atan(1.) / 45.
        bb     = (1.+muloc)**4 + (1.-muloc)**4 +                    &
             2. * (1.+muloc)**2 * (1.-muloc)**2 * cos(thetar)
        bb     = sqrt ( max ( bb , 0. ) )
        maxlam = 0.25 * bb
      end if
      !
      return
      !/
      !/ end of maxlam ----------------------------------------------------- /
      !/
    end function maxlam
    !/
    !/ end of insnl3 ----------------------------------------------------- /
    !/
  end subroutine insnl3
  !/
  !/ end of module w3snl3md -------------------------------------------- /
  !/
end module w3snl3md
