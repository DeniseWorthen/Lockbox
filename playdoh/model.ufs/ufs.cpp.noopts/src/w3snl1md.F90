!> @file
!> @brief bundles routines calculate nonlinear wave-wave interactions
!>  according to the discrete interaction approximation (dia).
!>
!> @author h. l. tolman
!> @date   03-sep-2012
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
!> @brief bundles routines to calculate nonlinear wave-wave interactions
!>  according to the discrete interaction approximation (dia) of
!>  hasselmann et al. (jpo, 1985).
!>
!> @author h. l. tolman
!> @date   03-sep-2012
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3snl1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         03-sep-2012 |
  !/                  +-----------------------------------+
  !/
  !/    04-feb-2000 : origination.                        ( version 2.00 )
  !/    09-may-2002 : switch clean up.                    ( version 2.21 )
  !/    24-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    03-sep-2012 : clean up of test output t0, t1      ( version 4.07 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     bundles routines calculate nonlinear wave-wave interactions
  !     according to the discrete interaction approximation (dia) of
  !     hasselmann et al. (jpo, 1985).
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
  !      w3snl1    subr. public   calculate interactions.
  !      insnl1    subr. public   initialization routine.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine documentation.
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/s      enable subroutine tracing.
  !       !/t(n)   test output, see subroutines.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
!>
!> @brief calculate nonlinear interactions and the diagonal term of
!>  its derivative.
!>
!> @param[in] a       action spectrum a(isp) as a function of
!>                    direction (rad) and wavenumber.
!> @param[in] cg      group velocities (dimension nk).
!> @param[in] kdmean  mean relative depth.
!> @param[out] s      source term.
!> @param[out] d      diagonal term of derivative.
!>
!> @author h. l. tolman
!> @date   06-jun-2018
!>
  subroutine w3snl1 (a, cg, kdmean, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    12-jun-1996 : final fortran 77                    ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    09-may-2002 : switch clean up.                    ( version 2.21 )
    !/    24-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    03-sep-2012 : clean up of test output t0, t1      ( version 4.07 )
    !/    06-jun-2018 : add optional debugsrc               ( version 6.04 )
    !/
    !  1. purpose :
    !
    !     calculate nonlinear interactions and the diagonal term of
    !     its derivative.
    !
    !  2. method :
    !
    !     discrete interaction approximation. (hasselmann and hasselmann
    !     1985; wamdi group 1988)
    !
    !     the dia is applied to the energy spectrum (instead of the action
    !     spectrum), for which is was originally developped. because the
    !     frequency grid is invariant, the nonlinear interactions are
    !     calculated for the frequency spectrum, as in wam. this requires
    !     only a single set of interpolation data which can be applied
    !     throughout the spatial domain. for deep water this is idenitical
    !     to a direct application to the wavenumber spectrum, for shallow
    !     water it is not. as the shallow water correction is nothing but
    !     a crude approximation, the choice between spectra is expected to
    !     be irrelevant.
    !
    !     the nonlinear interactions are calculated for two "mirror image"
    !     quadruplets as described in the manual. the central bin of these
    !     quadruples is placed on the discrete complonents of the spectrum,
    !     which requires interpolation to obtain other eneregy densities.
    !     the figure below defines the diferent basic counters and weights
    !     necessary for this interpolation.
    !
    !
    !               ifrm1  ifrm
    !                5        7    t |
    !          ithm1  +------+     h +
    !                 |      |     e |      ifrp      ifrp1
    !                 |   \  |     t |       3           1
    !           ithm  +------+     a +        +---------+  ithp1
    !                6       \8      |        |         |
    !                                |        |  /      |
    !                           \    +        +---------+  ithp
    !                                |      /4           2
    !                              \ |  /
    !          -+-----+------+-------#--------+---------+----------+
    !                              / |  \        freq.
    !                                |      \4           2
    !                           /    +        +---------+  ithp
    !                                |        |  \      |
    !                6       /8      |        |         |
    !           ithm  +------+       +        +---------+  ithp1
    !                 |   \  |       |       3           1
    !                 |      |       |      ifrp      ifrp1
    !          ithm1  +------+       +
    !                5        7      |
    !
    !     to create long vector loops and to efficiently deal with the
    !     closed nature of the directional space, the relative counters
    !     above are replaced by complete addresses stored in 32 arrays
    !     (see section 3 and insnl1). the interaction are furthermore
    !     calucated for an extended spectrum, making it unnecessary to
    !     introduce extra weight factors for low and high frequencies.
    !     therefore low and high frequencies are added to the local
    !     (auxiliary) spectrum as illustraed below.
    !
    !
    !              ^  +---+---------------------+---------+- nth
    !              |  |   :                     :         |
    !                 |   :                     :         |
    !              d  | 2 :  original spectrum  :    1    |
    !              i  |   :                     :         |
    !              r  |   :                     :         |
    !                 +---+---------------------+---------+-  1
    !                            frequencies -->     ^
    !         ifr =   0   1                    nfr   |  nfrhgh
    !                                                |
    !                                             nfrchg
    !
    !     where : 1 : extra tail added beyond nfr
    !             2 : empty bins at low frequencies
    !
    !             nfrhgh = nfr + ifrp1 - ifrm1
    !             nfrchg = nfr - ifrm1
    !
    !     all counters and arrays are set in insnl1. see also section 3
    !     and section 8.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action spectrum a(isp) as a function of
    !                         direction (rad)  and wavenumber.
    !       cg      r.a.  i   group velocities (dimension nk).
    !       kdmean  real  i   mean relative depth.
    !       s       r.a.  o   source term.                           *)
    !       d       r.a.  o   diagonal term of derivative.           *)
    !     ----------------------------------------------------------------
    !                             *) 1-d array with dimension nth*nk
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      prt2ds    subr. w3arrymd print plot of spectra.
    !      outmat    subr. w3wrrymd print out 2d matrix.
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
    !       none.
    !
    !  8. structure :
    !
    !     -------------------------------------------
    !      1.  calculate proportionality constant.
    !      2.  prepare auxiliary spectrum
    !      3.  calculate (unfolded) interactions
    !        a energy at interacting bins
    !        b contribution to interactions
    !        c fold interactions to side angles
    !      4.  put source and diagonal term together
    !     -------------------------------------------
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants
    use w3gdatmd, only: nk, nth, nspec, sig, fachfe,                &
         kdcon, kdmn, snlc1, snls1, snls2, snls3
    use w3adatmd, only: nfr, nfrhgh, nfrchg, nspecx, nspecy,        &
         ip11, ip12, ip13, ip14, im11, im12, im13, im14,   &
         ip21, ip22, ip23, ip24, im21, im22, im23, im24,   &
         ic11, ic12, ic21, ic22, ic31, ic32, ic41, ic42,   &
         ic51, ic52, ic61, ic62, ic71, ic72, ic81, ic82,   &
         dal1, dal2, dal3, af11,                           &
         awg1, awg2, awg3, awg4, awg5, awg6, awg7, awg8,   &
         swg1, swg2, swg3, swg4, swg5, swg6, swg7, swg8
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), cg(nk), kdmean
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ifr, isp
    real                    :: x, x2, cons, conx, factor,           &
         e00, ep1, em1, ep2, em2,             &
         sa1a, sa1b, sa2a, sa2b
    real               ::  ue  (1-nth:nspecy), sa1 (1-nth:nspecx),  &
         sa2 (1-nth:nspecx), da1c(1-nth:nspecx),  &
         da1p(1-nth:nspecx), da1m(1-nth:nspecx),  &
         da2c(1-nth:nspecx), da2p(1-nth:nspecx),  &
         da2m(1-nth:nspecx), con (      nspec )
    !/
    !/ ------------------------------------------------------------------- /
    !/
    ! initialisations
    !
    !
    ! 1.  calculate prop. constant --------------------------------------- *
    !
    x      = max ( kdcon*kdmean , kdmn )
    x2     = max ( -1.e15, snls3*x)
    cons   = snlc1 * ( 1. + snls1/x * (1.-snls2*x) * exp(x2) )
    !
    !
    ! 2.  prepare auxiliary spectrum and arrays -------------------------- *
    !
    do ifr=1, nfr
      conx = tpiinv / sig(ifr) * cg(ifr)
      do ith=1, nth
        isp       = ith + (ifr-1)*nth
        ue (isp) = a(isp) / conx
        con(isp) = conx
      end do
    end do
    !
    do ifr=nfr+1, nfrhgh
      do ith=1, nth
        isp      = ith + (ifr-1)*nth
        ue(isp) = ue(isp-nth) * fachfe
      end do
    end do
    !
    do isp=1-nth, 0
      ue  (isp) = 0.
      sa1 (isp) = 0.
      sa2 (isp) = 0.
      da1c(isp) = 0.
      da1p(isp) = 0.
      da1m(isp) = 0.
      da2c(isp) = 0.
      da2p(isp) = 0.
      da2m(isp) = 0.
    end do
    !
    ! 3.  calculate interactions for extended spectrum ------------------- *
    !
    do isp=1, nspecx
      !
      ! 3.a energy at interacting bins
      !
      e00    =        ue(isp)
      ep1    = awg1 * ue(ip11(isp)) + awg2 * ue(ip12(isp))        &
           + awg3 * ue(ip13(isp)) + awg4 * ue(ip14(isp))
      em1    = awg5 * ue(im11(isp)) + awg6 * ue(im12(isp))        &
           + awg7 * ue(im13(isp)) + awg8 * ue(im14(isp))
      ep2    = awg1 * ue(ip21(isp)) + awg2 * ue(ip22(isp))        &
           + awg3 * ue(ip23(isp)) + awg4 * ue(ip24(isp))
      em2    = awg5 * ue(im21(isp)) + awg6 * ue(im22(isp))        &
           + awg7 * ue(im23(isp)) + awg8 * ue(im24(isp))
      !
      ! 3.b contribution to interactions
      !
      factor = cons * af11(isp) * e00
      !
      sa1a   = e00 * ( ep1*dal1 + em1*dal2 )
      sa1b   = sa1a - ep1*em1*dal3
      sa2a   = e00 * ( ep2*dal1 + em2*dal2 )
      sa2b   = sa2a - ep2*em2*dal3
      !
      sa1 (isp) = factor * sa1b
      sa2 (isp) = factor * sa2b
      !
      da1c(isp) = cons * af11(isp) * ( sa1a + sa1b )
      da1p(isp) = factor * ( dal1*e00 - dal3*em1 )
      da1m(isp) = factor * ( dal2*e00 - dal3*ep1 )
      !
      da2c(isp) = cons * af11(isp) * ( sa2a + sa2b )
      da2p(isp) = factor * ( dal1*e00 - dal3*em2 )
      da2m(isp) = factor * ( dal2*e00 - dal3*ep2 )
      !
    end do
    !
    ! 4.  put source and diagonal term together -------------------------- *
    !
    do isp=1, nspec
      !
      s(isp) = con(isp) * ( - 2. * ( sa1(isp) + sa2(isp) )       &
           + awg1 * ( sa1(ic11(isp)) + sa2(ic12(isp)) )    &
           + awg2 * ( sa1(ic21(isp)) + sa2(ic22(isp)) )    &
           + awg3 * ( sa1(ic31(isp)) + sa2(ic32(isp)) )    &
           + awg4 * ( sa1(ic41(isp)) + sa2(ic42(isp)) )    &
           + awg5 * ( sa1(ic51(isp)) + sa2(ic52(isp)) )    &
           + awg6 * ( sa1(ic61(isp)) + sa2(ic62(isp)) )    &
           + awg7 * ( sa1(ic71(isp)) + sa2(ic72(isp)) )    &
           + awg8 * ( sa1(ic81(isp)) + sa2(ic82(isp)) ) )
      !
      d(isp) =  - 2. * ( da1c(isp) + da2c(isp) )                 &
           + swg1 * ( da1p(ic11(isp)) + da2p(ic12(isp)) )     &
           + swg2 * ( da1p(ic21(isp)) + da2p(ic22(isp)) )     &
           + swg3 * ( da1p(ic31(isp)) + da2p(ic32(isp)) )     &
           + swg4 * ( da1p(ic41(isp)) + da2p(ic42(isp)) )     &
           + swg5 * ( da1m(ic51(isp)) + da2m(ic52(isp)) )     &
           + swg6 * ( da1m(ic61(isp)) + da2m(ic62(isp)) )     &
           + swg7 * ( da1m(ic71(isp)) + da2m(ic72(isp)) )     &
           + swg8 * ( da1m(ic81(isp)) + da2m(ic82(isp)) )
      !
    end do
    !
    ! ... test output :
    !
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3snl1 ----------------------------------------------------- /
    !/
  end subroutine w3snl1
!/ ------------------------------------------------------------------- /
!>
!> @brief preprocessing for nonlinear interactions (weights).
!>
!> @param[in] imod  model number.
!>
!> @author h. l. tolman
!> @date   24-dec-2004
!>
  subroutine insnl1 ( imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    19-oct-1998 : final fortran 77                    ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    09-may-2002 : switch clean up.                    ( version 2.21 )
    !/    24-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     preprocessing for nonlinear interactions (weights).
    !
    !  2. method :
    !
    !     see w3snl1.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.  i   model number.
    !     ----------------------------------------------------------------
    !
    !     local variables
    !     ----------------------------------------------------------------
    !       ithxn   real  directional indices.                 (relative)
    !       ifrxn   real  frequency indices.                   (relative)
    !       it1     r.a.  directional indices.                      (1-d)
    !       ifn     r.a.  frequency indices.                        (1-d)
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
    !      w3iogr    subr. w3iogrmd model definition file processing.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on array dimensions for local arrays in w3snl.
    !
    !  7. remarks :
    !
    !     - test output is generated through w3iogr.
    !     - no testing of imod ir resetting of pointers.
    !
    !  8. structure :
    !
    !     - see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, nspec, dth, xfr, sig, lam
    use w3adatmd, only: w3dmnl
    use w3adatmd, only: nfr, nfrhgh, nfrchg, nspecx, nspecy,        &
         ip11, ip12, ip13, ip14, im11, im12, im13, im14,   &
         ip21, ip22, ip23, ip24, im21, im22, im23, im24,   &
         ic11, ic12, ic21, ic22, ic31, ic32, ic41, ic42,   &
         ic51, ic52, ic61, ic62, ic71, ic72, ic81, ic82,   &
         dal1, dal2, dal3, af11,                           &
         awg1, awg2, awg3, awg4, awg5, awg6, awg7, awg8,   &
         swg1, swg2, swg3, swg4, swg5, swg6, swg7, swg8
    use w3odatmd, only: ndst, ndse
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod
    !/
    !/ local parameters
    !/
    integer                 :: ifr, ith, isp, ithp, ithp1, ithm,    &
         ithm1,ifrp, ifrp1, ifrm, ifrm1
    integer, allocatable    :: if1(:), if2(:), if3(:), if4(:),      &
         if5(:), if6(:), if7(:), if8(:),      &
         it1(:), it2(:), it3(:), it4(:),      &
         it5(:), it6(:), it7(:), it8(:)
    real                    :: delth3, delth4, lamm2, lamp2, cthp,  &
         wthp, wthp1, cthm, wthm, wthm1,      &
         xfrln, wfrp, wfrp1, wfrm, wfrm1, fr, &
         af11a
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    nfr     = nk
    !
    ! 1.  internal angles of quadruplet.
    !
    lamm2  = (1.-lam)**2
    lamp2  = (1.+lam)**2
    delth3 = acos( (lamm2**2+4.-lamp2**2) / (4.*lamm2) )
    delth4 = asin(-sin(delth3)*lamm2/lamp2)
    !
    ! 2.  lambda dependend weight factors.
    !
    dal1   = 1. / (1.+lam)**4
    dal2   = 1. / (1.-lam)**4
    dal3   = 2. * dal1 * dal2
    !
    ! 3.  directional indices.
    !
    cthp   = abs(delth4/dth)
    ithp   = int(cthp)
    ithp1  = ithp + 1
    wthp   = cthp - real(ithp)
    wthp1  = 1.- wthp
    !
    cthm   = abs(delth3/dth)
    ithm   = int(cthm)
    ithm1  = ithm + 1
    wthm   = cthm - real(ithm)
    wthm1  = 1.- wthm
    !
    ! 4.  frequency indices.
    !
    xfrln  = log(xfr)
    !
    ifrp   = int( log(1.+lam) / xfrln )
    ifrp1  = ifrp + 1
    wfrp   = (1.+lam - xfr**ifrp) / (xfr**ifrp1 - xfr**ifrp)
    wfrp1  = 1. - wfrp
    !
    ifrm   = int( log(1.-lam) / xfrln )
    ifrm1  = ifrm - 1
    wfrm   = (xfr**ifrm -(1.-lam)) / (xfr**ifrm - xfr**ifrm1)
    wfrm1  = 1. - wfrm
    !
    ! 5.  range of calculations
    !
    nfrhgh = nfr + ifrp1 - ifrm1
    nfrchg = nfr - ifrm1
    nspecy = nfrhgh * nth
    nspecx = nfrchg * nth
    !
    ! 6.  allocate arrays or check array sizes
    !
    call w3dmnl ( imod, ndse, ndst, nspec, nspecx )
    !
    allocate ( if1(nfrchg), if2(nfrchg), if3(nfrchg), if4(nfrchg),  &
         if5(nfrchg), if6(nfrchg), if7(nfrchg), if8(nfrchg),  &
         it1(nth), it2(nth), it3(nth), it4(nth),              &
         it5(nth), it6(nth), it7(nth), it8(nth) )
    !
    ! 7.  spectral addresses
    !
    do ifr=1, nfrchg
      if1(ifr) =           ifr+ifrp
      if2(ifr) =           ifr+ifrp1
      if3(ifr) = max ( 0 , ifr+ifrm  )
      if4(ifr) = max ( 0 , ifr+ifrm1 )
      if5(ifr) = max ( 0 , ifr-ifrp  )
      if6(ifr) = max ( 0 , ifr-ifrp1 )
      if7(ifr) =           ifr-ifrm
      if8(ifr) =           ifr-ifrm1
    end do
    !
    do ith=1, nth
      it1(ith) = ith + ithp
      it2(ith) = ith + ithp1
      it3(ith) = ith + ithm
      it4(ith) = ith + ithm1
      it5(ith) = ith - ithp
      it6(ith) = ith - ithp1
      it7(ith) = ith - ithm
      it8(ith) = ith - ithm1
      if ( it1(ith).gt.nth) it1(ith) = it1(ith) - nth
      if ( it2(ith).gt.nth) it2(ith) = it2(ith) - nth
      if ( it3(ith).gt.nth) it3(ith) = it3(ith) - nth
      if ( it4(ith).gt.nth) it4(ith) = it4(ith) - nth
      if ( it5(ith).lt. 1 ) it5(ith) = it5(ith) + nth
      if ( it6(ith).lt. 1 ) it6(ith) = it6(ith) + nth
      if ( it7(ith).lt. 1 ) it7(ith) = it7(ith) + nth
      if ( it8(ith).lt. 1 ) it8(ith) = it8(ith) + nth
    end do
    !
    do isp=1, nspecx
      ifr       = 1 + (isp-1)/nth
      ith       = 1 + mod(isp-1,nth)
      ip11(isp) = it2(ith) + (if2(ifr)-1)*nth
      ip12(isp) = it1(ith) + (if2(ifr)-1)*nth
      ip13(isp) = it2(ith) + (if1(ifr)-1)*nth
      ip14(isp) = it1(ith) + (if1(ifr)-1)*nth
      im11(isp) = it8(ith) + (if4(ifr)-1)*nth
      im12(isp) = it7(ith) + (if4(ifr)-1)*nth
      im13(isp) = it8(ith) + (if3(ifr)-1)*nth
      im14(isp) = it7(ith) + (if3(ifr)-1)*nth
      ip21(isp) = it6(ith) + (if2(ifr)-1)*nth
      ip22(isp) = it5(ith) + (if2(ifr)-1)*nth
      ip23(isp) = it6(ith) + (if1(ifr)-1)*nth
      ip24(isp) = it5(ith) + (if1(ifr)-1)*nth
      im21(isp) = it4(ith) + (if4(ifr)-1)*nth
      im22(isp) = it3(ith) + (if4(ifr)-1)*nth
      im23(isp) = it4(ith) + (if3(ifr)-1)*nth
      im24(isp) = it3(ith) + (if3(ifr)-1)*nth
    end do
    !
    do isp=1, nspec
      ifr       = 1 + (isp-1)/nth
      ith       = 1 + mod(isp-1,nth)
      ic11(isp) = it6(ith) + (if6(ifr)-1)*nth
      ic21(isp) = it5(ith) + (if6(ifr)-1)*nth
      ic31(isp) = it6(ith) + (if5(ifr)-1)*nth
      ic41(isp) = it5(ith) + (if5(ifr)-1)*nth
      ic51(isp) = it4(ith) + (if8(ifr)-1)*nth
      ic61(isp) = it3(ith) + (if8(ifr)-1)*nth
      ic71(isp) = it4(ith) + (if7(ifr)-1)*nth
      ic81(isp) = it3(ith) + (if7(ifr)-1)*nth
      ic12(isp) = it2(ith) + (if6(ifr)-1)*nth
      ic22(isp) = it1(ith) + (if6(ifr)-1)*nth
      ic32(isp) = it2(ith) + (if5(ifr)-1)*nth
      ic42(isp) = it1(ith) + (if5(ifr)-1)*nth
      ic52(isp) = it8(ith) + (if8(ifr)-1)*nth
      ic62(isp) = it7(ith) + (if8(ifr)-1)*nth
      ic72(isp) = it8(ith) + (if7(ifr)-1)*nth
      ic82(isp) = it7(ith) + (if7(ifr)-1)*nth
    end do
    !
    deallocate ( if1, if2, if3, if4, if5, if6, if7, if8,  &
         it1, it2, it3, it4, it5, it6, it7, it8 )
    !
    ! 8.  fill scaling array (f**11)
    !
    do ifr=1, nfr
      af11a  = (sig(ifr)*tpiinv)**11
      do ith=1, nth
        af11(ith+(ifr-1)*nth) = af11a
      end do
    end do
    !
    fr     = sig(nfr)*tpiinv
    do ifr=nfr+1, nfrchg
      fr     = fr * xfr
      af11a  = fr**11
      do ith=1, nth
        af11(ith+(ifr-1)*nth) = af11a
      end do
    end do
    !
    ! 9.  interpolation weights
    !
    awg1   = wthp  * wfrp
    awg2   = wthp1 * wfrp
    awg3   = wthp  * wfrp1
    awg4   = wthp1 * wfrp1
    awg5   = wthm  * wfrm
    awg6   = wthm1 * wfrm
    awg7   = wthm  * wfrm1
    awg8   = wthm1 * wfrm1
    !
    swg1   = awg1**2
    swg2   = awg2**2
    swg3   = awg3**2
    swg4   = awg4**2
    swg5   = awg5**2
    swg6   = awg6**2
    swg7   = awg7**2
    swg8   = awg8**2
    !
    return
    !
    ! formats
    !
    !/
    !/ end of insnl1 ----------------------------------------------------- /
    !/
  end subroutine insnl1
  !/
  !/ end of module w3snl1md -------------------------------------------- /
  !/
end module w3snl1md
