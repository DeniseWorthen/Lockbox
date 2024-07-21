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
  !/                  | last update :         28-feb-2023 |
  !/                  +-----------------------------------+
  !/
  !/    04-feb-2000 : origination.                        ( version 2.00 )
  !/    09-may-2002 : switch clean up.                    ( version 2.21 )
  !/    24-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    03-sep-2012 : clean up of test output t0, t1      ( version 4.07 )
  !/    28-feb-2023 : adds gqm separate routines          ( version 7.07 )
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
  !/
  public
  !/
  !/ these are the arrays and variables used for gqm method
  !/
  integer              :: nconf
  integer, allocatable :: k_if2 (:,:,:) , k_if3 (:,:,:) , k_1p2p(:,:,:) , &
       k_1p3m(:,:,:) , k_1p2m(:,:,:) , k_1p3p(:,:,:) , &
       k_1m2p(:,:,:) , k_1m3m(:,:,:) , k_1m2m(:,:,:) , &
       k_1m3p(:,:,:)
  integer, allocatable :: f_poin(:) , t_poin(:) , k_if1(:) , k_1p(:,:) ,  &
       k_1m(:,:) , idconf(:,:)
  double precision, allocatable :: f_coef(:) , f_proj(:) , tb_sca(:) , tb_v14(:)
  double precision, allocatable :: tb_v24(:,:,:) , tb_v34(:,:,:) ,        &
       tb_tpm(:,:,:) , tb_tmp(:,:,:) , tb_fac(:,:,:)
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
  !/ ------------------------------------------------------------------- /
  subroutine w3snlgqm(a,cg,wn,depth,tstotn,tsdern)
    ! this and the following routines are adapted to ww3 from tomawac qnlin3.f
    !***********************************************************************
    ! tomawac   v6p1                                   24/06/2011
    !***********************************************************************
    !
    !brief    computes the contribution of the non-linear interactions
    !+                source term between quadruplets using the gqm method
    !+                ("gaussian quadrature method") proposed by lavrenov
    !+                (2001)
    !+
    !+            procedure specific to the case where the frequencies
    !+                follow a geometrical progression and the directions
    !+                are evenly distributed over [0;2.pi].
    !
    !note     this subroutine uses the output from 'prenl3' to optimise
    !+          the computations for dia.
    !
    !reference  lavrenov, i.v. (2001):
    !+           "effect of wind wave parameter fluctuation on the nonlinear
    !+           spectrum evolution". j. phys. oceanogr. 31, 861-873.
    !
    !history  e. gagnaire-renou
    !+        04/2011
    !+        v6p1
    !+   created
    !
    !history  g.mattarolo (edf - lnhe)
    !+        24/06/2011
    !+        v6p1
    !+   translation of french names of the variables in argument
    !
    !/ warning, contrary to the dia routine, there is no extension to frequencies below ik=1
    !/ as a result the first two frequencies are not fully treated.
    !==================================================================================
    !     this subroutine is same as qnlin3 in tomwac
    use constants, only: tpi
    use w3gdatmd,  only: sig, nk ,  nth , dth, xfr, fr1, gqthrsat, gqamp
    implicit none
    real, intent(in) :: a(nth,nk), cg(nk), wn(nk)
    real, intent(in) :: depth
    real, intent(out) :: tstotn(nth,nk), tsdern(nth,nk)
    integer          :: ith,ik,nt,nf
    real             :: q_dfac, satval(nk), sume, accval, accmax, ampfac
    double precision :: raisf, freq(nk)
    double precision :: tstot(nth,nk) , tsder(nth,nk), f(nth,nk)
    double precision :: temp
    !.....local variables
    integer             jf    , jt    , jf1   , jt1  , iq_om2 &
         , jfm0  , jfm1  , jfm2  , jfm3  , ixf1 , ixf2   &
         , ixf3  , jfmin , jfmax , iconf , lbuf
    integer            kt1p  , kt1m  , jt1p  , jt1m  , kt1p2p, kt1p2m &
         , kt1p3p, kt1p3m, kt1m2p, kt1m2m, kt1m3p, kt1m3m &
         , jt1p2p, jt1p2m, jt1p3p, jt1p3m, jt1m2p, jt1m2m &
         , jt1m3p, jt1m3m
    double precision  v1_4  , v2_4  , v3_4  , q_2p3m, q_2m3p, factor &
         , t_2p3m, t_2m3p, s_2p3m, s_2m3p, scal_t, t2p3m &
         , t2m3p , sp0   , sp1p  , sp1m  , sp1p2p, sp1p2m &
         , sp1p3p, sp1p3m, sp1m2p, sp1m2m, sp1m3p, sp1m3m &
         , cf0   , cp0   , cf1   , cp1   , cf2   , cp2   &
         , cf3   , cp3   , q2pd0 , q2pd1 , q2pd2p, q2pd3m &
         , q2md0 , q2md1 , q2md2m, q2md3p ,aux00 , aux01  &
         , aux02 , aux03 , aux04 , aux05 , seuil  &
         , aux06 , aux07 , aux08 , aux09 , aux10 , fseuil
    nt = nth
    nf = nk
    lbuf = 500
    seuil = 0.
    raisf = xfr
    do ik = 1,nk
      freq(ik) = fr1*raisf**(ik-1)
    enddo
    do ith = 1,nth
      do ik = 1,nk
        ! f is the e(f,theta) spectrum ...
        f(ith,ik) = dble(a(ith,ik)*sig(ik))*dble(tpi)/dble(cg(ik))
      enddo
    enddo
    !   call insnlgqm
    ! it returns: f_poin , t_poin , f_coef , f_proj, tb_sca , k_if1, k_1p, k_1m , k_if2
    !             k_if3, k_1p2p , k_1p3m , k_1p2m , k_1p3p , k_1m2p , k_1m3m ,  k_1m2m
    !             k_1m3p , tb_v14 , tb_fac , tb_v24 , tb_v34 , tb_tmp , tb_tpm , idconf, nconf
    !=======================================================================
    !     computes the generalized min and max frequencies : instead of going
    !     from 1 to nf in freq(jf) for the main frequency, it goes from jfmin
    !     to jfmax
    !     jfmin is given by fmin=freq(1) /gamma_min
    !     jfmax is given by fmax=freq(nf)*gamma_max
    !     tests have shown that it can be assumed gamma_min=1. (jfmin=1) and
    !     gamma_max=1.3 (jfmax>nf) to obtain improved results
    !     note by fabrice ardhuin: this appears to give the difference in tail benaviour with gerbrant's wrt
    !=======================================================================
    jfmin=max(1-int(log(1.0d0)/log(raisf)),1)
    jfmax=min(nf+int(log(1.3d0)/log(raisf)),nk)
    !
    !=======================================================================
    !     computes the spectrum threshold values (below which qnl4 is not
    !     calculated). the threshold is set within 0 and 1.
    ! this was commented by fa
    !=======================================================================
    !        aux00=0.0d0
    !        do jf=1,nf
    !          do jt=1,nt
    !            if (f(jt,jf).gt.aux00) aux00=f(jt,jf)
    !          enddo
    !        enddo
    !        fseuil=aux00*seuil
    tstot = 0.
    tsder = 0.
    !=======================================================================
    accmax=0.
    do jf=jfmin,jfmax
      sume=sum(f(:,jf))*dth
      satval(jf) = sume*freq(jf)**5
      accval = sume*freq(jf)**4
      if (accval.gt.accmax) accmax=accval
    end do
    !     ==================================================
    !     starts loop 1 over the selected configurations
    !     ==================================================
    do iconf=1,nconf
      !       ---------selected configuration characteristics
      jf1   =idconf(iconf,1)
      jt1   =idconf(iconf,2)
      iq_om2=idconf(iconf,3)
      !
      !       ---------recovers v1**4=(f1/f0)**4
      v1_4  =tb_v14(jf1)
      !       ---------recovers the shift of the frequency index on f1
      ixf1  =k_if1(jf1)
      !       ---------recovers the direction indexes for delat1
      kt1p  =k_1p(jt1,jf1)
      kt1m  =k_1m(jt1,jf1)
      !       ---------recovers v2**4=(f2/f0)**4 and v3**4=(f3/f0)**4
      v2_4  =tb_v24(iq_om2,jt1,jf1)
      v3_4  =tb_v34(iq_om2,jt1,jf1)
      !       ---------recovers the frequency indexes shift on f2 and f3
      ixf2  =k_if2 (iq_om2,jt1,jf1)
      ixf3  =k_if3 (iq_om2,jt1,jf1)
      !       ---------recovers the direction indexes shift
      kt1p2p=k_1p2p(iq_om2,jt1,jf1)
      kt1p2m=k_1p2m(iq_om2,jt1,jf1)
      kt1p3p=k_1p3p(iq_om2,jt1,jf1)
      kt1p3m=k_1p3m(iq_om2,jt1,jf1)
      kt1m2p=k_1m2p(iq_om2,jt1,jf1)
      kt1m2m=k_1m2m(iq_om2,jt1,jf1)
      kt1m3p=k_1m3p(iq_om2,jt1,jf1)
      kt1m3m=k_1m3m(iq_om2,jt1,jf1)
      !       ---------recovers the coupling coefficients
      t2p3m =tb_tpm(iq_om2,jt1,jf1)
      t2m3p =tb_tmp(iq_om2,jt1,jf1)
      !       ---------recovers the multiplicative factor of qnl4
      factor=tb_fac(iq_om2,jt1,jf1)
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      !       starts loop 2 over the spectrum frequencies
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      do jf=jfmin,jfmax
        if (satval(jf).gt.gqthrsat) then
          !
          !.........recovers the coefficient for the coupling factor
          !.........computes the coupling coefficients for the case +delta1 (sig=1)
          scal_t=tb_sca(lbuf+jf)*factor
          t_2p3m=t2p3m*scal_t
          t_2m3p=t2m3p*scal_t
          !
          !.........frequency indexes and coefficients
          jfm0=f_poin(jf+lbuf)
          cf0 =f_coef(jf+lbuf)
          cp0 =f_proj(jf+lbuf)
          jfm1=f_poin(jf+ixf1)
          cf1 =f_coef(jf+ixf1)
          cp1 =f_proj(jf+ixf1)
          jfm2=f_poin(jf+ixf2)
          cf2 =f_coef(jf+ixf2)
          cp2 =f_proj(jf+ixf2)
          jfm3=f_poin(jf+ixf3)
          cf3 =f_coef(jf+ixf3)
          cp3 =f_proj(jf+ixf3)
          !
          !         -------------------------------------------------
          !         starts loop 3 over the spectrum directions
          !         -------------------------------------------------
          do jt=1,nt
            !
            !...........direction indexes
            !           direct config (+delta1) (sig =1)
            jt1p  =t_poin(jt+kt1p)
            jt1p2p=t_poin(jt+kt1p2p)
            jt1p2m=t_poin(jt+kt1p2m)
            jt1p3p=t_poin(jt+kt1p3p)
            jt1p3m=t_poin(jt+kt1p3m)
            !           image config (-delta1)
            jt1m  =t_poin(jt+kt1m)
            jt1m2p=t_poin(jt+kt1m2p)
            jt1m2m=t_poin(jt+kt1m2m)
            jt1m3p=t_poin(jt+kt1m3p)
            jt1m3m=t_poin(jt+kt1m3m)
            !
            !           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !           starts loop 4 over the mesh nodes
            !           - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            !
            sp0=f(jt,jfm0)*cf0
            !
            !              if (sp0.gt.fseuil) then
            !
            !               config. +delta1 (sig=1)
            !               =======================
            !...............computes the spectrum values in 1, 2, 3
            sp1p  =f(jt1p  ,jfm1)*cf1
            sp1p2p=f(jt1p2p,jfm2)*cf2
            sp1p3m=f(jt1p3m,jfm3)*cf3
            sp1p2m=f(jt1p2m,jfm2)*cf2
            sp1p3p=f(jt1p3p,jfm3)*cf3
            !
            !...............computes auxiliary products and variables
            aux01=sp0*v1_4+sp1p
            aux02=sp0*sp1p
            aux03=sp1p2p*sp1p3m
            aux04=sp1p2p*v3_4+sp1p3m*v2_4
            aux05=sp1p2m*sp1p3p
            aux06=sp1p2m*v3_4+sp1p3p*v2_4
            aux07=aux02*v3_4
            aux08=aux02*v2_4
            !
            !...............computes the components of the transfer term
            s_2p3m=aux03*aux01-aux02*aux04
            s_2m3p=aux05*aux01-aux02*aux06
            q_2p3m=t_2p3m*s_2p3m
            q_2m3p=t_2m3p*s_2m3p
            aux00 =q_2p3m+q_2m3p
            !
            !...............computes the components of the derived terms (dq/df)
            q2pd0 =t_2p3m*(aux03*v1_4   - sp1p*aux04)*cf0
            q2pd1 =t_2p3m*(aux03        - sp0 *aux04)*cf1
            q2pd2p=t_2p3m*(aux01*sp1p3m - aux07     )*cf2
            q2pd3m=t_2p3m*(aux01*sp1p2p - aux08     )*cf3
            q2md0 =t_2m3p*(aux05*v1_4   - sp1p*aux06)*cf0
            q2md1 =t_2m3p*(aux03        - sp0 *aux06)*cf1
            q2md2m=t_2m3p*(aux01*sp1p3p - aux07     )*cf2
            q2md3p=t_2m3p*(aux01*sp1p2m - aux08     )*cf3
            aux09=q2pd0+q2md0
            aux10=q2pd1+q2md1
            !
            !...............sum of qnl4 term in the table tstot
            tstot(jt,jfm0    )=tstot(jt,jfm0    )+aux00 *cp0
            tstot(jt1p,jfm1  )=tstot(jt1p,jfm1  )+aux00 *cp1
            tstot(jt1p2p,jfm2)=tstot(jt1p2p,jfm2)-q_2p3m*cp2
            tstot(jt1p2m,jfm2)=tstot(jt1p2m,jfm2)-q_2m3p*cp2
            tstot(jt1p3m,jfm3)=tstot(jt1p3m,jfm3)-q_2p3m*cp3
            tstot(jt1p3p,jfm3)=tstot(jt1p3p,jfm3)-q_2m3p*cp3
            !
            !...............sum of the term dqnl4/df in the table tsder
            tsder(jt,jfm0)=tsder(jt,jfm0)+aux09 *cp0
            tsder(jt1p,jfm1)=tsder(jt1p,jfm1)+aux10 *cp1
            tsder(jt1p2p,jfm2)=tsder(jt1p2p,jfm2)-q2pd2p*cp2
            tsder(jt1p2m,jfm2)=tsder(jt1p2m,jfm2)-q2md2m*cp2
            tsder(jt1p3m,jfm3)=tsder(jt1p3m,jfm3)-q2pd3m*cp3
            tsder(jt1p3p,jfm3)=tsder(jt1p3p,jfm3)-q2md3p*cp3
            !
            !               config. -delta1 (sig=-1)
            !               ========================
            !...............computes the spectrum values in 1, 2, 3
            sp1m  =f(jt1m  ,jfm1)*cf1
            sp1m2p=f(jt1m2p,jfm2)*cf2
            sp1m3m=f(jt1m3m,jfm3)*cf3
            sp1m2m=f(jt1m2m,jfm2)*cf2
            sp1m3p=f(jt1m3p,jfm3)*cf3
            !
            !...............computes auxiliary products and variables
            aux01=sp0*v1_4+sp1m
            aux02=sp0*sp1m
            aux03=sp1m2p*sp1m3m
            aux04=sp1m2p*v3_4+sp1m3m*v2_4
            aux05=sp1m2m*sp1m3p
            aux06=sp1m2m*v3_4+sp1m3p*v2_4
            aux07=aux02*v3_4
            aux08=aux02*v2_4
            !
            !...............computes the transfer term components
            s_2p3m=aux03*aux01-aux02*aux04
            s_2m3p=aux05*aux01-aux02*aux06
            q_2p3m=t_2m3p*s_2p3m
            q_2m3p=t_2p3m*s_2m3p
            aux00 =q_2p3m+q_2m3p   ! same as in +delta1, can be commented out
            !
            !...............computes the derived terms components (dq/df)
            q2pd0 =t_2p3m*(aux03*v1_4   - sp1m*aux04)*cf0
            q2pd1 =t_2p3m*(aux03        - sp0 *aux04)*cf1
            q2pd2p=t_2p3m*(aux01*sp1m3m - aux07     )*cf2
            q2pd3m=t_2p3m*(aux01*sp1m2p - aux08     )*cf3
            q2md0 =t_2m3p*(aux05*v1_4   - sp1m*aux06)*cf0
            q2md1 =t_2m3p*(aux03        - sp0 *aux06)*cf1
            q2md2m=t_2m3p*(aux01*sp1m3p - aux07     )*cf2
            q2md3p=t_2m3p*(aux01*sp1m2m - aux08     )*cf3
            aux09=q2pd0+q2md0
            aux10=q2pd1+q2md1
            !
            !...............sum of qnl4 term in the table tstot
            tstot(jt    ,jfm0)=tstot(jt    ,jfm0)+aux00 *cp0
            tstot(jt1m  ,jfm1)=tstot(jt1m  ,jfm1)+aux00 *cp1
            tstot(jt1m2p,jfm2)=tstot(jt1m2p,jfm2)-q_2p3m*cp2
            tstot(jt1m2m,jfm2)=tstot(jt1m2m,jfm2)-q_2m3p*cp2
            tstot(jt1m3m,jfm3)=tstot(jt1m3m,jfm3)-q_2p3m*cp3
            tstot(jt1m3p,jfm3)=tstot(jt1m3p,jfm3)-q_2m3p*cp3
            !
            !...............sum of the term dqnl4/df in the table tsder
            tsder(jt    ,jfm0)=tsder(jt    ,jfm0)+aux09 *cp0
            tsder(jt1m  ,jfm1)=tsder(jt1m  ,jfm1)+aux10 *cp1
            tsder(jt1m2p,jfm2)=tsder(jt1m2p,jfm2)-q2pd2p*cp2
            tsder(jt1m2m,jfm2)=tsder(jt1m2m,jfm2)-q2md2m*cp2
            tsder(jt1m3m,jfm3)=tsder(jt1m3m,jfm3)-q2pd3m*cp3
            tsder(jt1m3p,jfm3)=tsder(jt1m3p,jfm3)-q2md3p*cp3
            !
            !
            !              endif ! this was the test on seuil
            !
          enddo
          !         -------------------------------------------------
          !         end of loop 3 over the spectrum directions
          !         -------------------------------------------------
          !
        endif ! end of test on saturation level
      enddo
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      !       end of loop 2 over the spectrum frequencies
      !       = = = = = = = = = = = = = = = = = = = = = = = = =
      !
    enddo
    !     ==================================================
    !     end of loop 1 over the selected configurations
    !     ==================================================
    ! applying wam depth scaling ! to be added later ...
    !      call q_dscale(f,wn,sig,dth,nk,nth,depth,q_dfac)
    q_dfac=1
    ! amplification inspired by lavrenov 2001, eq 10.
    ampfac=gqamp(4)*min(max(accmax/gqamp(2),1.)**gqamp(1),gqamp(3))
    !write(991,*) accmax,q_dfac,ampfac,gqamp(1:3),satval(10),satval(30)
    ! replacing double precision with simple real and scaling
    tstotn = tstot*q_dfac*ampfac
    tsdern = tsder*q_dfac*ampfac
    ! converting snl(theta,f) to snl(theta,k)/sigma
    do ith = 1,nt
      do ik = 1,nf
        tstotn(ith,ik) = tstotn(ith,ik)*cg(ik)/(tpi*sig(ik))
      enddo
    enddo
    !close(994)
    !stop
  end subroutine w3snlgqm
  !/ ------------------------------------------------------------------- /
  function couple(xk1 ,yk1 ,xk2 ,yk2 ,xk3 ,yk3 ,xk4 ,yk4)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  | m. benoit & e. gagnaire-renou     |
    !/                  | last update :         20-nov-2022 |
    !/                  +-----------------------------------+
    !/
    !/    19-nov-2022 : transfer from tomawac code          ( version 7.xx )
    !/
    !  1. purpose :
    !
    !     computes the 4-wave coupling coefficient used in snl4
    !
    !  2. method :
    !
    !     uses theoretical expression by webb (1978)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       xk1     real  i   x component of k1 wavenumber ...
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      innslgqm  subr. w3snl2   prepares source term integration.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: grav
    !
    implicit none
    
    double precision, intent(in)    :: xk1   , yk1   , xk2   , yk2
    double precision, intent(in)    :: xk3   , yk3
    double precision, intent(in)    :: xk4   , yk4
    double precision couple
    !
    !.....local variables
    !     """"""""""""""""""
    double precision rk1   , rk2   , rk3   , rk4   , wk1   , wk2
    double precision wk3   , wk4   , s12   , s13   , s14   , s23
    double precision s24   , s34   , w1p2  , q12   , w1m3  , q13
    double precision w1m4  , q14   , ddd   , coef  , deno13, nume13
    double precision deno14, nume14, zero, pi
    !
    pi = acos(-1.)
    coef=pi*grav*grav/4.d0
    zero=1.d-10
    !
    rk1=sqrt(xk1*xk1+yk1*yk1)
    rk2=sqrt(xk2*xk2+yk2*yk2)
    rk3=sqrt(xk3*xk3+yk3*yk3)
    rk4=sqrt(xk4*xk4+yk4*yk4)
    !
    wk1=sqrt(rk1)
    wk2=sqrt(rk2)
    wk3=sqrt(rk3)
    wk4=sqrt(rk4)
    !
    s12=xk1*xk2+yk1*yk2
    s13=xk1*xk3+yk1*yk3
    s14=xk1*xk4+yk1*yk4
    s23=xk2*xk3+yk2*yk3
    s24=xk2*xk4+yk2*yk4
    s34=xk3*xk4+yk3*yk4
    !
    w1p2=sqrt((xk1+xk2)*(xk1+xk2)+(yk1+yk2)*(yk1+yk2))
    w1m3=sqrt((xk1-xk3)*(xk1-xk3)+(yk1-yk3)*(yk1-yk3))
    w1m4=sqrt((xk1-xk4)*(xk1-xk4)+(yk1-yk4)*(yk1-yk4))
    q12=(wk1+wk2)*(wk1+wk2)
    q13=(wk1-wk3)*(wk1-wk3)
    q14=(wk1-wk4)*(wk1-wk4)
    !
    !.....computes the d coefficient of webb (1978)
    !     """"""""""""""""""""""""""""""""""""""
    ddd=2.00d0*q12*(rk1*rk2-s12)*(rk3*rk4-s34)/(w1p2-q12) &
         +0.50d0*(s12*s34+s13*s24+s14*s23) &
         +0.25d0*(s13+s24)*q13*q13 &
         -0.25d0*(s12+s34)*q12*q12 &
         +0.25d0*(s14+s23)*q14*q14 &
         +2.50d0*rk1*rk2*rk3*rk4 &
         +q12*q13*q14*(rk1+rk2+rk3+rk4)
    deno13=w1m3-q13
    nume13=2.00d0*q13*(rk1*rk3+s13)*(rk2*rk4+s24)
    if (abs(deno13).lt.zero) then
      if (abs(nume13).lt.zero) then
        write(*,*) 'w3snl2 error for coupling coefficient : (1-3)  0/0 !'
      else
        write(*,*) 'w3snl2 error for coupling coefficient : (1-3) inifinte value'
      endif
      write(*,*) 'w3snl2 error for coupling coefficient : (1-3) term not used'
    else
      ddd=ddd+nume13/deno13
    endif
    deno14=w1m4-q14
    nume14=2.00d0*q14*(rk1*rk4+s14)*(rk2*rk3+s23)
    if (abs(deno14).lt.zero) then
      if (abs(nume14).lt.zero) then
        write(*,*) 'w3snl2 error for coupling coefficient : (1-4)  0/0 !'
      else
        write(*,*) 'w3snl2 error for coupling coefficient : (1-4) inifinte value'
      endif
      write(*,*) 'w3snl2 error for coupling coefficient : (1-4) term not used'
    else
      ddd=ddd+nume14/deno14
    endif
    couple=coef*ddd*ddd/(wk1*wk2*wk3*wk4)
    !      return
  end function couple
  !/ ------------------------------------------------------------------- /
  subroutine gauleg (w_leg ,x_leg ,npoin)
    !/ ------------------------------------------------------------------- /
    !.....variables in argument
    !     """"""""""""""""""""
    implicit none
    integer ,         intent(in)    :: npoin
    double precision ,intent(inout) :: w_leg(npoin) , x_leg(npoin)
    !
    !.....local variables
    !     """""""""""""""""
    integer           i, m, j
    double precision  eps, z, p1, p2, p3, pp, z1, pi
    parameter        (eps=3.d-14)
    !
    pi = acos(-1.)
    m=(npoin+1)/2
    do i=1,m
      z=cos(pi*(dble(i)-0.25d0)/(dble(npoin)+0.5d0))
1     continue
      p1=1.0d0
      p2=0.0d0
      do j=1,npoin
        p3=p2
        p2=p1
        p1=((2.d0*dble(j)-1.d0)*z*p2-(dble(j)-1.d0)*p3)/dble(j)
      enddo
      pp=dble(npoin)*(z*p1-p2)/(z*z-1.d0)
      z1=z
      z=z-p1/pp
      if (abs(z-z1).gt.eps) goto 1
      x_leg(i)=-z
      x_leg(npoin+1-i)=z
      w_leg(i)=2.d0/((1.d0-z**2)*pp**2)
      w_leg(npoin+1-i)=w_leg(i)
    enddo
  end subroutine gauleg
  !/ ------------------------------------------------------------------- /
  subroutine f1f1f1(f1sf,nf1,iq_om1)
    ! tomawac   v6p3                                   15/06/2011
    !***********************************************************************
    !
    !brief   subroutine called by prenl3
    !+         computes values of ratio f1/f as function of the iq_om1
    !+         indicator
    !
    !history  e. gagnaire-renou
    !+        04/2011
    !+        v6p1
    !+   created
    !
    !history  g.mattarolo (edf - lnhe)
    !+        15/06/2011
    !+        v6p1
    !+   translation of french names of the variables in argument
    !
    !history  e. gagnaire-renou
    !+        12/03/2013
    !+        v6p3
    !+   better formatted: write(lu,*), etc.
    !/ ------------------------------------------------------------------- /
    implicit none
    integer,          intent(in)    :: iq_om1
    integer,          intent(inout) :: nf1
    double precision, intent(inout) :: f1sf(*)
    !
    integer i,m
    double precision raison
    !
    if(iq_om1.eq.1) then
      if(nf1.ne.14) then
        write(*,*) '#1 incorrect value for nf1',nf1
      endif
      f1sf( 1)=0.30d0
      f1sf( 2)=0.40d0
      f1sf( 3)=0.50d0
      f1sf( 4)=0.60d0
      f1sf( 5)=0.70d0
      f1sf( 6)=0.80d0
      f1sf( 7)=0.90d0
      f1sf( 8)=1.00d0
      f1sf( 9)=1.11d0
      f1sf(10)=1.25d0
      f1sf(11)=1.42d0
      f1sf(12)=1.67d0
      f1sf(13)=2.00d0
      f1sf(14)=2.50d0
      f1sf(15)=3.30d0
    elseif(iq_om1.eq.2) then
      if (nf1.ne.26) then
        write(*,*) '#2 incorrect value for nf1', nf1
      endif
      f1sf( 1)=0.32d0
      f1sf( 2)=0.35d0
      f1sf( 3)=0.39d0
      f1sf( 4)=0.44d0
      f1sf( 5)=0.50d0
      f1sf( 6)=0.56d0
      f1sf( 7)=0.63d0
      f1sf( 8)=0.70d0
      f1sf( 9)=0.78d0
      f1sf(10)=0.86d0
      f1sf(11)=0.92d0
      f1sf(12)=0.97d0
      f1sf(13)=1.00d0
      f1sf(14)=1.03d0
      f1sf(15)=1.08d0
      f1sf(16)=1.13d0
      f1sf(17)=1.20d0
      f1sf(18)=1.28d0
      f1sf(19)=1.37d0
      f1sf(20)=1.48d0
      f1sf(21)=1.50d0
      f1sf(22)=1.65d0
      f1sf(23)=1.85d0
      f1sf(24)=2.10d0
      f1sf(25)=2.40d0
      f1sf(26)=2.70d0
      f1sf(27)=3.20d0
    elseif(iq_om1.eq.3) then
      if(nf1.ne.11) then
        write(*,*) 'incorrect value for nf1', nf1
      endif
      f1sf( 1)=0.30d0
      f1sf( 2)=0.48d0
      f1sf( 3)=0.64d0
      f1sf( 4)=0.78d0
      f1sf( 5)=0.90d0
      f1sf( 6)=1.00d0
      f1sf( 7)=1.12d0
      f1sf( 8)=1.28d0
      f1sf( 9)=1.50d0
      f1sf(10)=1.80d0
      f1sf(11)=2.40d0
      f1sf(12)=3.40d0
    elseif(iq_om1.eq.4) then
      if(nf1.ne.40) then
        write(*,*) 'incorrect value for nf1', nf1
      endif
      nf1=20
      m=10
      raison=9.d0**(1.d0/dble(nf1))
      f1sf(m+1)=1.0d0/3.0d0
      nf1=2*m+nf1
      do i=m+2,nf1+1
        f1sf(i)=f1sf(i-1)*raison
      enddo
      do i=m,1,-1
        f1sf(i)=f1sf(i+1)/raison
      enddo
    elseif(iq_om1.eq.5) then
      raison=9.d0**(1.d0/dble(nf1))
      f1sf(1)=1.d0/3.d0
      do i=2,nf1+1
        f1sf(i)=f1sf(i-1)*raison
      enddo
    elseif(iq_om1.eq.6) then
      raison=(3.d0-1.d0/3.d0)/dble(nf1)
      f1sf(1)=1.d0/3.d0
      do i=2,nf1+1
        f1sf(i)=f1sf(i-1)+raison
      enddo
    elseif(iq_om1.eq.7) then
      if(nf1.ne.20) then
        write(*,*) 'incorrect value for nf1', nf1
      endif
      f1sf( 1)=1.d0/3.d0
      f1sf( 2)=0.40d0
      f1sf( 3)=0.46d0
      f1sf( 4)=0.52d0
      f1sf( 5)=0.60d0
      f1sf( 6)=0.70d0
      f1sf( 7)=0.79d0
      f1sf( 8)=0.86d0
      f1sf( 9)=0.92d0
      f1sf(10)=0.97d0
      f1sf(11)=1.00d0
      f1sf(12)=1.04d0
      f1sf(13)=1.10d0
      f1sf(14)=1.18d0
      f1sf(15)=1.28d0
      f1sf(16)=1.42d0
      f1sf(17)=1.60d0
      f1sf(18)=1.84d0
      f1sf(19)=2.14d0
      f1sf(20)=2.52d0
      f1sf(21)=3.00d0
    endif
    !
  end subroutine f1f1f1
  !/ ------------------------------------------------------------------- /
  subroutine insnlgqm
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |       e. gagnaire-renou &         |
    !/                  |       m. benoit                   |
    !/                  |       s. mostafa siadatamousavi   |
    !/		    |       m. beyramzadeh              |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-nov-2022 |
    !/                  +-----------------------------------+
    !/
    !/    20-nov-2022 : merging with nl2 in ww3.            ( version 7.00 )
    !/
    !  1. purpose :
    !
    !     preprocessing for nonlinear interactions (xnl).
    !
    !  2. method :
    !
    !     see xnl documentation.
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !      name      type  module      description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd    subroutine tracing.
    !                subr. gauleg      gauss-legendre weights
    !      xnl_init  subr. m_constants xnl initialization routine.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd model definition file management.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
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
    use constants, only: grav
    use w3gdatmd,  only: nk , nth , xfr , fr1, gqnf1, gqnt1, gqnq_om2, nltail, gqthrcou
    implicit none
    !.....local variables
    integer           jf    , jt    , jf1   , jt1   , nf1p1 , iaux , nt , nf , ik
    integer           iq_te1 , iq_om2 , lbuf , dimbuf , iq_om1 , nq_te1 , nconfm
    double precision  epsi_a, aux   , ccc   , deno  , aaa   , dp2sg , tailf
    double precision  v1    , v1_4  , dv1   , dtetar , elim , raisf
    double precision  v2    , v2_4  , v3    , v3_4
    double precision  w2    , w2_m  , w2_1  , w_mil , w_rad
    double precision  rk0   , xk0   , yk0   , rk1   , xk1   , yk1
    double precision  rk2   , xk2p  , yk2p  , xk2m  , yk2m
    double precision  rk3   , xk3p  , yk3p  , xk3m  , yk3m
    double precision  d01p  , c_d01p, s_d01p, d0ap  , c_d0ap, s_d0ap
    double precision  ga2p  , c_ga2p, s_ga2p, ga3p  , c_ga3p, s_ga3p, twopi, pi, seuil1 , seuil2 , seuil
    !
    !.....variables related to the gaussian quadratures
    double precision  w_che_te1, w_che_om2, c_leg_om2
    !
    !.....variables related to the configuration selection
    double precision  test1 , test2
    double precision :: freq(nk)
    double precision, allocatable :: f1sf(:) , x_che_te1(:) , x_che_om2(:) , x_leg_om2(:) , w_leg_om2(:) &
         ,  maxcla(:)
    pi = acos(-1.)
    lbuf = 500
    dimbuf = 2*lbuf+200
    twopi  = 2.*pi
    !
    ! defines some threshold values for filtering (see gagnaire-renou thesis,  p 52)
    !
    seuil1 = 1e10
    seuil2 = gqthrcou
    if(gqnf1.eq.14) iq_om1=1
    if(gqnf1.eq.26) iq_om1=2
    if(gqnf1.eq.11) iq_om1=3
    if(gqnf1.eq.40) iq_om1=4
    if(gqnf1.eq.11) iq_om1=3
    if(gqnf1.eq.40) iq_om1=4
    if(gqnf1.eq.20) iq_om1=7
    !
    ! note by fa: not sure what the 5 and 6 cases correspond to
    !
    nq_te1 = gqnt1/2
    nconfm = gqnf1*gqnt1*gqnq_om2
    raisf = xfr
    nt = nth
    nf = nk
    dtetar = twopi/dble(nt)
    do ik = 1,nk
      freq(ik) = fr1*raisf**(ik-1)
    enddo
    tailf = -nltail
    !===============allocate matrices=============================================
    if (allocated(k_if2) ) then
      deallocate(k_if2)
    endif
    allocate(k_if2(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_if3) ) then
      deallocate(k_if3)
    endif
    allocate(k_if3(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1p2p) ) then
      deallocate(k_1p2p)
    endif
    allocate(k_1p2p(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1p3m) ) then
      deallocate(k_1p3m)
    endif
    allocate(k_1p3m(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1p2m) ) then
      deallocate(k_1p2m)
    endif
    allocate(k_1p2m(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1p3p) ) then
      deallocate(k_1p3p)
    endif
    allocate(k_1p3p(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1m2p) ) then
      deallocate(k_1m2p)
    endif
    allocate(k_1m2p(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1m3m) ) then
      deallocate(k_1m3m)
    endif
    allocate(k_1m3m(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1m2m) ) then
      deallocate(k_1m2m)
    endif
    allocate(k_1m2m(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_1m3p) ) then
      deallocate(k_1m3p)
    endif
    allocate(k_1m3p(gqnq_om2,gqnt1,gqnf1))
    if (allocated(tb_v24) ) then
      deallocate(tb_v24)
    endif
    allocate(tb_v24(gqnq_om2,gqnt1,gqnf1))
    if (allocated(tb_v34) ) then
      deallocate(tb_v34)
    endif
    allocate(tb_v34(gqnq_om2,gqnt1,gqnf1))
    if (allocated(tb_tpm) ) then
      deallocate(tb_tpm)
    endif
    allocate(tb_tpm(gqnq_om2,gqnt1,gqnf1))
    if (allocated(tb_tmp) ) then
      deallocate(tb_tmp)
    endif
    allocate(tb_tmp(gqnq_om2,gqnt1,gqnf1))
    if (allocated(tb_fac) ) then
      deallocate(tb_fac)
    endif
    allocate(tb_fac(gqnq_om2,gqnt1,gqnf1))
    if (allocated(k_if1) ) then
      deallocate(k_if1)
    endif
    allocate(k_if1(gqnf1))
    if (allocated(k_1p) ) then
      deallocate(k_1p)
    endif
    allocate(k_1p(gqnt1,gqnf1))
    if (allocated(k_1m) ) then
      deallocate(k_1m)
    endif
    allocate(k_1m(gqnt1,gqnf1))
    if (allocated(tb_v14) ) then
      deallocate(tb_v14)
    endif
    allocate(tb_v14(gqnf1))
    if (allocated(idconf) ) then
      deallocate(idconf)
    endif
    allocate(idconf(nconfm,3))
    !=======================================================================
    !     initialisation of auxiliairy tables for spectrum interpolation
    !=======================================================================
    if (allocated(f_poin) ) then
      deallocate(f_poin)
    endif
    allocate(f_poin(dimbuf))
    if (allocated(t_poin) ) then
      deallocate(t_poin)
    endif
    allocate(t_poin(dimbuf))
    if (allocated(f_coef) ) then
      deallocate(f_coef)
    endif
    allocate(f_coef(dimbuf))
    if (allocated(f_proj) ) then
      deallocate(f_proj)
    endif
    allocate(f_proj(dimbuf))
    if (allocated(tb_sca) ) then
      deallocate(tb_sca)
    endif
    allocate(tb_sca(dimbuf))
    f_poin(:)=0
    t_poin(:)=0
    f_coef(:)=0.d0
    f_proj(:)=0.d0
    tb_sca(:)=0.0d0
    do jf=1,lbuf
      f_poin(jf)=1
      f_coef(jf)=0.0d0
      f_proj(jf)=0.0d0
    enddo
    do jf=1,nf
      iaux=lbuf+jf
      f_poin(iaux)=jf
      f_coef(iaux)=1.0d0
      f_proj(iaux)=1.0d0
    enddo
    aux=1.d0/raisf**tailf
    do jf=1,lbuf
      iaux=lbuf+nf+jf
      f_poin(iaux)=nf
      f_coef(iaux)=aux**jf
      f_proj(iaux)=0.0d0
    enddo
    !
    do jt=lbuf,1,-1
      t_poin(jt)=nt-mod(lbuf-jt,nt)
    enddo
    do jt=1,nt
      t_poin(lbuf+jt)=jt
    enddo
    do jt=1,lbuf
      t_poin(lbuf+nt+jt)=mod(jt-1,nt)+1
    enddo
    !======================================================================
    !
    !=======================================================================
    !     computes scale coefficients for the coupling coefficient
    !     would be easier to pass these on from w3srce ???
    !=======================================================================
    dp2sg=twopi*twopi/grav
    do jf=1,lbuf
      aux=freq(1)/raisf**(lbuf-jf+1)
      tb_sca(jf)=(dp2sg*aux**2)**6/(twopi**3*aux)
    enddo
    do jf=1,nf
      tb_sca(lbuf+jf)=(dp2sg*freq(jf)**2)**6/(twopi**3*freq(jf))
    enddo
    do jf=1,lbuf
      iaux=lbuf+nf+jf
      aux=freq(nf)*raisf**jf
      tb_sca(iaux)=(dp2sg*aux**2)**6/(twopi**3*aux)
    enddo
    !=======================================================================
    !
    !=======================================================================
    !     computes values for gaussian quadratures
    !=======================================================================
    if (allocated(x_che_te1) ) then
      deallocate(x_che_te1)
    endif
    allocate(x_che_te1(1:nq_te1),x_che_om2(1:gqnq_om2))
    if (allocated(x_leg_om2) ) then
      deallocate(x_leg_om2)
    endif
    allocate(x_leg_om2(1:gqnq_om2),w_leg_om2(1:gqnq_om2))
    !
    !.....abscissa and weight (constant) for gauss-chebyshev
    do iq_te1=1,nq_te1
      x_che_te1(iq_te1)=cos(pi*(dble(iq_te1)-0.5d0)/dble(nq_te1))
    enddo
    w_che_te1=pi/dble(nq_te1)
    do iq_om2=1,gqnq_om2
      x_che_om2(iq_om2)=cos(pi*(dble(iq_om2)-0.5d0)/dble(gqnq_om2))
    enddo
    w_che_om2=pi/dble(gqnq_om2)
    !
    !.....abscissa et weight for gauss-legendre
    call gauleg( w_leg_om2 , x_leg_om2 , gqnq_om2 )
    do iq_om2=1,gqnq_om2
      x_leg_om2(iq_om2)=0.25d0*(1.d0+x_leg_om2(iq_om2))**2
    enddo
    !=======================================================================
    !
    !
    !=======================================================================
    !     computes values of ratio f1/f as function of the iq_om1 indicator
    !=======================================================================
    nf1p1=gqnf1+1
    if (allocated(f1sf) ) then
      deallocate(f1sf)
    endif
    allocate(f1sf(1:nf1p1))
    call f1f1f1 ( f1sf  , gqnf1   , iq_om1)
    !=======================================================================
    !
    !     ==================================================
    !     starts loop 1 over the ratios f1/f0
    !     ==================================================
    do jf1=1,gqnf1
      !       ---------computes and stores v1=f1/f0 and v1**4
      v1=(f1sf(jf1+1)+f1sf(jf1))/2.d0
      k_if1(jf1)=nint(dble(lbuf)+log(v1)/log(raisf))
      v1_4=v1**4
      tb_v14(jf1)=v1_4
      !       ---------computes and stores dv1=df1/f0
      dv1=f1sf(jf1+1)-f1sf(jf1)
      !       ---------computes the a parameter
      aaa=((1.d0+v1)**4-4.d0*(1.d0+v1_4))/(8.d0*v1**2)
      !
      !       =================================================
      !       starts loop 2 over the delta_1+ values
      !       =================================================
      do jt1=1,gqnt1
        !
        !......computes the delta1+ values (=theta_1-theta_0) between 0 and pi.
        if (jt1.le.nq_te1) then
          !           ---------first interval : x from -1 to a
          iq_te1=jt1
          c_d01p=(-1.d0+aaa)/2.d0+(1.d0+aaa)/2.d0*x_che_te1(iq_te1)
          ccc=dv1*sqrt((aaa-c_d01p)/(1.d0-c_d01p))*w_che_te1
        else
          !           ---------second interval : x from a to 1
          iq_te1=jt1-nq_te1
          c_d01p=( 1.d0+aaa)/2.d0+(1.d0-aaa)/2.d0*x_che_te1(iq_te1)
          ccc=dv1*sqrt((c_d01p-aaa)/(1.d0+c_d01p))*w_che_te1
        endif
        s_d01p=sqrt(1.d0-c_d01p*c_d01p)
        d01p  =acos(c_d01p)
        k_1p(jt1,jf1)=lbuf+nint(d01p/dtetar)
        k_1m(jt1,jf1)=lbuf-nint(d01p/dtetar)
        !
        !         ---------computes epsilon_a
        epsi_a=2.d0*sqrt(1.d0+v1_4+2.d0*v1*v1*c_d01p)/(1.d0+v1)**2
        !         ---------computes delta_a+ and its cosinus
        c_d0ap=(1.d0-v1_4+0.25d0*epsi_a**2*(1.d0+v1)**4) &
             /(epsi_a*(1.d0+v1)**2)
        s_d0ap=sqrt(1.0d0-c_d0ap*c_d0ap)
        d0ap  = acos(c_d0ap)
        !
        !.......integration over omega2 depending on eps_a
        if (epsi_a.lt.1.d0) then
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          !........case of a single singularity (in omega2-)
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          w2_m=0.5d0*(1.d0-epsi_a/2.d0)
          w2_1=0.5d0
          !
          w_rad=w2_1-w2_m
          c_leg_om2=sqrt(w_rad)
          !
          !        ----------------------------------------------------
          !........starts loop 3 over omega_2 (case epsilon_a < 1)
          !........case of a single singularity (in omega2-)
          !........integration over omega2 via gauss-legendre quadrature
          !        ----------------------------------------------------
          do iq_om2=1,gqnq_om2
            !             ---------computes w2, v2, and v3
            w2=w2_m+w_rad*x_leg_om2(iq_om2)
            v2=w2*(1.d0+v1)
            v2_4=v2**4
            tb_v24(iq_om2,jt1,jf1)=v2_4
            k_if2 (iq_om2,jt1,jf1) = nint(dble(lbuf) &
                 + log(v2)/log(raisf))
            v3=1.d0+v1-v2
            v3_4=v3**4
            tb_v34(iq_om2,jt1,jf1)=v3_4
            k_if3 (iq_om2,jt1,jf1) = nint(dble(lbuf) &
                 + log(v3)/log(raisf))
            !             ---------computes gamma_2+ et gamma_3+ angles
            c_ga2p=(epsi_a**2/4.d0+w2**4-(1.d0-w2)**4)/(epsi_a*w2*w2)
            c_ga2p=max(min(c_ga2p,1.d0),-1.d0)
            s_ga2p=sqrt(1.d0-c_ga2p*c_ga2p)
            ga2p  =acos(c_ga2p)
            c_ga3p=(epsi_a**2/4.d0-w2**4+(1.d0-w2)**4)/epsi_a &
                 /(1.d0-w2)**2
            c_ga3p=max(min(c_ga3p,1.d0),-1.d0)
            s_ga3p=sqrt(1.d0-c_ga3p*c_ga3p)
            ga3p  =acos(c_ga3p)
            !             shifting of the direction indexes - config. +delta1 (sig=1)
            k_1p2p(iq_om2,jt1,jf1)=nint(( d0ap+ga2p)/dtetar &
                 +dble(lbuf))
            k_1p3m(iq_om2,jt1,jf1)=nint(( d0ap-ga3p)/dtetar &
                 +dble(lbuf))
            k_1p2m(iq_om2,jt1,jf1)=nint(( d0ap-ga2p)/dtetar &
                 +dble(lbuf))
            k_1p3p(iq_om2,jt1,jf1)=nint(( d0ap+ga3p)/dtetar &
                 +dble(lbuf))
            !             shifting of the direction indexes - config. -delta1 (sig=-1)
            k_1m2p(iq_om2,jt1,jf1)=nint((-d0ap+ga2p)/dtetar &
                 +dble(lbuf))
            k_1m3m(iq_om2,jt1,jf1)=nint((-d0ap-ga3p)/dtetar &
                 +dble(lbuf))
            k_1m2m(iq_om2,jt1,jf1)=nint((-d0ap-ga2p)/dtetar &
                 +dble(lbuf))
            k_1m3p(iq_om2,jt1,jf1)=nint((-d0ap+ga3p)/dtetar &
                 +dble(lbuf))
            !
            !.........computes the coupling coefficients (only for delta_1+ )
            rk0=1.d0
            rk1=v1*v1
            rk2=v2*v2
            rk3=(1.d0+v1-v2)**2
            xk0  = rk0
            yk0  = 0.0d0
            xk1  = rk1*c_d01p
            yk1  = rk1*s_d01p
            xk2p = rk2*(c_d0ap*c_ga2p-s_d0ap*s_ga2p)
            yk2p = rk2*(s_d0ap*c_ga2p+c_d0ap*s_ga2p)
            xk2m = rk2*(c_d0ap*c_ga2p+s_d0ap*s_ga2p)
            yk2m = rk2*(s_d0ap*c_ga2p-c_d0ap*s_ga2p)
            xk3p = rk3*(c_d0ap*c_ga3p-s_d0ap*s_ga3p)
            yk3p = rk3*(s_d0ap*c_ga3p+c_d0ap*s_ga3p)
            xk3m = rk3*(c_d0ap*c_ga3p+s_d0ap*s_ga3p)
            yk3m = rk3*(s_d0ap*c_ga3p-c_d0ap*s_ga3p)
            tb_tpm(iq_om2,jt1,jf1)=couple( xk0   , yk0   , xk1   , yk1   , xk2p  , yk2p  , xk3m  , yk3m)
            tb_tmp(iq_om2,jt1,jf1)=couple( xk0   , yk0   , xk1   , yk1   , xk2m  , yk2m  , xk3p  , yk3p)
            !
            !.........computes the multiplicative coefficient for qnl4
            deno=2.d0*sqrt( (0.5d0*(1.d0+epsi_a/2.d0)-w2) &
                 *((w2-0.5d0)**2+0.25d0*(1.d0+epsi_a)) &
                 *((w2-0.5d0)**2+0.25d0*(1.d0-epsi_a)) )
            tb_fac(iq_om2,jt1,jf1)=1.d0/(deno*v1*w2*(1.d0-w2)) &
                 /(1.d0+v1)**5 * w_leg_om2(iq_om2)*c_leg_om2* ccc
          enddo
          !        -----------------------------------------------
          !........end of the loop 3 over omega_2 (case epsilon_a < 1)
          !        -----------------------------------------------
          !
        else
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          !........starts loop 3 over omega_2 (case epsilon_a > 1)
          !........case of two singularities (in omega2- and omega2_1)
          !........integration over omega2 via gauss-chebyschev quadrature
          !        - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          w2_m=0.5d0*(1.d0-epsi_a/2.d0)
          w2_1=0.5d0*(1.d0-sqrt(epsi_a-1.d0))
          !
          w_mil=(w2_m+w2_1)/2.d0
          w_rad=(w2_1-w2_m)/2.d0
          !
          do iq_om2=1,gqnq_om2
            !             ---------computes w2, v2, and v3
            w2=w_mil+w_rad*x_che_om2(iq_om2)
            v2=w2*(1.d0+v1)
            v2_4=v2**4
            tb_v24(iq_om2,jt1,jf1)=v2_4
            k_if2 (iq_om2,jt1,jf1)=nint(dble(lbuf) &
                 +log(v2)/log(raisf))
            v3=1.d0+v1-v2
            v3_4=v3**4
            tb_v34(iq_om2,jt1,jf1)=v3_4
            k_if3 (iq_om2,jt1,jf1)=nint(dble(lbuf) &
                 +log(v3)/log(raisf))
            !             ---------computes gamma_2+ et gamma_3+ angles
            c_ga2p=(epsi_a**2/4.d0+w2**4-(1.d0-w2)**4)/(epsi_a*w2*w2)
            c_ga2p=max(min(c_ga2p,1.d0),-1.d0)
            s_ga2p=sqrt(1.d0-c_ga2p*c_ga2p)
            ga2p  =acos(c_ga2p)
            c_ga3p=(epsi_a**2/4.d0-w2**4+(1.d0-w2)**4)/epsi_a &
                 /(1.d0-w2)**2
            c_ga3p=max(min(c_ga3p,1.d0),-1.d0)
            s_ga3p=sqrt(1.d0-c_ga3p*c_ga3p)
            ga3p  =acos(c_ga3p)
            !             shifts the direction indexes - config. +delta1 (sig=1)
            k_1p2p(iq_om2,jt1,jf1)=nint(( d0ap+ga2p)/dtetar &
                 +dble(lbuf))
            k_1p3m(iq_om2,jt1,jf1)=nint(( d0ap-ga3p)/dtetar &
                 +dble(lbuf))
            k_1p2m(iq_om2,jt1,jf1)=nint(( d0ap-ga2p)/dtetar &
                 +dble(lbuf))
            k_1p3p(iq_om2,jt1,jf1)=nint(( d0ap+ga3p)/dtetar &
                 +dble(lbuf))
            !             shifts the direction indexes - config. -delta1 (sig=-1)
            k_1m2p(iq_om2,jt1,jf1)=nint((-d0ap+ga2p)/dtetar &
                 +dble(lbuf))
            k_1m3m(iq_om2,jt1,jf1)=nint((-d0ap-ga3p)/dtetar &
                 +dble(lbuf))
            k_1m2m(iq_om2,jt1,jf1)=nint((-d0ap-ga2p)/dtetar &
                 +dble(lbuf))
            k_1m3p(iq_om2,jt1,jf1)=nint((-d0ap+ga3p)/dtetar &
                 +dble(lbuf))
            !
            !.........computes the coupling coefficients (only for delta_1+ )
            rk0=1.d0
            rk1=v1*v1
            rk2=v2*v2
            rk3=(1.d0+v1-v2)**2
            xk0  = rk0
            yk0  = 0.0d0
            xk1  = rk1*c_d01p
            yk1  = rk1*s_d01p
            xk2p = rk2*(c_d0ap*c_ga2p-s_d0ap*s_ga2p)
            yk2p = rk2*(s_d0ap*c_ga2p+c_d0ap*s_ga2p)
            xk2m = rk2*(c_d0ap*c_ga2p+s_d0ap*s_ga2p)
            yk2m = rk2*(s_d0ap*c_ga2p-c_d0ap*s_ga2p)
            xk3p = rk3*(c_d0ap*c_ga3p-s_d0ap*s_ga3p)
            yk3p = rk3*(s_d0ap*c_ga3p+c_d0ap*s_ga3p)
            xk3m = rk3*(c_d0ap*c_ga3p+s_d0ap*s_ga3p)
            yk3m = rk3*(s_d0ap*c_ga3p-c_d0ap*s_ga3p)
            tb_tpm(iq_om2,jt1,jf1)=couple( xk0   , yk0   , xk1   , yk1   , xk2p  , yk2p  , xk3m  , yk3m)
            tb_tmp(iq_om2,jt1,jf1)=couple( xk0   , yk0   , xk1   , yk1   , xk2m  , yk2m  , xk3p  , yk3p)
            !
            !.........computes the multiplicative coefficient for qnl4
            deno=2.d0*sqrt( (0.5d0*(1.d0+epsi_a/2.d0)-w2) &
                 *((w2-0.5d0)**2+0.25d0*(1.d0+epsi_a)) &
                 *(0.5d0*(1.d0+sqrt(epsi_a-1.d0))-w2) )
            tb_fac(iq_om2,jt1,jf1)=1.d0/(deno*v1*w2*(1.d0-w2)) &
                 /(1.d0+v1)**5 * w_che_om2* ccc
            !
          enddo
          !        -----------------------------------------------
          !........end of loop 3 over omega_2 (case epsilon_a > 1)
          !        -----------------------------------------------
          !
        endif
      enddo
      !       =================================================
      !       end of loop 2 over the delta_1+ values
      !       =================================================
      !
    enddo
    !     ==================================================
    !     end of loop 1 over the f1/f0 ratios
    !     ==================================================
    deallocate(f1sf)
    deallocate(x_che_te1)
    deallocate(x_che_om2)
    deallocate(x_leg_om2)
    deallocate(w_leg_om2)
    !     ===========================================================
    !     post-processing to eliminate part of the configurations
    !     ===========================================================
    !
    !.....it looks, for every value of the ratio v1, for the maximum value
    !.....of factor*coupling : it is stored in the local table naxcla(.)
    !     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    allocate(maxcla(1:gqnf1))
    do jf1=1,gqnf1
      aux=0.0d0
      do jt1=1,gqnt1
        do iq_om2=1,gqnq_om2
          aux=max(aux,tb_fac(iq_om2,jt1,jf1)*tb_tpm(iq_om2,jt1,jf1),tb_fac(iq_om2,jt1,jf1)*tb_tmp(iq_om2,jt1,jf1))
        enddo
      enddo
      maxcla(jf1)=aux
    enddo
    !
    !.....it looks for the max v1 value
    !     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    aux=0.0d0
    do jf1=1,gqnf1
      if (maxcla(jf1).gt.aux) aux=maxcla(jf1)
    enddo
    test1=seuil1*aux
    !
    !.....set to zero the coupling coefficients not used
    !     """""""""""""""""""""""""""""""""""""""""""""""""""""
    nconf=0
    do jf1=1,gqnf1
      test2 =seuil2*maxcla(jf1)
      do jt1=1,gqnt1
        do iq_om2=1,gqnq_om2
          aaa=tb_fac(iq_om2,jt1,jf1)*tb_tpm(iq_om2,jt1,jf1)
          ccc=tb_fac(iq_om2,jt1,jf1)*tb_tmp(iq_om2,jt1,jf1)
          if ((aaa.gt.test1.or.aaa.gt.test2).or. &
               (ccc.gt.test1.or.ccc.gt.test2)) then
            nconf=nconf+1
            idconf(nconf,1)=jf1
            idconf(nconf,2)=jt1
            idconf(nconf,3)=iq_om2
          endif
        enddo
      enddo
    enddo
    deallocate(maxcla)
    !
    !..... counts the fraction of the eliminated configurations
    elim=(1.d0-dble(nconf)/dble(nconfm))*100.d0
  end subroutine insnlgqm
  !/
  !/ end of module w3snl1md -------------------------------------------- /
  !/
end module w3snl1md
