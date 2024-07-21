!> @file
!> @brief observation-based wind input and dissipation after donelan et al (2006),
!>  and babanin et al. (2010).
!>
!> @author s. zieger
!> @author q. liu
!> @date   26-jun-2018
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
!> @brief observation-based wind input and dissipation after donelan et al (2006),
!>  and babanin et al. (2010).
!>
!> @details parameterisation is based on the field
!>  data from lake george, australia. initial implementation of input
!>  and dissipation is based on work from tsagareli et al. (2010) and
!>  rogers et al. (2012). parameterisation extended and account for
!>  negative input due to opposing winds (see donelan et al, 2006) and
!>  the vector version of the stress computation.
!>
!> @author s. zieger
!> @author q. liu
!> @date   26-jun-2018
!>
module w3src6md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii      noaa/ncep/nopp |
  !/                  |           s. zieger               |
  !/                  |           q. liu                  |
  !/                  |                        fortran 90 |
  !/                  | last update :         26-jun-2018 |
  !/                  +-----------------------------------+
  !/
  !/    29-may-2009 : origination (w3srcxmd.ftn)          ( version 3.14 )
  !/    10-feb-2011 : implementation of source terms      ( version 4.04 )
  !/                                                         (s. zieger)
  !/    26-jun-2017 : recalibration of st6                ( verison 6.06 )
  !/                                                         (q. liu   )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !      observation-based wind input and dissipation after donelan et al (2006),
  !      and babanin et al. (2010). parameterisation is based on the field
  !      data from lake george, australia. initial implementation of input
  !      and dissipation is based on work from tsagareli et al. (2010) and
  !      rogers et al. (2012). parameterisation extended and account for
  !      negative input due to opposing winds (see donelan et al, 2006) and
  !      the vector version of the stress computation.
  !
  !      references:
  !       babanin   et al. 2010: jpo   40(4)  667- 683
  !       donelan   et al. 2006: jpo   36(8) 1672-1689
  !       tsagareli et al. 2010: jpo   40(4)  656- 666
  !       rogers    et al. 2012: jtech 29(9) 1329-1346
  !
  !  2. variables and types :
  !
  !      not applicable.
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3spr6    subr. public   integral parameter calculation following !/st1.
  !      w3sin6    subr. public   observation-based wind input.
  !      w3sds6    subr. public   observation-based dissipation.
  !
  !      irange    func. private  generate a sequence of integer values.
  !      lfactor   func. private  calculate reduction factor for sin.
  !      tauwinds  func. private  normal stress calculation for sin.
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
  !  6. switches :
  !
  !      !/s   enable subroutine tracing.
  !      !/t6  enable test output for wind input and dissipation subroutines.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public  ::  w3spr6, w3sin6, w3sds6
  private ::  lfactor, tauwinds, irange
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate mean wave parameters.
  !>
  !> @details see source term routines.
  !>
  !> @param[inout] a        action as a function of direction and wavenumber.
  !> @param[inout] cg       group velocities.
  !> @param[inout] wn       wavenumbers.
  !> @param[inout] emean    mean wave energy.
  !> @param[inout] fmean    mean wave frequency.
  !> @param[inout] wnmean   mean wavenumber.
  !> @param[inout] amax     maximum action density in spectrum.
  !> @param[inout] fp       peak frequency (rad).
  !>
  !> @author s. zieger
  !> @date   11-feb-2011
  !>
  subroutine w3spr6 (a, cg, wn, emean, fmean, wnmean, amax, fp)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii      noaa/ncep/nopp |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-feb-2011 |
    !/                  +-----------------------------------+
    !/
    !/    08-oct-2007 : origination.                        ( version 3.13 )
    !/    11-feb-2011 : implementation based on w3spr1      ( version 4.04 )
    !/                                                        (s. zieger)
    !/
    !  1. purpose :
    !      calculate mean wave parameters.
    !
    !  2. method :
    !      see source term routines.
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      a      r.a. i  action as a function of direction and wavenumber
    !      cg     r.a. i  group velocities
    !      wn     r.a. i  wavenumbers
    !      emean  real o  mean wave energy
    !      fmean  real o  mean wave frequency
    !      wnmean real o  mean wavenumber
    !      amax   real o  maximum action density in spectrum
    !      fp     real o  peak frequency (rad)
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
    !       none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !      see source code.
    !
    !  9. switches :
    !
    !      !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpiinv
    use w3gdatmd,  only: nk, nth, sig, dth, dden, fte, ftf, ftwn, dsii
    use w3odatmd,  only: ndst, ndse
    use w3servmd,  only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk), cg(nk), wn(nk)
    real, intent(out)       :: emean, fmean, wnmean, amax, fp
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: imax
    real                    :: eb(nk), eband
    real, parameter         :: hsmin = 0.05
    real                    :: coeff(3)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  integrate over directions -------------------------------------- /
    eb   = sum(a,1) * dden / cg
    amax = maxval(a)
    !
    ! 2.  integrate over wavenumbers ------------------------------------- /
    emean  = sum(eb)
    fmean  = sum(eb / sig(1:nk))
    wnmean = sum(eb / sqrt(wn))
    !
    ! 3.  add tail beyond discrete spectrum and get mean pars ------------ /
    !     ( dth * sig absorbed in ftxx )
    eband  = eb(nk) / dden(nk)
    emean  = emean  + eband * fte
    fmean  = fmean  + eband * ftf
    wnmean = wnmean + eband * ftwn
    !
    ! 4.  final processing
    fmean  = tpiinv * emean / max(1.0e-7, fmean)
    wnmean = ( emean / max(1.0e-7,wnmean) )**2
    !
    ! 5.  determine peak frequency using a weighted integral ------------- /
    !     young (1999) p239: integrate f f**4 df / integrate f**4 df ----- /
    !     todo: keep in mind that **fp** calculated in this way may not
    !           work under mixing (wind-sea and swell) sea states (ql)
    fp    = 0.0
    !
    if (4.0*sqrt(emean) .gt. hsmin) then
      eb = sum(a,1) * sig(1:nk) /cg * dth
      fp = sum(sig(1:nk) * eb**4 * dsii) / max(1e-10, sum(eb**4 * dsii))
      fp = fp * tpiinv
    end if
    !
    return
    !/
    !/ end of w3spr6 ----------------------------------------------------- /
    !/
  end subroutine w3spr6
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief observation-based source term for wind input.
  !>
  !> @details after donelan, babanin, young and banner (donelan et al ,2006)
  !>  following the implementation by rogers et al. (2012).
  !>
  !>      references:
  !>        donelan et al. 2006: jpo 36(8) 1672-1689.
  !>        rogers  et al. 2012: jtech 29(9) 1329-1346
  !>
  !>
  !> @param[in]  a        action density spectrum
  !> @param[in]  cg       group velocities.
  !> @param[in]  wn2      wavenumbers.
  !> @param[in]  uabs     wind speed at 10 m above sea level (u10).
  !> @param[in]  ustar    friction velocity.
  !> @param[in]  usdir    direction of ustar.
  !> @param[in]  cd       drag coefficient.
  !> @param[in]  dair     air density.
  !> @param[out] tauwx    component of the wave-supported stress.
  !> @param[out] tauwy    component of the wave-supported stress.
  !> @param[out] tauwnx   component of the negative part of the stress.
  !> @param[out] tauwny   component of the negative part of the stress.
  !> @param[out] s        source term.
  !> @param[out] d        diagonal term of derivative.
  !>
  !> @author s. zieger
  !> @author q. liu
  !> @date   13-aug-2021
  !>
  subroutine w3sin6 (a, cg, wn2, uabs, ustar, usdir, cd, dair, &
       tauwx, tauwy, taunwx, taunwy, s, d )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii      noaa/ncep/nopp |
    !/                  |           s. zieger               |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-aug-2021 |
    !/                  +-----------------------------------+
    !/
    !/    20-dec-2010 : origination.                        ( version 4.04 )
    !/                                                        (s. zieger)
    !/
    !/    26-jun-2018 : uproxy update & uabs                ( version 6.06 )
    !/                                                        (q. liu)
    !/    13-aug-2021 : consider dair a variable           ( version x.xx )
    !/
    !  1. purpose :
    !
    !      observation-based source term for wind input after donelan, babanin,
    !      young and banner (donelan et al ,2006) following the implementation
    !      by rogers et al. (2012).
    !
    !      references:
    !       donelan et al. 2006: jpo 36(8) 1672-1689.
    !       rogers  et al. 2012: jtech 29(9) 1329-1346
    !
    !  2. method :
    !
    !      sin = b * e
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      a¹       r.a. i  action density spectrum
    !      cg       r.a. i  group velocities
    !      wn2¹     r.a. i  wavenumbers
    !      uabs     real i  wind speed at 10 m above sea level (u10)
    !      ustar    real i  friction velocity
    !      usdir    real i  direction of ustar
    !      cd       real i  drag coefficient
    !      dair     real i  air density
    !      s¹       r.a. o  source term
    !      d¹       r.a. o  diagonal term of derivative
    !      tauwx-y  real o  component of the wave-supported stress
    !      taunwx-y real o  component of the negative part of the stress
    !      ¹ stored as 1-d array with dimension nth*nk (column by column).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      lfactor   subr. w3src6md
    !      irange    func. w3src6md
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
    !      none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !      see comments in source code.
    !
    !  9. switches :
    !
    !      !/s   enable subroutine tracing.
    !      !/t6  test and diagnostic output for tail reduction.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: dwat, tpi, grav
    use w3gdatmd,  only: nk, nth, nspec, dth, sig2, dden2
    use w3gdatmd,  only: ecos, esin, sin6a0, sin6ws
    use w3odatmd,  only: ndse
    use w3servmd,  only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)       :: a (nspec), cg(nk), wn2(nspec)
    real, intent(in)       :: uabs, ustar, usdir, cd, dair
    real, intent(out)      :: tauwx, tauwy, taunwx, taunwy
    real, intent(out)      :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    integer                :: ik, ith, ikn(nk)
    real                   :: cosu, sinu, uproxy
    real, dimension(nspec) :: cg2, ecos2, esin2, dsii2
    real, dimension(nk)    :: dsii, sig, wn
    real                   :: k(nth,nk), sdensig(nth,nk)           ! 1,2,5)
    real, dimension(nk)    :: adensig, kmax, anar, sqrtbn          ! 1,2,3)
    real, dimension(nspec) :: w1, w2, sqrtbn2, cinv2               ! 4,7)
    real, dimension(nk)    :: lfact, cinv                          ! 5)
    !/ ------------------------------------------------------------------- /
    !
    !/ 0) --- set up a basic variables ----------------------------------- /
    cosu   = cos(usdir)
    sinu   = sin(usdir)
    !
    taunwx = 0.
    taunwy = 0.
    tauwx  = 0.
    tauwy  = 0.
    !
    !/    --- scale  friction velocity to wind speed (10m) in
    !/        the boundary layer ----------------------------------------- /
    !/    donelan et al. (2006) used u10 or u_{λ/2} in their s_{in}
    !/    parameterization. to avoid some disadvantages of using u10 or
    !/    u_{λ/2}, rogers et al. (2012) used the following engineering
    !/    conversion:
    !/                    uproxy = sin6ws * ust
    !/
    !/    sin6ws = 28.0 following komen et al. (1984)
    !/    sin6ws = 32.0 suggested by e. rogers (2014)
    !
    uproxy = sin6ws * ustar        ! scale wind speed
    !
    ecos2  = ecos(1:nspec)         ! only indices from 1 to nspec
    esin2  = esin(1:nspec)         ! are requested.
    !
    ikn    = irange(1,nspec,nth)   ! index vector for elements of 1 ... nk
    !                                    ! such that e.g. sig(1:nk) = sig2(ikn).
    dsii2  = dden2 / dth / sig2    ! frequency bandwidths (int.)  (rad)
    dsii   = dsii2(ikn)
    sig    = sig2(ikn)
    wn     = wn2(ikn)
    cinv2  = wn2 / sig2            ! inverse phase speed
    !
    do ith = 1, nth
      cg2(ikn+(ith-1)) = cg       ! apply cg to all directions.
    end do
    !
    !/ 1) --- calculate 1d action density spectrum (a(sigma)) and
    !/        zero-out values less than 1.0e-32 to avoid nans when
    !/        computing directional narrowness in step 4). --------------- /
    k       = reshape(a,(/ nth,nk /))
    adensig = sum(k,1) * sig * dth ! integrate over directions.
    !
    !/ 2) --- calculate normalised directional spectrum k(theta,sigma) --- /
    kmax = maxval(k,1)
    do ik = 1,nk
      if (kmax(ik).lt.1.0e-34) then
        k(1:nth,ik) = 1.
      else
        k(1:nth,ik) = k(1:nth,ik)/kmax(ik)
      end if
    end do
    !
    !/ 3) --- calculate normalised spectral saturation bn(ik) ------------ /
    anar    = 1.0/( sum(k,1) * dth )          ! directional narrowness
    !
    sqrtbn  = sqrt( anar * adensig * wn**3 )
    do ith  = 1, nth
      sqrtbn2(ikn+(ith-1)) = sqrtbn          ! calculate sqrtbn for
    end do                                    ! the entire spectrum.
    !
    !/ 4) --- calculate growth rate gamma and s for all directions for
    !/        following winds (u10/c - 1 is positive; w1) and in 7) for
    !/        adverse winds (u10/c -1 is negative, w2). w1 and w2
    !/        complement one another. ------------------------------------ /
    w1      = max( 0., uproxy * cinv2* ( ecos2*cosu + esin2*sinu ) - 1. )**2
    !
    d       = (dair / dwat) * sig2 * &
         (2.8 - ( 1. + tanh(10.*sqrtbn2*w1 - 11.) )) *sqrtbn2*w1
    !
    s       = d * a
    !
    !/ 5) --- calculate reduction factor lfact using non-directional
    !         spectral density of the wind input ------------------------- /
    cinv    = cinv2(ikn)
    sdensig = reshape(s*sig2/cg2,(/ nth,nk /))
    call lfactor(sdensig, cinv, uabs, ustar, usdir, sig, dsii, &
         lfact, tauwx, tauwy                          )
    !
    !/ 6) --- apply reduction (lfact) to the entire spectrum ------------- /
    if (sum(lfact) .lt. nk) then
      do ith = 1, nth
        d(ikn+ith-1) = d(ikn+ith-1) * lfact
      end do
      s = d * a
    end if
    !
    !/ 7) --- compute negative wind input for adverse winds. negative
    !/        growth is typically smaller by a factor of ~2.5 (=.28/.11)
    !/        than those for the favourable winds [donelan, 2006, eq. (7)].
    !/        the factor is adjustable with namelist parameter in
    !/        ww3_grid.inp: '&sin6 sina0 = 0.04 /' ----------------------- /
    if (sin6a0.gt.0.0) then
      w2    = min( 0., uproxy * cinv2* ( ecos2*cosu + esin2*sinu ) - 1. )**2
      d     = d - ( (dair / dwat) * sig2 * sin6a0 *                   &
           (2.8 - ( 1. + tanh(10.*sqrtbn2*w2 - 11.) )) *sqrtbn2*w2 )
      s     = d * a
      !     --- compute negative component of the wave supported stresses
      !         from negative part of the wind input  ---------------------- /
      sdensig = reshape(s*sig2/cg2,(/ nth,nk /))
      call tau_wave_atmos(sdensig, cinv, sig, dsii, taunwx, taunwy )
    end if
    !
    !/
    !/ end of w3sin6 ----------------------------------------------------- /
    !/
  end subroutine w3sin6
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief observation-based source term for dissipation.
  !>
  !> @details after babanin et al. (2010) following the implementation by
  !>  rogers et al. (2012). the dissipation function sds accommodates an
  !>  inherent breaking term t1 and an additional cumulative term t2 at
  !>  all frequencies above the peak. the forced dissipation term t2 is
  !>  an integral that grows toward higher frequencies and dominates at
  !>  smaller scales (babanin et al. 2010).
  !>
  !>  references:
  !>    babanin et al. 2010: jpo 40(4), 667-683
  !>    rogers  et al. 2012: jtech 29(9) 1329-1346
  !>
  !> @param[in]  a   action density spectrum.
  !> @param[in]  cg  group velocities.
  !> @param[in]  wn  wavenumbers.
  !> @param[out] s   source term (1-d version).
  !> @param[out] d   diagonal term of derivative.
  !>
  !> @author s. zieger
  !> @author q. liu
  !> @date   26-jun-2018
  !>
  subroutine w3sds6 (a, cg, wn, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    23-jun-2010 : origination.                        ( version 4.04 )
    !/                                                        (s. zieger)
    !/    26-jun-2018 : revise the width of the last bin    ( version 6.06 )
    !/                                                        (q. liu)
    !/
    !  1. purpose :
    !
    !      observation-based source term for dissipation after babanin et al.
    !      (2010) following the implementation by rogers et al. (2012). the
    !      dissipation function sds accommodates an inherent breaking term t1
    !      and an additional cumulative term t2 at all frequencies above the
    !      peak. the forced dissipation term t2 is an integral that grows
    !      toward higher frequencies and dominates at smaller scales
    !      (babanin et al. 2010).
    !
    !      references:
    !       babanin et al. 2010: jpo 40(4), 667-683
    !       rogers  et al. 2012: jtech 29(9) 1329-1346
    !
    !  2. method :
    !
    !      sds = (t1 + t2) * e
    !
    !  3. parameters :
    !
    !      parameter list
    !     ----------------------------------------------------------------
    !      a¹      r.a. i  action density spectrum
    !      cg      r.a. i  group velocities
    !      wn      r.a. i  wavenumbers
    !      s¹      r.a. o  source term (1-d version)
    !      d¹      r.a. o  diagonal term of derivative
    !      ¹ stored as 1-d array with dimension nth*nk (column by column).
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
    !      none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !      see source code.
    !
    !  9. switches :
    !
    !      !/s   enable subroutine tracing.
    !      !/t6  test output for dissipation terms t1 and t2.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: grav, tpi
    use w3gdatmd,  only: nk, nth, nspec, dden, dsii, sig2, dth, xfr
    use w3gdatmd,  only: sds6a1, sds6a2, sds6p1, sds6p2, sds6et
    use w3odatmd,  only: ndse
    use w3servmd,  only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)  :: a(nspec), cg(nk), wn(nk)
    real, intent(out) :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    integer           :: ik, ith, ikn(nk)
    real              :: freq(nk)     ! frequencies [hz]
    real              :: dfii(nk)     ! frequency bandwiths [hz]
    real              :: anar(nk)     ! directional narrowness
    real              :: bnt          ! empirical constant for
    ! wave breaking probability
    real              :: edens (nk)   ! spectral density e(f)
    real              :: etdens(nk)   ! threshold spec. density et(f)
    real              :: exdens(nk)   ! excess spectral density ex(f)
    real              :: nexdens(nk)  ! normalised excess spectral density
    real              :: t1(nk)       ! inherent breaking term
    real              :: t2(nk)       ! forced dissipation term
    real              :: t12(nk)      ! = t1+t2 or combined dissipation
    real              :: adf(nk), xfac, edensmax ! temp. variables
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/ 0) --- initialize essential parameters ---------------------------- /
    ikn     = irange(1,nspec,nth)    ! index vector for elements of 1,
    !                                      ! 2,..., nk such that for example
    !                                      ! sig(1:nk) = sig2(ikn).
    freq    = sig2(ikn)/tpi
    anar    = 1.0
    bnt     = 0.035**2
    t1      = 0.0
    t2      = 0.0
    nexdens = 0.0
    !
    !/ 1) --- calculate threshold spectral density, spectral density, and
    !/        the level of exceedence exdens(f) -------------------------- /
    etdens  = ( tpi * bnt ) / ( anar * cg * wn**3 )
    edens   = sum(reshape(a,(/ nth,nk /)),1) * tpi * sig2(ikn) * dth / cg  ! e(f)
    exdens  = max(0.0,edens-etdens)
    !
    !/    --- normalise by a generic spectral density -------------------- /
    if (sds6et) then                ! ww3_grid.inp: &sds6 sdset = t or f
      nexdens = exdens / etdens    ! normalise by threshold spectral density
    else                            ! normalise by spectral density
      edensmax = maxval(edens)*1e-5
      if (all(edens .gt. edensmax)) then
        nexdens = exdens / edens
      else
        do ik = 1,nk
          if (edens(ik) .gt. edensmax) nexdens(ik) = exdens(ik) / edens(ik)
        end do
      end if
    end if
    !
    !/ 2) --- calculate inherent breaking component t1 ------------------- /
    t1 = sds6a1 * anar * freq * (nexdens**sds6p1)
    !
    !/ 3) --- calculate t2, the dissipation of waves induced by
    !/        the breaking of longer waves t2 ---------------------------- /
    adf    = anar * (nexdens**sds6p2)
    xfac   = (1.0-1.0/xfr)/(xfr-1.0/xfr)
    do ik = 1,nk
      dfii = dsii/tpi
      !        if (ik .gt. 1) dfii(ik) = dfii(ik) * xfac
      if (ik .gt. 1 .and. ik .lt. nk) dfii(ik) = dfii(ik) * xfac
      t2(ik) = sds6a2 * sum( adf(1:ik)*dfii(1:ik) )
    end do
    !
    !/ 4) --- sum up dissipation terms and apply to all directions ------- /
    t12 = -1.0 * ( max(0.0,t1)+max(0.0,t2) )
    do ith = 1, nth
      d(ikn+ith-1) = t12
    end do
    !
    s = d * a
    !
    !/ 5) --- diagnostic output (switch !/t6) ---------------------------- /
    !/
    !/ end of w3sds6 ----------------------------------------------------- /
    !/
  end subroutine w3sds6
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief numerical approximation for the reduction factor.
  !>
  !> @details numerical approximation for the reduction factor lfactor(f) to
  !>  reduce energy in the high-frequency part of the resolved part
  !>  of the spectrum to meet the constraint on total stress (tau).
  !>  the constraint is tau <= tau_tot (tau_tot = tau_wav + tau_vis),
  !>  thus the wind input is reduced to match our constraint.
  !>
  !> @param[in]  s      wind input energy density spectrum.
  !> @param[in]  cinv   inverse phase speed.
  !> @param[in]  u10    wind speed.
  !> @param[in]  ustar  friction velocity.
  !> @param[in]  usdir  wind direction.
  !> @param[in]  sig    relative frequencies (in rad.).
  !> @param[in]  dsii   frequency bandwidths (in rad.).
  !> @param[out] lfact  factor array.
  !> @param[out] tauwx  component of the wave-supported stress.
  !> @param[out] tauwy  component of the wave-supported stress.
  !>
  !> @author s. zieger
  !> @author q. liu
  !> @date   26-jun-2018
  !>
  subroutine lfactor(s, cinv, u10, ustar, usdir, sig, dsii, &
       lfact, tauwx, tauwy                    )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    15-feb-2011 : implemented following rogers et al. (2012)
    !/                                                        (s. zieger)
    !/    26-jun-2018 : uproxy, dsii10hz updates            ( version 6.06 )
    !/                                                        (q. liu   )
    !
    !     rogers et al. (2012) jtech 29(9), 1329-1346
    !
    !  1. purpose :
    !
    !      numerical approximation for the reduction factor lfactor(f) to
    !      reduce energy in the high-frequency part of the resolved part
    !      of the spectrum to meet the constraint on total stress (tau).
    !      the constraint is tau <= tau_tot (tau_tot = tau_wav + tau_vis),
    !      thus the wind input is reduced to match our constraint.
    !
    !  2. method :
    !
    !     1) if required, extend resolved part of the spectrum to 10hz using
    !        an approximation for the spectral slope at the high frequency
    !        limit: sin(f) prop. f**(-2) and for e(f) prop. f**(-5).
    !     2) calculate stresses:
    !        total stress:     tau_tot  = dair * ustar**2
    !        viscous stress:   tau_vis  = dair * cv * u10**2
    !        viscous stress (x,y-components):
    !                          tauv_x   = tau_vis * cos(usdir)
    !                          tauv_y   = tau_vis * sin(usdir)
    !        wave supported stress (x,y-components):    /10hz
    !                          tauw_x,y = grav * dwat * | [sinx,y(f)]/c(f) df
    !                                                   /
    !        total stress (input):   tau  = sqrt( (tauw_x + tauv_x)**2
    !                                     + (tauw_y + tauv_y)**2 )
    !     3) if tau does not meet our constraint reduce the wind input
    !        using reduction factor:
    !                          lfact(f) = min(1,exp((1-u/c(f))*rtau))
    !        then alter rtau and repeat 3) until our constraint is matched.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      s       r.a. i  wind input energy density spectrum  (s_{in}(σ, θ))
    !      cinv    r.a. i  inverse phase speed                  1/c(sigma)
    !      u10     real i  wind speed (10m)
    !      ustar   real i  friction velocity
    !      usdir   real i  wind direction
    !      sig     r.a. i  relative frequencies [in rad.]
    !      dsii    r.a. i  frequency bandwiths [in rad.]
    !      lfactor r.a. o  factor array                       lfact(sigma)
    !      tauwx-y real o  component of the wave-supported stress
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      irange    func. private  index generator (ie, array addressing)
    !      tauwinds  func. private  normal stress calculation (tau_nrm)
    !     ----------------------------------------------------------------
    !
    !     ----------------------------------------------------------------
    !
    !  5. error messages :
    !
    !      a warning is issued to ndst using format 280 if the iteration
    !      procedure reaches the upper iteration limit (itermax). in this
    !      case the last approximation for rtau is used.
    !
    !/
    use constants, only: dair, grav, tpi
    use w3gdatmd,  only: nk, nth, nspec, dth, xfr, ecos, esin
    use w3gdatmd,  only: sin6ws
    use w3odatmd,  only: ndst, ndse, iaproc, naperr
    use w3timemd,  only: stme21
    use w3wdatmd,  only: time
    implicit none
    !
    !/ ------ i/o parameters --------------------------------------------- /
    real, intent(in)  :: s(nth,nk)      ! wind-input source term sin
    real, intent(in)  :: cinv(nk)       ! inverse phase speed
    real, intent(in)  :: u10            ! wind speed
    real, intent(in)  :: ustar, usdir   ! friction velocity & direction
    real, intent(in)  :: sig(nk)        ! relative frequencies
    real, intent(in)  :: dsii(nk)       ! frequency bandwidths
    real, intent(out) :: lfact(nk)      ! correction factor
    real, intent(out) :: tauwx, tauwy   ! normal stress components
    !
    !/    --- local parameters (in order of appearance) ------------------ /
    real, parameter   :: frqmax  = 10.  ! upper freq. limit to extrapolate to.
    integer, parameter:: itermax = 80   ! maximum number of iterations to
    ! find numerical solution for lfact.
    integer           :: ik, nk10hz, sign_new, sign_old
    !
    real              :: ecos2(nspec), esin2(nspec)
    real, allocatable :: ik10hz(:), lf10hz(:), sig10hz(:), cinv10hz(:)
    real, allocatable :: sdens10hz(:), sdensx10hz(:), sdensy10hz(:)
    real, allocatable :: dsii10hz(:), ucinv10hz(:)
    real              :: tau_tot, tau, tau_vis, tau_wav
    real              :: tauvx, tauvy, taux, tauy
    real              :: tau_nnd, tau_init(2)
    real              :: uproxy, rtau, drtau, err
    logical           :: overshot
    character(len=23) :: idtime
    !
    !/ ------------------------------------------------------------------- /
    !
    !/ 0) --- find the number of frequencies required to extend arrays
    !/        up to f=10hz and allocate arrays --------------------------- /
    !/    alog is the same as log
    nk10hz = ceiling(alog(frqmax/(sig(1)/tpi))/alog(xfr))+1
    nk10hz = max(nk,nk10hz)
    !
    allocate(ik10hz(nk10hz))
    ik10hz = real( irange(1,nk10hz,1) )
    !
    allocate(sig10hz(nk10hz))
    allocate(cinv10hz(nk10hz))
    allocate(dsii10hz(nk10hz))
    allocate(lf10hz(nk10hz))
    allocate(sdens10hz(nk10hz))
    allocate(sdensx10hz(nk10hz))
    allocate(sdensy10hz(nk10hz))
    allocate(ucinv10hz(nk10hz))
    !
    ecos2  = ecos(1:nspec)
    esin2  = esin(1:nspec)
    !
    !/ 1) --- either extrapolate arrays up to 10hz or use discrete spectral
    !         grid per se. limit the constraint to the positive part of the
    !         wind input only. ---------------------------------------------- /
    if (nk .lt. nk10hz) then
      sdens10hz(1:nk)         = sum(s,1) * dth
      sdensx10hz(1:nk)        = sum(max(0.,s)*reshape(ecos2,(/nth,nk/)),1) * dth
      sdensy10hz(1:nk)        = sum(max(0.,s)*reshape(esin2,(/nth,nk/)),1) * dth
      sig10hz                 = sig(1)*xfr**(ik10hz-1.0)
      cinv10hz(1:nk)          = cinv
      cinv10hz(nk+1:nk10hz)   = sig10hz(nk+1:nk10hz)*0.101978 ! 1/c=σ/g
      dsii10hz                = 0.5 * sig10hz * (xfr-1.0/xfr)
      !        the first and last frequency bin:
      dsii10hz(1)             = 0.5 * sig10hz(1) * (xfr-1.0)
      dsii10hz(nk10hz)        = 0.5 * sig10hz(nk10hz) * (xfr-1.0) / xfr
      !
      !        --- spectral slope for s_in(f) is proportional to f**(-2) ------ /
      sdens10hz(nk+1:nk10hz)  = sdens10hz(nk)  * (sig10hz(nk)/sig10hz(nk+1:nk10hz))**2
      sdensx10hz(nk+1:nk10hz) = sdensx10hz(nk) * (sig10hz(nk)/sig10hz(nk+1:nk10hz))**2
      sdensy10hz(nk+1:nk10hz) = sdensy10hz(nk) * (sig10hz(nk)/sig10hz(nk+1:nk10hz))**2
    else
      sig10hz          = sig
      cinv10hz         = cinv
      dsii10hz         = dsii
      sdens10hz(1:nk)  = sum(s,1) * dth
      sdensx10hz(1:nk) = sum(max(0.,s)*reshape(ecos2,(/nth,nk/)),1) * dth
      sdensy10hz(1:nk) = sum(max(0.,s)*reshape(esin2,(/nth,nk/)),1) * dth
    end if
    !
    !/ 2) --- stress calculation ----------------------------------------- /
    !     --- the total stress ------------------------------------------- /
    tau_tot  = ustar**2 * dair
    !
    !     --- the viscous stress and check that it does not exceed
    !         the total stress. ------------------------------------------ /
    tau_vis  = max(0.0, -5.0e-5*u10 + 1.1e-3) * u10**2 * dair
    !     tau_vis  = min(0.9 * tau_tot, tau_vis)
    tau_vis  = min(0.95 * tau_tot, tau_vis)
    !
    tauvx    = tau_vis * cos(usdir)
    tauvy    = tau_vis * sin(usdir)
    !
    !     --- the wave supported stress. --------------------------------- /
    tauwx    = tauwinds(sdensx10hz,cinv10hz,dsii10hz)   ! normal stress (x-component)
    tauwy    = tauwinds(sdensy10hz,cinv10hz,dsii10hz)   ! normal stress (y-component)
    tau_nnd  = tauwinds(sdens10hz, cinv10hz,dsii10hz)   ! normal stress (non-directional)
    tau_wav  = sqrt(tauwx**2 + tauwy**2)                ! normal stress (magnitude)
    tau_init = (/tauwx,tauwy/)                          ! unadjusted normal stress components
    !
    taux     = tauvx + tauwx                            ! total stress (x-component)
    tauy     = tauvy + tauwy                            ! total stress (y-component)
    tau      = sqrt(taux**2  + tauy**2)                 ! total stress (magnitude)
    err      = (tau-tau_tot)/tau_tot                    ! initial error
    !
    !/ 3) --- find reduced sin(f) = l(f)*sin(f) to satisfy our constraint
    !/        tau <= tau_tot --------------------------------------------- /
    call stme21 ( time , idtime )
    lf10hz = 1.0
    ik = 0
    !
    if (tau .gt. tau_tot) then
      overshot    = .false.
      rtau        = err / 90.
      drtau       = 2.0
      sign_new    = int(sign(1.0,err))
      uproxy      = sin6ws * ustar
      ucinv10hz   = 1.0 - (uproxy * cinv10hz)
      !
      do ik=1,itermax
        lf10hz   = min(1.0, exp(ucinv10hz * rtau) )
        !
        tau_nnd  = tauwinds(sdens10hz *lf10hz,cinv10hz,dsii10hz)
        tauwx    = tauwinds(sdensx10hz*lf10hz,cinv10hz,dsii10hz)
        tauwy    = tauwinds(sdensy10hz*lf10hz,cinv10hz,dsii10hz)
        tau_wav  = sqrt(tauwx**2 + tauwy**2)
        !
        taux     = tauvx + tauwx
        tauy     = tauvy + tauwy
        tau      = sqrt(taux**2 + tauy**2)
        err      = (tau-tau_tot) / tau_tot
        !
        sign_old = sign_new
        sign_new = int(sign(1.0, err))
        !
        !        --- slow down drtau when overshot. -------------------------- /
        if (sign_new .ne. sign_old) overshot = .true.
        if (overshot) drtau = max(0.5*(1.0+drtau),1.00010)
        !
        rtau = rtau * (drtau**sign_new)
        !
        if (abs(err) .lt. 1.54e-4) exit
      end do
      !
      if (ik .ge. itermax) write (ndst,280) idtime(1:19), u10, tau, &
           tau_tot, err, tauwx, tauwy, tauvx, tauvy,tau_nnd
    end if
    !
    lfact(1:nk) = lf10hz(1:nk)
    !
274 format (' test w3sin6 : total ',a,' =', e13.5                  )
280 format (' warning lfactor (time,u10,tau,tau_tot,err,tauw_xy,'  &
         'tauv_xy,tau_scalar): ',a,f6.1,2f7.4,e10.3,4f7.4,f7.3  )
    !
    deallocate(ik10hz,sig10hz,cinv10hz,dsii10hz,lf10hz)
    deallocate(sdens10hz,sdensx10hz,sdensy10hz,ucinv10hz)
    !/
  end subroutine lfactor
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief calculated the stress for the negative part of the input term.
  !>
  !> @details calculated the stress for the negative part of the input term,
  !>  that is the stress from the waves to the atmosphere. relevant
  !>  in the case of opposing winds.
  !>
  !> @param[in]  s        wind-input source term sin.
  !> @param[in]  cinv     inverse phase speed.
  !> @param[in]  sig      relative frequencies.
  !> @param[in]  dsii     frequency bandwidths.
  !> @param[out] taunwx   stress components (wave->atmos).
  !> @param[out] taunwy   stress components (wave->atmos).
  !>
  !> @author s. zieger
  !> @author q. liu
  !> @date   26-jun-2018
  !>
  subroutine tau_wave_atmos(s, cinv, sig, dsii, taunwx, taunwy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         26-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    24-oct-2013 : origination following lfactor
    !/                                                        (s. zieger)
    !/    26-jun-2018 : updates on dsii10hz                 ( version 6.06)
    !/                                                        (q. liu)
    !
    !  1. purpose :
    !
    !     calculated the stress for the negative part of the input term,
    !     that is the stress from the waves to the atmosphere. relevant
    !     in the case of opposing winds.
    !
    !  2. method :
    !     1) if required, extend resolved part of the spectrum to 10hz using
    !        an approximation for the spectral slope at the high frequency
    !        limit: sin(f) prop. f**(-2) and for e(f) prop. f**(-5).
    !     2) calculate stresses:
    !        stress components (x,y):      /10hz
    !            taunw_x,y = grav * dwat * | [sinx,y(f)]/c(f) df
    !                                      /
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      s        r.a. i  wind input energy density spectrum
    !      cinv     r.a. i  inverse phase speed                 1/c(sigma)
    !      sig      r.a. i  relative frequencies [in rad.]
    !      dsii     r.a. i  frequency bandwiths [in rad.]
    !      taunwx-y real o  component of the negative wave-supported stress
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  scope    description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      irange    func. private  index generator (ie, array addressing)
    !      tauwinds  func. private  normal stress calculation (tau_nrm)
    !     ----------------------------------------------------------------
    !
    !  5. source code :
    !
    !/
    use constants, only: grav, tpi
    use w3gdatmd,  only: nk, nth, nspec, dth, xfr, ecos, esin
    implicit none
    !
    !/ ------ i/o parameters --------------------------------------------- /
    real, intent(in)  :: s(nth,nk)      ! wind-input source term sin
    real, intent(in)  :: cinv(nk)       ! inverse phase speed
    real, intent(in)  :: sig(nk)        ! relative frequencies
    real, intent(in)  :: dsii(nk)       ! frequency bandwidths
    real, intent(out) :: taunwx, taunwy ! stress components (wave->atmos)
    !
    !/    --- local parameters (in order of appearance) ------------------ /
    real, parameter   :: frqmax  = 10.  ! upper freq. limit to extrapolate to.
    integer           :: nk10hz
    !
    real              :: ecos2(nspec), esin2(nspec)
    real, allocatable :: ik10hz(:), sig10hz(:), cinv10hz(:)
    real, allocatable :: sdensx10hz(:), sdensy10hz(:)
    real, allocatable :: dsii10hz(:), ucinv10hz(:)
    !
    !/ ------------------------------------------------------------------- /
    !
    !/ 0) --- find the number of frequencies required to extend arrays
    !/        up to f=10hz and allocate arrays --------------------------- /
    nk10hz = ceiling(alog(frqmax/(sig(1)/tpi))/alog(xfr))+1
    nk10hz = max(nk,nk10hz)
    !
    allocate(ik10hz(nk10hz))
    ik10hz = real( irange(1,nk10hz,1) )
    !
    allocate(sig10hz(nk10hz))
    allocate(cinv10hz(nk10hz))
    allocate(dsii10hz(nk10hz))
    allocate(sdensx10hz(nk10hz))
    allocate(sdensy10hz(nk10hz))
    allocate(ucinv10hz(nk10hz))
    !
    ecos2  = ecos(1:nspec)
    esin2  = esin(1:nspec)
    !
    !/ 1) --- either extrapolate arrays up to 10hz or use discrete spectral
    !         grid per se. limit the constraint to the positive part of the
    !         wind input only. ---------------------------------------------- /
    if (nk .lt. nk10hz) then
      sdensx10hz(1:nk)        = sum(abs(min(0.,s))*reshape(ecos2,(/nth,nk/)),1) * dth
      sdensy10hz(1:nk)        = sum(abs(min(0.,s))*reshape(esin2,(/nth,nk/)),1) * dth
      sig10hz                 = sig(1)*xfr**(ik10hz-1.0)
      cinv10hz(1:nk)          = cinv
      cinv10hz(nk+1:nk10hz)   = sig10hz(nk+1:nk10hz)*0.101978
      dsii10hz                = 0.5 * sig10hz * (xfr-1.0/xfr)
      !        the first and last frequency bin:
      dsii10hz(1)             = 0.5 * sig10hz(1) * (xfr-1.0)
      dsii10hz(nk10hz)        = 0.5 * sig10hz(nk10hz) * (xfr-1.0) / xfr
      !
      !        --- spectral slope for s_in(f) is proportional to f**(-2) ------ /
      sdensx10hz(nk+1:nk10hz) = sdensx10hz(nk) * (sig10hz(nk)/sig10hz(nk+1:nk10hz))**2
      sdensy10hz(nk+1:nk10hz) = sdensy10hz(nk) * (sig10hz(nk)/sig10hz(nk+1:nk10hz))**2
    else
      sig10hz          = sig
      cinv10hz         = cinv
      dsii10hz         = dsii
      sdensx10hz(1:nk) = sum(abs(min(0.,s))*reshape(ecos2,(/nth,nk/)),1) * dth
      sdensy10hz(1:nk) = sum(abs(min(0.,s))*reshape(esin2,(/nth,nk/)),1) * dth
    end if
    !
    !/ 2) --- stress calculation ----------------------------------------- /
    !     --- the wave supported stress (waves to atmosphere) ------------ /
    taunwx = tauwinds(sdensx10hz,cinv10hz,dsii10hz)   ! x-component
    taunwy = tauwinds(sdensy10hz,cinv10hz,dsii10hz)   ! y-component
    !/
  end subroutine tau_wave_atmos
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief generate a sequence of linear-spaced integer numbers.
  !>
  !> @details used for instance array addressing (indexing).
  !>
  !> @param   x0
  !> @param   x1
  !> @param   dx
  !> @returns ix
  !>
  !> @author s. zieger
  !> @date   15-feb-2011
  !>
  function irange(x0,x1,dx) result(ix)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-feb-2011 |
    !/                  +-----------------------------------+
    !/
    !/    15-feb-2011 : origination                         ( version 4.04 )
    !/                                                        (s. zieger)
    !/
    !  1. purpose :
    !         generate a sequence of linear-spaced integer numbers.
    !         used for instance array addressing (indexing).
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
  !>
  !> @brief wind stress (tau) computation from wind-momentum-input.
  !>
  !> @verbatim
  !>      wind stress (tau) computation from wind-momentum-input
  !>      function which can be obtained from wind-energy-input (sin).
  !>
  !>                            / frmax
  !>      tau = g * rho_water * | sin(f)/c(f) df
  !>                            /
  !> @endverbatim
  !>
  !> @param    sdensig    sin(sigma) in (m2/rad-hz).
  !> @param    cinv       inverse phase speed.
  !> @param    dsii       frequency bandwidths in (rad).
  !> @returns  tau_winds  wind stress.
  !>
  !> @author s. zieger
  !> @date   13-aug-2012
  !>
  function tauwinds(sdensig,cinv,dsii) result(tau_winds)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-aug-2012 |
    !/                  +-----------------------------------+
    !/
    !/    15-feb-2011 : origination                         ( version 4.04 )
    !/                                                        (s. zieger)
    !/
    !  1. purpose :
    !      wind stress (tau) computation from wind-momentum-input
    !      function which can be obtained from wind-energy-input (sin).
    !
    !                            / frmax
    !      tau = g * rho_water * | sin(f)/c(f) df
    !                            /
    !/
    use constants, only: grav, dwat    ! gravity, density of water
    implicit none
    real, intent(in)  :: sdensig(:)    ! sin(sigma) in [m2/rad-hz]
    real, intent(in)  :: cinv(:)       ! inverse phase speed
    real, intent(in)  :: dsii(:)       ! freq. bandwidths in [radians]
    real              :: tau_winds     ! wind stress
    !
    tau_winds = grav * dwat * sum(sdensig*cinv*dsii)
    !/
  end function tauwinds
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3src6md -------------------------------------------- /
  !/
end module w3src6md
