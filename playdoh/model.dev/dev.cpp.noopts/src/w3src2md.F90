!> @file
!> @brief contains module w3src2md.
!>
!> @author h. l. tolman  @date 29-may-2009
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
!>
!> @brief tolman and chalikov (1996) input and dissipation source terms.
!>
!> @details bundled with interpolation tables.
!>
!> @author h. l. tolman  @date 29-may-2009
!>
!/ ------------------------------------------------------------------- /
module w3src2md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
  !/    21-feb-2004 : multiple model version.             ( version 3.06 )
  !/    03-jul-2006 : extract stress computation.         ( version 3.09 )
  !/    13-apr-2007 : emean in w3spr2 par list.           ( version 3.11 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     tolman and chalikov (1996) input and dissipation source terms.
  !     bundled with interpolation tables.
  !
  !  2. variables and types :
  !
  !      interpolation tables :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nrsiga    i.p.  public   array dimension (siga).
  !      nrdrag    i.p.  public   array dimension (drag coefficient).
  !      sigamx    r.p.  public   maximum nondiensional frequency.
  !      dragmx    r.p.  public   maximum drag coefficient.
  !      dsiga     real  public   table increment.
  !      ddrag     real  public   id.
  !      betatb    r.a.  public   interpolation table.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3spr2    subr. public   mean parameters from spectrum.
  !      w3sin2    subr. public   input source term.
  !      w3sds2    subr. public   dissipation source term.
  !      inptab    subr. public   interpolation table for wind-wave
  !                               interaction parameter.
  !      w3beta    r.f.  inptab   id. function.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.            ( !/s )
  !      prt2ds    subr. w3arrymd print plot of spectra.        ( !/t0 )
  !      outmat    subr. w3wrrymd print out 2d matrix.          ( !/t1 )
  !      ...       data  w3dispmd interpolation tables to solve
  !                               dispersion relation.
  !     ----------------------------------------------------------------
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
  public
  !/
  !/ interpolation table
  !/
  integer, parameter, private :: nrsiga =  400
  integer, parameter, private :: nrdrag =   20
  real, parameter, private    :: sigamx =   40.
  real, parameter, private    :: dragmx =    1.e-2
  !
  real, private           :: dsiga, ddrag,                        &
       betatb(-nrsiga:nrsiga+1,nrdrag+1)
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate mean wave parameters for the use in the source term
  !>  routines (tolman and chalikov).
  !>
  !> @param[in]  a      action density spectrum.
  !> @param[in]  cg     group velocities.
  !> @param[in]  wn     wavenumbers.
  !> @param[in]  depth  water depth.
  !> @param[in]  fpi    peak input frequency.
  !> @param[in]  u      wind speed.
  !> @param[in]  ustar  friction velocity.
  !> @param[out] emean  total energy (variance).
  !> @param[out] fmean  mean frequency.
  !> @param[out] wnmean mean wavenumber.
  !> @param[out] amax   maximum of action spectrum.
  !> @param[out] alfa   phillips' constant.
  !> @param[out] fp     peak frequency.
  !>
  !> @author h. l. tolman
  !> @author d. chalikov
  !> @date   13-apr-2007
  !>
  subroutine w3spr2 (a, cg, wn, depth, fpi, u, ustar,             &
       emean, fmean, wnmean, amax, alfa, fp )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |            d.chalikov             |
    !/                  |                        fortran 90 |
    !/                  | last update :         13-apr-2007 |
    !/                  +-----------------------------------+
    !/
    !/    06-dec-1996 : final version 1.18 / fortran 77 version.
    !/    16-nov-1999 : add itteration to section 5. for removal of w3apr2.
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    21-dec-2004 : multiple model version.             ( version 3.06 )
    !/    03-jul-2006 : extract stress computation.         ( version 3.09 )
    !/    13-apr-2007 : emean in parameter list.            ( version 3.11 )
    !
    !  1. purpose :
    !
    !     calculate mean wave parameters for the use in the source term
    !     routines. (tolman and chalikov)
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum.
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       depth   real  i   water depth.
    !       fpi     real  i   peak input frequency.
    !       u       real  i   wind speed.
    !       ustar   real  i   friction velocity.
    !       emean   real  o   total energy (variance).
    !       fmean   real  o   mean frequency.
    !       wnmean  real  o   mean wavenumber.
    !       amax    real  o   maximum of action spectrum.
    !       alfa    r.a.  o   phillips' constant.
    !       fp      real  o   peak frequency.
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
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !       !/t      enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, dth, sig, dden, fte, ftf, ftwn,    &
         nittin, zwind, cinxsi
    use w3dispmd, only: nar1d, dfac, n1max, ecg1, ewn1, dsie
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk), cg(nk), wn(nk), depth,    &
         fpi, u, ustar
    real, intent(out)       :: emean, fmean, wnmean, amax,          &
         alfa(nk), fp
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, i1, itt
    real                    :: eband, fpistr, eb(nk), ust
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ust    = max ( 0.0001 , ustar )
    !
    emean  = 0.
    fmean  = 0.
    wnmean = 0.
    amax   = 0.
    !
    ! 1.  integral over directions and maximum --------------------------- *
    !
    do ik=1, nk
      eb(ik) = 0.
      do ith=1, nth
        eb(ik) = eb(ik) + a(ith,ik)
        amax   = max ( amax , a(ith,ik) )
      end do
    end do
    !
    ! 2.  integrate over directions -------------------------------------- *
    !
    do ik=1, nk
      alfa(ik) = 2. * dth * sig(ik) * eb(ik) * wn(ik)**3
      eb(ik)   = eb(ik) * dden(ik) / cg(ik)
      emean    = emean  + eb(ik)
      fmean    = fmean  + eb(ik) / sig(ik)
      wnmean   = wnmean + eb(ik) / sqrt(wn(ik))
    end do
    !
    ! 3.  add tail beyond discrete spectrum and get mean pars ------------ *
    !     ( dth * sig absorbed in ftxx )
    !
    eband  = eb(nk) / dden(nk)
    emean  = emean  + eband * fte
    fmean  = fmean  + eband * ftf
    wnmean = wnmean + eband * ftwn
    !
    fmean  = tpiinv * emean / max ( 1.e-7 , fmean )
    wnmean = ( emean / max ( 1.e-7 , wnmean ) )**2
    !
    ! 4.  estimate peak frequency from fpi ------------------------------- *
    !
    fpistr = max ( 0.008 , fpi * ust / grav )
    fp     = ( 3.6e-4 + 0.92*fpistr - 6.3e-10/fpistr**3 )/ust*grav
    fp     = fp * tpiinv
    !
    return
    !/
    !/ end of w3spr2 ----------------------------------------------------- /
    !/
  end subroutine w3spr2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate input source term.
  !>
  !> @param[in]  a    action density spectrum (1-d).
  !> @param[in]  cg   group velocities for k-axis of spectrum.
  !> @param[in]  k    wavenumber for entire spectrum (1-d).
  !> @param[in]  u    wind speed at reference height.
  !> @param[in]  udir direction of u.
  !> @param[in]  cd   drag coefficient at wind level zwind.
  !> @param[in]  z0   corresponding z0.
  !> @param[out] fpi  input 'peak' frequency.
  !> @param[out] s    source term (1-d version).
  !> @param[out] d    diagonal term of derivative (1-d version).
  !>
  !> @author h. l. tolman
  !> @author d. chalikov
  !> @date   21-feb-2004
  !>
  subroutine w3sin2 ( a, cg, k, u, udir, cd, z0, fpi, s, d )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |            d.chalikov             |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-feb-2004 |
    !/                  +-----------------------------------+
    !/
    !/    14-jan-1997 : final fortran 77                    ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    21-feb-2004 : multiple model version.             ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     calculate input source term.
    !
    !  2. method :
    !
    !     tolman and chalikov (1996), see manual.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       cg      r.a.  i   group velocities for k-axis of spectrum.
    !       k       r.a.  i   wavenumber for entire spectrum (1-d).
    !       u       real  i   wind speed at reference height.
    !       udir    real  i   direction of u.
    !       cd      real  i   drag coefficient at wind level zwind.
    !       z0      real  i   corresponding z0.
    !       fpi     r.a.  o   input 'peak' frequency.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
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
    !  7. remarks :
    !
    !     - actual height of wind speed does not need to be 10 m, but is
    !       given by zwind.
    !     - abs(cos) > 0.0087 to asure continuity in beta. corresponds
    !       to shift of up to half a degree.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !       !/t      enable general test output.
    !       !/t0     print arrays.
    !       !/t1     calculation of diagonal within spectrum
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, nspec, xfr, dden, sig, sig2,       &
         esin, ecos, fte, fttr, fpimin, zwind,       &
         facti1, facti2, fswell
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), cg(nk), k(nspec), u, udir, &
         cd, z0
    real, intent(out)       :: s(nspec), d(nspec), fpi
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, ik, ioma, icl, nkfilt, nkfil2
    real                    :: cosu, sinu, cosfac, lambda, ulam,    &
         clam, oma, m0, m1, rd1, rd2, beta,   &
         facln1, facln2, ustar, trans, fpistr,&
         fp1str, fp1, sin1a(nk)
    real, parameter         :: transf = 0.75
    real, parameter         :: peakfc = 0.8
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  preparations
    !
    cosu   = cos(udir)
    sinu   = sin(udir)
    !
    ! 2.  loop over spectrum
    !
    !
    facln1 = u / log(zwind/z0)
    facln2 = log(z0)
    !
    do is=1, nspec
      cosfac = ecos(is)*cosu + esin(is)*sinu
      cosfac = sign ( max ( 0.0087 , abs(cosfac) ) , cosfac )
      lambda = tpi / ( k(is) * abs(cosfac) )
      ulam   = facln1 * ( log(lambda) - facln2 )
      clam   = cd * ( u / ulam )**2
      oma    = k(is) * ulam * cosfac / sig2(is)
      ioma   = int ( oma/dsiga ) +                                  &
           min ( 0 , int ( sign ( -1.1 , oma ) ) )
      icl    = int ( clam/ddrag )
      rd1    = oma/dsiga - real(ioma)
      rd2    = clam/ddrag - real(icl)
      ioma   = max ( -nrsiga , min ( nrsiga , ioma ) )
      icl    = max ( 1 , min ( nrdrag , icl ) )
      beta   = (1.-rd1) * (1.-rd2) * betatb( ioma , icl )           &
           +    rd1   * (1.-rd2) * betatb(ioma+1, icl )           &
           + (1.-rd1) *    rd2   * betatb( ioma ,icl+1)           &
           +    rd1   *    rd2   * betatb(ioma+1,icl+1)
      d(is)  = beta * sig2(is)
      s(is)  = a(is) * d(is)
    end do
    !
    ! 3.  calculate fpi
    !
    do ik=1, nk
      sin1a(ik) = 0.
      do is=(ik-1)*nth+1, ik*nth
        sin1a(ik) = sin1a(ik) + max ( 0. , s(is) )
      end do
    end do
    !
    m0     = 0.
    m1     = 0.
    do ik=1, nk
      sin1a(ik) = sin1a(ik) * dden(ik) / ( cg(ik) * sig(ik)**3 )
      m0        = m0 + sin1a(ik)
      m1        = m1 + sin1a(ik)/sig(ik)
    end do
    !
    sin1a(nk) = sin1a(nk) / dden(nk)
    m0        = m0 + sin1a(nk) * fte
    m1        = m1 + sin1a(nk) * fttr
    if ( m1 .lt. 1e-20 ) then
      fpi    = xfr * sig(nk)
    else
      fpi    = m0 / m1
    end if
    !
    ! 4.  filter for swell
    !
    ustar  = u * sqrt(cd)
    fpistr = max ( fpimin , fpi * ustar / grav )
    fp1str = 3.6e-4 + 0.92*fpistr - 6.3e-10/fpistr**3
    fp1    = peakfc * fp1str * grav / ustar
    !
    nkfilt = min ( nk , int(facti2+facti1*log(fp1)) )
    nkfil2 = min ( nk , int(facti2+facti1*log(transf*fp1)) )
    nkfil2 = max ( 0 , nkfil2 )
    !
    do is=1, nkfil2*nth
      d(is)  = max ( d(is) , fswell*d(is) )
      s(is)  = a(is) * d(is)
    end do
    !
    do ik=nkfil2+1, nkfilt
      trans  = ( sig(ik)/fp1 - transf ) / (1.-transf)
      do is=(ik-1)*nth+1, ik*nth
        d(is)  = (1.-trans)*max(d(is),fswell*d(is)) + trans*d(is)
        s(is)  = a(is) * d(is)
      end do
    end do
    !
    ! ... test output of arrays
    !
    !
    !
    return
    !
    ! formats
    !
    !
    !/
    !/ end of w3sin2 ----------------------------------------------------- /
    !/
  end subroutine w3sin2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate whitecapping source term and diagonal term of derivative.
  !>
  !> @param[in]  a     input action density spectrum.
  !> @param[in]  cg    group velocity array.
  !> @param[in]  k     wavenumber array.
  !> @param[in]  fpi   'peak frequency' of input (rad/s).
  !> @param[in]  ustar friction velocity (m/s).
  !> @param[in]  alfa  phillips' constant.
  !> @param[out] s     source term (1-d version).
  !> @param[out] d     diagonal term of derivative (1-d version).
  !>
  !> @author h. l. tolman  @date 21-feb-2004
  !>
  subroutine w3sds2 (a, cg, k, fpi, ustar, alfa, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-feb-2004 |
    !/                  +-----------------------------------+
    !/
    !/    12-jun-1996 : final fortran 77                    ( version 1.18 )
    !/    04-feb-2000 : upgrade to fortran 90               ( version 2.00 )
    !/    23-apr-2002 : erick rogers' fix                   ( version 2.19 )
    !/    21-feb-2004 : multiple model version.             ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     calculate whitecapping source term and diagonal term of der.
    !
    !  2. method :
    !
    !     tolman and chalikov (1995).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   i   input action density spectrum.
    !       cg      r.a.   i   group velocity array.
    !       k       r.a.   i   wavenumber array.
    !       fpi     real   i   'peak frequency' of input (rad/s).
    !       ustar   real   i   friction velocity (m/s).
    !       alfa    r.a.   i   phillips' constant.
    !       s       r.a.   o   source term (1-d version).
    !       d       r.a.   o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
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
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s     enable subroutine tracing.
    !     !/t     enable general test output.
    !     !/t0    print arrays.
    !     !/t1    print filter and constituents.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, sig, dden, dth, fte, fpimin,       &
         facti1, facti2, xf1, xf2, xfh, sdsaln,      &
         cdsa0, cdsa1, cdsa2, cdsb0, cdsb1, cdsb2,   &
         cdsb3
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk), cg(nk), k(nk), fpi,       &
         ustar, alfa(nk)
    real, intent(out)       :: s(nth,nk), d(nth,nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, ikhw
    real                    :: fhw, xhw, fpit, phi, af1, af2,       &
         afilt, bfilt, cdist, filt, pow,      &
         cdish, cdisp, hw, ehigh, ebd(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  preparations
    ! 1.a hw
    !
    fhw    = xfh*fpi
    xhw    = facti2 + facti1*log(fhw)
    ikhw   = min ( nk , int ( xhw + 0.5 ) )
    do ik=ikhw, nk
      ebd(ik) = 0.
      do ith=1, nth
        ebd(ik) = ebd(ik) + a(ith,ik)
      end do
    end do
    !
    if ( fhw .lt. sig(nk+1) ) then
      xhw    = 1. - mod ( xhw + 0.5 , 1. )
      if ( ikhw .eq. nk ) xhw = max ( 0. , xhw - 0.5 )
      hw     = xhw * ebd(ikhw)*dden(ikhw)/cg(ikhw)
      do ik=ikhw+1, nk
        hw     = hw + ebd(ik)*dden(ik)/cg(ik)
      end do
      hw     = 4. * sqrt ( hw + ebd(nk)/cg(nk)*fte )
    else
      ehigh  = ebd(nk)/cg(nk) * sig(nk)*dth * (sig(nk)/fhw)**5
      hw     = 4. * sqrt ( 0.25 * fhw * ehigh )
    end if
    !
    ! 1.b phi
    !
    fpit   = max ( fpimin , fpi*tpiinv*ustar/grav )
    phi    = cdsb0 + cdsb1*fpit + cdsb2/fpit**cdsb3
    !
    ! 1.c set-up filter
    !
    af2    = xf2*fpi
    af1    = xf1*fpi
    bfilt  = 1. / ( af2 - af1 )
    afilt  = - bfilt * af1
    !
    ! 1.d constants
    !
    cdist = - 2. * ustar * hw * phi
    cdish = g2pi3i * ustar**2
    cdisp = g1pi1i * ustar
    !
    ! 2.  combined diagonal factor
    !
    do ik=1, nk
      filt    = min ( 1., max ( 0. , afilt + bfilt*sig(ik) ))
      pow     = min ( 25. , cdsa1 / ( cdisp*sig(ik) )**cdsa2 )
      if ( filt .gt. 0. ) then
        d(1,ik) = (1.-filt)  * cdist * k(ik)**2                   &
             - filt * cdsa0 * cdish * sig(ik)**3             &
             * (alfa(ik)/sdsaln)**pow
      else
        d(1,ik) = (1.-filt)  * cdist * k(ik)**2
      end if
    end do
    !
    !
    ! 3.  2-d diagonal array
    !
    do ik=1, nk
      do ith=2, nth
        d(ith,ik) = d(1,ik)
      end do
    end do
    !
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3sds2 ----------------------------------------------------- /
    !/
  end subroutine w3sds2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief generate an interpolation table for the air-sea interaction
  !>  parameter of chalikov and belevich (1993).
  !>
  !> @details the size of the table is set in parameter statements,
  !>  the range is set by the input parameters of this routine. the first
  !>  counter of the table corresponds to the nondimensional frequency
  !>
  !> @verbatim
  !>                  sigma ul
  !>        siga  =  ----------  cos ( theta - theta     )           (1)
  !>                     g                          wind
  !> @endverbatim
  !>
  !>  the second counter of the table represents the drag coefficient.
  !>  the maximum values of both parameters are passed to the routine
  !>  through the parameter list.
  !>
  !> @author h. l. tolman  @date 21-feb-2004
  !>
  subroutine inptab
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-feb-2004 |
    !/                  +-----------------------------------+
    !/
    !/    03-jun-1996 : final version 1.18 / fortran 77 version.
    !/    06-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    21-feb-2004 : multiple model version.             ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     generate an interpolation table for the air-sea interaction
    !     parameter of chalikov and belevich (1993).
    !
    !  2. method :
    !
    !     the size of the table is set in parameter statements, the range
    !     is set by the input parameters of this routine. the first counter
    !     of the table corresponds to the nondimensional frequency
    !
    !                  sigma ul
    !        siga  =  ----------  cos ( theta - theta     )           (1)
    !                     g                          wind
    !
    !     the second counter of the table represents the drag coefficient.
    !     the maximum values of both parameters are passed to the routine
    !     through the parameter list.
    !
    !  3. parameters :
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      w3beta    func. internal function to calculate the
    !                               interaction parameter.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd model definition io routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable test output.
    !     !/t0  print table.
    !     !/t1  estimate maximum errors.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3odatmd, only: ndst
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: isiga, idrag
    real                    :: siga, drag
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  determine range and increments of table ------------------------ *
    !
    dsiga  = sigamx / real(nrsiga)
    ddrag  = dragmx / real(nrdrag)
    !
    !
    ! 2.  fill table ----------------------------------------------------- *
    !
    do isiga=-nrsiga,nrsiga+1
      siga   = real(isiga) * dsiga
      do idrag=1, nrdrag+1
        drag   = real(idrag) * ddrag
        betatb(isiga,idrag) = w3beta ( siga, drag , ndst )
      end do
    end do
    !
    ! 3.  test output ---------------------------------------------------- *
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
    !/    internal function w3beta
    !/
  contains
    !/ ------------------------------------------------------------------- /
    !>
    !> @brief calculate wind-wave interaction parameter beta.
    !>
    !> @param   oma    non-dimensional apparent frequency.
    !> @param   cl     drag coefficient at height l.
    !> @param   ndst
    !> @returns w3beta wind-wave interaction parameter multiplied
    !>          by density ratio.
    !>
    !> @author  h. l. tolman
    !> @author  d. chalikov
    !> @date    21-feb-2004
    !>
    real function w3beta ( oma , cl , ndst )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |            d.chalikov             |
      !/                  |                        fortran 90 |
      !/                  | last update :         21-feb-2004 |
      !/                  +-----------------------------------+
      !/
      !/    06-dec-1996 : final version 1.18 / fortran 77 version.
      !/    06-dec-1999 : upgrade to fortran 90               ( version 2.00 )
      !/    21-feb-2004 : multiple model version.             ( version 3.06 )
      !/
      !  1. purpose :
      !
      !     calculate wind-wave interaction parameter beta.
      !
      !  2. method :
      !
      !     chalikov and belevich (1992), see also manual.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       w3beta  real  o   wind-wave interaction parameter multiplied
      !                         by density ratio.
      !       oma     real  i   non-dimensional apparent frequency.
      !
      !                         oma = omega | u | cos(theta-theta ) / g
      !                                        l                 w
      !
      !       cl      real  i   drag coefficient at height l
      !     ----------------------------------------------------------------
      !
      !  4. subroutines used :
      !
      !  5. called by :
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
      !     !/s   enable subroutine tracing.
      !     !/t0  enable test output.
      !
      ! 10. source code :
      !
      !/ ------------------------------------------------------------------- /
      implicit none
      !/
      !/ ------------------------------------------------------------------- /
      !/ parameter list
      !/
      integer, intent(in)     :: ndst
      real, intent(in)        :: oma, cl
      !/
      !/ ------------------------------------------------------------------- /
      !/ local parameters
      !/
      real                    :: om1, om2, a0, a1, a2, a3, a4, a5,    &
           a6, a7, a8, a9, a10
      !/
      !/ ------------------------------------------------------------------- /
      !/
      !
      !
      ! calculate omegas
      !
      om1    =  1.075 +  75.*cl
      om2    =  1.2   + 300.*cl
      !
      ! calculate factors a
      !
      a1     =  0.25  + 395.*cl
      a2     =  0.35  + 150.*cl
      a4     =  0.3   + 300.*cl
      a9     =  0.35  + 240.*cl
      a10    = -0.06  + 470.*cl
      !
      a5     =  a4 * om1
      a0     =  0.25 * a5**2 / a4
      a3     = (a0-a2-a1) / (a0+a4+a5)
      a6     =  a0 * (1.-a3)
      a7     = (a9*(om2-1)**2+a10) / (om2-om1)
      a8     =  a7 * om1
      !
      !
      ! calculate beta * 1.e4
      !
      if  ( oma .lt. -1. ) then
        w3beta = -a1 * oma**2 - a2
      else if (oma .lt. om1/2.) then
        w3beta =  a3 * oma * ( a4 * oma - a5 ) - a6
      else if (oma .lt. om1) then
        w3beta =       oma * ( a4 * oma - a5 )
      else if (oma .lt. om2) then
        w3beta = a7 * oma - a8
      else
        w3beta = a9 * (oma-1.)**2 + a10
      end if
      !
      ! beta * dwat / dair
      !
      w3beta = w3beta * 1.e-4
      !
      return
      !
      ! formats
      !
      !/
      !/ end of w3beta ----------------------------------------------------- /
      !/
    end function w3beta
    !/
    !/ end of inptab ----------------------------------------------------- /
    !/
  end subroutine inptab
  !/
  !/ end of module w3src2md -------------------------------------------- /
  !/
end module w3src2md
