!> @file
!> @brief nonlinear interaction based `smoother' for high frequencies.
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
!> @brief nonlinear interaction based `smoother' for high frequencies.
!>
!> @author h. l. tolman
!> @date   13-jul-2012
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3snlsmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch-iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         13-jul-2012 |
  !/                  +-----------------------------------+
  !/
  !/    04-aug-2008 : origination in research model.      ( version 3.13 )
  !/    27-sep-2010 : added to svn repository.            ( version 3.15 )
  !/    13-jul-2012 : moved from version 3.15 to 4.08.    ( version 4.08 )
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     nonlinear interaction based `smoother' for high frequencies.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nkd       i.p.  private  number of nondimensional depths in
  !                               storage array.
  !      kdmin     r.p.  private  minimum relative depth in table.
  !      kdmax     r.p.  private  maximum relative depth in table.
  !      sitmin    real  private  minimum nondimensional radian
  !                               frequency in table.
  !      xsit      real  private  corresponding incremet factor.
  !      abmax     r.p.  public   maximum value of a34, b3 and b4.
  !     ----------------------------------------------------------------
  !
  !     variables in w3gdatmd :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      cnlsa     real  public   a34 in quadruplet definition.
  !      cnlsc     real  public   c in snl definition.
  !      cnlsfm    real  public   maximum relative spectral change.
  !      cnlsc1/3  real  public   constant in frequency filter.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3snls    subr. public   nonlinear 'smoother' algorithm.
  !      expand    subr. w3snls   expand spectrum for indirect address.
  !      insnls    subr. public   initialization routine.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      wavnu1    subr. w3dispmd solve dispersion relation.
  !      wavnu2    subr. w3dispmd solve dispersion relation.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd program abort.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !     !/s    enable subroutine tracing.
  !     !/t    enable test output.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  integer, private, parameter :: nkd = 100
  real, private, parameter    :: kdmin = 0.25 ,  kdmax = 10.
  real, private               :: sitmin, xsit
  !
  real, parameter             :: abmax = 0.25
  !
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief high-frequeny filter based on the nonlinear interactions for
  !>  an uresolved quadruplet.
  !>
  !> @details compute interactions for a quadruplet that is not resolved
  !>  by the discrete spectral rsolution, and then reduces to a simple
  !>  five-point stencil. furthermore interactions are filtered by
  !>  frequency to allow for high-frequency impact only, and the
  !>  integration schem is embedded, and reduces to a filter technique
  !>  for large time steps or strong interactions.
  !>
  !> @param[in]  a      action spectrum a(ith,ik) as a function of
  !>                    direction (rad)  and wavenumber.
  !> @param[in]  cg     group velocities (dimension nk).
  !> @param[in]  wn     wavenumbers (dimension nk).
  !> @param[in]  depth  water depth in meters.
  !> @param[in]  uabs   wind speed (m/s).
  !> @param[in]  dt     numerical time step (s).
  !> @param[out] snl    nonlinear source term.
  !> @param[out] aa     averaged spectrum.
  !>
  !> @author h. l. tolman
  !> @date   04-aug-2008
  !>
  subroutine w3snls (  a, cg, wn, depth, uabs, dt, snl, aa )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-aug-2008 |
    !/                  +-----------------------------------+
    !/
    !/    04-aug-2008 : origination.                        ( version 3.13 )
    !/
    !  1. purpose :
    !
    !     high-frequeny filter based on the nonlinear interactions for
    !     an uresolved quadruplet.
    !
    !  2. method :
    !
    !     compute interactions for a quadruplet that is not resolved by
    !     the discrete spectral rsolution, and then reduces to a simple
    !     five-point stencil. furthermore interactions are filtered by
    !     frequency to allow for high-frequency impact only, and the
    !     integration schem is embedded, and reduces to a filter technique
    !     for large time steps or strong interactions.
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
    !       uabs    real  i   wind speed (m/s).
    !       dt      real  i   numerical time step (s).
    !       snl     r.a.  o   nonlinear source term.                (opt)
    !       aa      r.a.  o   averaged spectrum.                    (opt)
    !     ----------------------------------------------------------------
    !       note: a and aa may safely be same array/address.
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      wavnu1    subr. w3dispmd solve dispersion relation.
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output.
    !     !/t1   test output frequency filter.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nfr => nk, nth, sig, xfr, fachfa, dth,      &
         nthx, nfrx, nspl, nsph, snsst, cnlsc,       &
         cnlsfm, cnlsc1, cnlsc2, cnlsc3
    use w3odatmd, only: ndst, ndse
    !
    use w3dispmd, only: wavnu1
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)            :: a(nth,nfr), cg(nfr), wn(nfr),    &
         depth, uabs, dt
    real, intent(out), optional :: snl(nth,nfr), aa(nth,nfr)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ifr, ifrmin, ith, ifrmn2,            &
         ikd, jkd(0:nfr+2), ispx0, ispx
    real                    :: sigp, cp, cm, xl, xh, el, eh, denom, &
         sit, xsitln, mc, f3a,  f3b, f3c,     &
         f4a, f4b, f4c, f00, f31, f32, f41,   &
         f42, auxb, aux11, aux21, aux12,      &
         aux22, fc1, fc2, fc3, fc4
    real                    :: xsi(nfr+2), xwn(nfr+2), xcg(nfr+2),  &
         up(nspl:nsph), un(nspl:nsph),        &
         e1(0:nfr+2), filtfp(nfr+2),          &
         fprop(nfr+2), ds1(nspl:nsph),        &
         ds2(nspl:nsph), ds3(nspl:nsph),      &
         da1(nspl:nsph), da2(nspl:nsph),      &
         da3(nspl:nsph)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  initializations ------------------------------------------------ *
    ! 1.a expanded frequency range
    !
    xsi(1:nfr) = sig(1:nfr)
    xwn(1:nfr) = wn
    xcg(1:nfr) = cg
    !
    xsi(nfr+1) = xsi(nfr) * xfr
    call wavnu1 ( xsi(nfr+1), depth, xwn(nfr+1), xcg(nfr+1) )
    xsi(nfr+2) = xsi(nfr+1) * xfr
    call wavnu1 ( xsi(nfr+2), depth, xwn(nfr+2), xcg(nfr+2) )
    !
    ! 1.b expanded psuedo spectrum
    !
    call expand ( up, un )
    !
    ! 1.c get relevant spectral peak frequency
    !
    sigp   = - tpi
    xl     = 1./xfr - 1.
    xh     =  xfr - 1.
    !
    ! 1.c.1 wind too weak
    !
    if ( uabs .lt. xsi(nfr)/xwn(nfr) ) then
      sigp   = grav / max ( 0.01 , uabs )
    else
      !
      ! 1.c.2 compute 1d spectrum
      !
      e1(nfr+2) = sum(a(:,nfr)) * fachfa**2 * xsi(nfr+2)          &
           / xcg(nfr+2) * tpi * dth
      e1(nfr+1) = sum(a(:,nfr)) * fachfa    * xsi(nfr+1)          &
           / xcg(nfr+1) * tpi * dth
      !
      do ifr=nfr, 1, -1
        e1(ifr) = sum(a(:,ifr)) * xsi(ifr) / xcg(ifr) * tpi * dth
        !
        ! 1.c.3 reached pm frequency
        !
        if ( uabs .lt. xsi(ifr)/xwn(ifr) ) then
          cp     = xsi(ifr)/xwn(ifr)
          cm     = xsi(ifr+1)/xwn(ifr+1)
          sigp   = xsi( ifr ) * (uabs-cm)/(cp-cm) +             &
               xsi(ifr+1) * (cp-uabs)/(cp-cm)
          exit
          !
        else if ( e1(ifr) .lt. e1(ifr+1) ) then
          !
          ! 1.c.4 reached first peak
          !
          el     = e1(ifr  ) - e1(ifr+1)
          eh     = e1(ifr+2) - e1(ifr+1)
          denom  = xl*eh - xh*el
          sigp   = xsi(ifr+1) * (1.+0.5*(xl**2*eh-xh**2*el)     &
               / sign ( max(abs(denom),1.e-15) , denom ) )
          exit
        endif
        !
        ! ... end loop 1.c.2
        !
      end do
      !
      ! 1.c.5 nothing found
      !
      if ( sigp .lt. 0. ) then
        !
        ! 1.c.5.a no energy there
        !
        if ( e1(1) .eq. 0. ) then
          sigp   = 2. * sig(nfr)
          !
          ! 1.c.5.b peak at low boundary
          !
        else
          sigp   = xsi(1)
        end if
      end if
      !
    end if
    !
    ! 1.d set up filter function etc.
    !
    xsitln = log(xsit)
    ifrmin =  1
    jkd    =  1
    !
    do ifr=nfr+2, 1, -1
      !
      filtfp(ifr) = exp(-cnlsc1/(xsi(ifr)/(cnlsc2*sigp))**cnlsc3)
      fprop (ifr) = filtfp(ifr) * cnlsc * xwn(ifr)**8 *             &
           xsi(ifr)**4 / tpi**9 / xcg(ifr)
      sit      = xsi(ifr) * sqrt(depth/grav)
      ikd      = 1 + nint ( ( log(sit) - log(sitmin) ) / xsitln )
      jkd(ifr) = max ( 1 , min(ikd,nkd) )
      !
      if ( filtfp(ifr) .lt. 1.e-10 ) then
        ifrmin = ifr
        exit
      end if
      !
    end do
    !
    ifrmn2 = max ( 1 , ifrmin - 1 )
    sit    = xsi(ifrmn2) * sqrt(depth/grav)
    ikd    = 1 + nint ( ( log(sit) - log(sitmin) ) / xsitln )
    jkd(ifrmn2) = max ( 1 , min(ikd,nkd) )
    !
    !
    ! 1.e initialize arrays
    !
    !
    ! 2.  compute base interactions -------------------------------------- *
    ! 2.a loop over frequencies
    !
    do ifr=ifrmin, nfr+1
      !
      ispx0  = (ifr-1)*nthx
      ikd    = jkd(ifr)
      !
      mc     = snsst( 1,ikd)
      f3a    = snsst( 2,ikd)
      f3b    = snsst( 3,ikd)
      f3c    = snsst( 4,ikd)
      f4a    = snsst( 5,ikd)
      f4b    = snsst( 6,ikd)
      f4c    = f3c
      !
      ! 2.b loop over directions
      !
      do ith=1, nth
        !
        ispx   = ispx0 + ith
        !
        f00    = up(ispx)
        f31    = up(ispx)*f3a + up(ispx+1)*f3b + up(ispx+nthx)*f3c
        f41    = up(ispx)*f4a + up(ispx-1)*f4b + up(ispx-nthx)*f4c
        f32    = up(ispx)*f3a + up(ispx-1)*f3b + up(ispx+nthx)*f3c
        f42    = up(ispx)*f4a + up(ispx+1)*f4b + up(ispx-nthx)*f4c
        !
        ds1(ispx) = fprop(ifr) * (f00**2*(f31+f41)-2.*f00*f31*f41)
        ds2(ispx) = fprop(ifr) * (f00**2*(f32+f42)-2.*f00*f32*f42)
        !
        aux11  = dt * ds1(ispx)
        aux21  = dt * ds2(ispx)
        auxb   = cnlsfm * filtfp(ifr) * max(1.e-10,un(ispx)) /   &
             max ( 1.e-10 , abs(aux11)+abs(aux21) ) / mc
        aux12  = auxb * abs(aux11)
        aux22  = auxb * abs(aux21)
        !
        ! expensive but more smooth limiter
        !
        !         da1(ispx) = aux12 * tanh(aux11/max(1.e-10,aux12))
        !         da2(ispx) = aux22 * tanh(aux21/max(1.e-10,aux22))
        !
        ! crude but cheaper limiter
        !
        da1(ispx) = max ( -aux12 , min ( aux11 , aux12 ) )
        da2(ispx) = max ( -aux22 , min ( aux21 , aux22 ) )
        !
      end do
      !
      ! ... end loop 2.b
      !
    end do
    !
    ! 2.c complete expanded arrays
    !
    ! ... end loop 2.a
    !
    ! 3.  compute source term if requested ------------------------------- *
    ! 3.a check for request
    !
    if ( present(snl) ) then
      !
      ! 3.b initializations
      !
      snl(:,1:ifrmn2-1) = 0.
      !
      ds1(nspl:ifrmn2*nthx-1) = 0.
      ds2(nspl:ifrmn2*nthx-1) = 0.
      ds3(nspl:ifrmn2*nthx-1) = 0.
      !
      ispx  = ifrmn2*nthx
      ds1(ispx+nth+1:nsph:nthx) = ds1(ispx+ 1 :nsph:nthx)
      ds1(ispx      :nsph:nthx) = ds1(ispx+nth:nsph:nthx)
      ds2(ispx+nth+1:nsph:nthx) = ds2(ispx+ 1 :nsph:nthx)
      ds2(ispx      :nsph:nthx) = ds2(ispx+nth:nsph:nthx)
      ds3(ifrmn2*nthx:nsph)     = ds1(ifrmn2*nthx:nsph)  +        &
           ds2(ifrmn2*nthx:nsph)
      !
      ! 3.c loop over frequencies
      !
      do ifr=ifrmn2, nfr
        !
        ispx0  = (ifr-1)*nthx
        ikd    = jkd(ifr)
        !
        fc1    = - snsst(1,ikd)
        fc2    =   snsst(4,ikd)
        fc3    =   snsst(3,ikd)
        fc4    =   snsst(6,ikd)
        !
        ! 3.d loop over directions
        !
        do ith=1, nth
          ispx         = ispx0 + ith
          snl(ith,ifr) = fc1 *             ds3(   ispx  )         &
               +    fc2 * ( ds3(ispx-nthx) + ds3(ispx+nthx) )       &
               +    fc3 * ( ds1(ispx-  1 ) + ds2(ispx+  1 ) )       &
               +    fc4 * ( ds1(ispx+  1 ) + ds2(ispx-  1 ) )
          !
        end do
        !
        ! ... end loop 3.d
        !
      end do
      !
      ! ... end loop 3.c
      !
    end if
    !
    ! 4.  compute filtered spectrum if requested ------------------------- *
    ! 4.a check for request
    !
    if ( present(aa) ) then
      !
      ! 4.b initializations
      !
      aa(:,1:ifrmn2-1) = a(:,1:ifrmn2-1)
      !
      da1(nspl:ifrmn2*nthx-1) = 0.
      da2(nspl:ifrmn2*nthx-1) = 0.
      da3(nspl:ifrmn2*nthx-1) = 0.
      !
      ispx  = ifrmn2*nthx
      da1(ispx+nth+1:nsph:nthx) = da1(ispx+ 1 :nsph:nthx)
      da1(ispx      :nsph:nthx) = da1(ispx+nth:nsph:nthx)
      da2(ispx+nth+1:nsph:nthx) = da2(ispx+ 1 :nsph:nthx)
      da2(ispx      :nsph:nthx) = da2(ispx+nth:nsph:nthx)
      da3(ifrmn2*nthx:nsph)     = da1(ifrmn2*nthx:nsph)  +        &
           da2(ifrmn2*nthx:nsph)
      !
      ! 4.c loop over frequencies
      !
      do ifr=ifrmn2, nfr
        !
        ispx0  = (ifr-1)*nthx
        ikd    = jkd(ifr)
        !
        fc1    = - snsst(1,ikd)
        fc2    =   snsst(4,ikd)
        fc3    =   snsst(3,ikd)
        fc4    =   snsst(6,ikd)
        !
        ! 4.d loop over directions
        !
        do ith=1, nth
          ispx         = ispx0 + ith
          aa(ith,ifr) = max ( 0. , a(ith,ifr) +                   &
               fc1 *   da3(ispx)                                  &
               + fc2 * ( da3(ispx-nthx) + da3(ispx+nthx) )          &
               + fc3 * ( da1(ispx-  1 ) + da2(ispx+  1 ) )          &
               + fc4 * ( da1(ispx+  1 ) + da2(ispx-  1 ) ) )
        end do
        !
        ! ... end loop 4.d
        !
      end do
      !
      ! ... end loop 4.c
      !
    end if
    !
    return
    !
    ! formats
    !
    !/
    !/ embedded subroutines
    !/
  contains
    !/ ------------------------------------------------------------------- /
    !>
    !> @brief expand spectrum to simplify indirect addressing.
    !>
    !> @param[out] pspc  expanded spectrum.
    !> @param[out] spec  expanded spectrum.
    !>
    !> @author h. l. tolman
    !> @date   23-jul-2008
    !>
    subroutine expand ( pspc, spec )
      !/
      !/                  +-----------------------------------+
      !/                  | wavewatch-iii           noaa/ncep |
      !/                  |           h. l. tolman            |
      !/                  |                        fortran 90 |
      !/                  | last update :         23-jul-2008 |
      !/                  +-----------------------------------+
      !/
      !  1. purpose :
      !
      !     expand spectrum to simplify indirect addressing.
      !
      !  3. parameters :
      !
      !     parameter list
      !     ----------------------------------------------------------------
      !       pspc    r.a.  o   expanded spectrum.
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
      real, intent(out)       :: pspc(0:nth+1,0:nfr+2),               &
           spec(0:nth+1,0:nfr+2)
      !/
      !/ local parameters
      !/
      integer                 :: ifr, ith
      !/
      !/ ------------------------------------------------------------------- /
      !
      spec(:,0) = 0.
      !
      spec(1:nth,1:nfr) = a
      spec(1:nth,nfr+1) = spec(1:nth,nfr) * fachfa
      spec(1:nth,nfr+2) = spec(1:nth,nfr+1) * fachfa
      !
      spec(nth+1,1:nfr+2) = spec( 1 ,1:nfr+2)
      spec(  0  ,1:nfr+2) = spec(nth,1:nfr+2)
      !
      do ifr=1, nfr+2
        pspc(:,ifr) = spec(:,ifr) / xwn(ifr)
      end do
      !
      return
      !/
      !/ end of expand ----------------------------------------------------- /
      !/
    end subroutine expand
    !/
    !/ end of w3snls ----------------------------------------------------- /
    !/
  end subroutine w3snls
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initializations for the snl / filter source term for high
  !>  frequencies.
  !>
  !> @details precompute weight functions and store in array.
  !>
  !> @author h. l. tolman
  !> @date   04-aug-2008
  !>
  subroutine insnls
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch-iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-aug-2008 |
    !/                  +-----------------------------------+
    !/
    !/    04-aug-2008 : origination.                        ( version 3.13 )
    !/
    !  1. purpose :
    !
    !     initializations for the snl / filter source term for high
    !     frequencies.
    !
    !  2. method :
    !
    !     precompute weight functions and store in array.
    !
    !  3. parameters :
    !
    !     no parameter list.
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      wavnu2    subr. w3dispmd solve dispersion relation.
    !      strace    subr. w3servmd subroutine tracing.
    !      extcde    subr. w3servmd program abort.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd process model definition file.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check a34, b4 and b5 against maxab to assure that the values
    !       are consistent with a reduced 5-point stencil for unresolved
    !       quadruplets. a34 is checked in ww3_grid, b3 and b4 are not.
    !
    !  7. remarks :
    !
    !     - small quadruplet compared to grid size reduces interactions
    !       so that distribution of results is purely local. this results
    !       in a much simpler model initialization than for the general
    !       mdia.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3odatmd, only: ndst, ndse
    use w3gdatmd,       nfr => nk, a34 => cnlsa
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
    integer                 :: ikd, ierr
    real                    :: depth, sitmax, off, s0, wn0, cg0,    &
         s3, wn3, cg3, s4, wn4, cg4, wn12,    &
         dt3, dt4, b3, b4
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  initializations ------------------------------------------------ *
    ! 1.a set up relative depths
    !
    depth  = 1.
    sitmin = sqrt ( kdmin * tanh(kdmin) )
    sitmax = sqrt ( kdmax * tanh(kdmax) )
    xsit   = (sitmax/sitmin)**(1./real(nkd-1))
    !
    !
    ! 1.b set up quadruplet
    !
    off    = (xfr-1.) * a34
    !
    ! 1.c set up storage
    !
    nthx   = nth + 2
    nfrx   = nfr + 2
    nspl   = - nthx
    nsph   = nfrx*nthx - 1
    !
    allocate ( mpars(igrid)%snlps%snsst(6,nkd) )
    snsst  => mpars(igrid)%snlps%snsst
    !
    ! 2.  building quadruplet data base ---------------------------------- *
    !     for quadruplet and interaction strength evaluation
    !
    s0     = sitmin * sqrt ( grav / depth )  / xsit
    !
    ! 2.a loop over relative depths
    !
    do ikd=1, nkd
      !
      ! 2.b base quadruplet set up
      !
      s0     = s0 * xsit
      s3     = ( 1. + off ) * s0
      s4     = ( 1. - off ) * s0
      !
      call wavnu2 ( s0, depth, wn0, cg0, 1.e-6, 25, ierr)
      call wavnu2 ( s3, depth, wn3, cg3, 1.e-6, 25, ierr)
      call wavnu2 ( s4, depth, wn4, cg4, 1.e-6, 25, ierr)
      !
      !
      ! 2.c offset angles
      !
      wn12   = 2. * wn0
      dt3    = acos( (wn3**2+wn12**2-wn4**2) / (2.*wn12*wn3) )
      dt4    = acos( (wn4**2+wn12**2-wn3**2) / (2.*wn12*wn4) )
      !
      b3     = dt3 / dth
      b4     = dt4 / dth
      !
      !
      if ( a34.gt.abmax .or. b3.gt.abmax .or. b4.gt.abmax .or.     &
           a34.lt.0. .or. b3.lt.0. .or. b4.lt.0. ) goto 801
      !
      ! 2.d store weights
      !
      snsst( 1,ikd) = 2.*a34 + b3 + b4
      snsst( 2,ikd) = 1. - a34 - b3
      snsst( 3,ikd) = b3
      snsst( 4,ikd) = a34
      snsst( 5,ikd) = 1. - a34 - b4
      snsst( 6,ikd) = b4
      !
      ! ... end loop 2.a
      !
    end do
    !
    return
    !
    ! error escape locations
    !
801 continue
    write (ndse,1001) a34, b3, b4
    call extcde (1001)
    !
    ! formats
    !
1001 format (/' *** wavewatch-iii error in insnls :'/                &
         '     parameter forced out of range '/                 &
         '     a34, b3, b4 :', 3f10.4/)
    !
    !/
    ! /end of insnls ------------------------------------------------------/
    !/
  end subroutine insnls
  !/
  !/ end of module w3snlsmd -------------------------------------------- /
  !/
end module w3snlsmd
