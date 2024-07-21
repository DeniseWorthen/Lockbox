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
module w3sln1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    23-jun-2006 : origination.                        ( version 3.09 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     linear wind input according to cavaleri and melanotte-rizzoli
  !     (1982) filtered for low frequencies according to tolman (1992).
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sln1    subr. public   user supplied linear input.
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
  !     !/s  enable subroutine tracing.
  !     !/t  test output.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3sln1 (k, fhigh, ustar, usdir, s)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    23-jun-2006 : origination.                        ( version 3.09 )
    !/
    !  1. purpose :
    !
    !     linear wind input according to cavaleri and melanotte-rizzoli
    !     (1982) filtered for low frequencies according to tolman (1992).
    !
    !  2. method :
    !
    !     the expression of cavaleri and melanotte-rizzoli, converted to
    !     action spectra defined in terms of wavenumber and direction
    !     becomes
    !
    !                       -1       /     /                \ \ 4
    !       sln  = slnc1 * k   * max | 0., | u* cos(dtheta) | |        (1)
    !                                \     \                / /
    !
    !                             2     -2
    !              slnc1 = 80 rhor  grav   filt                        (2)
    !
    !     where :
    !
    !        rhor     density of air dev. by density of water.
    !        u*       wind friction velocity.
    !        dtheta   difference in wind and wave direction.
    !        filt     filter based on pm and cut-off frequencies.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       k       r.a.  i   wavenumber for entire spectrum.
    !       fhigh   r.a.  i   cut-off frequency in integration (rad/s)
    !       ustar   real  i   friction velocity.
    !       usdir   real  i   direction of ustar.
    !       s       r.a.  o   source term.
    !     ----------------------------------------------------------------
    !                         *) stored as 1-d array with dimension nth*nk
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
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nth, nk, ecos, esin, sig, slnc1, fspm, fshf
    use w3odatmd, only: ndse, ndst
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: k(nk), fhigh, ustar, usdir
    real, intent(out)       :: s(nth,nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik
    real                    :: cosu, sinu, dirf(nth), fac, ff1, ff2, &
         ffilt, rfr, wnf(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  set up factors ------------------------------------------------- *
    !
    !
    cosu   = cos(usdir)
    sinu   = sin(usdir)
    !
    do ith=1, nth
      dirf(ith) = max ( 0. , (ecos(ith)*cosu+esin(ith)*sinu) )**4
    end do
    !
    fac    = slnc1 * ustar**4
    ff1    = fspm * grav/(28.*ustar)
    ff2    = fshf * min(sig(nk),fhigh)
    ffilt  = min ( max(ff1,ff2) , 2.*sig(nk) )
    do ik=1, nk
      rfr    = sig(ik) / ffilt
      if ( rfr .lt. 0.5 ) then
        wnf(ik) = 0.
      else
        wnf(ik) = fac / k(ik) * exp(-rfr**(-4))
      end if
    end do
    !
    ! 2.  compose source term -------------------------------------------- *
    !
    do ik=1, nk
      s(:,ik) = wnf(ik) * dirf(:)
    end do
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3sln1 ----------------------------------------------------- /
    !/
  end subroutine w3sln1
  !/
  !/ end of module insln1md -------------------------------------------- /
  !/
end module w3sln1md
