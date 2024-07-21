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
module w3sdb1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           j. h. alves             |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    25-apr-2007 : origination of module.              ( version 3.11 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     dummy slot for bottom friction source term.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sdb1    subr. public   battjes and janssen depth-induced
  !                               breaking.
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
  !     see subroutine documentation.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3sdb1 (ix, a, depth, emean, fmean, wnmean, cg, lbreak, s, d )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                        fortran 90 |
    !/                  |           j. h. alves             |
    !/                  |           h. l. tolman            |
    !/                  !           a. roland               |
    !/                  | last update :         08-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    25-apr-2007 : origination of module.              ( version 3.11 )
    !/    08-jun-2018 : add debugdb1.                       ( version 6.04 )
    !/    03-apr-2019 : rewrite in terms of energy density (a. roland,version 6.07)
    !/    03-apr-2019 : add thornton & guza, 1983          (a. roland,version 6.07)
    !/
    !  1. purpose :
    !
    !     compute depth-induced breaking using battjes and janssen bore
    !     model approach
    !
    !  2. method : battjes & janssen (1978),
    !
    !           sbr   = dtot/etot*wa = d * wa
    !           dtot  = 0.25*alpha*qb*fm*hmax²
    !           fm    = sigma/2pi
    !           bb    = hrms²/hmax² = 8etot/hmax²
    !           d     = dtot/etot = bjalfa * sigma / pi * qb/bb = 2 * bjalfa * fm * qb/bb
    !
    !           ar: only valid for hrms .le. hm, qb .le. 1, otherwise, in the degenrative regime it is
    !           due to qb > 1 that all wave are broken and hrms .le. hmax
    !           mlim can be used to enforce this conditions, source term will smoothly converge to this limit.
    !
    !     where cdb   = sdbc1 = bjalfa (defaults to bjalfa = 1)
    !                   modified via ww3_grid namelist parameter bjalfa
    !           hm    = gamma * dep
    !           gamma = sdbc2 defaults to 0.73 (mean battjes/janssen value)
    !                   modified via ww3_grid namelist parameter bjgam
    !
    !     and qb is estimated by iterations using the nonlinear expression
    !
    !           1 - qb = hrms**2
    !           ------   -------
    !            ln qb    hm**2
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d)
    !       emean   real  i   mean wave energy.
    !       fmean   real  i   mean wave frequency.
    !       wnmean  real  i   mean wave number.
    !       depth   real  i   mean water depth.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       strace   subroutine tracing (!/s switch).
    !
    !  5. called by :
    !
    !       w3srce   source term integration.
    !       w3expo   point output post-processor.
    !       gxexpo   grads point output post-processor.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     - note that the miche criterion con influence wave growth.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/tn  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants
    use w3gdatmd, only: nk, nth, nspec, sdbc1, sdbc2, fdonly, fssource, dden
    use w3odatmd, only: ndst
    use w3gdatmd, only: sig
    use w3odatmd, only : iaproc
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ix ! local grid number
    real, intent(in)        :: a(nspec)
    real, intent(inout)     :: emean, fmean, wnmean, depth
    real, intent(out)       :: s(nspec), d(nspec)
    real, intent(in)        :: cg(nk)
    logical, intent(out)    :: lbreak
    integer                 :: ith, ik, iwb
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is
    real*8                    :: hm, bb, arg, q0, qb, b, cbj, hrms, eb(nk)
    real*8                    :: aux, cbj2, ratio, s0, s1, thr, br1, br2, fak
    real                      :: etot, fmean2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initialzations ------------------------------------------------- /
    !     never touch this 4 lines below ... otherwise my exceptionhandling will not work.
    thr = dble(1.e-15)
    if (sum(a) .lt. thr) return
    s = 0.
    d = 0.
    iwb = 1
    !
    !
    ! 1.  integral quantities. ar: make sure mean quantities are computed, need to move upward
    !
    etot = 0.
    fmean2 = 0.
    do ik=1, nk
      eb(ik) = 0.
      do ith=1, nth
        eb(ik) = eb(ik) + a(ith+(ik-1)*nth)
      end do
    end do
    do ik=1, nk
      eb(ik) = eb(ik) * dden(ik) / cg(ik)
      etot  = etot  + eb(ik)
    end do
    do ik=1, nk
      fmean2 = fmean2 + eb(ik) * sig(ik)
    end do
    fmean2 = fmean2 / etot * tpiinv
    !
    ! 2do compute wlmean
    !
    ! 1.a. maximum wave height
    ! 1.a.1. simple limit
    !
    if ( fdonly ) then
      hm     = dble(sdbc2) * dble(depth)
    else
      !
      ! 1.a.2. miche style criterion
      !
      hm     = dble(sdbc2) / dble(wnmean) * tanh ( dble(wnmean) * max(depth,0.) )
    end if
    !
    !ar: add dingemans ...
    ! 1.b. hrms and ratio hrms / hmax
    !
    hrms = dsqrt (8.d0 * dble(emean))
    if ( hm .gt. thr) then
      bb     = hrms * hrms / ( hm * hm )
      b      = dsqrt(bb)
    else
      bb     = 0.d0
      b      = 0.d0
    end if
    !
    ! 2. fraction of breaking waves -------------------------------------- /
    ! 2.a. first guess breaking fraction
    !
    if ( b .le. 0.5d0 ) then
      q0     = 0.d0
    else if ( b .le. 1.d0 ) then
      q0     = ( 2.d0 * b - 1.d0 ) ** 2
    end if
    !
    ! 2.b. iterate to obtain actual breaking fraction
    !
    if ( b .le. 0.2d0 ) then
      qb     = 0.d0
    else if ( b .lt. 1.d0 ) then
      arg    = exp  (( q0 - 1.d0 ) / bb )
      qb     = q0 - bb * ( q0 - arg ) / ( bb - arg )
      do is=1, 3
        qb     = exp((qb-1.)/bb)
      end do
    else
      qb = 1.0 - thr
    end if
    !
    ! 3. estimate the breaking coefficient ------------------------------- /
    !
    cbj  = 0
    if (iwb == 1) then
      if ( ( bb .gt. thr) .and. ( abs ( bb - qb ) .gt. thr) ) then
        if ( bb .lt. 1.0) then
          cbj = 2 * dble(sdbc1) * qb * dble(fmean) / bb
        else
          cbj = 2 * dble(sdbc1) * dble(fmean) * bb ! ar: degenerative regime, all waves must be .le. hmax, we just smoothly let the excessive energy vanish by * bb.
        end if
      else
        cbj = 0.d0
      endif
      d = - cbj
      s = d * a
    else if (iwb == 2) then
      if (etot .gt. thr) then
        hrms = sqrt(8*emean)
        fak  = (1+4./sqrt(pi)*(b*bb+1.5*b)*exp(-bb)-erf(b))
        cbj  = -sdbc1*sqrt(pi)/16.*fmean*hrms**3/depth/etot
      else
        cbj  = 0.
      endif
      d = - cbj
      s = d * a
    endif
    if (cbj .gt. 0.) then
      lbreak = .true.
    else
      lbreak = .false.
    endif
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
    !/ end of w3sdb1 ----------------------------------------------------- /
    !/
  end subroutine w3sdb1
  !/
  !/
  !/ end of module w3sdb1md -------------------------------------------- /
  !/
end module w3sdb1md
