!> @file
!> @brief contains module w3src0md.
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
!> @brief mean wave parameter computation for case without input and
!>  dissipation.
!>
!> @author h. l. tolman  @date 29-may-2009
!>
!/ ------------------------------------------------------------------- /
module w3src0md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    05-jul-2006 : origination.                        ( version 3.09 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     mean wave parameter computation for case without input and
  !     dissipation.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3spr0    subr. public   mean parameters from spectrum.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.            ( !/s )
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !       !/s      enable subroutine tracing.
  !       !/t      test output, see subroutines.
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
  !> @brief calculate mean wave parameters.
  !>
  !> @param[in]  a      action as a function of direction and wavenumber.
  !> @param[in]  cg     group velocities.
  !> @param[in]  wn     wavenumbers.
  !> @param[out] emean  mean wave energy.
  !> @param[out] fmean  mean wave frequency.
  !> @param[out] wnmean mean wavenumber.
  !> @param[out] amax   maximum action density in spectrum.
  !>
  !> @author h. l. tolman  @date 05-jul-2006
  !>
  subroutine w3spr0 (a, cg, wn, emean, fmean, wnmean, amax)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         05-jul-2006 |
    !/                  +-----------------------------------+
    !/
    !/    05-jul-2006 : origination.                        ( version 3.09 )
    !/
    !  1. purpose :
    !
    !     calculate mean wave parameters.
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action as a function of direction and
    !                         wavenumber.
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       emean   real  o   mean wave energy.
    !       fmean   real  o   mean wave frequency.
    !       wnmean  real  o   mean wavenumber.
    !       amax    real  o   maximum action density in spectrum.
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
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nk, nth, sig, dden, fte, ftf, ftwn
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk), cg(nk), wn(nk)
    real, intent(out)       :: emean, fmean, wnmean, amax
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith
    real                    :: eb(nk), eband
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    emean  = 0.
    fmean  = 0.
    wnmean = 0.
    amax   = 0.
    !
    ! 1.  integral over directions
    !
    do ik=1, nk
      eb(ik) = 0.
      do ith=1, nth
        eb(ik) = eb(ik) + a(ith,ik)
        amax   = max ( amax , a(ith,ik) )
      end do
    end do
    !
    ! 2.  integrate over directions
    !
    do ik=1, nk
      eb(ik) = eb(ik) * dden(ik) / cg(ik)
      emean  = emean  + eb(ik)
      fmean  = fmean  + eb(ik) / sig(ik)
      wnmean = wnmean + eb(ik) / sqrt(wn(ik))
    end do
    !
    ! 3.  add tail beyond discrete spectrum
    !     ( dth * sig absorbed in ftxx )
    !
    eband  = eb(nk) / dden(nk)
    emean  = emean  + eband * fte
    fmean  = fmean  + eband * ftf
    wnmean = wnmean + eband * ftwn
    !
    ! 4.  final processing
    !
    fmean  = tpiinv * emean / max ( 1.e-7 , fmean )
    wnmean = ( emean / max ( 1.e-7 , wnmean ) )**2
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3spr0 ----------------------------------------------------- /
    !/
  end subroutine w3spr0
  !/
  !/ end of module w3src0md -------------------------------------------- /
  !/
end module w3src0md
