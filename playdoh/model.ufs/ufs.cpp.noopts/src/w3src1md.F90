!> @file
!> @brief contains module w3src1md.
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
!> @brief bundle wam cycle 3 input and dissipation source terms with
!>  their defining parameters.
!>
!> @author  h. l. tolman  @date 29-may-2009
!>
!/ ------------------------------------------------------------------- /
module w3src1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    06-dec-1996 : final fortran 77                    ( version 1.18 )
  !/    06-dec-1999 : upgrade to fortran 90               ( version 2.00 )
  !/    23-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     bundle wam cycle 3 input and dissipation source terms with
  !     their defining parameters.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3spr1    subr. public   mean parameters from spectrum.
  !      w3sin1    subr. public   input source term.
  !      w3sds1    subr. public   dissipation source term.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.            ( !/s )
  !      prt2ds    subr. w3arrymd print plot of spectra.        ( !/t0 )
  !      outmat    subr. w3wrrymd print out 2d matrix.          ( !/t1 )
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
  !/
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief
  !>
  !> @param[in]  a      action as a function of direction and wavenumber.
  !> @param[in]  cg     group velocities.
  !> @param[in]  wn     wavenumber.
  !> @param[out] emean  mean wave energy.
  !> @param[out] fmean  mean wave frequency.
  !> @param[out] wnmean mean wavenumber.
  !> @param[out] amax   maximum action density in spectrum.
  !>
  !> @author h. l. tolman  @date 23-dec-2004
  !>
  subroutine w3spr1 (a, cg, wn, emean, fmean, wnmean, amax)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    06-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    06-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    23-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     calculate mean wave parameters for the use in the source term
    !     routines. (wam-3)
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
    !/ end of w3spr1 ----------------------------------------------------- /
    !/
  end subroutine w3spr1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate diagonal of input source (actual source term put
  !>  together in w3srce).
  !>
  !> @param[in]  a     action density spectrum (1-d).
  !> @param[in]  k     wavenumber for entire spectrum.
  !> @param[in]  ustar friction velocity.
  !> @param[in]  usdir direction of ustar.
  !> @param[out] s     source term (1-d version).
  !> @param[out] d     diagonal term of derivative.
  !>
  !> @author h. l. tolman  @date 23-dec-2004
  !>
  subroutine w3sin1 (a, k, ustar, usdir, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    05-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    08-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    23-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     calculate diagonal of input source (actual source term put
    !     together in w3srce).
    !
    !  2. method :
    !
    !       wam-3 : snyder et al. (1981), komen et al. (1984).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       k       r.a.  i   wavenumber for entire spectrum.          *)
    !       ustar   real  i   friction velocity.
    !       usdir   real  i   direction of ustar.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative.             *)
    !     ----------------------------------------------------------------
    !                         *) stored as 1-d array with dimension nth*nk
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      prt2ds    subr. w3srrymd print plot of spectrum.
    !      outmat    subr. w3srrymd print out matrix.
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
    !     !/s   enable subroutine tracing.
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk, nth, nspec, sig, sig2, esin, ecos, sinc1
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), k(nspec), ustar, usdir
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is
    real                    :: cosu, sinu
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  preparations
    !
    cosu   = cos(usdir)
    sinu   = sin(usdir)
    !
    ! 2.  diagonal
    !
    do is=1, nspec
      d(is) = sinc1 * sig2(is) * max ( 0. ,                         &
           ( ustar * (ecos(is)*cosu+esin(is)*sinu)                  &
           * k(is)/sig2(is) - 0.035714) )
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
    !/ end of w3sin1 ----------------------------------------------------- /
    !/
  end subroutine w3sin1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate whitecapping source term and diagonal term of derivative.
  !>
  !> @param[in]  a      action density spectrum (1-d).
  !> @param[in]  k      wavenumber for entire spectrum.
  !> @param[in]  emean  mean wave energy.
  !> @param[in]  fmean  mean wave frequency.
  !> @param[in]  wnmean mean wavenumber.
  !> @param[out] s      source term (1-d version).
  !> @param[out] d      diagonal term of derivative.
  !>
  !> @author h. l. tolman  @date 23-dec-2004
  !>
  subroutine w3sds1 (a, k, emean, fmean, wnmean, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-dec-2004 |
    !/                  +-----------------------------------+
    !/
    !/    05-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    08-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    23-dec-2004 : multiple grid version.              ( version 3.06 )
    !/
    !  1. purpose :
    !
    !     calculate whitecapping source term and diagonal term of derivative.
    !
    !  2. method :
    !
    !       wam-3
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       k       r.a.  i   wavenumber for entire spectrum.          *)
    !       emean   real  i   mean wave energy.
    !       fmean   real  i   mean wave frequency.
    !       wnmean  real  i   mean wavenumber.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative.             *)
    !     ----------------------------------------------------------------
    !                         *) stored in 1-d array with dimension nth*nk
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      prt2ds    subr. w3srrymd print plot of spectrum.
    !      outmat    subr. w3srrymd print out matrix.
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
    !     !/s   enable subroutine tracing.
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk, nth, nspec, sig, sdsc1
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), k(nspec),                  &
         emean, fmean, wnmean
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is
    real                    :: factor
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  common factor
    !
    factor = sdsc1 * fmean * wnmean**3 * emean**2
    !
    !
    ! 3.  source term
    !
    d = factor * k
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
    !/ end of w3sds1 ----------------------------------------------------- /
    !/
  end subroutine w3sds1
  !/
  !/ end of module w3src1md -------------------------------------------- /
  !/
end module w3src1md
