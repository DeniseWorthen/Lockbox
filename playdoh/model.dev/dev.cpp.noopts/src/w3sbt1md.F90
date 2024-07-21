!> @file
!> @brief jonswap bottom friction routine.
!>
!> @author h. l. tolman
!> @date   29-may-2009
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
!> @brief jonswap bottom friction routine.
!>
!> @author h. l. tolman
!> @date   29-may-2009
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3sbt1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see w3sbt1 documentation.
  !/
  !  1. purpose :
  !
  !     jonswap bottom friction routine.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sbt1    subr. public   jonswap source term.
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
  !>
  !> @brief bottom friction source term according to the empirical jonswap
  !>  formulation.
  !>
  !> @verbatim
  !>              2 gamma   /    cg         \      sbtc1 /     \       .
  !>       sbt = ---------- | ------- - 0.5 | e  = ----- | ... | e    (1)
  !>             grav depth \  si/wn        /      depth \     /
  !>
  !>     where gamma = -0.038 m2/s3 (jonswap)
  !>                 = -0.067 m2/s3 (bouws and komen 1983)
  !>
  !>     in the routine, the constant 2 gamma / grav = sbtc1.
  !> @endverbatim
  !>
  !> @param[in]  a      action density spectrum (1-d).
  !> @param[in]  cg     group velocities.
  !> @param[in]  wn     wavenumbers.
  !> @param[in]  depth  mean water depth.
  !> @param[out] s      source term (1-d version).
  !> @param[out] d      diagonal term of derivative (1-d version).
  !>
  !> @author h. l. tolman
  !> @date   29-may-2009
  !>
  subroutine w3sbt1 (a, cg, wn, depth, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-may-2009 |
    !/                  +-----------------------------------+
    !/
    !/    05-dec-1996 : final fortran 77.                   ( version 1.18 )
    !/    08-dec-1999 : upgrade to fortran 90.              ( version 2.00 )
    !/    20-dec-2004 : multiple model version.             ( version 3.06 )
    !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
    !/
    !/    copyright 2009 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !  1. purpose :
    !
    !     bottom friction source term according to the empirical jonswap
    !     formulation.
    !
    !  2. method :
    !
    !              2 gamma   /    cg         \      sbtc1 /     \       .
    !       sbt = ---------- | ------- - 0.5 | e  = ----- | ... | e    (1)
    !             grav depth \  si/wn        /      depth \     /
    !
    !     where gamma = -0.038 m2/s3 (jonswap)
    !                 = -0.067 m2/s3 (bouws and komen 1983)
    !
    !     in the routine, the constant 2 gamma / grav = sbtc1.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d)
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       depth   real  i   mean water depth.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing (!/s switch).
    !      prt2ds    subr. w3arrymd print plot output (!/t1 switch).
    !      outmat    subr. w3arrymd matrix output (!/t2 switch).
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
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk, nth, nspec, sig, mapwn, sbtc1
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: cg(nk), wn(nk), depth, a(nspec)
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, ik, nscut
    real                    :: factor, cbeta(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  deep water ===================================================== *
    !
    if ( depth*wn(1) .gt. 6 ) then
      !
      d = 0.
      s = 0.
      !
      ! 2.  shallow water ================================================== *
      !
    else
      !
      ! 2.a set constant
      !
      factor = sbtc1 / depth
      !
      !
      ! 2.b wavenumber dependent part.
      !
      do ik=1, nk
        if ( wn(ik)*depth .gt. 6. ) exit
        cbeta(ik) = factor *                                      &
             max(0., (cg(ik)*wn(ik)/sig(ik)-0.5) )
      end do
      !
      ! 2.c fill diagional matrix
      !
      nscut  = (ik-1)*nth
      !
      do is=1, nscut
        d(is) = cbeta(mapwn(is))
      end do
      !
      do is=nscut+1, nspec
        d(is) = 0.
      end do
      !
      s = d * a
      !
    end if
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
    !/ end of w3sbt1 ----------------------------------------------------- /
    !/
  end subroutine w3sbt1
  !/
  !/ end of module w3sbt1md -------------------------------------------- /
  !/
end module w3sbt1md
