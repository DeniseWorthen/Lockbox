!> @file
!> @brief unified process to obtain friction velocity and drag when stresses
!>  are an input (from atmospheric model).
!>
!> @author n.g. valiente
!> @author j. edward
!> @author a. saulter
!> @date   01-jul-2021
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
!> @brief unified process to obtain friction velocity and drag when stresses
!>  are an input (from atmospheric model).
!>
!> @author n.g. valiente
!> @author j. edward
!> @author a. saulter
!> @date   01-jul-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3flx5md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |                                   |
  !/                  |           n.g. valiente           |
  !/                  |           j. edward               |
  !/                  |           a. saulter              |
  !/                  |                        fortran 90 |
  !/                  | last update :         01-jul-2021 |
  !/                  +-----------------------------------+
  !/
  !/    22-mar-2021 : origination.                        ( version 7.14 )
  !/    22-mar-2021 : enable direct use of atmospheric model wind stress
  !/                  by source terms st6
  !/    01-jul-2021 : enable direct use of atmospheric model wind stress
  !/                  by source terms st4
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     unified process to obtain friction velocity and drag when stresses are an
  !     input (from atmospheric model).
  !
  !     references:
  !          xx
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3flx5    subr. public   stresses closure
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
  !> @brief unified process to obtain friction velocity and drag when
  !>  stresses are an input (from atmospheric model).
  !>
  !> @verbatim
  !>     ust       = sqrt(taua / rhoair)
  !>     ustd      = tauadir
  !>     cd        = (ust/u10)**2
  !>     sqrtcdm1  = min(u10/ust,100.0)
  !>     z0        = zwnd*exp(-kappa*sqrtcdm1)
  !> @endverbatim
  !>
  !> @param[in]  zwnd    wind height.
  !> @param[in]  u10     wind speed.
  !> @param[in]  u10d    wind direction.
  !> @param[in]  taua    atmosphere total stress.
  !> @param[in]  tauadir atmosphere total stress directions.
  !> @param[in]  rhoair  air density.
  !> @param[out] ust     friction velocity.
  !> @param[out] ustd    direction of friction velocity.
  !> @param[out] z0      z0 in profile law.
  !> @param[out] cd      drag coefficient.
  !> @param[out] charn   charnock coefficient.
  !>
  !> @author n.g. valiente
  !> @author j. edward
  !> @author a. saulter
  !> @date   01-jul-2021
  !>
  subroutine w3flx5 ( zwnd, u10, u10d, taua, tauadir, rhoair, ust, ustd, z0, cd, charn )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                     |
    !/                  |           n.g. valiente           |
    !/                  |           j. edward               |
    !/                  |           a. saulter              |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-jul-2021 |
    !/                  +-----------------------------------+
    !/
    !/    22-mar-2021 : origination.                        ( version 7.14 )
    !/    22-mar-2021 : enable direct use of atmospheric model wind stress
    !/                  by source terms st6
    !/    01-jul-2021 : enable direct use of atmospheric model wind stress
    !/                  by source terms st4
    !/
    !  1. purpose :
    !
    !     unified process to obtain friction velocity and drag when stresses are an
    !     input (from atmospheric model).
    !
    !  2. method :
    !
    !     ust       = sqrt(taua / rhoair)
    !     ustd      = tauadir
    !     cd        = (ust/u10)**2
    !     sqrtcdm1  = min(u10/ust,100.0)
    !     z0        = zwnd*exp(-kappa*sqrtcdm1)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       zwnd    real   i   wind height.
    !       u10     real   i   wind speed.
    !       u10d    real   i   wind direction.
    !       taua    real   i   atm. total stress.
    !       tauadir real   i   atm. total stress direction.
    !       rhoair  real   i   air density.
    !       ust     real   o   friction velocity.
    !       ustd    real   0   direction of friction velocity.
    !       z0      real   o   z0 in profile law.
    !       cd      real   o   drag coefficient.
    !       charn   real   o   charnock coefficient
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
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: kappa, grav, nu_air
    use w3odatmd, only: ndse, iaproc, naperr
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: zwnd, u10, u10d, taua, tauadir, rhoair
    real, intent(out)       :: ust, ustd, z0, cd, charn
    real                    :: unz, sqrtcdm1
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  tests ---------------------------------------------------------- *
    !
    if ( abs(zwnd-10.) .gt. 0.01 ) then
      if ( iaproc .eq. naperr ) write (ndse,1000) zwnd
      call extcde (1)
    end if
    !
    ! 2.  computation ---------------------------------------------------- *
    !
    !
    ust    = max ( 1e-4, sqrt(taua/rhoair) )
    unz    = max ( 0.01 , u10 )
    cd     = (ust/unz)**2
    ustd   = tauadir
    sqrtcdm1  = min(unz/ust,100.0)
    z0        = zwnd*exp(-kappa*sqrtcdm1)
    if (unz.gt.2.5) then
      charn = (z0 - 0.11 * nu_air / ust) * grav / ust**2
      charn = max( charn , 0.0095 )
      charn = min( 0.035 , charn )
    else
      charn = 0.0095
    end if
    !
    return
    !
    ! formats
    !
1000 format (/' *** wavewatch iii error in w3flx5 : '/                 &
         '     height of wind should be 10m in this approach '/   &
         '     zwnd =',f8.2,'m'/)
    !/
    !/ end of w3flx5 ----------------------------------------------------- /
    !/
  end subroutine w3flx5
  !/
  !/ end of module w3flx5md -------------------------------------------- /
  !/
end module w3flx5md
