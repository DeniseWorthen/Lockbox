!> @file
!> @brief flux/stress computations according to hwang (2011).
!>
!> @author h. l. tolman
!> @author s. zieger
!> @author q. liu
!> @date   24-nov-2017
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
!> @brief flux/stress computations according to hwang ( 2011).
!>
!> @details hwang 2011: j atmos ocean tech 28(3)  436-443.
!>
!> @author h. l. tolman
!> @author s. zieger
!> @author q. liu
!> @date   24-nov-2017
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3flx4md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |           s. zieger               |
  !/                  |           q. liu                  |
  !/                  |                        fortran 90 |
  !/                  | last update :         24-nov-2017 |
  !/                  +-----------------------------------+
  !/
  !/    03-jul-2006 : origination.                        ( version 3.09 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    15-mar-2011 : implementation of hwang (2011)
  !/                  parameterization.
  !/    24-nov_2017 : modifying cdfac                     ( q. liu)
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     flux/stress computations according to hwang ( 2011).
  !
  !     references:
  !          hwang 2011: j atmos ocean tech 28(3)  436-443
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3flx4    subr. public   stresses according to hwang (jtech, 2011)
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
  !> @brief flux/stress computations according to hwang (jtech, 2011).
  !>
  !> @verbatim
  !>     cd    = 1e-4 ( -0.016 u10**2 + 0.967u10 + 8.058)
  !>     ustar = u10 * sqrt( u10 )
  !> @endverbatim
  !>
  !> @param[in]  zwnd wind height.
  !> @param[in]  u10  wind speed.
  !> @param[in]  u10d wind direction.
  !> @param[out] ust  friction velocity.
  !> @param[out] ustd direction of friction velocity.
  !> @param[out] z0   z0 in profile law.
  !> @param[out] cd   drag coefficient.
  !>
  !> @author h. l. tolman
  !> @date   03-jul-2006
  !>
  subroutine w3flx4 ( zwnd, u10, u10d, ust, ustd, z0, cd )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         03-jul-2006 |
    !/                  +-----------------------------------+
    !/
    !/    03-jul-2006 : origination.                        ( version 3.09 )
    !/
    !  1. purpose :
    !
    !     flux/stress computations according to hwang (jtech, 2011)
    !
    !  2. method :
    !
    !     cd    = 1e-4 ( -0.016 u10**2 + 0.967u10 + 8.058)
    !     ustar = u10 * sqrt( u10 )
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       zwnd    real   i   wind height.
    !       u10     real   i   wind speed.
    !       u10d    real   i   wind direction.
    !       ust     real   o   friction velocity.
    !       ustd    real   0   direction of friction velocity.
    !       z0      real   o   z0 in profile law.
    !       cd      real   o   drag coefficient.
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
    use w3odatmd, only: ndse, iaproc, naperr
    use w3gdatmd, only: flx4a0
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: zwnd, u10, u10d
    real, intent(out)       :: ust, ustd, z0, cd
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
    !     to prevent the drag coefficient from dropping to zero at extreme
    !     wind speeds, we use a simple modification ust = 2.026 m/s for
    !     u10 greater than 50.33 m/s.
    !
    if (u10 .ge. 50.33) then
      ust = 2.026 * sqrt(flx4a0)
      cd  = (ust/u10)**2
    else
      cd  = flx4a0 * ( 8.058 + 0.967*u10 - 0.016*u10**2 ) * 1e-4
      ust = u10 * sqrt(cd)
    end if
    !
    z0     = zwnd * exp ( -0.4 / sqrt(cd) )
    ustd   = u10d
    !
    return
    !
    ! formats
    !
1000 format (/' *** wavewatch iii error in w3flx4 : '/               &
         '     hight of wind should be 10m in this apprach '/   &
         '     zwnd =',f8.2,'m'/)
    !/
    !/ end of w3flx4 ----------------------------------------------------- /
    !/
  end subroutine w3flx4
  !/
  !/ end of module w3flx4md -------------------------------------------- /
  !/
end module w3flx4md
