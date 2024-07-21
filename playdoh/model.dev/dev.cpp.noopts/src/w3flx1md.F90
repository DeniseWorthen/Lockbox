!> @file
!> @brief flux/stress computations according to wu (1980).
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
!> @brief flux/stress computations according to wu (1980).
!>
!> @author h. l. tolman
!> @date   29-may-2009
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3flx1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         29-may-2009 |
  !/                  +-----------------------------------+
  !/
  !/    03-jul-2006 : origination.                        ( version 3.09 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     flux/stress computations according to wu (1980)
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3flx1    subr. public   stresses according to wu (1980).
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
  !     - originally used with source term !/st1.
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
  !> @brief flux/stress computations according to wu (1980).
  !>
  !> @param[inout] zwnd wind height.
  !> @param[inout] u10  wind speed.
  !> @param[inout] u10d wind direction.
  !> @param[inout] ust  friction velocity.
  !> @param[inout] ustd direction of friction velocity.
  !> @param[inout] z0   z0 in profile law.
  !> @param[inout] cd   drag coefficient.
  !>
  !> @author h. l. tolman
  !> @date   03-jul-2006
  !>
  subroutine w3flx1 ( zwnd, u10, u10d, ust, ustd, z0, cd )
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
    !     flux/stress computations according to wu (1980)
    !
    !  2. method :
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
    cd     = 0.001 * (0.8+0.065*u10)
    z0     = zwnd * exp ( -0.4 / sqrt(cd) )
    ust    = u10 * sqrt(cd)
    ustd   = u10d
    !
    return
    !
    ! formats
    !
1000 format (/' *** wavewatch iii error in w3str1 : '/               &
         '     hight of wind should be 10m in this apprach '/   &
         '     zwnd =',f8.2,'m'/)
    !/
    !/ end of w3flx1 ----------------------------------------------------- /
    !/
  end subroutine w3flx1
  !/
  !/ end of module inflx1md -------------------------------------------- /
  !/
end module w3flx1md
