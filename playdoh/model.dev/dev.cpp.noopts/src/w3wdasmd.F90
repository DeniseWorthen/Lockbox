!> @file
!> @brief contains module w3wdasmd.
!>
!> @author h. l. tolman @date 06-dec-2010
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
!> @brief intended as the interface for externally supplied
!>  data assimilation software.
!>
!> @details this module is intended as the interface for externally
!>  supplied data assimilation software to be used with wavewatch iii.
!>  the main subroutine w3wdas is incorporated in the generic wavewatch
!>  iii shell ww3_shel, and thus provides integrated time management
!>  and running of the wave model and data assimilation side by side.
!>
!>  present wave conditions (including dynamically changing wave
!>  grids), as well as wave data are passed to the routine through
!>  the dynamic data structrure, as introduced in model version 3.06.
!>
!>  a three tier data structure is used with three separate data
!>  sets. tentatively, they are intended for mean wave parameters,
!>  1-d and 2-d spectral data. this separation is made only for
!>  economy in file and memory usage. all three data sets are defined
!>  here only by a record length and a number of records. all data are
!>  treated as real numbers, but the meaning of all record components
!>  is completely at the discretion of the author of the data
!>  assimilation scheme.
!>
!>  to promote portability, it is suggested to use this module only
!>  as an interface to your own assimilation routine(s).
!>
!> @author h. l. tolman @date 06-dec-2010
!>
module w3wdasmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         06-dec-2010 |
  !/                  +-----------------------------------+
  !/
  !/    25-jan-2002 : origination.                        ( version 2.17 )
  !/    27-dec-2004 : multiple grid version.              ( version 3.06 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    06-dec-2010 : change from global (logical) to iclose (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     this module is intended as the interface for externally supplied
  !     data assimlation software to be used with wavewatch iii. the
  !     main subroutine w3wdas is incorporated in the generic wavewatch
  !     iii shell ww3_shel, and thus provides integrated time management
  !     and running of the wave model and data assimilation side by side.
  !
  !     present wave conditions (including dynamically changing wave
  !     grids), as well as wave data are passed to the routine through
  !     the dynamic data structrure, as introduced in model version 3.06
  !
  !     a three tier data structure is used with three separate data
  !     sets. tentatively, they are intended for mean wave parameters,
  !     1-d and 2-d spectral data. this separation is made only for
  !     economy in file and menory usage. all three data sets are defined
  !     here onlt by a record length and a number of records. all data are
  !     treated as real numbers, but the meaing of all record components
  !     is completely at the discretion of the author of the data
  !     assimilation scheme.
  !
  !     to promote portability, it is suggested to use this module only
  !     as an interface to your own assimilation routine(s).
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3wdas    subr. public   actual wave model.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      ....      subr. w3servmd service routines.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - this module still requires an openmp or  mpi setup to be made
  !       compatible with wavewatch iii inside the user supplied
  !       routines.
  !
  !  6. switches :
  !
  !       !/s     enable subroutine tracing.
  !       !/t     test output.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief wavewatch iii data assimilation interface routine.
  !>
  !> @param[in] dasflag flags for three data sets.
  !> @param[in] recl record lengths for three data sets.
  !> @param[in] ndat number of data for three data sets.
  !> @param[in] data0 observations.
  !> @param[in] data1 observations.
  !> @param[in] data2 observations.
  !>
  !> @author h. l. tolman  @date 06-dec-2010
  !>
  subroutine w3wdas ( dasflag, recl, ndat, data0, data1, data2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         06-dec-2010 |
    !/                  +-----------------------------------+
    !/
    !/    25-jan-2002 : origination.                        ( version 2.17 )
    !/    27-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     wavewatch iii data assimilation interface routine.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       dasflag   l.a.   i   flags for three data sets.
    !       recld   i.a.   i   record lengths for three data sets.
    !       nd      i.a.   i   number of data for three data sets.
    !       datan   r.a.   i   observations.
    !     ----------------------------------------------------------------
    !
    !     local parameters :
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      extcde    subr. w3servmd program abort.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !     any program shell or integrated model after initialization of
    !     wavewatch iii (to assure availability of data in used modules).
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
    !       !/s     enable subroutine tracing.
    !       !/t     enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd
    use w3wdatmd
    use w3adatmd
    use w3odatmd, only: ndso, ndse, ndst, screen, naproc, iaproc,   &
         naplog, napout, naperr
    !
    implicit none
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: recl(3), ndat(3)
    real, intent(in)        :: data0(recl(1),ndat(1))
    real, intent(in)        :: data1(recl(2),ndat(2))
    real, intent(in)        :: data2(recl(3),ndat(3))
    logical, intent(in)     :: dasflag(3)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters :
    !/
    integer                 :: j
    !/
    !/ ------------------------------------------------------------------- /
    ! 1.  initializations and test output
    ! 1.a subroutine tracing
    !
    !
    ! 1.b echo part of parameter list (test output only).
    !
    !
    ! 1.c test grid info from w3gdatmd
    !
    !
    ! 2.  actual data assimilation routine ------------------------------- /
    !
    !     user-defined data assimilation routines to be plugged in here.
    !     all that could be needed is avainalble in this subroutine,
    !     including the grid definition from w3gdatmd. all
    !     can thus be included in the parameter list, and no explcit links
    !     to other wavewatch iii routines will be needed within the
    !     data assimilation routines ( with the possible exception of the
    !     constants module ), if there is a reason to terminate the code,
    !     pass an error code out of the routine and use extcde to stop
    !     the wavewatch iii run altogether. check the system documentation
    !     on how to ad your routines to the compile and link system.
    !
    !     call .....
    !
    !     if ( ..... ) call extcde ( 99 )
    !
    return
    !
    ! formats
    !
    !1000 format (/' *** wavewatch iii error in w3wdas :'/                &
    !              '     illigal grid sizes input : ',4i8/                &
    !              '                         grid : ',4i8/)
    !
    !/
    !/ end of w3wdas ----------------------------------------------------- /
    !/
  end subroutine w3wdas
  !/
  !/ end of module w3wdasmd -------------------------------------------- /
  !/
end module w3wdasmd
