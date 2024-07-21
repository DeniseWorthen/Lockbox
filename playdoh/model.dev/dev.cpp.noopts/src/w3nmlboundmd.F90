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
module w3nmlboundmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           m. accensi              |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :         27-may-2021 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see subroutines.
  !/
  !  1. purpose :
  !
  !     manages namelists from configuration file ww3_bound.nml for ww3_bound program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! bound structure
  type nml_bound_t
    character(5)                :: mode      !< read/write mode
    integer                     :: interp    !< interpolation mode
    integer                     :: verbose   !< verbose flag
    character(128)              :: file      !< listing spec file unit
  end type nml_bound_t
  ! miscellaneous
  character(256)                :: msg      !< report message
  integer                       :: ndsn     !< namelist file unit
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlbound (ndsi, infile, nml_bound, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-may-2021 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. purpose :
    !
    !     reads all the namelist to define the input boundary
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      ndsi        int.
    !      infile      char.
    !      nml_bound   type.
    !      ierr        int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_bound_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_bound  prog.   n/a    preprocess input boundaries.
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
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    !/s      use w3servmd, only: strace
    implicit none
    integer, intent(in)                         :: ndsi       !< input file unit
    character*(*), intent(in)                   :: infile     !< input file name
    type(nml_bound_t), intent(inout)            :: nml_bound  !< bound structure
    integer, intent(out)                        :: ierr       !< error code
    !/s      integer, save                             :: ient = 0   !< strace error code
    ierr = 0
    !/s      call strace (ient, 'w3nmlbound')
    ! open namelist log file
    ndsn = 3
    open (ndsn, file=trim(infile)//'.log', form='formatted', iostat=ierr)
    if (ierr.ne.0) then
      write (ndse,'(a)') 'error: open full nml file '//trim(infile)//'.log failed'
      return
    end if
    ! open input file
    open (ndsi, file=trim(infile), form='formatted', status='old', iostat=ierr)
    if (ierr.ne.0) then
      write (ndse,'(a)') 'error: open input file '//trim(infile)//' failed'
      return
    end if
    ! read bound namelist
    call read_bound_nml (ndsi, nml_bound)
    call report_bound_nml (nml_bound)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmlbound
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_bound_nml (ndsi, nml_bound)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-may-2021 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      ndsi         int.
    !      nml_bound    type.
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
    !      w3nmlbound subr.   n/a    namelist configuration routine.
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
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/s      use w3servmd, only: strace
    implicit none
    integer, intent(in)                 :: ndsi        !< namelist file unit
    type(nml_bound_t), intent(inout)    :: nml_bound   !< bound structure
    ! locals
    integer                   :: ierr                  !< error code
    type(nml_bound_t) :: bound                         !< bound structure
    namelist /bound_nml/ bound                         !< boudn namelist
    !/s      integer, save                           :: ient = 0       !< strace error code
    ierr = 0
    !/s      call strace (ient, 'read_bound_nml')
    ! set default values for track structure
    bound%mode       = 'write'
    bound%interp     = 2
    bound%verbose    = 1
    bound%file       = 'spec.list'
    ! read bound namelist
    rewind (ndsi)
    read (ndsi, nml=bound_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_bound_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! save namelist
    nml_bound = bound
  end subroutine read_bound_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_bound_nml (nml_bound)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         27-may-2021 |
    !/                  +-----------------------------------+
    !/
    !/
    !  1. purpose :
    !
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      nml_bound  type.
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
    !      w3nmlbound subr.   n/a    namelist configuration routine.
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
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/s      use w3servmd, only: strace
    implicit none
    type(nml_bound_t), intent(in) :: nml_bound                  !< bound structure
    !/s      integer, save                           :: ient = 0            ! strace error code
    !/s      call strace (ient, 'report_bound_nml')
    write (msg,'(a)') 'bound % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'mode       = ', trim(nml_bound%mode)
    write (ndsn,11) trim(msg),'interp     = ', nml_bound%interp
    write (ndsn,11) trim(msg),'verbose    = ', nml_bound%verbose
    write (ndsn,10) trim(msg),'file       = ', trim(nml_bound%file)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
  end subroutine report_bound_nml
  !/ ------------------------------------------------------------------- /
end module w3nmlboundmd
!/ ------------------------------------------------------------------- /
