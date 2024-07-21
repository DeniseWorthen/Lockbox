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
module w3nmltrncmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           m. accensi              |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :         15-may-2018 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see subroutines.
  !/
  !  1. purpose :
  !
  !     manages namelists from configuration file ww3_trnc.nml for ww3_trnc program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! track structure
  type nml_track_t
    character(15)               :: timestart
    character(15)               :: timestride
    character(15)               :: timecount
    integer                     :: timesplit
  end type nml_track_t
  ! file structure
  type nml_file_t
    character(30)               :: prefix
    integer                     :: netcdf
  end type nml_file_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmltrnc (ndsi, infile, nml_track, nml_file, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. purpose :
    !
    !     reads all the namelist to define the output track
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
    !      nml_track   type.
    !      nml_file    type.
    !      ierr        int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_track_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_trnc  prog.   n/a    postprocess output tracks.
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
    implicit none
    integer, intent(in)                         :: ndsi
    character*(*), intent(in)                   :: infile
    type(nml_track_t), intent(inout)            :: nml_track
    type(nml_file_t), intent(inout)             :: nml_file
    integer, intent(out)                        :: ierr
    ierr = 0
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
    ! read track namelist
    call read_track_nml (ndsi, nml_track)
    call report_track_nml (nml_track)
    ! read file namelist
    call read_file_nml (ndsi, nml_file)
    call report_file_nml (nml_file)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmltrnc
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_track_nml (ndsi, nml_track)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
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
    !      nml_track    type.
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
    !      w3nmltrnc subr.   n/a    namelist configuration routine.
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
    implicit none
    integer, intent(in)                 :: ndsi
    type(nml_track_t), intent(inout)    :: nml_track
    integer                             :: ierr
    ! locals
    type(nml_track_t) :: track
    namelist /track_nml/ track
    ierr = 0
    ! set default values for track structure
    track%timestart  = '19000101 000000'
    track%timestride = '0'
    track%timecount  = '1000000000'
    track%timesplit  = 6
    ! read track namelist
    rewind (ndsi)
    read (ndsi, nml=track_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_track_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! save namelist
    nml_track = track
  end subroutine read_track_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_file_nml (ndsi, nml_file)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
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
    !      nml_file     type.
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
    !      w3nmltrnc subr.   n/a    namelist configuration routine.
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
    implicit none
    integer, intent(in)                 :: ndsi
    type(nml_file_t), intent(inout)     :: nml_file
    integer                             :: ierr
    ! locals
    type(nml_file_t) :: file
    namelist /file_nml/ file
    ierr = 0
    ! set default values for file structure
    file%prefix    = 'ww3.'
    file%netcdf    = 3
    ! read file namelist
    rewind (ndsi)
    read (ndsi, nml=file_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_file_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (2)
    end if
    ! save namelist
    nml_file = file
  end subroutine read_file_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_track_nml (nml_track)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
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
    !      nml_track  type.
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
    !      w3nmltrnc subr.   n/a    namelist configuration routine.
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
    implicit none
    type(nml_track_t), intent(in) :: nml_track
    write (msg,'(a)') 'track % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'timestart  = ', trim(nml_track%timestart)
    write (ndsn,10) trim(msg),'timestride = ', trim(nml_track%timestride)
    write (ndsn,10) trim(msg),'timecount  = ', trim(nml_track%timecount)
    write (ndsn,11) trim(msg),'timesplit  = ', nml_track%timesplit
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
  end subroutine report_track_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_file_nml (nml_file)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
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
    !      nml_file  type.
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
    !      w3nmltrnc subr.   n/a    namelist configuration routine.
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
    implicit none
    type(nml_file_t), intent(in) :: nml_file
    write (msg,'(a)') 'file % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'prefix    = ', trim(nml_file%prefix)
    write (ndsn,11) trim(msg),'netcdf    = ', nml_file%netcdf
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
  end subroutine report_file_nml
  !/ ------------------------------------------------------------------- /
end module w3nmltrncmd
!/ ------------------------------------------------------------------- /
