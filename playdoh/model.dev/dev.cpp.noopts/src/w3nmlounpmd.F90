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
module w3nmlounpmd
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
  !     manages namelists from configuration file ww3_ounp.nml for ww3_ounp program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! point structure
  type nml_point_t
    character(15)               :: timestart
    character(15)               :: timestride
    character(15)               :: timecount
    integer                     :: timesplit
    character(1024)             :: list
    logical                     :: samefile
    integer                     :: buffer
    integer                     :: type
    logical                     :: dimorder
  end type nml_point_t
  ! file structure
  type nml_file_t
    character(30)               :: prefix
    integer                     :: netcdf
  end type nml_file_t
  ! spectra structure
  type nml_spectra_t
    integer                     :: output
    real                        :: scale_fac
    real                        :: output_fac
    integer                     :: type
  end type nml_spectra_t
  ! param structure
  type nml_param_t
    integer                     :: output
  end type nml_param_t
  ! source structure
  type nml_source_t
    integer                     :: output
    real                        :: scale_fac
    real                        :: output_fac
    integer                     :: table_fac
    logical                     :: spectrum
    logical                     :: input
    logical                     :: interactions
    logical                     :: dissipation
    logical                     :: bottom
    logical                     :: ice
    logical                     :: total
  end type nml_source_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlounp (ndsi, infile, nml_point, nml_file,             &
       nml_spectra, nml_param, nml_source, ierr)
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
    !     reads all the namelist to define the output point
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      ndsi          int.
    !      infile        char.
    !      nml_point     type.
    !      nml_file      type.
    !      nml_spectra   type.
    !      nml_param     type.
    !      nml_source    type.
    !      ierr          int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_point_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_ounp  prog.   n/a    postprocess output points.
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
    type(nml_point_t), intent(inout)            :: nml_point
    type(nml_file_t), intent(inout)             :: nml_file
    type(nml_spectra_t), intent(inout)          :: nml_spectra
    type(nml_param_t), intent(inout)            :: nml_param
    type(nml_source_t), intent(inout)           :: nml_source
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
    ! read point namelist
    call read_point_nml (ndsi, nml_point)
    call report_point_nml (nml_point)
    ! read file namelist
    call read_file_nml (ndsi, nml_file)
    call report_file_nml (nml_file)
    ! read spectra namelist
    call read_spectra_nml (ndsi, nml_spectra)
    call report_spectra_nml (nml_spectra)
    ! read param namelist
    call read_param_nml (ndsi, nml_param)
    call report_param_nml (nml_param)
    ! read source namelist
    call read_source_nml (ndsi, nml_source)
    call report_source_nml (nml_source)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmlounp
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_point_nml (ndsi, nml_point)
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
    !      nml_point    type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_point_t), intent(inout)    :: nml_point
    ! locals
    integer                                :: ierr
    type(nml_point_t) :: point
    namelist /point_nml/ point
    ierr = 0
    ! set default values for point structure
    point%timestart  = '19000101 000000'
    point%timestride = '0'
    point%timecount  = '1000000000'
    point%timesplit  = 6
    point%list       = 'all'
    point%samefile   = .true.
    point%buffer     = 150
    point%type       = 1
    point%dimorder   = .true.
    ! read point namelist
    rewind (ndsi)
    read (ndsi, nml=point_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_point_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! save namelist
    nml_point = point
  end subroutine read_point_nml
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    ! locals
    integer                                :: ierr
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
  subroutine read_spectra_nml (ndsi, nml_spectra)
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
    !      nml_spectra  type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_spectra_t), intent(inout)  :: nml_spectra
    ! locals
    integer                                :: ierr
    type(nml_spectra_t) :: spectra
    namelist /spectra_nml/ spectra
    ierr = 0
    ! set default values for spectra structure
    spectra%output      = 3
    spectra%scale_fac   = 1
    spectra%output_fac  = 0
    spectra%type        = 4
    ! read spectra namelist
    rewind (ndsi)
    read (ndsi, nml=spectra_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_spectra_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (3)
    end if
    ! save namelist
    nml_spectra = spectra
  end subroutine read_spectra_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_param_nml (ndsi, nml_param)
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
    !      nml_param    type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_param_t), intent(inout)    :: nml_param
    ! locals
    integer                                :: ierr
    type(nml_param_t) :: param
    namelist /param_nml/ param
    ierr = 0
    ! set default values for param structure
    param%output      = 3
    ! read param namelist
    rewind (ndsi)
    read (ndsi, nml=param_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_param_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (4)
    end if
    ! save namelist
    nml_param = param
  end subroutine read_param_nml
  !/ ------------------------------------------------------------------- /
  subroutine read_source_nml (ndsi, nml_source)
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
    !      nml_source   type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_source_t), intent(inout)   :: nml_source
    ! locals
    integer                                :: ierr
    type(nml_source_t) :: source
    namelist /source_nml/ source
    ierr = 0
    ! set default values for source structure
    source%output      = 4
    source%scale_fac   = 0
    source%output_fac  = 0
    source%table_fac   = 0
    source%spectrum    = .true.
    source%input       = .true.
    source%interactions= .true.
    source%dissipation = .true.
    source%bottom      = .true.
    source%ice         = .true.
    source%total       = .true.
    ! read source namelist
    rewind (ndsi)
    read (ndsi, nml=source_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_source_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (5)
    end if
    ! save namelist
    nml_source = source
  end subroutine read_source_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_point_nml (nml_point)
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
    !      nml_point  type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_point_t), intent(in) :: nml_point
    write (msg,'(a)') 'point % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'timestart  = ', trim(nml_point%timestart)
    write (ndsn,10) trim(msg),'timestride = ', trim(nml_point%timestride)
    write (ndsn,10) trim(msg),'timecount  = ', trim(nml_point%timecount)
    write (ndsn,11) trim(msg),'timesplit  = ', nml_point%timesplit
    write (ndsn,10) trim(msg),'list       = ', trim(nml_point%list)
    write (ndsn,13) trim(msg),'samefile   = ', nml_point%samefile
    write (ndsn,11) trim(msg),'buffer     = ', nml_point%buffer
    write (ndsn,11) trim(msg),'type       = ', nml_point%type
    write (ndsn,13) trim(msg),'dimorder   = ', nml_point%dimorder
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
  end subroutine report_point_nml
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
  !/ ------------------------------------------------------------------- /
  subroutine report_spectra_nml (nml_spectra)
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
    !      nml_spectra  type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_spectra_t), intent(in) :: nml_spectra
    write (msg,'(a)') 'spectra % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'output     = ', nml_spectra%output
    write (ndsn,14) trim(msg),'scale_fac  = ', nml_spectra%scale_fac
    write (ndsn,14) trim(msg),'output_fac = ', nml_spectra%output_fac
    write (ndsn,11) trim(msg),'type       = ', nml_spectra%type
11  format (a,2x,a,i8)
14  format (a,2x,a,f8.2)
  end subroutine report_spectra_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_param_nml (nml_param)
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
    !      nml_param  type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_param_t), intent(in) :: nml_param
    write (msg,'(a)') 'param % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'output     = ', nml_param%output
11  format (a,2x,a,i8)
  end subroutine report_param_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_source_nml (nml_source)
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
    !      nml_source  type.
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
    !      w3nmlounp subr.   n/a    namelist configuration routine.
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
    type(nml_source_t), intent(in) :: nml_source
    write (msg,'(a)') 'source % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'output       = ', nml_source%output
    write (ndsn,14) trim(msg),'scale_fac    = ', nml_source%scale_fac
    write (ndsn,14) trim(msg),'output_fac   = ', nml_source%output_fac
    write (ndsn,11) trim(msg),'table_fac    = ', nml_source%table_fac
    write (ndsn,13) trim(msg),'spectrum     = ', nml_source%spectrum
    write (ndsn,13) trim(msg),'input        = ', nml_source%input
    write (ndsn,13) trim(msg),'interactions = ', nml_source%interactions
    write (ndsn,13) trim(msg),'dissipation  = ', nml_source%dissipation
    write (ndsn,13) trim(msg),'ice          = ', nml_source%ice
    write (ndsn,13) trim(msg),'total        = ', nml_source%total
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_source_nml
  !/ ------------------------------------------------------------------- /
end module w3nmlounpmd
!/ ------------------------------------------------------------------- /
