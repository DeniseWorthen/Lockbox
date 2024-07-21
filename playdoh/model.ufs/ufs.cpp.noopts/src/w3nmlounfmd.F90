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
module w3nmlounfmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           m. accensi              |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :         12-jan-2021 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see subroutines.
  !/
  !  1. purpose :
  !
  !     manages namelists from configuration file ww3_ounf.nml for ww3_ounf program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! field structure
  type nml_field_t
    character(15)               :: timestart
    character(15)               :: timestride
    character(15)               :: timecount
    integer                     :: timesplit
    character(1024)             :: list
    character(15)               :: partition
    logical                     :: samefile
    logical                     :: vector
    integer                     :: type
    character(15)               :: timeref
    logical                     :: fcvars
    character                   :: timevar
    character                   :: timeunit
    character(15)               :: timeepoch
    real                        :: noval
    logical                     :: mapsta
  end type nml_field_t
  ! file structure
  type nml_file_t
    character(30)               :: prefix
    integer                     :: netcdf
    integer                     :: ix0
    integer                     :: ixn
    integer                     :: iy0
    integer                     :: iyn
  end type nml_file_t
  ! smc grid structure
  type nml_smc_t
    integer                     :: type
    real                        :: sxo
    real                        :: syo
    real                        :: exo
    real                        :: eyo
    integer                     :: celfac
  end type nml_smc_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlounf (ndsi, infile, nml_field, nml_file, nml_smc, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-jan-2021 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. purpose :
    !
    !     reads all the namelist to define the output field
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
    !      nml_field   type.
    !      nml_file    type.
    !      nml_smc     type.
    !      ierr        int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_field_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_ounf  prog.   n/a    postprocess output fields.
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
    type(nml_field_t), intent(inout)            :: nml_field
    type(nml_file_t), intent(inout)             :: nml_file
    type(nml_smc_t), intent(inout)              :: nml_smc
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
    ! read field namelist
    call read_field_nml (ndsi, nml_field)
    call report_field_nml (nml_field)
    ! read file namelist
    call read_file_nml (ndsi, nml_file)
    call report_file_nml (nml_file)
    ! read smc namelist
    call read_smc_nml (ndsi, nml_smc)
    call report_smc_nml (nml_smc)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmlounf
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_field_nml (ndsi, nml_field)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         12-jan-2021 |
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
    !      nml_field    type.
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
    !      w3nmlounf subr.   n/a    namelist configuration routine.
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
    use constants, only: undef
    implicit none
    integer, intent(in)                 :: ndsi
    type(nml_field_t), intent(inout)    :: nml_field
    ! locals
    integer                                :: ierr
    type(nml_field_t) :: field
    namelist /field_nml/ field
    ierr = 0
    ! set default values for field structure
    field%timestart  = '19000101 000000'
    field%timestride = '0'
    field%timecount  = '1000000000'
    field%timesplit  = 6
    field%list       = 'unset'
    field%partition  = '0 1 2 3'
    field%samefile   = .true.
    field%vector     = .true.
    field%type       = 3
    field%timeref    = 'unset'
    field%fcvars     = .false.
    field%timevar    = 'd'
    field%timeunit   = 'd'
    field%timeepoch  = '19900101 000000'
    field%noval      = undef
    field%mapsta     = .true.
    ! read field namelist
    rewind (ndsi)
    read (ndsi, nml=field_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_field_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! default forecast reference time to start time
    if(field%timeref == 'unset') field%timeref = field%timestart
    ! save namelist
    nml_field = field
  end subroutine read_field_nml
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
    !      w3nmlounf subr.   n/a    namelist configuration routine.
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
    file%ix0       = 1
    file%ixn       = 1000000000
    file%iy0       = 1
    file%iyn       = 1000000000
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
  subroutine read_smc_nml (ndsi, nml_smc)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-sep-2018 |
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
    !      nml_smc      type.
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
    !      w3nmlounf subr.   n/a    namelist configuration routine.
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
    type(nml_smc_t), intent(inout)      :: nml_smc
    ! locals
    integer                                :: ierr
    type(nml_smc_t) :: smc
    namelist /smc_nml/ smc
    ierr = 0
    ! set default values for smc structure
    smc%sxo       = -999.9
    smc%syo       = -999.9
    smc%exo       = -999.9
    smc%eyo       = -999.9
    smc%celfac    = 1
    smc%type      = 1
    ! read smc namelist
    rewind (ndsi)
    read (ndsi, nml=smc_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_smc_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (3)
    end if
    ! save namelist
    nml_smc = smc
  end subroutine read_smc_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_field_nml (nml_field)
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
    !      nml_field  type.
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
    !      w3nmlounf subr.   n/a    namelist configuration routine.
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
    type(nml_field_t), intent(in) :: nml_field
    write (msg,'(a)') 'field % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'timestart  = ', trim(nml_field%timestart)
    write (ndsn,10) trim(msg),'timestride = ', trim(nml_field%timestride)
    write (ndsn,10) trim(msg),'timecount  = ', trim(nml_field%timecount)
    write (ndsn,11) trim(msg),'timesplit  = ', nml_field%timesplit
    write (ndsn,10) trim(msg),'list       = ', trim(nml_field%list)
    write (ndsn,10) trim(msg),'partition  = ', trim(nml_field%partition)
    write (ndsn,13) trim(msg),'samefile   = ', nml_field%samefile
    write (ndsn,11) trim(msg),'type       = ', nml_field%type
    write (ndsn,10) trim(msg),'fcvars     = ', nml_field%fcvars
    write (ndsn,10) trim(msg),'timeref    = ', nml_field%timeref
    write (ndsn,14) trim(msg),'noval      = ', nml_field%noval
    write (ndsn,13) trim(msg),'mapsta     = ', nml_field%mapsta
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_field_nml
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
    !      w3nmlounf subr.   n/a    namelist configuration routine.
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
    write (ndsn,11) trim(msg),'ix0       = ', nml_file%ix0
    write (ndsn,11) trim(msg),'ixn       = ', nml_file%ixn
    write (ndsn,11) trim(msg),'iy0       = ', nml_file%iy0
    write (ndsn,11) trim(msg),'iyn       = ', nml_file%iyn
10  format (a,2x,a,a)
11  format (a,2x,a,i12)
  end subroutine report_file_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_smc_nml (nml_smc)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-sep-2018 |
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
    !      nml_smc   type.
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
    !      w3nmlounf subr.   n/a    namelist configuration routine.
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
    type(nml_smc_t), intent(in) :: nml_smc
    write (msg,'(a)') 'smc % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'type      = ', nml_smc%type
    write (ndsn,14) trim(msg),'sxo       = ', nml_smc%sxo
    write (ndsn,14) trim(msg),'syo       = ', nml_smc%syo
    write (ndsn,14) trim(msg),'exo       = ', nml_smc%exo
    write (ndsn,14) trim(msg),'eyo       = ', nml_smc%eyo
    write (ndsn,11) trim(msg),'celfac    = ', nml_smc%celfac
11  format (a,2x,a,i12)
14  format (a,2x,a,f8.2)
  end subroutine report_smc_nml
  !/ ------------------------------------------------------------------- /
end module w3nmlounfmd
!/ ------------------------------------------------------------------- /
