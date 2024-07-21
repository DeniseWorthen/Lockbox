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
module w3nmlprncmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           m. accensi              |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see subroutines.
  !/
  !  1. purpose :
  !
  !     manages namelists from configuration file ww3_prnc.nml for ww3_prnc program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! field structure
  type nml_field_t
    logical                     :: ice_param1
    logical                     :: ice_param2
    logical                     :: ice_param3
    logical                     :: ice_param4
    logical                     :: ice_param5
    logical                     :: mud_density
    logical                     :: mud_thickness
    logical                     :: mud_viscosity
    logical                     :: water_levels
    logical                     :: currents
    logical                     :: winds
    logical                     :: winds_ast
    logical                     :: atm_momentum
    logical                     :: air_density
    logical                     :: ice_conc
    logical                     :: ice_berg
    logical                     :: data_assim
  end type nml_field_t
  ! grid structure
  type nml_grid_t
    logical                     :: asis
    logical                     :: latlon
  end type nml_grid_t
  ! forcing structure
  type nml_forcing_t
    character(15)               :: timestart
    character(15)               :: timestop
    type(nml_field_t)           :: field
    type(nml_grid_t)            :: grid
    character(256)              :: tidal
  end type nml_forcing_t
  ! file structure
  type nml_file_t
    character(256)              :: filename
    character(100)              :: longitude
    character(100)              :: latitude
    character(100)              :: var(3)
    character(15)               :: timeshift
  end type nml_file_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlprnc (ndsi, infile, nml_forcing, nml_file, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. purpose :
    !
    !     reads all the namelist to define the forcing field
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
    !      nml_forcing type.
    !      nml_file    type.
    !      ierr        int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_forcing_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_prnc  prog.   n/a    preprocess forcing fields.
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
    type(nml_forcing_t), intent(inout)          :: nml_forcing
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
    ! read forcing namelist
    call read_forcing_nml (ndsi, nml_forcing)
    call report_forcing_nml (nml_forcing)
    ! read file namelist
    call read_file_nml (ndsi, nml_file)
    call report_file_nml (nml_file)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmlprnc
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_forcing_nml (ndsi, nml_forcing)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
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
    !      nml_forcing  type.
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
    !      w3nmlprnc subr.   n/a    namelist configuration routine.
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
    type(nml_forcing_t), intent(inout)  :: nml_forcing
    ! locals
    integer                                :: ierr
    type(nml_forcing_t) :: forcing
    namelist /forcing_nml/ forcing
    ierr = 0
    ! set default values for forcing structure
    forcing%timestart  = '19000101 000000'
    forcing%timestop   = '29001231 000000'
    !
    forcing%field%ice_param1     = .false.
    forcing%field%ice_param2     = .false.
    forcing%field%ice_param3     = .false.
    forcing%field%ice_param4     = .false.
    forcing%field%ice_param5     = .false.
    forcing%field%mud_density    = .false.
    forcing%field%mud_thickness  = .false.
    forcing%field%mud_viscosity  = .false.
    forcing%field%water_levels   = .false.
    forcing%field%currents       = .false.
    forcing%field%winds          = .false.
    forcing%field%winds_ast      = .false.
    forcing%field%atm_momentum   = .false.
    forcing%field%air_density    = .false.
    forcing%field%ice_conc       = .false.
    forcing%field%ice_berg       = .false.
    forcing%field%data_assim     = .false.
    !
    forcing%grid%latlon  = .false.
    forcing%grid%asis    = .false.
    !
    forcing%tidal  = 'unset'
    ! read forcing namelist
    rewind (ndsi)
    read (ndsi, nml=forcing_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_forcing_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! set/check return values
    if (forcing%tidal.ne.'unset') then
      if (.not. forcing%field%water_levels .and. .not. forcing%field%currents) then
        write (ndse,'(a,i3)') 'error: tidal must only be used on field%water_levels or field%currents'
        call extcde (2)
      else if (.not. forcing%grid%asis) then
        write (ndse,'(a,i3)') 'error: tidal must only be used on grid%asis'
        call extcde (3)
      end if
    end if
    ! save namelist
    nml_forcing = forcing
  end subroutine read_forcing_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_file_nml (ndsi, nml_file)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
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
    !      w3nmlprnc subr.   n/a    namelist configuration routine.
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
    file%filename  = 'unset'
    file%longitude = 'unset'
    file%latitude  = 'unset'
    file%var(1)    = 'unset'
    file%var(2)    = 'unset'
    file%var(3)    = 'unset'
    file%timeshift = '00000000 000000'
    ! read file namelist
    rewind (ndsi)
    read (ndsi, nml=file_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_file_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (4)
    end if
    ! save namelist
    nml_file = file
  end subroutine read_file_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_forcing_nml (nml_forcing)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
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
    !      nml_forcing  type.
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
    !      w3nmlprnc subr.   n/a    namelist configuration routine.
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
    type(nml_forcing_t), intent(in) :: nml_forcing
    write (msg,'(a)') 'forcing % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'timestart  = ', trim(nml_forcing%timestart)
    write (ndsn,10) trim(msg),'timestop   = ', trim(nml_forcing%timestop)
    write (ndsn,13) trim(msg),'field % ice_param1     = ', nml_forcing%field%ice_param1
    write (ndsn,13) trim(msg),'field % ice_param2     = ', nml_forcing%field%ice_param2
    write (ndsn,13) trim(msg),'field % ice_param3     = ', nml_forcing%field%ice_param3
    write (ndsn,13) trim(msg),'field % ice_param4     = ', nml_forcing%field%ice_param4
    write (ndsn,13) trim(msg),'field % ice_param5     = ', nml_forcing%field%ice_param5
    write (ndsn,13) trim(msg),'field % mud_density    = ', nml_forcing%field%mud_density
    write (ndsn,13) trim(msg),'field % mud_thickness  = ', nml_forcing%field%mud_thickness
    write (ndsn,13) trim(msg),'field % mud_viscosity  = ', nml_forcing%field%mud_viscosity
    write (ndsn,13) trim(msg),'field % water_levels   = ', nml_forcing%field%water_levels
    write (ndsn,13) trim(msg),'field % currents       = ', nml_forcing%field%currents
    write (ndsn,13) trim(msg),'field % winds          = ', nml_forcing%field%winds
    write (ndsn,13) trim(msg),'field % winds_ast      = ', nml_forcing%field%winds_ast
    write (ndsn,13) trim(msg),'field % atm_momentum   = ', nml_forcing%field%atm_momentum
    write (ndsn,13) trim(msg),'field % air_density    = ', nml_forcing%field%air_density
    write (ndsn,13) trim(msg),'field % ice_conc       = ', nml_forcing%field%ice_conc
    write (ndsn,13) trim(msg),'field % ice_berg       = ', nml_forcing%field%ice_berg
    write (ndsn,13) trim(msg),'field % data_assim     = ', nml_forcing%field%data_assim
    write (ndsn,13) trim(msg),'grid % asis   = ', nml_forcing%grid%asis
    write (ndsn,13) trim(msg),'grid % latlon = ', nml_forcing%grid%latlon
    write (ndsn,10) trim(msg),'tidal = ', trim(nml_forcing%tidal)
10  format (a,2x,a,a)
13  format (a,2x,a,l1)
  end subroutine report_forcing_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_file_nml (nml_file)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-jan-2018 |
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
    !      w3nmlprnc subr.   n/a    namelist configuration routine.
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
    write (ndsn,10) trim(msg),'filename    = ', trim(nml_file%filename)
    write (ndsn,10) trim(msg),'longitude   = ', trim(nml_file%longitude)
    write (ndsn,10) trim(msg),'latitude    = ', trim(nml_file%latitude)
    write (ndsn,10) trim(msg),'var(1)      = ', trim(nml_file%var(1))
    write (ndsn,10) trim(msg),'var(2)      = ', trim(nml_file%var(2))
    write (ndsn,10) trim(msg),'var(3)      = ', trim(nml_file%var(3))
    write (ndsn,10) trim(msg),'timeshift   = ', trim(nml_file%timeshift)
10  format (a,2x,a,a)
  end subroutine report_file_nml
  !/ ------------------------------------------------------------------- /
end module w3nmlprncmd
!/ ------------------------------------------------------------------- /
