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
module w3nmlshelmd
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
  !     manages namelists from configuration file ww3_shel.nml for ww3_shel program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! domain structure
  type nml_domain_t
    integer                     :: iostyp
    character(15)               :: start
    character(15)               :: stop
    character(1)                :: outffile
  end type nml_domain_t
  ! input structure
  type nml_forcing_t
    character(13)               :: water_levels
    character(13)               :: currents
    character(13)               :: winds
    character(13)               :: atm_momentum
    character(13)               :: air_density
    character(13)               :: ice_conc
    character(13)               :: ice_param1
    character(13)               :: ice_param2
    character(13)               :: ice_param3
    character(13)               :: ice_param4
    character(13)               :: ice_param5
    character(13)               :: mud_density
    character(13)               :: mud_thickness
    character(13)               :: mud_viscosity
  end type nml_forcing_t
  !
  type nml_assim_t
    character(13)               :: mean
    character(13)               :: spec1d
    character(13)               :: spec2d
  end type nml_assim_t
  !
  type nml_input_t
    type(nml_forcing_t)         :: forcing
    type(nml_assim_t)           :: assim
  end type nml_input_t
  ! output type structure
  type nml_field_t
    character(1024)             :: list
  end type nml_field_t
  !
  type nml_point_t
    character(64)               :: file
  end type nml_point_t
  !
  type nml_track_t
    logical                     :: format
  end type nml_track_t
  !
  type nml_partition_t
    integer                     :: x0
    integer                     :: xn
    integer                     :: nx
    integer                     :: y0
    integer                     :: yn
    integer                     :: ny
    logical                     :: format
  end type nml_partition_t
  !
  !
  type nml_restart_t
    character(1024)             :: extra
  end type nml_restart_t
  !
  type nml_output_type_t
    type(nml_point_t)               :: point
    type(nml_field_t)               :: field
    type(nml_track_t)               :: track
    type(nml_partition_t)           :: partition
    type(nml_restart_t)             :: restart
  end type nml_output_type_t
  ! output date structure
  type nml_output_time_t
    character(15)               :: start
    character(15)               :: stride
    character(15)               :: stop
    character(15)               :: outffile
  end type nml_output_time_t
  !
  type nml_output_date_t
    type(nml_output_time_t)         :: field
    type(nml_output_time_t)         :: point
    type(nml_output_time_t)         :: track
    type(nml_output_time_t)         :: restart
    type(nml_output_time_t)         :: restart2
    type(nml_output_time_t)         :: boundary
    type(nml_output_time_t)         :: partition
    type(nml_output_time_t)         :: coupling
  end type nml_output_date_t
  ! homogeneous input structure
  type nml_homog_count_t
    integer                     :: n_ic1
    integer                     :: n_ic2
    integer                     :: n_ic3
    integer                     :: n_ic4
    integer                     :: n_ic5
    integer                     :: n_mdn
    integer                     :: n_mth
    integer                     :: n_mvs
    integer                     :: n_lev
    integer                     :: n_cur
    integer                     :: n_wnd
    integer                     :: n_ice
    integer                     :: n_tau
    integer                     :: n_rho
    integer                     :: n_mov
    integer                     :: n_tot
  end type nml_homog_count_t
  !
  type nml_homog_input_t
    character(15)               :: name
    character(15)               :: date
    real                        :: value1
    real                        :: value2
    real                        :: value3
  end type nml_homog_input_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlshel (mpi_comm, ndsi, infile, nml_domain,            &
       nml_input, nml_output_type, nml_output_date,   &
       nml_homog_count, nml_homog_input, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-apr-2018 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !
    !     reads all the namelist to define the single grid
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      mpi_comm  int.  public   communicator used in the wave model.
    !      ndsi
    !      infile
    !      nml_domain
    !      nml_input
    !      nml_output_type
    !      nml_output_date
    !      nml_homog_count
    !      nml_homog_input
    !      ierr
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_domain_nml
    !      report_domain_nml
    !      read_input_nml
    !      report_input_nml
    !      read_output_type_nml
    !      report_output_type_nml
    !      read_output_date_nml
    !      report_output_date_nml
    !      read_homogeneous_nml
    !      report_homogeneous_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_shel  prog.   n/a    single grid main program.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use wmmdatmd, only: mdse, improc, nmplog
    use wmmdatmd, only: mpi_comm_mwave
    implicit none
    integer, intent(in)                       :: mpi_comm, ndsi
    character*(*), intent(in)                 :: infile
    type(nml_domain_t), intent(inout)         :: nml_domain
    type(nml_input_t), intent(inout)          :: nml_input
    type(nml_output_type_t), intent(inout)    :: nml_output_type
    type(nml_output_date_t), intent(inout)    :: nml_output_date
    type(nml_homog_count_t), intent(inout)   :: nml_homog_count
    type(nml_homog_input_t), allocatable, intent(inout)    :: nml_homog_input(:)
    integer, intent(out)                      :: ierr
    ! locals
    integer                                 :: ierr_mpi
    logical                                 :: is_open
    ierr = 0
    mpi_comm_mwave = mpi_comm
    call mpi_comm_rank ( mpi_comm_mwave, improc, ierr_mpi )
    improc = improc + 1
    ! open namelist log file
    if ( nmplog .eq. improc ) then
      open (newunit=ndsn, file=trim(infile)//'.log', form='formatted', iostat=ierr)
      if (ierr.ne.0) then
        write (mdse,'(a)') 'error: open full nml file '//trim(infile)//'.log failed'
        return
      end if
    end if
    inquire (unit=ndsi, opened=is_open)
    if (.not. is_open) then
      ! open input file
      open (ndsi, file=trim(infile), form='formatted', status='old', iostat=ierr)
      if (ierr.ne.0) then
        write (mdse,'(a)') 'error: open input file '//trim(infile)//' failed'
        return
      end if
    end if
    ! read domain namelist
    call read_domain_nml (ndsi, nml_domain)
    if ( improc .eq. nmplog ) call report_domain_nml (nml_domain)
    ! read input namelist
    call read_input_nml (ndsi, nml_input)
    if ( improc .eq. nmplog ) call report_input_nml (nml_input)
    ! read output type namelist
    call read_output_type_nml (ndsi, nml_output_type)
    if ( improc .eq. nmplog ) call report_output_type_nml (nml_output_type)
    ! read output date namelist
    call read_output_date_nml (ndsi, nml_output_date)
    if ( improc .eq. nmplog ) call report_output_date_nml (nml_output_date)
    ! read homogeneous namelist
    call read_homogeneous_nml (ndsi, nml_homog_count, nml_homog_input)
    if ( improc .eq. nmplog ) call report_homogeneous_nml (nml_homog_count, nml_homog_input)
    ! close namelist files
    close (ndsi)
    if ( nmplog .eq. improc ) close (ndsn)
  end subroutine w3nmlshel
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_domain_nml (ndsi, nml_domain)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-apr-2018 |
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
    !      ndsi              int.
    !      nml_domain        type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use wmmdatmd, only: mdse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)               :: ndsi
    type(nml_domain_t), intent(out)   :: nml_domain
    ! locals
    integer                                :: ierr
    type(nml_domain_t) :: domain
    namelist /domain_nml/ domain
    ierr = 0
    ! set default values for domain structure
    domain%iostyp = 1
    domain%start  = '19680606 000000'
    domain%stop   = '19680607 000000'
    ! read domain namelist
    rewind (ndsi)
    read (ndsi, nml=domain_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_domain_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! set/check return values
    if (domain%iostyp.lt.0.or.domain%iostyp.gt.3) then
      write (mdse,'(a,i3)') 'error: bad iostyp input: ',domain%iostyp
      call extcde (2)
    end if
    ! save namelist
    nml_domain = domain
  end subroutine read_domain_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_input_nml (ndsi, nml_input)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
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
    !      ndsi              int.
    !      nml_input         type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use wmmdatmd, only: mdse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)                :: ndsi
    type(nml_input_t), intent(inout)   :: nml_input
    ! locals
    integer                                :: ierr
    type(nml_input_t) :: input
    namelist /input_nml/ input
    ierr = 0
    ! set default values for input structure
    input%forcing%water_levels  = 'f'
    input%forcing%currents      = 'f'
    input%forcing%winds         = 'f'
    input%forcing%atm_momentum  = 'f'
    input%forcing%air_density   = 'f'
    input%forcing%ice_conc      = 'f'
    input%forcing%ice_param1    = 'f'
    input%forcing%ice_param2    = 'f'
    input%forcing%ice_param3    = 'f'
    input%forcing%ice_param4    = 'f'
    input%forcing%ice_param5    = 'f'
    input%forcing%mud_density   = 'f'
    input%forcing%mud_thickness = 'f'
    input%forcing%mud_viscosity = 'f'
    input%assim%mean            = 'f'
    input%assim%spec1d          = 'f'
    input%assim%spec2d          = 'f'
    ! read input namelist
    rewind (ndsi)
    read (ndsi, nml=input_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_input_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (3)
    end if
    ! save namelist
    nml_input = input
  end subroutine read_input_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_output_type_nml (ndsi, nml_output_type)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-sep-2020 |
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
    !      ndsi              int.
    !      nml_output_type   type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use wmmdatmd, only: mdse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)                    :: ndsi
    type(nml_output_type_t), intent(inout) :: nml_output_type
    ! locals
    integer                                :: ierr
    type(nml_output_type_t) :: type
    namelist /output_type_nml/ type
    ierr = 0
    ! set default values for output type structure
    type%field%list   = 'unset'
    type%point%file   = 'points.list'
    type%track%format = .true.
    type%partition%x0 = 0
    type%partition%xn = 0
    type%partition%nx = 0
    type%partition%y0 = 0
    type%partition%yn = 0
    type%partition%ny = 0
    type%partition%format = .true.
    type%restart%extra = 'unset'
    ! read output type namelist
    rewind (ndsi)
    read (ndsi, nml=output_type_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_output_type_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (4)
    end if
    ! save namelist
    nml_output_type = type
  end subroutine read_output_type_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_output_date_nml (ndsi, nml_output_date)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-apr-2018 |
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
    !      ndsi              int.
    !      nml_output_date   type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use wmmdatmd, only: mdse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)                    :: ndsi
    type(nml_output_date_t), intent(inout) :: nml_output_date
    ! locals
    integer                                :: ierr
    type(nml_output_date_t) :: date
    namelist /output_date_nml/ date
    ierr = 0
    ! set default values for output_date structure
    date%field%start = '19680606 000000'
    date%field%stride = '0'
    date%field%stop = '19680607 000000'
    date%field%outffile = '0'
    date%point%outffile = '0'
    date%point%start = '19680606 000000'
    date%point%stride = '0'
    date%point%stop = '19680607 000000'
    date%track%start = '19680606 000000'
    date%track%stride = '0'
    date%track%stop = '19680607 000000'
    date%restart%start = '19680606 000000'
    date%restart%stride = '0'
    date%restart%stop = '19680607 000000'
    date%restart2%start = '19680606 000000'
    date%restart2%stride = '0'
    date%restart2%stop = '19680607 000000'
    date%boundary%start = '19680606 000000'
    date%boundary%stride = '0'
    date%boundary%stop = '19680607 000000'
    date%partition%start = '19680606 000000'
    date%partition%stride = '0'
    date%partition%stop = '19680607 000000'
    date%coupling%start = '19680606 000000'
    date%coupling%stride = '0'
    date%coupling%stop = '19680607 000000'
    ! read output date namelist
    rewind (ndsi)
    read (ndsi, nml=output_date_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_output_date_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (5)
    end if
    ! save namelist
    nml_output_date = date
  end subroutine read_output_date_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_homogeneous_nml (ndsi, nml_homog_count, nml_homog_input)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
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
    !      ndsi              int.
    !      nml_homog_count   type.
    !      nml_homog_input   type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use wmmdatmd, only: mdse
    use w3servmd, only: extcde
    implicit none
    integer, intent(in)                    :: ndsi
    type(nml_homog_count_t), intent(out)   :: nml_homog_count
    type(nml_homog_input_t), allocatable, intent(out)   :: nml_homog_input(:)
    ! locals
    integer                   :: ierr, i
    type(nml_homog_count_t)  :: homog_count
    namelist /homog_count_nml/  homog_count
    type(nml_homog_input_t), allocatable   :: homog_input(:)
    namelist /homog_input_nml/   homog_input
    ierr = 0
    ! set default values for homogeneous number structure
    homog_count%n_ic1   = 0
    homog_count%n_ic2   = 0
    homog_count%n_ic3   = 0
    homog_count%n_ic4   = 0
    homog_count%n_ic5   = 0
    homog_count%n_mdn   = 0
    homog_count%n_mth   = 0
    homog_count%n_mvs   = 0
    homog_count%n_lev   = 0
    homog_count%n_cur   = 0
    homog_count%n_wnd   = 0
    homog_count%n_ice   = 0
    homog_count%n_tau   = 0
    homog_count%n_rho   = 0
    homog_count%n_mov   = 0
    homog_count%n_tot   = 0
    ! read homogeneous count namelist
    rewind (ndsi)
    read (ndsi, nml=homog_count_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_homogeneous_nml: namelist homog_count read error', &
           'error: '//trim(msg)
      call extcde (6)
    end if
    ! allocate the total count of homogeneous input
    homog_count%n_tot = homog_count%n_ic1 + homog_count%n_ic2 + homog_count%n_ic3 + homog_count%n_ic4 + homog_count%n_ic5 + &
         homog_count%n_mdn + homog_count%n_mth + homog_count%n_mvs + homog_count%n_lev + homog_count%n_cur + &
         homog_count%n_wnd + homog_count%n_ice + homog_count%n_tau + homog_count%n_rho + homog_count%n_mov
    allocate(homog_input(homog_count%n_tot))
    allocate(nml_homog_input(homog_count%n_tot))
    ! set default values for homogeneous structure
    if (homog_count%n_tot .ne. 0 ) then
      do i=1,homog_count%n_tot
        homog_input(i)%name      = 'unset'
        homog_input(i)%date      = '19680606 000000'
        homog_input(i)%value1    = 0.
        homog_input(i)%value2    = 0.
        homog_input(i)%value3    = 0.
      end do
    end if
    ! read homogeneous input namelist
    rewind (ndsi)
    read (ndsi, nml=homog_input_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_homogeneous_nml: namelist homog_input_nml read error', &
           'error: '//trim(msg)
      call extcde (7)
    end if
    ! save namelist
    nml_homog_count = homog_count
    nml_homog_input = homog_input
  end subroutine read_homogeneous_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_domain_nml (nml_domain)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-apr-2018 |
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
    !      nml_domain      type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_domain_t), intent(in) :: nml_domain
    write (msg,'(a)') 'domain % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'iostyp = ', nml_domain%iostyp
    write (ndsn,10) trim(msg),'start  = ', trim(nml_domain%start)
    write (ndsn,10) trim(msg),'stop   = ', trim(nml_domain%stop)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
  end subroutine report_domain_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_input_nml (nml_input)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
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
    !      nml_input         type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_input_t), intent(in) :: nml_input
    ! locals
    write (msg,'(a)') 'input grid % :'
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'forcing % water_levels   = ', nml_input%forcing%water_levels
    write (ndsn,10) trim(msg),'forcing % currents       = ', nml_input%forcing%currents
    write (ndsn,10) trim(msg),'forcing % winds          = ', nml_input%forcing%winds
    write (ndsn,10) trim(msg),'forcing % atm_momentum   = ', nml_input%forcing%atm_momentum
    write (ndsn,10) trim(msg),'forcing % air_density    = ', nml_input%forcing%air_density
    write (ndsn,10) trim(msg),'forcing % ice_conc       = ', nml_input%forcing%ice_conc
    write (ndsn,10) trim(msg),'forcing % ice_param1     = ', nml_input%forcing%ice_param1
    write (ndsn,10) trim(msg),'forcing % ice_param2     = ', nml_input%forcing%ice_param2
    write (ndsn,10) trim(msg),'forcing % ice_param3     = ', nml_input%forcing%ice_param3
    write (ndsn,10) trim(msg),'forcing % ice_param4     = ', nml_input%forcing%ice_param4
    write (ndsn,10) trim(msg),'forcing % ice_param5     = ', nml_input%forcing%ice_param5
    write (ndsn,10) trim(msg),'forcing % mud_density    = ', nml_input%forcing%mud_density
    write (ndsn,10) trim(msg),'forcing % mud_thickness  = ', nml_input%forcing%mud_thickness
    write (ndsn,10) trim(msg),'forcing % mud_viscosity  = ', nml_input%forcing%mud_viscosity
    write (ndsn,10) trim(msg),'assim % mean             = ', nml_input%assim%mean
    write (ndsn,10) trim(msg),'assim % spec1d           = ', nml_input%assim%spec1d
    write (ndsn,10) trim(msg),'assim % spec2d           = ', nml_input%assim%spec2d
10  format (a,2x,a,a)
13  format (a,2x,a,l1)
  end subroutine report_input_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_output_type_nml (nml_output_type)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-sep-2020 |
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
    !      nml_output_type   type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_output_type_t), intent(in) :: nml_output_type
    ! locals
    write (msg,'(a)') 'output type % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'field % list         = ', trim(nml_output_type%field%list)
    write (ndsn,10) trim(msg),'point % file         = ', trim(nml_output_type%point%file)
    write (ndsn,13) trim(msg),'track % format       = ', nml_output_type%track%format
    write (ndsn,11) trim(msg),'partition % x0       = ', nml_output_type%partition%x0
    write (ndsn,11) trim(msg),'partition % xn       = ', nml_output_type%partition%xn
    write (ndsn,11) trim(msg),'partition % nx       = ', nml_output_type%partition%nx
    write (ndsn,11) trim(msg),'partition % y0       = ', nml_output_type%partition%y0
    write (ndsn,11) trim(msg),'partition % yn       = ', nml_output_type%partition%yn
    write (ndsn,11) trim(msg),'partition % ny       = ', nml_output_type%partition%ny
    write (ndsn,13) trim(msg),'partition % format   = ', nml_output_type%partition%format
    write (ndsn,10) trim(msg),'restart % extra      = ', trim(nml_output_type%restart%extra)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
  end subroutine report_output_type_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_output_date_nml (nml_output_date)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         18-apr-2018 |
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
    !      nml_output_date   type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_output_date_t), intent(in) :: nml_output_date
    ! locals
    write (msg,'(a)') 'output date model grid % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'field % start        = ', trim(nml_output_date%field%start)
    write (ndsn,10) trim(msg),'field % stride       = ', trim(nml_output_date%field%stride)
    write (ndsn,10) trim(msg),'field % stop         = ', trim(nml_output_date%field%stop)
    write (ndsn,10) trim(msg),'point % start        = ', trim(nml_output_date%point%start)
    write (ndsn,10) trim(msg),'point % stride       = ', trim(nml_output_date%point%stride)
    write (ndsn,10) trim(msg),'point % stop         = ', trim(nml_output_date%point%stop)
    write (ndsn,10) trim(msg),'track % start        = ', trim(nml_output_date%track%start)
    write (ndsn,10) trim(msg),'track % stride       = ', trim(nml_output_date%track%stride)
    write (ndsn,10) trim(msg),'track % stop         = ', trim(nml_output_date%track%stop)
    write (ndsn,10) trim(msg),'restart % start      = ', trim(nml_output_date%restart%start)
    write (ndsn,10) trim(msg),'restart % stride     = ', trim(nml_output_date%restart%stride)
    write (ndsn,10) trim(msg),'restart % stop       = ', trim(nml_output_date%restart%stop)
    write (ndsn,10) trim(msg),'restart2 % start      = ', trim(nml_output_date%restart2%start)
    write (ndsn,10) trim(msg),'restart2 % stride     = ', trim(nml_output_date%restart2%stride)
    write (ndsn,10) trim(msg),'restart2 % stop       = ', trim(nml_output_date%restart2%stop)
    write (ndsn,10) trim(msg),'boundary % start     = ', trim(nml_output_date%boundary%start)
    write (ndsn,10) trim(msg),'boundary % stride    = ', trim(nml_output_date%boundary%stride)
    write (ndsn,10) trim(msg),'boundary % stop      = ', trim(nml_output_date%boundary%stop)
    write (ndsn,10) trim(msg),'partition % start    = ', trim(nml_output_date%partition%start)
    write (ndsn,10) trim(msg),'partition % stride   = ', trim(nml_output_date%partition%stride)
    write (ndsn,10) trim(msg),'partition % stop     = ', trim(nml_output_date%partition%stop)
10  format (a,2x,a,a)
  end subroutine report_output_date_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_homogeneous_nml (nml_homog_count, nml_homog_input)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                                   |
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
    !      nml_homog_count   type.
    !      nml_homog_input   type.
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
    !      w3nmlshel subr.   n/a    namelist configuration routine.
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
    !     !/mpi  uses mpi communications
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(nml_homog_count_t), intent(in)  :: nml_homog_count
    type(nml_homog_input_t), intent(in) :: nml_homog_input(nml_homog_count%n_tot)
    ! locals
    integer              :: i
    write (msg,'(a)') 'homog_count % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'n_ic1       = ', nml_homog_count%n_ic1
    write (ndsn,11) trim(msg),'n_ic2       = ', nml_homog_count%n_ic2
    write (ndsn,11) trim(msg),'n_ic3       = ', nml_homog_count%n_ic3
    write (ndsn,11) trim(msg),'n_ic4       = ', nml_homog_count%n_ic4
    write (ndsn,11) trim(msg),'n_ic5       = ', nml_homog_count%n_ic5
    write (ndsn,11) trim(msg),'n_mdn       = ', nml_homog_count%n_mdn
    write (ndsn,11) trim(msg),'n_mth       = ', nml_homog_count%n_mth
    write (ndsn,11) trim(msg),'n_mvs       = ', nml_homog_count%n_mvs
    write (ndsn,11) trim(msg),'n_lev       = ', nml_homog_count%n_lev
    write (ndsn,11) trim(msg),'n_cur       = ', nml_homog_count%n_cur
    write (ndsn,11) trim(msg),'n_wnd       = ', nml_homog_count%n_wnd
    write (ndsn,11) trim(msg),'n_ice       = ', nml_homog_count%n_ice
    write (ndsn,11) trim(msg),'n_tau       = ', nml_homog_count%n_tau
    write (ndsn,11) trim(msg),'n_rho       = ', nml_homog_count%n_rho
    write (ndsn,11) trim(msg),'n_mov       = ', nml_homog_count%n_mov
    if (nml_homog_count%n_tot .ne. 0) then
      do i=1,nml_homog_count%n_tot
        write (msg,'(a,i5,a)') 'homog_input(',i,') % '
        write (ndsn,'(a)')
        write (ndsn,10) trim(msg),'name      = ', trim(nml_homog_input(i)%name)
        write (ndsn,10) trim(msg),'date      = ', trim(nml_homog_input(i)%date)
        write (ndsn,14) trim(msg),'value1    = ', nml_homog_input(i)%value1
        write (ndsn,14) trim(msg),'value2    = ', nml_homog_input(i)%value2
        write (ndsn,14) trim(msg),'value3    = ', nml_homog_input(i)%value3
        write (ndsn,'(a)')
      end do
    end if
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
14  format (a,2x,a,f8.2)
  end subroutine report_homogeneous_nml
  !/ ------------------------------------------------------------------- /
end module w3nmlshelmd
!/ ------------------------------------------------------------------- /
