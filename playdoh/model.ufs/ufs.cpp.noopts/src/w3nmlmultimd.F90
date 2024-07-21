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
module w3nmlmultimd
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
  !     manages namelists from configuration file ww3_multi.nml for ww3_multi program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! domain definition structure
  type nml_domain_t
    integer                     :: nrinp
    integer                     :: nrgrd
    logical                     :: unipts
    integer                     :: iostyp
    logical                     :: upproc
    logical                     :: pshare
    logical                     :: flghg1
    logical                     :: flghg2
    character(15)               :: start
    character(15)               :: stop
  end type nml_domain_t
  ! model grid data structure
  type nml_model_forcing_t
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
  end type nml_model_forcing_t
  !
  type nml_model_assim_t
    character(13)               :: mean
    character(13)               :: spec1d
    character(13)               :: spec2d
  end type nml_model_assim_t
  !
  type nml_model_resource_t
    integer                     :: rank_id
    integer                     :: group_id
    real(4)                     :: comm_frac(2)
    logical                     :: bound_flag
  end type nml_model_resource_t
  !
  type nml_model_grid_t
    character(13)               :: name
    type(nml_model_forcing_t)   :: forcing
    type(nml_model_assim_t)     :: assim
    type(nml_model_resource_t)  :: resource
  end type nml_model_grid_t
  ! input grid data structure
  type nml_input_forcing_t
    logical                     :: water_levels
    logical                     :: currents
    logical                     :: winds
    logical                     :: atm_momentum
    logical                     :: air_density
    logical                     :: ice_conc
    logical                     :: ice_param1
    logical                     :: ice_param2
    logical                     :: ice_param3
    logical                     :: ice_param4
    logical                     :: ice_param5
    logical                     :: mud_density
    logical                     :: mud_thickness
    logical                     :: mud_viscosity
  end type nml_input_forcing_t
  !
  type nml_input_assim_t
    logical                     :: mean
    logical                     :: spec1d
    logical                     :: spec2d
  end type nml_input_assim_t
  !
  type nml_input_grid_t
    character(13)               :: name
    type(nml_input_forcing_t)   :: forcing
    type(nml_input_assim_t)     :: assim
  end type nml_input_grid_t
  ! output type structure
  type nml_field_t
    character(1024)             :: list
  end type nml_field_t
  !
  type nml_point_t
    character(13)               :: name
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
    type(nml_point_t)           :: point
    type(nml_field_t)           :: field
    type(nml_track_t)           :: track
    type(nml_partition_t)       :: partition
    type(nml_restart_t)         :: restart
  end type nml_output_type_t
  ! output date structure
  type nml_output_time_t
    character(15)               :: start
    character(15)               :: stride
    character(15)               :: stop
    character(1)                :: outffile
    !
  end type nml_output_time_t
  !
  type nml_output_date_t
    type(nml_output_time_t)     :: field
    type(nml_output_time_t)     :: point
    type(nml_output_time_t)     :: track
    type(nml_output_time_t)     :: restart
    type(nml_output_time_t)     :: restart2
    type(nml_output_time_t)     :: boundary
    type(nml_output_time_t)     :: partition
  end type nml_output_date_t
  ! homogeneous input structure
  type nml_homog_count_t
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
  subroutine w3nmlmultidef (mpi_comm, ndsi, infile, nml_domain, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !
    !  1. purpose :
    !
    !     reads the domain definition namelist to define the number of
    !     model and forcing grids
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !      mpi_comm   int.  public   communicator used in the wave model.
    !      ndsi       int.
    !      infile     char.
    !      nml_domain type.
    !      ierr       int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_domain_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      wminitnml subr.   n/a    model configuration routine.
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
    type(nml_domain_t), intent(out)           :: nml_domain
    integer, intent(out)                      :: ierr
    ! locals
    integer                                 :: ierr_mpi
    ierr = 0
    mpi_comm_mwave = mpi_comm
    call mpi_comm_rank ( mpi_comm_mwave, improc, ierr_mpi )
    improc = improc + 1
    ! open namelist log file
    if ( nmplog .eq. improc ) then
      ndsn = 3
      open (ndsn, file=trim(infile)//'.log', form='formatted', iostat=ierr)
      if (ierr.ne.0) then
        write (mdse,'(a)') 'error: open full nml file '//trim(infile)//'.log failed'
        return
      end if
    end if
    ! open input file
    open (ndsi, file=trim(infile), form='formatted', status='old', iostat=ierr)
    if (ierr.ne.0) then
      write (mdse,'(a)') 'error: open input file '//trim(infile)//' failed'
      return
    end if
    ! read domain def namelist
    call read_domain_nml (ndsi, nml_domain)
    if ( nmplog .eq. improc ) call report_domain_nml (nml_domain)
    ! close namelist files
    close (ndsi)
    if ( nmplog .eq. improc ) close (ndsn)
  end subroutine w3nmlmultidef
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlmulticonf (mpi_comm, ndsi, infile, nml_domain,       &
       nml_input_grid, nml_model_grid,           &
       nml_output_type, nml_output_date,         &
       nml_homog_count, nml_homog_input, ierr)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !  1. purpose :
    !
    !     reads all the namelist to define the multi grid
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
    !      nml_input_grid
    !      nml_model_grid
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
    !      read_input_grid_nml
    !      report_input_grid_nml
    !      read_model_grid_nml
    !      report_model_grid_nml
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
    !      wminitnml subr.   n/a    model configuration routine.
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
    type(nml_input_grid_t), intent(inout)     :: nml_input_grid(:)
    type(nml_model_grid_t), intent(inout)     :: nml_model_grid(:)
    type(nml_output_type_t), intent(inout)    :: nml_output_type(:)
    type(nml_output_date_t), intent(inout)    :: nml_output_date(:)
    type(nml_homog_count_t), intent(inout)    :: nml_homog_count
    type(nml_homog_input_t), allocatable, intent(inout)    :: nml_homog_input(:)
    integer, intent(out)                      :: ierr
    ! locals
    integer                                 :: ierr_mpi
    ierr = 0
    mpi_comm_mwave = mpi_comm
    call mpi_comm_rank ( mpi_comm_mwave, improc, ierr_mpi )
    improc = improc + 1
    ! open namelist log file
    if ( nmplog .eq. improc ) then
      ndsn = 3
      open (ndsn, file=trim(infile)//'.log', form='formatted', status='old', iostat=ierr)
      if (ierr.ne.0) then
        write (mdse,'(a)') 'error: open full nml file '//trim(infile)//'.log failed'
        return
      end if
    end if
    ! open input file
    open (ndsi, file=trim(infile), form='formatted', status='old', iostat=ierr)
    if (ierr.ne.0) then
      write (mdse,'(a)') 'error: open input file '//trim(infile)//' failed'
      return
    end if
    ! read input grid namelist
    call read_input_grid_nml (ndsi, nml_domain%nrinp, nml_input_grid)
    if ( nmplog .eq. improc ) call report_input_grid_nml (nml_domain%nrinp, nml_input_grid)
    ! read model grid namelist
    call read_model_grid_nml (ndsi, nml_domain%nrgrd, nml_model_grid)
    if ( nmplog .eq. improc ) call report_model_grid_nml (nml_domain%nrgrd, nml_model_grid)
    ! read output type namelist
    call read_output_type_nml (ndsi, nml_domain%nrgrd, nml_output_type)
    if ( nmplog .eq. improc ) call report_output_type_nml (nml_domain%nrgrd, nml_output_type)
    ! read output date namelist
    call read_output_date_nml (ndsi, nml_domain%nrgrd, nml_output_date)
    if ( nmplog .eq. improc ) call report_output_date_nml (nml_domain%nrgrd,nml_output_date)
    ! read homogeneous namelist
    call read_homogeneous_nml (ndsi, nml_homog_count, nml_homog_input)
    if ( nmplog .eq. improc ) call report_homogeneous_nml (nml_homog_count, nml_homog_input)
    ! close namelist files
    close (ndsi)
    if ( nmplog .eq. improc ) close (ndsn)
  end subroutine w3nmlmulticonf
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_domain_nml (ndsi, nml_domain)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |           f. ardhuin              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
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
    !      ndsi            int.
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
    !      w3nmlmultidef subr.   n/a    namelist configuration routine.
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
    integer                   :: ierr
    type(nml_domain_t) :: domain
    namelist /domain_nml/ domain
    ierr = 0
    ! set default values for model definition data
    domain%nrinp  = 0
    domain%nrgrd  = 1
    domain%unipts = .false.
    domain%iostyp = 1
    domain%upproc = .false.
    domain%pshare = .false.
    domain%flghg1 = .false.
    domain%flghg2 = .false.
    domain%start  = '19680606 000000'
    domain%stop   = '19680607 000000'
    ! read model def namelist
    rewind (ndsi)
    read (ndsi, nml=domain_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_domain_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! set/check return values
    if (domain%nrinp.lt.0) then
      write (mdse,'(a,i3)') 'error: bad nrinp input: ',domain%nrinp
      call extcde (2)
    end if
    if (domain%nrgrd.le.0) then
      write (mdse,'(a,i3)') 'error: bad nrgrd input: ',domain%nrgrd
      call extcde (3)
    end if
    if (domain%iostyp.lt.0.or.domain%iostyp.gt.3) then
      write (mdse,'(a,i3)') 'error: bad iostyp input: ',domain%iostyp
      call extcde (4)
    end if
    ! save namelist
    nml_domain = domain
  end subroutine read_domain_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_input_grid_nml (ndsi, nrinp, nml_input_grid)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !/    22-mar-2021 : update namelist, new input fields   ( version 7.13 )
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
    !      nrinp             int.
    !      nml_input_grid    type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)               :: ndsi, nrinp
    type(nml_input_grid_t), intent(inout) :: nml_input_grid(nrinp)
    ! locals
    integer                   :: ierr, i
    integer, parameter :: max_nrinp = 99
    type(nml_input_grid_t) :: input(max_nrinp)
    namelist /input_grid_nml/ input
    ierr = 0
    ! test nrinp
    if (nrinp.gt.max_nrinp) then
      write (mdse,'(a,/a,i6,/a,i6)') &
           'error: read_input_grid_nml: nrinp > max_nrinp',      &
           'error: read_input_grid_nml:     nrinp = ',    nrinp, &
           'error: read_input_grid_nml: max_nrinp = ',max_nrinp
      call extcde (6)
    end if
    ! if no input grids, then exit
    if (nrinp.eq.0) return
    ! set default values for grid input data
    do i = 1,nrinp
      input(i)%name                  = 'unset'
      input(i)%forcing%water_levels  = .false.
      input(i)%forcing%currents      = .false.
      input(i)%forcing%winds         = .false.
      input(i)%forcing%atm_momentum  = .false.
      input(i)%forcing%air_density   = .false.
      input(i)%forcing%ice_conc      = .false.
      input(i)%forcing%ice_param1    = .false.
      input(i)%forcing%ice_param2    = .false.
      input(i)%forcing%ice_param3    = .false.
      input(i)%forcing%ice_param4    = .false.
      input(i)%forcing%ice_param5    = .false.
      input(i)%forcing%mud_density   = .false.
      input(i)%forcing%mud_thickness = .false.
      input(i)%forcing%mud_viscosity = .false.
      input(i)%assim%mean            = .false.
      input(i)%assim%spec1d          = .false.
      input(i)%assim%spec2d          = .false.
    end do
    ! read input grid namelist
    rewind (ndsi)
    read (ndsi, nml=input_grid_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_input_grid_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (7)
    end if
    ! set/check return values
    do i = 1,nrinp
      if (trim(input(i)%name).eq.'unset') then
        write (mdse,10) 'error: read_input_grid_nml: required input(',i,')%name not set'
        call extcde (8)
      end if
    end do
    ! save namelist
    nml_input_grid = input(1:nrinp)
10  format (a,i0,a)
  end subroutine read_input_grid_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_model_grid_nml (ndsi, nrgrd, nml_model_grid)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !/    22-mar-2021 : update namelist, new input fields   ( version 7.13 )
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
    !      nrgrd             int.
    !      nml_model_grid    type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)               :: ndsi, nrgrd
    type(nml_model_grid_t), intent(inout) :: nml_model_grid(nrgrd)
    ! locals
    integer                   :: ierr, i
    integer, parameter :: max_nrgrd = 99
    type(nml_model_grid_t) :: model(max_nrgrd)
    namelist /model_grid_nml/ model
    ierr = 0
    ! test nrgrd
    if (nrgrd.gt.max_nrgrd) then
      write (mdse,'(a,/a,i6,/a,i6)') &
           'error: read_model_grid_nml: nrgrd > max_nrgrd',      &
           'error: read_model_grid_nml:     nrgrd = ',    nrgrd, &
           'error: read_model_grid_nml: max_nrgrd = ',max_nrgrd
      call extcde (9)
    end if
    ! if no model grids, then exit
    if (nrgrd.eq.0) return
    ! set default values for model input data
    do i = 1,nrgrd
      model(i)%name                  = 'unset'
      model(i)%forcing%water_levels  = 'no'
      model(i)%forcing%currents      = 'no'
      model(i)%forcing%winds         = 'no'
      model(i)%forcing%atm_momentum  = 'no'
      model(i)%forcing%air_density   = 'no'
      model(i)%forcing%ice_conc      = 'no'
      model(i)%forcing%ice_param1    = 'no'
      model(i)%forcing%ice_param2    = 'no'
      model(i)%forcing%ice_param3    = 'no'
      model(i)%forcing%ice_param4    = 'no'
      model(i)%forcing%ice_param5    = 'no'
      model(i)%forcing%mud_density   = 'no'
      model(i)%forcing%mud_thickness = 'no'
      model(i)%forcing%mud_viscosity = 'no'
      model(i)%assim%mean            = 'no'
      model(i)%assim%spec1d          = 'no'
      model(i)%assim%spec2d          = 'no'
      model(i)%resource%rank_id      = i
      model(i)%resource%group_id     = 1
      model(i)%resource%comm_frac    = (/0.00,1.00/)
      model(i)%resource%bound_flag   = .false.
    end do
    ! read model namelist
    rewind (ndsi)
    read (ndsi, nml=model_grid_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (mdse,'(a,/a)') &
           'error: read_model_grid_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (10)
    end if
    ! set/check return values
    do i = 1,nrgrd
      if (trim(model(i)%name).eq.'unset') then
        write (mdse,10) 'error: read_model_grid_nml: required model(',i,')%name not set'
        call extcde (11)
      end if
    end do
    ! save namelist
    nml_model_grid = model(1:nrgrd)
10  format (a,i0,a)
  end subroutine read_model_grid_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_output_type_nml (ndsi, nrgrd, nml_output_type)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-sep-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !/    25-sep-2020 : update namelist                     ( version 7.10 )
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
    !      nrgrd             int.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                :: ndsi, nrgrd
    type(nml_output_type_t), intent(inout) :: nml_output_type(nrgrd)
    ! locals
    integer                   :: ierr, i
    integer, parameter        :: max_nrgrd = 99
    type(nml_output_type_t)   :: alltype
    type(nml_output_type_t)   :: itype(max_nrgrd)
    namelist /output_type_nml/ alltype, itype
    ierr = 0
    ! if no model grids, then exit
    if (nrgrd.eq.0) return
    ! set default values for output type data
    do i=1,nrgrd
      itype(i)%field%list   = 'unset'
      itype(i)%point%name   = 'unset'
      itype(i)%point%file   = 'points.list'
      itype(i)%track%format = .true.
      itype(i)%partition%x0 = 0
      itype(i)%partition%xn = 0
      itype(i)%partition%nx = 0
      itype(i)%partition%y0 = 0
      itype(i)%partition%yn = 0
      itype(i)%partition%ny = 0
      itype(i)%partition%format = .true.
      itype(i)%restart%extra = 'unset'
    end do
    alltype%field%list   = 'unset'
    alltype%point%name   = 'unset'
    alltype%point%file   = 'points.list'
    alltype%track%format = .true.
    alltype%partition%x0 = 0
    alltype%partition%xn = 0
    alltype%partition%nx = 0
    alltype%partition%y0 = 0
    alltype%partition%yn = 0
    alltype%partition%ny = 0
    alltype%partition%format = .true.
    alltype%restart%extra = 'unset'
    ! read output_type namelist
    rewind (ndsi)
    read (ndsi, nml=output_type_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_output_type_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (12)
    end if
    ! propagate general type on all grids
    do i=1,nrgrd
      itype(i) = alltype
    end do
    ! read output_type namelist
    rewind (ndsi)
    read (ndsi, nml=output_type_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_output_type_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (13)
    end if
    ! save namelist
    nml_output_type = itype(1:nrgrd)
  end subroutine read_output_type_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_output_date_nml (ndsi, nrgrd, nml_output_date)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
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
    !      nrgrd             int.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                :: ndsi, nrgrd
    type(nml_output_date_t), intent(inout) :: nml_output_date(nrgrd)
    ! locals
    integer                   :: ierr, i
    integer, parameter        :: max_nrgrd = 99
    type(nml_output_date_t)   :: alldate
    type(nml_output_date_t)   :: idate(max_nrgrd)
    namelist /output_date_nml/ alldate, idate
    ierr = 0
    ! if no model grids, then exit
    if (nrgrd.eq.0) return
    ! set default values for output_date input data
    do i=1,nrgrd
      idate(i)%field%start = '19680606 000000'
      idate(i)%field%stride = '0'
      idate(i)%field%stop = '19680607 000000'
      idate(i)%field%outffile = '0'
      idate(i)%point%outffile = '0'
      idate(i)%point%start = '19680606 000000'
      idate(i)%point%stride = '0'
      idate(i)%point%stop = '19680607 000000'
      idate(i)%track%start = '19680606 000000'
      idate(i)%track%stride = '0'
      idate(i)%track%stop = '19680607 000000'
      idate(i)%restart%start = '19680606 000000'
      idate(i)%restart%stride = '0'
      idate(i)%restart%stop = '19680607 000000'
      idate(i)%restart2%start = '19680606 000000'
      idate(i)%restart2%stride = '0'
      idate(i)%restart2%stop = '19680607 000000'
      idate(i)%boundary%start = '19680606 000000'
      idate(i)%boundary%stride = '0'
      idate(i)%boundary%stop = '19680607 000000'
      idate(i)%partition%start = '19680606 000000'
      idate(i)%partition%stride = '0'
      idate(i)%partition%stop = '19680607 000000'
    end do
    alldate%field%start = '19680606 000000'
    alldate%field%stride = '0'
    alldate%field%stop = '19680607 000000'
    alldate%field%outffile = '0'
    alldate%point%start = '19680606 000000'
    alldate%point%stride = '0'
    alldate%point%stop = '19680607 000000'
    alldate%point%outffile = '0'
    alldate%track%start = '19680606 000000'
    alldate%track%stride = '0'
    alldate%track%stop = '19680607 000000'
    alldate%restart%start = '19680606 000000'
    alldate%restart%stride = '0'
    alldate%restart%stop   = '19680607 000000'
    alldate%restart2%start  = '19680606 000000'
    alldate%restart2%stride = '0'
    alldate%restart2%stop   = '19680607 000000'
    alldate%boundary%start = '19680606 000000'
    alldate%boundary%stride = '0'
    alldate%boundary%stop = '19680607 000000'
    alldate%partition%start = '19680606 000000'
    alldate%partition%stride = '0'
    alldate%partition%stop = '19680607 000000'
    ! read output_date namelist
    rewind (ndsi)
    read (ndsi, nml=output_date_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_output_date_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (14)
    end if
    ! propagate general date to all grids
    do i=1,nrgrd
      idate(i) = alldate
    end do
    ! read output_date namelist
    rewind (ndsi)
    read (ndsi, nml=output_date_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_output_date_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (15)
    end if
    ! save namelist
    nml_output_date = idate(1:nrgrd)
  end subroutine read_output_date_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_homogeneous_nml (ndsi, nml_homog_count, nml_homog_input)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
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
    !      ndsi               int.
    !      nml_homog_count    type.
    !      nml_homog_input    type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                      :: ndsi
    type(nml_homog_count_t), intent(out)  :: nml_homog_count
    type(nml_homog_input_t), allocatable, intent(out)   :: nml_homog_input(:)
    ! locals
    integer                   :: ierr, i
    type(nml_homog_count_t)  :: homog_count
    namelist /homog_count_nml/  homog_count
    type(nml_homog_input_t), allocatable   :: homog_input(:)
    namelist /homog_input_nml/   homog_input
    ierr = 0
    ! set default values for homogeneous number structure
    homog_count%n_mov   = 0
    homog_count%n_tot   = 0
    ! read homogeneous count namelist
    rewind (ndsi)
    read (ndsi, nml=homog_count_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (mdse,'(a,/a)') &
           'error: read_homogeneous_nml: namelist homog_count read error', &
           'error: '//trim(msg)
      call extcde (16)
    end if
    ! allocate the total count of homogeneous input
    homog_count%n_tot = homog_count%n_mov
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
      call extcde (17)
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
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
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
    !      nml_domain       type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    write (ndsn,11) trim(msg),'nrinp  = ', nml_domain%nrinp
    write (ndsn,11) trim(msg),'nrgrd  = ', nml_domain%nrgrd
    write (ndsn,13) trim(msg),'unipts = ', nml_domain%unipts
    write (ndsn,11) trim(msg),'iostyp = ', nml_domain%iostyp
    write (ndsn,13) trim(msg),'upproc = ', nml_domain%upproc
    write (ndsn,13) trim(msg),'pshare = ', nml_domain%pshare
    write (ndsn,13) trim(msg),'flghg1 = ', nml_domain%flghg1
    write (ndsn,13) trim(msg),'flghg2 = ', nml_domain%flghg2
    write (ndsn,10) trim(msg),'start  = ', trim(nml_domain%start)
    write (ndsn,10) trim(msg),'stop   = ', trim(nml_domain%stop)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
  end subroutine report_domain_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_input_grid_nml (nrinp, nml_input_grid)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !/    22-mar-2021 : update namelist, new input fields   ( version 7.13 )
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
    !      nrinp             int.
    !      nml_input_grid    type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)            :: nrinp
    type(nml_input_grid_t), intent(in) :: nml_input_grid(nrinp)
    ! locals
    integer              :: i
    do i = 1,nrinp
      write (msg,'(a,i0.2,a)') 'input grid ',i,' % '
      write (ndsn,'(a)')
      write (ndsn,10) trim(msg),'name                     = ', trim(nml_input_grid(i)%name)
      write (ndsn,13) trim(msg),'forcing % water_levels   = ', nml_input_grid(i)%forcing%water_levels
      write (ndsn,13) trim(msg),'forcing % currents       = ', nml_input_grid(i)%forcing%currents
      write (ndsn,13) trim(msg),'forcing % winds          = ', nml_input_grid(i)%forcing%winds
      write (ndsn,13) trim(msg),'forcing % atm_momentum   = ', nml_input_grid(i)%forcing%atm_momentum
      write (ndsn,13) trim(msg),'forcing % air_density    = ', nml_input_grid(i)%forcing%air_density
      write (ndsn,13) trim(msg),'forcing % ice_conc       = ', nml_input_grid(i)%forcing%ice_conc
      write (ndsn,13) trim(msg),'forcing % ice_param1     = ', nml_input_grid(i)%forcing%ice_param1
      write (ndsn,13) trim(msg),'forcing % ice_param2     = ', nml_input_grid(i)%forcing%ice_param2
      write (ndsn,13) trim(msg),'forcing % ice_param3     = ', nml_input_grid(i)%forcing%ice_param3
      write (ndsn,13) trim(msg),'forcing % ice_param4     = ', nml_input_grid(i)%forcing%ice_param4
      write (ndsn,13) trim(msg),'forcing % ice_param5     = ', nml_input_grid(i)%forcing%ice_param5
      write (ndsn,13) trim(msg),'forcing % mud_density    = ', nml_input_grid(i)%forcing%mud_density
      write (ndsn,13) trim(msg),'forcing % mud_thickness  = ', nml_input_grid(i)%forcing%mud_thickness
      write (ndsn,13) trim(msg),'forcing % mud_viscosity  = ', nml_input_grid(i)%forcing%mud_viscosity
      write (ndsn,13) trim(msg),'assim % mean             = ', nml_input_grid(i)%assim%mean
      write (ndsn,13) trim(msg),'assim % spec1d           = ', nml_input_grid(i)%assim%spec1d
      write (ndsn,13) trim(msg),'assim % spec2d           = ', nml_input_grid(i)%assim%spec2d
    end do
    write (*,*)
10  format (a,2x,a,a)
13  format (a,2x,a,l1)
  end subroutine report_input_grid_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_model_grid_nml (nrgrd, nml_model_grid)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !/    22-mar-2021 : update namelist, new input fields   ( version 7.13 )
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
    !      nrgrd             int.
    !      nml_model_grid    type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)            :: nrgrd
    type(nml_model_grid_t), intent(in) :: nml_model_grid(nrgrd)
    ! locals
    integer              :: i
    do i = 1,nrgrd
      write (msg,'(a,i0.4,a)') 'model grid ',i,' % '
      write (ndsn,'(a)')
      write (ndsn,10) trim(msg),'name                     = ', trim(nml_model_grid(i)%name)
      write (ndsn,10) trim(msg),'forcing % water_levels   = ', trim(nml_model_grid(i)%forcing%water_levels)
      write (ndsn,10) trim(msg),'forcing % currents       = ', trim(nml_model_grid(i)%forcing%currents)
      write (ndsn,10) trim(msg),'forcing % winds          = ', trim(nml_model_grid(i)%forcing%winds)
      write (ndsn,10) trim(msg),'forcing % atm_momentum   = ', trim(nml_model_grid(i)%forcing%atm_momentum)
      write (ndsn,10) trim(msg),'forcing % air_density    = ', trim(nml_model_grid(i)%forcing%air_density)
      write (ndsn,10) trim(msg),'forcing % ice_conc       = ', trim(nml_model_grid(i)%forcing%ice_conc)
      write (ndsn,10) trim(msg),'forcing % ice_param1     = ', trim(nml_model_grid(i)%forcing%ice_param1)
      write (ndsn,10) trim(msg),'forcing % ice_param2     = ', trim(nml_model_grid(i)%forcing%ice_param2)
      write (ndsn,10) trim(msg),'forcing % ice_param3     = ', trim(nml_model_grid(i)%forcing%ice_param3)
      write (ndsn,10) trim(msg),'forcing % ice_param4     = ', trim(nml_model_grid(i)%forcing%ice_param4)
      write (ndsn,10) trim(msg),'forcing % ice_param5     = ', trim(nml_model_grid(i)%forcing%ice_param5)
      write (ndsn,10) trim(msg),'forcing % mud_density    = ', trim(nml_model_grid(i)%forcing%mud_density)
      write (ndsn,10) trim(msg),'forcing % mud_thickness  = ', trim(nml_model_grid(i)%forcing%mud_thickness)
      write (ndsn,10) trim(msg),'forcing % mud_viscosity  = ', trim(nml_model_grid(i)%forcing%mud_viscosity)
      write (ndsn,10) trim(msg),'assim % mean             = ', trim(nml_model_grid(i)%assim%mean)
      write (ndsn,10) trim(msg),'assim % spec1d           = ', trim(nml_model_grid(i)%assim%spec1d)
      write (ndsn,10) trim(msg),'assim % spec2d           = ', trim(nml_model_grid(i)%assim%spec2d)
      write (ndsn,11) trim(msg),'resource % rank_id       = ', nml_model_grid(i)%resource%rank_id
      write (ndsn,11) trim(msg),'resource % group_id      = ', nml_model_grid(i)%resource%group_id
      write (ndsn,12) trim(msg),'resource % comm_frac     = ', nml_model_grid(i)%resource%comm_frac(1), &
           nml_model_grid(i)%resource%comm_frac(2)
      write (ndsn,13) trim(msg),'resource % bound_flag    = ', nml_model_grid(i)%resource%bound_flag
    end do
    write (ndsn,'(a)')
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
12  format (a,2x,a,'(',f5.2,',',f5.2,' )')
13  format (a,2x,a,l1)
  end subroutine report_model_grid_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_output_type_nml (nrgrd, nml_output_type)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-sep-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
    !/    25-sep-2020 : update namelist                     ( version 7.10 )
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
    !      nrgrd             int.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)             :: nrgrd
    type(nml_output_type_t), intent(in) :: nml_output_type(nrgrd)
    ! locals
    integer              :: i
    do i=1,nrgrd
      write (msg,'(a,i1,a)') 'output type model grid',i, ' % '
      write (ndsn,'(a)')
      write (ndsn,10) trim(msg),'field % list         = ', trim(nml_output_type(i)%field%list)
      write (ndsn,10) trim(msg),'point % name         = ', trim(nml_output_type(i)%point%name)
      write (ndsn,10) trim(msg),'point % file         = ', trim(nml_output_type(i)%point%file)
      write (ndsn,13) trim(msg),'track % format       = ', nml_output_type(i)%track%format
      write (ndsn,11) trim(msg),'partition % x0       = ', nml_output_type(i)%partition%x0
      write (ndsn,11) trim(msg),'partition % xn       = ', nml_output_type(i)%partition%xn
      write (ndsn,11) trim(msg),'partition % nx       = ', nml_output_type(i)%partition%nx
      write (ndsn,11) trim(msg),'partition % y0       = ', nml_output_type(i)%partition%y0
      write (ndsn,11) trim(msg),'partition % yn       = ', nml_output_type(i)%partition%yn
      write (ndsn,11) trim(msg),'partition % ny       = ', nml_output_type(i)%partition%ny
      write (ndsn,13) trim(msg),'partition % format   = ', nml_output_type(i)%partition%format
      write (ndsn,10) trim(msg),'restart % extra      = ', trim(nml_output_type(i)%restart%extra)
    end do
    write (ndsn,'(a)')
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
  end subroutine report_output_type_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_output_date_nml (nrgrd, nml_output_date)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
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
    !      nrgrd             int.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
    integer, intent(in)             :: nrgrd
    type(nml_output_date_t), intent(in) :: nml_output_date(nrgrd)
    ! locals
    integer              :: i
    do i=1,nrgrd
      write (msg,'(a,i1,a)') 'output date model grid',i, ' % '
      write (ndsn,'(a)')
      write (ndsn,10) trim(msg),'field % start        = ', trim(nml_output_date(i)%field%start)
      write (ndsn,10) trim(msg),'field % stride       = ', trim(nml_output_date(i)%field%stride)
      write (ndsn,10) trim(msg),'field % stop         = ', trim(nml_output_date(i)%field%stop)
      write (ndsn,10) trim(msg),'point % start        = ', trim(nml_output_date(i)%point%start)
      write (ndsn,10) trim(msg),'point % stride       = ', trim(nml_output_date(i)%point%stride)
      write (ndsn,10) trim(msg),'point % stop         = ', trim(nml_output_date(i)%point%stop)
      write (ndsn,10) trim(msg),'track % start        = ', trim(nml_output_date(i)%track%start)
      write (ndsn,10) trim(msg),'track % stride       = ', trim(nml_output_date(i)%track%stride)
      write (ndsn,10) trim(msg),'track % stop         = ', trim(nml_output_date(i)%track%stop)
      write (ndsn,10) trim(msg),'restart % start      = ', trim(nml_output_date(i)%restart%start)
      write (ndsn,10) trim(msg),'restart % stride     = ', trim(nml_output_date(i)%restart%stride)
      write (ndsn,10) trim(msg),'restart % stop       = ', trim(nml_output_date(i)%restart%stop)
      write (ndsn,10) trim(msg),'restart2 % start     = ', trim(nml_output_date(i)%restart2%start)
      write (ndsn,10) trim(msg),'restart2 % stride    = ', trim(nml_output_date(i)%restart2%stride)
      write (ndsn,10) trim(msg),'restart2 % stop      = ', trim(nml_output_date(i)%restart2%stop)
      write (ndsn,10) trim(msg),'boundary % start     = ', trim(nml_output_date(i)%boundary%start)
      write (ndsn,10) trim(msg),'boundary % stride    = ', trim(nml_output_date(i)%boundary%stride)
      write (ndsn,10) trim(msg),'boundary % stop      = ', trim(nml_output_date(i)%boundary%stop)
      write (ndsn,10) trim(msg),'partition % start    = ', trim(nml_output_date(i)%partition%start)
      write (ndsn,10) trim(msg),'partition % stride   = ', trim(nml_output_date(i)%partition%stride)
      write (ndsn,10) trim(msg),'partition % stop     = ', trim(nml_output_date(i)%partition%stop)
    end do
    write (ndsn,'(a)')
10  format (a,2x,a,a)
  end subroutine report_output_date_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_homogeneous_nml (nml_homog_count, nml_homog_input)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           m. accensi              |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-may-2018 |
    !/                  +-----------------------------------+
    !/
    !/    09-aug-2016 : adding comments                     ( version 5.12 )
    !/    15-may-2018 : update namelist                     ( version 6.05 )
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
    !      nml_homog_count    type.
    !      nml_homog_input    type.
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
    !      w3nmlmulticonf subr.   n/a    namelist configuration routine.
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
end module w3nmlmultimd
!/ ------------------------------------------------------------------- /
