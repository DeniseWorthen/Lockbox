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
module w3nmlgridmd
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
  !     manages namelists from configuration file ww3_grid.nml for ww3_grid program
  !
  !/ ------------------------------------------------------------------- /
  ! module defaults
  implicit none
  public
  ! coord file structure
  type nml_coord_file_t
    real                        :: sf
    real                        :: off
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_coord_file_t
  ! file structure
  type nml_file_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_file_t
  ! smc file structure
  type nml_smc_file_t
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
  end type nml_smc_file_t
  ! spectrum structure
  type nml_spectrum_t
    real                        :: xfr
    real                        :: freq1
    integer                     :: nk
    integer                     :: nth
    real                        :: thoff
  end type nml_spectrum_t
  ! run structure
  type nml_run_t
    logical                     :: fldry
    logical                     :: flcx
    logical                     :: flcy
    logical                     :: flcth
    logical                     :: flck
    logical                     :: flsou
  end type nml_run_t
  ! timesteps structure
  type nml_timesteps_t
    real                        :: dtmax
    real                        :: dtxy
    real                        :: dtkth
    real                        :: dtmin
  end type nml_timesteps_t
  ! grid structure
  type nml_grid_t
    character(256)              :: name
    character(256)              :: nml
    character(256)              :: type
    character(256)              :: coord
    character(256)              :: clos
    real                        :: zlim
    real                        :: dmin
  end type nml_grid_t
  ! rect structure
  type nml_rect_t
    integer                     :: nx
    integer                     :: ny
    real                        :: sx
    real                        :: sy
    real                        :: sf
    real                        :: x0
    real                        :: y0
    real                        :: sf0
  end type nml_rect_t
  ! curv structure
  type nml_curv_t
    integer                     :: nx
    integer                     :: ny
    type(nml_coord_file_t)      :: xcoord
    type(nml_coord_file_t)      :: ycoord
  end type nml_curv_t
  ! unst structure
  type nml_unst_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: ugobcfile
  end type nml_unst_t
  ! smc structure
  type nml_smc_t
    type(nml_smc_file_t)        :: mcels
    type(nml_smc_file_t)        :: iside
    type(nml_smc_file_t)        :: jside
    type(nml_smc_file_t)        :: subtr
    type(nml_smc_file_t)        :: bundy
    type(nml_smc_file_t)        :: mbarc
    type(nml_smc_file_t)        :: aisid
    type(nml_smc_file_t)        :: ajsid
  end type nml_smc_t
  ! depth structure
  type nml_depth_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_depth_t
  ! mask structure
  type nml_mask_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_mask_t
  ! obst structure
  type nml_obst_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_obst_t
  ! slope structure
  type nml_slope_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_slope_t
  ! sed structure
  type nml_sed_t
    real                        :: sf
    character(256)              :: filename
    integer                     :: idf
    integer                     :: idla
    integer                     :: idfm
    character(256)              :: format
    character(256)              :: from
  end type nml_sed_t
  ! inbound structure
  type nml_inbnd_count_t
    integer                     :: n_point
  end type nml_inbnd_count_t
  type nml_inbnd_point_t
    integer                     :: x_index
    integer                     :: y_index
    logical                     :: connect
  end type nml_inbnd_point_t
  ! excluded structure
  type nml_excl_count_t
    integer                     :: n_point
    integer                     :: n_body
  end type nml_excl_count_t
  type nml_excl_point_t
    integer                     :: x_index
    integer                     :: y_index
    logical                     :: connect
  end type nml_excl_point_t
  type nml_excl_body_t
    integer                     :: x_index
    integer                     :: y_index
  end type nml_excl_body_t
  ! outbound structure
  type nml_outbnd_count_t
    integer                     :: n_line
  end type nml_outbnd_count_t
  type nml_outbnd_line_t
    real                        :: x0
    real                        :: y0
    real                        :: dx
    real                        :: dy
    integer                     :: np
  end type nml_outbnd_line_t
  ! miscellaneous
  character(256)                :: msg
  integer                       :: ndsn
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3nmlgrid (ndsi, infile, nml_spectrum, nml_run,           &
       nml_timesteps, nml_grid, nml_rect, nml_curv,   &
       nml_unst, nml_smc, nml_depth, nml_mask,        &
       nml_obst, nml_slope, nml_sed, nml_inbnd_count, &
       nml_inbnd_point, nml_excl_count,               &
       nml_excl_point, nml_excl_body,                 &
       nml_outbnd_count, nml_outbnd_line, ierr)
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
    !     reads all the namelist to define the model grid
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
    !      infile            char.
    !      nml_spectrum      type
    !      nml_run           type
    !      nml_timesteps     type
    !      nml_grid          type
    !      nml_rect          type
    !      nml_curv          type
    !      nml_unst          type
    !      nml_smc           type
    !      nml_depth         type
    !      nml_mask          type
    !      nml_obst          type
    !      nml_slope         type
    !      nml_sed           type
    !      nml_inbnd_count   type
    !      nml_inbnd_point   type
    !      nml_excl_count    type
    !      nml_excl_point    type
    !      nml_excl_body     type
    !      nml_outbnd_count  type
    !      nml_outbnd_line   type
    !      ierr              int.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      read_spectrum_nml
    !      report_spectrum_nml
    !      read_run_nml
    !      report_run_nml
    !      read_timesteps_nml
    !      report_timesteps_nml
    !      read_grid_nml
    !      report_grid_nml
    !      read_rect_nml
    !      report_rect_nml
    !      read_curv_nml
    !      report_curv_nml
    !      read_unst_nml
    !      report_unst_nml
    !      read_smc_nml
    !      report_smc_nml
    !      read_depth_nml
    !      report_depth_nml
    !      read_mask_nml
    !      report_mask_nml
    !      read_obst_nml
    !      report_obst_nml
    !      read_slope_nml
    !      report_slope_nml
    !      read_sed_nml
    !      report_sed_nml
    !      read_inbound_nml
    !      report_inbound_nml
    !      read_excluded_nml
    !      report_excluded_nml
    !      read_outbound_nml
    !      report_outbound_nml
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_grid  prog.   n/a    preprocess model grid.
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
    type(nml_spectrum_t), intent(inout)         :: nml_spectrum
    type(nml_run_t), intent(inout)              :: nml_run
    type(nml_timesteps_t), intent(inout)        :: nml_timesteps
    type(nml_grid_t), intent(inout)             :: nml_grid
    type(nml_rect_t), intent(inout)             :: nml_rect
    type(nml_curv_t), intent(inout)             :: nml_curv
    type(nml_unst_t), intent(inout)             :: nml_unst
    type(nml_smc_t), intent(inout)              :: nml_smc
    type(nml_depth_t), intent(inout)            :: nml_depth
    type(nml_mask_t), intent(inout)             :: nml_mask
    type(nml_obst_t), intent(inout)             :: nml_obst
    type(nml_slope_t), intent(inout)            :: nml_slope
    type(nml_sed_t), intent(inout)              :: nml_sed
    type(nml_inbnd_count_t), intent(inout)      :: nml_inbnd_count
    type(nml_inbnd_point_t), allocatable, intent(inout)      :: nml_inbnd_point(:)
    type(nml_excl_count_t), intent(inout)       :: nml_excl_count
    type(nml_excl_point_t), allocatable, intent(inout)       :: nml_excl_point(:)
    type(nml_excl_body_t), allocatable, intent(inout)        :: nml_excl_body(:)
    type(nml_outbnd_count_t), intent(inout)     :: nml_outbnd_count
    type(nml_outbnd_line_t), allocatable, intent(inout)      :: nml_outbnd_line(:)
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
    ! read spectrum namelist
    call read_spectrum_nml (ndsi, nml_spectrum)
    call report_spectrum_nml (nml_spectrum)
    ! read run namelist
    call read_run_nml (ndsi, nml_run)
    call report_run_nml (nml_run)
    ! read timesteps namelist
    call read_timesteps_nml (ndsi, nml_timesteps)
    call report_timesteps_nml (nml_timesteps)
    ! read grid namelist
    call read_grid_nml (ndsi, nml_grid)
    call report_grid_nml (nml_grid)
    ! read rect namelist
    call read_rect_nml (ndsi, nml_rect)
    call report_rect_nml (nml_rect)
    ! read curv namelist
    call read_curv_nml (ndsi, nml_curv)
    call report_curv_nml (nml_curv)
    ! read unst namelist
    call read_unst_nml (ndsi, nml_unst)
    call report_unst_nml (nml_unst)
    ! read smc namelist
    call read_smc_nml (ndsi, nml_smc)
    call report_smc_nml (nml_smc)
    ! read depth namelist
    call read_depth_nml (ndsi, nml_depth)
    call report_depth_nml (nml_depth)
    ! read mask namelist
    call read_mask_nml (ndsi, nml_mask)
    call report_mask_nml (nml_mask)
    ! read obst namelist
    call read_obst_nml (ndsi, nml_obst)
    call report_obst_nml (nml_obst)
    ! read slope namelist
    call read_slope_nml (ndsi, nml_slope)
    call report_slope_nml (nml_slope)
    ! read sed namelist
    call read_sed_nml (ndsi, nml_sed)
    call report_sed_nml (nml_sed)
    ! read inbound namelist
    call read_inbound_nml (ndsi, nml_inbnd_count, nml_inbnd_point)
    call report_inbound_nml (nml_inbnd_count, nml_inbnd_point)
    ! read excluded namelist
    call read_excluded_nml (ndsi, nml_excl_count, nml_excl_point, nml_excl_body)
    call report_excluded_nml (nml_excl_count, nml_excl_point, nml_excl_body)
    ! read outbound namelist
    call read_outbound_nml (ndsi, nml_outbnd_count, nml_outbnd_line)
    call report_outbound_nml (nml_outbnd_count, nml_outbnd_line)
    ! close namelist files
    close (ndsi)
    close (ndsn)
  end subroutine w3nmlgrid
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_spectrum_nml (ndsi, nml_spectrum)
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
    !      ndsi            int.
    !      nml_spectrum    type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_spectrum_t), intent(inout) :: nml_spectrum
    ! locals
    integer                   :: ierr
    type(nml_spectrum_t) :: spectrum
    namelist /spectrum_nml/ spectrum
    ierr = 0
    ! set default values for spectrum structure
    spectrum%xfr        = 0.
    spectrum%freq1      = 0.
    spectrum%nk         = 0
    spectrum%nth        = 0
    spectrum%thoff      = 0.
    ! read spectrum namelist
    rewind (ndsi)
    read (ndsi, nml=spectrum_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_spectrum_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (1)
    end if
    ! save namelist
    nml_spectrum = spectrum
  end subroutine read_spectrum_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_run_nml (ndsi, nml_run)
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
    !      nml_run      type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)            :: ndsi
    type(nml_run_t), intent(inout) :: nml_run
    ! locals
    integer                   :: ierr
    type(nml_run_t) :: run
    namelist /run_nml/ run
    ierr = 0
    ! set default values for run structure
    run%fldry      = .false.
    run%flcx       = .false.
    run%flcy       = .false.
    run%flcth      = .false.
    run%flck       = .false.
    run%flsou      = .false.
    ! read run namelist
    rewind (ndsi)
    read (ndsi, nml=run_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_run_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (2)
    end if
    ! save namelist
    nml_run = run
  end subroutine read_run_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_timesteps_nml (ndsi, nml_timesteps)
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
    !      ndsi             int.
    !      nml_timesteps    type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_timesteps_t), intent(inout) :: nml_timesteps
    ! locals
    integer                   :: ierr
    type(nml_timesteps_t) :: timesteps
    namelist /timesteps_nml/ timesteps
    ierr = 0
    ! set default values for timesteps structure
    timesteps%dtmax      = 0.
    timesteps%dtxy       = 0.
    timesteps%dtkth      = 0.
    timesteps%dtmin      = 0.
    ! read timesteps namelist
    rewind (ndsi)
    read (ndsi, nml=timesteps_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_timesteps_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (3)
    end if
    ! save namelist
    nml_timesteps = timesteps
  end subroutine read_timesteps_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_grid_nml (ndsi, nml_grid)
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
    !      ndsi             int.
    !      nml_grid         type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_grid_t), intent(inout)      :: nml_grid
    ! locals
    integer                   :: ierr
    type(nml_grid_t) :: grid
    namelist /grid_nml/ grid
    ierr = 0
    ! set default values for grid structure
    grid%name       = 'unset'
    grid%nml        = 'namelists.nml'
    grid%type       = 'unset'
    grid%coord      = 'unset'
    grid%clos       = 'unset'
    grid%zlim       = 0.
    grid%dmin       = 0.
    ! read grid namelist
    rewind (ndsi)
    read (ndsi, nml=grid_nml, iostat=ierr, iomsg=msg)
    if (ierr.ne.0) then
      write (ndse,'(a,/a)') &
           'error: read_grid_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (4)
    end if
    ! save namelist
    nml_grid = grid
  end subroutine read_grid_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_rect_nml (ndsi, nml_rect)
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
    !      ndsi             int.
    !      nml_rect         type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_rect_t), intent(inout)      :: nml_rect
    ! locals
    integer                   :: ierr
    type(nml_rect_t) :: rect
    namelist /rect_nml/ rect
    ierr = 0
    ! set default values for rect structure
    rect%nx         = 0
    rect%ny         = 0
    rect%sx         = 0.
    rect%sy         = 0.
    rect%sf         = 1.
    rect%x0         = 0.
    rect%y0         = 0.
    rect%sf0        = 1.
    ! read rect namelist
    rewind (ndsi)
    read (ndsi, nml=rect_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_rect_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (5)
    end if
    ! save namelist
    nml_rect = rect
  end subroutine read_rect_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_curv_nml (ndsi, nml_curv)
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
    !      ndsi             int.
    !      nml_curv         type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_curv_t), intent(inout)      :: nml_curv
    ! locals
    integer                   :: ierr
    type(nml_curv_t) :: curv
    namelist /curv_nml/ curv
    ierr = 0
    ! set default values for curv structure
    curv%nx              = 0
    curv%ny              = 0
    !
    curv%xcoord%sf        = 1.
    curv%xcoord%off       = 0.
    curv%xcoord%filename  = 'unset'
    curv%xcoord%idf       = 21
    curv%xcoord%idla      = 1
    curv%xcoord%idfm      = 1
    curv%xcoord%format    = '(....)'
    curv%xcoord%from      = 'name'
    !
    curv%ycoord%sf        = 1.
    curv%ycoord%off       = 0.
    curv%ycoord%filename  = 'unset'
    curv%ycoord%idf       = 22
    curv%ycoord%idla      = 1
    curv%ycoord%idfm      = 1
    curv%ycoord%format    = '(....)'
    curv%ycoord%from      = 'name'
    ! read curv namelist
    rewind (ndsi)
    read (ndsi, nml=curv_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_curv_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (6)
    end if
    ! save namelist
    nml_curv = curv
  end subroutine read_curv_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_unst_nml (ndsi, nml_unst)
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
    !      ndsi             int.
    !      nml_unst         type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_unst_t), intent(inout)      :: nml_unst
    ! locals
    integer                   :: ierr
    type(nml_unst_t) :: unst
    namelist /unst_nml/ unst
    ierr = 0
    ! set default values for unst structure
    unst%sf              = 0
    unst%filename        = 'unset'
    unst%idf             = 20
    unst%idla            = 1
    unst%idfm            = 1
    unst%format          = '(....)'
    unst%ugobcfile       = 'unset'
    ! read unst namelist
    rewind (ndsi)
    read (ndsi, nml=unst_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_unst_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (7)
    end if
    ! save namelist
    nml_unst = unst
  end subroutine read_unst_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_smc_nml (ndsi, nml_smc)
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
    !      ndsi             int.
    !      nml_smc          type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_smc_t), intent(inout)       :: nml_smc
    ! locals
    integer                   :: ierr
    type(nml_smc_t) :: smc
    namelist /smc_nml/ smc
    ierr = 0
    ! set default values for smc structure
    smc%mcels%filename   = 'unset'
    smc%mcels%idf        = 31
    smc%mcels%idla       = 1
    smc%mcels%idfm       = 1
    smc%mcels%format     = '(....)'
    !
    smc%iside%filename   = 'unset'
    smc%iside%idf        = 32
    smc%iside%idla       = 1
    smc%iside%idfm       = 1
    smc%iside%format     = '(....)'
    !
    smc%jside%filename   = 'unset'
    smc%jside%idf        = 33
    smc%jside%idla       = 1
    smc%jside%idfm       = 1
    smc%jside%format     = '(....)'
    !
    smc%subtr%filename   = 'unset'
    smc%subtr%idf        = 34
    smc%subtr%idla       = 1
    smc%subtr%idfm       = 1
    smc%subtr%format     = '(....)'
    !
    smc%bundy%filename   = 'unset'
    smc%bundy%idf        = 35
    smc%bundy%idla       = 1
    smc%bundy%idfm       = 1
    smc%bundy%format     = '(....)'
    !
    smc%mbarc%filename   = 'unset'
    smc%mbarc%idf        = 36
    smc%mbarc%idla       = 1
    smc%mbarc%idfm       = 1
    smc%mbarc%format     = '(....)'
    !
    smc%aisid%filename   = 'unset'
    smc%aisid%idf        = 37
    smc%aisid%idla       = 1
    smc%aisid%idfm       = 1
    smc%aisid%format     = '(....)'
    !
    smc%ajsid%filename   = 'unset'
    smc%ajsid%idf        = 38
    smc%ajsid%idla       = 1
    smc%ajsid%idfm       = 1
    smc%ajsid%format     = '(....)'
    ! read smc namelist
    rewind (ndsi)
    read (ndsi, nml=smc_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_smc_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (8)
    end if
    ! save namelist
    nml_smc = smc
  end subroutine read_smc_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_depth_nml (ndsi, nml_depth)
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
    !      ndsi             int.
    !      nml_depth        type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_depth_t), intent(inout)     :: nml_depth
    ! locals
    integer                   :: ierr
    type(nml_depth_t) :: depth
    namelist /depth_nml/ depth
    ierr = 0
    ! set default values for depth structure
    depth%sf         = 1.
    depth%filename   = 'unset'
    depth%idf        = 50
    depth%idla       = 1
    depth%idfm       = 1
    depth%format     = '(....)'
    depth%from       = 'name'
    ! read depth namelist
    rewind (ndsi)
    read (ndsi, nml=depth_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_depth_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (9)
    end if
    ! save namelist
    nml_depth = depth
  end subroutine read_depth_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_mask_nml (ndsi, nml_mask)
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
    !      ndsi             int.
    !      nml_mask         type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_mask_t), intent(inout)      :: nml_mask
    ! locals
    integer                   :: ierr
    type(nml_mask_t) :: mask
    namelist /mask_nml/ mask
    ierr = 0
    ! set default values for mask structure
    mask%sf         = 1.
    mask%filename   = 'unset'
    mask%idf        = 60
    mask%idla       = 1
    mask%idfm       = 1
    mask%format     = '(....)'
    mask%from       = 'name'
    ! read mask namelist
    rewind (ndsi)
    read (ndsi, nml=mask_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_mask_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (10)
    end if
    ! save namelist
    nml_mask = mask
  end subroutine read_mask_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_obst_nml (ndsi, nml_obst)
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
    !      ndsi             int.
    !      nml_obst         type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_obst_t), intent(inout)      :: nml_obst
    ! locals
    integer                   :: ierr
    type(nml_obst_t) :: obst
    namelist /obst_nml/ obst
    ierr = 0
    ! set default values for obst structure
    obst%sf         = 1.
    obst%filename   = 'unset'
    obst%idf        = 70
    obst%idla       = 1
    obst%idfm       = 1
    obst%format     = '(....)'
    obst%from       = 'name'
    ! read obst namelist
    rewind (ndsi)
    read (ndsi, nml=obst_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_obst_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (11)
    end if
    ! save namelist
    nml_obst = obst
  end subroutine read_obst_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_slope_nml (ndsi, nml_slope)
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
    !      ndsi             int.
    !      nml_slope        type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_slope_t), intent(inout)     :: nml_slope
    ! locals
    integer                   :: ierr
    type(nml_slope_t) :: slope
    namelist /slope_nml/ slope
    ierr = 0
    ! set default values for slope structure
    slope%sf         = 1.
    slope%filename   = 'unset'
    slope%idf        = 80
    slope%idla       = 1
    slope%idfm       = 1
    slope%format     = '(....)'
    slope%from       = 'name'
    ! read slope namelist
    rewind (ndsi)
    read (ndsi, nml=slope_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_slope_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (12)
    end if
    ! save namelist
    nml_slope = slope
  end subroutine read_slope_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_sed_nml (ndsi, nml_sed)
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
    !      ndsi             int.
    !      nml_sed          type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                  :: ndsi
    type(nml_sed_t), intent(inout)       :: nml_sed
    ! locals
    integer                   :: ierr
    type(nml_sed_t) :: sed
    namelist /sed_nml/ sed
    ierr = 0
    ! set default values for sed structure
    sed%sf         = 1.
    sed%filename   = 'unset'
    sed%idf        = 90
    sed%idla       = 1
    sed%idfm       = 1
    sed%format     = '(....)'
    sed%from       = 'name'
    ! read sed namelist
    rewind (ndsi)
    read (ndsi, nml=sed_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_sed_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (13)
    end if
    ! save namelist
    nml_sed = sed
  end subroutine read_sed_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_inbound_nml (ndsi, nml_inbnd_count, nml_inbnd_point)
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
    !      ndsi             int.
    !      nml_inbnd_count  type.
    !      nml_inbnd_point  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                      :: ndsi
    type(nml_inbnd_count_t), intent(inout)   :: nml_inbnd_count
    type(nml_inbnd_point_t), allocatable, intent(inout)   :: nml_inbnd_point(:)
    ! locals
    integer                   :: ierr, i
    type(nml_inbnd_count_t) :: inbnd_count
    namelist /inbnd_count_nml/ inbnd_count
    type(nml_inbnd_point_t), allocatable :: inbnd_point(:)
    namelist /inbnd_point_nml/ inbnd_point
    ierr = 0
    ! set default values for inbnd count structure
    inbnd_count%n_point    = 0
    ! read inbnd count namelist
    rewind (ndsi)
    read (ndsi, nml=inbnd_count_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_inbnd_count_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (14)
    end if
    ! allocate the total count of inbound points
    allocate(inbnd_point(inbnd_count%n_point))
    allocate(nml_inbnd_point(inbnd_count%n_point))
    ! set default values for inbnd point structure
    if (inbnd_count%n_point .ne. 0 ) then
      do i=1,inbnd_count%n_point
        inbnd_point(i)%x_index  = 0
        inbnd_point(i)%y_index  = 0
        inbnd_point(i)%connect  = .false.
      end do
    end if
    ! read inbnd point namelist
    rewind (ndsi)
    read (ndsi, nml=inbnd_point_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_inbnd_point_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (15)
    end if
    ! save namelist
    nml_inbnd_count = inbnd_count
    nml_inbnd_point = inbnd_point
  end subroutine read_inbound_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_excluded_nml (ndsi, nml_excl_count, nml_excl_point, &
       nml_excl_body)
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
    !      ndsi             int.
    !      nml_excl_count   type.
    !      nml_excl_point   type.
    !      nml_excl_body    type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                      :: ndsi
    type(nml_excl_count_t), intent(inout)    :: nml_excl_count
    type(nml_excl_point_t), allocatable, intent(inout)   :: nml_excl_point(:)
    type(nml_excl_body_t), allocatable, intent(inout)    :: nml_excl_body(:)
    ! locals
    integer                   :: ierr, j, k
    type(nml_excl_count_t) :: excl_count
    namelist /excl_count_nml/ excl_count
    type(nml_excl_point_t), allocatable :: excl_point(:)
    namelist /excl_point_nml/ excl_point
    type(nml_excl_body_t), allocatable :: excl_body(:)
    namelist /excl_body_nml/ excl_body
    ierr = 0
    ! set default values for excl count structure
    excl_count%n_point    = 0
    excl_count%n_body     = 0
    ! read excl count namelist
    rewind (ndsi)
    read (ndsi, nml=excl_count_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_excl_count_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (16)
    end if
    ! allocate the total count of excluded points
    allocate(excl_point(excl_count%n_point))
    allocate(nml_excl_point(excl_count%n_point))
    ! set default values for excl point structure
    if (excl_count%n_point .ne. 0 ) then
      do j=1,excl_count%n_point
        excl_point(j)%x_index  = 0
        excl_point(j)%y_index  = 0
        excl_point(j)%connect  = .false.
      end do
    end if
    ! read excl point namelist
    rewind (ndsi)
    read (ndsi, nml=excl_point_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_excl_point_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (17)
    end if
    ! allocate the total count of excluded bodies
    allocate(excl_body(excl_count%n_body))
    allocate(nml_excl_body(excl_count%n_body))
    ! set default values for excl body structure
    if (excl_count%n_body .ne. 0 ) then
      do k=1,excl_count%n_body
        excl_body(k)%x_index  = 0
        excl_body(k)%y_index  = 0
      end do
    end if
    ! read excl body namelist
    rewind (ndsi)
    read (ndsi, nml=excl_point_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_excl_point_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (18)
    end if
    ! save namelist
    nml_excl_count = excl_count
    nml_excl_point = excl_point
    nml_excl_body  = excl_body
  end subroutine read_excluded_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine read_outbound_nml (ndsi, nml_outbnd_count, nml_outbnd_line)
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
    !      ndsi             int.
    !      nml_outbnd_count type.
    !      nml_outbnd_line  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    integer, intent(in)                      :: ndsi
    type(nml_outbnd_count_t), intent(inout)    :: nml_outbnd_count
    type(nml_outbnd_line_t), allocatable, intent(inout)   :: nml_outbnd_line(:)
    ! locals
    integer                   :: ierr, i
    type(nml_outbnd_count_t) :: outbnd_count
    namelist /outbnd_count_nml/ outbnd_count
    type(nml_outbnd_line_t), allocatable :: outbnd_line(:)
    namelist /outbnd_line_nml/ outbnd_line
    ierr = 0
    ! set default values for outbnd count structure
    outbnd_count%n_line    = 0
    ! read outbnd count namelist
    rewind (ndsi)
    read (ndsi, nml=outbnd_count_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_outbnd_count_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (19)
    end if
    ! allocate the total count of outbound lines
    allocate(outbnd_line(outbnd_count%n_line))
    allocate(nml_outbnd_line(outbnd_count%n_line))
    ! set default values for outbnd line structure
    if (outbnd_count%n_line .ne. 0 ) then
      do i=1,outbnd_count%n_line
        outbnd_line(i)%x0   = 0.
        outbnd_line(i)%y0   = 0.
        outbnd_line(i)%dx   = 0.
        outbnd_line(i)%dy   = 0.
        outbnd_line(i)%np   = 0
      end do
    end if
    ! read outbnd line namelist
    rewind (ndsi)
    read (ndsi, nml=outbnd_line_nml, iostat=ierr, iomsg=msg)
    if (ierr.gt.0) then
      write (ndse,'(a,/a)') &
           'error: read_outbnd_line_nml: namelist read error', &
           'error: '//trim(msg)
      call extcde (20)
    end if
    ! save namelist
    nml_outbnd_count = outbnd_count
    nml_outbnd_line  = outbnd_line
  end subroutine read_outbound_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_spectrum_nml (nml_spectrum)
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
    !      nml_spectrum  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_spectrum_t), intent(in) :: nml_spectrum
    write (msg,'(a)') 'spectrum % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'xfr        = ', nml_spectrum%xfr
    write (ndsn,14) trim(msg),'freq1      = ', nml_spectrum%freq1
    write (ndsn,11) trim(msg),'nk         = ', nml_spectrum%nk
    write (ndsn,11) trim(msg),'nth        = ', nml_spectrum%nth
    write (ndsn,14) trim(msg),'thoff      = ', nml_spectrum%thoff
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_spectrum_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_run_nml (nml_run)
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
    !      nml_run  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_run_t), intent(in) :: nml_run
    write (msg,'(a)') 'run % '
    write (ndsn,'(a)')
    write (ndsn,13) trim(msg),'fldry      = ', nml_run%fldry
    write (ndsn,13) trim(msg),'flcx       = ', nml_run%flcx
    write (ndsn,13) trim(msg),'flcy       = ', nml_run%flcy
    write (ndsn,13) trim(msg),'flcth      = ', nml_run%flcth
    write (ndsn,13) trim(msg),'flck       = ', nml_run%flck
    write (ndsn,13) trim(msg),'flsou      = ', nml_run%flsou
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_run_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_timesteps_nml (nml_timesteps)
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
    !      nml_timesteps  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_timesteps_t), intent(in) :: nml_timesteps
    write (msg,'(a)') 'timesteps % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'dtmax      = ', nml_timesteps%dtmax
    write (ndsn,14) trim(msg),'dtxy       = ', nml_timesteps%dtxy
    write (ndsn,14) trim(msg),'dtkth      = ', nml_timesteps%dtkth
    write (ndsn,14) trim(msg),'dtmin      = ', nml_timesteps%dtmin
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_timesteps_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_grid_nml (nml_grid)
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
    !      nml_grid  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_grid_t), intent(in) :: nml_grid
    write (msg,'(a)') 'grid % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'name       = ', trim(nml_grid%name)
    write (ndsn,10) trim(msg),'nml        = ', trim(nml_grid%nml)
    write (ndsn,10) trim(msg),'type       = ', trim(nml_grid%type)
    write (ndsn,10) trim(msg),'coord      = ', trim(nml_grid%coord)
    write (ndsn,10) trim(msg),'clos       = ', trim(nml_grid%clos)
    write (ndsn,14) trim(msg),'zlim       = ', nml_grid%zlim
    write (ndsn,14) trim(msg),'dmin       = ', nml_grid%dmin
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_grid_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_rect_nml (nml_rect)
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
    !      nml_rect  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_rect_t), intent(in) :: nml_rect
    write (msg,'(a)') 'rect % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'nx         = ', nml_rect%nx
    write (ndsn,11) trim(msg),'ny         = ', nml_rect%ny
    write (ndsn,14) trim(msg),'sx         = ', nml_rect%sx
    write (ndsn,14) trim(msg),'sy         = ', nml_rect%sy
    write (ndsn,14) trim(msg),'sf         = ', nml_rect%sf
    write (ndsn,14) trim(msg),'x0         = ', nml_rect%x0
    write (ndsn,14) trim(msg),'y0         = ', nml_rect%y0
    write (ndsn,14) trim(msg),'sf0        = ', nml_rect%sf0
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f12.2)
  end subroutine report_rect_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_curv_nml (nml_curv)
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
    !      nml_curv  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_curv_t), intent(in) :: nml_curv
    write (msg,'(a)') 'curv % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'nx                 = ', nml_curv%nx
    write (ndsn,11) trim(msg),'ny                 = ', nml_curv%ny
    !
    write (ndsn,14) trim(msg),'xcoord % sf        = ', nml_curv%xcoord%sf
    write (ndsn,14) trim(msg),'xcoord % off       = ', nml_curv%xcoord%off
    write (ndsn,10) trim(msg),'xcoord % filename  = ', trim(nml_curv%xcoord%filename)
    write (ndsn,11) trim(msg),'xcoord % idf       = ', nml_curv%xcoord%idf
    write (ndsn,11) trim(msg),'xcoord % idla      = ', nml_curv%xcoord%idla
    write (ndsn,11) trim(msg),'xcoord % idfm      = ', nml_curv%xcoord%idfm
    write (ndsn,10) trim(msg),'xcoord % format    = ', trim(nml_curv%xcoord%format)
    write (ndsn,10) trim(msg),'xcoord % from      = ', trim(nml_curv%xcoord%from)
    !
    write (ndsn,14) trim(msg),'ycoord % sf        = ', nml_curv%ycoord%sf
    write (ndsn,14) trim(msg),'ycoord % off       = ', nml_curv%ycoord%off
    write (ndsn,10) trim(msg),'ycoord % filename  = ', trim(nml_curv%ycoord%filename)
    write (ndsn,11) trim(msg),'ycoord % idf       = ', nml_curv%ycoord%idf
    write (ndsn,11) trim(msg),'ycoord % idla      = ', nml_curv%ycoord%idla
    write (ndsn,11) trim(msg),'ycoord % idfm      = ', nml_curv%ycoord%idfm
    write (ndsn,10) trim(msg),'ycoord % format    = ', trim(nml_curv%ycoord%format)
    write (ndsn,10) trim(msg),'ycoord % from      = ', trim(nml_curv%ycoord%from)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_curv_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_unst_nml (nml_unst)
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
    !      nml_unst  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_unst_t), intent(in) :: nml_unst
    write (msg,'(a)') 'unst % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'sf        = ', nml_unst%sf
    write (ndsn,10) trim(msg),'filename  = ', trim(nml_unst%filename)
    write (ndsn,11) trim(msg),'idf       = ', nml_unst%idf
    write (ndsn,11) trim(msg),'idla      = ', nml_unst%idla
    write (ndsn,11) trim(msg),'idfm      = ', nml_unst%idfm
    write (ndsn,10) trim(msg),'format    = ', trim(nml_unst%format)
    !
    write (ndsn,10) trim(msg),'ugobcfile = ', trim(nml_unst%ugobcfile)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_unst_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_smc_nml (nml_smc)
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    !
    write (ndsn,10) trim(msg),'mcels % filename  = ', trim(nml_smc%mcels%filename)
    write (ndsn,11) trim(msg),'mcels % idf       = ', nml_smc%mcels%idf
    write (ndsn,11) trim(msg),'mcels % idla      = ', nml_smc%mcels%idla
    write (ndsn,11) trim(msg),'mcels % idfm      = ', nml_smc%mcels%idfm
    write (ndsn,10) trim(msg),'mcels % format    = ', trim(nml_smc%mcels%format)
    !
    write (ndsn,10) trim(msg),'iside % filename  = ', trim(nml_smc%iside%filename)
    write (ndsn,11) trim(msg),'iside % idf       = ', nml_smc%iside%idf
    write (ndsn,11) trim(msg),'iside % idla      = ', nml_smc%iside%idla
    write (ndsn,11) trim(msg),'iside % idfm      = ', nml_smc%iside%idfm
    write (ndsn,10) trim(msg),'iside % format    = ', trim(nml_smc%iside%format)
    !
    write (ndsn,10) trim(msg),'jside % filename  = ', trim(nml_smc%jside%filename)
    write (ndsn,11) trim(msg),'jside % idf       = ', nml_smc%jside%idf
    write (ndsn,11) trim(msg),'jside % idla      = ', nml_smc%jside%idla
    write (ndsn,11) trim(msg),'jside % idfm      = ', nml_smc%jside%idfm
    write (ndsn,10) trim(msg),'jside % format    = ', trim(nml_smc%jside%format)
    !
    write (ndsn,10) trim(msg),'subtr % filename  = ', trim(nml_smc%subtr%filename)
    write (ndsn,11) trim(msg),'subtr % idf       = ', nml_smc%subtr%idf
    write (ndsn,11) trim(msg),'subtr % idla      = ', nml_smc%subtr%idla
    write (ndsn,11) trim(msg),'subtr % idfm      = ', nml_smc%subtr%idfm
    write (ndsn,10) trim(msg),'subtr % format    = ', trim(nml_smc%subtr%format)
    !
    write (ndsn,10) trim(msg),'bundy % filename  = ', trim(nml_smc%bundy%filename)
    write (ndsn,11) trim(msg),'bundy % idf       = ', nml_smc%bundy%idf
    write (ndsn,11) trim(msg),'bundy % idla      = ', nml_smc%bundy%idla
    write (ndsn,11) trim(msg),'bundy % idfm      = ', nml_smc%bundy%idfm
    write (ndsn,10) trim(msg),'bundy % format    = ', trim(nml_smc%bundy%format)
    !
    write (ndsn,10) trim(msg),'mbarc % filename  = ', trim(nml_smc%mbarc%filename)
    write (ndsn,11) trim(msg),'mbarc % idf       = ', nml_smc%mbarc%idf
    write (ndsn,11) trim(msg),'mbarc % idla      = ', nml_smc%mbarc%idla
    write (ndsn,11) trim(msg),'mbarc % idfm      = ', nml_smc%mbarc%idfm
    write (ndsn,10) trim(msg),'mbarc % format    = ', trim(nml_smc%mbarc%format)
    !
    write (ndsn,10) trim(msg),'aisid % filename  = ', trim(nml_smc%aisid%filename)
    write (ndsn,11) trim(msg),'aisid % idf       = ', nml_smc%aisid%idf
    write (ndsn,11) trim(msg),'aisid % idla      = ', nml_smc%aisid%idla
    write (ndsn,11) trim(msg),'aisid % idfm      = ', nml_smc%aisid%idfm
    write (ndsn,10) trim(msg),'aisid % format    = ', trim(nml_smc%aisid%format)
    !
    write (ndsn,10) trim(msg),'ajsid % filename  = ', trim(nml_smc%ajsid%filename)
    write (ndsn,11) trim(msg),'ajsid % idf       = ', nml_smc%ajsid%idf
    write (ndsn,11) trim(msg),'ajsid % idla      = ', nml_smc%ajsid%idla
    write (ndsn,11) trim(msg),'ajsid % idfm      = ', nml_smc%ajsid%idfm
    write (ndsn,10) trim(msg),'ajsid % format    = ', trim(nml_smc%ajsid%format)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_smc_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_depth_nml (nml_depth)
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
    !      nml_depth  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_depth_t), intent(in) :: nml_depth
    write (msg,'(a)') 'depth % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'sf        = ', nml_depth%sf
    write (ndsn,10) trim(msg),'filename  = ', trim(nml_depth%filename)
    write (ndsn,11) trim(msg),'idf       = ', nml_depth%idf
    write (ndsn,11) trim(msg),'idla      = ', nml_depth%idla
    write (ndsn,11) trim(msg),'idfm      = ', nml_depth%idfm
    write (ndsn,10) trim(msg),'format    = ', trim(nml_depth%format)
    write (ndsn,10) trim(msg),'from      = ', trim(nml_depth%from)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_depth_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_mask_nml (nml_mask)
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
    !      nml_mask  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_mask_t), intent(in) :: nml_mask
    write (msg,'(a)') 'mask % '
    write (ndsn,'(a)')
    write (ndsn,10) trim(msg),'filename  = ', trim(nml_mask%filename)
    write (ndsn,11) trim(msg),'idf       = ', nml_mask%idf
    write (ndsn,11) trim(msg),'idla      = ', nml_mask%idla
    write (ndsn,11) trim(msg),'idfm      = ', nml_mask%idfm
    write (ndsn,10) trim(msg),'format    = ', trim(nml_mask%format)
    write (ndsn,10) trim(msg),'from      = ', trim(nml_mask%from)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_mask_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_obst_nml (nml_obst)
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
    !      nml_obst  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_obst_t), intent(in) :: nml_obst
    write (msg,'(a)') 'obst % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'sf        = ', nml_obst%sf
    write (ndsn,10) trim(msg),'filename  = ', trim(nml_obst%filename)
    write (ndsn,11) trim(msg),'idf       = ', nml_obst%idf
    write (ndsn,11) trim(msg),'idla      = ', nml_obst%idla
    write (ndsn,11) trim(msg),'idfm      = ', nml_obst%idfm
    write (ndsn,10) trim(msg),'format    = ', trim(nml_obst%format)
    write (ndsn,10) trim(msg),'from      = ', trim(nml_obst%from)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_obst_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_slope_nml (nml_slope)
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
    !      nml_slope  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_slope_t), intent(in) :: nml_slope
    write (msg,'(a)') 'slope % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'sf        = ', nml_slope%sf
    write (ndsn,10) trim(msg),'filename  = ', trim(nml_slope%filename)
    write (ndsn,11) trim(msg),'idf       = ', nml_slope%idf
    write (ndsn,11) trim(msg),'idla      = ', nml_slope%idla
    write (ndsn,11) trim(msg),'idfm      = ', nml_slope%idfm
    write (ndsn,10) trim(msg),'format    = ', trim(nml_slope%format)
    write (ndsn,10) trim(msg),'from      = ', trim(nml_slope%from)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_slope_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_sed_nml (nml_sed)
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
    !      nml_sed  type.
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_sed_t), intent(in) :: nml_sed
    write (msg,'(a)') 'sed % '
    write (ndsn,'(a)')
    write (ndsn,14) trim(msg),'sf        = ', nml_sed%sf
    write (ndsn,10) trim(msg),'filename  = ', trim(nml_sed%filename)
    write (ndsn,11) trim(msg),'idf       = ', nml_sed%idf
    write (ndsn,11) trim(msg),'idla      = ', nml_sed%idla
    write (ndsn,11) trim(msg),'idfm      = ', nml_sed%idfm
    write (ndsn,10) trim(msg),'format    = ', trim(nml_sed%format)
    write (ndsn,10) trim(msg),'from      = ', trim(nml_sed%from)
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_sed_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_inbound_nml (nml_inbnd_count, nml_inbnd_point)
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
    !      nml_inbnd_count  type
    !      nml_inbnd_point  type
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_inbnd_count_t), intent(in) :: nml_inbnd_count
    type(nml_inbnd_point_t), intent(in) :: nml_inbnd_point(nml_inbnd_count%n_point)
    ! locals
    integer              :: i
    write (msg,'(a)') 'inbnd_count % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'n_point       = ', nml_inbnd_count%n_point
    if (nml_inbnd_count%n_point .ne. 0) then
      do i=1,nml_inbnd_count%n_point
        write (msg,'(a,i8,a)') 'inbnd_point(',i,') % '
        write (ndsn,'(a)')
        write (ndsn,11) trim(msg),'x_index   = ', nml_inbnd_point(i)%x_index
        write (ndsn,11) trim(msg),'y_index   = ', nml_inbnd_point(i)%y_index
        write (ndsn,13) trim(msg),'connect   = ', nml_inbnd_point(i)%connect
        write (ndsn,'(a)')
      end do
    end if
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f8.2)
  end subroutine report_inbound_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_excluded_nml (nml_excl_count, nml_excl_point, nml_excl_body)
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
    !      nml_excl_count  type
    !      nml_excl_point  type
    !      nml_excl_body   type
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_excl_count_t), intent(in) :: nml_excl_count
    type(nml_excl_point_t), intent(in) :: nml_excl_point(nml_excl_count%n_point)
    type(nml_excl_body_t), intent(in)  :: nml_excl_body(nml_excl_count%n_body)
    ! locals
    integer              :: i
    write (msg,'(a)') 'excl_count % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'n_point       = ', nml_excl_count%n_point
    write (ndsn,11) trim(msg),'n_body        = ', nml_excl_count%n_body
    if (nml_excl_count%n_point .ne. 0) then
      do i=1,nml_excl_count%n_point
        write (msg,'(a,i8,a)') 'excl_point(',i,') % '
        write (ndsn,'(a)')
        write (ndsn,11) trim(msg),'x_index   = ', nml_excl_point(i)%x_index
        write (ndsn,11) trim(msg),'y_index   = ', nml_excl_point(i)%y_index
        write (ndsn,13) trim(msg),'connect   = ', nml_excl_point(i)%connect
        write (ndsn,'(a)')
      end do
    end if
    if (nml_excl_count%n_body .ne. 0) then
      do i=1,nml_excl_count%n_body
        write (msg,'(a,i8,a)') 'excl_body(',i,') % '
        write (ndsn,'(a)')
        write (ndsn,11) trim(msg),'x_index   = ', nml_excl_body(i)%x_index
        write (ndsn,11) trim(msg),'y_index   = ', nml_excl_body(i)%y_index
        write (ndsn,'(a)')
      end do
    end if
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f12.2)
  end subroutine report_excluded_nml
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine report_outbound_nml (nml_outbnd_count, nml_outbnd_line)
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
    !      nml_outbnd_count  type
    !      nml_outbnd_line   type
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
    !      w3nmlgrid subr.   n/a    namelist configuration routine.
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
    type(nml_outbnd_count_t), intent(in) :: nml_outbnd_count
    type(nml_outbnd_line_t), intent(in)  :: nml_outbnd_line(nml_outbnd_count%n_line)
    ! locals
    integer              :: i
    write (msg,'(a)') 'outbnd_count % '
    write (ndsn,'(a)')
    write (ndsn,11) trim(msg),'n_line        = ', nml_outbnd_count%n_line
    if (nml_outbnd_count%n_line .ne. 0) then
      do i=1,nml_outbnd_count%n_line
        write (msg,'(a,i8,a)') 'outbnd_line(',i,') % '
        write (ndsn,'(a)')
        write (ndsn,14) trim(msg),'x0        = ', nml_outbnd_line(i)%x0
        write (ndsn,14) trim(msg),'y0        = ', nml_outbnd_line(i)%y0
        write (ndsn,14) trim(msg),'dx        = ', nml_outbnd_line(i)%dx
        write (ndsn,14) trim(msg),'dy        = ', nml_outbnd_line(i)%dy
        write (ndsn,11) trim(msg),'np        = ', nml_outbnd_line(i)%np
        write (ndsn,'(a)')
      end do
    end if
10  format (a,2x,a,a)
11  format (a,2x,a,i8)
13  format (a,2x,a,l1)
14  format (a,2x,a,f12.2)
  end subroutine report_outbound_nml
  !/ ------------------------------------------------------------------- /
end module w3nmlgridmd
!/ ------------------------------------------------------------------- /
