!> @file
!> @brief parmeterization of the unresolved obstacles.
!>
!> @author lorenzo mentaschi
!> @date   08-oct-2018
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
!> @brief parmeterization of the unresolved obstacles.
!>
!> @author lorenzo mentaschi
!> @date   08-oct-2018
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>  national oceanic and atmospheric administration.  all rights
!>  reserved.  wavewatch iii is a trademark of the nws.
!>  no unauthorized use without permission.
!>
module w3uostmd
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |             lorenzo mentaschi     |
  !/                  |                        fortran 90 |
  !/                  | last update :         08-oct-2018 |
  !/                  +-----------------------------------+
  !/
  !/    aug-2018 : origination.                        ( version 6.07 )
  !/ 18-sep-2019 : added unit_ab and changed unit      ( version 7.06 )
  !/               number to 110 (c. bunney, ukmo)
  !/
  !/    copyright 2010 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose : parmeterization of the unresoled obstacles
  !
  !  2. subroutines and functions :
  !
  !      name               type  scope    description
  !     ----------------------------------------------------------------
  !    uost_initgrid        subr. public   allocates uost variables on each grid,
  !                                        and loads the matrices of alpha and beta coefficient
  !    uost_setgrid         subr. public   sets the actual computation grid in the source term
  !    uost_srctrmcompute   subr. public   computes the source term for a given input spectrum
  !     ----------------------------------------------------------------
  !
  !  3. switches :
  !
  !  4. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  use w3gdatmd, only: grid, sgrd, grids, sgrds
  use w3odatmd, only: ndso, ndse
  use w3servmd, only: extcde
  implicit none
  public :: uost_initgrid
  public :: uost_setgrid
  public :: uost_srctrmcompute
  private
  type uost_sourceterm
    real, allocatable :: costh(:), sinth(:)
    real :: gammaup = 10
    real :: gammadown = 20
    ! griddata is a pointer to the grid actually computed
    type(grid), pointer :: grd
    type(sgrd), pointer :: sgd
  contains
    !procedure, pass, private :: compute_psi => uost_sourceterm_compute_psi
    !compute_ld: estimates the local dissipation (private method)
    procedure, pass, private :: compute_ld => uost_sourceterm_compute_ld
    !compute_se: estimates the shadow effect (private method)
    procedure, pass, private :: compute_se => uost_sourceterm_compute_se
    !compute: estimates the whole dissipation
    procedure, pass :: compute => uost_sourceterm_compute
    !setgrid: sets grd pointer and computes some cached structures
    procedure, pass :: setgrid => uost_sourceterm_setgrid
  end type uost_sourceterm
  ! srctrm: global singleton source term
  class(uost_sourceterm), allocatable :: srctrm
  integer, parameter :: unit_ab = 110   ! unit number for load_alphabeta
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief allocate the uost variables for a given grid, and load
  !>        them from file.
  !>
  !> @param[in] igrid         id of the grid being initialized.
  !> @param[in] filelocal     file from where the alpha/beta coefficient
  !>                             for the local dissipation are loaded.
  !> @param[in] fileshadow    file from where the alpha/beta coefficient
  !>                             for the shadow effect are loaded.
  !> @param[in] localfactor   adjustment parameter for the local
  !>                             dissipation alpha and beta.
  !> @param[in] shadowfactor  adjustment parameter for the shadow
  !>                             dissipation alpha and beta.
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_initgrid(igrid, filelocal, fileshadow, localfactor, shadowfactor)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : allocate the uost variables for a given grid, and load
    !               them from file
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     igrid, integer:        id of the grid being initialized
    !     filelocal, string:     file from where the alpha/beta coefficient
    !                            for the local dissipation are loaded
    !     fileshadow, string:    file from where the alpha/beta coefficient
    !                            for the shadow effect are loaded
    !     localfactor, double:   adjustment parameter for the local
    !                            dissipation alpha and beta
    !     shadowfactor, double:  adjustment parameter for the shadow
    !                            dissipation alpha and beta
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name      type  module     description
    !     ----------------------------------------------------------------
    !      w3iogr    subr. w3iogrmd   initialization of grid objects
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: igrid
    character(len=*), intent(in) :: filelocal, fileshadow
    real, intent(in) :: localfactor, shadowfactor
    type(grid), pointer :: grd
    type(sgrd), pointer :: sgd
    real :: cgmax, minsize
    if ( (igrid .le. 0) .or. (.not. allocated(grids)) ) then
      return
    endif
    grd => grids(igrid)
    sgd => sgrds(igrid)
    grd%uostfilelocal = filelocal
    grd%uostfileshadow = fileshadow
    grd%uostlocalfactor = localfactor
    grd%uostshadowfactor = shadowfactor
    allocate( grd%uost_lcl_obstructed(grd%nx, grd%ny) )
    grd%uost_lcl_obstructed = .false.
    allocate( grd%uost_shd_obstructed(grd%nx, grd%ny) )
    grd%uost_shd_obstructed = .false.
    allocate( grd%uostcellsize(grd%nx, grd%ny, sgd%nth) )
    grd%uostcellsize = 0
    allocate( grd%uostlocalalpha(grd%nx, grd%ny, sgd%nk, sgd%nth) )
    grd%uostlocalalpha = 100
    allocate( grd%uostlocalbeta(grd%nx, grd%ny, sgd%nk, sgd%nth) )
    grd%uostlocalbeta = 100
    allocate( grd%uostshadowalpha(grd%nx, grd%ny, sgd%nk, sgd%nth) )
    grd%uostshadowalpha = 100
    allocate( grd%uostshadowbeta(grd%nx, grd%ny, sgd%nk, sgd%nth) )
    grd%uostshadowbeta = 100
    if ( (igrid .gt. 0) .and. ( allocated(srctrm)) ) then
      !  loading local/shadow alpha/beta
      call load_alphabeta(grd, sgd, unit_ab)
      !  warning the user that for cells too small uost may be inaccurate
      cgmax = 20 ! simply taking a high value for the max group velocity to give an indication of this threshold
      minsize = cgmax*grd%dtmax/1000
      write(ndso,*)'*** wavewatch-iii warning in w3uost/uost_initgrid'
      write(ndso,*)'uost: grid ',trim(grd%gname),':'
      write(ndso,*)'      global time step == ', grd%dtmax, ' s'
      write(ndso,*)'      for cells smaller than about ', minsize,    &
           ' km uost may underestimate the dissipation'
      write(ndso,*)
    endif
  end subroutine uost_initgrid
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief sets the current grid in the sourceterm object.
  !>
  !> @param[in] igrid  id of the actual grid.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_setgrid(igrid)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |         lorenzo mentaschi         |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : sets the current grid in the sourceterm object
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     igrid, integer:        id of the actual grid
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name      type  module     description
    !     ----------------------------------------------------------------
    !      w3init    subr. w3initmd   initialization of grid objects
    !      w3wave    subr. w3wavemd   initialization of grid objects
    !                                 before computation
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: igrid
    if ( .not. allocated(srctrm) ) then
      allocate(srctrm)
    endif
    call srctrm%setgrid(grids(igrid), sgrds(igrid))
  end subroutine uost_setgrid
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief estimates the uost source term for a give spectrum.
  !>
  !> @param[in]  ix
  !> @param[in]  iy
  !> @param[in]  spec
  !> @param[in]  cg
  !> @param[in]  dt
  !> @param[in]  u10abs
  !> @param[in]  u10dir
  !> @param[out] s
  !> @param[out] d
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_srctrmcompute(ix, iy, spec, cg, dt, u10abs, u10dir, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : estimates the uost source term for a give spectrum
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     igrid, integer:        id of the actual grid
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name      type  module     description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd   computation of the source terms
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: ix, iy
    real, intent(in) :: dt
    real, intent(in) :: spec(srctrm%sgd%nspec), cg(srctrm%sgd%nk)
    real, intent(in) :: u10abs, u10dir
    real, intent(out) :: s(srctrm%sgd%nspec), d(srctrm%sgd%nspec)
    call srctrm%compute(ix, iy, spec, cg, dt, u10abs, u10dir, s, d)
  end subroutine uost_srctrmcompute
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief loads local and shadow alpha and beta from files.
  !>
  !> @param[inout] grd       object representing the spatial grid to be loaded.
  !> @param[inout] srd       object representing the current spectral grid.
  !> @param[inout] fileunit  unit id of the input files.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine load_alphabeta(grd, sgd, fileunit)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : loads local and shadow alpha and beta from files
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     grd, grid type:        object representing the spatial grid to
    !                            be loaded
    !     sgd, sgrd type:        object representing the current spectral grid
    !     fileunit, integer:     unit id of the input files
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name            type  module     description
    !     ----------------------------------------------------------------
    !      uost_initgrid   subr. w3uostmd   initialization of the uost grid
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    type(grid), intent(inout) :: grd
    type(sgrd), intent(in) :: sgd
    integer, intent(in) :: fileunit
    character(256) :: filename
    logical :: fileexists
    integer :: jg, j, l, i, ix, iy
    ! loading local alpha/beta
    filename = grd%uostfilelocal
    inquire(file=filename, exist=fileexists)
    j = len_trim(filename)
    if (.not. fileexists) then
      write(ndse,*)'*** wavewatch iii error in w3uost: '// &
           'file '//filename(:j)//' not found. quitting'
      call extcde (9999)
    endif
    write(ndso,*)'file '//filename(:j)//' found.'// &
         'loading uost settings for grid '//grd%gname
    call load_alphabeta_fromfile(fileunit, filename(:j), grd%nx, grd%ny, sgd%nk, sgd%nth,&
         grd%uostabmultfactor, grd%uostlocalalpha, grd%uostlocalbeta,&
         grd%uostcellsize, grd%uost_lcl_obstructed)
    ! loading shadow alpha/beta
    filename = grd%uostfileshadow
    inquire(file=filename, exist=fileexists)
    j = len_trim(filename)
    if (.not. fileexists) then
      write(ndse,*)'*** wavewatch iii error in w3uost: '// &
           'file '//filename(:j)//' not found. quitting'
      call extcde (9999)
    endif
    write(ndso,*)'file '//filename(:j)//' found.'//&
         'loading uost settings for grid '//grd%gname
    call load_alphabeta_fromfile(fileunit, filename(:j), grd%nx, grd%ny, sgd%nk, sgd%nth,&
         grd%uostabmultfactor, grd%uostshadowalpha, grd%uostshadowbeta,&
         grd%uostcellsize, grd%uost_shd_obstructed)
  end subroutine load_alphabeta
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief loads alpha and beta from a single obstructions file.
  !>
  !> @param[in]    fileunit      unit of the file to be opened.
  !> @param[in]    filename      name of the file.
  !> @param[in]    nx            size of spatial grid x dim.
  !> @param[in]    ny            size of spatial grid y dim.
  !> @param[in]    nk            size of spectral grid k dim.
  !> @param[in]    nth           size of spectral grid th dim.
  !> @param[in]    multfactor    multiplication factor for alpha and beta.
  !> @param[inout] alphamtx      loaded alpha spatial/spectral matrices.
  !> @param[inout] betamtx       loaded beta spatial/spectral matrices.
  !> @param[inout] cellsize      cell size for each spectral direction,
  !>                             also loaded from the file.
  !> @param[inout] isobstructed  matrix of logicals, indicating for each cell
  !>                             if it is obstructed or not.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine load_alphabeta_fromfile(fileunit, filename, nx, ny, nk, nth,&
       multfactor, alphamtx, betamtx, cellsize, isobstructed)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : loads alpha and beta from a single obstructions file
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     fileunit, integer:             unit of the file to be opened
    !     filename, string:              name of the file
    !     nx, ny, nk, nth, integer:      size of the spatial/spectral grid
    !     multfactor, real:              multiplication factor for alpha and beta:
    !                                    alpha and beta should be real in [0,1]
    !                                    but to save memory the are stored in integer*1
    !     alphamtx, betamtx, integer*1:  loaded alpha and beta spatial/spectral matrices
    !     cellsize, real, real:          cell size for each spectral direction,
    !                                    also loaded from the file
    !     isobstructed, logical:         matrix of logicals, indicating for each cell
    !                                    if it is obstructed or not
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name            type  module     description
    !     ----------------------------------------------------------------
    !      load_alphabeta  subr. w3uostmd   initialization of the uost grid
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    character(*), intent(in) :: filename
    real, intent(in) :: multfactor
    integer, intent(in) :: fileunit, nx, ny, nk, nth
    integer*1, intent(inout) :: alphamtx(:,:,:,:), betamtx(:,:,:,:)
    real*4, intent(inout) ::  cellsize(:,:,:)
    logical, intent(inout) ::  isobstructed(:,:)
    character(len=600) :: line
    integer :: fiostat
    logical :: header, filestart, readingcellsize, readingalpha
    integer :: ix, iy, spgrds_size, ik
    real, allocatable :: trans(:)
    !  initializing logicals representing the different phases of the load
    filestart = .true.
    header = .true.;
    readingcellsize = .false.
    readingalpha = .false.
    ik = 0
    allocate(trans(nth))
    open(fileunit, file=filename, status='old', action='read')
    read_loop: do
      read(fileunit, '(a)', iostat=fiostat) line
      if (fiostat .ne. 0) exit read_loop
      if (line(1:1) .eq. '$') cycle
      if (filestart) then
        !  reading the first line
        read(line, '(i5)') spgrds_size
        filestart = .false.
      elseif (header) then
        !  reading the position of an obstructed cell
        read(line, *) ix, iy
        isobstructed(ix, iy) = .true.
        if ((ix .gt. nx) .or. (iy .gt. ny)) then
          write(ndse,*) '*** wavewatch iii error in w3uost: '// &
               'grid indices out of range.'// &
               'check file '//filename
          call extcde (9999)
        endif
        !  marking the end of the reading of the header
        header = .false.
        ik = 1
        readingcellsize = .true.
      elseif (readingcellsize) then
        !  reading the sizes of the cell
        read(line, *) cellsize(ix, iy, :)
        readingcellsize = .false.
        readingalpha = .true.
      else
        read(line, *) trans
        if (readingalpha) then
          !  reading alpha for frequency ik
          alphamtx(ix, iy, ik, :) = nint(trans*multfactor)
        else
          !  reading beta for frequency ik
          betamtx(ix, iy, ik, :) = nint(trans*multfactor)
        endif
        if (ik .lt. nk) then
          ik = ik + 1
        else if (readingalpha) then
          !  preparing to read the next cell
          readingalpha = .false.
          ik = 1
        else
          header = .true.
          ik = 1
        endif
      endif
    enddo read_loop
    close(fileunit)
    deallocate(trans)
  end subroutine load_alphabeta_fromfile
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief method of the class uost_sourceterm, to set the
  !>  actual spatial and spectral grid.
  !>
  !> @param[inout] this
  !> @param[in]    grd   object representing the spatial grid to be loaded.
  !> @param[in]    sgd   object representing the current spectral grid.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_sourceterm_setgrid(this, grd, sgd)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : method of the class uost_sourceterm,
    !               to set the actual spatial and spectral grid
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     grd, grid type:        object representing the spatial grid to
    !                            be loaded
    !     sgd, sgrd type:        object representing the current spectral grid
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name            type  module     description
    !     ----------------------------------------------------------------
    !      uost_setgrid  subr. w3uostmd   setting the actual computation grid
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    class(uost_sourceterm), intent(inout) :: this
    type(grid), target, intent(in) :: grd
    type(sgrd), target, intent(in) :: sgd
    integer :: ith, nth
    this%grd => grd
    this%sgd => sgd
    if (allocated(this%costh)) then
      deallocate(this%costh)
      deallocate(this%sinth)
    endif
    nth = this%sgd%nth
    allocate(this%costh(nth))
    allocate(this%sinth(nth))
    do ith=1,nth
      this%costh(ith) = cos(sgd%th(ith))
      this%sinth(ith) = sin(sgd%th(ith))
    enddo
  end subroutine uost_sourceterm_setgrid
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief in conditions of wind sea, the effect of the unresolved
  !>  obstacles is reduced.
  !>
  !> @details here a reduction psi is computed, as a function of the
  !>  wave age.
  !>
  !> @param[in]  u10abs  absolute value of u10.
  !> @param[in]  u10dir  direction of u10.
  !> @param[in]  cgabs   absolute value of the group velocity.
  !> @param[in]  cgdir   direction of the group velocity.
  !> @param[in]  dt      time step.
  !> @param[out] psi     output psi factor.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine compute_reduction_psi(u10abs, u10dir, cgabs, cgdir, dt, psi)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : in conditions of wind sea, the effect
    !               of the unresolved obstacles is reduced.
    !               here a reduction psi is computed, as a function of the
    !               wave age.
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     u10abs, real:     absolute value of u10
    !     u10dir, real:     direction of u10
    !     cgabs, real:      absolute value of the group velocity
    !     cgdir, real:      direction of the group velocity
    !     dt, real:         time step
    !     psi, real:        output psi factor
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name                   type  module     description
    !     ----------------------------------------------------------------
    !      uost_sourceterm_compute_ld  subr. w3uostmd   computing the local dissipation
    !      uost_sourceterm_compute_se  subr. w3uostmd   computing the shadow effect
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: pi
    implicit none
    real, parameter :: tolerance = 0.000001
    real, parameter :: whthr1 = .5, whthr2 = 1.5
    real, intent(in) :: u10abs, u10dir, cgabs, cgdir, dt
    real, intent(out) :: psi
    real :: thdelta, cp, wa
    ! computing the wave age
    thdelta = abs(u10dir - cgdir)
    do while (thdelta .gt. pi)
      thdelta = thdelta - 2*pi
    enddo
    thdelta = abs(thdelta)
    if (pi/2 - thdelta .gt. tolerance) then
      cp = cgabs*2 ! this is scrictly valid only in deep water
      wa = cp/u10abs/cos(thdelta)
    else
      wa = 9999999 ! a very high number
    endif
    if (wa .le. whthr1) then
      !  if the wave age is less that 0.5, psi = 0, i.e.
      !  no unresolved obstacle is considered
      psi = 0
    elseif ((wa .gt. whthr1) .and. (wa .lt. whthr2)) then
      !  if the wave age is between 0.5 and 1.5
      !  psi scales linearly with wa
      psi = (wa - whthr1)/(whthr2 - whthr1)
    else
      !  if the wave age is greater than 1.5 psi = 1
      psi = 1
    endif
  end subroutine compute_reduction_psi
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief method of the class uost_sourceterm.
  !>
  !> @details computation of the local dissipation of the spectrum.
  !>
  !> @param[inout] this    instance of uost_sourceterm passed to the method.
  !> @param[in]    ix      x coordinates of actual call.
  !> @param[in]    iy      y coordinates of actual call.
  !> @param[in]    spec    input spectrum.
  !> @param[in]    cg      group velocity.
  !> @param[in]    dt      time step.
  !> @param[in]    u10abs  absolute value of u10.
  !> @param[in]    u10dir  direction of u10.
  !> @param[out]   s       source term.
  !> @param[out]   d       differential of the source term over the spectrum.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_sourceterm_compute_ld(this, ix, iy, spec, cg, dt, u10abs, u10dir, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : method of the class uost_sourceterm.
    !               computation of the local dissipation of the spectrum
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     this: uost_sourceterm      instance of uost_sourceterm passed to the method
    !                           (compulsory in oo programming)
    !     ix, iy: integer       coordinates of the actual cell
    !     spec: real            input spectrum
    !     cg: real              group velocity
    !     dt: real              time step
    !     u10abs: real          absolute value of u10
    !     u10dir: real          direction of u10
    !     s: real               source term
    !     d: real               differential of the source term over the spectrum
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name                   type  module     description
    !     ----------------------------------------------------------------
    !      uost_sourceterm_compute     subr. w3uostmd   computing the source term
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    class(uost_sourceterm), intent(inout) :: this
    integer, intent(in) :: ix, iy
    real, intent(in) :: spec(this%sgd%nspec), cg(this%sgd%nk)
    real, intent(out) :: s(this%sgd%nspec), d(this%sgd%nspec)
    real, intent(in) :: u10abs, u10dir
    real, intent(in) :: dt
    integer :: ik, ith, isp, nk, nth
    real :: alpha, beta, cgi, cellsize, speci, sfc
    logical :: cellobstructed
    real :: th, psi
    s = 0
    d = 0
    cellobstructed =  this%grd%uost_lcl_obstructed(ix, iy)
    if (.not. cellobstructed) return
    nk = this%sgd%nk
    nth = this%sgd%nth
    do ik = 1,nk
      cgi = cg(ik)
      do ith = 1,nth
        !  getting alpha and beta for local dissipation
        alpha = this%grd%uostlocalalpha(ix, iy, ik, ith)/this%grd%uostabmultfactor
        alpha = max(min(alpha*this%grd%uostlocalfactor, 1.), 0.)
        beta = this%grd%uostlocalbeta(ix, iy, ik, ith)/this%grd%uostabmultfactor
        beta = max(min(beta*this%grd%uostlocalfactor, 1.), 0.)
        if (alpha .eq. 1) cycle
        !  getting the size of the cell along direction ith
        cellsize = this%grd%uostcellsize(ix, iy, ith)*this%grd%uostcellsizefactor
        isp = ith + (ik-1)*nth
        speci = spec(isp)
        th = this%sgd%th(ith)
        call compute_reduction_psi(u10abs, u10dir, cg(ik), th, dt, psi)
        if (beta > 0.09) then
          !  computing the local dissipation for a partially obstructed cell
          sfc = - cgi/cellsize * (1 - beta)/beta
        else
          !  the cell is almost completely obstructed.
          !  dissipating the energy almost completely.
          sfc = - cgi/cellsize * this%gammaup
        endif
        s(isp) = sfc * speci * psi
        d(isp) = sfc * psi
      enddo
    enddo
  end subroutine uost_sourceterm_compute_ld
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief method of the class uost_sourceterm.
  !>
  !> @details computation of the shadow dissipation of the spectrum.
  !>
  !> @param[inout] this    instance of uost_sourceterm passed to the method.
  !> @param[in]    ix      x coordinates of actual call.
  !> @param[in]    iy      y coordinates of actual call.
  !> @param[in]    spec    input spectrum.
  !> @param[in]    cg      group velocity.
  !> @param[in]    dt      time step.
  !> @param[in]    u10abs  absolute value of u10.
  !> @param[in]    u10dir  direction of u10.
  !> @param[out]   s       source term.
  !> @param[out]   d       differential of the source term over the spectrum.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_sourceterm_compute_se(this, ix, iy, spec, cg, dt, u10abs, u10dir, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : method of the class uost_sourceterm.
    !               computation of the shadow dissipation of the spectrum
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     this: uost_sourceterm      instance of uost_sourceterm passed to the method
    !                           (compulsory in oo programming)
    !     ix, iy: integer       coordinates of the actual cell
    !     spec: real            input spectrum
    !     cg: real              group velocity
    !     dt: real              time step
    !     u10abs: real          absolute value of u10
    !     u10dir: real          direction of u10
    !     s: real               source term
    !     d: real               differential of the source term over the spectrum
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name                   type  module     description
    !     ----------------------------------------------------------------
    !      uost_sourceterm_compute     subr. w3uostmd   computing the source term
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    class(uost_sourceterm), intent(inout), target :: this
    integer, intent(in) :: ix, iy
    real, intent(in) :: spec(this%sgd%nspec), cg(this%sgd%nk)
    real, intent(out) :: s(this%sgd%nspec), d(this%sgd%nspec)
    real, intent(in) :: u10abs, u10dir
    real, intent(in) :: dt
    integer :: ik, ith, is
    real :: cgi, speci, sfc, cellsize, &
         sfcleft, sfcright, sfccenter, thdiag, cgdiag, &
         alphash, betash, gammma, gg
    integer :: n = 8, ithdiag, isp, nk, nth, nx, ny
    logical :: cellobstructed
    real :: th, psi
    s = 0
    d = 0
    nk = this%sgd%nk
    nth = this%sgd%nth
    nx = this%grd%nx
    ny = this%grd%ny
    if ((ix .eq. 1) .or. (ix .eq. nx) .or. (iy .eq. 1) .or. (iy .eq. ny)) return
    cellobstructed = this%grd%uost_shd_obstructed(ix, iy)
    if (.not. cellobstructed) return
    do ik=1,nk
      do ith=1,nth
        !  getting alpha and beta of the shadow
        alphash = this%grd%uostshadowalpha(ix, iy, ik, ith)/this%grd%uostabmultfactor
        alphash = max(min(alphash*this%grd%uostshadowfactor, 1.), 0.)
        betash = this%grd%uostshadowbeta(ix, iy, ik, ith)/this%grd%uostabmultfactor
        betash = max(min(betash*this%grd%uostshadowfactor, 1.), 0.)
        if (alphash .eq. 1) cycle
        !  getting the size of the cell along direction ith
        cellsize = this%grd%uostcellsize(ix, iy, ith)*this%grd%uostcellsizefactor
        cgi = cg(ik)
        gg = cgi/cellsize
        if (alphash > 0.2) then
          !  computing the shadow gamma coefficient for a partially obstructed cell
          gammma = (betash/alphash - 1)
        else
          !  alpha is small. the shadow dissipates the energy almost completely
          gammma = this%gammadown
        endif
        th = this%sgd%th(ith)
        !  computing the reduction psi related with the wind component of the spectrum
        call compute_reduction_psi(u10abs, u10dir, cg(ik), th, dt, psi)
        sfc = - gg*gammma
        isp = ith + (ik-1)*nth
        speci = spec(isp)
        s(isp) = sfc * speci * psi
        d(isp) = sfc * psi
      enddo
    enddo
  end subroutine uost_sourceterm_compute_se
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief method of the class uost_sourceterm.
  !>
  !> @details computation of the source term.
  !>
  !> @param[inout] this    instance of uost_sourceterm passed to the method.
  !> @param[in]    ix      x coordinates of actual call.
  !> @param[in]    iy      y coordinates of actual call.
  !> @param[in]    spec    input spectrum.
  !> @param[in]    cg      group velocity.
  !> @param[in]    dt      time step.
  !> @param[in]    u10abs  absolute value of u10.
  !> @param[in]    u10dir  direction of u10.
  !> @param[out]   s       source term.
  !> @param[out]   d       differential of the source term over the spectrum.
  !>
  !> @author lorenzo mentaschi
  !> @date   01-oct-2018
  !>
  subroutine uost_sourceterm_compute(this, ix, iy, spec, cg, dt, u10abs, u10dir, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |          lorenzo mentaschi        |
    !/                  |                        fortran 90 |
    !/                  | last update :         01-oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    aug-2018 : origination.                        ( version 6.07 )
    !/
    !  1. purpose : method of the class uost_sourceterm.
    !               computation of the source term
    !  2. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     this: uost_sourceterm      instance of uost_sourceterm passed to the method
    !                           (compulsory in oo programming)
    !     ix, iy: integer       coordinates of the actual cell
    !     spec: real            input spectrum
    !     cg: real              group velocity
    !     dt: real              time step
    !     u10abs: real          absolute value of u10
    !     u10dir: real          direction of u10
    !     s: real               source term
    !     d: real               differential of the source term over the spectrum
    !     ----------------------------------------------------------------
    !
    !  3. called by :
    !
    !      name                   type  module     description
    !     ----------------------------------------------------------------
    !      uost_srctrmcompute     subr. w3uostmd   computing the source term
    !     ----------------------------------------------------------------
    !
    !  4. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    class(uost_sourceterm), intent(inout) :: this
    integer, intent(in) :: ix, iy
    real, intent(in) :: spec(this%sgd%nspec), cg(this%sgd%nk)
    real, intent(in) :: dt
    real, intent(in) :: u10abs, u10dir
    real, intent(out) :: s(this%sgd%nspec), d(this%sgd%nspec)
    real :: s_ld(this%sgd%nspec), s_se(this%sgd%nspec)
    real :: d_ld(this%sgd%nspec), d_se(this%sgd%nspec)
    if (.not. this%grd%uostenabled) then
      s = 0
      return
    endif
    !  initializing the ld and se components
    s_ld = 0
    s_se = 0
    !  local dissipation
    call this%compute_ld(ix, iy, spec, cg, dt, u10abs, u10dir, s_ld, d_ld)
    !  shadow effect
    call this%compute_se(ix, iy, spec, cg, dt, u10abs, u10dir, s_se, d_se)
    s = s_ld + s_se
    d = d_ld + d_se
  end subroutine uost_sourceterm_compute
  !/ ------------------------------------------------------------------- /
end module w3uostmd
!/ ------------------------------------------------------------------- /
