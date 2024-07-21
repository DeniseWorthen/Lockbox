!> @file
!> @brief define data structures to set up wave model input data for
!>  several models simultaneously.
!>
!> @author h. l. tolman @date 22-mar-2021
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
!> @brief define data structures to set up wave model input data for
!>  several models simultaneously.
!>
!> @author h. l. tolman @date 22-mar-2021
!>
!/ ------------------------------------------------------------------- /
module w3idatmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    02-apr-2004 : origination.                        ( version 3.06 )
  !/    19-jul-2006 : adding auxiliary grids.             ( version 3.10 )
  !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
  !/    24-apr-2015 : adding oasis coupling calls         ( version 5.07 )
  !/                  (m. accensi & f. ardhuin, ifremer)
  !/    21-jun-2018 : add fswnd input for smc grid. jgli  ( version 6.04 )
  !/    22-mar-2021 : momentum and air density support    ( version 7.13 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     define data structures to set up wave model input data for
  !     several models simultaneously.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nidata    int.  public   number of models in array dim.
  !      iidata    int.  public   selected model for output, init. at -1.
  !      input     type  public   basic data structure.
  !      inputs    input public   array of data structures.
  !     ----------------------------------------------------------------
  !
  !     all elements of input are aliased to pointers with the same
  !     name. some aditional pointer provide previous equivalenced
  !     parameters. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      tln       i.a.  public   time for water level field.
  !      tc0/n     i.a.  public   times for current fields.
  !      tw0/n     i.a.  public   times for wind fields.
  !      tdn       i.a.  public   time for mud density field.
  !      ttn       i.a.  public   time for mud thickness field.
  !      tvn       i.a.  public   time for mud viscosity field.
  !      tin       i.a.  public   time for ice field. (concentration)
  !      tu0/n     i.a.  public   times for momentum fields.
  !      tr0/n     i.a.  public   times for air density fields.
  !      ti1n      i.a.  public   time for ice field. (parameter 1)
  !      ti2n      i.a.  public   time for ice field. (parameter 2)
  !      ti3n      i.a.  public   time for ice field. (parameter 3)
  !      ti4n      i.a.  public   time for ice field. (parameter 4)
  !      ti5n      i.a.  public   time for ice field. (parameter 5)
  !      tnn       i.a.  public   time for data types 1-3.
  !      tdn       i.a.  public   time for next data.
  !      tg0/n     i.a.  public   times for grid motion data.
  !      tfn       i.a.  public   array consolidating most above times.
  !      ga0/n     real  public   norm of grid speed vector.
  !      gd0/n     real  public   direction of grid speed vector.
  !      wx0/n     r.a.  public   cartesian x and y wind components
  !      wy0/n     r.a.  public      for both times.
  !      dt0/n     r.a.  public   corr. air-sea temperature differences.
  !      cx0/n     r.a.  public   cartesian x and y current components
  !      cy0/n     r.a.  public      for both times.
  !      wlev      r.a.  public   next water level field.
  !      icei      r.a.  public   ice concentrations.
  !      ux0/n     r.a.  public   cartesian x and y momentum components
  !      uy0/n     r.a.  public      for both times.
  !      rh0/n     r.a.  public   air density for both times
  !      bergi     r.a.  public   iceberg damping coefficient
  !      iinit     log.  public   flag for array initialization.
  !      fllev     log.  public   flag for water level input.
  !      flcur     log.  public   flag for current input.
  !      flwind    log.  public   flag for wind input.
  !      flice     log.  public   flag for ice input.
  !      fltaua    log.  public   flag for atmospheric momentum input
  !      flrhoa    log.  public   flag for air density input
  !      inflags1  l.a.  public   array consolidating the above six
  !                               flags, as well as four additional
  !                               data flags.
  !      inflags2  l.a.  public   like inflags1 but does *not* get changed
  !                               when model reads last record of ice.ww3
  !      flagsc    l.a.  public   coupling or not for input variables
  !      jfirst    int   public   first index of arrays related to
  !                               input fields.  at present this is
  !                               hardwired below. field-related arrays
  !                               (e.g., inflags1) will be allocated from
  !                               jfirst:9 (e.g., allocate(inflags1(jfirst:9))).
  !       cxtide    r.a.  public   tidal constituents of x current component
  !       cytide    r.a.  public   tidal constituents of y current component
  !       wltide    r.a.  public   tidal constituents of water level
  !       fllevtide log.  public   flag for use of tidal const. in water level input.
  !       flcurtide log.  public   flag for use of tidal const. in current input.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3ninp    subr. public   set number of grids/models.
  !      w3dimi    subr. public   set dimensions of arrays.
  !      w3seti    subr. public   point to selected grid / model.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3setg    subr. w3gdatmd point to proper model grid.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - the number of grids is taken from w3gdatmd, and needs to be
  !       set first with w3dimg.
  !
  !     - inflags1 dimensioning is hardwired as inflags1(-7:14) where lowest possible
  !       value of jfirst is jfirst=-7
  !
  !  6. switches :
  !
  !     !/s    enable subroutine tracing.
  !     !/t    enable test output
  !     !/tide use of tidal constituents
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !/
  !/ module private variable for checking error returns
  !/
  integer, private        :: istat
  !/
  !/ conventional declarations
  !/
  integer                 :: nidata = -1, iidata = -1
  integer                 :: jfirst = 1
  !/
  !/ data structure input
  !/
  type, public :: input
    integer               :: tfn(2,-7:10)
    integer               :: tc0(2)
    integer               :: tw0(2)
    integer               :: tu0(2)
    integer               :: tr0(2)
    integer               :: tdn(2)
    integer               :: tg0(2)
    real                  :: ga0
    real                  :: gd0
    real                  :: gan
    real                  :: gdn
    real, pointer         :: wx0(:,:)
    real, pointer         :: wy0(:,:)
    real, pointer         :: dt0(:,:)
    real, pointer         :: wxn(:,:)
    real, pointer         :: wyn(:,:)
    real, pointer         :: dtn(:,:)
    real, pointer         :: cx0(:,:)
    real, pointer         :: cy0(:,:)
    real, pointer         :: cxn(:,:)
    real, pointer         :: cyn(:,:)
    real, pointer         :: wlev(:,:)
    real, pointer         :: icei(:,:)
    real, pointer         :: ux0(:,:)
    real, pointer         :: uy0(:,:)
    real, pointer         :: uxn(:,:)
    real, pointer         :: uyn(:,:)
    real, pointer         :: rh0(:,:)
    real, pointer         :: rhn(:,:)
    real, pointer         :: bergi(:,:)
    real, pointer         :: mudt(:,:)
    real, pointer         :: mudv(:,:)
    real, pointer         :: mudd(:,:)
    real, pointer         :: icep1(:,:)
    real, pointer         :: icep2(:,:)
    real, pointer         :: icep3(:,:)
    real, pointer         :: icep4(:,:)
    real, pointer         :: icep5(:,:)
    logical               :: iinit
    ! note that if size of inflags1 is changed, then tflags in wminitmd.ftn
    !    also must be resized.
    logical               :: inflags1(-7:14)
    logical               :: flagsc(-7:14)
    logical               :: inflags2(-7:14)
  end type input
  !/
  !/ data storage
  !/
  type(input), target, allocatable :: inputs(:)
  !/
  !/ data aliasses for structure input(s)
  !/
  integer, pointer        :: tfn(:,:), tln(:), tc0(:), tcn(:),    &
       tw0(:), twn(:), tu0(:), tun(:),      &
       tin(:), tr0(:), trn(:), t0n(:),      &
       t1n(:), t2n(:), tdn(:), tg0(:),      &
       tgn(:), ttn(:), tvn(:), tzn(:),      &
       ti1(:), ti2(:), ti3(:), ti4(:), ti5(:)
  real, pointer           :: ga0, gd0, gan, gdn
  real, pointer           :: wx0(:,:), wy0(:,:), dt0(:,:),        &
       wxn(:,:), wyn(:,:), dtn(:,:),        &
       cx0(:,:), cy0(:,:), cxn(:,:),        &
       cyn(:,:), wlev(:,:), icei(:,:),      &
       ux0(:,:), uy0(:,:), uxn(:,:),        &
       uyn(:,:), rh0(:,:), rhn(:,:),        &
       bergi(:,:), mudt(:,:), mudv(:,:),    &
       mudd(:,:), icep1(:,:), icep2(:,:),   &
       icep3(:,:), icep4(:,:), icep5(:,:)
  logical, pointer        :: iinit
  logical, pointer        :: inflags1(:), inflags2(:), flagsc(:)
  logical, pointer        :: fllev, flcur, flwind, flice, fltaua, &
       flrhoa
  logical, pointer        :: flmth, flmvs, flmdn
  logical, pointer        :: flic1, flic2, flic3, flic4, flic5
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set up the number of grids to be used.
  !>
  !> @details use data stored in ngrids in w3gdatmd.
  !>
  !> @param[in] ndse  error output unit number.
  !> @param[in] ndst  test output unit number.
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3ninp ( ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    02-apr-2004 : origination.                        ( version 3.06 )
    !/    19-jul-2006 : adding auxiliary grids.             ( version 3.10 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    22-mar-2021 : momentum and air density support    ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     set up the number of grids to be used.
    !
    !  2. method :
    !
    !     use data stored in ngrids in w3gdatmd.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     any program that uses this grid structure.
    !
    !  6. error messages :
    !
    !     - error checks on previous setting of variable ngrids.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: ngrids, nauxgr
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001) ngrids
      call extcde (1)
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set variable and allocate arrays
    !
    allocate ( inputs(-nauxgr:ngrids), stat=istat )
    check_alloc_status ( istat )
    nidata = ngrids
    !
    ! -------------------------------------------------------------------- /
    ! 3.  initialize parameters
    !
    do i=-nauxgr, ngrids
      inputs(i)%tfn(1,:) = -1
      inputs(i)%tfn(2,:) =  0
      inputs(i)%tc0(1)   = -1
      inputs(i)%tc0(2)   =  0
      inputs(i)%tw0(1)   = -1
      inputs(i)%tw0(2)   =  0
      inputs(i)%tu0(1)   = -1
      inputs(i)%tu0(2)   =  0
      inputs(i)%tr0(1)   = -1
      inputs(i)%tr0(2)   =  0
      inputs(i)%tdn(1)   = -1
      inputs(i)%tdn(2)   =  0
      inputs(i)%tg0(1)   = -1
      inputs(i)%tg0(2)   =  0
      inputs(i)%ga0      = 0.
      inputs(i)%gd0      = 0.
      inputs(i)%gan      = 0.
      inputs(i)%gdn      = 0.
      inputs(i)%iinit    = .false.
      inputs(i)%inflags1 = .false.
      inputs(i)%inflags2 = .false.
      inputs(i)%flagsc   = .false.
    end do
    !
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3ninp : ngrids not yet set *** '/         &
         '                    ngrids = ',i10/                   &
         '                    run w3nmod first'/)
    !
    !/
    !/ end of w3ninp ----------------------------------------------------- /
    !/
  end subroutine w3ninp
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialize an individual data grid at the proper dimensions.
  !>
  !> @details allocate directly into the structure array. note that
  !>  this cannot be done through the pointer alias!
  !>
  !> @param[in] imod model number to point to.
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !> @param[in] flagstidein
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3dimi  ( imod, ndse, ndst, flagstidein )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    02-apr-2004 : origination.                        ( version 3.06 )
    !/    19-jul-2006 : adding auxiliary grids.             ( version 3.10 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    21-jun-2018 : add fswnd input for smc grid. jgli  ( version 6.04 )
    !/    22-mar-2021 : momentum and air density support    ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     initialize an individual data grid at the proper dimensions.
    !
    !  2. method :
    !
    !     allocate directly into the structure array. note that
    !     this cannot be done through the pointer alias!
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     main wave model drivers.
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - w3seti needs to be called after allocation to point to
    !       proper allocated arrays.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd,  only: ngrids, nauxgr, igrid, w3setg, nx, ny
    use w3servmd,  only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, ndse, ndst
    logical, intent(in), optional     :: flagstidein(4)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid
    logical                 :: flagstide(4)=.false.
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.nidata ) then
      write (ndse,1002) imod, -nauxgr, nidata
      call extcde (2)
    end if
    !
    if ( inputs(imod)%iinit ) then
      write (ndse,1003)
      call extcde (3)
    end if
    !
    !
    jgrid  = igrid
    if ( jgrid .ne. imod ) call w3setg ( imod, ndse, ndst )
    !
    ! -------------------------------------------------------------------- /
    ! 2.  allocate arrays
    !
    flic1  => inputs(imod)%inflags1(-7)
    flic2  => inputs(imod)%inflags1(-6)
    flic3  => inputs(imod)%inflags1(-5)
    flic4  => inputs(imod)%inflags1(-4)
    flic5  => inputs(imod)%inflags1(-3)
    !
    flmdn  => inputs(imod)%inflags1(-2)
    flmth  => inputs(imod)%inflags1(-1)
    flmvs  => inputs(imod)%inflags1(0)
    !
    fllev  => inputs(imod)%inflags1(1)
    flcur  => inputs(imod)%inflags1(2)
    flwind => inputs(imod)%inflags1(3)
    flice  => inputs(imod)%inflags1(4)
    fltaua => inputs(imod)%inflags1(5)
    flrhoa => inputs(imod)%inflags1(6)
    !
    ! notes: future improvement: flags for icepx should be
    !     "all or nothing" rather than 5 individual flags
    if ( flic1  ) then
      allocate ( inputs(imod)%icep1(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( flic2  ) then
      allocate ( inputs(imod)%icep2(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( flic3  ) then
      allocate ( inputs(imod)%icep3(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( flic4  ) then
      allocate ( inputs(imod)%icep4(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( flic5  ) then
      allocate ( inputs(imod)%icep5(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( flmdn  ) then
      allocate ( inputs(imod)%mudd(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( flmth  ) then
      allocate ( inputs(imod)%mudt(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    if ( flmvs  ) then
      allocate ( inputs(imod)%mudv(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( fllev  ) then
      allocate ( inputs(imod)%wlev(nx,ny), stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( flcur  ) then
        allocate ( inputs(imod)%cx0(nx,ny) ,              &
             inputs(imod)%cy0(nx,ny) ,              &
             inputs(imod)%cxn(nx,ny) ,              &
             inputs(imod)%cyn(nx,ny) , stat=istat )
      check_alloc_status ( istat )
    end if
    !
    !
    if ( flwind ) then
        allocate ( inputs(imod)%wx0(nx,ny) ,              &
             inputs(imod)%wy0(nx,ny) ,              &
             inputs(imod)%dt0(nx,ny) ,              &
             inputs(imod)%wxn(nx,ny) ,              &
             inputs(imod)%wyn(nx,ny) ,              &
             inputs(imod)%dtn(nx,ny) , stat=istat )
      check_alloc_status ( istat )
      inputs(imod)%dt0 = 0.
      inputs(imod)%dtn = 0.
    end if
    !
    if ( flice  ) then
      allocate ( inputs(imod)%icei(nx,ny),              &
           inputs(imod)%bergi(nx,ny), stat=istat )
      check_alloc_status ( istat )
      inputs(imod)%bergi = 0.
    end if
    !
    if ( fltaua  ) then
        allocate ( inputs(imod)%ux0(nx,ny) ,              &
             inputs(imod)%uy0(nx,ny) ,              &
             inputs(imod)%uxn(nx,ny) ,              &
             inputs(imod)%uyn(nx,ny) , stat=istat )
      check_alloc_status ( istat )
    end if
    !
    if ( flrhoa  ) then
        allocate ( inputs(imod)%rh0(nx,ny) ,              &
             inputs(imod)%rhn(nx,ny) , stat=istat )
      check_alloc_status ( istat )
    end if
    !
    inputs(imod)%iinit  = .true.
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3seti ( imod, ndse, ndst )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !
    !
    ! -------------------------------------------------------------------- /
    ! 5.  restore previous grid setting if necessary
    !
    if ( jgrid .ne. imod ) call w3setg ( jgrid, ndse, ndst )
    !
    return
    !
    ! check inputs for stresses
    if(fltaua) then
    end if
    !
    ! formats
    !
1001 format (/' *** error w3dimi : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dimi : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nauxgr = ',i10/                   &
         '                    nidata = ',i10/)
1003 format (/' *** error w3dimi : array(s) already allocated *** ')
    !
    !/
    !/ end of w3dimi ----------------------------------------------------- /
    !/
  end subroutine w3dimi
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief select one of the wavewatch iii grids / models.
  !>
  !> @details point pointers to the proper variables in the proper element of
  !>  the grids array.
  !>
  !> @param[in] imod  model number to point to.
  !> @param[in] ndse  error output unit number.
  !> @param[in] ndst  test output unit number.
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3seti ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    02-apr-2004 : origination.                        ( version 3.06 )
    !/    19-jul-2006 : adding auxiliary grids.             ( version 3.10 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    22-mar-2021 : momentum and air density support    ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     select one of the wavewatch iii grids / models.
    !
    !  2. method :
    !
    !     point pointers to the proper variables in the proper element of
    !     the grids array.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number to point to.
    !       ndse    int.   i   error output unit number.
    !       ndst    int.   i   test output unit number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !     any subroutine.
    !
    !  6. error messages :
    !
    !     many subroutines in the wavewatch system.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !  9. switches :
    !
    !     !/s    enable subroutine tracing.
    !     !/t    enable test output
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nauxgr
    !
    use w3servmd, only: extcde
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod, ndse, ndst
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( nidata .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.-nauxgr .or. imod.gt.nidata ) then
      write (ndse,1002) imod, -nauxgr, nidata
      call extcde (2)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set model numbers
    !
    iidata = imod
    !
    ! -------------------------------------------------------------------- /
    ! 3.  set pointers
    !
    tfn    => inputs(imod)%tfn
    tc0    => inputs(imod)%tc0
    tw0    => inputs(imod)%tw0
    tu0    => inputs(imod)%tu0
    tr0    => inputs(imod)%tr0
    tg0    => inputs(imod)%tg0
    tdn    => inputs(imod)%tdn
    !
    ti1    => inputs(imod)%tfn(:,-7)
    ti2    => inputs(imod)%tfn(:,-6)
    ti3    => inputs(imod)%tfn(:,-5)
    ti4    => inputs(imod)%tfn(:,-4)
    ti5    => inputs(imod)%tfn(:,-3)
    !
    tzn    => inputs(imod)%tfn(:,-2)
    ttn    => inputs(imod)%tfn(:,-1)
    tvn    => inputs(imod)%tfn(:,0)
    !
    tln    => inputs(imod)%tfn(:,1)
    tcn    => inputs(imod)%tfn(:,2)
    twn    => inputs(imod)%tfn(:,3)
    tin    => inputs(imod)%tfn(:,4)
    tun    => inputs(imod)%tfn(:,5)
    trn    => inputs(imod)%tfn(:,6)
    t0n    => inputs(imod)%tfn(:,7)
    t1n    => inputs(imod)%tfn(:,8)
    t2n    => inputs(imod)%tfn(:,9)
    tgn    => inputs(imod)%tfn(:,10)
    !
    ga0    => inputs(imod)%ga0
    gd0    => inputs(imod)%gd0
    gan    => inputs(imod)%gan
    gdn    => inputs(imod)%gdn
    !
    iinit  => inputs(imod)%iinit
    inflags1  => inputs(imod)%inflags1
    inflags2  => inputs(imod)%inflags2
    flagsc => inputs(imod)%flagsc
    !
    flic1  => inputs(imod)%inflags1(-7)
    flic2  => inputs(imod)%inflags1(-6)
    flic3  => inputs(imod)%inflags1(-5)
    flic4  => inputs(imod)%inflags1(-4)
    flic5  => inputs(imod)%inflags1(-3)
    !
    flmdn  => inputs(imod)%inflags1(-2)
    flmth  => inputs(imod)%inflags1(-1)
    flmvs  => inputs(imod)%inflags1(0)
    !
    fllev  => inputs(imod)%inflags1(1)
    flcur  => inputs(imod)%inflags1(2)
    flwind => inputs(imod)%inflags1(3)
    flice  => inputs(imod)%inflags1(4)
    fltaua => inputs(imod)%inflags1(5)
    flrhoa => inputs(imod)%inflags1(6)
    !
    if ( iinit ) then
      !
      if ( flic1  ) then
        icep1  => inputs(imod)%icep1
      end if
      if ( flic2  ) then
        icep2  => inputs(imod)%icep2
      end if
      if ( flic3  ) then
        icep3  => inputs(imod)%icep3
      end if
      if ( flic4  ) then
        icep4  => inputs(imod)%icep4
      end if
      if ( flic5  ) then
        icep5  => inputs(imod)%icep5
      end if
      !
      if ( flmdn  ) then
        mudd   => inputs(imod)%mudd
      end if
      if ( flmth  ) then
        mudt   => inputs(imod)%mudt
      end if
      if ( flmvs  ) then
        mudv   => inputs(imod)%mudv
      end if
      !
      if ( fllev  ) then
        wlev   => inputs(imod)%wlev
      end if
      !
      if ( flcur  ) then
        cx0    => inputs(imod)%cx0
        cy0    => inputs(imod)%cy0
        cxn    => inputs(imod)%cxn
        cyn    => inputs(imod)%cyn
      end if
      !
      if ( flwind  ) then
        wx0    => inputs(imod)%wx0
        wy0    => inputs(imod)%wy0
        dt0    => inputs(imod)%dt0
        wxn    => inputs(imod)%wxn
        wyn    => inputs(imod)%wyn
        dtn    => inputs(imod)%dtn
      end if
      !
      if ( flice  ) then
        icei   => inputs(imod)%icei
        bergi  => inputs(imod)%bergi
      end if
      !
      if ( fltaua  ) then
        ux0    => inputs(imod)%ux0
        uy0    => inputs(imod)%uy0
        uxn    => inputs(imod)%uxn
        uyn    => inputs(imod)%uyn
      end if
      !
      if ( flrhoa  ) then
        rh0    => inputs(imod)%rh0
        rhn    => inputs(imod)%rhn
      end if
      !
    end if
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3seti : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3seti : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nauxgr = ',i10/                   &
         '                    nidata = ',i10/)
    !
    !/
    !/ end of w3seti ----------------------------------------------------- /
    !/
  end subroutine w3seti
  !/
  !/ end of module w3idatmd -------------------------------------------- /
  !/
end module w3idatmd
