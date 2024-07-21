!> @file
!> @brief contains module w3wdatmd.
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
!/ ------------------------------------------------------------------- /
!>
!> @brief define data structures to set up wave model dynamic data for
!>  several models simultaneously.
!>
!> @details the number of grids is taken from w3gdatmd, and needs to be
!>  set first with w3dimg.
!>
!> @author h. l. tolman  @date 22-mar-2021
!>
module w3wdatmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    22-oct-2004 : origination.                        ( version 3.06 )
  !/    13-jun-2006 : allocate va consistent with mpi     ( version 3.09 )
  !/                  data types and initialize as needed.
  !/    05-jul-2006 : consolidate stress vector.          ( version 3.09 )
  !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    16-may-2010 : add iceberg damping                 ( version 3.14.4 )
  !/    14-nov-2013 : initialize ust and ustdir.          ( version 4.13 )
  !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
  !/    06-jun-2018 : add pdlib/setup/debuginit           ( version 6.04 )
  !/    22-mar-2021 : support for variable air density    ( version 7.13 )
  !/    28-jun-2021 : gke nl5 parameters (q. liu)         ( version 7.13 )
  !/
  !/    copyright 2009-2013 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     define data structures to set up wave model dynamic data for
  !     several models simultaneously.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      nwdata    int.  public   number of models in array dim.
  !      iwdata    int.  public   selected model for output, init. at -1.
  !      wdata     type  public   basic data structure.
  !      wdatas    wdata public   array of data structures.
  !     ----------------------------------------------------------------
  !
  !     all elements of wdata are aliased to pointers with the same
  !     name. these pointers are defined as :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      time      i.a.  public   valid time for spectra.
  !      time00    i.a.  public   initial time
  !      timeend   i.a.  public   final time
  !      qi5tbeg   i.a.  public   initial time for nl5 (absol. time)
  !      qr5tim0   r.a.  public   previous time step t0 (relat. time)
  !      qr5cvk0   r.a.  public   cvk @ t0
  !      qc5int0   c.a.  public   inpqr (time integral) @ t0
  !      qr5tmix   r.a.  public   previous time for phase mixing
  !      tlev      i.a.  public   valid time for water levels.
  !      tice      i.a.  public   valid time for ice concentration
  !      trho      i.a.  public   valid time for air density
  !      tic1      i.a.  public   valid time for ice thickness
  !      tic5      i.a.  public   valid time for ice floe
  !      va        r.a.  public   storage array for spectra.
  !      wlv       r.a.  public   water levels.
  !      ice       r.a.  public   ice coverage.
  !      rhoair    r.a.  public   air density
  !      iceh      r.a.  public   ice thickness.
  !      icef      r.a.  public   ice flow maximum diameter.
  !      icedmax   r.a.  public   ice flow maximum diameter for updates.
  !      berg      r.a.  public   iceberg damping.
  !      ust       r.a.  public   friction velocity (absolute).
  !      ustdir    r.a.  public   friction velocity direction.
  !      asf       r.a.  public   stability correction factor.
  !      fpis      r.a.  public   input peak frequencies.
  !      dinit     log.  public   flag for array initialization.
  !      fl_all    log.  public   flag for initializing all arrays,
  !                               otherwise va is skipped.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3ndat    subr. public   set number of grids/models.
  !      w3dimw    subr. public   set dimensions of arrays.
  !      w3setw    subr. public   point to selected grid / model.
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
  !  6. switches :
  !
  !     !/s    enable subroutine tracing.
  !     !/t    enable test output
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
  integer                 :: nwdata = -1, iwdata = -1
  !/
  !/ data structures
  !/
  type wdata
    integer               :: time(2), tlev(2), tice(2), trho(2),  &
         tic1(2), tic5(2)
    real, pointer         :: va(:,:), wlv(:), ice(:), rhoair(:),   &
         ust(:), ustdir(:), asf(:), fpis(:),  &
         berg(:), iceh(:), icef(:), icedmax(:)
    logical               :: dinit, fl_all
  end type wdata
  !
  !/
  !/ data storage
  !/
  type(wdata), target, allocatable :: wdatas(:)
  !/
  !/ data aliasses for structure wdata(s)
  !/
  integer, pointer        :: time(:), tlev(:), tice(:), trho(:),  &
       tic1(:), tic5(:)
  real, pointer           :: va(:,:), wlv(:), ice(:), rhoair(:),  &
       ust(:), ustdir(:), asf(:), fpis(:),  &
       berg(:), iceh(:), icef(:), icedmax(:)
  logical, pointer        :: dinit, fl_all
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set up the number of grids to be used.
  !>
  !> @details use data stored in ngrids in w3gdatmd.
  !>
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !>
  !> @author h. l. tolman  @date 10-dec-2014
  !>
  subroutine w3ndat ( ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-dec-2014 !
    !/                  +-----------------------------------+
    !/
    !/    31-mar-2004 : origination.                        ( version 3.06 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
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
    use w3gdatmd, only: ngrids
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
    !/
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
    allocate ( wdatas(0:ngrids), stat=istat )
    check_alloc_status ( istat )
    nwdata = ngrids
    !
    ! -------------------------------------------------------------------- /
    ! 3.  initialize parameters
    !
    do i=0, ngrids
      wdatas(i)%dinit  = .false.
      wdatas(i)%fl_all = .false.
    end do
    !
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3ndat : ngrids not yet set *** '/         &
         '                    ngrids = ',i10/                   &
         '                    run w3nmod first'/)
    !
    !/
    !/ end of w3ndat ----------------------------------------------------- /
    !/
  end subroutine w3ndat
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief  initialize an individual data grid at the proper dimensions.
  !>
  !> @details allocate directly into the structure array. note that
  !>  this cannot be done through the pointer alias!
  !>
  !> @param[in] imod model number to point to.
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !> @param[in] f_only flag for initializing field arrays only.
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3dimw  ( imod, ndse, ndst, f_only )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    22-oct-2004 : origination.                        ( version 3.06 )
    !/    13-jun-2006 : allocate va consistent with mpi     ( version 3.09 )
    !/                  data types and initialize as needed.
    !/    05-jul-2006 : consolidate stress vector.          ( version 3.09 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    14-nov-2013 : initialize ust and ustdir.          ( version 4.13 )
    !/    10-dec-2014 : add checks for allocate status      ( version 5.04 )
    !/    22-mar-2021 : support for variable air density    ( version 7.13 )
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
    !       f_only  l.o.   i   flag for initializing field arrays only.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3iogo    subr. w3iogomd grid output io routine.
    !      w3iors    subr. w3iorsmd restart file io routine.
    !      ww3_shel  prog.   n/a    main wave model driver.
    !      ww3_strt  prog.   n/a    initial conditions program.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - check on input parameters.
    !     - check on previous allocation.
    !
    !  7. remarks :
    !
    !     - w3setw needs to be called after allocation to point to
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
    use w3gdatmd, only: ngrids, igrid, w3setg, nspec, nsea, nseal, grids
    use w3odatmd, only: naproc, iaproc
    use w3servmd, only: extcde
    use constants, only : lpdlib, dair
    use w3parall, only: set_up_nseal_nsealm, lsloc
    !
    implicit none
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, ndse, ndst
    logical, intent(in), optional :: f_only
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: jgrid, nsealm, nseatm
    integer                 :: nseal_dummy, isea
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( present(f_only) ) then
      fl_all = .not. f_only
    else
      fl_all = .true.
    end if
    !
    if ( ngrids .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.1 .or. imod.gt.nwdata ) then
      write (ndse,1002) imod, nwdata
      call extcde (2)
    end if
    !
    if ( wdatas(imod)%dinit ) then
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
    call set_up_nseal_nsealm(nseal_dummy, nsealm)
    nseatm = nsealm * naproc
    !
    if ( fl_all ) then
      allocate ( wdatas(imod)%va(nspec,0:nsealm), stat=istat ); wdatas(imod)%va = 0.
      check_alloc_status ( istat )
      !
      ! * four arrays for nl5 (ql)
      ! * afaik, the set up of qr5tim0, qr5cvk0, qc5int0 should be similar
      ! * to va, though i am not really clear about how fl_all works.
      ! *
      !
      ! * initialized nl5 arrays with zero (ql)
      !
      !
      if ( nseal .ne. nsealm ) then
        do isea=nseal+1,nsealm
          wdatas(imod)%va(:,isea) = 0.
          !
        end do
      end if
    end if
    !
    ! ice, iceh, icef must be defined from 0:nsea
    allocate ( wdatas(imod)%wlv(nsea),                              &
         wdatas(imod)%ice(0:nsea),                            &
         wdatas(imod)%rhoair(nsea),                           &
         wdatas(imod)%berg(nsea),                             &
         wdatas(imod)%iceh(0:nsea),                           &
         wdatas(imod)%icef(0:nsea),                           &
         wdatas(imod)%icedmax(nsea),                          &
         wdatas(imod)%ust(0:nseatm),                          &
         wdatas(imod)%ustdir(0:nseatm),                       &
         wdatas(imod)%asf(nseatm),                            &
         wdatas(imod)%fpis(nseatm), stat=istat                )
    check_alloc_status ( istat )
    wdatas(imod)%wlv   (:) = 0.
    wdatas(imod)%ice   (0:nsea) = 0.
    wdatas(imod)%rhoair(:) = dair
    wdatas(imod)%berg  (:) = 0.
    wdatas(imod)%iceh  (0:nsea) = grids(imod)%iicehinit
    wdatas(imod)%icef  (0:nsea) = 1000.
    wdatas(imod)%icedmax(:) = 1000.
    wdatas(imod)%ust   (0:nseatm) = 1.e-5
    wdatas(imod)%ustdir(0:nseatm) = 0.
    wdatas(imod)%asf   (:) = 0.
    wdatas(imod)%fpis  (:) = 0.
    wdatas(imod)%dinit     = .true.
    call w3setw ( imod, ndse, ndst )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 5.  restore previous grid setting if necessary
    !
    if ( jgrid .ne. imod ) call w3setg ( jgrid, ndse, ndst )
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3dimw : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3dimw : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nwdata = ',i10/)
1003 format (/' *** error w3dimw : array(s) already allocated *** ')
    !
    !
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to allocated arrays
    !
    call w3setw ( imod, ndse, ndst )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 4.  update counters in grid
    !/
    !/ end of w3dimw ----------------------------------------------------- /
    !/
  end subroutine w3dimw
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief select one of the wavewatch iii grids / models.
  !>
  !> @details point pointers to the proper variables in the proper element of
  !>  the grids array.
  !>
  !> @param[in] imod model number to point to.
  !> @param[in] ndse error output unit number.
  !> @param[in] ndst test output unit number.
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3setw ( imod, ndse, ndst )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 !
    !/                  +-----------------------------------+
    !/
    !/    31-mar-2004 : origination.                        ( version 3.06 )
    !/    05-jul-2006 : consolidate stress vector.          ( version 3.09 )
    !/    04-oct-2006 : add filter to array pointers.       ( version 3.10 )
    !/    22-mar-2021 : support for variable air density    ( version 7.13 )
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
    !     many subroutines in the wavewatch system.
    !
    !  6. error messages :
    !
    !     checks on parameter list imod.
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
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 1.  test input and module status
    !
    if ( nwdata .eq. -1 ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    if ( imod.lt.0 .or. imod.gt.nwdata ) then
      write (ndse,1002) imod, nwdata
      call extcde (2)
    end if
    !
    !
    ! -------------------------------------------------------------------- /
    ! 2.  set model numbers
    !
    iwdata = imod
    !
    ! -------------------------------------------------------------------- /
    ! 3.  set pointers
    !
    time   => wdatas(imod)%time
    tlev   => wdatas(imod)%tlev
    tice   => wdatas(imod)%tice
    trho   => wdatas(imod)%trho
    tic1   => wdatas(imod)%tic1
    tic5   => wdatas(imod)%tic5
    dinit  => wdatas(imod)%dinit
    fl_all => wdatas(imod)%fl_all
    !
    if ( dinit ) then
      if ( fl_all ) then
        va     => wdatas(imod)%va
      end if
      wlv    => wdatas(imod)%wlv
      ice    => wdatas(imod)%ice
      rhoair => wdatas(imod)%rhoair
      berg   => wdatas(imod)%berg
      iceh   => wdatas(imod)%iceh
      icef   => wdatas(imod)%icef
      icedmax=> wdatas(imod)%icedmax
      ust    => wdatas(imod)%ust
      ustdir => wdatas(imod)%ustdir
      asf    => wdatas(imod)%asf
      fpis   => wdatas(imod)%fpis
    end if
    !
    return
    !
    ! formats
    !
1001 format (/' *** error w3setw : grids not initialized *** '/      &
         '                    run w3nmod first '/)
1002 format (/' *** error w3setw : illegal model number *** '/       &
         '                    imod   = ',i10/                   &
         '                    nwdata = ',i10/)
    !
    !/
    !/ end of w3setw ----------------------------------------------------- /
    !/
  end subroutine w3setw
  !/
  !/ end of module w3wdatmd -------------------------------------------- /
  !/
end module w3wdatmd
