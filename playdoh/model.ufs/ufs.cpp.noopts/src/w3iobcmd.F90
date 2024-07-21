!> @file
!> @brief processing of boundary data output.
!>
!> @author h. l. tolman @date 01-mar-2018
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
!> @brief processing of boundary data output.
!>
!> @author h. l. tolman @date 01-mar-2018
!>
module w3iobcmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         01-mar-2018 |
  !/                  +-----------------------------------+
  !/
  !/    see subroutine for update log.
  !/
  !/    copyright 2009-2010 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     processing of boundary data output.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      verbptbc  c*10  public   nest file version number.
  !      idstrbc   c*32  public   restart file id string.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3iobc    subr. public   boundary data io.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3seto, w3setg, w3setw, w3seta, w3dmo5
  !                subr. w3xdatmd manage data structures.
  !      w3cspc    subr. w3cspcmd spectral grid conversion.
  !      w3lltoeq  subr. w3cspcmd standard to rotated lat/lon conversion.
  !      strace    subr. w3servmd subroutine tracing.
  !      extcde    subr. w3servmd abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     none.
  !
  !  6. switches :
  !
  !     see subroutine w3iobc.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  public
  !/
  !/ public variables  (id strings)
  !/
  character(len=10), parameter :: verbptbc = '2018-03-01'
  character(len=32), parameter ::                        &
       idstrbc  = 'wavewatch iii boundary data file'
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief write/read boundary conditions file(s).
  !>
  !> @details the file(s) are opened within the routine, the names are
  !>  pre-defined as nest.filext for the input file and nest1.filext
  !>  through nest9.filext for up to 9 output files.
  !>
  !> @param[inout] inxout  test string for read/write.
  !> @param[inout] ndsb    data set unit number.
  !> @param[inout] time1   present time (w), time of first field (r).
  !> @param[inout] time2   time of second field.
  !> @param[inout] iotst   test indictor for reading.
  !> @param[inout] imod    optional grid number, defaults to 1.
  !>
  !> @author h. l. tolman  @date 20-jan-2017
  !>
  subroutine w3iobc ( inxout, ndsb, time1, time2, iotst, imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    12-jan-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    20-may-1999 : remove read bug for ipbp and rdbp   ( see web page )
    !/    30-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    13-dec-2004 : multiple grid version.              ( version 3.06 )
    !/    19-sep-2005 : allow for change of spec. res.      ( version 3.08 )
    !/                  (on read only).
    !/    30-sep-2005 : add 'dump' option.                  ( version 3.08 )
    !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
    !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    28-jul-2010 : moving nki, nthi, xfri, fr1i and
    !/                  th1i to w3odatmd.                   ( version 3.14.3 )
    !/    31-oct-2010 : implementing unstructured grid      ( version 3.14.3 )
    !/                  (a. roland and f. ardhuin)
    !/    05-apr-2011 : moved the w3cspc call into loop     ( version 3.14.3 )
    !/    12-jun-2012 : add /rtd option or rotated grid option.
    !/                  (jian-guo li)                       ( version 4.06 )
    !/    03-jul-2013 : corrected abpin indices             ( version 4.11 )
    !/    14-jan-2014 : corrected abpin indices for w3cspc  ( version 4.18 )
    !/    20-jan-2017 : allow input boundary points to lie outside the grid
    !/                  within a distance of 0.1 times the grid cell size.
    !/                  (t.j. campbell, nrl)                ( version 6.02 )
    !/    01-mar-2018 : rotate boundary points and directions
    !/                  of input spectra for rotated grids  ( version 6.02 )
    !/    07-oct-2019 : rtd option with standard lat-lon
    !/                  grid when nesting to rotated grid   ( version 7.11 )
    !/
    !  1. purpose :
    !
    !     write/read boundary conditions file(s).
    !
    !  2. method :
    !
    !     the file(s) are opened within the routine, the names are
    !     pre-defined as nest.filext for the input file and nest1.filext
    !     through nest9.filext for up to 9 output files.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*)  i   test string for read/write, valid are:
    !                          'read', 'write' or 'dump'.
    !       ndsb    int.   i   data set unit number.
    !       time1   i.a.  i/o  present time.                          (w)
    !                          time of first field.                   (r)
    !       time2   i.a.   o   time of second field.                  (r)
    !       iotst   int.   o   test indictor for reading.
    !                           1 : file not found.
    !                           0 : fields read.
    !                          -1 : past end of file.
    !       imod    int.   i   optional grid number, defaults to 1.
    !     ----------------------------------------------------------------
    !                                            (w) used for write only
    !                                            (r) used for write only
    !
    !  4. subroutines used :
    !
    !     see module documentation.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       tests on inxout, file status and data present in file.
    !
    !  7. remarks :
    !
    !     - array dimensions are tested in w3iogr.
    !     - spectra are stored as frequency (sigma) spectra to guarantee
    !       conservation under grid transformation.
    !     - at the moment it is mplicitly assumed that the number of
    !       spectral components is larger that the number of spectra
    !       per time step per file.
    !     - dump option used in multi-grid model.
    !
    !  8. structure :
    !
    !       see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/s     enable subroutine tracing.
    !     !/t     general test output.
    !     !/t0    point info test output.
    !     !/t1    wave heights at input/output points.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !
    use w3gdatmd, only: w3setg
    use w3wdatmd, only: w3setw
    use w3adatmd, only: w3seta
    use w3odatmd, only: w3seto, w3dmo5
    use w3cspcmd, only: w3cspc
    use w3triamd, only: w3nestug
    !
    use w3gdatmd, only: nk, nth, nspec, nsea, nseal, nx, ny,        &
         x0, y0, sx, sy, gsu, mapsta, mapfs, mapsf,  &
         xfr, fr1, sig2, th, dth, filext, fachfe,    &
         gtype, ungtype, smctype
    use w3gdatmd, only: dxymax
    use w3wdatmd, only: va
    use w3adatmd, only: cg
    use w3odatmd, only: ndse, ndst, iaproc, naproc, naperr, napbpt, &
         nbi, nbi2, nfbpo, nbo, nbo2, ndsl,          &
         nki, nthi, xfri, fr1i, th1i,                &
         ipbpi, isbpi, xbpi, ybpi, rdbpi,            &
         ipbpo, isbpo, xbpo, ybpo, rdbpo,            &
         abpi0, abpin, abpos, flbpi, filer, filew,   &
         filed, spconv, fnmpre
    use w3gsrumd
    !
    use w3servmd, only: extcde
    !
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: ndsb
    integer, intent(inout)        :: time1(2)
    integer, intent(out)          :: time2(2), iotst
    integer, intent(in), optional :: imod
    character, intent(in)         :: inxout*(*)
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ifile, ierr, i, j, ix, iy, isea,     &
         ip, isp, npts, isout, is, igrd
    real, allocatable       :: tmpspc(:,:)
    logical                 :: flok
    character(len=18)       :: filen
    character(len=10)       :: vertst
    character(len=32)       :: idtst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    iotst  = 0
    !
    ! test parameter list input ------------------------------------------ *
    !
    if ( present(imod) ) then
      igrd   = imod
    else
      igrd   = 1
    end if
    !
    call w3seto ( igrd, ndse, ndst )
    call w3setg ( igrd, ndse, ndst )
    call w3setw ( igrd, ndse, ndst )
    call w3seta ( igrd, ndse, ndst )
    !
    if (inxout.ne.'read' .and. inxout.ne.'write' .and.              &
         inxout.ne.'dump' ) then
      if ( iaproc .eq. naperr ) write (ndse,900) inxout
      call extcde ( 1 )
    end if
    !
    !
    ! open file ---------------------------------------------------------- *
    !
    i      = len_trim(filext)
    j      = len_trim(fnmpre)
    !
    if ( inxout.eq.'read'  .and. filer ) then
      write (filen,'(a5,a)') 'nest.', filext(:i)
      open (ndsb,file=fnmpre(:j)//filen(:5+i),form='unformatted', convert=file_endian, &
           err=801,iostat=ierr,status='old')
    end if
    !
    if ( inxout.eq.'write' .and. filew ) then
      do ifile=1, nfbpo
        ndsl(ifile) = ndsb + ifile - 1
        write (filen,'(a4,i1,a1,a)') 'nest', ifile, '.',          &
             filext(:i)
        open (ndsl(ifile),file=fnmpre(:j)//filen(:6+i),           &
             form='unformatted', convert=file_endian,err=800,iostat=ierr)
      end do
    end if
    !
    if ( inxout.eq.'dump'  .and. filed ) then
      write (filen,'(a5,a)') 'nest.', filext(:i)
      open (ndsb,file=fnmpre(:j)//filen(:5+i),form='unformatted', convert=file_endian, &
           err=800,iostat=ierr)
    end if
    !
    ! test info ---------------------------------------------------------- *
    ! ( new files only )
    ! ... writing
    !
    if ( inxout.eq.'write' .and. filew ) then
      if ( iaproc .eq. napbpt ) then
        do ifile=1, nfbpo
          write (ndsl(ifile))                                   &
               idstrbc, verbptbc, nk, nth, xfr, fr1, th(1),    &
               nbo(ifile)-nbo(ifile-1)
          !
          !
          write (ndsl(ifile))                                   &
               (xbpo(i),i=nbo(ifile-1)+1,nbo(ifile)),         &
               (ybpo(i),i=nbo(ifile-1)+1,nbo(ifile)),         &
               ((ipbpo(i,j),i=nbo(ifile-1)+1,nbo(ifile)),j=1,4),&
               ((rdbpo(i,j),i=nbo(ifile-1)+1,nbo(ifile)),j=1,4)
          !
          !
        end do
      end if
    end if
    !
    ! ... dumping
    !
    if ( inxout.eq.'dump' .and. filed ) then
      if ( iaproc .eq. napbpt ) then
        write (ndsb) idstrbc, verbptbc, nk, nth, xfr, fr1, th(1), nbi
        !
        !
        write (ndsb) (xbpi(i),i=1,nbi), (ybpi(i),i=1,nbi),      &
             ((ipbpi(i,j),i=1,nbi),j=1,4),              &
             ((rdbpi(i,j),i=1,nbi),j=1,4)
        !
        !
      end if
    end if
    !
    ! ... reading
    !
    if ( inxout.eq.'read' .and. filer ) then
      !
      read (ndsb,err=803,iostat=ierr)                             &
           idtst, vertst, nki, nthi, xfri, fr1i, th1i, nbi
      !
      !
      if ( idtst .ne. idstrbc ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,901) idtst, idstrbc
        call extcde ( 10 )
      end if
      if ( vertst .ne. verbptbc ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,902) vertst, verbptbc
        call extcde ( 11 )
      end if
      !
      ! determines if the spectrum in nest file needs to be converted
      !
      spconv = nki.ne.nk .or. nthi.ne.nth .or.                    &
           abs(xfri/xfr-1.).gt.0.01 .or.                      &
           abs(fr1i/fr1-1.).gt.0.01 .or.                      &
           abs(th1i-th(1)).gt.0.01*dth
      !
      call w3dmo5 ( igrd, ndse, ndst, 1 )
      !
      read (ndsb,err=803,iostat=ierr)                             &
           (xbpi(i),i=1,nbi), (ybpi(i),i=1,nbi),                   &
           ((ipbpi(i,j),i=1,nbi),j=1,4),                           &
           ((rdbpi(i,j),i=1,nbi),j=1,4)
      !
      flok   = .true.
      if (gtype .eq. ungtype) then
        call w3nestug(dxymax,flok)
      else
        do i=1, nbi
          ! w3gftp: find the nearest grid point to the input boundary point
          ! dcin=0.1 is the distance outside of source grid in units of
          ! cell width to treat target point as inside the source grid.
          if ( w3gfpt( gsu, xbpi(i), ybpi(i), ix, iy, dcin=0.1 ) ) then
            if ( abs(mapsta(iy,ix)) .ne. 2 ) then
              if ( iaproc .eq. naperr )                         &
                   write (ndse,909) ix, iy, abs(mapsta(iy,ix))
              flok   = .false.
            end if
          else
            if ( iaproc .eq. naperr )                             &
                 write (ndse,910) i, xbpi(i), ybpi(i)
            call extcde ( 12 )
          end if
          isbpi(i) = mapfs(iy,ix)
        end do
      end if
      !
      !
      if ( .not.flok ) call extcde ( 20 )
      !
      do isea=1, nsea
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        if ( abs(mapsta(iy,ix)) .eq. 2 ) then
          flok   = .false.
          do i=1, nbi
            if ( isea .eq. isbpi(i) ) flok = .true.
          end do
          if ( .not.flok .and. iaproc.eq.naperr )               &
               write (ndse,911) ix, iy
        end if
      end do
      !
      !     read first time and allocate abpi0/n
      !
      read (ndsb,end=810,err=810) time2, nbi2
      backspace (ndsb)
      call w3dmo5 ( igrd, ndse, ndst, 3 )
      !
    end if
    !
    ! save previous spectra on read -------------------------------------- *
    !
    if ( inxout.eq.'read' .and. .not.filer ) then
      time1  = time2
      abpi0(:,1:nbi2) = abpin(:,1:nbi2)
    end if
    !
    ! time --------------------------------------------------------------- *
    !
    if ( inxout .eq. 'write'  ) then
      do ifile=1, nfbpo
        npts   = nbo2(ifile) - nbo2(ifile-1)
        write (ndsl(ifile)) time1, npts
      end do
    end if
    !
    if ( inxout .eq. 'dump'  ) then
      write (ndsb) time1, nbi2
    end if
    !
    if ( inxout .eq. 'read'  ) then
      read (ndsb,err=810,end=810) time2, nbi2
    end if
    !
    ! spectra ------------------------------------------------------------ *
    !
    if ( inxout .eq. 'write' ) then
      !
      !
      do ifile=1, nfbpo
        do isout=nbo2(ifile-1)+1, nbo2(ifile)
          !
          isea   = isbpo(isout)
          !
          ! ... shared memory version data gather
          !
          !
          ! ... distributed memory version data gather
          !   ( array pre-filled in w3wave )
          !
          !
          !
          write (ndsl(ifile)) (abpos(is,isout),is=1,nspec)
          !
          !
        end do
      end do
      !
    end if
    !
    if ( inxout .eq. 'dump' ) then
      do i=1, nbi2
        write (ndsb) abpin(:,i)
      end do
    end if
    !
    if ( inxout .eq. 'read' ) then
      !
      if ( .not. spconv ) then
        do ip=1, nbi2
          read (ndsb,err=803,iostat=ierr) abpin(:,ip)
        end do
      else
        !
        ! in this case the spectral resolution is not compatible and
        ! the spectrum tmpspc in nest file must be re-gridded into abpin to fit the model run
        ! spectral conversion is done by w3cspc in w3cspcmd.ftn
        !
        allocate ( tmpspc(nki*nthi,nbi2) )
        do ip=1, nbi2
          read (ndsb,err=803,iostat=ierr) tmpspc(:,ip)
        end do
        call w3cspc ( tmpspc     ,    nki, nthi, xfri, fr1i, th1i, &
             abpin(:,1:nbi2),nk,  nth,  xfr,  fr1,  th(1),&
             nbi2, ndst, ndse, fachfe )
        deallocate ( tmpspc )
      end if
      !
      !
    end if
    !
    ! set first spectra on first read ------------------------------------ *
    !
    if ( inxout.eq.'read' .and. filer ) then
      time1 = time2
      do ip=1, nbi2
        abpi0(:,ip) = abpin(:,ip)
      end do
      abpi0(:,0) = 0.
      abpin(:,0) = 0.
    end if
    !
    ! reset flags -------------------------------------------------------- *
    !
    if ( inxout .eq. 'write' ) filew  = .false.
    if ( inxout .eq. 'dump'  ) filed  = .false.
    if ( inxout .eq. 'read'  ) filer  = .false.
    !
    return
    !
    ! escape locations io errors
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) filen, ierr
    call extcde ( 40 )
    !
801 continue
    if ( iaproc .eq. naperr ) write (ndse,1001) imod
    iotst  = 1
    flbpi  = .false.
    return
    !
802 continue
    if ( iaproc .eq. naperr ) write (ndse,1002)
    call extcde ( 41 )
    !
803 continue
    if ( iaproc .eq. naperr ) write (ndse,1003) ierr
    call extcde ( 42 )
    !
810 continue
    if ( filer ) then
      if ( iaproc .eq. naperr ) write (ndse,1010)
      call extcde ( 43 )
    end if
    !
    time1(1) = time2(1)
    time1(2) = time2(2)
    do ip=0, nbi2
      do isp=1, nspec
        abpi0(isp,ip) = abpin(isp,ip)
      end do
    end do
    !
    iotst  = -1
    flbpi  = .false.
    return
    !
    ! formats
    !
900 format (/' *** wavewatch iii error in w3iobc :'/                &
         '     illegal inxout value: ',a/)
901 format (/' *** wavewatch iii error in w3iobc :'/                &
         '     illegal idstrbc, read : ',a/                     &
         '                  check : ',a/)
902 format (/' *** wavewatch iii error in w3iobc :'/                &
         '     illegal verogr, read : ',a/                      &
         '                   check : ',a/)
    !
909 format (/' *** wavewatch iii error in w3iobc :'/                &
         '     point',2i4,' not active sea point (',i1,')')
910 format (/' *** wavewatch iii error in w3iobc :'/                &
         '     point',i4,2e14.6,' not located in grid')
911 format ( ' *** wavewatch iii warning : point',2i7,              &
         ' will not be updated')
920 format (/' *** smctype mapped boundary cells:'/ ((i8,2f9.3)) )
    !
1000 format (/' *** wavewatch iii error in w3iobc : '/               &
         '     error in opening file ',a/                       &
         '     iostat =',i5/)
    !
    ! note: this 1001 error can occur when multi-grid time steps are not
    !       compatible.
1001 format (/' *** wavewatch iii warning in w3iobc : '/             &
         '     input file with boundary conditions not found'/  &
         '     boundary conditions will not be updated ',i5/)
1002 format (/' *** wavewatch iii error in w3iobc : '/               &
         '     premature end of file'/)
1003 format (/' *** wavewatch iii error in w3iobc : '/               &
         '     error in reading from file'/                     &
         '     iostat =',i5/)
    !
1010 format (/' *** wavewatch iii error in w3iobc : '/               &
         '     no data in input file'/)
    !
    !
    !
    !
    !/
    !/ end of w3iobc ----------------------------------------------------- /
    !/
  end subroutine w3iobc
  !/
  !/ end of module w3iobcmd -------------------------------------------- /
  !/
end module w3iobcmd
