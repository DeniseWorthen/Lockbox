!> @file
!> @brief generate track output.
!>
!> @author h. l. tolman  @date 26-dec-2012
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
!> @brief generate track output.
!>
!> @author h. l. tolman  @date 26-dec-2012
!>
module w3iotrmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         26-dec-2012 |
  !/                  +-----------------------------------+
  !
  !/    see subroutine for update history.
  !/
  !  1. purpose :
  !
  !     generate track output.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      vertrk    c*10  private  version number of routine.
  !      idstri    c*34  private  id string input file.
  !      idstro    c*34  private  id string output file.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3iotr    subr. public   track output subroutine.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3seto    subr. w3odatmd point to data structure.
  !      w3setg    subr. w3gdatmd point to data structure.
  !      w3setw    subr. w3wdatmd point to data structure.
  !      w3seta    subr. w3adatmd point to data structure.
  !      w3dmo3    subr. w3odatmd allocate work arrays.
  !      strace    subr. w3servmd subroutine tracing.
  !      tick21    subr. w3timemd increment time.
  !      dsec21    func. w3timemd time difference.
  !      mpi_send, mpi_recv, mpi_startall, mpi_waitall
  !                               mpi send and recieve routines
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !     see documentation of w3iotr.
  !
  !  7. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ private parameter statements (id strings)
  !/
  character(len=10), parameter, private :: vertrk = '2018-06-08'
  character(len=34), parameter, private ::                        &
       idstri = 'wavewatch iii track locations data', &
       idstro = 'wavewatch iii track output spectra'
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief perform output of spectral information along provided tracks.
  !>
  !> @details
  !> @verbatim
  !>     time and location data for the track is read from the file
  !>     track_i.filext, and output spectra additional information are
  !>     written to track_o.filext.
  !>
  !>     the spectrum dumped is the frequency-direction spectrum in
  !>     m**2/hz/rad.
  !>
  !>     the output spectra are energy density spectra in terms of the
  !>     true frequency and a direction in radians. the corresponding
  !>     band widths are part of the file header.
  !> @endverbatim
  !>
  !> @param[inout] ndsinp  unit number of input file track_i.filext.
  !> @param[inout] ndsout  unit number of output file track_o.filext.
  !> @param[inout] a       spectra (shape conversion through par list).
  !> @param[inout] imod    model grid number.
  !>
  !> @author h. l. tolman  @date 08-jun-2018
  !>
  subroutine w3iotr ( ndsinp, ndsout, a, imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         08-jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/    22-dec-1998 : final fortran 77                    ( version 1.18 )
    !/    27-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    24-jan-2001 : flat grid version                   ( version 2.06 )
    !/    20-aug-2003 : output through naptrk, seq. file.   ( version 3.04 )
    !/    24-nov-2004 : multiple grid version.              ( version 3.06 )
    !/    04-may-2005 : change to mpi_comm_wave.            ( version 3.07 )
    !/    27-jun-2005 : adding mapst2,                      ( version 3.07 )
    !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
    !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    06-dec-2010 : change from global (logical) to iclose (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    26-dec-2012 : initialize asptrk.                  ( version 4.11 )
    !/    12-dec-2014 : modify instanciation of nrqtr       ( version 5.04 )
    !/    08-jun-2018 : use w3parall/init_get_jsea_isproc   ( version 6.04 )
    !/
    !/    copyright 2009-2014 national weather service (nws),
    !/       national oceanic and atmospheric administration.  all rights
    !/       reserved.  wavewatch iii is a trademark of the nws.
    !/       no unauthorized use without permission.
    !/
    !  1. purpose :
    !
    !     perform output of spectral information along provided tracks.
    !
    !  2. method :
    !
    !     time and location data for the track is read from the file
    !     track_i.filext, and output spectra additional information are
    !     written to track_o.filext.
    !
    !     the spectrum dumped is the frequency-direction spectrum in
    !     m**2/hz/rad.
    !
    !     the output spectra are energy density spectra in terms of the
    !     true frequency and a direction in radians. the corresponding
    !     band widths are part of the file header.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndsinp  int.   i   unit number of input file track_i.filext
    !                          if negative, file is unformatted and v.v.
    !       ndsout  int.   i   unit number of output file track_o.filext
    !       a       r.a.   i   spectra (shape conversion through par list).
    !       imod    int.   i   model grid number.
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
    !      w3wave    subr. w3wavemd actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     - if input file not found, a warning is printed and output
    !       type is disabled.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !     !/mpi   mpi interface routines.
    !
    !     !/s     enable subroutine tracing.
    !     !/t     general test output.
    !     !/t1    test output on track point status.
    !     !/t2    test output of mask arrays.
    !     !/t3    test output for writing of file.
    !
    ! 10. remarks :
    !
    !     regarding section 3.e.2 "optimize: omit points that are not
    !       strictly required.". this optimization saves disk space but
    !       results in output files that are more difficult to use. for
    !       example, matlab built-in function "griddata" requires all four
    !       bounding points. this means that a post-processing code must
    !       have extra logic do deal with cases without all four bounding
    !       points (interpolation along a line, or nearest neighbor).
    !       a namelist variable has been add to make this feature optional.
    !       default, original behavior: trckcmpr = t (in /misc/ namelist).
    !       save all points: trckcmpr =  f (in /misc/ namelist).
    !       within the present routine, the logical is named "cmprtrck".
    !
    ! 11. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    !/
    use w3gdatmd, only: w3setg, cmprtrck
    use w3wdatmd, only: w3setw
    use w3adatmd, only: w3seta
    use w3odatmd, only: w3seto, w3dmo3
    !/
    use w3gdatmd, only: nk, nth, nspec, nsea, nseal, nx, ny,        &
         flagll, iclose, xgrd, ygrd, gsu,            &
         dpdx, dpdy, dqdx, dqdy, mapsta, mapst2,     &
         mapfs, th, dth, sig, dsip, xfr, filext
    use w3gsrumd, only: w3gfcl
    use w3gdatmd, only: maxx, maxy, gtype, ungtype
    use w3wdatmd, only: time, ust
    use w3adatmd, only: cg, dw, cx, cy, ua, ud, as
    use w3adatmd, only: mpi_comm_wave
    use w3odatmd, only: ndst, ndse, iaproc, naproc, naptrk, naperr, &
         ipass => ipass3, atolast => tolast,         &
         adtout => dtout, o3init, stop, mask1,       &
         mask2, trckid, fnmpre
    use w3odatmd, only: it0trk, nrqtr, irqtr
    !/
    use w3timemd
    use w3parall, only : init_get_jsea_isproc
    use w3servmd, only : strsplit
    !
    implicit none
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndsinp, ndsout, imod
    real, intent(in)        :: a(nth,nk,0:nseal)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, parameter      :: otype = 3
    integer                 :: ndsti, ndsto, isproc, ierr,          &
         ik, ith, ix, iy, timeb(2), timee(2), &
         ttime(2), ix1, ix2, iy1, iy2,        &
         ixx(4), iyy(4), i, j, isea, jsea,    &
         tolast(2)
    integer                 :: it, iroot, ifrom, ierr_mpi
    integer, allocatable    :: status(:,:)
    real                    :: xn, yn, xt, yt, rd, x, y, wx, wy,    &
         spec(nk,nth), factor, asptrk(nth,nk),&
         dtout, xx(4), yy(4)
    real, save              :: rdchck = 0.05, rtchck = 0.05
    logical                 :: formi, flag1, flag2, ingrid
    character               :: trckt*32, line*1024, tststr*3, idtst*34
    character(len=100)      :: list(5)
    !
    equivalence                (ixx(1),ix1) , (ixx(2),ix2) ,        &
         (iyy(1),iy1) , (iyy(3),iy2)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    call w3seto ( imod, ndse, ndst )
    call w3setg ( imod, ndse, ndst )
    call w3seta ( imod, ndse, ndst )
    call w3setw ( imod, ndse, ndst )
    !
    tolast = atolast(:,otype)
    dtout  = adtout(otype)
    !
    if ( .not. o3init ) call w3dmo3 ( imod, ndse, ndst )
    !
    formi  = ndsinp .gt. 0
    ndsti  = abs(ndsinp)
    ndsto  = abs(ndsout)
    if (gtype .eq. ungtype) then
      xn     = maxx
      yn     = maxy
    endif
    !
    isproc = iaproc
    ipass  = ipass + 1
    !
    if ( flagll ) then
      factor = 1.
    else
      factor = 1.e-3
    end if
    !
    asptrk = 0.
    !
    !
    if ( nrqtr .ne. 0 ) then
      call mpi_startall ( nrqtr, irqtr, ierr_mpi )
      allocate ( status(mpi_status_size,nrqtr) )
      call mpi_waitall ( nrqtr, irqtr , status, ierr_mpi )
      deallocate ( status )
    end if
    !
    ! 1.  first pass through routine ------------------------------------- *
    !
    if ( ipass .eq. 1 ) then
      !
      !   removed by f.a. 2010/12/24  /t          call w3gsup ( gsu, ndst )
      !
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      !
      ! 1.a open input file
      !
      if ( formi ) then
        open (ndsti,file=fnmpre(:j)//'track_i.'//filext(:i),     &
             status='old',err=800,form='formatted',iostat=ierr)
        read (ndsti,'(a)',err=801,end=802,iostat=ierr) idtst
      else
        open (ndsti,file=fnmpre(:j)//'track_i.'//filext(:i),     &
             status='old',err=800,form='unformatted', convert=file_endian,iostat=ierr)
        read (ndsti,err=801,end=802,iostat=ierr) idtst
      end if
      !
      if ( idtst .ne. idstri ) goto 803
      !
      ! 1.b open output file
      !
      if ( iaproc .eq. naptrk ) then
        open (ndsto,file=fnmpre(:j)//'track_o.'//filext(:i),     &
             form='unformatted', convert=file_endian,err=810,iostat=ierr)
        write (ndsto,err=811,iostat=ierr) idstro, flagll, nk,    &
             nth, xfr
        write (ndsto,err=811,iostat=ierr) 0.5*pi-th(1), -dth,    &
             (sig(ik)*tpiinv,ik=1,nk),                          &
             (dsip(ik)*tpiinv,ik=1,nk)
      end if
      !
      ! 1.c initialize maps
      !
      !
      mask2  = .false.
      trckid = ''
      !
    end if
    !
    ! 2.  preparations --------------------------------------------------- *
    ! 2.a shift mask arrays
    !
    !
    mask1  = mask2
    mask2  = .false.
    !
    ! 2.b set time frame
    !
    timeb  = time
    timee  = time
    call tick21 ( timee ,  dtout )
    !
    if ( dsec21(timee,tolast) .lt. 0. ) then
      timee  = tolast
    end if
    !
    !
    ! 3.  loop over input points ----------------------------------------- *
    !
    !
    ! 3.a read new track point (infinite loop)
    !
    if ( stop ) then
      tolast = time
      goto 399
    end if
    !
    !
    do
      !
      if ( formi ) then
        read (ndsti,'(a)',err=801,end=390,iostat=ierr) line
        list(:)=''
        call strsplit(line,list)
        read(list(1),'(i8)') ttime(1)
        read(list(2),'(i6)') ttime(2)
        read(list(3),*) xt
        read(list(4),*) yt
        if(size(list).ge.5) trckt=list(5)
      else
        read (ndsti, err=801,end=390,iostat=ierr) ttime, xt, yt, trckt
      end if
      !
      ! 3.b point before time interval
      !
      if ( dsec21(timeb,ttime) .lt. 0. ) then
        cycle
      end if
      !
      ! 3.c point after time interval
      !
      if ( dsec21(timee,ttime) .gt. 0. ) then
        backspace (ndsti)
        goto 399
      end if
      !
      ! 3.d check time in interval
      !
      flag1  = dsec21(ttime,timee) .gt. rtchck*dtout
      flag2  = dsec21(timeb,ttime) .gt. rtchck*dtout
      !
      ! 3.e check point coordinates
      !
      ! 3.e.1 initial identification of computational grid points to include.
      !
      !       find cell that encloses target point (note that the returned
      !       cell corner indices are adjusted for global wrapping and the
      !       coordinates are adjusted to avoid branch cut crossings)
      ingrid = w3gfcl( gsu, xt, yt, ixx, iyy, xx, yy )
      if ( .not. ingrid ) then
        cycle
      end if
      !
      !       change cell-corners from counter-clockwise to column-major order
      ix     = ixx(4);  iy     = iyy(4);
      ixx(4) = ixx(3);  iyy(4) = iyy(3);
      ixx(3) = ix;      iyy(3) = iy;
      !
      ! 3.e.2 optimize: omit points that are not strictly required.
      !       see "remarks"
      if(cmprtrck)then ! perform track compression
        !         project onto i-axis
        rd = dpdx(iyy(1),ixx(1))*(xt-xx(1)) &
             + dpdy(iyy(1),ixx(1))*(yt-yy(1))
        !
        !         collapse to left or right if within tolerance
        if ( rd .lt. rdchck ) then
          ixx(2) = ixx(1)
          ixx(4) = ixx(3)
        else if ( rd .gt. 1.-rdchck ) then
          ixx(1) = ixx(2)
          ixx(3) = ixx(4)
        end if
        !
        !         project onto j-axis
        rd = dqdx(iyy(1),ixx(1))*(xt-xx(1)) &
             + dqdy(iyy(1),ixx(1))*(yt-yy(1))
        !
        !         collapse to top or bottom if within tolerance
        if ( rd .lt. rdchck ) then
          iyy(3) = iyy(1)
          iyy(4) = iyy(2)
        else if ( rd .gt. 1.-rdchck ) then
          iyy(1) = iyy(3)
          iyy(2) = iyy(4)
        end if
      end if ! if(cmprtrck)then
      !
      ! 3.f mark the four corner points
      !
      do j=1, 4
        !
        ix     = ixx(j)
        iy     = iyy(j)
        if(gtype .eq. ungtype) then
          x = xgrd(1,ix)
          y = ygrd(1,ix)
        endif
        mask1(iy,ix) = mask1(iy,ix) .or. flag1
        mask2(iy,ix) = mask2(iy,ix) .or. flag2
        trckid(iy,ix) = trckt
        !
        !
      end do
      !
      !
      !
    end do
    !
    ! 3.g end of input file escape location
    !
390 continue
    stop   = .true.
    !
    ! 3.h read end escape location
    !
399 continue
    !
    ! 3.h mask test output
    !
    !
    ! 4.  write data for flagged locations ------------------------------- *
    !
    it     = it0trk
    iroot  = naptrk - 1
    allocate ( status(mpi_status_size,1) )
    !
    do iy=1, ny
      do ix=1, nx
        if ( mask1(iy,ix) ) then
          !
          if(gtype .eq. ungtype) then
            x = xgrd(1,ix)
            y = ygrd(1,ix)
          else
            x = xgrd(iy,ix)
            y = ygrd(iy,ix)
          endif
          it     = it + 1
          !
          ! 4.a status of point
          !
          if ( mapsta(iy,ix) .eq. 0 ) then
            if ( mapst2(iy,ix) .eq. 0 ) then
              tststr = 'lnd'
            else
              tststr = 'xcl'
            end if
          else if ( mapsta(iy,ix) .lt. 0 ) then
            if ( mapst2(iy,ix) .eq. 1 ) then
              tststr = 'ice'
            else if ( mapst2(iy,ix) .eq. 2 ) then
              tststr = 'dry'
            else
              tststr = 'dis'
            end if
          else
            tststr = 'sea'
          end if
          !
          !
          ! 4.b determine where point is stored
          !     ( land point assumed stored on iaproc = naptrk
          !       set to -99 in test output )
          !
          isea   = mapfs(iy,ix)
          if ( isea .eq. 0 ) then
            isproc = naptrk
          else
            call init_get_jsea_isproc(isea, jsea, isproc)
          end if
          ifrom  = isproc - 1
          ! 4.c spectrum is at local processor, but this is not the naptrk
          !     send the spectrum to naptrk
          if ( isproc.eq.iaproc .and. iaproc.ne.naptrk ) then
            call mpi_send ( a(1,1,jsea), nspec, mpi_real,  &
                 iroot, it, mpi_comm_wave, ierr_mpi )
          end if
          !
          ! 4.d this is naptrk, perform all output
          !
          if ( iaproc .eq. naptrk ) then
            !
            ! 4.e sea point, prepare data
            !
            if ( tststr .eq. 'sea' ) then
              !
              wx     = ua(isea) * cos(ud(isea))
              wy     = ua(isea) * sin(ud(isea))
              !
              ! ..... local spectra
              !
              if ( iaproc .eq. isproc ) then
                do ik=1, nk
                  do ith=1, nth
                    spec(ik,ith) =                          &
                         tpi*a(ith,ik,jsea)*sig(ik)/cg(ik,isea)
                  end do
                end do
                !
                ! ..... non-local spectra
                !
              else
                call mpi_recv (asptrk, nspec, mpi_real,&
                     ifrom, it, mpi_comm_wave,   &
                     status, ierr_mpi )
                !
                do ik=1, nk
                  do ith=1, nth
                    spec(ik,ith) =                          &
                         tpi*asptrk(ith,ik)*sig(ik)/cg(ik,isea)
                  end do
                end do
              end if
              !
              ! 4.e sea point, write general data + spectrum
              !
              write (ndsto,err=811,iostat=ierr)               &
                   time, x, y, tststr, trckid(iy,ix)
              write (ndsto,err=811,iostat=ierr)               &
                   dw(isea), cx(isea), cy(isea), wx, wy,        &
                   ust(isea), as(isea), spec
              !
              ! 4.f non-sea point, write
              !
            else
              write (ndsto,err=811,iostat=ierr)               &
                   time, x, y, tststr, trckid(iy,ix)
              !
              ! ..... sea and non-sea points processed
              !
            end if
            !
            ! ..... end of action at naptrk
            !
          end if
          !
          ! ..... close if for mask flag (top section 4)
          !
        end if
        !
        ! ..... end of loop over map
        !
      end do
    end do
    !
    deallocate ( status )
    !
    !
    goto 888
    !
    !     error escape locations
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) filext(:i), ierr
    goto 880
    !
801 continue
    if ( iaproc .eq. naperr ) write (ndse,1001) filext(:i), ierr
    goto 880
    !
802 continue
    if ( iaproc .eq. naperr ) write (ndse,1002) filext(:i)
    goto 880
    !
803 continue
    if ( iaproc .eq. naperr ) write (ndse,1003) filext(:i), idstri, idtst
    goto 880
    !
810 continue
    if ( iaproc .eq. naperr ) write (ndse,1010) filext(:i), ierr
    goto 880
    !
811 continue
    if ( iaproc .eq. naperr ) write (ndse,1011) filext(:i), ierr
    !
    !     disabeling output
    !
880 continue
    atolast(:,3) = time
    !
888 continue
    !
    return
    !
    ! formats
    !
1000 format (/' *** wavewatch iii warning in w3iotr : '/             &
         '     input file with track data not found ',          &
         '(file track_i.',a,' iostat =',i6,')'/                 &
         '     track output disabled '/)
1001 format (/' *** wavewatch iii warning in w3iotr : '/             &
         '     error in reading file track_i.',a,' iostat =',i6/&
         '     (aditional) track output disabled '/)
1002 format (/' *** wavewatch iii warning in w3iotr : '/             &
         '     premature end of file track_i.',a/               &
         '     track output disabled '/)
1003 format (/' *** wavewatch iii warning in w3iotr : '/             &
         '     unexpected contents of of file track_i.',a/      &
         '       expected : ',a/                                &
         '       found    : ',a/                                &
         '     track output disabled '/)
1010 format (/' *** wavewatch iii warning in w3iotr : '/             &
         '     error in opening output file ',                  &
         '(file track_o.',a,' iostat =',i6,')'/                 &
         '     track output disabled '/)
1011 format (/' *** wavewatch iii warning in w3iotr : '/             &
         '     error in writing to file track_o.',a,' iostat =',i6/ &
         '     (aditional) track output disabled '/)
    !
    !
    !
    !/
    !/ end of w3iotr ----------------------------------------------------- /
    !/
  end subroutine w3iotr
  !/
  !/ end of module w3iotrmd -------------------------------------------- /
  !/
end module w3iotrmd
