!> @file
!> @brief i/o and computational routines for the wave-field separation
!>  output.
!>
!> @author h. l. tolman  @date 25-jul-2018
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
!> @brief i/o and computational routines for the wave-field separation
!>  output.
!>
!> @author h. l. tolman  @date 25-jul-2018
!>
module w3iosfmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         25-jul-2018 |
  !/                  +-----------------------------------+
  !/
  !/    27-jun-2006 : origination.                        ( version 3.09 )
  !/    02-nov-2006 : origination w3cprt and w3iosf.      ( version 3.10 )
  !/    24-mar-2007 : add pars for entire spectrum.       ( version 3.11 )
  !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    30-oct-2009 : fix unitialized dtsiz in w3iosf.    ( version 3.14 )
  !/                  (t. j. campbell, nrl)
  !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
  !/                  (w. e. rogers & t. j. campbell, nrl)
  !/    06-mar-2012 : reparing test output under mpi.     ( version 4.07 )
  !/    08-jun-2018 : use w3adatmd, w3parall, init_get_isea and
  !/                            init_get_jsea_isproc      ( version 6.04 )
  !/    25-jul-2018 : changed dimxp size for partitioning ( version 6.05 )
  !/                  methods 4 and 5. (c bunney, ukmo)
  !/
  !/    copyright 2009-2012 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     i/o and computational routines for the wave-field separation
  !     output.
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      verprt    c*10  private  partition file version number.
  !      idstr     c*35  private  partition file id string.
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3cprt    subr. public   partition all requested local spectra.
  !      w3iosf    subr. public   processing and output of partitioned
  !                               wave data.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      w3part    subr. w3partmd spectral partition for single spectrum.
  !      strace    sur.  w3servmd subroutine tracing.
  !      extcde    subr.   id.    program abort.
  !      mpi_send, mpi_recv
  !                               mpi send and recieve routines
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
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
  !/ private parameter statements (id strings)
  !/
  character(len=10), parameter, private :: verprt = '2018-07-25'
  character(len=35), parameter, private ::                        &
       idstr = 'wavewatch iii partitioned data file'
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief partitioning of spectra into fields for all grid points that
  !>  are locally stored.
  !>
  !> @param[in] imod  grid number.
  !>
  !> @author h. l. tolman  @date 25-jul-2018
  !>
  subroutine w3cprt ( imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         25-jul-2018 !
    !/                  +-----------------------------------+
    !/
    !/    30-oct-2006 : origination.                        ( version 3.10 )
    !/    24-mar-2007 : add pars for entire spectrum.       ( version 3.11 )
    !/    25-jul-2018 : changed dimxp size for partitioning ( version 6.05 )
    !/                  methods 4 and 5. (c bunney, ukmo)
    !/
    !  1. purpose :
    !
    !     partitioning of spectra into fields for all grid points that
    !     are locally stored.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   grid number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3part    subr. w3partmd spectral partition for single spectrum.
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
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
    !  7. remarks :
    !
    !     - although a sparse (ix,iy) grid is looked for, th major loop
    !       is still over nseal to simplify storage.
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
    !/
    use constants
    !
    use w3partmd, only: w3part
    !
    use w3gdatmd, only: nsea, nseal, mapsf, mapsta, nk, nth, sig
    use w3adatmd, only: wn, cg, u10, u10d, dw
    use w3odatmd, only: iaproc, naproc, outpts, o6init,       &
         icprt, dtprt, dimp, ptmeth
    use w3wdatmd, only: va, asf
    use w3adatmd, only: nsealm
    use w3parall, only: init_get_isea, init_get_jsea_isproc
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: dimxp, jsea, isea, ix, iy,     &
         ik, ith, np, tmpsiz, oldsiz, finsiz
    integer, save           :: tsfac = 7
    real                    :: uabs, udir, depth, fact, e2(nk,nth)
    real, allocatable       :: xp(:,:), tmp(:,:), tmp2(:,:)
    !/
    !
    ! -------------------------------------------------------------------- /
    ! 0.  initializations
    !
    if(ptmeth .eq. 4 .or. ptmeth .eq. 5) then
      ! partitioning methods 4 and 5 only ever create 2 partitions
      ! c. bunney, 25-jul-18
      dimxp = 2
    else
      dimxp  = ((nk+1)/2) * ((nth-1)/2)
    endif
    allocate ( xp(dimp,0:dimxp) )
    !
    if ( o6init ) then
      deallocate ( outpts(imod)%out6%dtprt )
    else
      allocate ( outpts(imod)%out6%icprt(nsealm+1,2) )
      icprt => outpts(imod)%out6%icprt
      o6init = .true.
    end if
    icprt  = 0
    icprt(1,2) = 1
    !
    tmpsiz = tsfac * nseal
    allocate ( tmp(dimp,tmpsiz) )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 1.  loop over sea points
    !
    do jsea=1, nseal
      !
      ! -------------------------------------------------------------------- /
      ! 2.  check need for processing
      !
      call init_get_isea(isea, jsea)
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      icprt(jsea+1,2) = icprt(jsea,2)
      !
      if ( mapsta(iy,ix) .lt. 0 ) cycle
      !
      ! -------------------------------------------------------------------- /
      ! 3.  prepare for partitioning
      !
      uabs   = u10(isea)*asf(isea)
      udir   = u10d(isea)*rade
      depth  = dw(isea)
      !
      do ik=1, nk
        fact   = tpi * sig(ik) / cg(ik,isea)
        do ith=1, nth
          e2(ik,ith) = va(ith+(ik-1)*nth,jsea) * fact
        end do
      end do
      !
      ! -------------------------------------------------------------------- /
      ! 4.  perform partitioning
      !
      !ar: nan checks should results in immediate stop after trace ...
      if (depth.ne.depth) then
        write(6,*) 'iosf:',isea,ix,iy,dw(isea),depth
        write(*,*) 'found nan in depth'
        stop 'critical error in depth array'
      end if
      call w3part ( e2, uabs, udir, depth, wn(1:nk,isea),           &
           np, xp, dimxp )
      !
      ! -------------------------------------------------------------------- /
      ! 5.  store results (temp)
      !
      if ( np .ge. 0 ) then
        icprt( jsea ,1) = np + 1
        icprt(jsea+1,2) = icprt(jsea,2) + np + 1
        !
        if ( icprt(jsea,2)+np .gt. tmpsiz ) then
          allocate ( tmp2(dimp,tmpsiz) )
          tmp2   = tmp
          deallocate ( tmp )
          oldsiz = tmpsiz
          tmpsiz = tmpsiz + max ( tsfac*nseal , dimxp )
          allocate ( tmp(dimp,tmpsiz) )
          tmp(:,1:oldsiz) = tmp2(:,1:oldsiz)
          tmp(:,oldsiz+1:) = 0.
          deallocate ( tmp2 )
        end if
        !
        tmp(:,icprt(jsea,2):icprt(jsea,2)+np) = xp(:,0:np)
        !
      end if
      !
      ! -------------------------------------------------------------------- /
      ! 6.  end of loop and clean up
      !
    end do
    !
    finsiz = icprt(nseal+1,2) - 1
    !
    !
    allocate ( outpts(imod)%out6%dtprt(dimp,max(1,finsiz)) )
    dtprt => outpts(imod)%out6%dtprt
    if ( finsiz .gt. 0 ) then
      dtprt = tmp(:,1:finsiz)
    else
      dtprt = 0.
    end if
    !
    deallocate ( xp, tmp )
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3cprt ----------------------------------------------------- /
    !/
  end subroutine w3cprt
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief write partitioned spectral data to file.
  !>
  !> @details  unlike other wavewatch iii io routines, this one writes only.
  !>  first ad-hoc version.
  !>
  !>  writing to formatted or unformatted file with id headers.
  !>
  !> @param[in] ndspt  unit number.
  !> @param[in] imod   grid number.
  !>
  !> @author h. l. tolman  @date 30-oct-2009
  !>
  subroutine w3iosf ( ndspt, imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    02-nov-2006 : origination.                        ( version 1.10 )
    !/    24-mar-2007 : add pars for entire spectrum.       ( version 3.11 )
    !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    30-oct-2009 : fix unitialized dtsiz error.        ( version 3.14 )
    !/                  (t. j. campbell, nrl)
    !/    30-oct-2009 : implement run-time grid selection.  ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     write partitioned spectrakl data to file. unlike other
    !     wavewatch iii io routines, this one writes only.
    !     first ad-hoc version.
    !
    !  2. method :
    !
    !     writing to formatted or unformatted file with id headers.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ndspt   int.   i   unit number.
    !       imod    int.   i   grid number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      extcde    subr.   id.    program abort.
    !      mpi_send, mpi_recv
    !                               mpi send and recieve routines
    !     ----------------------------------------------------------------
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
    !/
    use constants
    use w3servmd, only: extcde
    !
    use w3gdatmd, only: filext, nsea, xgrd, ygrd, mapsf, flagll
    use w3gdatmd, only: nseal
    use w3wdatmd, only: time, asf
    use w3odatmd, only: ndse, iaproc, naproc, napprt, naperr, &
         ipass => ipass6, flform, fnmpre, outpts,    &
         ix0, ixn, ixs, iy0, iyn, iys, dimp
    use w3adatmd, only: dw, u10, u10d, cx, cy
    use w3adatmd, only: nsealm
    use w3parall, only: init_get_jsea_isproc
    use w3adatmd, only: mpi_comm_wave
    use w3odatmd, only: icprt, dtprt, it0prt
    !
    implicit none
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndspt, imod
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i, j, ierr, isea, jsea, japroc,      &
         ix, iy, ip, ioff, dtsiz=0
    integer                 :: icsiz, ierr_mpi, it,            &
         status(mpi_status_size,1), jslm
    integer, pointer        :: icp(:,:)
    real                    :: x, y, depth, uabs, udir, cabs, cdir
    real, pointer           :: dtp(:,:)
    !
    type procs
      integer, pointer      :: icprt(:,:)
      real, pointer         :: dtprt(:,:)
    end type procs
    !
    type(procs), target, allocatable :: proc(:)
    !
    ! -------------------------------------------------------------------- /
    ! 0.  initializations
    !
    !
    ipass  = ipass + 1
    icsiz  = 2 * ( nsealm + 1 )
    !
    !
    ! -------------------------------------------------------------------- /
    ! 1.  set up file ( ipass = 1 and proper processor )
    !
    if ( ipass.eq.1 .and. iaproc.eq.napprt ) then
      !
      ! 1.a open file
      !
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      !
      !
      if ( flform ) then
        open (ndspt,file=fnmpre(:j)//'partition.'//filext(:i),   &
             err=800,iostat=ierr)
      else
        open (ndspt,file=fnmpre(:j)//'partition.'//filext(:i),   &
             form='unformatted',convert=file_endian,err=800,iostat=ierr)
      end if
      !
      rewind (ndspt)
      !
      ! 1.b header info
      !
      if ( flform ) then
        write (ndspt,910) idstr, verprt
        if ( flagll ) then
          write (ndspt,911) ' yyyymmdd hhmmss     '//         &
               'lat     lon   name       nprt'// &
               ' depth ubas  udir cabs  cdir'
        else
          write (ndspt,911) ' yyyymmdd hhmmss     '//         &
               'x       y     name       nprt'// &
               ' depth ubas  udir cabs  cdir'
        end if
        write (ndspt,911) '        hs     tp     lp  '//    &
             '     theta     sp      wf'
      else
        write (  ndspt  ) idstr, verprt
        if ( flagll ) then
          write (  ndspt  ) ' yyyymmdd hhmmss     '//         &
               'lat     lon   name       nprt'// &
               ' depth ubas  udir cabs  cdir'
        else
          write (  ndspt  ) ' yyyymmdd hhmmss     '//         &
               'x       y     name       nprt'// &
               ' depth ubas  udir cabs  cdir'
        end if
        write (  ndspt  ) '        hs     tp     lp  '//    &
             '     theta     sp      wf'
      end if
      !
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  send data if output is non-local ( mpi only )
    !     leave routine after send
    !
    if ( iaproc.ne.napprt .and. iaproc.le.naproc ) then
      !
      !
      it     = it0prt + iaproc - 1
      call mpi_send ( icprt, icsiz, mpi_real, napprt-1, it,  &
           mpi_comm_wave, ierr_mpi )
      dtsiz  = icprt(nseal+1,2) - 1
      !
      !
      it     = it0prt + naproc + iaproc - 1
      if ( dtsiz .gt. 0 ) call mpi_send                      &
           ( dtprt, 6*dtsiz, mpi_real, napprt-1,    &
           it, mpi_comm_wave, ierr_mpi )
      !
    end if
    !
    if ( iaproc .ne. napprt ) return
    !
    ! -------------------------------------------------------------------- /
    ! 3.  point to and/or gather data
    ! 3.a set up storage
    !
    allocate ( proc(naproc) )
    !
    ! 3.b point to local data
    !
    if ( iaproc .le. naproc ) then
      proc(iaproc)%icprt => outpts(imod)%out6%icprt
      proc(iaproc)%dtprt => outpts(imod)%out6%dtprt
    end if
    !
    ! 3.c allocate and get counters and arrrays
    !
    do japroc=1, naproc
      if ( iaproc .eq. japroc ) cycle
      !
      !
      allocate ( proc(japroc)%icprt(nsealm+1,2) )
      icp   => proc(japroc)%icprt
      it     = it0prt + japroc - 1
      call mpi_recv ( icp, icsiz, mpi_real, japroc-1, it,  &
           mpi_comm_wave, status, ierr_mpi )
      jslm   = 1 + (nsea-japroc)/naproc
      dtsiz  = icp(jslm+1,2) - 1
      !
      !
      allocate ( proc(japroc)%dtprt(dimp,max(1,dtsiz)) )
      dtp   => proc(japroc)%dtprt
      it     = it0prt + naproc + japroc - 1
      if ( dtsiz .gt. 0 ) call mpi_recv                    &
           ( dtp, dimp*dtsiz, mpi_real, japroc-1, &
           it, mpi_comm_wave, status, ierr_mpi )
      !
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 4.  write all data for which partitions are found
    ! 4.a general loop over all sea points
    !
    do isea=1, nsea
      !
      ! 4.b check for partitioned data at sea point
      !
      call init_get_jsea_isproc(isea, jsea, japroc)
      !
      icp   => proc(japroc)%icprt
      dtp   => proc(japroc)%dtprt
      !
      if ( icp(jsea,1) .eq. 0 ) cycle
      !
      ! 4.c process point id line
      !
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      if ( ix.lt.ix0 .or. ix.gt.ixn .or. mod(ix-ix0,ixs).ne.0 ) cycle
      if ( iy.lt.iy0 .or. iy.gt.iyn .or. mod(iy-iy0,iys).ne.0 ) cycle
      x      = xgrd(iy,ix)
      y      = ygrd(iy,ix)
      depth   = dw(isea)
      uabs   = u10(isea)*asf(isea)
      udir   = mod ( 270. - u10d(isea)*rade , 360. )
      cabs   = sqrt ( cx(isea)**2 + cy(isea)**2 )
      if ( cabs .lt. 1.e-3 ) then
        cdir   = 0.
      else
        cdir   = atan2 ( cy(isea), cx(isea) ) * rade
        cdir   = mod ( 270. - cdir , 360. )
      end if
      !
      if ( flform ) then
        if ( flagll ) then
          write (ndspt,940) time, y, x,                        &
               'grid_point', icp(jsea,1) - 1,      &
               depth, uabs, udir, cabs, cdir
        else
          write (ndspt,941) time, x*1.e-3, y*1.e-3,            &
               'grid_point', icp(jsea,1) - 1,      &
               depth, uabs, udir, cabs, cdir
        end if
      else
        if ( flagll ) then
          write (  ndspt  ) time, y, x,                        &
               'grid_point', icp(jsea,1) - 1,      &
               depth, uabs, udir, cabs, cdir
        else
          write (  ndspt  ) time, x*1.e-3, y*1.e-3,            &
               'grid_point', icp(jsea,1) - 1,      &
               depth, uabs, udir, cabs, cdir
        end if
      end if
      !
      ! 4.d process partitions for this point
      !
      ioff   = icp(jsea,2)
      !
      if ( flform ) then
        do ip=0, icp(jsea,1) - 1
          write (ndspt,942) ip, dtp(:,ip+ioff)
        end do
      else
        do ip=0, icp(jsea,1) - 1
          write (  ndspt  ) ip, dtp(:,ip+ioff)
        end do
      end if
      !
    end do
    !
    ! -------------------------------------------------------------------- /
    ! 5.  clean up data structure
    !
    do japroc=1, naproc
      if ( iaproc .eq. japroc ) cycle
      deallocate ( proc(japroc)%icprt, proc(japroc)%dtprt )
    end do
    !
    deallocate ( proc )
    !
    return
    !
    ! escape locations read errors --------------------------------------- *
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,1000) ierr
    call extcde ( 1 )
    !
    ! formats
    !
910 format (a,1x,a)
911 format (a)
    !
940 format (1x,i8.8,1x,i6.6,2f8.3,2x,'''',a10,'''',            &
         1x,i2,f7.1,f5.1,f6.1,f5.2,f6.1)
941 format (1x,i8.8,1x,i6.6,2(f8.1,'e3'),2x,'''',a10,'''',     &
         1x,i2,f7.1,f5.1,f6.1,f5.2,f6.1)
942 format (i3,3f8.2,2f9.2,f7.2)
    !
1000 format (/' *** wavewatch iii error in w3iosf : '/               &
         '     error in opening file'/                          &
         '     iostat =',i5/)
    !
    !/
    !/ end of w3iosf ----------------------------------------------------- /
    !/
  end subroutine w3iosf
  !/
  !/ end of module w3iosfmd -------------------------------------------- /
  !/
end module w3iosfmd
