!> @file
!> @brief contains module w3initmd.
!>
!> @author h. l. tolman  @date 22-mar-2021
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
!>
!> @brief contains module w3initmd.
!>
!> @author h. l. tolman  @date 22-mar-2021
!>
!/ ------------------------------------------------------------------- /
module w3initmd
  ! module default
  implicit none
  public
  !/
  real, parameter                :: critos = 15.
  character(len=10), parameter   :: wwver  = '7.14  '
  character(len=512), parameter  :: switches  = &
       __ww3_switches__
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialize wavewatch iii.
  !>
  !> @details initialize data structure and wave fields from data files.
  !>  initialize grid from local and instantaneous data.
  !>
  !> @param[in] imod        model number.
  !> @param[in] ismulti
  !> @param[in] fext        extension of data files.
  !> @param[in] mds         array with dataset numbers saved as nds in w3odatmd.
  !> @param[in] mtrace      array with subroutine tracing information.
  !> @param[in] odat        output data, five parameters per output type.
  !> @param[inout] flgrd    flags for gridded output.
  !> @param[inout] flgr2    flags for coupling output.
  !> @param[inout] flgd
  !> @param[inout] flg2
  !> @param[in] npt           number of output points.
  !> @param[inout] xpt        coordinates of output points.
  !> @param[inout] ypt        coordinates of output points.
  !> @param[in] pnames        output point names.
  !> @param[in] iprt          partitioning grid info.
  !> @param[inout] prtfrm     partitioning format flag.
  !> @param[in] mpi_comm      mpi communicator to be used for model.
  !> @param[in] flagstidein
  !>
  !> @author h. l. tolman  @date 03-sep-2012
  !>
  subroutine w3init ( imod, ismulti, fext, mds, mtrace, odat, flgrd,  flgr2, flgd, &
       flg2, npt, xpt, ypt, pnames, iprt, prtfrm, mpi_comm, flagstidein)
    use w3servmd, only : print_memcheck
    use constants
    !/
    use w3gdatmd, only: w3setg, rstype
    use w3wdatmd, only: w3setw, w3dimw
    use w3adatmd, only: w3seta, w3dima
    use w3idatmd, only: w3seti, w3dimi
    use w3odatmd, only: w3seto, w3dmo5
    use w3iogomd, only: w3flgrdupdt
    use w3iogrmd, only: w3iogr
    use w3iorsmd, only: w3iors
    use w3iopomd, only: w3iopp
    use w3servmd, only: itrace, extcde, wwdate, wwtime
    use w3timemd, only: dsec21, tick21, stme21
    use w3arrymd, only: prtblk
    !/
    use w3gdatmd, only: nx, ny, nsea, nseal, mapsta, mapst2, mapfs, &
         mapsf, flagll,   &
         iclose, zb, trnx, trny, dmin, dtcfl, dtmax, &
         flck, nk, nth, nspec, sig, gname
    use w3wdatmd, only: time, tlev, tice, trho, wlv, ust, ustdir, va
    use w3odatmd, only: ndso, ndse, ndst, screen, nds, ntproc,      &
         naproc, iaproc, naplog, napout, naperr,     &
         napfld, nappnt, naptrk, naprst, napbpt,     &
         napprt, tofrst, dtout, tonext, tolast,      &
         flout, flogrd, flbpo, nopts, ptnme,         &
         ptloc, iptint, ptifac, undef, idout, flbpi, &
         outpts, fnmpre, ix0, ixn, ixs, iy0, iyn,    &
         iys, flform, iostyp, unipts, upproc, notype,&
         flogr2, nogrp, ngrpp, flogd, flog2
    use w3adatmd, only: nsealm, iappro, flcold, fliwnd, dw, cg, wn, &
         ua, ud, u10, u10d, as
    use w3adatmd, only: mpi_comm_wave, mpi_comm_wcmp
    use w3idatmd, only: fllev, flcur, flwind, flice, fltaua, flrhoa,&
         flmdn, flmth, flmvs, flic1, flic2, flic3,   &
         flic4, flic5
    use w3dispmd, only: wavnu1, wavnu3
    use w3parall, only: set_up_nseal_nsealm
    use w3gdatmd, only: gtype, ungtype
    use w3triamd, only: nvectri, area_si, coordmax, spatial_grid
    use w3gdatmd, only: fsn,fspsi,fsfct,fsnimp, fstotalimp, fstotalexp, xgrd, ygrd
    use w3gdatmd, only: fsrefraction, fsfreqshift
    use w3parall, only: init_get_jsea_isproc, init_get_isea
    !/
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)           :: imod, mds(13), mtrace(2),      &
         odat(40),npt, iprt(6),&
         mpi_comm
    logical, intent(in)           :: ismulti
    real, intent(inout)           :: xpt(npt), ypt(npt)
    logical, intent(inout)        :: flgrd(nogrp,ngrpp), flgd(nogrp),&
         flgr2(nogrp,ngrpp), flg2(nogrp),&
         prtfrm
    character, intent(in)         :: fext*(*)
    character(len=40), intent(in) :: pnames(npt)
    logical, intent(in), optional :: flagstidein(4)
    integer                       :: nsealout, nsealmout
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: irank, i, istat
    integer                 :: ie, ifl, ift, ierr, nttot, ntloc,    &
         nttarg, ik, ip, ith, ix, iy, &
         j, j0, tout(2), tlst(2), isea, is,   &
         k, i1, i2, jsea, nttmax
    integer                 :: ierr_mpi, bgroup, lgroup
    integer, allocatable    :: tmprnk(:)
    integer, allocatable    :: nt(:), maptst(:,:)
    real                    :: dttst, depth, fracos
    real                    :: factor
    real                    :: wlveff
    logical                 :: opened
    character(len=8)        :: sttime
    character(len=10)       :: stdate
    integer                 :: isproc
    character(len=23)       :: dtme21
    character(len=30)       :: lfile, tfile
    integer                 :: memunit
    !/
    !/ ------------------------------------------------------------------- /
    !
    ! 1.  set-up of data structures and i/o  ----------------------------- /
    ! 1.a point to proper data structures.
    call w3seto ( imod, mds(2), mds(3) )
    memunit = 10000+iaproc
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 1a')
    call w3setg ( imod, mds(2), mds(3) )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 1b')
    call w3setw ( imod, mds(2), mds(3) )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 1c')
    call w3seta ( imod, mds(2), mds(3) )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 1d')
    call w3seti ( imod, mds(2), mds(3) )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 1e')
    !
    !
    ! 1.b number of processors and processor number.
    !     overwrite some initializations from w3odatmd.
    !
    !     *******************************************************
    !     *** note : output processor assignment needs to be  ***
    !     ***        consistent with assignment in wminit.    ***
    !     *******************************************************
    !
    !
    mpi_comm_wave = mpi_comm
    call mpi_comm_size ( mpi_comm_wave, ntproc, ierr_mpi )
    naproc = ntproc
    call mpi_comm_rank ( mpi_comm_wave, iaproc, ierr_mpi )
    iaproc = iaproc + 1
    memunit = 10000+iaproc
    !
    if ( iostyp .le. 1 ) then
      !
      napfld = max(1,naproc-1)
      nappnt = max(1,naproc-2)
      naptrk = max(1,naproc-5)
      naprst = naproc
      napbpt = max(1,naproc-3)
      napprt = max(1,naproc-4)
      !
    else
      !
      nappnt = naproc
      if ( unipts .and. upproc ) naproc = max(1,ntproc - 1)
      napfld = naproc
      naprst = naproc
      napbpt = naproc
      naptrk = naproc
      napprt = naproc
      !
      if ( iostyp .eq. 2 ) then
        naproc = max(1,naproc-1)
      else if ( iostyp .eq. 3 ) then
        !
        ! for field or coupling output
        !
        if ( odat( 3).gt.0 .or.  odat(33).gt.0 ) then
          napfld =       naproc
          naproc = max(1,naproc-1)
        end if
        if ( odat(13).gt.0 ) then
          naptrk =       naproc
          naproc = max(1,naproc-1)
        end if
        if ( odat(28).gt.0 ) then
          napprt =       naproc
          naproc = max(1,naproc-1)
        end if
        if ( odat( 8).gt.0 ) nappnt = naproc
        if ( odat(18).gt.0 ) naprst = naproc
        if ( odat(23).gt.0 ) napbpt = naproc
        if ( ( odat( 8).gt.0 .or. odat(18).gt.0 .or.           &
             odat(23).gt.0 ) ) naproc = max(1,naproc-1)
      end if
    end if
    !
    fracos = 100. * real(ntproc-naproc) / real(ntproc)
    if ( fracos.gt.critos .and. iaproc.eq.naperr ) write (ndse,8002) fracos
    !
    if ( naproc .eq. ntproc ) then
      mpi_comm_wcmp = mpi_comm_wave
    else
      call mpi_comm_group ( mpi_comm_wave, bgroup, ierr_mpi )
      allocate ( tmprnk(naproc) )
      do j=1, naproc
        tmprnk(j) = j - 1
      end do
      call mpi_group_incl ( bgroup, naproc, tmprnk, lgroup, ierr_mpi )
      call mpi_comm_create ( mpi_comm_wave, lgroup, mpi_comm_wcmp, ierr_mpi )
      call mpi_group_free ( lgroup, ierr_mpi )
      call mpi_group_free ( bgroup, ierr_mpi )
      deallocate ( tmprnk )
    end if
    !
    lpdlib = .false.
    if (fstotalimp .and. .not. lpdlib) then
      write(ndse,*) 'imptotal is selected'
      write(ndse,*) 'but pdlib is not'
      call flush(ndse)
      stop
    else if (fstotalexp .and. .not. lpdlib) then
      write(ndse,*) 'exptotal is selected'
      write(ndse,*) 'but pdlib is not'
      call flush(ndse)
      stop
    end if
    !
    ! 1.c open files without unpacking mds ,,,
    !
    ie     = len_trim(fext)
    lfile  = 'log.' // fext(:ie)
    ifl    = len_trim(lfile)
    ift    = len_trim(tfile)
    j      = len_trim(fnmpre)
    !
    if ( outpts(imod)%iaproc .eq. outpts(imod)%naplog )             &
         open (mds(1),file=fnmpre(:j)//lfile(:ifl),err=888,iostat=ierr)
    !
    if ( mds(3).ne.mds(1) .and. mds(3).ne.mds(4) .and. tstout ) then
      inquire (mds(3),opened=opened)
      if ( .not. opened ) open (mds(3),file=fnmpre(:j)//tfile(:ift), err=889, &
           iostat=ierr)
    end if
    !
    ! 1.d dataset unit numbers
    !
    nds    = mds
    ndso   = nds(1)
    ndse   = nds(2)
    ndst   = nds(3)
    screen = nds(4)
    !
    ! 1.e subroutine tracing
    !
    call itrace ( mtrace(1), mtrace(2) )
    !
    ! 1.f initial and test outputs
    !
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2')
    !
    if ( iaproc .eq. naplog ) then
      call wwdate ( stdate )
      call wwtime ( sttime )
      write (ndso,900) wwver, stdate, sttime
    end if
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2a')
    !
    !
    ! 2.  model definition ---------------------------------------------- /
    ! 2.a read model definition file
    !
    call w3iogr ( 'read', nds(5), imod, fext )
    if (gtype .eq. ungtype) then
      call spatial_grid
      call nvectri
      call coordmax
        call area_si(1)
    endif
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2b')
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2c')
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2cc')
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2d')
    ! update of output parameter flags based on mod_def parameters (for 3d arrays)
    call w3flgrdupdt ( ndso, ndse, flgrd, flgr2, flgd, flg2 )
    if ( flagll ) then
      factor = 1.
    else
      factor = 1.e-3
    end if
    if ( iaproc .eq. naplog ) write (ndso,920)
    !
    ! 2.b save mapsta
    !
    allocate ( maptst(ny,nx) )
    maptst  = mapsta
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2e')
    !
    !
    ! 2.c mpp preparation
    ! 2.c.1 set simple counters and variables
    !
    call set_up_nseal_nsealm(nsealout, nsealmout)
    nseal  = nsealout
    nsealm = nsealmout
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2f')
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2g')
    !
    !
    ! 2.c.2 allocate arrays
    !
    if ( iaproc .le. naproc ) then
      call w3dimw ( imod, ndse, ndst )
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2h')
    else
      call w3dimw ( imod, ndse, ndst, .false. )
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2i')
    end if
    call w3dima ( imod, ndse, ndst )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 2j')
    call w3dimi ( imod, ndse, ndst , flagstidein )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 3')
    !
    ! 2.c.3 calculated expected number of prop. calls per processor
    !
    nttot  = 0
    do ik=1, nk
      ntloc  = 1 + int(dtmax/(dtcfl*sig(ik)/sig(1))-0.001)
      nttot  = nttot + ntloc*nth
    end do
    nttarg = 1 + (nttot-1)/naproc
    nttarg = nttarg + int(dtmax/(dtcfl*sig(nk)/sig(1))-0.001)
    nttmax = nttarg + 5
    !
    ! 2.c.4 initialize iappro
    !
    iappro = 1
    allocate ( nt(nspec) )
    nt     = nttot
    !
    ! 2.c.8 test output
    !
    !
    ! 2.c.9 test if any spectral points are left out
    !
    deallocate ( nt )
    !
    ! 3.  model initialization ------------------------------------------- /
    ! 3.a read restart file
    !
    va(:,:) = 0.
    call w3iors ( 'read', nds(6), sig(nk), imod)
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 3a')
    flcold = rstype.le.1  .or. rstype.eq.4
    if ( iaproc .eq. naplog ) then
      if (rstype.eq.0) then
        write (ndso,930) 'cold start (idealized).'
      else if ( rstype .eq. 1 ) then
        write (ndso,930) 'cold start (wind).'
      else if ( rstype .eq. 4 ) then
        write (ndso,930) 'cold start (calm).'
      else
        write (ndso,930) 'full restart.'
      end if
    end if
    !
    ! 3.b compare mapsta from grid and restart
    !
    do ix=1, nx
      do iy=1, ny
        if ( abs(mapsta(iy,ix)).eq.2 .or.                           &
             abs(maptst(iy,ix)).eq.2 ) then
          mapsta(iy,ix) = sign ( maptst(iy,ix) , mapsta(iy,ix) )
        end if
      end do
    end do
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 3b')
    !
    !
    ! 3.b2 set mapsta associated to pdlib
    !
    !
    ! 3.c initialization from wind fields
    !
    fliwnd = rstype.eq.1
    !
    ! 3.d initialization with calm conditions
    !
    if ( rstype .eq. 4 ) then
      va(:,:) = 0.
    end if
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 4')
    !
    ! 3.e prepare propagation scheme
    !
    if ( .not. flcur ) flck = .false.
    !
    ! 4.  set-up output times -------------------------------------------- *
    ! 4.a unpack odat
    !
    do j=1, notype
      j0 = (j-1)*5
      tonext(1,j) =        odat(j0+1)
      tonext(2,j) =        odat(j0+2)
      dtout (  j) = real ( odat(j0+3) )
      tolast(1,j) =        odat(j0+4)
      tolast(2,j) =        odat(j0+5)
    end do
    !
    ! j=8, second stream of restart files
    j=8
    j0 = (j-1)*5
    if(odat(j0+1) .ne. 0) then
      tonext(1,j) =        odat(j0+1)
      tonext(2,j) =        odat(j0+2)
      dtout (  j) = real ( odat(j0+3) )
      tolast(1,j) =        odat(j0+4)
      tolast(2,j) =        odat(j0+5)
      flout(8) = .true.
    else
      flout(8) = .false.
    end if
    !
    ! 4.b check if output available
    !
    flout(1) = .false.
    flogrd   = flgrd
    flogd    = flgd
    do j=1, nogrp
      do k=1, ngrpp
        flout(1) = flout(1) .or. flogrd(j,k)
      end do
    end do
    !
    flout(7) = .false.
    flogr2   = flgr2
    flog2    = flg2
    do j=1, nogrp
      do k=1, ngrpp
        flout(7) = flout(7) .or. flogr2(j,k)
      end do
    end do
    !
    flout(2) = npt .gt. 0
    !
    flout(3) = .true.
    !
    flout(4) = .true.
    !
    flout(5) = flbpo
    if ( flbpo ) then
      call w3dmo5 ( imod, ndse, ndst, 4 )
    else
      dtout(5) = 0.
    end if
    !
    ix0    = max (  1, iprt(1) )
    ixn    = min ( nx, iprt(2) )
    ixs    = max (  1, iprt(3) )
    iy0    = max (  1, iprt(4) )
    iyn    = min ( ny, iprt(5) )
    iys    = max (  1, iprt(6) )
    flform = prtfrm
    flout(6) = ix0.le.ixn .and. iy0.le.iyn
    !
    ! 4.c get first time per output and overall.
    !
    tofrst(1) = -1
    tofrst(2) =  0
    !
    !      write(*,*) 'we set notype=0 just for debugging'
    !      notype=0 ! only for debugging purpose
    do j=1, notype
      !
      ! ... check time step
      !
      dtout(j) = max ( 0. , dtout(j) )
      flout(j) = flout(j) .and. ( dtout(j) .gt. 0.5 )
      !
      ! ... get first time
      !
      if ( flout(j) ) then
        tout = tonext(:,j)
        tlst = tolast(:,j)
        !
        do
          dttst   = dsec21 ( time , tout )
          if ( ( j.ne.4 .and. dttst.lt.0. ) .or.                  &
               ( j.eq.4 .and. dttst.le.0. ) ) then
            call tick21 ( tout, dtout(j) )
          else
            exit
          end if
        end do
        !
        ! ... reset first time
        !
        tonext(:,j) = tout
        !
        ! ... check last time
        !
        dttst  = dsec21 ( tout , tlst )
        if ( dttst.lt.0.) flout(j) = .false.
        !
        ! ... check overall first time
        !
        if ( flout(j) ) then
          if ( tofrst(1).eq.-1 ) then
            tofrst = tout
          else
            dttst  = dsec21 ( tout , tofrst )
            if ( dttst.gt.0.) then
              tofrst = tout
            end if
          end if
        end if
        !
      end if
      !
    end do
    !
    ! j=8, second stream of restart files
    !
    j=8
    !
    ! ... check time step
    !
    dtout(j) = max ( 0. , dtout(j) )
    flout(j) = flout(j) .and. ( dtout(j) .gt. 0.5 )
    !
    ! ... get first time
    !
    if ( flout(j) ) then
      tout = tonext(:,j)
      tlst = tolast(:,j)
      !
      do
        dttst   = dsec21 ( time , tout )
        if ( ( j.ne.4 .and. dttst.lt.0. ) .or.                  &
             ( j.eq.4 .and. dttst.le.0. ) ) then
          call tick21 ( tout, dtout(j) )
        else
          exit
        end if
      end do
      !
      ! ... reset first time
      !
      tonext(:,j) = tout
      !
      ! ... check last time
      !
      dttst  = dsec21 ( tout , tlst )
      if ( dttst.lt.0.) flout(j) = .false.
      !
      ! ... check overall first time
      !
      if ( flout(j) ) then
        if ( tofrst(1).eq.-1 ) then
          tofrst = tout
        else
          dttst  = dsec21 ( tout , tofrst )
          if ( dttst.gt.0.) then
            tofrst = tout
          end if
        end if
      end if
      !
    end if
    ! end j=8
    !
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 5')
    !
    ! 4.d preprocessing for point output.
    !
    if ( flout(2) ) call w3iopp ( npt, xpt, ypt, pnames, imod )
    !
    !
    ! 5.  define wavenumber grid ----------------------------------------- *
    ! 5.a calculate depth
    !
    !
    maptst = mod(mapst2/2,2)
    mapst2 = mapst2 - 2*maptst
    !
    !li   for multi-resolution smc grid, these 1-nx and 1-ny nested loops
    !li   may miss the refined cells as they are not 1-1 corresponding to
    !li   the (nx,ny) regular grid.  the loop is now modified to run over
    !li   full nsea points.   jgli24jan2012
    !li   do iy=1, ny
    !li     do ix=1, nx
    !li       isea   = mapfs(iy,ix)
    do isea=1, nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      !li     if ( isea .ne. 0) then
      wlveff=wlv(isea)
      dw(isea) = max ( 0. , wlveff-zb(isea) )
      if ( wlveff-zb(isea) .le.0. ) then
        maptst(iy,ix) = 1
        mapsta(iy,ix) = -abs(mapsta(iy,ix))
      end if
      !li     end if
    end do
    !li   end do
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      wlveff=wlv(isea)
      dw(isea) = max ( 0. , wlveff-zb(isea) )
      if ( wlveff-zb(isea) .le.0. ) then
        va(:,jsea) = 0.
      end if
    end do
    !
    !
    !
    mapst2 = mapst2 + 2*maptst
    !
    deallocate ( maptst )
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 6')
    !
    !
    ! 5.b fill wavenumber and group velocity arrays.
    !
    do is=0, nsea
      if (is.gt.0) then
        depth  = max ( dmin , dw(is) )
      else
        depth = dmin
      end if
      !
      !
      do ik=0, nk+1
        !
        !         calculate wavenumbers and group velocities.
        call wavnu1(sig(ik),depth,wn(ik,is),cg(ik,is))
        !
        !
      end do
    end do
    !
    ! 6.  initialize arrays ---------------------------------------------- /
    !     some initialized in w3iors
    !
    ua     = 0.
    ud     = 0.
    u10    = 0.
    u10d   = 0.
    !
    as     = undef
    !
    as    (0) = 0.
    dw    (0) = 0.
    !
    ! 7.  write info to log file ----------------------------------------- /
    !
    if ( iaproc .eq. naplog ) then
      !
      write (ndso,970) gname
      if (   fllev    ) write (ndso,971) 'prescribed'
      if (.not. fllev ) write (ndso,971) 'no'
      if (   flcur    ) write (ndso,972) 'prescribed'
      if (.not. flcur ) write (ndso,972) 'no'
      if (   flwind   ) write (ndso,973) 'prescribed'
      if (.not. flwind) write (ndso,973) 'no'
      if (   flice    ) write (ndso,974) 'prescribed'
      if (.not. flice ) write (ndso,974) 'no'
      if (   fltaua   ) write (ndso,988) 'prescribed'
      if (.not. fltaua) write (ndso,988) 'no'
      if (   flrhoa   ) write (ndso,989) 'prescribed'
      if (.not. flrhoa) write (ndso,989) 'no'
      !
      if (   flmdn    ) write (ndso,9972) 'prescribed'
      if (.not. flmdn ) write (ndso,9972) 'no'
      if (   flmth    ) write (ndso,9971) 'prescribed'
      if (.not. flmth ) write (ndso,9971) 'no'
      if (   flmvs    ) write (ndso,9970) 'prescribed'
      if (.not. flmvs ) write (ndso,9970) 'no'
      if (   flic1    ) write (ndso,9973) 'prescribed'
      if (.not. flic1 ) write (ndso,9973) 'no'
      if (   flic2    ) write (ndso,9974) 'prescribed'
      if (.not. flic2 ) write (ndso,9974) 'no'
      if (   flic3    ) write (ndso,9975) 'prescribed'
      if (.not. flic3 ) write (ndso,9975) 'no'
      if (   flic4    ) write (ndso,9976) 'prescribed'
      if (.not. flic4 ) write (ndso,9976) 'no'
      if (   flic5    ) write (ndso,9977) 'prescribed'
      if (.not. flic5 ) write (ndso,9977) 'no'
      if ( flout(1) ) then
        write (ndso,975)
        do j=1,nogrp
          do k=1,ngrpp
            if ( flogrd(j,k) ) write (ndso,976) idout(j,k)
          end do
        end do
      end if
      !
      if ( flout(7) ) then
        write (ndso,987)
        do j=1,nogrp
          do k=1,ngrpp
            if ( flogr2(j,k) ) write (ndso,976) idout(j,k)
          end do
        end do
      end if
      !
      if ( flout(2) ) then
        write (ndso,977) nopts
        if ( nopts .eq. 0 ) then
          write (ndso,978)
        else
          if ( flagll ) then
            write (ndso,979)
          else
            write (ndso,985)
          end if
          do ip=1, nopts
            if ( flagll ) then
              write (ndso,980) ip, factor*ptloc(1,ip), factor*ptloc(2,ip), ptnme(ip)
            else
              write (ndso,986) ip, factor*ptloc(1,ip), factor*ptloc(2,ip), ptnme(ip)
            end if
          end do
        end if
      end if
      !
      call stme21 ( time , dtme21 )
      write (ndso,981) dtme21
      if (fllev) then
        call stme21 ( tlev , dtme21 )
        write (ndso,982) dtme21
      end if
      if (flice) then
        call stme21 ( tice , dtme21 )
        write (ndso,983) dtme21
      end if
      if (flrhoa) then
        call stme21 ( trho , dtme21 )
        write (ndso,990) dtme21
      end if
      !
      write (ndso,984)
      !
    end if
    !
    if ( nopts .eq. 0 ) flout(2) = .false.
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_init section 7 - after allocation of group velocities')
    !
    ! boundary set up for the directions
    !
    !
    ! 8.  final mpi set up ----------------------------------------------- /
    !
    call w3mpii ( imod )
    call w3mpio ( imod )
    if ( flout(2) ) call w3mpip ( imod )
    !
    return
    !
    ! escape locations read errors :
    !
    !
888 continue
    if ( iaproc .eq. naperr ) write (ndse,8000) ierr
    call extcde ( 1 )
    !
889 continue
    ! === no process number filtering for test file !!! ===
    write (ndse,8001) ierr
    call extcde ( 2 )
    !
    ! formats
    !
900 format ( ' wavewatch iii log file            ',             &
         '                     version ',a/                     &
         ' ==================================',                 &
         '==================================='/                 &
         50x,'date : ',a10/50x,'time :  ',a8)
920 format (/' model definition file read.')
930 format ( ' restart file read; ',a)
    !
970 format (/' grid name : ',a)
971 format (/' ',a,' water levels.')
972 format ( ' ',a,' curents.')
973 format ( ' ',a,' winds.')
974 format ( ' ',a,' ice fields.')
988 format ( ' ',a,' momentum')
989 format ( ' ',a,' air density')
9972 format( ' ',a,' mud density.')
9971 format( ' ',a,' mud thickness.')
9970 format( ' ',a,' mud viscosity.')
9973 format( ' ',a,' ice parameter 1')
9974 format( ' ',a,' ice parameter 2')
9975 format( ' ',a,' ice parameter 3')
9976 format( ' ',a,' ice parameter 4')
9977 format( ' ',a,' ice parameter 5')
    !
975 format (/' gridded output fields : '/                            &
         '--------------------------------------------------')
976 format ( '     ',a)
    !
977 format (/' point output requested for',i6,' points : '/          &
         '------------------------------------------')
978 format (/'      point output disabled')
979 format                                                           &
         (/'      point  |  longitude  |   latitude  |  name  '/     &
         '     --------|-------------|-------------|----------------')
985 format                                                           &
         (/'      point  |      x      |      y      |  name  '/     &
         '     --------|-------------|-------------|----------------')
980 format ( 5x,i5,'   |',2(f10.2,'   |'),2x,a)
986 format ( 5x,i5,'   |',2(f8.1,'e3   |'),2x,a)
    !
981 format (/' initial time     : ',a)
982 format ( ' water level time : ',a)
983 format ( ' ice field time   : ',a)
990 format ( ' air density time : ',a)
    !
984 format (//                                                       &
         37x,'  |         input         |      output      |'/       &
         37x,'  |-----------------------|------------------|'/       &
         2x,'   step | pass |    date      time   |',                &
         ' b w l c t r i i1 i5 d | g p t r b f c r2 |'/              &
         2x,'--------|------|---------------------|',                &
         '-----------------------|------------------|'/              &
         2x,'--------+------+---------------------+',                &
         '---------------------------+--------------+')
987 format (/' coupling output fields : '/                           &
         '--------------------------------------------------')
    !
8000 format (/' *** wavewatch iii error in w3init : '/               &
         '     error in opening log file'/                           &
         '     iostat =',i5/)
8001 format (/' *** wavewatch iii error in w3init : '/               &
         '     error in opening test file'/                          &
         '     iostat =',i5/)
8002 format (/' *** wavewatch iii warning in w3init : '/             &
         '     significant part of resources reserved for',          &
         ' output :',f6.1,'%'/)
    !
    !/
    !/ end of w3init ----------------------------------------------------- /
    !/
  end subroutine w3init
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief perform initializations for mpi version of model.
  !>  data transpose only.
  !>
  !> @details some derived data types are defined.  all communiction in
  !>  w3gath, w3scat and w3wave are initialized so that all
  !>  communication can be performed with single mpi_startall,
  !>  mpi_testall and mpi_waitall calls.
  !>
  !> @param[in] imod  model number.
  !>
  !> @author h. l. tolman  @date 11-may-2007
  !>
  subroutine w3mpii ( imod )
    !
    use w3gdatmd, only: nsea
    use w3adatmd, only: nsealm
    use w3gdatmd, only: gtype, ungtype
    use constants, only: lpdlib
    use w3gdatmd, only: nspec
    use w3wdatmd, only: va
    use w3adatmd, only: mpi_comm_wave, ww3_field_vec,         &
         ww3_spec_vec, iappro, wadats,         &
         nrqsg1, irqsg1, nrqsg2, irqsg2,       &
         gstore, sstore, mpibuf, bstat,        &
         bispl, isploc, ibfloc, nsploc
    use w3odatmd, only: ndst, naproc, iaproc
    !/
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: nxxxx
    integer                 :: ierr_mpi, isp, ih, itarg,       &
         ierr1, ierr2, ip
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  set up derived data types -------------------------------------- /
    !
    nxxxx  = nsealm * naproc
    !
    call mpi_type_vector ( nsealm, 1, naproc, mpi_real, ww3_field_vec, ierr_mpi )
    call mpi_type_vector ( nsealm, 1, nspec, mpi_real, ww3_spec_vec, ierr_mpi )
    call mpi_type_commit ( ww3_field_vec, ierr_mpi )
    call mpi_type_commit ( ww3_spec_vec, ierr_mpi )
    !
    !
    if( iaproc .gt. naproc ) then
      nsploc = 0
      nrqsg1 = 0
      nrqsg2 = 0
      return
    end if
    !
    ! 2.  set up scatters and gathers for w3wave ------------------------- /
    !     ( persistent communication calls )
    !
      nsploc = 0
      do isp=1, nspec
        if ( iappro(isp) .eq. iaproc ) nsploc = nsploc + 1
      end do
      !
      nrqsg1 = nspec - nsploc
      allocate ( wadats(imod)%irqsg1(max(1,nrqsg1),2) )
      irqsg1 => wadats(imod)%irqsg1
      ih     = 0
      !
      do isp=1, nspec
        if ( iappro(isp) .ne. iaproc ) then
          itarg  = iappro(isp) - 1
          ih     = ih + 1
          call mpi_send_init ( va(isp,1), 1, ww3_spec_vec, itarg, isp, mpi_comm_wave, &
               irqsg1(ih,1), ierr1 )
          call mpi_recv_init ( va(isp,1), 1, ww3_spec_vec, itarg, isp, mpi_comm_wave, &
               irqsg1(ih,2), ierr2 )
        end if
      end do
      !
      ! 3.  set up scatters and gathers for w3scat and w3gath -------------- /
      !     also set up buffering of data.
      !
      nrqsg2 = max( 1 , naproc-1 )
      allocate ( wadats(imod)%irqsg2(nrqsg2*nsploc,2), &
           wadats(imod)%gstore(naproc*nsealm,mpibuf),  &
           wadats(imod)%sstore(naproc*nsealm,mpibuf) )
      nrqsg2 = naproc - 1
      !
      irqsg2 => wadats(imod)%irqsg2
      gstore => wadats(imod)%gstore
      sstore => wadats(imod)%sstore
      !
      ih     = 0
      isploc = 0
      ibfloc = 0
      wadats(imod)%gstore = 0.
      wadats(imod)%sstore = 0.
      !
      ! 3.a loop over local spectral components
      !
      !
      do isp=1, nspec
        if ( iappro(isp) .eq. iaproc ) then
          !
          isploc = isploc + 1
          ibfloc = ibfloc + 1
          if ( ibfloc .gt. mpibuf ) ibfloc = 1
          !
          ! 3.b loop over non-local processes
          !
          do ip=1, naproc
            if ( ip .ne. iaproc ) then
              !
              itarg  = ip - 1
              ih     = ih + 1
              !
              call mpi_recv_init ( wadats(imod)%gstore(ip,ibfloc), 1, ww3_field_vec, &
                   itarg, isp, mpi_comm_wave, irqsg2(ih,1), ierr2 )
              call mpi_send_init ( wadats(imod)%sstore(ip,ibfloc), 1, ww3_field_vec, &
                   itarg, isp, mpi_comm_wave, irqsg2(ih,2), ierr2 )
              !
              ! ... end of loops
              !
            end if
          end do
          !
        end if
      end do
      !
      !
      ! 4.  initialize buffer management ----------------------------------- /
      !
      bstat  = 0
      bispl  = 0
      isploc = 0
      ibfloc = 0
      !
    return
    !
    ! format statements
    !
    !/
    !/ end of w3mpii ----------------------------------------------------- /
    !/
  end subroutine w3mpii
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief prepare mpi persistent communication needed for wavewatch i/o
  !>  routines.
  !>
  !> @details create handles as needed. the communication as set up
  !>  in w3mpii uses tags with number ranging from 1 through nspec.
  !>  new and unique tags for io related communication are assigned
  !>  here dynamically. no testing on imod, since only called by w3init.
  !>
  !> @param[in] imod  model number.
  !>
  !> @author h. l. tolman  @date 11-nov-2015
  !>
  subroutine w3mpio ( imod )
    use w3adatmd, only: w3xdma, w3seta, w3xeta
    use w3iorsmd, only: oarst
    use w3servmd, only: extcde
    !/
    use w3gdatmd, only: nsea
    use w3adatmd, only: nsealm
    use w3gdatmd, only: nx, nspec, mapfs, e3df, p2msf, us3df, usspf
    use w3wdatmd, only: va, ust, ustdir, asf, fpis, icef
    use w3adatmd, only: mpi_comm_wave, ww3_field_vec
    use w3adatmd, only: hs, wlm, t02
    use w3adatmd, only: t0m1, thm, ths, fp0, thp0, &
         dtdyn, fcut, sppnt, aba, abd, uba, ubd,   &
         sxx, syy, sxy, usero, phs, ptp, plp,      &
         pdir, psi, pws, pwst, pnr, phiaw, phioc,  &
         tusx, tusy, tauwix, tauwiy, tauox,        &
         tauoy, ussx, ussy, mssx, mssy, mssd,      &
         mscx, mscy, mscd, prms, tpms, charn,      &
         tws, tauwnx, tauwny, bhd, cge,            &
         cflxymax, cflthmax, cflkmax, whitecap,    &
         bedforms, phibbl, taubbl, t01,            &
         p2sms, us3d, ef,  th1m, sth1m, th2m,      &
         sth2m, hsig, phice, tauice, ussp,         &
         stmaxe, stmaxd, hmaxe, hcmaxe, hmaxd,     &
         hcmaxd, qp, pthp0, pqp, ppe, pgw, psw,    &
         ptm1, pt1, pt2, pep, wbt, cx, cy,         &
         tauocx, tauocy, wnmean
    use w3adatmd, only: usshx, usshy
    use w3gdatmd, only: nk
    use w3odatmd, only: ndst, iaproc, naproc, ntproc, flout,  &
         napfld, nappnt, naprst, napbpt, naptrk,              &
         nogrp, ngrpp, noge, flogrr
    use w3odatmd, only: outpts, nrqgo, nrqgo2, irqgo, irqgo2, &
         flogrd, nrqpo, nrqpo2, irqpo1, irqpo2,               &
         nopts, iptint, nrqrs, irqrs, nblkrs,                 &
         rsblks, irqrss, vaaux, nrqbp, nrqbp2,                &
         irqbp1, irqbp2, nfbpo, nbo2, isbpo,                  &
         abpos, nrqtr, irqtr, it0pnt, it0trk,                 &
         it0prt, noswll, noextr, ndse, iostyp,                &
         flogr2
    use w3parall, only : init_get_jsea_isproc
    use w3gdatmd, only: gtype, ungtype
    use constants, only: lpdlib
    !/
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ifj
    integer                 :: ih, it0, iroot, it, ierr, i0,   &
         ifrom, ix(4), iy(4), is(4),     &
         ip(4), i, j, jsea, itarg, ib,   &
         jsea0, jsean, nseab, iboff,     &
         isea, isproc, k, nrqmax
    logical                 :: flgrdall(nogrp,ngrpp)
    logical                 :: flgrdarst(nogrp,ngrpp)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  set-up for w3iogo ---------------------------------------------- /
    !
    do j=1, nogrp
      do k=1, ngrpp
        flgrdall (j,k) =  (flogrd(j,k) .or. flogr2(j,k))
        flgrdarst(j,k) =  (flgrdall(j,k) .or. flogrr(j,k))
      end do
    end do
    !
    nrqgo  = 0
    nrqgo2 = 0
    it0    = nspec
    iroot  = napfld - 1
    !
    !
    if ((flout(1) .or. flout(7)) .and. (.not. lpdlib)) then
      !
      ! nrqmax is the maximum number of output fields that require mpi communication,
      ! aimed to gather field values stored in each processor into one processor in
      ! charge of model output; for each of such fields, this routine requires one
      ! call to mpi_send_init and mpi_recv_init storing the communication request
      ! handles in the vectors irqgo and irqgo2 respectively.
      ! nrqmax is calculated as the sum of all fields described before (hs)
      !    + 2 or 3 component fields (cur) + 3 component fields + extra fields
      ! for group 1 fields except icef, all processors contain information on all
      ! grid points because they are input fields, and therefore this mpi
      ! communication is not necessary and they do not contribute to nrqmax.
      !
      ! calculation of nrqmax splitted by output groups and field type
      !       scalar                2-comp   3-comp
      nrqmax =   1           +    0  +    0  +  &  ! group 1
           18                +    0  +    0  +  &  ! group 2
           0                 +    0  +    0  +  &  ! group 3 (extra contributions below)
           2+(noge(4)-2)*(noswll+1) +    0  +    0  +  &  ! group 4
           11                +    3  +    1  +  &  ! group 5
           10                +    7  +    1  +  &  ! group 6 (extra contributions below)
           5                 +    4  +    1  +  &  ! group 7
           5                 +    2  +    0  +  &  ! group 8
           5                 +    0  +    0  +  &  ! group 9
           noextr            +    0  +    0        ! group 10
      ! extra contributions to nrqmax from group 3
      do ifj=1,5
        if ( flgrdall( 3,ifj)) nrqmax = nrqmax + e3df(3,ifj) - e3df(2,ifj) + 1
      end do
      ! extra contributions to nrqmax from group 6
      if ( flgrdall( 6,9)) nrqmax = nrqmax + p2msf(3) - p2msf(2) + 1
      if ( flgrdall( 6, 8) ) nrqmax = nrqmax + 2*nk
      if ( flgrdall( 6,12) ) nrqmax = nrqmax + 2*nk
      if ( flgrdall( 6,14) ) nrqmax = nrqmax + 2
      !
      if ( nrqmax .gt. 0 ) then
        allocate ( outpts(imod)%out1%irqgo(nrqmax) )
        allocate ( outpts(imod)%out1%irqgo2(nrqmax*naproc) )
      end if
      irqgo  => outpts(imod)%out1%irqgo
      irqgo2 => outpts(imod)%out1%irqgo2
      !
      ! 1.a sends of fields
      !
      ih     = 0
      !
      if ( iaproc .le. naproc ) then
        it     = it0
        !
        if ( flgrdall( 1, 12) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (icef (iaproc), 1, ww3_field_vec, iroot, it, &
               mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 1) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (hs   (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 2) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (wlm  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 3) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (t02  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 4) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (t0m1  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 5) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (t01  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 6) .or. flgrdall( 2,18) ) then
          ! tp output shares fp0 internal field with fp
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (fp0  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 7) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (thm  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 8) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (ths  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 9) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (thp0 (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 10) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (hsig (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 11) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (stmaxe (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 12) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (stmaxd (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 13) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (hmaxe (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 14) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (hcmaxe (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 15) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (hmaxd (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 16) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (hcmaxd (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 17) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (wbt  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 2, 19) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (wnmean(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 3, 1) ) then
          do ik=e3df(2,1),e3df(3,1)
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (ef(1,ik),nsealm , mpi_real, iroot, &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 3, 2) ) then
          do ik=e3df(2,2),e3df(3,2)
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (th1m(1,ik),nsealm , mpi_real, iroot, &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 3, 3) ) then
          do ik=e3df(2,3),e3df(3,3)
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (sth1m(1,ik),nsealm , mpi_real, iroot, &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 3, 4) ) then
          do ik=e3df(2,4),e3df(3,4)
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (th2m(1,ik),nsealm , mpi_real, iroot, &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 3, 5) ) then
          do ik=e3df(2,5),e3df(3,5)
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (sth2m(1,ik),nsealm , mpi_real, iroot, &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 1) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (phs(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 2) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (ptp(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 3) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (plp(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 4) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pdir(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 5) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (psi(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 6) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pws(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 7) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pthp0(1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 8) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pqp (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4, 9) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (ppe (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4,10) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pgw (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4,11) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (psw (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4,12) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (ptm1(1,k),nsealm , mpi_real, iroot,   &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        !
        if ( flgrdall( 4,13) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pt1 (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4,14) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pt2 (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4,15) ) then
          do k=0, noswll
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (pep (1,k),nsealm , mpi_real, iroot,    &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 4,16) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (pwst (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 4,17) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (pnr  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 1) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (ust   (iaproc), 1, ww3_field_vec,      &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr )
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (ustdir(iaproc), 1, ww3_field_vec,       &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr )
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (asf   (iaproc), 1, ww3_field_vec,       &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr )
        end if
        !
        if ( flgrdall( 5, 2) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (charn(1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 3) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (cge  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 4) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (phiaw(1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 5) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauwix(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauwiy(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 6) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauwnx(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauwny(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 7) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (whitecap(1,1),nsealm , mpi_real, iroot,&
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 8) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (whitecap(1,2),nsealm , mpi_real, iroot,&
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 9) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (whitecap(1,3),nsealm , mpi_real, iroot,&
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5,10) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (whitecap(1,4),nsealm , mpi_real, iroot,&
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 5, 11) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tws(1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 1) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (sxx   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (syy   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (sxy   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 2) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauox (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauoy (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 3) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (bhd(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 4) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (phioc (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 5) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tusx  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tusy  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 6) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (ussx  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (ussy  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 7) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (prms  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tpms  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 8) ) then
          do ik=1,2*nk
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (us3d(1,ik),nsealm , mpi_real, iroot,  &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 6, 9) ) then
          do k=p2msf(2),p2msf(3)
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (p2sms(1,k),nsealm , mpi_real, iroot,  &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 6,10) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauice (1,1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauice (1,2),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6,11) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (phice (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 12) ) then
          do ik=1,2*nk
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (ussp(1,ik),nsealm , mpi_real, iroot,  &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end do
        end if
        !
        if ( flgrdall( 6, 13) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauocx(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (tauocy(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 6, 14) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (usshx (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (usshy (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        if ( flgrdall( 7, 1) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (aba   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (abd   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 7, 2) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (uba   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (ubd   (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 7, 3) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (bedforms(1,1),nsealm , mpi_real,      &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (bedforms(1,2),nsealm , mpi_real,      &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (bedforms(1,3),nsealm , mpi_real,      &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 7, 4) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (phibbl(1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 7, 5) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (taubbl(1,1),nsealm , mpi_real,        &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (taubbl(1,2),nsealm , mpi_real,        &
               iroot, it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 8, 1) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (mssx  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (mssy  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 8, 2) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (mscx  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (mscy  (1),nsealm , mpi_real, iroot,   &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 8, 3) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (mssd  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 8, 4) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (mscd  (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 8, 5) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (qp    (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 9, 1) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (dtdyn(1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 9, 2) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (fcut (1),nsealm , mpi_real, iroot,    &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 9, 3) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (cflxymax(1),nsealm , mpi_real, iroot, &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 9, 4) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (cflthmax(1),nsealm , mpi_real, iroot, &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        if ( flgrdall( 9, 5) ) then
          ih     = ih + 1
          it     = it + 1
          call mpi_send_init (cflkmax(1),nsealm , mpi_real, iroot,  &
               it, mpi_comm_wave, irqgo(ih), ierr)
        end if
        !
        do i=1, noextr
          if ( flgrdall(10, i) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_send_init (usero(1,i),nsealm , mpi_real, iroot,  &
                 it, mpi_comm_wave, irqgo(ih), ierr)
          end if
        end do
        !
        nrqgo  = ih
        !
      end if !if ( iaproc .le. naproc ) then
      !
      if ( nrqgo .gt. nrqmax ) then
        write (ndse,1010) nrqgo, nrqmax
        call extcde (10)
      end if
      !
      if ( iaproc .eq. napfld ) then
        !
        ! 1.b setting up expanded arrays
        !
        if (napfld .eq. naprst) then
          call w3xdma ( imod, ndse, ndst, flgrdarst )
        else
          call w3xdma ( imod, ndse, ndst, flgrdall )
        endif
        !
        ! 1.c receives of fields
        !
        call w3xeta ( imod, ndse, ndst )
        !
        ih     = 0
        !
        do i0=1, naproc
          it     = it0
          ifrom  = i0 - 1
          !
          if ( flgrdall( 1, 12) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (icef (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 1) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (hs   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 2) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (wlm  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 3) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (t02  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 4) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (t0m1  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 5) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (t01(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 6) .or. flgrdall( 2,18) ) then
            ! tp output shares fp0 internal field with fp
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (fp0  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 7) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (thm  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 8) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (ths  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 9) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (thp0 (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 10) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (hsig (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 11) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (stmaxe (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 12) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (stmaxd(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 13) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (hmaxe (i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 14) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (hcmaxe(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 15) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (hmaxd (i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 16) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (hcmaxd(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 17) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (wbt(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 2, 19) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (wnmean(i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 3, 1) ) then
            do ik=e3df(2,1),e3df(3,1)
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (ef(i0,ik),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 3, 2) ) then
            do ik=e3df(2,2),e3df(3,2)
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (th1m(i0,ik),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 3, 3) ) then
            do ik=e3df(2,3),e3df(3,3)
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (sth1m(i0,ik),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 3, 4) ) then
            do ik=e3df(2,4),e3df(3,4)
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (th2m(i0,ik),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 3, 5) ) then
            do ik=e3df(2,5),e3df(3,5)
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (sth2m(i0,ik),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 1) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (phs(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 2) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (ptp(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 3) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (plp(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 4) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pdir(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 5) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (psi(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 6) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pws(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 7) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pthp0(i0,k),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 8) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pqp(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4, 9) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (ppe(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,10) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pgw(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,11) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (psw(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,12) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (ptm1(i0,k),1,ww3_field_vec, ifrom, it,&
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,13) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pt1(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,14) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pt2(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,15) ) then
            do k=0, noswll
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (pep(i0,k),1,ww3_field_vec, ifrom, it,  &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 4,16) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (pwst (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 4,17) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (pnr  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 1) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (ust   (i0), 1, ww3_field_vec, ifrom,   &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (ustdir(i0), 1, ww3_field_vec, ifrom,   &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (asf   (i0), 1, ww3_field_vec, ifrom,   &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 2) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (charn(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 3) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (cge  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 4) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (phiaw(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 5) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauwix(i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauwiy(i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 6) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauwnx(i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauwny(i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 7) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (whitecap(i0,1),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 8) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (whitecap(i0,2),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5, 9) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (whitecap(i0,3),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5,10) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (whitecap(i0,4),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 5,11) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tws(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 1) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (sxx   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (syy   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (sxy   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 2) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauox (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauoy (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 3) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (bhd(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 4) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (phioc (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 5) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tusx  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tusy  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 6) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (ussx  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (ussy  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 7) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (prms  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tpms  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 8) ) then
            do ik=1,2*nk
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (us3d(i0,ik),1,ww3_field_vec, ifrom, it, &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if (  flgrdall( 6, 9) ) then
            do k=p2msf(2),p2msf(3)
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (p2sms(i0,k),1,ww3_field_vec, ifrom, it, &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 6,10) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauice (i0,1),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauice (i0,2),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6,11) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (phice (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 12) ) then
            do ik=1,2*nk
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (ussp(i0,ik),1,ww3_field_vec, ifrom, it, &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end do
          end if
          !
          if ( flgrdall( 6, 13) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauocx(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (tauocy(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 6, 14) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (usshx (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (usshy (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 7, 1) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (aba   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (abd   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 7, 2) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (uba   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (ubd   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 7, 3) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (bedforms(i0,1),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (bedforms(i0,2),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (bedforms(i0,3),1,ww3_field_vec, ifrom,  &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 7, 4) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (phibbl(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 7, 5) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (taubbl(i0,1),1,ww3_field_vec, ifrom,    &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (taubbl(i0,2),1,ww3_field_vec, ifrom,    &
                 it, mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 8, 1) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (mssx  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (mssy  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 8, 2) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (mscx  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (mscy  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 8, 3) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (mssd  (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 8, 4) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (mscd (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 8, 5) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (qp   (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 9, 1) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (dtdyn(i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 9, 2) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (fcut (i0),1,ww3_field_vec, ifrom, it,  &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 9, 3) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (cflxymax(i0),1,ww3_field_vec, ifrom, it,&
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 9, 4) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (cflthmax(i0),1,ww3_field_vec, ifrom, it,&
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          if ( flgrdall( 9, 5) ) then
            ih     = ih + 1
            it     = it + 1
            call mpi_recv_init (cflkmax(i0),1,ww3_field_vec, ifrom, it, &
                 mpi_comm_wave, irqgo2(ih), ierr )
          end if
          !
          do i=1, noextr
            !write(740+iaproc,*) 'second : i=', i, ' / ', noextr, ' val=', flgrdall(10, i)
            if ( flgrdall(10, i) ) then
              ih     = ih + 1
              it     = it + 1
              call mpi_recv_init (usero(i0,i),1,ww3_field_vec, ifrom, it, &
                   mpi_comm_wave, irqgo2(ih), ierr )
            end if
          end do
          !
        end do
        !
        nrqgo2 = ih
        !
        call w3seta ( imod, ndse, ndst )
        !
      end if ! if ( iaproc .eq. napfld ) then
      !
      if ( nrqgo2 .gt. nrqmax*naproc ) then
        write (ndse,1011) nrqgo2, nrqmax*naproc
        call extcde (11)
      end if
      !
    end if ! if ((flout(1) .or. flout(7)) .and. (.not. lpdlib)) then
    !
    ! 2.  set-up for w3iors ---------------------------------------------- /
    ! 2.a general preparations
    !
    nrqrs  = 0
    ih     = 0
    iroot  = naprst - 1
    !
    if ((flout(4) .or. flout(8)) .and. (.not. lpdlib)) then
      if (oarst) then
        allocate ( outpts(imod)%out4%irqrs(34*naproc) )
      else
        allocate ( outpts(imod)%out4%irqrs(3*naproc) )
      endif
      irqrs  => outpts(imod)%out4%irqrs
      !
      ! 2.b fields at end of file (always)
      !
      !
      if ( iaproc.ne.naprst .and. iaproc.le.naproc ) then
        !
        ih     = ih + 1
        it     = it0 + 1
        call mpi_send_init (ust (iaproc), 1, ww3_field_vec, &
             iroot, it, mpi_comm_wave, irqrs(ih), ierr )
        !
        ih     = ih + 1
        it     = it0 + 2
        call mpi_send_init (ustdir(iaproc), 1, ww3_field_vec, &
             iroot, it, mpi_comm_wave, irqrs(ih), ierr )
        !
        ih     = ih + 1
        it     = it0 + 3
        call mpi_send_init (fpis(iaproc), 1, ww3_field_vec, &
             iroot, it, mpi_comm_wave, irqrs(ih), ierr )
        !
      else if ( iaproc .eq. naprst ) then
        do i0=1, naproc
          ifrom  = i0 - 1
          if ( i0 .ne. iaproc ) then
            !
            ih     = ih + 1
            it     = it0 + 1
            call mpi_recv_init (ust (i0),1,ww3_field_vec, &
                 ifrom, it, mpi_comm_wave, irqrs(ih), ierr )
            !
            ih     = ih + 1
            it     = it0 + 2
            call mpi_recv_init (ustdir(i0),1,ww3_field_vec, &
                 ifrom, it, mpi_comm_wave, irqrs(ih), ierr )
            !
            ih     = ih + 1
            it     = it0 + 3
            call mpi_recv_init (fpis(i0),1,ww3_field_vec, &
                 ifrom, it, mpi_comm_wave, irqrs(ih), ierr )
          end if
        end do
      end if
      !
 !
      nrqrs  = ih
      if (oarst) then
        it0    = it0 + 34
      else
        it0    = it0 + 3
      endif
      !
      !
      ! 2.c data server mode
      !
      if ( iostyp .gt. 0 ) then
        !
        nblkrs = 10
        rsblks = max ( 5 , nsealm/nblkrs )
        if ( nblkrs*rsblks .lt. nsealm ) rsblks = rsblks + 1
        nblkrs = 1 + (nsealm-1)/rsblks
        !
        ih     = 0
        !
        if ( iaproc .ne. naprst ) then
          !
          allocate ( outpts(imod)%out4%irqrss(nblkrs) )
          irqrss => outpts(imod)%out4%irqrss
          !
          do ib=1, nblkrs
            ih     = ih + 1
            it     = it0 + 3 + ib
            jsea0  = 1 + (ib-1)*rsblks
            jsean  = min ( nsealm , ib*rsblks )
            nseab  = 1 + jsean - jsea0
            call mpi_send_init (va(1,jsea0), nspec*nseab, mpi_real, iroot, it, &
                 mpi_comm_wave, irqrss(ih), ierr )
          end do
          !
        else
          !
          allocate ( outpts(imod)%out4%irqrss(naproc*nblkrs) ,  &
               outpts(imod)%out4%vaaux(nspec,2*rsblks,naproc) )
          !
          irqrss => outpts(imod)%out4%irqrss
          vaaux  => outpts(imod)%out4%vaaux
          do ib=1, nblkrs
            it     = it0 + 3 + ib
            jsea0  = 1 + (ib-1)*rsblks
            jsean  = min ( nsealm , ib*rsblks )
            nseab  = 1 + jsean - jsea0
            do i0=1, naproc
              if ( i0 .ne. naprst ) then
                ih     = ih + 1
                ifrom  = i0 - 1
                iboff  = mod(ib-1,2)*rsblks
                call mpi_recv_init (vaaux(1,1+iboff,i0), nspec*nseab, mpi_real, &
                     ifrom, it, mpi_comm_wave, irqrss(ih), ierr )
              end if
            end do
          end do
          !
        end if
        !
        it0    = it0 + nblkrs
        !
      end if
      !
    end if ! if ((flout(4) .or. flout(8)) .and. (.not. lpdlib)) then
    !
    ! 3.  set-up for w3iobc ( sends ) ------------------------------------ /
    !
    nrqbp  = 0
    nrqbp2 = 0
    ih     = 0
    it     = it0
    iroot  = napbpt - 1
    !
    if ( flout(5) ) then
      allocate ( outpts(imod)%out5%irqbp1(nbo2(nfbpo)),  &
           outpts(imod)%out5%irqbp2(nbo2(nfbpo)) )
      irqbp1 => outpts(imod)%out5%irqbp1
      irqbp2 => outpts(imod)%out5%irqbp2
      !
      ! 3.a loops over files and points
      !
      !
      do j=1, nfbpo
        do i=nbo2(j-1)+1, nbo2(j)
          !
          it     = it + 1
          !
          ! 3.b residence processor of point
          !
          isea   = isbpo(i)
          call init_get_jsea_isproc(isea, jsea, isproc)
          !
          ! 3.c if stored locally, send data
          !
          if ( iaproc .eq. isproc ) then
            ih     = ih + 1
            call mpi_send_init (va(1,jsea),nspec,mpi_real, iroot, it, mpi_comm_wave, &
                 irqbp1(ih), ierr)
          end if
          !
        end do
      end do
      !
      ! ... end of loops 4.a
      !
      nrqbp  = ih
      !
      !
      ! 3.d set-up for w3iobc ( recvs ) ------------------------------------ /
      !
      if ( iaproc .eq. napbpt ) then
        !
        ih     = 0
        it     = it0
        !
        ! 3.e loops over files and points
        !
        !
        do j=1, nfbpo
          do i=nbo2(j-1)+1, nbo2(j)
            !
            ! 3.f residence processor of point
            !
            isea   = isbpo(i)
            call init_get_jsea_isproc(isea, jsea, isproc)
            !
            ! 3.g receive in correct array
            !
            ih     = ih + 1
            it     = it + 1
            itarg  = isproc - 1
            call mpi_recv_init (abpos(1,ih),nspec,mpi_real, itarg, it, mpi_comm_wave, &
                 irqbp2(ih), ierr)
            !
          end do
        end do
        !
        nrqbp2 = ih
        !
        ! ... end of loops 4.e
        !
        !
      end if
      !
      it0    = it0 + nbo2(nfbpo)
      !
    end if
    !
    !
    ! 4.  set-up for w3iotr ---------------------------------------------- /
    !
    ih     = 0
    iroot  = naptrk - 1
    !
    if ( flout(3) ) then
      !
      ! 4.a u*
      !
      !
      if ( iaproc .ne. naptrk ) then
        allocate ( outpts(imod)%out3%irqtr(2) )
        irqtr  => outpts(imod)%out3%irqtr
        ih     = ih + 1
        it     = it0 + 1
        call mpi_send_init (ust   (iaproc),1,ww3_field_vec, iroot, it, mpi_comm_wave, &
             irqtr(ih), ierr )
        ih     = ih + 1
        it     = it0 + 2
        call mpi_send_init (ustdir(iaproc),1,ww3_field_vec, iroot, it, mpi_comm_wave, &
             irqtr(ih), ierr )
      else
        allocate ( outpts(imod)%out3%irqtr(2*naproc) )
        irqtr  => outpts(imod)%out3%irqtr
        do i0=1, naproc
          ifrom  = i0 - 1
          if ( i0 .ne. iaproc ) then
            ih     = ih + 1
            it     = it0 + 1
            call mpi_recv_init(ust   (i0),1,ww3_field_vec, ifrom, it, mpi_comm_wave, &
                 irqtr(ih), ierr)
            ih     = ih + 1
            it     = it0 + 2
            call mpi_recv_init(ustdir(i0),1,ww3_field_vec, ifrom, it, mpi_comm_wave, &
                 irqtr(ih), ierr)
          end if
        end do
      end if
      !
      nrqtr  = ih
      it0    = it0 + 2
      !
      !
    end if
    !
    ! 5.  set-up remaining counters -------------------------------------- /
    !
    it0prt = it0
    it0pnt = it0prt + 2*naproc
    it0trk = it0pnt + 5000
    !
    return
    !
    !     formats :
    !
1010 format (/' *** error w3mpio : array irqgo too small *** '/)
1011 format (/' *** error w3mpio : array irqgo2 too small *** '/)
    !
    !/
    !/ end of w3mpio ----------------------------------------------------- /
    !/
  end subroutine w3mpio
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief prepare mpi persistent communication needed for wavewatch i/o
  !>  routines.
  !>
  !> @details create handles as needed.
  !>
  !> @param[in] imod  model number.
  !>
  !> @author h. l. tolman  @date 30-oct-2009
  !>
  subroutine w3mpip ( imod )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         30-oct-2009 |
    !/                  +-----------------------------------+
    !/
    !/    02-aug-2006 : origination.                        ( version 3.10 )
    !/    17-may-2007 : adding ntproc/naproc separation.    ( version 3.11 )
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/
    !  1. purpose :
    !
    !     prepare mpi persistent communication needed for wavewatch i/o
    !     routines.
    !
    !  2. method :
    !
    !     create handles as needed.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       imod    int.   i   model number.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !
    !      mpi_send_init, mpi_recv_init
    !                subr. mpif.h   mpi persistent communication calls.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3init    subr. w3initmd wave model initialization routine.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/mpi   mpi communication calls.
    !
    !       !/s     enable subroutine tracing.
    !       !/mpit  enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only: extcde
    !/
    use w3gdatmd, only: nx, ny, nspec, mapfs
    use w3wdatmd, only: va
    use w3adatmd, only: mpi_comm_wave, sppnt
    use w3odatmd, only: ndst, ndse, iaproc, naproc, nappnt, flout
    use w3odatmd, only: outpts, nrqpo, nrqpo2, irqpo1, irqpo2, &
         nopts, iptint, it0pnt, it0trk, o2irqi
    use w3parall, only: init_get_jsea_isproc
    !/
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: imod
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ih, iroot, i, j, it, it0, jsea, &
         ierr, itarg, ix(4), iy(4),      &
         k, is(4), ip(4)
    integer                 :: itout
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    if ( o2irqi ) then
      write (ndse,1001)
      call extcde (1)
    end if
    !
    ! 1.  set-up for w3iope/o ( sends ) ---------------------------------- /
    !
    nrqpo  = 0
    nrqpo2 = 0
    ih     = 0
    it0    = it0pnt
    iroot  = nappnt - 1
    !
    allocate ( outpts(imod)%out2%irqpo1(4*nopts),  &
         outpts(imod)%out2%irqpo2(4*nopts) )
    irqpo1 => outpts(imod)%out2%irqpo1
    irqpo2 => outpts(imod)%out2%irqpo2
    o2irqi = .true.
    !
    ! 1.a loop over output locations
    !
    !
    do i=1, nopts
      do k=1,4
        ix(k)=iptint(1,k,i)
        iy(k)=iptint(2,k,i)
      end do
      ! 1.b loop over corner points
      !
      do j=1, 4
        !
        it     = it0 + (i-1)*4 + j
        is(j)  = mapfs (iy(j),ix(j))
        if ( is(j) .eq. 0 ) then
          jsea   = 0
          ip(j)  = nappnt
        else
          call init_get_jsea_isproc(is(j), jsea, ip(j))
        end if
        !
        ! 1.c send if point is stored here
        !
        if ( ip(j) .eq. iaproc ) then
          ih     = ih + 1
          call mpi_send_init ( va(1,jsea), nspec, mpi_real, iroot, it, mpi_comm_wave, &
               irqpo1(ih), ierr )
        end if
        !
        ! ... end of loop 1.b
        !
      end do
      !
      ! ... end of loop 1.a
      !
    end do
    !
    nrqpo  = ih
    !
    !
    ! 1.d set-up for w3iope/o ( recvs ) ---------------------------------- /
    !
    if ( iaproc .eq. nappnt ) then
      !
      ih     = 0
      !
      ! 2.e loop over output locations
      !
      !
      do i=1, nopts
        do k=1,4
          ix(k)=iptint(1,k,i)
          iy(k)=iptint(2,k,i)
        end do
        !
        do j=1, 4
          !
          it     = it0 + (i-1)*4 + j
          is(j)  = mapfs (iy(j),ix(j))
          if ( is(j) .eq. 0 ) then
            jsea   = 0
            ip(j)  = nappnt
          else
            call init_get_jsea_isproc(is(j), jsea, ip(j))
          end if
          !
          ! 1.g receive in correct array
          !
          ih     = ih + 1
          itarg  = ip(j) - 1
          call mpi_recv_init ( sppnt(1,1,j), nspec, mpi_real, itarg, it, mpi_comm_wave, &
               irqpo2(ih), ierr )
          !
          ! ... end of loop 1.f
          !
        end do
        !
        ! ... end of loop 1.e
        !
      end do
      !
      nrqpo2 = nopts*4
      !
      !
    end if
    !
    !
    it0    = it0 + 8*nopts
    !
    ! 1.h base tag number for track output
    !
    it0trk = it0
    !
    return
    !
    !     formats :
    !
1001 format (/' *** error w3mpip : arrays already allocated *** '/)
    !
    !/
    !/ end of w3mpip ----------------------------------------------------- /
    !/
  end subroutine w3mpip
  !/
  !/ end of module w3initmd -------------------------------------------- /
  !/
end module w3initmd
