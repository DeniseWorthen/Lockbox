!> @file
!> @brief read/write restart files.
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
!/ ------------------------------------------------------------------- /
!>
!> @brief read/write restart files.
!>
!> @author h. l. tolman  @date 22-mar-2021
!>
module w3iorsmd
  !
  !/ ------------------------------------------------------------------- /
  !module default
  implicit none
  public
  !/
  ! add fields needed for oasis coupling in restart
  logical :: oarst
  !/
  !/ private parameter statements (id strings)
  !/
  character(len=10), parameter, private :: verini = '2021-05-28'
  character(len=26), parameter, private ::                        &
       idstr = 'wavewatch iii restart file'
  !/
contains
  !> @param[in]    inxout   test string for read/write.
  !> @param[inout] ndsr     file unit number.
  !> @param[in]    dumfpi   dummy values for fpis for cold start.
  !> @param[in]    imod     optional grid number, defaults to 1.
  !> @param[in]    flrstrt  a second request for restart files (optional true).
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3iors ( inxout, ndsr, dumfpi, imod, flrstrt )
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: w3setg, w3setref, rstype
    use w3odatmd, only: w3seto
    use w3wdatmd, only: w3setw, w3dimw
    use w3adatmd, only: w3seta, w3xeta, nsealm
    use w3adatmd, only: cx, cy, hs, wlm, t0m1, t01, fp0, thm, charn,&
         tauwix, tauwiy, tws, tauox, tauoy, bhd,     &
         phioc, tusx, tusy, ussx, ussy, tauice,      &
         uba, ubd, phibbl, taubbl, tauocx, tauocy,   &
         wnmean
    !/
    use w3gdatmd, only: nx, ny, nsea, nseal, nspec, mapsta, mapst2, &
         gname, filext, gtype, ungtype
    use w3triamd, only: set_ug_iobp
    use w3wdatmd, only : dinit, va, time, tlev, tice, trho, ice, ust
    use w3wdatmd, only : ustdir, asf, fpis, icef, tic1, tic5, wlv
    use w3odatmd, only: ndse, ndst, iaproc, naproc, naperr, naprst, &
         ifile => ifile4, fnmpre, ntproc, iostyp,    &
         flogrr, nogrp, ngrpp, screen
    use w3odatmd, only: nrqrs, nblkrs, rsblks, irqrs, irqrss,  &
         vaaux
    use w3adatmd, only: mpi_comm_wcmp
    !/
    use w3servmd, only: extcde
    use constants, only: lpdlib, file_endian
    use w3parall, only: init_get_isea, init_get_jsea_isproc
    use w3gdatmd, only: nk, nth
    use w3odatmd, only : runtype, initfile
    use w3adatmd, only : usshx, usshy
    !
    use w3timemd, only: set_user_timestring
    use w3odatmd, only: use_user_restname, user_restfname, ndso
    !
    include "mpif.h"
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer                       :: ndsr
    !      integer, intent(in)           :: ndsr
    integer, intent(in), optional :: imod
    real, intent(inout)           :: dumfpi
    character, intent(in)         :: inxout*(*)
    logical, intent(in),optional  :: flrstrt
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, parameter      :: lrb = 4
    !
    integer                 :: igrd, i, j, lrecl, nsize, ierr,      &
         nseat, mspec, ttime(2), isea, jsea,  &
         nrec, npart, ipart, ix, iy, ixl, ip, &
         nprtx2, nprty2, iyl, itmp
    integer, allocatable    :: maptmp(:,:)
    integer                 :: ierr_mpi, ih, ib, isea0, isean, &
         nrq, nseal_min
    integer(kind=8)         :: rpos
    integer, allocatable    :: stat1(:,:), stat2(:,:)
    real, allocatable       :: vgbuff(:), vlbuff(:)
    real(kind=lrb), allocatable :: writebuff(:), tmp(:), tmp2(:)
    logical                 :: write, iosflg
    logical                 :: flogoa(nogrp,ngrpp)
    logical                 :: ndsropn
    character(len=4)        :: type
    character(len=10)       :: vertst
    character(len=512)      :: fname
    character(len=26)       :: idtst
    character(len=30)       :: tname
    character(len=15)       :: timetag
    character(len=16)       :: user_timestring    !yyyy-mm-dd-sssss
    logical                 :: exists
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! constant ndsr for using mpiifort in zeus ... paralell runs crashing
    !  because compiler doesn't accept reciclyng of unit for formatted or
    !  unformatted files in open
    !
    !     ndsr = 525
    iosflg = iostyp .gt. 0
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
    !
    if (inxout.ne.'read' .and. inxout.ne.'hot'  .and.               &
         inxout.ne.'cold' .and. inxout.ne.'wind' .and.               &
         inxout.ne.'calm' ) then
      if ( iaproc .eq. naperr ) write (ndse,900) inxout
      call extcde ( 1 )
    end if
    !
    write = inxout .ne. 'read'
    if ( inxout .eq. 'hot' ) then
      type   = 'full'
    else
      type   = inxout
    end if
    !
    !
    ! initializations ---------------------------------------------------- *
    !
    if ( .not.dinit ) then
      if ( iaproc .le. naproc ) then
        call w3dimw ( imod, ndse, ndst )
      else
        call w3dimw ( imod, ndse, ndst, .false. )
      end if
    end if
    !
    if ( iaproc .le. naproc ) va(:,0) = 0.
    !
    lrecl  = max ( lrb*nspec ,                                      &
         lrb*(6+(25/lrb)+(9/lrb)+(29/lrb)+(3/lrb)) )
    nsize  = lrecl / lrb
    !     --- allocate buffer array with zeros (used to
    !         fill bytes up to size lrecl). ---
    allocate(writebuff(nsize))
    writebuff(:) = 0.
    !
    !     allocate memory to receive fields needed for coupling
    if (oarst) then
      allocate(tmp(nsea))
      allocate(tmp2(nsea))
    endif
    !
    ! open file ---------------------------------------------------------- *
    !
    if (use_user_restname) then
      ierr = -99
      if (.not. write) then
        if (runtype == 'initial') then
          if (len_trim(initfile) == 0) then
            ! no ic file, use startup option
            goto 800
          else
            ! ic file exists - use it
            fname = trim(initfile)
          end if
        else
          call set_user_timestring(time,user_timestring)
          fname = trim(user_restfname)//trim(user_timestring)
          inquire( file=trim(fname), exist=exists)
          if (.not. exists) then
            call extcde (60, msg="required initial/restart file " // trim(fname) // " does not exist")
          end if
        end if
      else
        call set_user_timestring(time,user_timestring)
        fname = trim(user_restfname)//trim(user_timestring)
      end if
      ! write out filename
      if (iaproc == naprst) then
        if ( write ) then
          write (ndso,'(a)') 'ww3: writing restart file '//trim(fname)
        else
          write (ndso,'(a)') 'ww3: reading initial/restart file '//trim(fname)
        end if
      end if
      if ( write ) then
        if ( .not.iosflg .or. iaproc.eq.naprst )        &
             open (ndsr,file=trim(fname), form='unformatted', convert=file_endian,       &
             access='stream',err=800,iostat=ierr)
      else  ! read
        open (ndsr, file=trim(fname), form='unformatted', convert=file_endian,       &
             access='stream',err=800,iostat=ierr,           &
             status='old',action='read')
      end if
    else
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      !
      !checkpoint restart file
      itmp=0
      if ( present(flrstrt) ) then
        if (flrstrt) then
          write(timetag,"(i8.8,'.'i6.6)")time(1),time(2)
          fname=timetag//'.restart.'//filext(:i)
          itmp=1
        end if
      end if
      if(itmp.ne.1)then ! fname is not set above, so do it here
        if ( ifile.eq.0 ) then
          fname  = 'restart.'//filext(:i)
        else
          fname  = 'restartnnn.'//filext(:i)
          if ( write .and. iaproc.eq.naprst )                         &
               write (fname(8:10),'(i3.3)') ifile
        end if
      end if
      ifile  = ifile + 1
      !
      !
      if(ndst.eq.ndsr)then
        if ( iaproc .eq. naperr )                                    &
             write(ndse,'(a,i8)')'unit numbers of restart file and '&
             //'test output are the same : ',ndst
        call extcde ( 15 )
      endif
      if ( write ) then
        if ( .not.iosflg .or. iaproc.eq.naprst )                    &
             open (ndsr,file=fnmpre(:j)//trim(fname),form='unformatted', convert=file_endian,       &
             access='stream',err=800,iostat=ierr)
      else
        open (ndsr,file=fnmpre(:j)//trim(fname),form='unformatted', convert=file_endian,       &
             access='stream',err=800,iostat=ierr,                  &
             status='old',action='read')
      end if
    end if
    !
    ! test info ---------------------------------------------------------- *
    !
    if ( write ) then
      !
      if ( iaproc .eq. naprst ) then
        !           because data has mixed data types we do not know how many
        !           bytes remain to fill up to lrecl. ---
        !           --- make the entire record zero ---
        writebuff(:) = 0.
        write (ndsr,pos=1) writebuff
        !           --- replace zeros with data ---
        write (ndsr,pos=1) idstr, verini, gname, type, nsea,      &
             nspec, flogrr
      end if
      rstype = 3
      !
    else
      read (ndsr,pos=1,err=802,iostat=ierr)                       &
           idtst, vertst, tname, type, nseat, mspec, flogoa
      !
      if ( idtst .ne. idstr ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,901) idtst, idstr
        call extcde ( 10 )
      end if
      if ( vertst .ne. verini ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,902) vertst, verini
        call extcde ( 11 )
      end if
      if ( tname .ne. gname ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,903) tname, gname
      end if
      if (type.ne.'full' .and. type.ne.'cold' .and.               &
           type.ne.'wind' .and. type.ne.'calm' ) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,904) type
        call extcde ( 12 )
      end if
      if (nseat.ne.nsea .or. nspec.ne.mspec) then
        if ( iaproc .eq. naperr )                               &
             write (ndse,905) mspec, nseat, nspec, nsea
        call extcde ( 13 )
      end if
      if (type.eq.'full') then
        rstype = 2
      else if (type.eq.'wind') then
        rstype = 1
      else if (type.eq.'calm') then
        rstype = 4
      else
        rstype = 0
      end if
      if (.not. write .and. oarst .and. iaproc .eq. naproc) then
        do i=1, nogrp
          do j=1, ngrpp
            if (flogrr(i,j) .and. .not. flogoa(i,j)) then
              write(screen,1000) i, j
            endif
          enddo
        enddo
      endif
      !
    end if
    !
100 continue
    !
    !
    ! time if required --------------------------------------------------- *
    !
    if (type.eq.'full') then
      rpos  = 1_8 + lrecl*(2-1_8)
      if ( write ) then
        if ( iaproc .eq. naprst ) then
          writebuff(:) = 0.
          write (ndsr,pos=rpos) writebuff
          write (ndsr,pos=rpos) time
        end if
      else
        read (ndsr,pos=rpos,err=802,iostat=ierr) ttime
        if (time(1).ne.ttime(1) .or. time(2).ne.ttime(2)) then
          if ( iaproc .eq. naperr )                           &
               write (ndse,906) ttime, time
          call extcde ( 20 )
        end if
      end if
      !
      !
    end if
    !
    ! spectra ------------------------------------------------------------ *
    !          ( bail out if write for type.eq.'wind' )
    !
    if ( write ) then
      if ( type.eq.'wind' .or. type.eq.'calm' ) then
        if ( .not.iosflg .or. iaproc.eq.naprst ) then
          close ( ndsr )
        end if
        ! clean up file handles and allocated arrays
        inquire (unit=ndsr, opened=ndsropn)
        if (ndsropn)              close(ndsr)
        if (allocated(writebuff)) deallocate(writebuff)
        if (allocated(tmp))       deallocate(tmp)
        if (allocated(tmp2))      deallocate(tmp2)
        return
      else if ( iaproc.le.naproc .or. iaproc.eq. naprst ) then
        !
        ! original non-server version writing of spectra
        !
        if ( .not.iosflg .or. (naproc.eq.1.and.naprst.eq.1) ) then
          do jsea=1, nseal
            call init_get_isea(isea, jsea)
            nrec   = isea + 2
            rpos  = 1_8 + lrecl*(nrec-1_8)
            writebuff(:) = 0.
            writebuff(1:nspec) = va(1:nspec,jsea)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
          end do
          !
          ! i/o server version writing of spectra ( !/mpi )
          !
        else
          !
          if (lpdlib) then
          else
            if ( iaproc .ne. naprst ) then
              nrq    = 1
            else if ( naprst .le. naproc ) then
              nrq    = naproc - 1
            else
              nrq    = naproc
            end if
            !
            allocate ( stat1(mpi_status_size,nrq) )
            if ( iaproc .eq. naprst ) call mpi_startall    &
                 ( nrq, irqrss, ierr_mpi )
            !
            do ib=1, nblkrs
              isea0  = 1 + (ib-1)*rsblks*naproc
              isean  = min ( nsea , ib*rsblks*naproc )
              !
              if ( iaproc .eq. naprst ) then
                !
                ih     = 1 + nrq * (ib-1)
                call mpi_waitall                         &
                     ( nrq, irqrss(ih), stat1, ierr_mpi )
                if ( ib .lt. nblkrs ) then
                  ih     = 1 + nrq * ib
                  call mpi_startall                    &
                       ( nrq, irqrss(ih), ierr_mpi )
                end if
                !
                do isea=isea0, isean
                  nrec   = isea + 2
                  call init_get_jsea_isproc(isea, jsea, ip)
                  rpos   = 1_8 + lrecl*(nrec-1_8)
                  writebuff(:) = 0.
                  if ( ip .eq. naprst ) then
                    writebuff(1:nspec) = va(1:nspec,jsea)
                  else
                    jsea   = jsea - 2*((ib-1)/2)*rsblks
                    writebuff(1:nspec) = vaaux(1:nspec,jsea,ip)
                  end if
                  write (ndsr,pos=rpos,err=803,iostat=ierr) &
                       writebuff
                end do
                !
              else
                !
                call mpi_startall                        &
                     ( 1, irqrss(ib), ierr_mpi )
                call mpi_waitall                         &
                     ( 1, irqrss(ib), stat1, ierr_mpi )
                !
              end if
            end do
            !
            deallocate ( stat1 )
          end if
          !
        end if
        !
      end if
    else
      !
      ! reading spectra
      !
      if ( type.eq.'wind' .or. type.eq.'calm' ) then
      else
        if (lpdlib) then
        else
          nseal_min = 1 + (nsea-naproc)/naproc
          if ( naproc.gt.1 ) then
            !/ ----------- large number of small-sized record reads will tend ---- *
            !/             to perform badly on most file systems. we read this part
            !/             using streams and scatter the results using mpi.
            !/                                                      ( m. ward, nci )
            !
            !              begin computational proc. only section ---------------- *
            if ( iaproc.le.naproc ) then
              !
              !              main loop --------------------------------------------- *
              allocate( vgbuff( nsize * naproc ) )
              allocate( vlbuff( nsize ) )
              !
              do jsea = 1, nseal_min
                !                read naproc records into buffer vgbuff. ------------- *
                if ( iaproc .eq. naproc ) then
                  rpos = 1_8 + (2 + (jsea - 1_8) * naproc) * lrecl
                  read(ndsr, pos=rpos,err=802,iostat=ierr) vgbuff(:)
                else
                  vgbuff(:) = 0.
                end if
                !                distribute one record to each rank.
                call mpi_scatter(vgbuff, nsize, mpi_real,             &
                     vlbuff, nsize, mpi_real,             &
                     naproc-1, mpi_comm_wcmp, ierr        )
                !                transfer the spectral content of vlbuff to va. ------ *
                va(1:nspec,jsea) = vlbuff(1:nspec)
              end do
              !
              !              include remainder values (switch to record format) ---- *
              jsea = nseal_min + 1
              if ( jsea.eq.nseal ) then
                call init_get_isea(isea, jsea)
                nrec = isea + 2
                rpos = 1_8 + lrecl*(nrec-1_8)
                read (ndsr, pos=rpos, err=802, iostat=ierr)          &
                     (va(i,jsea), i=1,nspec)
              end if
              !
              deallocate( vgbuff )
              deallocate( vlbuff )
              !
              !              end computational proc. only section ------------------ *
            end if
            !
          else
            va = 0.
            do jsea=1, nsea
              call init_get_isea(isea, jsea)
              nrec   = isea + 2
              rpos   = 1_8 + lrecl*(nrec-1_8)
              read (ndsr,pos=rpos,err=802,iostat=ierr)              &
                   (va(i,jsea),i=1,nspec)
            enddo
          end if
        end if
      end if
    end if
    va = max(0.,va)
    !
    !
    ! water level etc. if required --------------------------------------- *
    !     ( for cold start write test output and cold start initialize
    !       water levels. note that mapsta overwrites the one read from the
    !       model definition file, so that it need not be initialized. )
    !
    nrec   = nsea + 3
    npart  = 1 + (nsea-1)/nsize
    nprtx2 = 1 + (nx-1)/nsize
    nprty2 = 1 + (ny-1)/nsize
    !
    if ( write ) then
      !
      if (type.eq.'full') then
        !
        if ( iaproc .eq. naprst ) then
          !
          if (associated(irqrs)) then
            allocate ( stat2(mpi_status_size,nrqrs) )
            call mpi_waitall                               &
                 ( nrqrs, irqrs , stat2, ierr_mpi )
            deallocate ( stat2 )
          end if
          !
          rpos  = 1_8 + lrecl*(nrec-1_8)
          writebuff(:) = 0.
          write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
          write (ndsr,pos=rpos,err=803,iostat=ierr)           &
               tlev, tice, trho
          do ipart=1,npart
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
            write (ndsr,pos=rpos,err=803,iostat=ierr)         &
                 (wlv(isea),isea=1+(ipart-1)*nsize,          &
                 min(nsea,ipart*nsize))
          end do
          do ipart=1,npart
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
            write (ndsr,pos=rpos,err=803,iostat=ierr)         &
                 (ice(isea),isea=1+(ipart-1)*nsize,          &
                 min(nsea,ipart*nsize))
          end do
          allocate ( maptmp(ny,nx) )
          maptmp = mapsta + 8*mapst2
          do iy=1, ny
            do ipart=1,nprtx2
              nrec  = nrec + 1
              rpos  = 1_8 + lrecl*(nrec-1_8)
              write (ndsr,pos=rpos,err=803,iostat=ierr)       &
                   writebuff
              write (ndsr,pos=rpos,err=803,iostat=ierr)       &
                   (maptmp(iy,ixl),ixl=1+(ipart-1)*nsize,    &
                   min(nx,ipart*nsize))
            end do
          end do
          deallocate ( maptmp )
          do ipart=1,npart
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
            write (ndsr,pos=rpos,err=803,iostat=ierr)         &
                 (ust(isea),isea=1+(ipart-1)*nsize,          &
                 min(nsea,ipart*nsize))
          end do
          do ipart=1,npart
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
            write (ndsr,pos=rpos,err=803,iostat=ierr)         &
                 (ustdir(isea),isea=1+(ipart-1)*nsize,       &
                 min(nsea,ipart*nsize))
          end do
          do ipart=1,npart
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
            write (ndsr,pos=rpos,err=803,iostat=ierr)         &
                 (asf(isea),isea=1+(ipart-1)*nsize,          &
                 min(nsea,ipart*nsize))
          end do
          do ipart=1,npart
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            write (ndsr,pos=rpos,err=803,iostat=ierr) writebuff
            write (ndsr,pos=rpos,err=803,iostat=ierr)         &
                 (fpis(isea),isea=1+(ipart-1)*nsize,         &
                 min(nsea,ipart*nsize))
          end do
        end if
      end if
    else
      if (type.eq.'full') then
        rpos = 1_8 + lrecl*(nrec-1_8)
        read (ndsr,pos=rpos,err=802,iostat=ierr)                &
             tlev, tice, trho
        do ipart=1,npart
          nrec  = nrec + 1
          rpos = 1_8 + lrecl*(nrec-1_8)
          read (ndsr,pos=rpos,err=802,iostat=ierr)              &
               (wlv(isea),isea=1+(ipart-1)*nsize,              &
               min(nsea,ipart*nsize))
        end do
        do ipart=1,npart
          nrec  = nrec + 1
          rpos = 1_8 + lrecl*(nrec-1_8)
          read (ndsr,pos=rpos,err=802,iostat=ierr)              &
               (ice(isea),isea=1+(ipart-1)*nsize,              &
               min(nsea,ipart*nsize))
        end do
        allocate ( maptmp(ny,nx) )
        do iy=1, ny
          do ipart=1,nprtx2
            nrec  = nrec + 1
            rpos  = 1_8 + lrecl*(nrec-1_8)
            read (ndsr,pos=rpos,err=802,iostat=ierr)            &
                 (maptmp(iy,ixl),ixl=1+(ipart-1)*nsize,        &
                 min(nx,ipart*nsize))
          end do
        end do
        mapsta = mod(maptmp+2,8) - 2
        mapst2 = (maptmp-mapsta) / 8
        deallocate ( maptmp )
        !
        ! updates reflections maps:
        !
        if (gtype.eq.ungtype) then
        endif
        !
        do ipart=1,npart
          nrec  = nrec + 1
          rpos  = 1_8 + lrecl*(nrec-1_8)
          read (ndsr,pos=rpos,err=802,iostat=ierr)              &
               (ust(isea),isea=1+(ipart-1)*nsize,              &
               min(nsea,ipart*nsize))
        end do
        do ipart=1,npart
          nrec  = nrec + 1
          rpos  = 1_8 + lrecl*(nrec-1_8)
          read (ndsr,pos=rpos,err=802,iostat=ierr)              &
               (ustdir(isea),isea=1+(ipart-1)*nsize,           &
               min(nsea,ipart*nsize))
        end do
        do ipart=1,npart
          nrec  = nrec + 1
          rpos  = 1_8 + lrecl*(nrec-1_8)
          read (ndsr,pos=rpos,err=802,iostat=ierr)              &
               (asf(isea),isea=1+(ipart-1)*nsize,              &
               min(nsea,ipart*nsize))
        end do
        do ipart=1,npart
          nrec  = nrec + 1
          rpos  = 1_8 + lrecl*(nrec-1_8)
          read (ndsr,pos=rpos,err=802,iostat=ierr)              &
               (fpis(isea),isea=1+(ipart-1)*nsize,             &
               min(nsea,ipart*nsize))
        end do
      else
        tlev(1) = -1
        tlev(2) =  0
        tice(1) = -1
        tice(2) =  0
        trho(1) = -1
        tic1(1) = -1
        tic1(2) =  0
        tic5(1) = -1
        tic5(2) =  0
        wlv     =  0.
        ice     =  0.
        asf     =  1.
        fpis    =  dumfpi
      end if
    end if
    !
    ! close file --------------------------------------------------------- *
    !
    if (write) then
      if ( .not.iosflg .or. iaproc.eq.naprst ) then
        close ( ndsr )
      end if
    else
      close ( ndsr )
    end if
    !
    if (allocated(writebuff)) deallocate(writebuff)
    if (allocated(tmp))  deallocate(tmp)
    if (allocated(tmp2)) deallocate(tmp2)
    !
    return
    !
    ! escape locations read errors :
    !
800 continue
    if ( iaproc .eq. naperr ) write (ndse,990) type, ierr
    goto 100
    !
801 continue
    if ( iaproc .eq. naperr ) write (ndse,991)
    call extcde ( 30 )
    !
802 continue
    if ( iaproc .eq. naperr ) write (ndse,992) ierr
    call extcde ( 31 )
    !
803 continue
    if ( iaproc .eq. naperr ) write (ndse,993) ierr, rpos
    call extcde ( 31 )
    !
    !
    ! formats
    !
900 format (/' *** wavewatch iii error in w3iors :'/                &
         '     illegal inxout value: ',a/)
901 format (/' *** wavewatch iii error in w3iors :'/                &
         '     illegal idstr, read : ',a/                       &
         '                   check : ',a/)
902 format (/' *** wavewatch iii error in w3iors :'/                &
         '     illegal verini, read : ',a/                      &
         '                    check : ',a/)
903 format (/' *** wavewatch iii warning in w3iors :'/              &
         '     illegal gname, read : ',a/                       &
         '                   check : ',a/)
904 format (/' *** wavewatch iii error in w3iors :'/                &
         '     illegal type : ',a/)
905 format (/' *** wavewatch iii error in w3iors :'/                &
         '     conflicting nspec, nsea grid : ',2i8/            &
         '                         expected : ',2i8/)
906 format (/' *** wavewatch iii error in w3iors :'/                &
         '     conflicting times: file : ',i10.8,i8.6/          &
         '                       model : ',i10.8,i8.6/)
    !
990 format (/' *** wavewatch iii warning in w3iors : '/             &
         '     no readable restart file, ',                     &
         'initialize with ''',a,''' instead'/              &
         '     iostat =',i5/)
991 format (/' *** wavewatch iii error in w3iors : '/               &
         '     premature end of file'/)
992 format (/' *** wavewatch iii error in w3iors : '/               &
         '     error in reading from file'/                     &
         '     iostat =',i5/)
993 format (/' *** wavewatch iii error in w3iors : '/               &
         '     error in writing to file'/                       &
         '     iostat =',i5,', pos =',i11 /)
1000 format (/' *** wavewatch iii warning in w3iors : '/             &
         '     requested extra restart group',i2,' field',i2, / &
         '     is not present in the restart file.'/            &
         '     this may cause instabilities in coupled configurations')
    !
    !
    !/
    !/ end of w3iors ----------------------------------------------------- /
    !/
  end subroutine w3iors
  !/
  !/ end of module w3iorsmd -------------------------------------------- /
  !/
end module w3iorsmd
