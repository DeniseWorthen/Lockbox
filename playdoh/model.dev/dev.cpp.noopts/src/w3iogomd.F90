!> @file
!> @brief gridded output of mean wave parameters.
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
!> @brief gridded output of mean wave parameters.
!>
!> @author h. l. tolman  @date 22-mar-2021
!>
!/ ------------------------------------------------------------------- /
module w3iogomd
  !module default
  implicit none
  !/
  public
  character(len=1024)                   :: fldout
  !/
  !/ private parameter statements (id strings)
  !/
  character(len=10), parameter, private :: verogr = '2019-10-04'
  character(len=30), parameter, private ::                        &
       idstr = 'wavewatch iii grid output file'
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief updates the flags for output parameters based on the mod_def file
  !>  this is to prevent the allocation of big 3d arrays when not requested.
  !>
  !> @param[in]    ndso   output file logical unit number.
  !> @param[in]    ndsen  error output file logical unit number.
  !> @param[inout] flgrd  1d array of flags for groups.
  !> @param[inout] flgr2  1d array of flags for groups.
  !> @param[inout] flgd   2d array of flags.
  !> @param[inout] flg2   2d array of flags.
  !>
  !> @author f. ardhuin  @date 15-apr-2013
  !>
  subroutine w3flgrdupdt ( ndso, ndsen, flgrd, flgr2, flgd, flg2 )
    use constants
    use w3gdatmd, only: e3df, p2msf, us3df, usspf
    use w3odatmd, only: nogrp, ngrpp
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndso, ndsen
    logical, intent(inout)  :: flgrd(nogrp,ngrpp), flgd(nogrp),     &
         flgr2(nogrp,ngrpp), flg2(nogrp)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer             :: i
    character(len=10)  :: varname1(5),varname2(5)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    varname1(1) = 'ef';    varname2(1) = 'e3d'
    varname1(2) = 'th1m';  varname2(2) = 'th1mf'
    varname1(3) = 'sth1m'; varname2(3) = 'sth1mf'
    varname1(4) = 'th2m';  varname2(4) = 'th2mf'
    varname1(5) = 'sth2m'; varname2(5) = 'sth2mf'
    do i=1,5
      if (e3df(1,i).le.0.or.e3df(3,i).lt.e3df(2,i)) then
        if (flgrd(3,i).or.flgr2(3,i)) then
          write(ndsen,1008) varname1(i),varname2(i)
        end if
        flgrd(3,i)=.false.
        flgr2(3,i)=.false.
      end if
    end do
    if (us3df(1).le.0.or.us3df(3).lt.us3df(2)) then
      if (flgrd(6,8).or.flgr2(6,8)) then
        write(ndsen,1008) 'usf','us3d'
      end if
      flgrd(6,8)=.false.
      flgr2(6,8)=.false.
    end if
    if (usspf(1).le.0.or.usspf(2).le.0) then
      if (flgrd(6,12).or.flgr2(6,12)) then
        write(ndsen,1008) 'usp','ussp'
      end if
      flgrd(6,12)=.false.
      flgr2(6,12)=.false.
    end if
    if (p2msf(1).le.0.or.p2msf(3).lt.p2msf(2)) then
      if (flgrd(6,9).or.flgr2(6,9)) then
        write(ndsen,1008) 'p2l','p2sf'
      end if
      flgrd(6,9)=.false.
      flgr2(6,9)=.false.
    end if
    !
    flgd(3) = .false.
    flg2(3) = .false.
    if(any(flgrd(3,:))) flgd(3)=.true.
    if(any(flgr2(3,:))) flg2(3)=.true.
    flgd(6) = .false.
    flg2(6) = .false.
    if(any(flgrd(6,:))) flgd(6)=.true.
    if(any(flgr2(6,:))) flg2(6)=.true.
    !
    return
    !
1008 format (/' *** wavewatch iii warning  : '/                       &
         '     parameter ',a,' not allowed: need to set',        &
         ' parameter ',a,' in outs namelist (in ww3_grid.inp)'   &
         ' with proper bounds' )
    !
  end subroutine w3flgrdupdt
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief fills in flg1d and flg2d arrays from ascii input file.
  !>
  !> @param[in] ndsi    input file logical unit number.
  !> @param[in] ndso    output file logical unit number.
  !> @param[in] ndss    screen file logical unit number.
  !> @param[in] ndsen   error output file logical unit number.
  !> @param[in] comstr  comment string, usually '$'.
  !> @param[out] flg1d  1d array of flags for groups.
  !> @param[out] flg2d  2d array of flags.
  !> @param[in] iaproc  index of current processor.
  !> @param[in] napout  index of processor for output (screen).
  !> @param[out] ierr   error message number.
  !>
  !> @author f. ardhuin  @date 25-sep-2020
  !>
  subroutine w3readflgrd ( ndsi , ndso, ndss, ndsen, comstr,      &
       flg1d, flg2d, iaproc, napout, ierr)
    use constants
    use w3gdatmd, only: us3df, usspf
    use w3odatmd, only: nogrp, ngrpp, noge, idout
    use w3servmd, only: nextln, strsplit, str_to_upper
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndsi, ndso, ndss, ndsen, iaproc, napout
    integer, intent(out)    :: ierr
    character(len=1)        :: comstr
    logical, intent(out)    :: flg2d(nogrp,ngrpp), flg1d(nogrp)
    character(len=100)      :: out_names(100), teststr
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer             :: ifi, ifj, iout
    character(len=1)    :: aflg
    logical             :: flt, names
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  initialize flags -------------------------------------- *
    !
    ierr=0
    flg2d(:,:)=.false. ! initialize flg2d
    flg1d(:)=.false. ! initialize flog
    names =.false.
    !
    do ifi=1,nogrp ! loop over field output groups
      !
      call nextln ( comstr , ndsi , ndsen )
      read (ndsi,*,end=2001,err=2002) aflg
      if (aflg.eq.'t') then
        flg1d(ifi)=.true.
      else if (aflg.eq.'f') then
        flg1d(ifi)=.false.
      else if (aflg.eq.'n') then
        names=.true.
        exit
      else
        ierr=1
        goto 2005
      end if
      if ( flg1d (ifi) ) then ! skip if group not requested
        call nextln ( comstr , ndsi , ndsen )
        read (ndsi,'(a)',end=2001,err=2006,iostat=ierr)              &
             fldout
        out_names(:)=''
        call strsplit(fldout,out_names)
        ifj=0
        do while (len_trim(out_names(ifj+1)).ne.0)
          ifj=ifj+1
          if ( out_names(ifj) .eq. 't' )                            &
               flg2d(ifi,ifj)=.true.
        enddo
        if ( iaproc .eq. napout .and. ifj .lt. noge(ifi) ) write(ndsen,1007) ifi
      endif
    end do
    !
    if (names) then
      !
      ! 2. reads and splits list of output field names
      !
      call nextln ( comstr , ndsi , ndsen )
      read (ndsi,'(a)',end=2001,err=2003,iostat=ierr) fldout
      out_names(:)=''
      call strsplit(fldout,out_names)
      iout=0
      do while (len_trim(out_names(iout+1)).ne.0)
        call str_to_upper(out_names(iout+1))
        !
        ! 2. matches names with expected ...
        !
        teststr=out_names(iout+1)
        call w3fldtoij(teststr, ifi, ifj, iaproc, napout, ndsen)
        if(ifi .ne. -1) then
          flg2d(ifi, ifj) = .true.
        endif
        !
        iout=iout+1
        !
      end do
      !
    end if
    !
    flt    = .true.
    do ifi=1, nogrp
      if ( iaproc .eq. napout ) then
        do ifj=1, ngrpp
          if ( flg2d(ifi,ifj) ) then
            if ( flt ) then
              write (ndso,1945) idout(ifi,ifj)
              flt    = .false.
            else
              write (ndso,1946) idout(ifi,ifj)
            end if
          end if
        end do
      end if
      if(any(flg2d(ifi,:))) flg1d(ifi)=.true. !update flg1d
    end do
    if ( iaproc .eq. napout ) then
      if ( flt ) write (ndso,1945) 'no fields defined'
    end if
    !
    return
    !
2001 continue
    if ( iaproc .eq. napout ) write (ndsen,1001)
    return
2002 continue
    if ( iaproc .eq. napout ) write (ndsen, 1002) ifi, ierr
    return
2003 continue
    if ( iaproc .eq. napout ) write (ndsen, 1003) ierr
    return
    !2004 continue ! replaced by warning in code ....
2005 continue
    if ( iaproc .eq. napout ) write (ndsen, 1005) aflg
    return
2006 continue
    if ( iaproc .eq. napout ) write (ndsen, 1006) ifi,ierr
    return
    !
1945 format ( '            fields   : ',a)
1946 format ( '                       ',a)
    !
1001 format (/' *** wavewatch iii error  : '/                         &
         '     premature end of input file'/)
    !
1002 format (/' *** wavewatch iii error  : '/                         &
         '     error in reading output fields group flags ',     &
         i2, /, '     iostat =',i5/)
    !
1003 format (/' *** wavewatch iii error  : '/                         &
         '     error reading output field names from input file'/&
         '     iostat =',i5/)
    !
1005 format (/' *** wavewatch iii error    : '/                       &
         '     was expecting "t" "f" or "n", but found "',a,'".'/)
    !
1006 format (/' *** wavewatch iii error  : '/                         &
         '     error in reading output fields flags for group ', &
         i2, /, '     iostat =',i5/)
    !
1007 format (/' *** wavewatch iii warning  : '/                       &
         '     number of requested output field flags in group ',&
         i2, /,' less than available, check docs for more options')
    !
  end subroutine w3readflgrd
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief fills in flg1d and flg2d arrays from ascii input file.
  !>
  !> @param[in] ndso    output file logical unit number.
  !> @param[in] ndss    screen file logical unit number.
  !> @param[in] ndsen   error output file logical unit number.
  !> @param[in] fldout  list of field names.
  !> @param[out] flg1d  1d array of flags for groups.
  !> @param[out] flg2d  2d array of flags.
  !> @param[in] iaproc  index of current processor.
  !> @param[in] napout  index of processor for output (screen).
  !> @param[out] ierr   error message number.
  !>
  !> @author f. ardhuin  @date 25-sep-2020
  !>
  subroutine w3flgrdflag ( ndso, ndss, ndsen, fldout,      &
       flg1d, flg2d, iaproc, napout, ierr)
    use constants
    use w3odatmd, only: nogrp, ngrpp, idout
    use w3servmd, only: strsplit, str_to_upper
    use w3gdatmd, only: us3df, usspf
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: ndso, ndss, ndsen, iaproc, napout
    character(1024), intent(in)   :: fldout
    integer, intent(out)    :: ierr
    logical, intent(out)    :: flg2d(nogrp,ngrpp), flg1d(nogrp)
    character(len=100)      :: out_names(100), teststr
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer             :: i, ifi, ifj, iout
    logical             :: flt
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  initialize flags -------------------------------------- *
    !
    ierr=0
    flg2d(:,:)=.false. ! initialize flg2d
    flg1d(:)=.false. ! initialize flog
    !
    ! 2. splits list of output field names
    !
    out_names(:)=''
    call strsplit(fldout,out_names)
    iout=0
    do while (len_trim(out_names(iout+1)).ne.0)
      call str_to_upper(out_names(iout+1))
      !
      ! 2. matches names with expected ...
      !
      teststr=out_names(iout+1)
      call w3fldtoij(teststr, ifi, ifj, iaproc, napout, ndsen)
      if(ifi .ne. -1) then
        flg2d(ifi, ifj) = .true.
      endif
      !
      iout=iout+1
      !
    end do
    !
    flt    = .true.
    do ifi=1, nogrp
      if ( iaproc .eq. napout ) then
        do ifj=1, ngrpp
          if ( flg2d(ifi,ifj) ) then
            if ( flt ) then
              write (ndso,1945) idout(ifi,ifj)
              flt    = .false.
            else
              write (ndso,1946) idout(ifi,ifj)
            end if
          end if
        end do
      endif
      if(any(flg2d(ifi,:))) flg1d(ifi)=.true. !update flg1d
    end do
    if ( iaproc .eq. napout ) then
      if ( flt ) write (ndso,1945) 'no fields defined'
    endif
    !
    return
    !
1945 format ( '            fields   : ',a)
1946 format ( '                       ',a)
    !
    ! 1004 format (/' *** wavewatch iii warning  : '/                       &
    !               '     requested output field ',a,' was not recognized.'/)
    !!
    ! 1008 format (/' *** wavewatch iii warning  : '/                       &
    !               '     parameter ',a,' not allowed: need to set',        &
    !               ' parameter ',a,' in outs namelist (in ww3_grid.inp)')
    !
  end subroutine w3flgrdflag
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief returns the group/field (i/j) indices for a named output field.
  !>
  !> @param[in]  fld     field names.
  !> @param[out] i       output group number (ifi).
  !> @param[out] j       output field number (ifj).
  !> @param[in]  iaproc  index of current processor.
  !> @param[in]  napout  index of processor for output (screen).
  !> @param[in]  ndsen   error output file logical unit number.
  !>
  !> @author c. bunney  @date 22-mar-2021
  !>
  subroutine w3fldtoij(fld, i, j, iaproc, napout, ndsen)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            c. bunney              |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    03-nov-2020 : origination.                        ( version 7.12 )
    !/    22-mar-2021 : add extra coupling fields as output ( version 7.13 )
    !
    !  1. purpose :
    !
    !     returns the group/field (i/j) indices for a named output field.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       fld     cha.   i   field names
    !       i       int.   o   output group number (ifi)
    !       j       int.   o   output field number (ifj)
    !       iaproc  int.   i   index of current processor
    !       napout  int.   i   index of processor for output (screen)
    !       ndsen   r.a.   i   error output file logical unit number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: us3df, usspf
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    character(len=*), intent(in) :: fld
    integer, intent(in) :: iaproc, napout, ndsen
    integer, intent(out) :: i, j
    i = -1
    j = -1
    select case(trim(fld(1:6)))
      !
      ! group 1
      !
    case('dpt')
      i = 1
      j = 1
    case('cur')
      i = 1
      j = 2
    case('wnd')
      i = 1
      j = 3
    case('ast')
      i = 1
      j = 4
    case('wlv')
      i = 1
      j = 5
    case('ice')
      i = 1
      j = 6
    case('ibg')
      i = 1
      j = 7
    case('tau')
      i = 1
      j = 8
    case('rho')
      i = 1
      j = 9
      ! group 2
      !
    case('hs')
      i = 2
      j = 1
    case('lm')
      i = 2
      j = 2
    case('t02')
      i = 2
      j = 3
    case('t0m1')
      i = 2
      j = 4
    case('t01')
      i = 2
      j = 5
    case('fp')
      i = 2
      j = 6
    case('dir')
      i = 2
      j = 7
    case('spr')
      i = 2
      j = 8
    case('dp')
      i = 2
      j = 9
    case('hig')
      i = 2
      j = 10
    case('mxe')
      i = 2
      j = 11
    case('mxes')
      i = 2
      j = 12
    case('mxh')
      i = 2
      j = 13
    case('mxhc')
      i = 2
      j = 14
    case('sdmh')
      i = 2
      j = 15
    case('sdmhc')
      i = 2
      j = 16
    case('wbt')
      i = 2
      j = 17
    case('tp') ! uses fp0 internally, as per fp
      i = 2
      j = 18
    case('wnm')
      i = 2
      j = 19
      !
      ! group 3
      !
    case('ef')
      i = 3
      j = 1
    case('th1m')
      i = 3
      j = 2
    case('sth1m')
      i = 3
      j = 3
    case('th2m')
      i = 3
      j = 4
    case('sth2m')
      i = 3
      j = 5
    case('wn')
      i = 3
      j = 6
      !
      ! group 4
      !
    case('phs')
      i = 4
      j = 1
    case('ptp')
      i = 4
      j = 2
    case('plp')
      i = 4
      j = 3
    case('pdir')
      i = 4
      j = 4
    case('pspr')
      i = 4
      j = 5
    case('pws')
      i = 4
      j = 6
    case('pdp')
      i = 4
      j = 7
    case('pqp')
      i = 4
      j = 8
    case('ppe')
      i = 4
      j = 9
    case('pgw')
      i = 4
      j = 10
    case('psw')
      i = 4
      j = 11
    case('ptm10')
      i = 4
      j = 12
    case('pt01')
      i = 4
      j = 13
    case('pt02')
      i = 4
      j = 14
    case('pep')
      i = 4
      j = 15
    case('tws')
      i = 4
      j = 16
    case('pnr')
      i = 4
      j = 17
      !
      ! group 5
      !
    case('ust')
      i = 5
      j = 1
    case('cha')
      i = 5
      j = 2
    case('cge')
      i = 5
      j = 3
    case('faw')
      i = 5
      j = 4
    case('taw')
      i = 5
      j = 5
    case('twa')
      i = 5
      j = 6
    case('wcc')
      i = 5
      j = 7
    case('wcf')
      i = 5
      j = 8
    case('wch')
      i = 5
      j = 9
    case('wcm')
      i = 5
      j = 10
    case('fws')
      i = 5
      j = 11
      !
      ! group 6
      !
    case('sxy')
      i = 6
      j = 1
    case('two')
      i = 6
      j = 2
    case('bhd')
      i = 6
      j = 3
    case('foc')
      i = 6
      j = 4
    case('tus')
      i = 6
      j = 5
    case('uss')
      i = 6
      j = 6
    case('p2s')
      i = 6
      j = 7
    case('usf')
      if (us3df(1).ge.1) then
        i = 6
        j = 8
      else
        if ( iaproc .eq. napout ) write(ndsen,1008) 'usf','us3d'
      end if
    case('p2l')
      i = 6
      j = 9
    case('twi')
      i = 6
      j = 10
    case('fic')
      i = 6
      j = 11
    case('usp')
      if (usspf(1).ge.1) then
        i = 6
        j = 12
      else
        if ( iaproc .eq. napout ) write(ndsen,1008) 'usp','ussp'
      end if
    case('toc')
      i = 6
      j = 13
      !
      ! group 7
      !
    case('abr')
      i = 7
      j = 1
    case('ubr')
      i = 7
      j = 2
    case('bed')
      i = 7
      j = 3
    case('fbb')
      i = 7
      j = 4
    case('tbb')
      i = 7
      j = 5
      !
      ! group 8
      !
    case('mss')
      i = 8
      j = 1
    case('msc')
      i = 8
      j = 2
    case('msd')
      i = 8
      j = 3
    case('mcd')
      i = 8
      j = 4
    case('qp')
      i = 8
      j = 5
    case('qkk')
      i = 8
      j = 6
    case('skw')
      i = 8
      j = 7
    case('emb')
      i = 8
      j = 8
    case('emc')
      i = 8
      j = 9
      !
      ! group 9
      !
    case('dtd')
      i = 9
      j = 1
    case('fc')
      i = 9
      j = 2
    case('cfx')
      i = 9
      j = 3
    case('cfd')
      i = 9
      j = 4
    case('cfk')
      i = 9
      j = 5
      !
      ! group 10
      !
    case('u1')
      i = 10
      j = 1
    case('u2')
      i = 10
      j = 1
      ! not found:
    case('unset')
    case default
      i = -1
      j = -1
      if ( iaproc .eq. napout ) write (ndsen,1004) trim(fld)
    end select
1004 format (/' *** wavewatch iii warning  : '/                       &
         '     requested output field ',a,' was not recognized.'/)
    !
1008 format (/' *** wavewatch iii warning  : '/                       &
         '     parameter ',a,' not allowed: need to set',        &
         ' parameter ',a,' in outs namelist (in ww3_grid.inp)')
    !
  end subroutine w3fldtoij
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief fill necessary arrays with gridded data for output.
  !>
  !> @param[in] a        input spectra, left in par list to changeshape.
  !> @param[in] flpart   flag for filling fields with partition data.
  !> @param[in] floutg   flag for file field output.
  !> @param[in] floutg2  flag for coupling field output.
  !>
  !> @author h. l. tolman  @date 10-apr-2015
  !>
  subroutine w3outg ( a, flpart, floutg, floutg2 )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-apr-2015 |
    !/                  +-----------------------------------+
    !/
    !/    10-dec-1998 : distributed fortran 77 version.     ( version 1.18 )
    !/    04-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    09-may-2002 : switch clean up.                    ( version 2.21 )
    !/    19-oct-2004 : multiple grid version.              ( version 3.06 )
    !/    21-jul-2005 : adding output fields 19-21.         ( version 3.07 )
    !/    23-apr-2006 : filter for directional spread.      ( version 3.09 )
    !/    02-apr-2007 : adding partitioned output.          ( version 3.11 )
    !/                  adding user slots for outputs.
    !/    08-oct-2007 : adding st3 source term option.      ( version 3.13 )
    !/                  ( f. ardhuin )
    !/    05-mar-2008 : added nec sxf90 compiler directives
    !/                  (chris bunney, uk met office)       ( version 3.13 )
    !/    25-dec-2012 : new output structure and smaller    ( version 4.11 )
    !/                  memory footprint.
    !/    10-feb-2014 : bug correction for us3d: div. by df ( version 4.18 )
    !/    30-apr-2014 : add th2m and sth2m calculation      ( version 5.01 )
    !/    27-may-2014 : switch to ompg switch.              ( version 5.02 )
    !/    10-apr-2015 : remove unused variables             ( version 5.08 )
    !/    10-jan-2017 : separate stokes drift calculation   ( version 6.01 )
    !/    01-mar-2018 : removed rtd code (now used in post  ( version 6.02 )
    !/                  processing code)
    !/    22-aug-2018 : add wbt parameter                   ( version 6.06 )
    !/    25-sep-2019 : corrected th2m and sth2m            ( version 6.07 )
    !/                  calculations. (j dykes, nrl)
    !/
    !  1. purpose :
    !
    !     fill necessary arrays with gridded data for output.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   i   input spectra. left in par list to change
    !                          shape.
    !       flpart  log.   i   flag for filling fields with part. data.
    !       floutg  log.   i   flag for file field output
    !       floutg2 log.   i   flag for coupling field output
    !     ----------------------------------------------------------------
    !
    !     locally saved parameters
    !     ----------------------------------------------------------------
    !       hsmin   real  filter level in hs for calculation of mean
    !                     wave parameters.
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
    !     none.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/ompg  openmp compiler directive for loop splitting.
    !
    !     !/o8    filter for low wave heights ( hsmin )
    !     !/o9    negative wave height alowed, other mean parameters will
    !             not be correct.
    !
    !     !/st0   no source terms.
    !     !/st1   source term set 1 (wam equiv.)
    !     !/st2   source term set 2 (tolman and chalikov)
    !     !/st3   source term set 3 (wam 4+)
    !     !/st6   source term set 6 (bydrz)
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd
    use w3wdatmd, only: ust, fpis
    use w3adatmd, only: cg, wn, dw
    use w3adatmd, only: hs, wlm, t02, t0m1, t01, fp0,               &
         thm, ths, thp0
    use w3adatmd, only: aba, abd, uba, ubd, fcut, sxx,              &
         syy, sxy, phs, ptp, plp, pdir, psi, pws,    &
         pwst, pnr, usero, tusx, tusy, prms, tpms,   &
         ussx, ussy, mssx, mssy, mssd, mscx, mscy,   &
         mscd, charn,                                &
         bhd, cge, p2sms, us3d, ef, th1m, sth1m,     &
         th2m, sth2m, hsig, stmaxe, stmaxd,          &
         hcmaxe, hmaxe, hcmaxd, hmaxd, ussp, qp, pqp,&
         pthp0, ppe, pgw, psw, ptm1, pt1, pt2, pep,  &
         wbt, qkk
    use w3odatmd, only: ndst, undef, iaproc, naproc, napfld,        &
         icprt, dtprt, wscut, noswll, flogrd, flogr2,&
         nogrp, ngrpp
    use w3adatmd, only: nsealm
    !
    use w3parall, only : init_get_isea
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk,0:nseal)
    logical, intent(in)     :: flpart, floutg, floutg2
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, jsea, isea, ix, iy,         &
         ikp0(nseal), nkh(nseal),             &
         i, j, lkms, hkms, itl
    real                    :: fxpmc, factor, factor2, eband, fkd,  &
         aabs, uabs,                          &
         xl, xh, xl2, xh2, el, eh, denom, kd, &
         m1, m2, ma, mb, mc, stex, stey, sted
    real                    :: et(nseal), ewn(nseal), etr(nseal),   &
         etx(nseal), ety(nseal), ab(nseal),   &
         etxx(nseal), etyy(nseal), etxy(nseal),&
         abx(nseal), aby(nseal),et02(nseal),  &
         ebd(nk,nseal), ec(nseal),            &
         abr(nseal), ubr(nseal), ubs(nseal),  &
         abx2(nseal), aby2(nseal),            &
         ab2x(nseal), ab2y(nseal),            &
         abst(nseal), abxx(nseal),            &
         abyy(nseal), abxy(nseal),            &
         abyx(nseal), eet1(nseal),            &
         etuscx(nseal), etuscy(nseal),        &
         etmssl(nseal), etmsscl(nseal),       &
         ettpmm(nseal), etf(nseal),           &
         et1(nseal), abx2m(nseal),            &
         aby2m(nseal), abxm(nseal),           &
         abym(nseal), abxym(nseal),           &
         mssxm(nseal), mssym(nseal),          &
         mssxtm(nseal), mssytm(nseal),        &
         mssxym(nseal), thmp(nseal),          &
         t02p(nseal), nv(nseal), ns(nseal),   &
         nb(nseal), mode(nseal),              &
         mu(nseal), ni(nseal), stmaxel(nseal),&
         phi(21,nseal),phist(nseal),         &
         ebc(nk,nseal), abp(nseal),           &
         stmaxdl(nseal), tlphi(nseal),        &
         wl02x(nseal), wl02y(nseal),          &
         alpxt(nseal), alpyt(nseal),          &
         alpxy(nseal), screst(nseal),         &
         qk1(nseal), qk2(nseal)
    real                       ussco, ft1
    real, save              :: hsmin = 0.01
    logical                 :: floloc(nogrp,ngrpp)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    do i=1,nogrp
      do j=1,ngrpp
        floloc(i,j) =   &
             ((floutg.and.flogrd(i,j)).or.(floutg2.and.flogr2(i,j)))
      end do
    end do
    !
    fxpmc  = 0.66 * grav / 28.
    hsmin  = hsmin
    ft1    =  0.3333 * sig(nk)**2 * dth * sig(nk)
    !
    ! 1.  initialize storage arrays -------------------------------------- *
    !
    et     = 0.
    et02   = 0.
    ewn    = 0.
    etr    = 0.
    et1    = 0.
    eet1   = 0.
    etx    = 0.
    ety    = 0.
    etxx   = 0.
    etyy   = 0.
    etxy   = 0.
    abr    = 0.
    aba    = 0.
    abd    = 0.
    ubr    = 0.
    uba    = 0.
    ubd    = 0.
    ubs    = 0.
    sxx    = 0.
    syy    = 0.
    sxy    = 0.
    ussx   = 0.
    ussy   = 0.
    tusx   = 0.
    tusy   = 0.
    mssx   = 0.
    mssy   = 0.
    mssd   = 0.
    mscx   = 0.
    mscy   = 0.
    mscd   = 0.
    prms   = 0.
    tpms   = 0.
    etuscy = 0.
    etuscy = 0.
    etmssl = 0.
    etmsscl= 0.
    ettpmm = 0.
    ebd    = 0.
    ec     = 0.
    etf    = 0.
    ebc    = 0.
    bhd = 0.
    mssxm = 0.
    mssym = 0.
    mssxtm = 0.
    mssytm = 0.
    mssxym = 0.
    phi    = 0.
    phist  = 0.
    tlphi  = 0.
    stmaxel = 0.
    stmaxdl = 0.
    qk2 = 0.
    !
    hs     = undef
    wlm    = undef
    t0m1   = undef
    t01    = undef
    t02    = undef
    fp0    = undef
    thm    = undef
    ths    = undef
    thp0   = undef
    hsig   = undef
    wl02x  = undef
    wl02y  = undef
    alpxy  = undef
    alpxt  = undef
    alpyt  = undef
    qkk    = undef
    thmp = undef
    t02p = undef
    screst = undef
    nv = undef
    ns = undef
    nb = undef
    mu = undef
    ni = undef
    mode = undef
    stmaxe = undef
    stmaxd = undef
    hcmaxe = undef
    hmaxe = undef
    hcmaxd = undef
    hmaxd = undef
    qp    = undef
    wbt    = undef
    !
    ! 2.  integral over discrete part of spectrum ------------------------ *
    !
    do ik=1, nk
      !
      ! 2.a initialize energy in band
      !
      ab     = 0.
      abx    = 0.
      aby    = 0.
      abx2   = 0.
      aby2   = 0.
      ab2x   = 0.
      ab2y   = 0.
      abxx   = 0.
      abyy   = 0.
      abxy   = 0.
      abyx   = 0.
      abst   = 0.
      qk1    = 0.
      !
      ! 2.b integrate energy in band
      !
      do ith=1, nth
        !
        !
        do jsea=1, nseal
          nkh(jsea)  = min ( nk ,   &
               int(facti2+facti1*log(max(1.e-7,fcut(jsea)))) )
          ab (jsea)  = ab (jsea) + a(ith,ik,jsea)
          abx(jsea)  = abx(jsea) + a(ith,ik,jsea)*ecos(ith)
          aby(jsea)  = aby(jsea) + a(ith,ik,jsea)*esin(ith)
          ! these are the integrals with cos^2 and sin^2
          abx2(jsea) = abx2(jsea) + a(ith,ik,jsea)*ec2(ith)
          aby2(jsea) = aby2(jsea) + a(ith,ik,jsea)*es2(ith)
          ! using trig identities to represent cos2theta and sin2theta.
          ab2x(jsea) = ab2x(jsea) + a(ith,ik,jsea)*(2*ec2(ith) - 1)
          ab2y(jsea) = ab2y(jsea) + a(ith,ik,jsea)*(2*esc(ith))
          abyx(jsea) = abyx(jsea) + a(ith,ik,jsea)*esc(ith)
          if (ith.le.nth/2) then
            abst(jsea) = abst(jsea) +                               &
                 a(ith,ik,jsea)*a(ith+nth/2,ik,jsea)
            qk1 (jsea) = qk1(jsea) + (a(ith,ik,jsea)+a(ith+nth/2,ik,jsea))**2
          end if
          call init_get_isea(isea, jsea)
          factor     = max ( 0.5 , cg(ik,isea)/sig(ik)*wn(ik,isea) )
          abxx(jsea) = abxx(jsea) + ((1.+ec2(ith))*factor-0.5) *    &
               a(ith,ik,jsea)
          abyy(jsea) = abyy(jsea) + ((1.+es2(ith))*factor-0.5) *    &
               a(ith,ik,jsea)
          abxy(jsea) = abxy(jsea) + esc(ith)*factor * a(ith,ik,jsea)
        end do
        !
        !
      end do
      !
      ! 2.c finalize integration over band and update mean arrays
      !
      !
      !
      do jsea=1, nseal
        call init_get_isea(isea, jsea)
        factor       = dden(ik) / cg(ik,isea)
        ebd(ik,jsea) = ab(jsea) * factor            ! this is e(f)*df
        et (jsea)    = et (jsea) + ebd(ik,jsea)
        etf(jsea)  = etf(jsea) + ebd(ik,jsea) * cg(ik,isea)
        ewn(jsea)  = ewn(jsea) + ebd(ik,jsea) / wn(ik,isea)
        etr(jsea)  = etr(jsea) + ebd(ik,jsea) / sig(ik)
        et1(jsea)  = et1(jsea) + ebd(ik,jsea) * sig(ik)
        !          eet1(jsea) = eet1(jsea)+ ebd(ik,jsea)**2 * sig(ik)
        eet1(jsea) = eet1(jsea)+ ebd(ik,jsea)**2 * sig(ik)/dsii(ik)
        et02(jsea) = et02(jsea)+ ebd(ik,jsea) * sig(ik)**2
        etx(jsea)  = etx(jsea) + abx(jsea) * factor
        ety(jsea)  = ety(jsea) + aby(jsea) * factor
        tusx(jsea) = tusx(jsea) + abx(jsea)*factor               &
             *grav*wn(ik,isea)/sig(ik)
        tusy(jsea)  = tusy(jsea) + aby(jsea)*factor               &
             *grav*wn(ik,isea)/sig(ik)
        etxx(jsea) = etxx(jsea) + abx2(jsea) * factor* wn(ik,isea)**2
        ! nb:     qk1 (jsea) = qk1(jsea) + a(ith,ik,jsea)**2
        qk2 (jsea) = qk2 (jsea) + qk1(jsea)  * factor* sig(ik) /wn(ik,isea)
        etyy(jsea) = etyy(jsea) + aby2(jsea) * factor* wn(ik,isea)**2
        etxy(jsea) = etxy(jsea) + abyx(jsea) * factor* wn(ik,isea)**2
        if (sig(ik)*0.5*(1+xfr).lt.0.4*tpi) then
          etmssl(jsea)  = etmssl(jsea) + ab(jsea)*factor           &
               *wn(ik,isea)**2
        else
          if (sig(max(ik-1,1))*0.5*(1+xfr).lt.0.4*tpi) then
            etmssl(jsea)  = etmssl(jsea) + ab(jsea)*factor         &
                 *(sig(ik)*0.5*(1+1/xfr)-(0.4*tpi))/dsii(ik)     &
                 *wn(ik,isea)**2
            factor2       = sig(ik)**5/(grav**2)/dsii(ik)
            etmsscl(jsea) = ab(jsea)*factor*factor2
          end if
        end if
        !
        ubs(jsea) = ubs(jsea) + ab(jsea) * sig(ik)**2
        !
        !   2nd order equivalent surface pressure spectral density at k=0
        !   this is used for microseismic or microbarom sources
        !   finite water depth corrections (ardhuin & herbers 2013) are not
        !   included here.
        !
        factor2 = dth*2/(tpi**2)                        &
             * sig(ik)                             &
             * (tpi*sig(ik)/cg(ik,isea))**2        &  ! jacobian^2 to get e(f,th) from a(k,th)
             * abst(jsea)
        !
        !   integration over seismic radian frequency : *2*dsigma
        !
        prms(jsea)  = prms(jsea) + factor2 * 2 * dsii(ik)
        if ( floloc (6, 9).and.(ik.ge.p2msf(2).and.ik.le.p2msf(3)))   &
             p2sms(jsea,ik) = factor2 * 2 * tpi
        if (factor2 .gt. ettpmm(jsea)) then
          ettpmm(jsea) = factor2
          tpms(jsea) = tpi/sig(ik)
        end if
        !
        ! directional moments in the last freq. band
        !
        if (ik.eq.nk) then
          factor2       = sig(ik)**5/(grav**2)/dsii(ik)
          etuscx(jsea)  = abx(jsea)*factor*factor2
          etuscy(jsea)  = aby(jsea)*factor*factor2
          !
          !     nb: the slope pdf is proportional to ell1=etyy*ec2-2*etxy*ecs+etyy*es2 = a*ec2-2*b*ecs+c*es2
          !     this is an ellipse equation with axis direction given by dir=0.5*atan2(-2.*etxy,etyy-etxx)
          !
          ma  = abx2(jsea) * factor * factor2
          mc  = aby2(jsea) * factor * factor2
          mb  = abyx(jsea) * factor * factor2
          !
          ! old definitions:  mscx(jsea)  = abx2(jsea) * factor * factor2
          !                   mscy(jsea)  = aby2(jsea) * factor * factor2
          mscd(jsea)=0.5*atan2(2*mb,ma-mc)
          mscx(jsea)= ma*cos(mscd(jsea))**2   &
               +2*mb*sin(mscd(jsea))*cos(mscd(jsea))+ma*sin(mscd(jsea))**2
          mscy(jsea)= mc*cos(mscd(jsea))**2   &
               -2*mb*sin(mscd(jsea))*cos(mscd(jsea))+ma*sin(mscd(jsea))**2
        end if
        !
        ! deep water limits
        !
        kd    = max ( 0.001 , wn(ik,isea) * dw(isea) )
        if ( kd .lt. 6. ) then
          fkd       = factor / sinh(kd)**2
          abr(jsea) = abr(jsea) + ab(jsea) * fkd
          aba(jsea) = aba(jsea) + abx(jsea) * fkd
          abd(jsea) = abd(jsea) + aby(jsea) * fkd
          ubr(jsea) = ubr(jsea) + ab(jsea) * sig(ik)**2 * fkd
          uba(jsea) = uba(jsea) + abx(jsea) * sig(ik)**2 * fkd
          ubd(jsea) = ubd(jsea) + aby(jsea) * sig(ik)**2 * fkd
          ussco=fkd*sig(ik)*wn(ik,isea)*cosh(2.*kd)
          bhd(jsea) = bhd(jsea) +                             &
               grav*wn(ik,isea) * ebd(ik,jsea) / (sinh(2.*kd))
        else
          ussco=factor*sig(ik)*2.*wn(ik,isea)
        end if
        !
        abxx(jsea)   = max ( 0. , abxx(jsea) ) * factor
        abyy(jsea)   = max ( 0. , abyy(jsea) ) * factor
        abxy(jsea)   = abxy(jsea) * factor
        sxx(jsea)    = sxx(jsea)  + abxx(jsea)
        syy(jsea)    = syy(jsea)  + abyy(jsea)
        sxy(jsea)    = sxy(jsea)  + abxy(jsea)
        ebd(ik,jsea) = ebd(ik,jsea) / dsii(ik)
        !
        if ( floloc( 3, 1).and.(ik.ge.e3df(2,1).and.ik.le.e3df(3,1)))   &
             ef(jsea,ik)  = ebd(ik,jsea) * tpi
        !
        ussx(jsea)  = ussx(jsea) + abx(jsea)*ussco
        ussy(jsea)  = ussy(jsea) + aby(jsea)*ussco
        !
        ! fills the 3d stokes drift spectrum array
        !  ! the us3d stokes drift specrum array is now calculated in a
        !  subroutine and called at the end of this subroutine
        !          if ( floloc( 6, 8).and.(ik.ge.us3df(2).and.ik.le.us3df(3) ))   then
        !            us3d(jsea,ik)    =  abx(jsea)*ussco/(dsii(ik)*tpiinv)
        !            us3d(jsea,nk+ik) =  aby(jsea)*ussco/(dsii(ik)*tpiinv)
        !          end if
        if ( floloc( 3, 2).and.(ik.ge.e3df(2,2).and.ik.le.e3df(3,2)))  &
             th1m(jsea,ik)= mod ( 630. - rade*atan2(aby(jsea),abx(jsea)) , 360. )
        m1 = sqrt(abx(jsea)**2+aby(jsea)**2)/max(1e-20,ab(jsea))
        if ( floloc( 3, 3).and.(ik.ge.e3df(2,3).and.ik.le.e3df(3,3)))  &
             sth1m(jsea,ik)= sqrt(abs(2.*(1-m1)))*rade
        if ( floloc( 3, 4).and.(ik.ge.e3df(2,4).and.ik.le.e3df(3,4)))  &
             th2m(jsea,ik)= mod ( 270. - rade*0.5*atan2(aby2(jsea),ab2x(jsea)) , 180. )
        m2 = sqrt(ab2x(jsea)**2+ab2y(jsea)**2)/max(1e-20,ab(jsea))
        if ( floloc( 3, 5).and.(ik.ge.e3df(2,5).and.ik.le.e3df(3,5)))  &
             sth2m(jsea,ik)= sqrt(abs(0.5*(1-m2)))*rade
      end do
      !
      !
    end do
    !
    ! start of space-time extremes section
    if ( ( stexu .gt. 0. .and. steyu .gt. 0. ) &
         .or. ( stedu .gt. 0. ) ) then
      !  space-time extremes
      !    (for references:
      !     - krogstad et al, omae 2004
      !     - baxevani and rychlik, oe 2006
      !     - adler and taylor, 2007
      !     - fedele, jpo 2012
      !     - fedele et al, om 2013
      !     - benetazzo et al, jpo 2015)
      !
      !  compute spectral parameters wrt the mean wave direction
      !  (no tail contribution - prognostic)
      do jsea=1, nseal
        call init_get_isea(isea, jsea)
        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        if ( mapsta(iy,ix) .gt. 0 ) then
          if ( abs(etx(jsea))+abs(ety(jsea)) .gt. 1.e-7 ) then
            thmp(jsea) = atan2(ety(jsea),etx(jsea))
          end if
        end if
      end do
      !
      do ik=1, nk
        !
        abx2m = 0.
        aby2m = 0.
        abxm = 0.
        abym = 0.
        abxym = 0.
        !
        do ith=1, nth
          !
          !
          do jsea=1, nseal
            abx2m(jsea) = abx2m(jsea) + a(ith,ik,jsea)*                &
                 (ecos(ith)*cos(thmp(jsea))+esin(ith)*sin(thmp(jsea)))**2
            aby2m(jsea) = aby2m(jsea) + a(ith,ik,jsea)*                &
                 (esin(ith)*cos(thmp(jsea))-ecos(ith)*sin(thmp(jsea)))**2
            abxm(jsea)  = abxm(jsea) + a(ith,ik,jsea)*                 &
                 (ecos(ith)*cos(thmp(jsea))+esin(ith)*sin(thmp(jsea)))
            abym(jsea)  = abym(jsea) + a(ith,ik,jsea)*                 &
                 (esin(ith)*cos(thmp(jsea))-ecos(ith)*sin(thmp(jsea)))
            abxym(jsea) = abxym(jsea) + a(ith,ik,jsea)*                &
                 (ecos(ith)*cos(thmp(jsea))+esin(ith)*sin(thmp(jsea)))*   &
                 (esin(ith)*cos(thmp(jsea))-ecos(ith)*sin(thmp(jsea)))
          end do
          !
          !
        end do
        !
        !
        do jsea=1, nseal
          call init_get_isea(isea, jsea)
          factor       = dden(ik) / cg(ik,isea)
          mssxm(jsea)  = mssxm(jsea) + abx2m(jsea)*factor*             &
               wn(ik,isea)**2
          mssym(jsea)  = mssym(jsea) + aby2m(jsea)*factor*             &
               wn(ik,isea)**2
          mssxtm(jsea)  = mssxtm(jsea) + abxm(jsea)*factor*wn(ik,isea)* &
               sig(ik)
          mssytm(jsea)  = mssytm(jsea) + abym(jsea)*factor*wn(ik,isea)* &
               sig(ik)
          mssxym(jsea)  = mssxym(jsea) + abxym(jsea)*factor*           &
               wn(ik,isea)**2
        end do
        !
        !
      end do
      !
      do jsea=1, nseal
        !
        !  mean wave period (no tail contribution - prognostic)
        if ( et02(jsea) .gt. 1.e-7 ) then
          t02p(jsea) = tpi * sqrt(et(jsea) / et02(jsea) )
        end if
        !
        !  mean wavelength and mean crest length (02) for space-time extremes
        if ( mssxm(jsea) .gt. 1.e-7 ) then
          wl02x(jsea) = tpi * sqrt(et(jsea) / mssxm(jsea))
        end if
        if ( mssym(jsea) .gt. 1.e-7 ) then
          wl02y(jsea) = tpi * sqrt(et(jsea) / mssym(jsea))
        end if
        !
        !  irregularity parameters for space-time extremes
        if ((mssxm(jsea) .gt. 1.e-7) .and. (et02(jsea) .gt. 1.e-7)) then
          alpxt(jsea) = mssxtm(jsea) / (sqrt(mssxm(jsea) * et02(jsea)))
        endif
        if ((mssym(jsea) .gt. 1.e-7) .and. (et02(jsea) .gt. 1.e-7)) then
          alpyt(jsea) = mssytm(jsea) / (sqrt(mssym(jsea) * et02(jsea)))
        endif
        if ((mssxm(jsea) .gt. 1.e-7) .and. (mssym(jsea) .gt. 1.e-7)) then
          alpxy(jsea) = mssxym(jsea) / (sqrt(mssxm(jsea) * mssym(jsea)))
        endif
        !
        !  short-crestedness parameter
        if (mssxm(jsea) .gt. 1.e-7)  then
          screst(jsea) = sqrt(mssym(jsea)/mssxm(jsea))
        end if
        !
        !  space domain size (user-defined or default)
        if ( stexu .gt. 0 .and. steyu .gt. 0 ) then
          stex = stexu
          stey = steyu
        else
          stex = 0.
          stey = 0.
        end if
        !
        !  time domain size (user-defined or default)
        if ( stedu .gt. 0 ) then
          sted = stedu
        else
          sted = 0.
        end if
        !
        !  average numbers of waves in the space-time domain (volume+sides+borders)
        if ((wl02x(jsea) .gt. 1.e-7) .and. (wl02y(jsea) .gt. 1.e-7)    &
             .and. (t02p(jsea) .gt. 1.e-7)) then
          nv(jsea) = tpi*(stex*stey*sted)/                             &
               (wl02x(jsea)*wl02y(jsea)*t02p(jsea))  *                    &
               sqrt(1-alpxt(jsea)**2-alpyt(jsea)**2  -                    &
               alpxy(jsea)**2+2*alpxt(jsea)*alpyt(jsea)*alpxy(jsea))
          ns(jsea) = sqrt(tpi)*((stex*sted)/(wl02x(jsea)*t02p(jsea)) * &
               sqrt(1-alpxt(jsea)**2) +                                   &
               (stey*sted)/(wl02y(jsea)*t02p(jsea)) *                     &
               sqrt(1-alpyt(jsea)**2) +                                   &
               (stex*stey)/(wl02x(jsea)*wl02y(jsea)) *                    &
               sqrt(1-alpxy(jsea)**2))
          nb(jsea) = stex/wl02x(jsea) + stey/wl02y(jsea) +             &
               sted/t02p(jsea)
        end if
        !
        ! integral measure of wave steepness (fedele & tayfun, 2009) mu, as a
        ! function of the spectral width parameter ni (longuet-higgins, 1985)
        if (et1(jsea) .gt. 1.e-7) then
          ni(jsea) = sqrt(et(jsea)*et02(jsea)/et1(jsea)**2 - 1)
        endif
        if (et(jsea) .gt. 1.e-7) then
          mu(jsea) = et1(jsea)**2/grav * (et(jsea))**(-1.5) *          &
               (1-ni(jsea)+ni(jsea)**2)
        endif
        !
        ! mode of the adler&taylor distribution
        ! (normalized on the standard deviation = hs/4)
        ! time extremes
        if ((stex .eq. 0) .and. (stey .eq. 0)) then
          mode(jsea) = sqrt(2.*log(nb(jsea)))
          ! space extremes (strictly for stex*stey >> wl02x*wl02y)
        elseif (sted .eq. 0) then
          mode(jsea) = sqrt(2.*log(ns(jsea))+log(2.*log(ns(jsea))+     &
               log(2.*log(ns(jsea)))))
          ! space-time extremes (strictly for stex*stey >> wl02x*wl02y)
        elseif ((wl02x(jsea) .gt. 1.e-7) .and. (wl02y(jsea) .gt. 1.e-7) &
             .and. (t02p(jsea) .gt. 1.e-7)) then
          mode(jsea) = sqrt(2.*log(nv(jsea))+2.*log(2.*log(nv(jsea))+  &
               2.*log(2.*log(nv(jsea)))))
        endif
        !
        ! expected maximum sea surface elevation in the st domain - nonlinear
        ! (in meters, hs/4=sqrt(et(jsea)))
        stmaxe(jsea) = sqrt(et(jsea)) *                                &
             ( mode(jsea)+0.5*mu(jsea)*mode(jsea)**2 +                  &
             0.5772*(1+mu(jsea)*mode(jsea)) /                           &
             (mode(jsea)-(2*nv(jsea)*mode(jsea)+ns(jsea)) /             &
             (nv(jsea)*mode(jsea)**2+ns(jsea)*mode(jsea)+nb(jsea))) )
        !
        ! standard deviation of the maximum sea surface elevation in st domain
        !  - nonlinear (in meters, hs/4=sqrt(et(jsea)))
        stmaxd(jsea) =  sqrt(et(jsea)) *                               &
             ( pi*(1+mu(jsea)*mode(jsea))/sqrt(6.) /                    &
             (mode(jsea)-(2*nv(jsea)*mode(jsea)+ns(jsea)) /             &
             (nv(jsea)*mode(jsea)**2+ns(jsea)*mode(jsea)+nb(jsea))) )
        !
        ! autocovariance (time) function (normalized on the maximum, i.e. total
        ! variance)
        if (t02p(jsea) .gt. 1.e-7) then
          tlphi(jsea) = 0.3*t02p(jsea)
          do itl = 1, 21
            do ik = 1, nk-3, 4
              phi(itl,jsea) = phi(itl,jsea) +                          &
                   (xfr**3*ebd(ik+3,jsea)*cos(xfr**3*sig(ik)*tlphi(jsea))+    &
                   xfr**2*ebd(ik+2,jsea)*cos(xfr**2*sig(ik)*tlphi(jsea))+     &
                   xfr*ebd(ik+1,jsea)*cos(xfr*sig(ik)*tlphi(jsea)) +          &
                   ebd(ik,jsea)*cos(sig(ik)*tlphi(jsea)))*dsii(ik)
            enddo
            tlphi(jsea) = tlphi(jsea) + t02p(jsea)/20.
          enddo
          phi(:,jsea) = phi(:,jsea)/et(jsea)
          !
          ! first minimum of the autocovariance function (absolute value)
          phist(jsea) = abs(minval(phi(:,jsea),1))
        endif
        !
        ! wave height of the wave with the maximum expected crest height
        ! and corresponding standard deviation
        ! (according to boccotti quasi-determinism theory - linear)
        stmaxel(jsea) = sqrt(et(jsea)) * ( mode(jsea)+0.5772 /         &
             (mode(jsea)-(2*nv(jsea)*mode(jsea)+ns(jsea)) /             &
             (nv(jsea)*mode(jsea)**2+ns(jsea)*mode(jsea)+nb(jsea))) )
        stmaxdl(jsea) = sqrt(et(jsea)) *                               &
             ( pi/sqrt(6.) /                                            &
             (mode(jsea)-(2*nv(jsea)*mode(jsea)+ns(jsea)) /             &
             (nv(jsea)*mode(jsea)**2+ns(jsea)*mode(jsea)+nb(jsea))) )
        hcmaxe(jsea) = stmaxel(jsea)*(1+phist(jsea))
        hcmaxd(jsea) = stmaxdl(jsea)*(1+phist(jsea))
        ! maximum expected wave height and corresponding standard deviation
        ! (according to boccotti quasi-determinism theory - linear)
        hmaxe(jsea) = stmaxel(jsea)*sqrt(2*(1+phist(jsea)))
        hmaxd(jsea) = stmaxdl(jsea)*sqrt(2*(1+phist(jsea)))
      enddo
      !
      !
      ! end of space-time extremes section
    endif
    !
    ! 3.  finalize computation of mean parameters ------------------------ *
    !
    !
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      !
      ! 3.a directional mss parameters
      !     nb: the slope pdf is proportional to ell1=etyy*ec2-2*etxy*ecs+etxx*es2 = c*ec2-2*b*ecs+a*es2
      !     this is an ellipse equation with axis direction given by dir=0.5*atan2(2.*etxy,etxx-etyy)
      !     from matlab script: t0=0.5*(atan2(2.*b,a-c));
      !     from matlab script: a2=a.*cos(t0).^2+2.*b.*sin(t0).*cos(t0)+a.*cos(t0).^2+c.*sin(t0)^2;
      !     from matlab script: c2=c.*cos(t0)^2-2.*b.*sin(t0).*cos(t0)+a.*sin(t0).^2;
      mssd(jsea)=0.5*(atan2(2*etxy(jsea),etxx(jsea)-etyy(jsea)))
      mssx(jsea)  = etxx(jsea)*cos(mssd(jsea))**2   &
           +2*etxy(jsea)*sin(mssd(jsea))*cos(mssd(jsea))+etyy(jsea)*sin(mssd(jsea))**2
      mssy(jsea)  = etyy(jsea)*cos(mssd(jsea))**2   &
           -2*etxy(jsea)*sin(mssd(jsea))*cos(mssd(jsea))+etxx(jsea)*sin(mssd(jsea))**2
      !
      ! 3.b add tail
      !     ( dth * sig absorbed in ftxx )
      eband     = ab(jsea) / cg(nk,isea)           ! eband is e(sigma)/sigma for the last frequency band
      et (jsea) = et (jsea) + fte  * eband
      ewn(jsea) = ewn(jsea) + ftwl * eband
      etf(jsea) = etf(jsea) + grav * fttr * eband  ! this is the integral of cge in deep water
      etr(jsea) = etr(jsea) + fttr * eband
      et1(jsea) = et1(jsea) + ft1  * eband
      !        eet1(jsea)= eet1(jsea) + ft1  * eband**2 : this was not correct. actually tail may not be needed for qp.
      et02(jsea)= et02(jsea)+ eband* 0.5 * sig(nk)**4 * dth
      etx(jsea) = etx(jsea) + fte * abx(jsea) / cg(nk,isea)
      ety(jsea) = ety(jsea) + fte * aby(jsea) / cg(nk,isea)
      sxx(jsea) = sxx(jsea) + fte * abxx(jsea) / cg(nk,isea)
      syy(jsea) = syy(jsea) + fte * abyy(jsea) / cg(nk,isea)
      sxy(jsea) = sxy(jsea) + fte * abxy(jsea) / cg(nk,isea)
      !
      ! tail for surface stokes drift is commented out: very sensitive to tail power
      !
      !       ussx(jsea)  = ussx(jsea) + 2*grav*etuscx(jsea)/sig(nk)
      !       ussy(jsea)  = ussy(jsea) + 2*grav*etuscy(jsea)/sig(nk)
      ubs(jsea) = ubs(jsea) + ftwl * eband/grav
    end do
    !
    !
    sxx    = sxx * dwat * grav
    syy    = syy * dwat * grav
    sxy    = sxy * dwat * grav
    !
    !
    do jsea=1, nseal
      call init_get_isea(isea, jsea)
      ix     = mapsf(isea,1)
      iy     = mapsf(isea,2)
      if ( mapsta(iy,ix) .gt. 0 ) then
          hs (jsea) = 4. * sqrt ( et(jsea) )
        if ( et(jsea) .gt. 1.e-7 ) then
          qp(jsea) = ( 2. / et(jsea)**2 ) * eet1(jsea)
          wlm(jsea) = ewn(jsea) / et(jsea) * tpi
          t0m1(jsea) = etr(jsea) / et(jsea) * tpi
          ths(jsea) = rade * sqrt ( max ( 0. , 2. * ( 1. - sqrt ( &
               max(0.,(etx(jsea)**2+ety(jsea)**2)/et(jsea)**2) ) ) ) )
          if ( ths(jsea) .lt. 0.01*rade*dth ) ths(jsea) = 0.
          ! nb:           qk1 (jsea) = qk1(jsea) + a(ith,ik,jsea)**2
          !               qk2 (jsea) = qk2 (jsea) + qk1(jsea)  * factor* sig(ik) /wn(ik,isea)
          qkk (jsea) = sqrt(0.5*qk2 (jsea))/et(jsea)
        else
          wlm(jsea) = 0.
          t0m1(jsea) = tpi / sig(nk)
          ths(jsea) = 0.
        end if
        if ( abs(etx(jsea))+abs(ety(jsea)) .gt. 1.e-7 ) then
          thm(jsea) = atan2(ety(jsea),etx(jsea))
        else
          thm(jsea) = 0.
        end if
        abr(jsea) = sqrt ( 2. * max ( 0. , abr(jsea) ) )
        if ( abr(jsea) .ge. 1.e-7 ) then
          abd(jsea) = atan2(abd(jsea),aba(jsea))
        else
          abd(jsea) = 0.
        endif
        aba(jsea) = abr(jsea)
        ubr(jsea) = sqrt ( 2. * max ( 0. , ubr(jsea) ) )
        if ( ubr(jsea) .ge. 1.e-7 ) then
          ubd(jsea) = atan2(ubd(jsea),uba(jsea))
        else
          ubd(jsea) = 0.
        endif
        uba(jsea) = ubr(jsea)
        cge(jsea) = dwat*grav*etf(jsea)
        if ( et02(jsea) .gt. 1.e-7  .and.  et(jsea) .gt. 0 ) then
          t02(jsea) = tpi * sqrt(et(jsea) / et02(jsea) )
          t01(jsea) = tpi * et(jsea) / et1(jsea)
        else
          t02(jsea) = tpi / sig(nk)
          t01(jsea)= t02(jsea)
        endif
        !
        !  add here usero(jsea,1) ...
        !
      end if
    end do
    !
    !
    ! 3.b clean-up small values if !/o8 switch selected
    !
    !
    ! 4.  peak frequencies and directions -------------------------------- *
    ! 4.a initialize
    !
    !
    do jsea=1, nseal
      ec  (jsea) = ebd(nk,jsea)
      fp0 (jsea) = undef
      ikp0(jsea) = nk
      thp0(jsea) = undef
    end do
    !
    !
    ! 4.b discrete peak frequencies
    !
    do ik=nk-1, 1, -1
      !
      !
      do jsea=1, nseal
        if ( ec(jsea) .lt. ebd(ik,jsea) ) then
          ec  (jsea) = ebd(ik,jsea)
          ikp0(jsea) = ik
        end if
      end do
      !
      !
    end do
    !
    !
    do jsea=1, nseal
      if ( ikp0(jsea) .ne. nk ) fp0(jsea) = sig(ikp0(jsea)) * tpiinv
    end do
    !
    !
    ! 4.c continuous peak frequencies
    !
    xl     = 1./xfr - 1.
    xh     =  xfr - 1.
    xl2    = xl**2
    xh2    = xh**2
    !
    !
    do jsea=1, nseal
      if ( ikp0(jsea) .ne. nk ) then
        if ( ikp0(jsea) .eq. 1 ) then
          el = - ebd(ikp0(jsea), jsea)
        else
          el = ebd(ikp0(jsea)-1, jsea) - ebd(ikp0(jsea), jsea)
        end if
        eh = ebd(ikp0(jsea)+1, jsea) - ebd(ikp0(jsea), jsea)
        denom  = xl*eh - xh*el
        fp0(jsea) = fp0 (jsea) * ( 1. + 0.5 * ( xl2*eh - xh2*el )   &
             / sign ( max(abs(denom),1.e-15) , denom ) )
      end if
    end do
    !
    !
    ! 4.d peak directions
    !
    !
    do jsea=1, nseal
      etx(jsea) = 0.
      ety(jsea) = 0.
    end do
    !
    !
    do ith=1, nth
      !
      !
      do jsea=1, nseal
        if ( ikp0(jsea) .ne. nk ) then
          etx(jsea) = etx(jsea) + a(ith,ikp0(jsea),jsea)*ecos(ith)
          ety(jsea) = ety(jsea) + a(ith,ikp0(jsea),jsea)*esin(ith)
        end if
      end do
      !
      !
    end do
    !
    !
    do jsea=1, nseal
      if ( abs(etx(jsea))+abs(ety(jsea)) .gt. 1.e-7 .and.           &
           fp0(jsea).ne.undef )                                     &
           thp0(jsea) = atan2(ety(jsea),etx(jsea))
      etx(jsea) = 0.
      ety(jsea) = 0.
    end do
    !
    !
    do jsea =1, nseal
      call init_get_isea(isea, jsea)
      ix          = mapsf(isea,1)
      iy          = mapsf(isea,2)
      if ( mapsta(iy,ix) .le. 0 ) then
        fp0 (jsea) = undef
        thp0(jsea) = undef
      end if
    end do
    !
    !
    ! 5.  test output (local to mpp only)
    !
    !
    ! 6.  fill arrays wth partitioned data
    !
    if ( flpart ) then
      !
      ! 6.a initializations
      !
      phs    = undef
      ptp    = undef
      plp    = undef
      pdir   = undef
      psi    = undef
      pws    = undef
      pwst   = undef
      pnr    = undef
      pthp0  = undef
      pqp    = undef
      ppe    = undef
      pgw    = undef
      psw    = undef
      ptm1   = undef
      pt1    = undef
      pt2    = undef
      pep    = undef
      !
      ! 6.b loop over local sea points
      !
      !
      do jsea=1, nseal
        call init_get_isea(isea, jsea)
        ix          = mapsf(isea,1)
        iy          = mapsf(isea,2)
        !
        if ( mapsta(iy,ix).gt.0 ) then
          i         = icprt(jsea,2)
          pnr(jsea) = max ( 0. , real(icprt(jsea,1)-1) )
          if ( icprt(jsea,1).ge.1 ) pwst(jsea) = dtprt(6,i)
        end if
        !
        if ( mapsta(iy,ix).gt.0 .and. icprt(jsea,1).gt.1 ) then
          i      = icprt(jsea,2) + 1
          if ( dtprt(6,i) .ge. wscut ) then
            phs(jsea,0) = dtprt(1,i)
            ptp(jsea,0) = dtprt(2,i)
            plp(jsea,0) = dtprt(3,i)
            ! (pdir is already in degrees nautical - convert back to
            !  cartesian in radians to maintain internal convention)
            if(dtprt(4,i) .ne. undef) then
              pdir(jsea,0) = (270. - dtprt(4,i)) * dera
            endif
            psi(jsea,0) = dtprt(5,i)
            pws(jsea,0) = dtprt(6,i)
            ! (pthp0 is already in degrees nautical - convert back to
            !  cartesian in radians to maintain internal convention)
            if(dtprt(7,i) .ne. undef) then
              pthp0(jsea,0) = (270. - dtprt(7,i)) * dera
            endif
            psw(jsea,0) = dtprt(8,i)
            ppe(jsea,0) = dtprt(9,i)
            pqp(jsea,0) = dtprt(10,i)
            pgw(jsea,0) = dtprt(11,i)
            ptm1(jsea,0) = dtprt(12,i)
            pt1(jsea,0) = dtprt(13,i)
            pt2(jsea,0) = dtprt(14,i)
            pep(jsea,0) = dtprt(15,i)
            i      = i + 1
          end if
          do j=1, noswll
            if ( i .gt.  icprt(jsea,2)+icprt(jsea,1)-1 ) exit
            phs(jsea,j) = dtprt(1,i)
            ptp(jsea,j) = dtprt(2,i)
            plp(jsea,j) = dtprt(3,i)
            ! (pdir is already in degrees nautical - convert back to
            !  cartesian in radians to maintain internal convention)
            if(dtprt(4,i) .ne. undef) then
              pdir(jsea,j) = (270. - dtprt(4,i)) * dera
            endif
            psi(jsea,j) = dtprt(5,i)
            pws(jsea,j) = dtprt(6,i)
            ! (pthp0 is already in degrees nautical - convert back to
            !  cartesian in radians to maintain internal convention)
            if(dtprt(7,i) .ne. undef) then
              pthp0(jsea,j) = (270. - dtprt(7,i)) * dera
            endif
            psw(jsea,j) = dtprt(8,i)
            ppe(jsea,j) = dtprt(9,i)
            pqp(jsea,j) = dtprt(10,i)
            pgw(jsea,j) = dtprt(11,i)
            ptm1(jsea,j) = dtprt(12,i)
            pt1(jsea,j) = dtprt(13,i)
            pt2(jsea,j) = dtprt(14,i)
            pep(jsea,j) = dtprt(15,i)
            i      = i + 1
          end do
        end if
        !
      end do
      !
      !
    end if
    if (floloc( 6, 8)) then
      call calc_u3stokes(a,1)
    end if
    !
    if (floloc( 6, 12)) then
      call calc_u3stokes(a,2)
    endif
    !
    if (floloc( 8, 7).or.floloc( 8, 8).or.floloc( 8, 9)) then
      call skewness(a)
    end if
    
    !
    ! dominant wave breaking probability
    !
    if (floloc(2, 17)) call calc_wbt(a)
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3outg ----------------------------------------------------- /
    !/
  end subroutine w3outg
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief  read/write gridded output.
  !>
  !> @details fields in file are determined by flags in flogrd in w3odatmd.
  !>
  !> @param[inout] inxout  test string for read/write.
  !> @param[inout] ndsog   file unit number.
  !> @param[inout] iotst   test indictor for reading.
  !> @param[inout] imod    model number for w3gdat etc.
  !>
  !> @author h. l. tolman  @date 22-mar-2021
  !>
  subroutine w3iogo ( inxout, ndsog, iotst, imod &
          )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    17-mar-1999 : distributed fortran 77 version.     ( version 1.18 )
    !/    04-jan-2000 : upgrade to fortran 90               ( version 2.00 )
    !/                  major changes to logistics.
    !/    24-jan-2001 : flat grid version (formats only)    ( version 2.06 )
    !/    23-apr-2002 : clean up                            ( version 2.19 )
    !/    29-apr-2002 : add output types 17-18.             ( version 2.20 )
    !/    13-nov-2002 : add stress vector.                  ( version 3.00 )
    !/    25-oct-2004 : multiple grid version.              ( version 3.06 )
    !/    27-jun-2005 : adding mapst2.                      ( version 3.07 )
    !/    21-jul-2005 : adding output fields 19-21.         ( version 3.07 )
    !/    27-jun-2006 : adding file name preamble.          ( version 3.09 )
    !/    05-jul-2006 : consolidate stress arrays.          ( version 3.09 )
    !/    02-apr-2007 : adding partitioned output.          ( version 3.11 )
    !/                  adding user slots for outputs.
    !/    30-oct-2009 : implement curvilinear grid type.    ( version 3.14 )
    !/                  (w. e. rogers & t. j. campbell, nrl)
    !/    31-oct-2010 : implement unstructured grids        ( version 3.14 )
    !/                  (a. roland and f. ardhuin)
    !/    05-feb-2011 : renumbering of output fields        ( version 3.14 )
    !/                  (f. ardhuin)
    !/    25-dec-2012 : new output structure and smaller    ( version 4.11 )
    !/                  memory footprint.
    !/    21-aug-2013 : added missing cos,sin for uba, aba  ( version 4.11 )
    !/    27-nov-2013 : management of coupling output       ( version 4.18 )
    !/    01-mar-2018 : removed rtd code (now used in post  ( version 6.02 )
    !/                  processing code)
    !/    25-aug-2018 : add wbt parameter                   ( version 6.06 )
    !/    22-mar-2021 : add extra coupling fields as output ( version 7.13 )
    !/    07-mar-2024 : add skewness parameters             ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     read/write gridded output.
    !
    !  2. method :
    !
    !     fields in file are determined by flags in flogrd in w3odatmd.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       inxout  c*(*)  i   test string for read/write, valid are:
    !                          'read' and 'write'.
    !       ndsog   int.   i   file unit number.
    !       iotst   int.   o   test indictor for reading.
    !                           0 : fields read.
    !                          -1 : past end of file.
    !       imod    int.   i   model number for w3gdat etc.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       see module documentation above.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3wave    subr. w3wavemd actual wave model routine.
    !      ww3_outf  prog.   n/a    ouput postprocessor.
    !      ww3_grib  prog.   n/a    ouput postprocessor.
    !      gx_outf   prog.   n/a    ouput postprocessor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       tests on inxout, file status and on array dimensions.
    !
    !  7. remarks :
    !
    !     - mapsta is dumped as it contains information on the ice edge.
    !       dynamic ice edges require mapsta to be dumped every time step.
    !     - the output file has the pre-defined name 'out_grd.filext'.
    !     - the current components cx and cy are written to out_grd as
    !       components, but converted to magnitude and direction in most
    !       gridded and point output post-processors (except gx_outf).
    !     - all written direction are in degrees, nautical convention,
    !       but in reading, all is convered back to radians and cartesian
    !       conventions.
    !     - before writing, wind and current directions are converted,
    !       wave directions are already in correct convention (see w3outg).
    !     - in mpp version of model data is supposed to be gatherd at the
    !       correct processor before the routine is called.
    !     - in mpp version routine is called by only one process, therefore
    !       no test on process for error messages is needed.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/st1   first source term package (wam3).
    !     !/st2   second source term package (tc96).
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd
    !/
    use w3wdatmd, only: w3setw, w3dimw
    use w3adatmd, only: w3seta, w3dima, w3xeta
    use w3odatmd, only: w3seto
    !/
    use w3wdatmd, only: time, dinit, wlv, ice, icef, iceh, berg,    &
         ust,  ustdir, asf, rhoair
    use w3adatmd, only: ainit, dw, ua, ud, as, cx, cy, wn,          &
         taua, tauadir
    use w3adatmd, only: hs, wlm, t02, t0m1, t01, fp0, thm, ths, thp0,&
         wbt, wnmean
    use w3adatmd, only: dtdyn, fcut, aba, abd, uba, ubd, sxx, syy, sxy,&
         phs, ptp, plp, pdir, psi, pws, pwst, pnr,    &
         pthp0, pqp, ppe, pgw, psw, ptm1, pt1, pt2,  &
         pep, usero, tauox, tauoy, tauwix, tauwiy,    &
         phiaw, phioc, tusx, tusy, prms, tpms,        &
         ussx, ussy, mssx, mssy, mssd, mscx, mscy,    &
         mscd, qp, tauwnx, tauwny, charn, tws, bhd,   &
         phibbl, taubbl, whitecap, bedforms, cge, ef, &
         cflxymax, cflthmax, cflkmax, p2sms, us3d,    &
         th1m, sth1m, th2m, sth2m, hsig, phice, tauice,&
         stmaxe, stmaxd, hmaxe, hcmaxe, hmaxd, hcmaxd,&
         ussp, tauocx, tauocy, qkk, skew, embia1, embia2
    !/
    use w3odatmd, only: nogrp, ngrpp, idout, undef, ndst, ndse,     &
         flogrd, ipass => ipass1, write => write1,   &
         fnmpre, noswll, noextr
    !/
    use w3servmd, only: extcde
    use w3odatmd, only : iaproc
    use w3odatmd, only :  ofiles
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(inout)        :: iotst
    integer, intent(in)           :: ndsog
    integer, intent(in), optional :: imod
    character, intent(in)         :: inxout*(*)
    character(len=15) :: timetag
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: igrd, ierr, i, j, ix, iy, mogrp,     &
         mgrpp, isea, moswll, ik, ifi, ifj    &
         ,ifilout
    integer, allocatable    :: maptmp(:,:)
    real                    :: aux1(nsea), aux2(nsea),              &
         aux3(nsea), aux4(nsea)
    character(len=30)       :: idtst, tname
    character(len=10)       :: vertst
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! test input parameters ---------------------------------------------- *
    !
    if ( present(imod) ) then
      igrd   = imod
    else
      igrd   = 1
    end if
    !
    call w3seto ( igrd, ndse, ndst )
    call w3setg ( igrd, ndse, ndst )
    call w3seta ( igrd, ndse, ndst )
    call w3xeta ( igrd, ndse, ndst )
    call w3setw ( igrd, ndse, ndst )
    !
    ipass  = ipass + 1
    iotst  = 0
    !
    if (inxout.ne.'read' .and. inxout.ne.'write' ) then
      write (ndse,900) inxout
      call extcde ( 1 )
    end if
    !
    if ( ipass.eq.1 .and. ofiles(1) .eq. 0) then
      write  = inxout.eq.'write'
    else
      if ( write .and. inxout.eq.'read' ) then
        write (ndse,901) inxout
        call extcde ( 2 )
      end if
    end if
    !
    !
    !
    ! open file ---------------------------------------------------------- *
    ! ( ipass = 1 )
    !
    if ( ipass.eq.1 .and. ofiles(1) .eq. 0) then
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      !
      if ( write ) then
        open (ndsog,file=fnmpre(:j)//'out_grd.'//filext(:i),    &
             form ='unformatted', convert=file_endian,err=800,iostat=ierr)
      else
        open (ndsog,file=fnmpre(:j)//'out_grd.'//filext(:i),    &
             form='unformatted', convert=file_endian,err=800,iostat=ierr,status='old')
      end if
      !
      rewind ( ndsog )
      !
      ! test info --------------------------------------------------------- *
      ! ( ipass = 1 )
      !
      if ( write ) then
        write (ndsog)                                           &
             idstr, verogr, gname, nogrp, ngrpp, nsea, nx, ny,     &
             undef, noswll
      else
        read (ndsog,end=801,err=802,iostat=ierr)                &
             idtst, vertst, tname, mogrp, mgrpp, nsea, nx, ny,     &
             undef, moswll
        !
        if ( idtst .ne. idstr ) then
          write (ndse,902) idtst, idstr
          call extcde ( 20 )
        end if
        if ( vertst .ne. verogr ) then
          write (ndse,903) vertst, verogr
          call extcde ( 21 )
        end if
        if ( nogrp .ne. mogrp .or. ngrpp .ne. mgrpp ) then
          write (ndse,904) mogrp, mgrpp, nogrp, ngrpp
          call extcde ( 22 )
        end if
        if ( tname .ne. gname ) then
          write (ndse,905) tname, gname
        end if
        if ( noswll .ne. moswll ) then
          write (ndse,906) moswll, noswll
          call extcde ( 24 )
        end if
        !
      end if
      !
      !
    end if
    !
    !  in case of generation of a new file output every delta output
    ! open file ---------------------------------------------------------- *
    ! ( ipass = 1 )
    !
    if ( ipass.ge.1 .and. ofiles(1) .eq. 1) then
      write  = inxout.eq.'write'
    else
      if ( write .and. inxout.eq.'read' ) then
        write (ndse,901) inxout
        call extcde ( 2 )
      end if
    end if
    !
    if ( ipass.ge.1 .and. ofiles(1) .eq. 1) then
      i      = len_trim(filext)
      j      = len_trim(fnmpre)
      !
      ! create timetag for file name using yyyymmdd.hhmms prefix
      write(timetag,"(i8.8,'.'i6.6)")time(1),time(2)
      if ( write ) then
        open (ndsog,file=fnmpre(:j)//timetag//'.out_grd.'  &
             //filext(:i),form='unformatted', convert=file_endian,err=800,iostat=ierr)
      else
        open (ndsog,file=fnmpre(:j)//'out_grd.'//filext(:i),    &
             form='unformatted', convert=file_endian,err=800,iostat=ierr,status='old')
      end if
      !
      rewind ( ndsog )
      !
      ! test info --------------------------------------------------------- *
      ! ( ipass >= 1 & ofiles(1) = 1)
      !
      if ( write ) then
        write (ndsog)                                           &
             idstr, verogr, gname, nogrp, ngrpp, nsea, nx, ny,     &
             undef, noswll
      else
        read (ndsog,end=801,err=802,iostat=ierr)                &
             idtst, vertst, tname, mogrp, mgrpp, nsea, nx, ny,     &
             undef, moswll
        !
        if ( idtst .ne. idstr ) then
          write (ndse,902) idtst, idstr
          call extcde ( 20 )
        end if
        if ( vertst .ne. verogr ) then
          write (ndse,903) vertst, verogr
          call extcde ( 21 )
        end if
        if ( nogrp .ne. mogrp .or. ngrpp .ne. mgrpp ) then
          write (ndse,904) mogrp, mgrpp, nogrp, ngrpp
          call extcde ( 22 )
        end if
        if ( tname .ne. gname ) then
          write (ndse,905) tname, gname
        end if
        if ( noswll .ne. moswll ) then
          write (ndse,906) moswll, noswll
          call extcde ( 24 )
        end if
        !
      end if
      !
      !
    end if
    !
    ! time and flags ----------------------------------------------------- *
    !
    if ( write ) then
      write (ndsog)                            time, flogrd
    else
      read (ndsog,end=803,err=802,iostat=ierr) time, flogrd
    end if
    !
    !
    ! mapsta ------------------------------------------------------------- *
    !
    allocate ( maptmp(ny,nx) )
    if ( write ) then
      maptmp = mapsta + 8*mapst2
      write (ndsog)                                               &
           ((maptmp(iy,ix),ix=1,nx),iy=1,ny)
    else
      read (ndsog,end=801,err=802,iostat=ierr)                    &
           ((maptmp(iy,ix),ix=1,nx),iy=1,ny)
      mapsta = mod(maptmp+2,8) - 2
      mapst2 = (maptmp-mapsta) / 8
    end if
    deallocate ( maptmp )
    !
    ! fields ---------------------------------------------- *
    !
    ! initialization ---------------------------------------------- *
    !
    if ( write ) then
      do isea=1, nsea
        if ( mapsta(mapsf(isea,2),mapsf(isea,1)) .lt. 0 ) then
          !
          if ( flogrd( 2, 2) ) wlm   (isea) = undef
          if ( flogrd( 2, 3) ) t02   (isea) = undef
          if ( flogrd( 2, 4) ) t0m1  (isea) = undef
          if ( flogrd( 2, 5) ) t01   (isea) = undef
          if ( flogrd( 2, 6) .or. flogrd( 2,18) )                 &
               fp0   (isea) = undef  ! fp or tp
          if ( flogrd( 2, 7) ) thm   (isea) = undef
          if ( flogrd( 2, 8) ) ths   (isea) = undef
          if ( flogrd( 2, 9) ) thp0  (isea) = undef
          ust   (isea) = undef
          ustdir(isea) = undef
          if ( flogrd( 2,10) ) hsig  (isea) = undef
          if ( flogrd( 2,11) ) stmaxe(isea) = undef
          if ( flogrd( 2,12) ) stmaxd(isea) = undef
          if ( flogrd( 2,13) ) hmaxe (isea) = undef
          if ( flogrd( 2,14) ) hcmaxe(isea) = undef
          if ( flogrd( 2,15) ) hmaxd (isea) = undef
          if ( flogrd( 2,16) ) hcmaxd(isea) = undef
          if ( flogrd( 2,17) ) wbt   (isea) = undef
          if ( flogrd( 2,19) ) wnmean(isea) = undef
          !
          if ( flogrd( 3, 1) ) ef   (isea,:) = undef
          if ( flogrd( 3, 2) ) th1m (isea,:) = undef
          if ( flogrd( 3, 3) ) sth1m(isea,:) = undef
          if ( flogrd( 3, 4) ) th2m (isea,:) = undef
          if ( flogrd( 3, 5) ) sth2m(isea,:) = undef
          !
          if ( flogrd( 4, 1) ) phs (isea,:) = undef
          if ( flogrd( 4, 2) ) ptp (isea,:) = undef
          if ( flogrd( 4, 3) ) plp (isea,:) = undef
          if ( flogrd( 4, 4) ) pdir (isea,:) = undef
          if ( flogrd( 4, 5) ) psi (isea,:) = undef
          if ( flogrd( 4, 6) ) pws (isea,:) = undef
          if ( flogrd( 4, 7) ) pthp0(isea,:) = undef
          if ( flogrd( 4, 8) ) pqp (isea,:) = undef
          if ( flogrd( 4, 9) ) ppe(isea,:)  = undef
          if ( flogrd( 4,10) ) pgw(isea,:)  = undef
          if ( flogrd( 4,11) ) psw (isea,:) = undef
          if ( flogrd( 4,12) ) ptm1(isea,:) = undef
          if ( flogrd( 4,13) ) pt1 (isea,:) = undef
          if ( flogrd( 4,14) ) pt2 (isea,:) = undef
          if ( flogrd( 4,15) ) pep (isea,:) = undef
          if ( flogrd( 4,16) ) pwst(isea  ) = undef
          if ( flogrd( 4,17) ) pnr (isea  ) = undef
          !
          if ( flogrd( 5, 2) ) charn (isea) = undef
          if ( flogrd( 5, 3) ) cge   (isea) = undef
          if ( flogrd( 5, 4) ) phiaw (isea) = undef
          if ( flogrd( 5, 5) ) then
            tauwix(isea) = undef
            tauwiy(isea) = undef
          end if
          if ( flogrd( 5, 6) ) then
            tauwnx(isea) = undef
            tauwny(isea) = undef
          end if
          if ( flogrd( 5, 7) ) whitecap(isea,1) = undef
          if ( flogrd( 5, 8) ) whitecap(isea,2) = undef
          if ( flogrd( 5, 9) ) whitecap(isea,3) = undef
          if ( flogrd( 5,10) ) whitecap(isea,4) = undef
          !
          if ( flogrd( 6, 1) ) then
            sxx   (isea) = undef
            syy   (isea) = undef
            sxy   (isea) = undef
          end if
          if ( flogrd( 6, 2) ) then
            tauox (isea) = undef
            tauoy (isea) = undef
          end if
          if ( flogrd( 6, 3) ) bhd(isea) = undef
          if ( flogrd( 6, 4) ) phioc (isea) = undef
          if ( flogrd( 6, 5) ) then
            tusx  (isea) = undef
            tusy  (isea) = undef
          end if
          if ( flogrd( 6, 6) ) then
            ussx  (isea) = undef
            ussy  (isea) = undef
          end if
          if ( flogrd( 6, 7) ) then
            prms  (isea) = undef
            tpms  (isea) = undef
          end if
          if ( flogrd( 6, 8) ) us3d(isea,:) = undef
          if ( flogrd( 6, 9) ) p2sms(isea,:) = undef
          if ( flogrd( 6, 10) ) tauice(isea,:) = undef
          if ( flogrd( 6, 11) ) phice(isea) = undef
          if ( flogrd( 6, 12) ) ussp(isea,:) = undef
          if ( flogrd( 6, 13) ) then
            tauocx(isea) = undef
            tauocy(isea) = undef
          end if
          !
          if ( flogrd( 7, 1) ) then
            aba   (isea) = undef
            abd   (isea) = undef
          end if
          if ( flogrd( 7, 2) ) then
            uba   (isea) = undef
            ubd   (isea) = undef
          end if
          if ( flogrd( 7, 3) ) bedforms(isea,:) = undef
          if ( flogrd( 7, 4) ) phibbl(isea) = undef
          if ( flogrd( 7, 5) ) taubbl(isea,:) = undef
          !
          if ( flogrd( 8, 1) ) then
            mssx  (isea) = undef
            mssy  (isea) = undef
          end if
          if ( flogrd( 8, 2) ) then
            mscx  (isea) = undef
            mscy  (isea) = undef
          end if
          if ( flogrd( 8, 3) ) mssd (isea) = undef
          if ( flogrd( 8, 4) ) mscd (isea) = undef
          if ( flogrd( 8, 5) ) qp   (isea) = undef
          if ( flogrd( 8, 6) ) qkk  (isea) = undef
          if ( flogrd( 8, 7) ) skew (isea) = undef
          if ( flogrd( 8, 8) ) embia1(isea) = undef
          if ( flogrd( 8, 9) ) embia2(isea) = undef
          !
          if ( flogrd( 9, 1) ) dtdyn (isea) = undef
          if ( flogrd( 9, 2) ) fcut  (isea) = undef
          if ( flogrd( 9, 3) ) cflxymax(isea) = undef
          if ( flogrd( 9, 4) ) cflthmax(isea) = undef
          if ( flogrd( 9, 5) ) cflkmax(isea) = undef
          !
        end if
        !
        if ( mapsta(mapsf(isea,2),mapsf(isea,1)) .eq. 2 ) then
          !
          if ( flogrd( 5, 4) ) phiaw (isea) = undef
          if ( flogrd( 5, 5) ) then
            tauwix(isea) = undef
            tauwiy(isea) = undef
          end if
          if ( flogrd( 5, 6) ) then
            tauwnx(isea) = undef
            tauwny(isea) = undef
          end if
          if ( flogrd( 5, 7) ) whitecap(isea,1) = undef
          if ( flogrd( 5, 8) ) whitecap(isea,2) = undef
          if ( flogrd( 5, 9) ) whitecap(isea,3) = undef
          if ( flogrd( 5,10) ) whitecap(isea,4) = undef
          !
          if ( flogrd( 6, 2) )then
            tauox (isea) = undef
            tauoy (isea) = undef
          end if
          if ( flogrd( 6, 4) ) phioc (isea) = undef
          !
          if ( flogrd( 7, 3) ) bedforms(isea,:) = undef
          if ( flogrd( 7, 4) ) phibbl(isea) = undef
          if ( flogrd( 7, 5) ) taubbl(isea,:) = undef
          !
        end if
        !
      end do
      !
    else
      if (.not.dinit) call w3dimw ( igrd, ndse, ndst, .true. )
      if (.not.ainit) call w3dima ( igrd, ndse, ndst, .true. )
    end if
    !
    ! actual output  ---------------------------------------------- *
    do ifi=1, nogrp
      do ifj=1, ngrpp
        if ( flogrd(ifi,ifj) ) then
          !
          !
          if ( write ) then
            !
            !     section 1)
            !
            if ( ifi .eq. 1 .and. ifj .eq. 1 ) then
              write ( ndsog ) dw(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 2 ) then
              write ( ndsog ) cx(1:nsea)
              write ( ndsog ) cy(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 3 ) then
              do isea=1, nsea
                if (ua(isea) .ne.undef) then
                  aux1(isea) = ua(isea)*cos(ud(isea))
                  aux2(isea) = ua(isea)*sin(ud(isea))
                else
                  aux1(isea) = undef
                  aux2(isea) = undef
                end if
              end do
              write ( ndsog ) aux1
              write ( ndsog ) aux2
            else if ( ifi .eq. 1 .and. ifj .eq. 4 ) then
              write ( ndsog ) as(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 5 ) then
              write ( ndsog ) wlv(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 6 ) then
              write ( ndsog ) ice(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 7 ) then
              write ( ndsog ) berg(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 8 ) then
              do isea=1, nsea
                if (taua(isea) .ne.undef) then
                  aux1(isea) = taua(isea)*cos(tauadir(isea))
                  aux2(isea) = taua(isea)*sin(tauadir(isea))
                else
                  aux1(isea) = undef
                  aux2(isea) = undef
                end if
              end do
              write ( ndsog ) aux1
              write ( ndsog ) aux2
            else if ( ifi .eq. 1 .and. ifj .eq. 9 ) then
              write ( ndsog ) rhoair(1:nsea)
              !
              !     section 2)
              !
            else if ( ifi .eq. 2 .and. ifj .eq. 1 ) then
              write ( ndsog ) hs(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 2 ) then
              write ( ndsog ) wlm(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 3 ) then
              write ( ndsog ) t02(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 4 ) then
              write ( ndsog ) t0m1(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 5 ) then
              write ( ndsog ) t01(1:nsea)
            else if ( (ifi .eq. 2 .and. ifj .eq. 6) .or.         &
                 (ifi .eq. 2 .and. ifj .eq. 18) ) then
              ! note: tp output is derived from fp field.
              write ( ndsog ) fp0(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 7 ) then
              write ( ndsog ) thm(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 8 ) then
              write ( ndsog ) ths(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 9 ) then
              write ( ndsog ) thp0(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 10 ) then
              write ( ndsog ) hsig(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 11 ) then
              write ( ndsog ) stmaxe(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 12 ) then
              write ( ndsog ) stmaxd(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 13 ) then
              write ( ndsog ) hmaxe(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 14 ) then
              write ( ndsog ) hcmaxe(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 15 ) then
              write ( ndsog ) hmaxd(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 16 ) then
              write ( ndsog ) hcmaxd(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 17 ) then
              write ( ndsog ) wbt(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 19 ) then
              write ( ndsog ) wnmean(1:nsea)
              !
              !     section 3)
              !
            else if ( ifi .eq. 3 .and. ifj .eq. 1 ) then
              write ( ndsog ) ef(1:nsea,e3df(2,1):e3df(3,1))
            else if ( ifi .eq. 3 .and. ifj .eq. 2 ) then
              write ( ndsog ) th1m(1:nsea,e3df(2,2):e3df(3,2))
            else if ( ifi .eq. 3 .and. ifj .eq. 3 ) then
              write ( ndsog ) sth1m(1:nsea,e3df(2,3):e3df(3,3))
            else if ( ifi .eq. 3 .and. ifj .eq. 4 ) then
              write ( ndsog ) th2m(1:nsea,e3df(2,4):e3df(3,4))
            else if ( ifi .eq. 3 .and. ifj .eq. 5 ) then
              write ( ndsog ) sth2m(1:nsea,e3df(2,5):e3df(3,5))
            else if ( ifi .eq. 3 .and. ifj .eq. 6) then
              write ( ndsog ) wn(1:nk,1:nsea)
              !
              !     section 4)
              !
            else if ( ifi .eq. 4 .and. ifj .eq. 1 ) then
              write ( ndsog ) phs(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 2 ) then
              write ( ndsog ) ptp(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 3 ) then
              write ( ndsog ) plp(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 4 ) then
              write ( ndsog ) pdir(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 5 ) then
              write ( ndsog ) psi(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 6 ) then
              write ( ndsog ) pws(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 7 ) then
              write ( ndsog ) pthp0(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 8  ) then
              write ( ndsog ) pqp(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 9  ) then
              write ( ndsog ) ppe(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 10 ) then
              write ( ndsog ) pgw(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 11 ) then
              write ( ndsog ) psw(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 12 ) then
              write ( ndsog ) ptm1(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 13 ) then
              write ( ndsog ) pt1(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 14 ) then
              write ( ndsog ) pt2(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 15 ) then
              write ( ndsog ) pep(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 16 ) then
              write ( ndsog ) pwst(1:nsea)
            else if ( ifi .eq. 4 .and. ifj .eq. 17 ) then
              write ( ndsog ) pnr(1:nsea)
              !
              !     section 5)
              !
            else if ( ifi .eq. 5 .and. ifj .eq. 1 ) then
              do isea=1, nsea
                ix     = mapsf(isea,1)
                iy     = mapsf(isea,2)
                if ( mapsta(iy,ix) .eq. 1 ) then
                  aux1(isea) = ust(isea) * asf(isea) *        &
                       cos(ustdir(isea))
                  aux2(isea) = ust(isea) * asf(isea) *        &
                       sin(ustdir(isea))
                else
                  aux1(isea) = undef
                  aux2(isea) = undef
                end if
              end do
              write ( ndsog ) aux1
              write ( ndsog ) aux2
            else if ( ifi .eq. 5 .and. ifj .eq. 2 ) then
              write ( ndsog ) charn(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 3 ) then
              write ( ndsog ) cge(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 4 ) then
              write ( ndsog ) phiaw(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 5 ) then
              write ( ndsog ) tauwix(1:nsea)
              write ( ndsog ) tauwiy(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 6 ) then
              write ( ndsog ) tauwnx(1:nsea)
              write ( ndsog ) tauwny(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 7 ) then
              write ( ndsog ) whitecap(1:nsea,1)
            else if ( ifi .eq. 5 .and. ifj .eq. 8 ) then
              write ( ndsog ) whitecap(1:nsea,2)
            else if ( ifi .eq. 5 .and. ifj .eq. 9 ) then
              write ( ndsog ) whitecap(1:nsea,3)
            else if ( ifi .eq. 5 .and. ifj .eq. 10 ) then
              write ( ndsog ) whitecap(1:nsea,4)
            else if ( ifi .eq. 5 .and. ifj .eq. 11 ) then
              write ( ndsog ) tws(1:nsea)
              !
              !     section 6)
              !
            else if ( ifi .eq. 6 .and. ifj .eq. 1 ) then
              write ( ndsog ) sxx(1:nsea)
              write ( ndsog ) syy(1:nsea)
              write ( ndsog ) sxy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 2 ) then
              write ( ndsog ) tauox(1:nsea)
              write ( ndsog ) tauoy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 3  ) then
              write ( ndsog ) bhd(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 4 ) then
              write ( ndsog ) phioc(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 5 ) then
              write ( ndsog ) tusx(1:nsea)
              write ( ndsog ) tusy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 6 ) then
              write ( ndsog ) ussx(1:nsea)
              write ( ndsog ) ussy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 7 ) then
              write ( ndsog ) prms(1:nsea)
              write ( ndsog ) tpms(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 8 ) then
              write ( ndsog ) us3d(1:nsea,   us3df(2):us3df(3))
              write ( ndsog ) us3d(1:nsea,nk+us3df(2):nk+us3df(3))
            else if ( ifi .eq. 6 .and. ifj .eq.  9 ) then
              write ( ndsog ) p2sms(1:nsea,p2msf(2):p2msf(3))
            else if ( ifi .eq. 6 .and. ifj .eq. 10 ) then
              write ( ndsog ) tauice(1:nsea,1)
              write ( ndsog ) tauice(1:nsea,2)
            else if ( ifi .eq. 6 .and. ifj .eq. 11 ) then
              write ( ndsog ) phice(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 12 ) then
              write ( ndsog ) ussp(1:nsea,   1:usspf(2))
              write ( ndsog ) ussp(1:nsea,nk+1:nk+usspf(2))
            else if ( ifi .eq. 6 .and. ifj .eq. 13 ) then
              write ( ndsog ) tauocx(1:nsea)
              write ( ndsog ) tauocy(1:nsea)
              !
              !     section 7)
              !
            else if ( ifi .eq. 7 .and. ifj .eq. 1 ) then
              do isea=1, nsea
                if ( aba(isea) .ne. undef ) then
                  aux1(isea) = aba(isea)*cos(abd(isea))
                  aux2(isea) = aba(isea)*sin(abd(isea))
                else
                  aux1(isea) = undef
                  aux2(isea) = undef
                end if
              end do
              write ( ndsog ) aux1
              write ( ndsog ) aux2
              !write ( ndsog ) aba(1:nsea)
              !write ( ndsog ) abd(1:nsea)
            else if ( ifi .eq. 7 .and. ifj .eq. 2 ) then
              do isea=1, nsea
                if ( uba(isea) .ne. undef ) then
                  aux1(isea) = uba(isea)*cos(ubd(isea))
                  aux2(isea) = uba(isea)*sin(ubd(isea))
                else
                  aux1(isea) = undef
                  aux2(isea) = undef
                end if
              end do
              write ( ndsog ) aux1
              write ( ndsog ) aux2
              !                    write ( ndsog ) uba(1:nsea)
              !                    write ( ndsog ) ubd(1:nsea)
            else if ( ifi .eq. 7 .and. ifj .eq. 3 ) then
              write ( ndsog ) bedforms(1:nsea,1)
              write ( ndsog ) bedforms(1:nsea,2)
              write ( ndsog ) bedforms(1:nsea,3)
            else if ( ifi .eq. 7 .and. ifj .eq. 4 ) then
              write ( ndsog ) phibbl(1:nsea)
            else if ( ifi .eq. 7 .and. ifj .eq. 5 ) then
              write ( ndsog ) taubbl(1:nsea,1)
              write ( ndsog ) taubbl(1:nsea,2)
              !
              !     section 8)
              !skewness
            else if ( ifi .eq. 8 .and. ifj .eq. 1 ) then
              write ( ndsog ) mssx(1:nsea)
              write ( ndsog ) mssy(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 2 ) then
              write ( ndsog ) mscx(1:nsea)
              write ( ndsog ) mscy(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 3 ) then
              write ( ndsog ) mssd(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 4 ) then
              write ( ndsog ) mscd(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 5 ) then
              write ( ndsog ) qp(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 6 ) then
              write ( ndsog ) qkk(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 7 ) then
              write ( ndsog ) skew(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 8 ) then
              write ( ndsog ) embia1(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 9 ) then
              write ( ndsog ) embia2(1:nsea)
              !
              !     section 9)
              !
            else if ( ifi .eq. 9 .and. ifj .eq. 1 ) then
              write ( ndsog ) dtdyn(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 2 ) then
              write ( ndsog ) fcut(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 3 ) then
              write ( ndsog ) cflxymax(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 4 ) then
              write ( ndsog ) cflthmax(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 5 ) then
              write ( ndsog ) cflkmax(1:nsea)
              !
              !     section 10)
              !
            else if ( ifi .eq. 10 ) then
              write ( ndsog ) usero(1:nsea,ifj)
              !
            end if
            !
          else
            !
            !     start of reading ......
            !
            !     section 1)
            !
            if ( ifi .eq. 1 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr) dw(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr) cx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) cy(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr) ua(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) ud(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr) as(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr) wlv(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 6 ) then
              read (ndsog,end=801,err=802,iostat=ierr) ice(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 7 ) then
              read (ndsog,end=801,err=802,iostat=ierr) berg(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 8 ) then
              read (ndsog,end=801,err=802,iostat=ierr) taua(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) tauadir(1:nsea)
            else if ( ifi .eq. 1 .and. ifj .eq. 9 ) then
              read (ndsog,end=801,err=802,iostat=ierr) rhoair(1:nsea)
              !
              !     section 2)
              !
            else if ( ifi .eq. 2 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr) hs(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr) wlm(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr) t02(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr) t0m1(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr) t01(1:nsea)
            else if ( (ifi .eq. 2 .and. ifj .eq. 6) .or.       &
                 (ifi .eq. 2 .and. ifj .eq. 18) ) then
              ! note: tp output is derived from fp field.
              read (ndsog,end=801,err=802,iostat=ierr) fp0(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 7 ) then
              read (ndsog,end=801,err=802,iostat=ierr) thm(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 8 ) then
              read (ndsog,end=801,err=802,iostat=ierr) ths(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 9 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   thp0(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 10 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   hsig(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 11 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   stmaxe(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 12 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   stmaxd(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 13 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   hmaxe(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 14 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   hcmaxe(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 15 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   hmaxd(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 16 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   hcmaxd(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 17 ) then
              read (ndsog,end=801,err=802,iostat=ierr) wbt(1:nsea)
            else if ( ifi .eq. 2 .and. ifj .eq. 19 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   wnmean(1:nsea)
              !
              !     section 3)
              !
            else if ( ifi .eq. 3 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   ef(1:nsea,e3df(2,1):e3df(3,1))
            else if ( ifi .eq. 3 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   th1m(1:nsea,e3df(2,2):e3df(3,2))
            else if ( ifi .eq. 3 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   sth1m(1:nsea,e3df(2,3):e3df(3,3))
            else if ( ifi .eq. 3 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   th2m(1:nsea,e3df(2,4):e3df(3,4))
            else if ( ifi .eq. 3 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   sth2m(1:nsea,e3df(2,5):e3df(3,5))
            else if ( ifi .eq. 3 .and. ifj .eq. 6) then
              read (ndsog,end=801,err=802,iostat=ierr)  &
                   wn(1:nk,1:nsea)
              !
              !     section 4)
              !
            else if ( ifi .eq. 4 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   phs(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   ptp(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   plp(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pdir(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   psi(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 6 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pws(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 7 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pthp0(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 8  ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pqp(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 9  ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   ppe(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 10 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pgw(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 11 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   psw(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 12 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   ptm1(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 13 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pt1(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 14 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pt2(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 15 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pep(1:nsea,0:noswll)
            else if ( ifi .eq. 4 .and. ifj .eq. 16) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   pwst(1:nsea)
            else if ( ifi .eq. 4 .and. ifj .eq. 17) then
              read (ndsog,end=801,err=802,iostat=ierr) pnr(1:nsea)
              !
              !     section 5)
              !
            else if ( ifi .eq. 5 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr)          &
                   ust(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)          &
                   ustdir(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   charn(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr) cge(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   phiaw(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauwix(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauwiy(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 6 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauwnx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauwny(1:nsea)
            else if ( ifi .eq. 5 .and. ifj .eq. 7 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   whitecap(1:nsea,1)
            else if ( ifi .eq. 5 .and. ifj .eq. 8 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   whitecap(1:nsea,2)
            else if ( ifi .eq. 5 .and. ifj .eq. 9 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   whitecap(1:nsea,3)
            else if ( ifi .eq. 5 .and. ifj .eq. 10 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   whitecap(1:nsea,4)
            else if ( ifi .eq. 5 .and. ifj .eq. 11 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tws(1:nsea)
              !
              !     section 6)
              !
            else if ( ifi .eq. 6 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr) sxx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) syy(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) sxy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauox(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauoy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   bhd(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   phioc(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tusx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tusy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 6 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   ussx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   ussy(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 7 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   prms(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tpms(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 8 ) then
              read (ndsog,end=801,err=802,iostat=ierr)  &
                   us3d(1:nsea,us3df(2):us3df(3))
              read (ndsog,end=801,err=802,iostat=ierr)  &
                   us3d(1:nsea,nk+us3df(2):nk+us3df(3))
            else if ( ifi .eq. 6 .and. ifj .eq.  9 ) then
              read (ndsog,end=801,err=802,iostat=ierr)     &
                   p2sms(1:nsea,p2msf(2):p2msf(3))
            else if ( ifi .eq. 6 .and. ifj .eq. 10 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauice(1:nsea,1)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauice(1:nsea,2)
            else if ( ifi .eq. 6 .and. ifj .eq. 11 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   phice(1:nsea)
            else if ( ifi .eq. 6 .and. ifj .eq. 12 ) then
              read (ndsog,end=801,err=802,iostat=ierr)  &
                   ussp(1:nsea,1:usspf(2))
              read (ndsog,end=801,err=802,iostat=ierr)  &
                   ussp(1:nsea,nk+1:nk+usspf(2))
            else if ( ifi .eq. 6 .and. ifj .eq. 13 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauocx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   tauocy(1:nsea)
              !
              !     section 7)
              !
            else if ( ifi .eq. 7 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr) aba(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) abd(1:nsea)
            else if ( ifi .eq. 7 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr) uba(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr) ubd(1:nsea)
            else if ( ifi .eq. 7 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   bedforms(1:nsea,1)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   bedforms(1:nsea,2)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   bedforms(1:nsea,3)
            else if ( ifi .eq. 7 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   phibbl(1:nsea)
            else if ( ifi .eq. 7 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   taubbl(1:nsea,1)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   taubbl(1:nsea,2)
              !
              !     section 8)
              !
            else if ( ifi .eq. 8 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   mssx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   mssy(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   mscx(1:nsea)
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   mscy(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   mssd(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   mscd(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr) qp(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 6 ) then
              read (ndsog,end=801,err=802,iostat=ierr) qkk(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 7 ) then
              read (ndsog,end=801,err=802,iostat=ierr) skew(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 8 ) then
              read (ndsog,end=801,err=802,iostat=ierr) embia1(1:nsea)
            else if ( ifi .eq. 8 .and. ifj .eq. 9 ) then
              read (ndsog,end=801,err=802,iostat=ierr) embia2(1:nsea)
              !
              !     section 9)
              !
            else if ( ifi .eq. 9 .and. ifj .eq. 1 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   dtdyn(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 2 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   fcut(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 3 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   cflxymax(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 4 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   cflthmax(1:nsea)
            else if ( ifi .eq. 9 .and. ifj .eq. 5 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   cflkmax(1:nsea)
              !
              !     section 10)
              !
            else if ( ifi .eq. 10 ) then
              read (ndsog,end=801,err=802,iostat=ierr)         &
                   usero(1:nsea,ifj)
            end if
            !
            ! end of test on write/read:
            !
          end if
          !
          ! end of test on  flogrd(ifi,ifj):
          !
        end if
        !
        ! end of ifi and ifj loops
        !
      end do
    end do
    !
    ! flush the buffers for write
    !
    if ( write ) call flush ( ndsog )
    !
    if(ofiles(1) .eq. 1) close(ndsog)
    !
    call w3seta ( igrd, ndse, ndst )
    !
    return
    !
    ! escape locations read errors
    !
800 continue
    write (ndse,1000) ierr
    call extcde ( 41 )
    !
801 continue
    write (ndse,1001)
    call extcde ( 42 )
    !
802 continue
    write (ndse,1002) ierr
    call extcde ( 43 )
    !
803 continue
    iotst  = -1
    return
    !
    ! formats
    !
900 format (/' *** wavewatch iii error in w3iogo :'/                &
         '     ilegal inxout value: ',a/)
901 format (/' *** wavewatch iii error in w3iogo :'/                &
         '     mixed read/write, last request: ',a/)
902 format (/' *** wavewatch iii error in w3iogo :'/                &
         '     ilegal idstr, read : ',a/                        &
         '                  check : ',a/)
903 format (/' *** wavewatch iii error in w3iogo :'/                &
         '     ilegal verogr, read : ',a/                       &
         '                   check : ',a/)
904 format (/' *** wavewatch iii error in w3iogo :'/                &
         '     different number of fields, file :',i8,i8/       &
         '                              program :',i8,i8/)
905 format (/' *** wavewatch iii warning in w3iogo :'/              &
         '     ilegal gname, read : ',a/                        &
         '                  check : ',a/)
906 format (/' *** wavewatch iii error in w3iogo :'/                &
         '     ilegal noswll, read : ',i4/                      &
         '                   check : ',i4/)
    !
    !  999 format (/' *** wavewatch iii error in w3iogo :'/                &
    !               '     please update fields !!! '/)
    !
1000 format (/' *** wavewatch iii error in w3iogo : '/               &
         '     error in opening file'/                          &
         '     iostat =',i5/)
1001 format (/' *** wavewatch iii error in w3iogo : '/               &
         '     premature end of file'/)
1002 format (/' *** wavewatch iii error in w3iogo : '/               &
         '     error in reading from file'/                     &
         '     iostat =',i5/)
    !
    !/
    !/ end of w3iogo ----------------------------------------------------- /
    !/
  end subroutine w3iogo
  !/
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief output stokes drift related parameters.
  !>
  !> @details this code is built for the purpose of outputting stokes
  !>  drift related parameters that can be utilized to obtain full
  !>  stokes drift profiles external to the wave model.
  !>
  !> @param[in] a  input spectra, left in par list to change shape.
  !> @param[in] uss_switch  switch if computing us3d (spectral) or ussp (partitions).
  !>
  !> @author h. l. tolman  @date 10-jan-2017
  !>
  subroutine calc_u3stokes ( a , uss_switch )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    10-jan-2017 : separate stokes drift calculation  ( version 6.01 )
    !/
    !  1. purpose :
    !
    !     this code is built for the purpose of outputting stokes drift
    !       related parameters that can be utilized to obtain full
    !       stokes drift profiles external to the wave model.
    !
    !     option 1: uss_switch == 1
    !               this method is for outputing the stokes drift frequency
    !               spectrum for spectral frequency bands as defined by the
    !               ww3 computation spectral frequency grid.
    !               output quantity: stokes drift frequency spectrum [m/s/hz]
    !                                x and y componenets.
    !
    !     option 2: uss_switch == 2
    !               this method is for outputing the surface stokes drift
    !               for a specified frequency partition/band of the
    !               wave spectrum.  these partitions do not need to be
    !               matched to ww3's computation spectral frequency grid,
    !               and will rather sum the contributions of the ww3 bands
    !               into the output partition.  the partitions are defined
    !               in the ww3_grid.inp namelist section.
    !               output quantity: stokes drift surface velocity [m/s]
    !                                x and y components
    !                                for each partition (up to 25).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   i   input spectra. left in par list to change
    !                          shape.
    !      uss_switch  i   i   switch if computing us3d (spectral) or ussp
    !                           (partitions)
    !     ----------------------------------------------------------------
    !
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
    !     none.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/shrd  switch for shared / distributed memory architecture.
    !     !/dist  id.
    !
    !     !/ompg  openmp compiler directive for loop splitting.
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpiinv, grav, tpi
    use w3gdatmd,  only: dden, dsii, xfr, sig, nk, nth, nseal,    &
         ecos, esin, us3df, usspf, ussp_wn
    use w3adatmd,  only: cg, wn, dw
    use w3adatmd,  only: ussx, ussy,  us3d, ussp
    use w3odatmd, only: iaproc, naproc
    use w3parall, only: init_get_isea
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk,0:nseal)
    integer, intent(in)     :: uss_switch
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, isea, jsea
    integer                 :: ikst, ikfi, ib
    real                    :: factor, fkd,kd
    real                    :: abx(nseal), aby(nseal), ussco
    real                    :: mindiff
    integer                 :: spc2bnd(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  initialize storage arrays -------------------------------------- *
    !
    ! 2.  integral over discrete part of spectrum ------------------------ *
    !
    !two options ----------------------------------------------------|
    ! uss_switch == 1 -> old option, stokes drift integrated in same |
    !                    wavenumber bands as model integrates.       |
    ! uss_switch == 2 -> new option, stokes drift integrated in a    |
    !                    defined number (np) of user specified       |
    !                    partitions, where np and the frequency      |
    !                    ranges for each partition can be user       |
    !                    defined at run-time.                        |
    !----------------------------------------------------------------|
    if (uss_switch==1) then
      ikst=us3df(2)!start at us3df(2)
      ikfi=us3df(3)!end at us3df(3)
    elseif (uss_switch==2) then
      ikst=1  ! start at 1
      ikfi=nk ! end at nk
    endif
    ! initialize us3d/ussp
    if (uss_switch.eq.1) then
      us3d(:,:)=0.0
    elseif (uss_switch.eq.2) then
      ussp(:,:)=0.0
    endif
    do ik=ikst,ikfi   !1, nk
      !
      ! 2.a initialize energy in band
      !
      abx    = 0.
      aby    = 0.
      !
      ! 2.b integrate energy in band
      !
      do ith=1, nth
        !
        !
        do jsea=1, nseal
          abx(jsea)  = abx(jsea) + a(ith,ik,jsea)*ecos(ith)
          aby(jsea)  = aby(jsea) + a(ith,ik,jsea)*esin(ith)
        end do
        !
        !
      end do
      !
      ! 2.c finalize integration over band and update mean arrays
      !
      !
      !
      do jsea=1, nseal
        call init_get_isea(isea, jsea)
        factor       = dden(ik) / cg(ik,isea)
        !
        ! deep water limits
        !
        kd    = max ( 0.001 , wn(ik,isea) * dw(isea) )
        if ( kd .lt. 6. ) then
          fkd       = factor / sinh(kd)**2
          ussco=fkd*sig(ik)*wn(ik,isea)*cosh(2.*kd)
        else
          ussco=factor*sig(ik)*2.*wn(ik,isea)
        end if
        !
        !
        !ussx(jsea)  = ussx(jsea) + abx(jsea)*ussco
        !ussy(jsea)  = ussy(jsea) + aby(jsea)*ussco
        !
        ! fills the 3d stokes drift spectrum array or surface stokes partitions
        !
        if (uss_switch==1) then
          !old method fills into ww3 bands
          if (ik.ge.us3df(2).and.ik.le.us3df(3)) then
            us3d(jsea,ik)    =  abx(jsea)*ussco/(dsii(ik)*tpiinv)
            us3d(jsea,nk+ik) =  aby(jsea)*ussco/(dsii(ik)*tpiinv)
          endif
        elseif (uss_switch==2) then
          ! match each spectral component to the nearest partition
          mindiff=1.e8
          spc2bnd(ik) = 1
          mindiff=abs(ussp_wn(1)-wn(ik,isea))
          do ib=2,usspf(2)
            if (mindiff .gt. abs(ussp_wn(ib)-wn(ik,isea))) then
              spc2bnd(ik) = ib
              mindiff = abs(ussp_wn(ib)-wn(ik,isea))
            endif
          enddo
          !put spectral energey into whichever band central wavenumber fits in
          ussp(jsea,spc2bnd(ik))    =  ussp(jsea,spc2bnd(ik)) + abx(jsea)*ussco
          ussp(jsea,nk+spc2bnd(ik)) =  ussp(jsea,nk+spc2bnd(ik)) + aby(jsea)*ussco
        endif
      end do
    end do
    !
    return
    !
    !/ end of calc_u3stokes
    !----------------------------------------------------- /
    !/
  end subroutine calc_u3stokes
  !/
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief estimate the dominant wave breaking probability b_t.
  !>
  !> @details estimate the dominant wave breaking probability b_t based on
  !>  the empirical parameterization proposed by babanin et al. (2001).
  !>
  !> @verbatim
  !>     from their fig. 12, we have
  !>
  !>         b_t = 85.1 * [(εp - 0.055) * (1 + h_s/d)]^2.33,
  !>
  !>     where ε is the significant steepness of the spectral peak, h_s is
  !>     the significant wave height, d is the water depth.
  !>
  !>     for more details, please see
  !>         banner et al.  2000: jpo,      30,  3145 -  3160.
  !>         babanin et al. 2001: jgr, 106(c6), 11569 - 11676.
  !> @endverbatim
  !>
  !> @param[in] a  input wave action spectra n(j, θ, k).
  !>
  !> @author q. liu  @date 24-aug-2018
  !>
  subroutine calc_wbt (a)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           q. liu                  |
    !/                  |                        fortran 90 |
    !/                  | last update :         24-aug-2018 |
    !/                  +-----------------------------------+
    !/
    !/    24-aug-2018 : origination.                       ( version 6.06 )
    !/
    !  1. purpose :
    !
    !     estimate the dominant wave breaking probability b_t based on
    !     the empirical parameterization proposed by babanin et al. (2001).
    !     from their fig. 12, we have
    !
    !         b_t = 85.1 * [(εp - 0.055) * (1 + h_s/d)]^2.33,
    !
    !     where ε is the significant steepness of the spectral peak, h_s is
    !     the significant wave height, d is the water depth.
    !
    !     for more details, please see
    !         banner et al.  2000: jpo,      30,  3145 -  3160.
    !         babanin et al. 2001: jgr, 106(c6), 11569 - 11676.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.   i   input wave action spectra n(j, θ, k)
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3outg    subr. public   calculate mean parameters.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s     enable subroutine tracing.
    !     !/t     test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3dispmd, only: wavnu1
    use w3adatmd, only: u10, u10d, wbt
    use w3adatmd, only: cg, wn, dw
    use w3gdatmd, only: nk, nth, nseal, sig, esin, ecos, dth, dsii, &
         fte, xfr, mapsf, mapsta, dmin
    use w3gdatmd, only: btbeta
    use w3parall, only: init_get_isea
    !
    implicit none
    !
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)     :: a  (nth, nk, 0:nseal)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !
    integer              :: fpopt = 0
    !
    integer              :: ik, ith, isea, jsea, ikm, ikl, ikh, ix, iy
    real                 :: tdpt, tu10, tudir, sinu, cosu, tc, tforce
    real                 :: esig(nk) ! e(σ)
    real                 :: factor, et, hs, etp, hsp, sigp, kp, &
         cgp, wstp
    real                 :: xl, xh, xl2, xh2, el, eh, denom
    real                 :: twbt
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    do jsea = 1, nseal
      ! jsea 2 isea
      call init_get_isea(isea, jsea)
      !
      ! check the status of this grid point [escape if this point is excluded]
      !
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      if ( mapsta(iy,ix) .le. 0 ) cycle
      !
      ! wind info. is required to select wind sea partition from the wave
      ! spectrum. two wind velocities are availabe:
      ! - u10 & u10d   (w3adatmd)
      ! - ust & ustdir (w3wdatmd)
      !     * u10d & ustdir are not really the same when swell are present.
      !
      ! following janssen et al. (1989) and bidlot (2001), spectral components
      ! are considered to be subject to local wind forcing when
      !
      !          c / [u cos(θ - φ)] < β,
      !
      ! where c is the phase velocity c = σ/k, φ is the wind direction, u is
      ! the wind speed u10, (sometimes approximated by u10≅ 28 * ust), β is
      ! the constant forcing parameter with β∈ [1.0, 2.0]. by default, we use
      ! β = 1.2(bidlot 2001).
      !
      tdpt  = max(dw(isea), dmin)          ! water depth d
      tu10  = u10(isea)                    ! wind velocity u10
      tudir = u10d(isea)                   ! wind direction φ (rad)
      sinu  = sin(tudir)                   ! sinφ
      cosu  = cos(tudir)                   ! cosφ
      !
      esig  = 0.                           ! e(σ)
      et    = 0.                           ! σe(σ)δσ
      etp   = 0.                           ! σe(σ)δσ at peak only
      !
      do ik = 1, nk
        tc     = sig(ik) / wn(ik, isea)  ! phase velocity c=σ/k
        factor = sig(ik) / cg(ik, isea)  ! σ / cg
        factor = factor * dth            ! σ / cg * δθ
        !
        do ith = 1, nth
          tforce = tc - tu10 * (cosu*ecos(ith)+sinu*esin(ith)) &
               * btbeta
          if (tforce .lt. 0.) then ! wind sea component
            esig(ik) = esig(ik) + a(ith, ik, jsea) * factor
          endif
        enddo ! ith
        !
      enddo ! ik
      !
      ! esig is e(σ) of the wind sea after filtration of any background swell.
      ! now we need to get hs & σp for the wind sea spectrum.
      ! fte    = 0.25 * sig(nk) * dth * sig(nk) [ww3_grid.ftn]
      !
      et = sum(esig * dsii)
      et = et + esig(nk) * fte / (dth * sig(nk))  ! fte: add tail
      hs = 4. * sqrt(max(0., et))                 ! hs
      !
      ! get σp from e(σ)
      !
      ! here we have tried three different ways to calculate fp:
      !
      ! fpopt = 0: fp defined by young (1999, p. 239)
      ! fpopt = 1: parabolic fit around the discrete peak frequency, as used
      !            by ww3_outp
      ! fpopt = 2: discrete peak frequency
      !
      ! when the discrete peak frequency is used:
      ! * for xfr = 1.1, the **discrete** peak region [0.7σp, 1.3σp] will be
      !     {0.75, 0.83, 0.91, 1., 1.1, 1.21, 1.33}σp,
      ! * and for xfr = 1.07, the **discrete** peak region becomes
      !     {0.71, 0.76, 0.82, 0.87, 0.93, 1., 1.07, 1.14, 1.23, 1.31}σp.
      !
      ! thus, a good approximation to the range [0.7σp, 1.3σp] is guranteed
      ! by each xfr. i however found using the discrete peak frequency yielded
      ! step-wise results. according to my test, the smoothest results were
      ! obtained with fpopt = 0. for simplicity, the δσ values (dsii) are
      ! not modified.
      !
      ikm   = maxloc(esig, 1)                     ! index for σp
      !
      if (fpopt .eq. 0) then
        !             fp defined in ian's book
        sigp = sum(esig**4. * sig(1:nk) * dsii) /  &
             max(1e-10, sum(esig**4. * dsii))
        !
      else if (fpopt .eq. 1) then
        !             parabolic fit around the discrete peak (ww3_outp.ftn)
        xl    = 1./xfr - 1.
        xh    = xfr - 1.
        xl2   = xl**2.
        xh2   = xh**2.
        ikl   = max (  1 , ikm-1 )
        ikh   = min ( nk , ikm+1 )
        el    = esig(ikl) - esig(ikm)
        eh    = esig(ikh) - esig(ikm)
        denom = xl*eh - xh*el
        sigp  = sig(ikm) * (1. + 0.5 * ( xl2*eh - xh2*el) &
             / sign (max(abs(denom), 1.e-15), denom)) ! σp
        !
      else if (fpopt .eq. 2) then
        !             discrete peak (give stepwise results, not used by default)
        sigp  = sig(ikm)
      endif
      !
      ! kp from σp (linear dispersion)
      !
      ! n(k, θ) at first step is zero →  σp=0 → floating divided by zero error
      if (sigp < 1e-6) sigp = sig(nk)     ! hsp & b_t will be still 0.
      !
      call wavnu1 (sigp, tdpt, kp, cgp)
      !
      !                         { /1.3σp         }1/2
      ! peak wave height hp = 4 { |      e(σ) dσ }
      !                         { /0.7σp         }
      !
      do ik = 1, nk
        if ( (sig(ik) >= 0.7 * sigp) .and. &
             (sig(ik) <= 1.3 * sigp) ) then
          etp = etp + esig(ik) * dsii(ik)
        endif
      enddo ! ik
      hsp = 4. * sqrt(max(0., etp))
      !
      ! significant steepness of the peak region εp
      !
      wstp = 0.5 * kp * hsp
      !
      ! dominant wave breaking b_t
      !
      twbt = 85.1 * (max(0.0, wstp - 0.055) * (1 + hs/tdpt))**2.33
      wbt(jsea) = min(1.0, twbt)
      !
    enddo ! jsea
    !/
    !/ end of  calc_wbt -------------------------------------------------- /
    !/
  end subroutine calc_wbt
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief  computation of second order harmonics and
  !>         relevant tables for the altimeter corrections
  !>
  !> @param[in]    nkhf   extended number of frequencies.
  !> @param[out]   fac0   2nd order coef correction.
  !> @param[out]   fac1   2nd order coef correction.
  !> @param[out]   fac2   2nd order coef correction.
  !> @param[out]   fac3   2nd order coef correction.
  !>
  !> @author p. janssen  @date 29-mar-2024
  !>
      subroutine secondhh(nkhf,fac0,fac1,fac2,fac3)
!----------------------------------------------------------------
!**** *secondhh* - computation of second order harmonics and
!                  relevant tables for the altimeter corrections.
!     p.a.e.m. janssen
!     purpose.
!     ---------
!          compute second harmonics
!**   interface.
!     ----------
!          *call* *secondhh*
!     method.
!     -------
!          see reference.
!     externals.
!     ----------
!         vmin_d
!         vplus_d          
!     references.
!     -----------
!          v e zakharov(1967)
!-------------------------------------------------------------------
!-------------------------------------------------------------------
use constants, only: grav, tpi
use w3gdatmd,  only: nk, nth, xfr, sig, th, dth, ecos, esin
      implicit none
 !     real(kind=4) :: vmin_d,vplus_d
      integer, intent(in) :: nkhf
      real(kind=4), dimension(nth,nth,nkhf,nkhf), intent(out)  :: fac0, fac1, fac2, fac3
      real(kind=4), parameter   :: fratio = 1.1
      integer :: m, k1, m1, k2, m2
      real(kind=4), parameter :: del1=1.0e-8
      real(kind=4), parameter :: zconst = 0.0281349
      !real(kind=4) :: vmin_d, vplus_d
      real(kind=4) :: co1
      real(kind=4) :: xk1, xk1sq, xk2, xk2sq, xk3
      real(kind=4) :: cosdiff
      real(kind=4) :: x12, x13, x32, om1, om2, om3, f1, f2, f3
      real(kind=4) :: vm, vp
      real(kind=4) :: delom1, delom2
      real(kind=4) :: delom321, delom312
      real(kind=4) :: c22, s22
      real(kind=4), dimension(nth,nth,nkhf,nkhf) :: b
      real(kind=4), dimension(:), allocatable:: fak, sighf, dfimhf
 
!-----------------------------------------------------------------------
!*    1. initialise relevant quantities.
      allocate(fak(nkhf))
      allocate(sighf(nkhf))
      allocate(dfimhf(nkhf))
      sighf(1)  = sig(1)
      do m=2,nkhf
        sighf(m) = xfr*sighf(m-1)
      enddo
      do m=1,nkhf
         fak(m) = (sighf(m))**2/grav
      enddo
      co1 = 0.5*(xfr-1.)*dth
      dfimhf(1) = co1*sighf(1)
      do m=2,nkhf-1
         dfimhf(m)=co1*(sighf(m)+sighf(m-1))
      enddo
      dfimhf(nkhf)=co1*sighf(nkhf-1)
      do m2=1,nkhf
        xk2 = fak(m2)
        xk2sq = fak(m2)**2
        do  m1=1,nkhf
          xk1 = fak(m1)
          xk1sq = fak(m1)**2
          do k1=1,nth
            do k2=1,nth
              cosdiff = cos(th(k1)-th(k2))
              x12 = xk1*xk2*cosdiff
              xk3 = xk1sq + xk2sq +2.0*x12 +del1
              xk3 = sqrt(xk3)
              x13 = xk1sq+x12
              x32 = x12+xk2sq
              om1 = sqrt(grav*xk1)
              om2 = sqrt(grav*xk2)
              om3 = sqrt(grav*xk3)
              f1 = sqrt(xk1/(2.0*om1))
              f2 = sqrt(xk2/(2.0*om2))
              f3 = sqrt(xk3/(2.0*om3))
              vm = tpi*vmin_d(xk3,xk1,xk2,x13,x32,x12,om3,om1,om2)
              vp = tpi*vplus_d(-xk3,xk1,xk2,-x13,-x32,x12,om3,om1,om2)
              delom1 = om3-om1-om2+del1
              delom2 = om3+om1+om2+del1
              fac0(k1,k2,m1,m2) = -f3/(f1*f2)*(vm/(delom1)+             &
     &                            vp/(delom2))
            enddo
          enddo
        enddo
      enddo
      do m2=1,nkhf
        xk2 = fak(m2)
        xk2sq = fak(m2)**2
        do  m1=1,nkhf
          xk1 = fak(m1)
          xk1sq = fak(m1)**2
          do k1=1,nth
            do k2=1,nth
              cosdiff = cos(th(k1)-th(k2))
              x12 = xk1*xk2*cosdiff
              xk3 = xk1sq + xk2sq - 2.*x12 + del1
              xk3 = sqrt(xk3)
              x13 = xk1sq-x12
              x32 = x12-xk2sq
              om1 = sqrt(grav*xk1)
              om2 = sqrt(grav*xk2)
              om3 = sqrt(grav*xk3)+del1
              f1 = sqrt(xk1/(2.0*om1))
              f2 = sqrt(xk2/(2.0*om2))
              f3 = sqrt(abs(xk3)/(2.0*om3))
              vm = tpi*vmin_d(xk1,xk3,xk2,x13,x12,x32,om1,om3,om2)
              vp = tpi*vmin_d(xk2,-xk3,xk1,-x32,x12,-x13,om2,om3,om1)
              delom321 = om3+om2-om1+del1
              delom312 = om3+om1-om2+del1
              b(k1,k2,m1,m2) = -f3/(f1*f2)*(vm/(delom321)+              &
     &                         vp/(delom312))
            enddo
          enddo
        enddo
      enddo
      do m2=1,nkhf
        xk2sq = fak(m2)**2
        do m1=1,nkhf
          xk1sq = fak(m1)**2
          do k2=1,nth
            do k1=1,nth
              c22 = fac0(k1,k2,m1,m2)+b(k1,k2,m1,m2)
              s22 = b(k1,k2,m1,m2)-fac0(k1,k2,m1,m2)
              fac1(k1,k2,m1,m2) =                                       &
     &             (xk1sq*ecos(k1)**2 + xk2sq*ecos(k2)**2)*c22        &
     &             -fak(m1)*fak(m2)*ecos(k1)*ecos(k2)*s22
              fac2(k1,k2,m1,m2) =                                       &
     &             (xk1sq*esin(k1)**2 + xk2sq*esin(k2)**2)*c22        &
     &             -fak(m1)*fak(m2)*esin(k1)*esin(k2)*s22
              fac3(k1,k2,m1,m2) =                                       &
     &             (xk1sq*esin(k1)*ecos(k1) +                         &
     &              xk2sq*esin(k2)*ecos(k2))*c22                      &
     &             -fak(m1)*fak(m2)*ecos(k1)*esin(k2)*s22
              fac0(k1,k2,m1,m2) = c22
            enddo
          enddo
        enddo
      enddo
     contains
!-----------------------------------------------------------------------
     real(kind=4) function vmin_d(xi,xj,xk,xij,xik,xjk,xoi,xoj,xok)
     
!     peter janssen
!     purpose.
!     --------
!              gives nonlinear transfer coefficient for three
!              wave interactions of deep-water waves in the
!              ideal case of no current. (cf.zakharov)
!     interface.
!     ----------
!              *vmin_d(xi,xj,xk)*
!                      *xi*  - wave number
!                      *xj*  - wave number
!                      *xk*  - wave number
!     method.
!     -------
!              none
!     externals.
!     ----------
!              none.
!***  1. determine nonlinear transfer.
!     --------------------------------
      implicit none
      real, intent(in) :: xi, xj, xk, xij, xik, xjk, xoi, xoj, xok
      real :: ri, rj, rk, oi, oj, ok, sqijk, sqikj, sqjki
      ri=abs(xi)+del1
      rj=abs(xj)+del1
      rk=abs(xk)+del1
      oi=xoi+del1
      oj=xoj+del1
      ok=xok+del1
      sqijk=sqrt(oi*oj*rk/(ok*ri*rj))
      sqikj=sqrt(oi*ok*rj/(oj*ri*rk))
      sqjki=sqrt(oj*ok*ri/(oi*rj*rk))
      vmin_d=zconst*( (xij-ri*rj)*sqijk + (xik-ri*rk)*sqikj             &
     &                + (xjk+rj*rk)*sqjki )
      end function vmin_d      
!-----------------------------------------------------------------------
      real(kind=4) function vplus_d(xi,xj,xk,xij,xik,xjk,xoi,xoj,xok)
!***  *vplus_d*  determines the nonlinear transfer coefficient for three
!                wave interactions of deep-water waves.
!     peter janssen
!     purpose.
!     --------
!              gives nonlinear transfer coefficient for three
!              wave interactions of gravity-capillary waves in the
!              ideal case of no current. (cf.zakharov)
!     interface.
!     ----------
!              *vplus_d(xi,xj,xk)*
!                        *xi*  - wave number
!                        *xj*  - wave number
!                        *xk*  - wave number
!     method.
!     -------
!              none
!     externals.
!     ----------
!              none.
!***  1. determine nonlinear transfer.
!     --------------------------------
      implicit none
      real, intent(in) :: xi, xj, xk, xij, xik, xjk, xoi, xoj, xok
      real :: ri, rj, rk, oi, oj, ok, sqijk, sqikj, sqjki
      ri=abs(xi)+del1
      rj=abs(xj)+del1
      rk=abs(xk)+del1
      oi=xoi+del1
      oj=xoj+del1
      ok=xok+del1
      sqijk=sqrt(oi*oj*rk/(ok*ri*rj))
      sqikj=sqrt(oi*ok*rj/(oj*ri*rk))
      sqjki=sqrt(oj*ok*ri/(oi*rj*rk))
      vplus_d=zconst*( (xij+ri*rj)*sqijk + (xik+ri*rk)*sqikj            &
     &               + (xjk+rj*rk)*sqjki )
      end function vplus_d
!     -----------------------------------------------------------------
      end subroutine secondhh
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief  determines skewness paramters in order to obtain
  !>         correction on altimeter wave height
  !>
  !> @details evaluate deviations from gaussianity following the work 
  !>          of srokosz and longuet-higgins. for second order
  !>          corrections to surface elevation, the approach of
  !>          zaharov has been used.
  !>
  !> @param[in]    nkhf   extended number of frequencies.
  !> @param[out]   fac0   2nd order coef correction.
  !> @param[out]   fac1   2nd order coef correction.
  !> @param[out]   fac2   2nd order coef correction.
  !> @param[out]   fac3   2nd order coef correction.
  !>
  !> @author p. janssen  @date 29-mar-2024
  !>
      subroutine skewness(a)
!--------------------------------------------------------------------
!*****skewness** computes parameters of the nearly-gaussian
!             distribution of ocean waves at a fixed grid point.
!     p.janssen july 1997
!     purpose
!     -------
!             determines skewness parameters in order to obtain
!             correction on altimeter wave height.
!     interface
!     ---------
!             *call* *skewness(iu06,f1,ncoll,xkappa1,delh_alt)*
!     method
!     ------
!             evaluate deviations from gaussianity following the work
!             of srokosz and longuet-higgins. for second order
!             corrections to surface elevation the approach of
!             zakharov has been used.
!     externals
!     ---------
!             none
!     references
!     ----------
!             m.a. srokosz, j.g.r.,91,995-1006(1986)
!             v.e. zakharov, hamiltonian approach(1967)
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!      *th*        real      directions in radians.
use constants, only: grav, tpi, tpiinv
use w3gdatmd,  only: nk, nth, xfr, sig, dth, ecos, esin, nseal
use w3parall,  only: init_get_isea
use w3adatmd,  only: cg, skew, embia1, embia2
    implicit none
    real, intent(in)        :: a(nth,nk,0:nseal)
    integer :: nkhf
    real(kind=4), dimension(:,:,:,:) , allocatable:: fac0,fac1,fac2,fac3
    integer :: m, k, m1, k1, m2, k2, i, j
    integer :: mstart, jsea
   
    real(kind=4) :: conx, delta
    real(kind=4) :: fh, delf, xk1
    real(kind=4) :: xpi, xpj, xpk, xn, xfac, co1
    real(kind=4), dimension(:,:), allocatable :: f2
    real(kind=4), dimension(0:3,0:2,0:2) :: xmu, xlambda
    real(kind=4), dimension(:) , allocatable::  sighf, dfimhf, fak
! ----------------------------------------------------------------------
    nkhf=nk+13 ! same offset as in ecwam 
    allocate(fac0(nth,nth,nkhf,nkhf))
    allocate(fac1(nth,nth,nkhf,nkhf))
    allocate(fac2(nth,nth,nkhf,nkhf))
    allocate(fac3(nth,nth,nkhf,nkhf))
      
    call secondhh(nkhf,fac0,fac1,fac2,fac3)
    allocate(f2(nth,nkhf))
    allocate(sighf(nkhf), dfimhf(nkhf), fak(nkhf)) 
!     1. computation of frequency-direction increment
!     -----------------------------------------------
    mstart = 1
    do jsea=1, nseal
      xmu(:,:,:) = 0.0
      do k=1,nth
        do m=1,nk
          conx = tpiinv / sig(m) * cg(m,jsea)
          f2(k,m)=a(k,m,jsea)/ conx
          end do
        end do
      sighf(1)  = sig(1)
      do m=2,nkhf
        sighf(m) = xfr*sighf(m-1)
      enddo
      co1 = 0.5*(xfr-1.)*dth*tpiinv 
      dfimhf(1) = co1*sighf(1)        ! this is df*dth
      do m=2,nkhf-1
        dfimhf(m)=co1*(sighf(m)+sighf(m-1))
      enddo
      dfimhf(nkhf)=co1*sighf(nkhf-1)
      do m=1,nkhf
        fak(m) = (sighf(m))**2/grav
      enddo
! deals with the tail ... 
      do m=nk+1,nkhf
        fh=(sighf(nk)/sighf(m))**5
        do k=1,nth
          f2(k,m)=f2(k,nk)*fh
        enddo
      enddo
!     2. computation of the skewness coefficients
!     --------------------------------------------
      do m1=mstart,nkhf
        do m2=mstart,nkhf
          do k1=1,nth
            do k2=1,nth
              delf = dfimhf(m1)*dfimhf(m2)*f2( k1,m1)*f2(k2,m2)
              xmu(3,0,0) = xmu(3,0,0)+3.0*fac0(k1,k2,m1,m2)*delf
              xmu(1,2,0) = xmu(1,2,0)+fac1(k1,k2,m1,m2)*delf
              xmu(1,0,2) = xmu(1,0,2)+fac2(k1,k2,m1,m2)*delf
              xmu(1,1,1) = xmu(1,1,1)+fac3(k1,k2,m1,m2)*delf
            enddo
          enddo
        enddo
      enddo
      do k1=1,nth
        do m1=mstart,nkhf
          xk1 = fak(m1)**2
          delf = dfimhf(m1)*f2(k1,m1)
          xmu(2,0,0) = xmu(2,0,0) + delf
          xmu(0,2,0) = xmu(0,2,0) + xk1*ecos(k1)**2*delf
          xmu(0,0,2) = xmu(0,0,2) + xk1*esin(k1)**2*delf
          xmu(0,1,1) = xmu(0,1,1) + xk1*ecos(k1)*esin(k1)*delf
        enddo
      enddo
!     3. computation of the normalised skewness coefficients
!     ------------------------------------------------------
      do i=0,3
        xpi = 0.5*float(i)
        do j=0,2
          xpj = 0.5*float(j)
          do k=0,2
            xpk = 0.5*float(k)
            xn = xmu(2,0,0)**xpi*xmu(0,2,0)**xpj*xmu(0,0,2)**xpk  ! denom in srokosz eq. 11
            if (xn .ne. 0) then
              xlambda(i,j,k) = xmu(i,j,k)/xn
            else
              xlambda(i,j,k) = 0
            end if
          end do
        end do
      end do
      if ( xmu(2,0,0) .gt. 1.e-7 ) then
        skew(jsea)=xlambda(3,0,0)
        delta = ( xlambda(1,2,0) + xlambda(1,0,2)           &
                  - 2.0*xlambda(0,1,1)*xlambda(1,1,1) )/    &
                   (1.0 - xlambda(0,1,1)**2)             ! this is called gamma eq. 20 
        embia1(jsea)=-0.125*delta                             ! em bias coefficient 
        embia2(jsea)=-0.125*xlambda(3,0,0)/3.0           ! tracker bias (least squares only) 	
      end if
    end do  ! end of loop on jsea
        !
    deallocate(fac0,fac1,fac2,fac3)
    deallocate(f2,sighf,dfimhf,fak) 
      end subroutine skewness
end module w3iogomd
