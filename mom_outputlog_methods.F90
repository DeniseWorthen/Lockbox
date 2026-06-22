
  !> Determine if the netcdf output file is complete
  !!
  !! @param[in]   fname         the file name
  !! @param[in]   chk4size      logical flag for check method in use
  !! @param[in]   createsize    the filesize at creation
  !! @param[out]  rc            return code
  !! @return                    logical flag, true if the file is complete
  logical function file_is_complete(fname, chk4size, createsize, rc) result(filecomplete)

    character(len=*), intent(in)  :: fname
    logical,          intent(in)  :: chk4size
    integer,          intent(in)  :: createsize
    integer,          intent(out) :: rc

    integer :: nlen(1), fsize(1)
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    filecomplete = .false.
    nlen(1) = nf90_fill_int
    fsize(1) = nf90_fill_int

    inquire(file=fname, exist=existflag)
    if (existflag) then
      if (is_root_pe()) then
        nlen(1) = get_unlimited_len(fname)
        inquire(file=fname, size=fsize(1))
      end if
      call ESMF_VMBroadCast(vm, nlen, 1, 0, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_VMBroadCast(vm, fsize, 1, 0, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    if (chk4size) then
      filecomplete = (nlen(1) > 0 .and. fsize(1) > createsize)
    else
      filecomplete = (nlen(1) > 0)
    end if
  end function file_is_complete

  !> Return the length of the unlimited dimension
  !!
  !! @param[in]  fname   the file name
  !! @return             unlimited dimension length
  integer function get_unlimited_len(fname) result(unlen)

    character(len=*), intent(in) :: fname

    integer :: ncid, dimid
    !----------------------------------------------------------------------------

    unlen = 0
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'nf90_open: '//trim(fname))
    call nf90_err(nf90_inquire(ncid, unlimiteddimid=dimid), 'inquire unlimiteddimid')
    call nf90_err(nf90_inquire_dimension(ncid, dimid, len=unlen), 'inquire unlimited dimension')
    call nf90_err(nf90_close(ncid), 'close: '//trim(fname))
  end function get_unlimited_len

  !> Convenience function to return a 16-character time string
  !!
  !! @param[in]  MyTime   an ESMF_Time object
  !! @param[out] rc       return code
  !! @return              16-character formatted time string (YYYY_MM_DD_HH_MM)
  character(len=16) function get_timestr(MyTime, rc) result(timestr)

    type(ESMF_Time), intent(in)  :: MyTime
    integer,         intent(out) :: rc

    integer :: year, month, day, hour, minute
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TimeGet(MyTime, yy=year, mm=month, dd=day, h=hour, m=minute, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(timestr,'(I4.4,4(A,I2.2))')year,'_',month,'_',day,'_',hour,'_',minute
  end function get_timestr

  !> Convenience function to return import/export timestring
  !!
  !! @param[in]  currTime   an ESMF_Time object
  !! @param[in]  nextTime   an ESMF_Time object
  !! @param[out] rc         return code
  !! @return                40-character string
  character(len=40) function get_importexport(currTime, nextTime, rc) result(importexport)

    type(ESMF_Time), intent(in)  :: currTime, nextTime
    integer,         intent(out) :: rc

    character(len=19) :: import_timestr, export_timestr
    !----------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_TimeGet(currTime, timestring=import_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_TimeGet(nextTime, timestring=export_timestr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    importexport = trim(import_timestr)//'  '//trim(export_timestr)
  end function get_importexport

  !> Write debug info to stdout, only called on root pe
  !!
  !! @param[in]    tag            an information tag
  !! @param[in]    fname          the filename to check
  !! @param[in]    filesize       the filesize at creation time
  !! @param[in]    chkflag        logical flag for checking next Advance
  !! @param[in]    timestring     a timestring
  subroutine debug_info(tag,fname,chkflag,filesize,timestring)

    character(len=*), intent(in) :: tag
    character(len=*), intent(in) :: fname
    integer,          intent(in) :: filesize
    logical,          intent(in) :: chkflag
    character(len=*), intent(in) :: timestring

    integer :: fsize
    character(len=256) :: msgString
    !----------------------------------------------------------------------------

    inquire(file=fname, exist=existflag)
    if (existflag) then
      inquire(file=fname, size=fsize)
      write(msgString,'(A)')tag//'  '//fname//' exists '//timestring
      if (chkflag) then
        print '(A,L,2i16)',trim(msgString)//' not complete, chkflag ',chkflag,filesize,fsize
      else
        print '(A,L,2i16)',trim(msgString)//'     complete, chkflag ',chkflag,filesize,fsize
      end if
    else
      write(msgString,'(A)')tag//'  '//fname//' does not exist '//timestring
      print '(A)',trim(msgString)
    end if
  end subroutine debug_info

  !> Handle netcdf errors
  !!
  !! @param[in]  ierr        the error code
  !! @param[in]  string      the error message
  subroutine nf90_err(ierr, string)

    integer,          intent(in) :: ierr
    character(len=*), intent(in) :: string
    !----------------------------------------------------------------------------

    if (ierr /= nf90_noerr) then
      write(0, '(A)') 'FATAL ERROR: ' // trim(string)// ' : ' // trim(nf90_strerror(ierr))
      ! This fails on WCOSS2 with Intel 19 compiler. See https://community.intel.com/
      ! Search term "STOP and ERROR STOP with variable stop codes"
      ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
      !stop ierr
      stop 99
    end if
  end subroutine nf90_err
