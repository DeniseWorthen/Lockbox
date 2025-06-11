module alarmconfig

  use NUOPC
  use ESMF

  character(len=*),parameter :: u_FILE_u = &
       __FILE__

contains

  subroutine readconfig(gcomp, fname, alarmcnt, restart_n, restart_fh, rc)

    type(ESMF_GridComp), intent(inout) :: gcomp
    character(len=*), intent(in) :: fname
    integer, intent(out)         :: restart_n
    integer, intent(out)         :: restart_fh(:)
    integer, intent(out)         :: alarmcnt
    integer, intent(inout)       :: rc

    type(ESMF_Config)   :: cf      ! the Config itself

    integer, parameter   :: alarmmax = 200
    integer :: fhcount
    integer :: alarmfreq(alarmmax)
    integer :: n, nn, alarm_n

    rc = ESMF_SUCCESS

    cf = ESMF_ConfigCreate(rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ConfigLoadFile(cf, trim(fname), rc=rc) ! Load the Resource File  into the empty Config
    if (rc .ne. ESMF_SUCCESS) print *,'failed'
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridCompSet(gcomp, config=cf, rc=RC)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    alarmfreq = -99
    call NUOPC_CompAttributeGet(gcomp, name='restart_n', valuelist=alarmfreq, itemCount=alarmcnt, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    print *,alarmfreq
    print *,alarmcnt
    multi_restart_n(1:alarmcnt) = alarmfreq(1:alarmcnt)

#ifdef test
    !call ESMF_ConfigFindLabel(cf, 'restart_n:', rc=rc) ! Step a) Find the  label
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! read through and find the alarmcount
    alarmcnt = 0
    do n = 1,alarmmax
       call ESMF_ConfigGetAttribute(cf, alarm_n, default=-99, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (alarm_n /= -99)alarmcnt = alarmcnt + 1
    end do
    if (alarmcnt > alarmmax) then
       rc = ESMF_FAILURE
       print *,' too many alarms'
       return
    endif
    print *,alarmcnt

    call ESMF_ConfigFindLabel(cf, 'restart_n:', rc=rc) ! Go back to label
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_ConfigGetAttribute(cf, multi_restart_n, default=-99, rc=rc)
#endif
    print *,multi_restart_n

  end subroutine readconfig

  logical function ChkErr(rc, line, file)
    integer, intent(in) :: rc            !< return code to check
    integer, intent(in) :: line          !< Integer source line number
    character(len=*), intent(in) :: file !< User-provided source file name
    integer :: lrc
    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       ChkErr = .true.
    endif
  end function ChkErr

end module alarmconfig
