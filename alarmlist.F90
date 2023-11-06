program alarmlist

  use ESMF
  !use alarmconfig
  !use med_time_mod, only : med_time_alarmInit

  implicit none

  type(ESMF_VM)    :: vm
  type(ESMF_GridComp) :: gcomp
  type(ESMF_Clock) :: mclock
  type(ESMF_TimeInterval) :: mtimeStep,alarmtimeStep,alarmInterval
  type(ESMF_Time) :: mstartTime, mstopTime, mcurrTime, alarmTime
  type(ESMF_Alarm) :: alarm
  type(ESMF_Alarm),allocatable :: alarms(:)

  integer, parameter   :: alarmmax = 200
  integer, dimension(3) :: multi_restart_n
  integer :: alarmcnt
  integer :: ringingAlarmCount
  character(len=40) :: fname = 'alarm.rc'
  character(len=40) :: alarmname
  character(len=3)  :: cvalue
  character(len=40) :: cstring
  character(len=80) :: msg
  integer :: rc
  integer :: n

  rc = ESMF_SUCCESS

  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN,rc=rc)
  call ESMF_VMGetGlobal(vm, rc=rc)               ! Establish the VM

  !gcomp = ESMF_GridCompCreate(name='test',rc=rc)

  ! initialize time interval to 1 day
  call ESMF_TimeIntervalSet(mtimeStep, h=1, rc=rc)

  ! initialize start time to 9/1/2003
  call ESMF_TimeSet(mstartTime, yy=2003, mm=9, dd=1, rc=rc)

  ! initialize stop time to 9/30/2003
  call ESMF_TimeSet(mstopTime, yy=2003, mm=9, dd=3, rc=rc)
  ! create & initialize the clock with the above values
  mclock = ESMF_ClockCreate(mtimeStep, mstartTime, stopTime=mstopTime, name="The Clock", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_ClockGet(mclock, currTime=mcurrTime, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ClockPrint(mclock, options="currTime string", rc=rc)


  alarmcnt=3
  allocate(alarms(alarmcnt))
  multi_restart_n = (/3,6,9/)

  if (alarmcnt > 1) then
     do n = 1,alarmcnt
        write(cvalue,'(i3.3)')multi_restart_n(n)
        alarmname = 'alarm_restart'//trim(cvalue)
        ! set an alarm at a specific time
        call ESMF_TimeIntervalSet(AlarmTimeStep, h=multi_restart_n(n), rc=rc)
        !if (ChkErr(rc,__LINE__,u_FILE_u)) return
        print *,trim(alarmname)

        call ESMF_ClockPrint(mclock, options="currTime", unit=cstring, rc=rc)
        write(msg,'(a,i6)')"currtime b4 advance  = "//trim(cstring),multi_restart_n(n)
        !call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        print *,trim(msg)

        ! advance the clock to get the ring time
        call ESMF_ClockAdvance(mclock, timestep=AlarmTimeStep, rc=rc)
        !if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_ClockGet(mclock, currTime=alarmTime, rc=rc)
        !if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_ClockPrint(mclock, options="currTime", unit=cstring, rc=rc)
        write(msg,'(a,i6)')"currtime at restart_fh alarm  = "//trim(cstring),multi_restart_n(n)
        !call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        print *,trim(msg)

        ! create the alarm
        alarm = ESMF_AlarmCreate(name=alarmname, clock=mclock, ringTime=alarmTime, rc=rc)
        !if (ChkErr(rc,__LINE__,u_FILE_u)) return

        !call ESMF_AlarmPrint(alarm, options="ringbegin",rc=rc)
        !if (ChkErr(rc,__LINE__,u_FILE_u)) return

        ! set the clock back to the current time
        call ESMF_ClockSet(mclock, currTime=mcurrTime, rc=rc)
        call ESMF_ClockPrint(mclock, options="currTime", unit=cstring, rc=rc)
        write(msg,'(a,i6)')"currtime after reset  = "//trim(cstring),multi_restart_n(n)
        !call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        print *,trim(msg)

       !if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do
 end if

end program alarmlist
