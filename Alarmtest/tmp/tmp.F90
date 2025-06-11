program alarmlist

  use ESMF
  use alarmconfig
  use med_time_mod, only : med_time_alarmInit

  implicit none

  type(ESMF_VM)    :: vm
  type(ESMF_GridComp) :: gcomp
  type(ESMF_Clock) :: mclock
  type(ESMF_TimeInterval) :: mtimeStep,ltimeStep,alarmInterval
  type(ESMF_Time) :: mstartTime, mstopTime, mcurrTime, alarmTime
  type(ESMF_Alarm) :: alarm
  type(ESMF_Alarm),allocatable :: alarms(:)

  integer, parameter  :: alarmmax = 200
  integer, dimension(3) :: multi_restart_n
  integer :: alarmcnt
  integer :: ringingAlarmCount
  integer                 :: timestep_length
  character(len=40) :: fname = 'alarm.rc'
  character(len=40) :: alarmname
  character(len=3)  :: cvalue
  character(len=40) :: cstring
  integer :: rc
  integer :: n

  rc = ESMF_SUCCESS

  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN,rc=rc)
  call ESMF_VMGetGlobal(vm, rc=rc)               ! Establish the VM

  gcomp = ESMF_GridCompCreate(name='test',rc=rc)

#ifdef test
  alarmcnt=2
  allocate(alarms(alarmcnt))

  call ESMF_TimeIntervalSet(mtimeStep, d=1, rc=rc)
  call ESMF_TimeSet(mstartTime, yy=2003, mm=9, dd=1, rc=rc)
  call ESMF_TimeSet(mstopTime, yy=2003, mm=9, dd=30, rc=rc)
  mclock = ESMF_ClockCreate(mtimeStep, mstartTime, stopTime=mstopTime, &
       name="The Clock", rc=rc)

  call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=15, rc=rc)
  alarms(1) = ESMF_AlarmCreate(mclock, &
       ringTime=alarmTime, name="Example alarm 1", rc=rc)

  call ESMF_TimeSet(alarmTime, yy=2003, mm=9, dd=1, rc=rc)
  call ESMF_TimeIntervalSet(alarmInterval, d=7, rc=rc)
  alarms(2) = ESMF_AlarmCreate(clock=mclock, ringTime=alarmTime, &
       ringInterval=alarmInterval, rc=rc)
#endif
!#ifdef test
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

  !call readconfig(gcomp, trim(fname), alarmcnt, multi_restart_n,rc)
  alarmcnt=3
  allocate(alarms(alarmcnt))
  multi_restart_n = (/3,6,9/)

  if (alarmcnt > 1) then
     allocate(alarms(1:alarmcnt))
     do n = 1,alarmcnt
        write(cvalue,'(i3.3)')multi_restart_n(n)
        alarmname = 'alarm_restart'//trim(cvalue)
       ! set an alarm at a specific time
       call ESMF_TimeIntervalSet(AlarmTimeStep, h=multi_restart_n(n), rc=rc)
       !if (ChkErr(rc,__LINE__,u_FILE_u)) return
       ! advance the clock to get the ring time
       call ESMF_ClockAdvance(mclock, timestep=AlarmTimeStep, rc=rc)
       !if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_ClockGet(mclock, currTime=alarmTime, rc=rc)
       !if (ChkErr(rc,__LINE__,u_FILE_u)) return

       call ESMF_ClockPrint(mclock, options="currTime", unit=cvalue, rc=rc)
       write(msg,'(a,i6)')trim(subname)//": currtime at restart_fh alarm  = "//trim(cvalue),restart_fh
       call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

       ! create the alarm
       alarm = ESMF_AlarmCreate(name=alarmname, clock=mclock, ringTime=alarmTime, rc=rc)
       !if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_AlarmPrint(alarm, options="ringbegin",rc=rc)
       !if (ChkErr(rc,__LINE__,u_FILE_u)) return
       ! set the clock back to the current time
       call ESMF_ClockSet(mclock, currTime=mcurrTime, rc=rc)
       !if (ChkErr(rc,__LINE__,u_FILE_u)) return


        !call ESMF_TimeIntervalSet(ltimeStep, h=multi_restart_n(n), rc=rc)
        !call ESMF_ClockAdvance(mclock,timestep=ltimestep,rc=rc)
        !call ESMF_ClockGet(mclock, currTime=alarmTime, rc=rc)
        !alarms(n) = ESMF_AlarmCreate(mclock, ringTime=alarmTime, name=trim(alarmname), rc=rc)
        !call ESMF_ClockSet(mclock, currTime=mcurrtime, rc=rc)
        !call ESMF_TimeSet(alarmTime, h=multi_restart_n(n), rc=rc)

        !call med_time_alarmInit(mclock, alarms(n), option='hours', opt_n=multi_restart_n(n), &
        !     reftime=mcurrTime, alarmname=trim(alarmname),alarmtype='set-time',rc=rc)
        !call ESMF_AlarmSet(alarms(n), clock=mclock, rc=rc)
        !call ESMF_TimeIntervalSet(ltimeStep, h=multi_restart_n(n), rc=rc)
        !call ESMF_ClockAdvance(mclock,timestep=ltimestep,rc=rc)
        !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))print *,'alarm is ringing 0'
        !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))call ESMF_AlarmRingerOff( alarms(n), rc=rc )
        !call ESMF_ClockSet(mclock, currTime=mcurrtime, rc=rc)
     end do
        !call ESMF_ClockSet(mclock, currTime=mcurrtime, rc=rc)
  !else
  !   call med_time_alarmInit(mclock, alarm, option='hours', opt_n=6, &
  !        reftime=mcurrTime, alarmname='alarm_restart', alarmtype='interval',rc=rc)
  !   call ESMF_AlarmSet(alarm, clock=mclock, rc=rc)
  end if

  !do n = 1,alarmcnt
     !call ESMF_ClockAdvance(mclock,rc=rc)
     !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))print *,'alarm is ringing 0'
     !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))call ESMF_AlarmRingerOff( alarms(n), rc=rc )
     !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))print *,'alarm is ringing 1'
     !call ESMF_ClockSet(mclock, currTime=mcurrtime, rc=rc)
  !end do

  ! Advance model clock to trigger alarm then reset model clock back to currtime
  !call ESMF_ClockGet(mclock, currTime=mCurrTime, timeStep=mtimestep, rc=rc)
  !if (ChkErr(rc,__LINE__,u_FILE_u)) return
  !call ESMF_TimeIntervalGet(mtimestep, s=timestep_length, rc=rc)
  !if (ChkErr(rc,__LINE__,u_FILE_u)) return
  !call ESMF_ClockAdvance(mclock,rc=rc)
  !if (ChkErr(rc,__LINE__,u_FILE_u)) return
  !call ESMF_ClockSet(mclock, currTime=mcurrtime, rc=rc)
  !if (ChkErr(rc,__LINE__,u_FILE_u)) return

  !do n = 1,alarmcnt
  !   call ESMF_ClockAdvance(mclock,rc=rc)
  !   if (ESMF_AlarmIsRinging(alarms(n), rc=rc))print *,'alarm is ringing 2'
     !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))call ESMF_AlarmRingerOff( alarms(n), rc=rc )
     !if (ESMF_AlarmIsRinging(alarms(n), rc=rc))print *,'alarm is ringing 1'
   !  call ESMF_ClockSet(mclock, currTime=mcurrtime, rc=rc)
  !end do
!#endif
#ifdef test
  call ESMF_ClockGetAlarmList(mclock, alarmlistflag=ESMF_ALARMLIST_RINGING, alarmCount=ringingalarmcount, rc=rc)
  print *,'ringing alarms ',ringingalarmcount
  do n = 1,ringingalarmcount
     if (ESMF_AlarmIsRinging(alarms(n), rc=rc)) then
        call ESMF_AlarmGet(alarms(n), name=alarmname, rc=rc)
        call ESMF_ClockGet(mclock, currTime=mcurrTime, rc=rc)
        call ESMF_ClockPrint(mclock, options="currTime", unit=cstring,rc=rc)
        print *, trim(alarmname), " is ringing! at "//trim(cstring)
        ! after processing alarm, turn it off
        call ESMF_AlarmRingerOff(alarms(n), rc=rc)
     end if
  end do
!#ifdef test
  ! time step clock from start time to stop time
  do while (.not.ESMF_ClockIsStopTime(mclock, rc=rc))
     ! perform time step and get the number of any ringing alarms
     call ESMF_ClockAdvance(mclock, ringingAlarmCount=ringingAlarmCount, rc=rc)

     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_ClockPrint(mclock, options="currTime", unit=cstring,rc=rc)
     print *,trim(cstring)

     if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     ! check if alarms are ringing
     if (ringingAlarmCount > 0) then
        !print *, "number of ringing alarms = ", ringingAlarmCount

        do n = 1,alarmcnt
           if (ESMF_AlarmIsRinging(alarms(n), rc=rc)) then

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
              call ESMF_ClockGet(mclock, currTime=mcurrTime, rc=rc)
              call ESMF_ClockPrint(mclock, options="currTime", unit=cstring,rc=rc)
              call ESMF_AlarmGet(alarms(n), name=alarmname, rc=rc)
              print *, trim(alarmname), " is ringing! at "//trim(cstring)

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

              ! after processing alarm, turn it off
              call ESMF_AlarmRingerOff(alarms(n), rc=rc)

              if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if ! this alarm is ringing
        end do ! each ringing alarm
     endif ! ringing alarms
  end do ! timestep clock
#endif

end program alarmlist
