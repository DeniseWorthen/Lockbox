

  ! set up alarm at 6h, starting at 6h+startime

  !in alarm init
  ! Create history clock from model clock - THIS CALL DOES NOT COPY ALARMS
  hclock = ESMF_ClockCreate(mclock, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  call ESMF_TimeIntervalSet(outputInterval, h=6+1, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  call alarmInit(hclock, history_alarm, &
       option='nhours', &
       opt_n=6, &
       reftime=StartTime+outputInterval, &
       alarmname=trim('history_alarm'), &
       advance_clock=.true., rc=rc)
  ! start at h=7, ring at 13,19,1,
  ! output interval=6, creates alarm at 12,18,24,6,12,18,24

  ! in model advance
  willRing=ESMF_AlarmWillRingNext(history_alarm, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return
  ! will ring at currtime=12,nexttime=13
  ! cur=18, next=19

  if (willRing) then
     fname = currenttime - 9
     ! fname = 03, 15, 21 etc
     ! check nlen>0
  end if


  ! if want
  ! daily output
  ! h=24+1
  ! 6h_alarm, 24h_alarm
  ! output6h_alarm
  ! output24h_alarm
  ! config output6h=true, output24h=true in mom configs
  ! if (output6h = true) outputInterval = 6+1
  ! if (output24h = true) outputInterval = 24+1

  ! could hardwire it to 6h and daily output; 2 output freq
  type, private :: outputfile_type
     integer          :: hist_n
     type(ESMF_Clock) :: clock
     type(ESMF_Alarm) :: alarm
     character(CS)    :: alarmname
     type(ESMF_TimeInterval) :: outputinterval
     type(ESMF_Time)  :: outputStartTime
     logical          :: is_clockset = .false.
     logical          :: is_active = .false.
  end type outputfile_type
  type(outputfile_type) , allocatable, public :: outfiles(:)

  ! config output6h=true, output24h=true in mom configs
  ! noutput_freq = 0
  ! if (output6h = true) outputInterval = 6+1; noutput_freq=noutput_freq+1
  ! if (output24h = true) outputInterval = 24+1;  noutput_freq=noutput_freq+1

  allocate(outfiles(1:noutput_freq))
  do n = 1,noutput_freq
     if (output6h = true) then
        !set output interval = 6+1
        outputStartTime = startime + outputInterval

     if (.not. outfiles(n)%is_clockset) then
        ! set the alarm and initialize clock
        call init_output_clock(outfiles(n)%clock, outfiles(n)%alarm, outfiles(n)%alarmname, &
             outputfiles(n)%outputinterval, outputfiles(n)%outputStartTime, rc=rc)
        ! set the clock active, is active
