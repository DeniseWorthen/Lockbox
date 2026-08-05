program test_outputlog_methods

  use ESMF
  use mom_outputlog_methods, only : outputlog_state_type, check_completion

  implicit none

  integer :: total_errors = 0
  logical :: verbose = .true.
  integer :: rc

  type(ESMF_Calendar) :: calendar
  type(ESMF_Time)     :: zeroTime   ! reference epoch (base_yy/base_mm/base_dd, 00Z) all "elapsed hours" are measured from

  ! Base calendar date used purely so filenames match the original "2021_03_DD" convention.
  ! Change this if the feature you're testing cares about a different reference date.
  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22

  integer :: nt, testdt, testfrq, teststart, testhours, testfilecnt
  logical :: test_nleninit

  character(len= 32) :: testtype
  character(len=256) :: testmsg

  nt = 0

  ! --- Real ESMF init (was previously not needed at all since everything was mocked) ---
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (rc /= ESMF_SUCCESS) then
     print *, "ERROR: ESMF_Initialize failed, rc=", rc
     stop 1
  end if

  calendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, name="test_calendar", rc=rc)
  call check_rc(rc, "ESMF_CalendarCreate")

  call ESMF_TimeSet(zeroTime, yy=base_yy, mm=base_mm, dd=base_dd, h=0, calendar=calendar, rc=rc)
  call check_rc(rc, "ESMF_TimeSet(zeroTime)")

  print *, "========================================================"
  print *, " Starting Generalized Outputlog Test Suite"
  print *, "========================================================"

  ! Test: 6-hourly average, dt=720, start=06, run=24h
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 24; testdt = 720
  testtype = 'average'; test_nleninit = .false.

  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)
  call run_test(trim(testmsg),               &
       dt                   = testdt,        &
       freq                 = testfrq,       &
       file_type            = testtype,      &
       init_nlen_zero       = test_nleninit, &
       start_hour           = teststart,     &
       run_hours            = testhours,     &
       expected_completions = testfilecnt,   &
       err_count            = total_errors)

  ! Test: 1-hourly snapshot, dt=1800 (30 mins), start=00Z, run=6h
  nt = nt + 1
  testfrq = 1; teststart=0; testhours = 6; testdt = 1800
  testtype = 'snapshot'; test_nleninit = .true.

  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)
  call run_test(trim(testmsg),               &
       dt                   = testdt,        &
       freq                 = testfrq,       &
       file_type            = testtype,      &
       init_nlen_zero       = test_nleninit, &
       start_hour           = teststart,     &
       run_hours            = testhours,     &
       expected_completions = testfilecnt,   &
       err_count            = total_errors)

  ! Test: 3-hourly snapshot, dt=3600 (1 hour), start=12Z, run=12h
  nt = nt + 1
  testfrq = 3; teststart=12; testhours = 12; testdt = 3600
  testtype = 'snapshot'; test_nleninit = .true.

  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)
  call run_test(trim(testmsg),               &
       dt                   = testdt,        &
       freq                 = testfrq,       &
       file_type            = testtype,      &
       init_nlen_zero       = test_nleninit, &
       start_hour           = teststart,     &
       run_hours            = testhours,     &
       expected_completions = testfilecnt,   &
       err_count            = total_errors)

  ! Test: 24-hourly average, dt=7200 (2 hours), start=00Z, run=48h
  nt = nt + 1
  testfrq = 24; teststart=0; testhours = 48; testdt = 7200
  testtype = 'average'; test_nleninit = .false.

  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)
  call run_test(trim(testmsg),               &
       dt                   = testdt,        &
       freq                 = testfrq,       &
       file_type            = testtype,      &
       init_nlen_zero       = test_nleninit, &
       start_hour           = teststart,     &
       run_hours            = testhours,     &
       expected_completions = testfilecnt,   &
       err_count            = total_errors)

  ! Test: 6-hourly average, dt=720 (12 mins), start=06, run=24h (End-of-period naming)
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 24; testdt = 720
  testtype = 'average_end_named'; test_nleninit = .false.

  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)
  call run_test(trim(testmsg),               &
       dt                   = testdt,        &
       freq                 = testfrq,       &
       file_type            = testtype,      &
       init_nlen_zero       = test_nleninit, &
       start_hour           = teststart,     &
       run_hours            = testhours,     &
       expected_completions = testfilecnt,   &
       err_count            = total_errors)

  ! Test: 24-hourly average, dt=7200 (2 hours), start=00Z, run=54h
  nt = nt + 1
  testfrq = 24; teststart=0; testhours = 54; testdt = 7200
  testtype = 'average'; test_nleninit = .false.; testfilecnt = testhours/testfrq

  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt)
  call run_test(trim(testmsg),               &
       dt                   = testdt,        &
       freq                 = testfrq,       &
       file_type            = testtype,      &
       init_nlen_zero       = test_nleninit, &
       start_hour           = teststart,     &
       run_hours            = testhours,     &
       expected_completions = testfilecnt,   &
       err_count            = total_errors)

  print *, "========================================================"

  call ESMF_CalendarDestroy(calendar, rc=rc)
  call check_rc(rc, "ESMF_CalendarDestroy")

  if (total_errors == 0) then
     print *, "SUCCESS: All test cases passed with zero errors!"
     call ESMF_Finalize(rc=rc)
     stop 0
  else
     print *, "FAILURE: ", total_errors, " assertions failed."
     call ESMF_Finalize(rc=rc)
     stop 1
  end if

contains

  !> A generalized routine to run a specific simulation configuration, now driven
  !> by a real ESMF_Clock/ESMF_Alarm pair instead of manually mocked ring logic.
  subroutine run_test(test_name, dt, freq, file_type, init_nlen_zero, start_hour, run_hours, expected_completions, err_count)

    character(len=*), intent(in)    :: test_name
    integer,          intent(in)    :: dt
    integer,          intent(in)    :: freq
    character(len=*), intent(in)    :: file_type
    logical,          intent(in)    :: init_nlen_zero
    integer,          intent(in)    :: start_hour
    integer,          intent(in)    :: run_hours
    integer,          intent(in)    :: expected_completions
    integer,          intent(inout) :: err_count

    type(outputlog_state_type) :: tracker

    ! --- Real ESMF time-management objects (replace the old mocked current_time/alarm_time ints) ---
    type(ESMF_Clock)        :: clock
    type(ESMF_Alarm)        :: alarm
    type(ESMF_Time)         :: startTime, stopTime, currTime, prevTime, fileTime, ringTime
    type(ESMF_TimeInterval) :: timeStep, ringInterval, runDuration, offsetInterval, elapsedInterval

    integer            :: alarm_hour
    integer             :: elapsed_secs, offset_hours
    integer            :: file_day, file_hour
    integer            :: size, nlen
    logical            :: filecomplete, is_valid_file, ringing, atStopTime
    character(len=5)   :: curr_hm, next_hm
    character(len=256) :: filename, timestring
    integer            :: ierr, lrc
    integer            :: num_completions

    ierr = 0
    num_completions = 0
    is_valid_file = .false.

    ! Reset the tracker state for a fresh test run
    tracker%createsize = 0
    tracker%chkfile_nextAdvance = .false.
    tracker%isringing = .false.
    tracker%atstop = .false.
    tracker%filename = ""

    ! --- A. Build the real ESMF Clock and Alarm for this test configuration ---

    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, &
         calendar=calendar, rc=lrc)
    call check_rc(lrc, "ESMF_TimeSet(startTime)")

    call ESMF_TimeIntervalSet(timeStep, s=dt, rc=lrc)
    call check_rc(lrc, "ESMF_TimeIntervalSet(timeStep)")

    call ESMF_TimeIntervalSet(runDuration, h=run_hours, rc=lrc)
    call check_rc(lrc, "ESMF_TimeIntervalSet(runDuration)")
    stopTime = startTime + runDuration

    clock = ESMF_ClockCreate(name="test_clock", timeStep=timeStep, &
         startTime=startTime, stopTime=stopTime, rc=lrc)
    call check_rc(lrc, "ESMF_ClockCreate")

    ! FMS offset: the first alarm rings one full frequency block after the start
    call ESMF_TimeIntervalSet(ringInterval, h=freq, rc=lrc)
    call check_rc(lrc, "ESMF_TimeIntervalSet(ringInterval)")
    ringTime = startTime + ringInterval

    ! sticky=.false. so the alarm's ringing state is transient (one timestep), matching
    ! the original mock, which recomputed isringing fresh on every loop iteration.
    alarm = ESMF_AlarmCreate(clock=clock, name="test_alarm", ringTime=ringTime, &
         ringInterval=ringInterval, sticky=.false., rc=lrc)
    call check_rc(lrc, "ESMF_AlarmCreate")

    print *, ""
    print *, "--- ", trim(test_name), " ---"

    do while (.not. ESMF_ClockIsStopTime(clock, rc=lrc))

       call ESMF_ClockGet(clock, currTime=prevTime, rc=lrc)
       call check_rc(lrc, "ESMF_ClockGet(prevTime)")

       ! --- B. Advance the real clock; ESMF (not us) updates the alarm's ringing state ---
       call ESMF_ClockAdvance(clock, rc=lrc)
       call check_rc(lrc, "ESMF_ClockAdvance")

       call ESMF_ClockGet(clock, currTime=currTime, rc=lrc)
       call check_rc(lrc, "ESMF_ClockGet(currTime)")

       curr_hm = esmf_time_hm(prevTime)
       next_hm = esmf_time_hm(currTime)
       write(timestring,'(4(A,I2.2))') "Time: "//trim(curr_hm)//" -> "//trim(next_hm)

       ringing = ESMF_AlarmIsRinging(alarm, rc=lrc)
       call check_rc(lrc, "ESMF_AlarmIsRinging")

       if (ringing) then
          tracker%isringing = .true.

          ! elapsed hours (from the fixed zeroTime epoch) at which the alarm rang,
          ! same quantity the mock called "alarm_hour"
          elapsedInterval = currTime - zeroTime
          call ESMF_TimeIntervalGet(elapsedInterval, s=elapsed_secs, rc=lrc)
          call check_rc(lrc, "ESMF_TimeIntervalGet(elapsed)")
          alarm_hour = elapsed_secs / 3600

          ! 1. Offset (business logic, unrelated to ESMF -- unchanged from the mock)
          if (trim(file_type) == "average") then
             offset_hours = freq + (freq / 2)
          else if (trim(file_type) == "average_end_named" .or. trim(file_type) == "snapshot") then
             offset_hours = freq
          end if

          ! 2. Use real calendar arithmetic for the file's Day/Hour label instead of
          !    manual modulo math -- this correctly rolls into a new month if needed.
          call ESMF_TimeIntervalSet(offsetInterval, h=offset_hours, rc=lrc)
          call check_rc(lrc, "ESMF_TimeIntervalSet(offsetInterval)")
          fileTime = currTime - offsetInterval
          call ESMF_TimeGet(fileTime, dd=file_day, h=file_hour, rc=lrc)
          call check_rc(lrc, "ESMF_TimeGet(fileTime)")

          write(filename, '("./MOM6_OUTPUT/ocn_2021_03_", I2.2, "_", I2.2, "_00.nc")') file_day, file_hour
          tracker%filename = trim(filename)

          ! Validate if this file is a no-op (i.e., its interval began before start_time)
          if ((alarm_hour - (2 * freq)) >= start_hour) then
             is_valid_file = .true.
          else
             is_valid_file = .false.
          end if

          ! Non-sticky alarm: explicitly turn the ringer off now that we've handled it.
          call ESMF_AlarmRingerOff(alarm, rc=lrc)
          call check_rc(lrc, "ESMF_AlarmRingerOff")
       else
          tracker%isringing = .false.
          filename = trim(tracker%filename)
       end if

       ! Mock the file state based on ringing status and NO-OP status
       ! (this part stays mocked deliberately -- it's standing in for netCDF file
       !  inquiry, which is out of scope for this ESMF-focused test)
       if (.not. is_valid_file) then
          size = -2147483647
          nlen = -2147483647
       else
          if (tracker%isringing) then
             size = 199276
             if (init_nlen_zero) then
                nlen = 0
             else
                nlen = 1
             end if
          else
             size = 90532460
             nlen = 1
          end if
       end if

       ! --- C. Call the REAL Feature ---
       if (len_trim(tracker%filename) > 0) then
          call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
          if (verbose) call test_loginfo(timestring, filename, tracker%chkfile_nextAdvance, size, filecomplete)

          ! --- D. Generalized Dynamic Assertions ---
          if (tracker%isringing) then
             call assert_false(filecomplete, "Ringing: filecomplete must be false", ierr)
             call assert_true(tracker%chkfile_nextAdvance, "Ringing: chkfile_nextAdvance must trigger true", ierr)
          end if

          if (filecomplete) then
             call assert_false(tracker%chkfile_nextAdvance, "Complete: chkfile_nextAdvance must flip false", ierr)
             num_completions = num_completions + 1
          end if
       end if

    end do

    ! ==================================================================
    ! --- E. Simulate ocean_model_finalize (Double Call Sequence) ---
    ! ==================================================================

    if (verbose) print *, "--- Simulating ocean_model_finalize ---"

    ! Call 1: Standard outputlog_run equivalent (catches stranded file)
    tracker%isringing = .false.
    tracker%atstop = .false.
    write(timestring,'(A)') "Time: Finalize   "
    size = 90532460
    nlen = 1
    call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
    if (filecomplete) num_completions = num_completions + 1
    if (verbose) call test_loginfo(timestring, filename, tracker%chkfile_nextAdvance, size, filecomplete)

    ! Call 2: atStopTime = .true. equivalent (Forces final file)
    atStopTime = ESMF_ClockIsStopTime(clock, rc=lrc)
    call check_rc(lrc, "ESMF_ClockIsStopTime")

    tracker%isringing = .false.
    tracker%atstop = atStopTime
    tracker%chkfile_nextAdvance = .true.

    ! Finalize Math using real elapsed hours at the clock's stop time
    call ESMF_ClockGet(clock, currTime=currTime, rc=lrc)
    call check_rc(lrc, "ESMF_ClockGet(stopTime)")
    elapsedInterval = currTime - zeroTime
    call ESMF_TimeIntervalGet(elapsedInterval, s=elapsed_secs, rc=lrc)
    call check_rc(lrc, "ESMF_TimeIntervalGet(elapsed, finalize)")
    alarm_hour = elapsed_secs / 3600

    if (trim(file_type) == "average") then
       offset_hours = freq / 2
    else
       offset_hours = 0
    end if

    call ESMF_TimeIntervalSet(offsetInterval, h=offset_hours, rc=lrc)
    call check_rc(lrc, "ESMF_TimeIntervalSet(offsetInterval, finalize)")
    fileTime = currTime - offsetInterval
    call ESMF_TimeGet(fileTime, dd=file_day, h=file_hour, rc=lrc)
    call check_rc(lrc, "ESMF_TimeGet(fileTime, finalize)")

    write(filename, '("./MOM6_OUTPUT/ocn_2021_03_", I2.2, "_", I2.2, "_00.nc")') file_day, file_hour
    tracker%filename = trim(filename)

    write(timestring,'(A)') "Time: StopTime"
    call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
    if (verbose) call test_loginfo(timestring, filename, tracker%chkfile_nextAdvance, size, filecomplete)

    if (filecomplete) then
       num_completions = num_completions + 1
       call assert_true(filecomplete, "Finalize: atStopTime must trigger a final file completion", ierr)
    end if

    ! --- Final Macro Assertion ---
    call assert_equal(expected_completions, num_completions, "Total completed files must match run_hours / freq", ierr)

    if (ierr == 0) then
       print *, "  -> Passed. Expected number of files detected as complete"
    else
       print *, "  -> FAILED with ", ierr, " errors."
    end if

    call ESMF_AlarmDestroy(alarm, rc=lrc)
    call check_rc(lrc, "ESMF_AlarmDestroy")
    call ESMF_ClockDestroy(clock, rc=lrc)
    call check_rc(lrc, "ESMF_ClockDestroy")

    err_count = err_count + ierr
  end subroutine run_test

  ! --- Assertion Helpers (unchanged) ---
  subroutine assert_true(condition, msg, err_count)
    logical,          intent(in)    :: condition
    character(len=*), intent(in)    :: msg
    integer,          intent(inout) :: err_count
    if (.not. condition) then
       print *, "  -> ASSERTION FAILED: ", trim(msg)
       err_count = err_count + 1
    end if
  end subroutine assert_true

  subroutine assert_false(condition, msg, err_count)
    logical          , intent(in)    :: condition
    character(len=*) , intent(in)    :: msg
    integer          , intent(inout) :: err_count
    if (condition) then
       print *, "  -> ASSERTION FAILED: ", trim(msg)
       err_count = err_count + 1
    end if
  end subroutine assert_false

  subroutine assert_equal(expected, actual, msg, err_count)
    integer,          intent(in)    :: expected, actual
    character(len=*), intent(in)    :: msg
    integer,          intent(inout) :: err_count
    if (expected /= actual) then
       print *, "  -> ASSERTION FAILED: ", trim(msg), " (Expected: ", expected, ", Got: ", actual, ")"
       err_count = err_count + 1
    end if
  end subroutine assert_equal

  !> Format an ESMF_Time as HH:MM, replacing the old integer-seconds set_timestr()
  function esmf_time_hm(time) result(ctime)
    type(ESMF_Time), intent(in) :: time
    character(len=5)            :: ctime

    integer :: hour, minu, lrc

    call ESMF_TimeGet(time, h=hour, m=minu, rc=lrc)
    call check_rc(lrc, "ESMF_TimeGet(esmf_time_hm)")

    write(ctime,'(I2.2,A,I2.2)') hour,':',minu
  end function esmf_time_hm

  subroutine test_loginfo(timestr, fname, chknext, size, complete)

    character(len=*), intent(in) :: timestr
    character(len=*), intent(in) :: fname
    logical,          intent(in) :: chknext
    integer,          intent(in) :: size
    logical,          intent(in) :: complete

    print '(A, L1, A, I15, A, L1)', trim(timestr)//" | " //trim(fname)// " | chkflag: ", &
         chknext, " | size: ", size, " | complete: ", complete

  end subroutine test_loginfo

  !> Small helper so every ESMF call site doesn't need its own if-block
  subroutine check_rc(rc, context)
    integer,          intent(in) :: rc
    character(len=*), intent(in) :: context
    if (rc /= ESMF_SUCCESS) then
       print *, "ESMF ERROR in ", trim(context), ": rc=", rc
       call ESMF_Finalize(rc=rc)
       stop 1
    end if
  end subroutine check_rc

  function set_testmsg(num, dt, freq, start, hours, atype, nleninit0, nfiles) result(testmsg)

    integer,          intent(in) :: num, dt, freq, start, hours
    character(len=*), intent(in) :: atype
    logical,          intent(in) :: nleninit0
    integer,          intent(in) :: nfiles

    character(len=64)  :: cnum, cdt, cstart, chours, freqtype, initype, cfilecnt

    character(len=256) :: testmsg

    write(cnum, '(I2.2)') num
    write(cdt, '(I4.4,A)') dt, ' secs'
    write(cstart, '(I2.2,A)') start, 'Z'
    write(chours, '(I2.2,A)') hours, 'h'
    write(cfilecnt,'(I4.2,A)')nfiles,' expected file competions'

    if (trim(atype) == 'average' .or. trim(atype) == 'average_end_named') then
       write(freqtype,'(I2.2,A)') freq, 'h Avg'
    else
       write(freqtype,'(I2.2,A)') freq, 'h Inst'
    endif

    if (nleninit0) then
       write(initype,'(A)') 'unlimdim len = 0 at creation'
    else
       write(initype,'(A)') 'unlimdim len = 1 at creation'
    endif

    testmsg = 'Test ' // trim(cnum) //': ' // trim(freqtype)  &
         //',  start : ' // trim(cstart)                      &
         //', fhmax : ' // trim(chours)                       &
         //', dt : ' // trim(cdt)                             &
         //', ' // trim(initype)                              &
         //', ' // trim(cfilecnt)

  end function set_testmsg
end program test_outputlog_methods
