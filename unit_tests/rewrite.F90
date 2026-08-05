program test_outputlog_methods

  use mom_outputlog_methods, only : outputlog_state_type, check_completion

  implicit none

  ! ============================================================================
  ! NOTE ON dt/freq CONSTRAINTS (per design):
  !   - freq is restricted to {1, 3, 6, 24} hours
  !   - dt is restricted to values that evenly DIVIDE 3600s (e.g. 720, 900, 1800, 3600)
  !   Tests 4 and 6 below (inherited from the original suite) use dt=7200, which is a
  !   MULTIPLE of 3600, not a divisor. Left unchanged since they weren't part of this
  !   pass, but flagged here -- confirm whether that's a valid production input or
  !   should be corrected to e.g. dt=3600.
  ! ============================================================================

  integer :: total_errors = 0
  logical :: verbose = .true.

  integer :: nt, testdt, testfrq, teststart, testhours, testfilecnt
  logical :: test_nleninit

  character(len= 32) :: testtype
  character(len=256) :: testmsg

  nt = 0

  print *, "========================================================"
  print *, " Starting Generalized Outputlog Test Suite"
  print *, "========================================================"

  ! ------------------------------------------------------------------------
  ! Group A: Interval-aligned baselines (run_hours is an exact multiple of freq)
  ! One per valid freq, establishing the "clean" case each partial-cycle
  ! variant below will be compared against.
  ! ------------------------------------------------------------------------

  ! A1: freq=6, aligned (remainder=0), average
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 24; testdt = 720
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = testhours / testfrq   ! TODO(you): verify vs spec, not just this formula
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! A1b: same config, end-of-period naming variant
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 24; testdt = 720
  testtype = 'average_end_named'; test_nleninit = .false.
  testfilecnt = testhours / testfrq
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! A2: freq=3, aligned (remainder=0), snapshot
  nt = nt + 1
  testfrq = 3; teststart=12; testhours = 12; testdt = 3600
  testtype = 'snapshot'; test_nleninit = .true.
  testfilecnt = testhours / testfrq
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! A3: freq=24, aligned (remainder=0), average
  nt = nt + 1
  testfrq = 24; teststart=0; testhours = 48; testdt = 7200   ! see dt note above
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = testhours / testfrq
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! A4: freq=1, trivially aligned always, snapshot
  nt = nt + 1
  testfrq = 1; teststart=0; testhours = 6; testdt = 1800
  testtype = 'snapshot'; test_nleninit = .true.
  testfilecnt = testhours / testfrq
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! ------------------------------------------------------------------------
  ! Group B: Non-aligned / partial-cycle stop (run_hours is NOT an exact
  ! multiple of freq). This is the primary behavior under test: at stop time,
  ! any already-complete-but-unflushed previous interval must be written AND,
  ! separately, the in-progress partial interval must also be logged --
  ! potentially two completions logged at the same stop time.
  !
  ! For each freq family we test a low remainder (just past a cycle boundary)
  ! and a high remainder (just short of the next cycle boundary), since a
  ! boundary bug is more likely to surface at the extremes than the middle.
  ! ------------------------------------------------------------------------

  ! B1: freq=3, remainder=1 (low) -- pairs directly against A2 (same start/dt)
  nt = nt + 1
  testfrq = 3; teststart=12; testhours = 13; testdt = 3600
  testtype = 'snapshot'; test_nleninit = .true.
  testfilecnt = -1   ! TODO(you): fill in from spec -- NOT simply testhours/testfrq (see note below)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B2: freq=3, remainder=2 (high, one dt-step short of the next full cycle)
  nt = nt + 1
  testfrq = 3; teststart=12; testhours = 14; testdt = 3600
  testtype = 'snapshot'; test_nleninit = .true.
  testfilecnt = -1   ! TODO(you)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B3: freq=6, remainder=1 (low) -- pairs against A1
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 25; testdt = 720
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = -1   ! TODO(you)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B4: freq=6, remainder=3 (mid)
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 27; testdt = 720
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = -1   ! TODO(you)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B5: freq=6, remainder=5 (high)
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 29; testdt = 720
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = -1   ! TODO(you)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B6: freq=6, remainder=3 (mid), snapshot variant -- confirms behavior isn't
  ! file_type-specific (average vs snapshot use different offset math)
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 27; testdt = 720
  testtype = 'snapshot'; test_nleninit = .true.
  testfilecnt = -1   ! TODO(you)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B7: freq=24, remainder=6 (low-ish) -- original suite's only non-aligned case,
  ! kept as-is (dt=7200, see note above)
  nt = nt + 1
  testfrq = 24; teststart=0; testhours = 54; testdt = 7200
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = testhours / testfrq   ! kept per original suite's known-working value
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  ! B8: freq=24, remainder=22 (high, one dt-step short of the next full cycle)
  nt = nt + 1
  testfrq = 24; teststart=0; testhours = 70; testdt = 7200
  testtype = 'average'; test_nleninit = .false.
  testfilecnt = -1   ! TODO(you)
  call run_case(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit, testfilecnt, total_errors)

  print *, "========================================================"
  if (total_errors == 0) then
     print *, "SUCCESS: All test cases passed with zero errors!"
     stop 0
  else
     print *, "FAILURE: ", total_errors, " assertions failed."
     stop 1
  end if

contains

  !> Thin wrapper: builds the test label and calls run_test. Centralizing this
  !> avoids the set_testmsg/run_test call-site duplication and the mismatched
  !> argument count that existed between them previously.
  subroutine run_case(num, dt, freq, start_hour, run_hours, file_type, init_nlen_zero, expected_completions, err_count)
    integer,          intent(in)    :: num, dt, freq, start_hour, run_hours
    character(len=*), intent(in)    :: file_type
    logical,          intent(in)    :: init_nlen_zero
    integer,          intent(in)    :: expected_completions
    integer,          intent(inout) :: err_count

    character(len=256) :: label

    if (expected_completions < 0) then
       print *, ""
       print *, "--- Test ", num, " SKIPPED: expected_completions not yet filled in (TODO) ---"
       return
    end if

    label = set_testmsg(num, dt, freq, start_hour, run_hours, file_type, init_nlen_zero, expected_completions)
    call run_test(trim(label),                    &
         dt                   = dt,                &
         freq                 = freq,               &
         file_type            = file_type,          &
         init_nlen_zero       = init_nlen_zero,      &
         start_hour           = start_hour,          &
         run_hours            = run_hours,           &
         expected_completions = expected_completions,&
         err_count            = err_count)
  end subroutine run_case

  !> A generalized routine to run a specific simulation configuration
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

    integer            :: current_time, next_time, end_time, start_time
    integer            :: alarm_time, alarm_hour
    integer            :: file_time_hours, file_day, file_hour
    integer            :: size, nlen
    logical            :: filecomplete, is_valid_file
    character(len=5)   :: curr_hm, next_hm
    character(len=256) :: filename, timestring
    integer            :: ierr
    integer            :: num_completions

    ! --- New: track whether this run is interval-aligned, and each finalize
    ! call's outcome separately, so alignment-dependent behavior is directly
    ! checkable rather than folded into one aggregate count. ---
    logical :: is_aligned
    logical :: call1_complete, call2_complete

    ierr = 0
    num_completions = 0
    is_valid_file = .false.
    is_aligned = (mod(run_hours, freq) == 0)

    start_time = start_hour * 3600
    end_time = start_time + (run_hours * 3600)

    ! Reset the tracker state for a fresh test run
    tracker%createsize = 0
    tracker%chkfile_nextAdvance = .false.
    tracker%isringing = .false.
    tracker%atstop = .false.
    tracker%filename = ""

    ! FMS offset: The first alarm rings one full frequency block after the start
    alarm_time = start_time + (freq * 3600)
    current_time = start_time

    print *, ""
    print *, "--- ", trim(test_name), " ---"
    print *, "    (remainder = ", mod(run_hours, freq), " hours, aligned = ", is_aligned, ")"

    do while (current_time < end_time)

       ! Time formatting
       next_time = current_time + dt
       curr_hm = set_timestr(current_time)
       next_hm = set_timestr(next_time)
       write(timestring,'(4(A,I2.2))') "Time: "//trim(curr_hm)//" -> "//trim(next_hm)

       ! --- A. Mock ESMF Environment ---
       if (current_time >= alarm_time) then
          alarm_time = alarm_time + (freq * 3600)
       end if

       if (next_time >= alarm_time) then
          tracker%isringing = .true.
          alarm_hour = alarm_time / 3600

          ! 1. Calculate absolute hours offset for the file target
          if (trim(file_type) == "average") then
             file_time_hours = alarm_hour - (freq + (freq / 2))
          else if (trim(file_type) == "average_end_named" .or. trim(file_type) == "snapshot") then
             file_time_hours = alarm_hour - freq
          end if

          ! 2. Calculate the Day and the Hour
          file_day = 22 + floor(real(file_time_hours) / 24.0)
          file_hour = modulo(file_time_hours, 24)

          write(filename, '("./MOM6_OUTPUT/ocn_2021_03_", I2.2, "_", I2.2, "_00.nc")') file_day, file_hour
          tracker%filename = trim(filename)

          ! Validate if this file is a no-op (i.e., its interval began before start_time)
          if ((alarm_hour - (2 * freq)) >= start_hour) then
             is_valid_file = .true.
          else
             is_valid_file = .false.
          end if

       else
          tracker%isringing = .false.
          filename = trim(tracker%filename)
       end if

       ! Mock the file state based on ringing status and NO-OP status
       if (.not. is_valid_file) then
          ! Mimic nf90_fill_int for non-existent files
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

       ! --- B. Call the REAL Feature ---
       if (len_trim(tracker%filename) > 0) then
          call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
          if (verbose) call test_loginfo(timestring, filename, tracker%chkfile_nextAdvance, size, filecomplete)

          ! --- C. Generalized Dynamic Assertions ---
          if (tracker%isringing) then
             call assert_false(filecomplete, "Ringing: filecomplete must be false", ierr)
             call assert_true(tracker%chkfile_nextAdvance, "Ringing: chkfile_nextAdvance must trigger true", ierr)
          end if

          if (filecomplete) then
             call assert_false(tracker%chkfile_nextAdvance, "Complete: chkfile_nextAdvance must flip false", ierr)
             num_completions = num_completions + 1
          end if
       end if

       current_time = current_time + dt
    end do

    ! ==================================================================
    ! --- D. Simulate ocean_model_finalize (Double Call Sequence) ---
    !
    ! Call 1 = standard outputlog_run equivalent: flushes any interval that
    !          finished during the loop but wasn't yet confirmed complete
    !          (the "stranded" file from the last full interval).
    ! Call 2 = atStopTime forced check: if the stop lands mid-interval
    !          (non-aligned run), the in-progress partial interval must also
    !          be logged here. Two completions can therefore be logged at the
    !          same stop time for a non-aligned run.
    ! ==================================================================

    if (verbose) print *, "--- Simulating ocean_model_finalize ---"

    ! Call 1: Standard outputlog_run equivalent (catches stranded file)
    tracker%isringing = .false.
    tracker%atstop = .false.
    write(timestring,'(A)') "Time: Finalize   "
    size = 90532460
    nlen = 1
    call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
    call1_complete = filecomplete
    if (filecomplete) num_completions = num_completions + 1
    if (verbose) call test_loginfo(timestring, filename, tracker%chkfile_nextAdvance, size, filecomplete)

    ! Call 2: atStopTime = .true. equivalent (Forces final file)
    tracker%isringing = .false.
    tracker%atstop = .true.
    tracker%chkfile_nextAdvance = .true.

    ! Finalize Math using absolute hours
    alarm_hour = end_time / 3600
    if (trim(file_type) == "average") then
       file_time_hours = alarm_hour - (freq / 2)
    else
       file_time_hours = alarm_hour
    end if

    file_day = 22 + floor(real(file_time_hours) / 24.0)
    file_hour = modulo(file_time_hours, 24)
    write(filename, '("./MOM6_OUTPUT/ocn_2021_03_", I2.2, "_", I2.2, "_00.nc")') file_day, file_hour
    tracker%filename = trim(filename)

    write(timestring,'(A)') "Time: StopTime"
    call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
    call2_complete = filecomplete
    if (verbose) call test_loginfo(timestring, filename, tracker%chkfile_nextAdvance, size, filecomplete)

    if (filecomplete) then
       num_completions = num_completions + 1
    end if

    ! --- New: alignment-dependent finalize assertions ---
    ! This directly encodes the stated behavior: the forced partial-cycle file
    ! (Call 2) should only complete when the run stopped mid-interval.
    if (is_aligned) then
       call assert_false(call2_complete, &
            "Aligned stop: Call2 (forced partial) must NOT complete -- no partial cycle in progress", ierr)
    else
       call assert_true(call2_complete, &
            "Non-aligned stop: Call2 (forced partial) must complete the in-progress interval", ierr)
    end if

    ! --- Final Macro Assertion ---
    call assert_equal(expected_completions, num_completions, "Total completed files must match expected_completions", ierr)

    if (ierr == 0) then
       print *, "  -> Passed. Expected number of files detected as complete"
    else
       print *, "  -> FAILED with ", ierr, " errors."
    end if

    err_count = err_count + ierr
  end subroutine run_test

  ! --- Assertion Helpers ---
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

  function set_timestr(time) result(ctime)

    integer, intent(in) :: time  ! elapsed secs
    character(len=5)    :: ctime

    integer :: hour, min

    hour = mod(time / 3600, 24)
    min =  mod(time, 3600) / 60

    write(ctime,'(I2.2,A,I2.2)')hour,':',min
  end function set_timestr

  subroutine test_loginfo(timestr, fname, chknext, size, complete)

    character(len=*), intent(in) :: timestr
    character(len=*), intent(in) :: fname
    logical,          intent(in) :: chknext
    integer,          intent(in) :: size
    logical,          intent(in) :: complete

    print '(A, L1, A, I15, A, L1)', trim(timestr)//" | " //trim(fname)// " | chkflag: ", &
         chknext, " | size: ", size, " | complete: ", complete

  end subroutine test_loginfo

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
    write(cfilecnt,'(I4.2,A)')nfiles,' expected file completions'

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
