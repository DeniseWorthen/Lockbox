program test_outputlog_methods

  use mom_outputlog_methods, only : outputlog_state_type, check_completion

  implicit none

  integer :: total_errors = 0
  logical :: verbose = .false.

  integer :: nt, testdt, testfrq, teststart, testhours
  logical :: test_nleninit

  character(len= 32) :: testtype
  character(len=256) :: testmsg

  nt = 0

  print *, "========================================================"
  print *, " Starting Generalized Outputlog Test Suite"
  print *, "========================================================"

  ! Test 1: 6-hourly average, dt=720, start=06, run=24h
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 24; testdt = 720
  testtype = 'average'; test_nleninit = .false.
  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)

  call run_test(trim(testmsg), dt=testdt, freq=testfrq, file_type=testtype,         &
       init_nlen_zero=test_nleninit, start_hour = teststart, run_hours = testhours, &
       err_count = total_errors)

  ! Test 2: 1-hourly snapshot, dt=1800 (30 mins), start=00Z, run=6h
  nt = nt + 1
  testfrq = 1; teststart=0; testhours = 6; testdt = 1800
  testtype = 'snapshot'; test_nleninit = .true.
  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)

  call run_test(trim(testmsg), dt=testdt, freq=testfrq, file_type=testtype,         &
       init_nlen_zero=test_nleninit, start_hour = teststart, run_hours = testhours, &
       err_count = total_errors)

  ! Test 3: 3-hourly snapshot, dt=3600 (1 hour), start=12Z, run=12h
  nt = nt + 1
  testfrq = 3; teststart=12; testhours = 12; testdt = 3600
  testtype = 'snapshot'; test_nleninit = .true.
  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)

  call run_test(trim(testmsg), dt=testdt, freq=testfrq, file_type=testtype,         &
       init_nlen_zero=test_nleninit, start_hour = teststart, run_hours = testhours, &
       err_count = total_errors)

  ! Test 4: 24-hourly average, dt=7200 (2 hours), start=00Z, run=48h
  nt = nt + 1
  testfrq = 24; teststart=0; testhours = 48; testdt = 7200
  testtype = 'average'; test_nleninit = .false.
  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)

  call run_test(trim(testmsg), dt=testdt, freq=testfrq, file_type=testtype,         &
       init_nlen_zero=test_nleninit, start_hour = teststart, run_hours = testhours, &
       err_count = total_errors)

  ! Test 5: 6-hourly average, dt=720 (12 mins), start=06, run=24h (End-of-period naming)
  nt = nt + 1
  testfrq = 6; teststart=6; testhours = 24; testdt = 720
  testtype = 'average_end_named'; test_nleninit = .false.
  testmsg = set_testmsg(nt, testdt, testfrq, teststart, testhours, testtype, test_nleninit)

  call run_test(trim(testmsg), dt=testdt, freq=testfrq, file_type=testtype,         &
       init_nlen_zero=test_nleninit, start_hour = teststart, run_hours = testhours, &
       err_count = total_errors)

  print *, "========================================================"
  if (total_errors == 0) then
     print *, "SUCCESS: All test cases passed with zero errors!"
     stop 0
  else
     print *, "FAILURE: ", total_errors, " assertions failed."
     stop 1
  end if

contains

  !> A generalized routine to run a specific simulation configuration
  subroutine run_test(test_name, dt, freq, file_type, init_nlen_zero, start_hour, run_hours, err_count)

    character(len=*), intent(in)    :: test_name
    integer,          intent(in)    :: dt
    integer,          intent(in)    :: freq
    character(len=*), intent(in)    :: file_type
    logical,          intent(in)    :: init_nlen_zero
    integer,          intent(in)    :: start_hour
    integer,          intent(in)    :: run_hours
    integer,          intent(inout) :: err_count

    type(outputlog_state_type) :: tracker

    integer            :: current_time, next_time, end_time, start_time
    integer            :: current_alarm_time, alarm_hour
    integer            :: file_time_hours, file_day, file_hour
    integer            :: size, nlen
    logical            :: filecomplete, is_valid_file
    character(len=5)   :: curr_hm, next_hm
    character(len=256) :: filename, timestring
    integer            :: ierr
    integer            :: num_completions
    integer            :: expected_completions

    ierr = 0
    num_completions = 0
    expected_completions = run_hours / freq

    is_valid_file = .false.

    start_time = start_hour * 3600
    end_time = start_time + (run_hours * 3600)

    ! Reset the tracker state for a fresh test run
    tracker%createsize = 0
    tracker%chkfile_nextAdvance = .false.
    tracker%isringing = .false.
    tracker%atstop = .false.
    tracker%filename = ""

    ! FMS offset: The first alarm rings one full frequency block after the start
    current_alarm_time = start_time + (freq * 3600)
    current_time = start_time

    print *, ""
    print *, "--- ", trim(test_name), " ---"

    do while (current_time < end_time)

       ! Time formatting
       next_time = current_time + dt
       curr_hm = set_timestr(current_time)
       next_hm = set_timestr(next_time)

       ! --- A. Mock ESMF Environment ---
       if (current_time >= current_alarm_time) then
          current_alarm_time = current_alarm_time + (freq * 3600)
       end if

       if (next_time >= current_alarm_time) then
          tracker%isringing = .true.
          alarm_hour = current_alarm_time / 3600

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
       write(timestring,'(4(A,I2.2))') "Time: "//trim(curr_hm)//" -> "//trim(next_hm)

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
    ! ==================================================================

    if (verbose) print *, "--- Simulating ocean_model_finalize ---"

    ! Call 1: Standard outputlog_run equivalent (catches stranded file)
    tracker%isringing = .false.
    tracker%atstop = .false.
    write(timestring,'(A)') "Time: Finalize (Call 1)"
    size = 90532460
    nlen = 1
    call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
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

    write(timestring,'(A)') "Time: Finalize (Call 2 - atStopTime)"
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

  function set_testmsg(num, dt, freq, start, hours, atype, nleninit0) result(testmsg)

    integer,          intent(in) :: num, dt, freq, start, hours
    character(len=*), intent(in) :: atype
    logical,          intent(in) :: nleninit0

    character(len=64)  :: cnum, cdt, cstart, chours, freqtype, initype
    character(len=256) :: testmsg

    write(cnum,  '(I2.2)') num
    write(cdt,   '(I4.4,A)') dt, ' secs'
    write(cstart,'(I2.2,A)') start, 'Z'
    write(chours,'(I2.2,A)') hours, 'h'

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
         //', ' // trim(initype)

  end function set_testmsg

end program test_outputlog_methods
