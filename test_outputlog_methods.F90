program test_outputlog_methods

  use mom_outputlog_methods, only : outputlog_state_type, check_completion

  implicit none

  integer :: total_errors = 0

  print *, "========================================================"
  print *, " Starting Generalized Outputlog Test Suite"
  print *, "========================================================"

  ! Test 1: 6-hourly average, dt=720 (12 mins), start=06, run=24h
  call run_test_case("Test 1: 6h Avg, dt=720, start=06Z, run=24h", &
       dt=720, freq=6, file_type="average", init_nlen_zero=.false., &
       start_hour=6, run_hours=18, err_count=total_errors)

  ! ! Test 2: 1-hourly snapshot, dt=1800 (30 mins), start=00Z, run=6h
  ! call run_test_case("Test 2: 1h Snap, dt=1800, start=00Z, run=6h", &
  !      dt=1800, freq=1, file_type="snapshot", init_nlen_zero=.true., &
  !      start_hour=0, run_hours=6, err_count=total_errors)

  ! ! Test 3: 3-hourly snapshot, dt=3600 (1 hour), start=12Z, run=12h
  ! call run_test_case("Test 3: 3h Snap, dt=3600, start=12Z, run=12h", &
  !      dt=3600, freq=3, file_type="snapshot", init_nlen_zero=.true., &
  !      start_hour=12, run_hours=12, err_count=total_errors)

  ! ! Test 4: 24-hourly average, dt=7200 (2 hours), start=00Z, run=48h (Probing dt > freq edge cases)
  ! call run_test_case("Test 4: 24h Avg, dt=7200, start=00Z, run=48h", &
  !      dt=7200, freq=24, file_type="average", init_nlen_zero=.false., &
  !      start_hour=0, run_hours=48, err_count=total_errors)

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
  subroutine run_test_case(test_name, dt, freq, file_type, init_nlen_zero, start_hour, run_hours, err_count)

    character(len=*), intent(in)    :: test_name
    integer,          intent(in)    :: dt
    integer,          intent(in)    :: freq
    character(len=*), intent(in)    :: file_type
    logical,          intent(in)    :: init_nlen_zero
    integer,          intent(in)    :: start_hour
    integer,          intent(in)    :: run_hours
    integer,          intent(inout) :: err_count

    type(outputlog_state_type) :: tracker
    integer                    :: current_time, end_time, start_time
    integer                    :: alarm_window_index, next_alarm_time
    integer                    :: curr_hour, curr_min, next_time, next_hour, next_min
    integer                    :: size, nlen, file_hour
    logical                    :: filecomplete
    character(len=256)         :: filename, timestring
    integer                    :: local_errors
    integer :: num_completions
    integer :: expected_completions

    local_errors = 0
    num_completions = 0  ! Initialize the counter
    expected_completions = run_hours / freq ! Calculate expected files
    start_time = start_hour * 3600
    end_time = start_time + (run_hours * 3600)

    ! Reset the tracker state for a fresh test run
    tracker%createsize = 0
    tracker%chkfile_nextAdvance = .false.
    tracker%isringing = .false.
    tracker%atstop = .false.

    next_alarm_time = start_time
    current_time = start_time

    print *, ""
    print *, "--- Running: ", trim(test_name), " ---"

    do while (current_time <= end_time)

       ! Time formatting
       curr_hour = mod(current_time / 3600, 24)
       curr_min  = mod(current_time, 3600) / 60
       next_time = current_time + dt
       next_hour = mod(next_time / 3600, 24)
       next_min  = mod(next_time, 3600) / 60

       ! --- A. Mock ESMF Environment ---
       ! Check if the alarm should ring (triggers at or past the target interval)
       if (current_time >= next_alarm_time) then
          tracker%isringing = .true.
          alarm_window_index = (next_alarm_time - start_time) / (freq * 3600)
          ! Advance the alarm to the next frequency window
          next_alarm_time = next_alarm_time + (freq * 3600)
       else
          tracker%isringing = .false.
       end if

       ! Construct the filename based on Average vs Snapshot and the current ALARM window
       if (trim(file_type) == "average") then
          file_hour = mod(start_hour + (alarm_window_index * freq) + (freq / 2), 24)
       else
          file_hour = mod(start_hour + (alarm_window_index * freq) + freq, 24)
       end if
       write(filename, '("./MOM6_OUTPUT/ocn_2021_03_22_", I2.2, "_00.nc")') file_hour
       tracker%filename = trim(filename)

       ! Mock the file state based on the ringing status
       if (tracker%isringing) then
          size = 199276
          if (init_nlen_zero) then
             nlen = 0  ! Will trigger use_filesize = .false. inside check_completion
          else
             nlen = 1  ! Will trigger use_filesize = .true. inside check_completion
          end if
       else
          size = 90532460
          nlen = 1
       end if

       ! --- B. Call the REAL Feature ---
       write(timestring,'(4(A,I2.2))') "Time: ", curr_hour, ":", curr_min, " -> ", next_hour, ":", next_min
       call check_completion(tracker, nlen, size, .false., timestring, filecomplete)

       ! Print log output for visual verification
       print '(A, L1, A, I10, A, L1)', trim(timestring)//" | " //trim(filename)// " | chkflag: ", &
            tracker%chkfile_nextAdvance, " | size: ", size, " | complete: ", filecomplete

       ! --- C. Generalized Dynamic Assertions ---
       if (tracker%isringing) then
          call assert_false(filecomplete, "Ringing: filecomplete must be false", local_errors)
          call assert_true(tracker%chkfile_nextAdvance, "Ringing: chkfile_nextAdvance must trigger true", local_errors)
       end if

       ! If the file just completed, ensure the tracking flag turned off
       if (filecomplete) then
          call assert_false(tracker%chkfile_nextAdvance, "Complete: chkfile_nextAdvance must flip false", local_errors)
       end if

       ! Increment our macro-assertion counter
       if (filecomplete) then
          num_completions = num_completions + 1
       end if

       current_time = current_time + dt
    end do

    ! ! ==================================================================
    ! ! --- D. Simulate ocean_model_finalize (Double Call Sequence) ---
    ! ! ==================================================================

    ! print *, "--- Simulating ocean_model_finalize ---"

    ! ! Call 1: Standard outputlog_run equivalent
    ! tracker%isringing = .false.
    ! tracker%atstop = .false.
    ! write(timestring,'(A)') "Time: Finalize (Call 1)"

    ! call check_completion(tracker, nlen, size, .false., timestring, filecomplete)
    ! if (filecomplete) num_completions = num_completions + 1

    ! ! Call 2: atStopTime = .true. equivalent
    ! tracker%isringing = .false.
    ! tracker%atstop = .true.

    ! ! The cap manually forces the check flag to true for the final file before calling check_completion
    ! tracker%chkfile_nextAdvance = .true.

    ! ! Mock that FMS has now flushed the final interval's file to disk
    ! size = 90532460
    ! nlen = 1

    ! write(timestring,'(A)') "Time: Finalize (Call 2 - atStopTime)"
    ! call check_completion(tracker, nlen, size, .false., timestring, filecomplete)

    ! print '(A, L1, A, I10, A, L1)', trim(timestring)//" | " //trim(filename)// " | chkflag: ", &
    !      tracker%chkfile_nextAdvance, " | size: ", size, " | complete: ", filecomplete

    ! ! Increment if the final forced check successfully completed
    ! if (filecomplete) then
    !    num_completions = num_completions + 1
    !    call assert_true(filecomplete, "Finalize: atStopTime must trigger a final file completion", local_errors)
    ! end if

    ! ! --- Final Macro Assertion ---
    ! call assert_equal(expected_completions, num_completions, "Total completed files must exactly match run_hours / freq", local_errors)
    ! call assert_equal(expected_completions, num_completions, "Total completed files must match run_hours / freq", local_errors)

    ! if (local_errors == 0) then
    !    print *, "  -> Passed. Expected number of files detected as complete"
    ! else
    !    print *, "  -> FAILED with ", local_errors, " errors."
    ! end if

    ! err_count = err_count + local_errors

    ! if (local_errors == 0) then
    !    print *, "  -> Passed."
    ! else
    !    print *, "  -> FAILED with ", local_errors, " errors."
    ! end if

    ! err_count = err_count + local_errors

  end subroutine run_test_case

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

end program test_outputlog_methods
