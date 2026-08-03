!> Test code for outputlog Alarm Initialization
!!
!! Probes the use of AlarmInit from MOM6 NUOPC cap to intialize alarms for
!! use by outputlog feature. Tests to ensure the outputlog alarms will trigger
!! for start hours which are not multiples of 6 (eg IAU use cases)
!!
!> @authorDenise.Worthen@noaa.gov
!> @date 08-01-2026
program test_alarminit

  use test_utils

  use ESMF,  only : ESMF_Initialize, ESMF_Finalize, ESMF_SUCCESS, ESMF_FAILURE
  use ESMF,  only : ESMF_CALKIND_GREGORIAN, ESMF_Calendar, ESMF_CalendarCreate, ESMF_Alarm
  use ESMF,  only : ESMF_Clock, ESMF_ClockCreate, ESMF_ClockGet, ESMF_ClockAdvance
  use ESMF,  only : ESMF_Time, ESMF_TimeSet, ESMF_TimeGet, ESMF_TimeInterval, ESMF_TimeIntervalSet
  use ESMF,  only : operator(==), operator(/=), operator(+), operator(-), operator(*)
  use MOM_cap_time, only : AlarmInit

  implicit none

  integer, parameter :: base_yy = 2021, base_mm = 3, base_dd = 22
  integer, parameter :: maxtests = 25

  character(len=128) :: testname
  character(len=256) :: errmsg
  character(len=256) :: assertmsg
  character(len=20)  :: subname = 'test_alarminit'

  type(testsummary)  :: alarmtests

  logical :: is_passing, assertrc
  integer :: teststart, testfreq
  integer :: rc,nt,n,ierr
  ! debug printing
  logical :: verbose = .false.

  ! initialize test tracker
  call alarmtests%init(maxtests)

  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
  call esmf_err(rc, subname, "ESMF_Initialize")

  nt = 0
  ! ===========================================================================
  ! test capture of ringtime via test -- deliberately uses a small dt so that
  ! the fixed max_steps bound (200 steps) covers LESS real time than the 6h
  ! needed to reach the actual ring, verifying the "never rang" failure path
  ! itself. Every other case below uses the default dt, which covers a
  ! generous 100h -- only dt varies here, not the step count, so the 200-step
  ! bound stays one constant, uniform invariant across every case.
  ! ===========================================================================

  nt = nt + 1
  teststart = 0; testfreq = 6
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ierr, errmsg, dt=60)   ! 200*60s ~ 3.3h, well short of the 6h needed

  is_passing = (ierr /= 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ===========================================================================
  ! test IAU offset for start hours, freqs=6,24; Ring hour must land on a
  ! 6h boundary
  ! ===========================================================================

  testfreq = 6
  do n = 1,8
     nt = nt + 1
     teststart = (n-1)*3
     write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
     call run_case(testfreq, teststart, ierr, errmsg)

     is_passing = (ierr == 0)
     call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
     call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))
  enddo

  ! ------------------
  testfreq = 24
  do n = 1,8
     nt = nt + 1
     teststart = (n-1)*3
     write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
     call run_case(testfreq, teststart, ierr, errmsg)

     is_passing = (ierr == 0)
     call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
     call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))
  enddo

  ! ===========================================================================
  ! test no IAU offset for start hours, freq=1,3; Ring hour must be start+freq
  ! exactly
  ! ===========================================================================

  nt = nt + 1
  teststart = 0; testfreq = 1
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ierr, errmsg)

  is_passing = (ierr == 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 9; testfreq = 1
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ierr, errmsg)

  is_passing = (ierr == 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 0; testfreq = 3
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ierr, errmsg)

  is_passing = (ierr == 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  nt = nt + 1
  teststart = 9; testfreq = 3
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ierr, errmsg)

  is_passing = (ierr == 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  ! start=21, freq=3 crosses midnight (21+3=24 -> hour 0, next day). No
  ! special-casing needed here: run_case compares full ESMF_Time objects via
  ! the native == operator, so day rollover is handled by ESMF's own time
  ! arithmetic rather than manual modulo-24 hour math at the call site.
  nt = nt + 1
  teststart = 21; testfreq = 3
  write(testname,'(3(A,I2.2))')'test ',nt,' start_hour ',teststart,' freq ',testfreq
  call run_case(testfreq, teststart, ierr, errmsg)

  is_passing = (ierr == 0)
  call assert_equal(is_passing, .true., testname, assertrc, assertmsg)
  call addresult(alarmtests, assertrc, trim(assertmsg), trim(errmsg))

  ! ------------------
  ! Test results
  ! ------------------

  print '(3(A,I0))','Total tests = ',alarmtests%count,' Passing = ',alarmtests%npass,' Failing = ',alarmtests%nfail
  if (alarmtests%nfail > 0) then
     print '(A)', 'FAIL: At least one test failed '
     do n = 1,alarmtests%count
        if (.not. alarmtests%teststatus(n)) print '(A)', trim(alarmtests%testmessage(n)%str)//'  [' &
             //trim(alarmtests%errmessage(n)%str)//']'
     enddo
     stop 1
  else
     do n = 1,alarmtests%count
        if (verbose .and. len_trim(alarmtests%errmessage(n)%str) > 0) then
           print '(A)', trim(alarmtests%testmessage(n)%str)//'  ['//trim(alarmtests%errmessage(n)%str)//']'
        else
           print '(A)', trim(alarmtests%testmessage(n)%str)
        endif
     enddo
  endif

  call ESMF_Finalize(rc=rc)
  call esmf_err(rc, subname, "ESMF_Finalize")

contains

  !> Runs one freq/start_hour case end to end and reports a SINGLE result:
  !! ierr==0 means the alarm rang in time AND both the primary (structural)
  !! and secondary (regression) checks passed; any other outcome sets
  !! ierr/=0 with errmsg describing specifically what failed. Callers never
  !! need to assemble their own pass/fail expression -- every call site
  !! reduces to `call run_case(...); is_passing = (ierr==0)` (or /=0 for the
  !! one case that's expected to time out).
  subroutine run_case(freq, start_hour, ierr, errmsg, dt)

    integer,           intent(in)  :: freq, start_hour
    integer,           intent(out) :: ierr
    character(len=*),  intent(out) :: errmsg
    integer, optional, intent(in)  :: dt

    type(ESMF_Clock)        :: clock
    type(ESMF_Calendar)     :: cal
    type(ESMF_Time)         :: startTime, refTime, ringTime, expectedTime, regressionTime
    type(ESMF_TimeInterval) :: timeStep, tincrement, alarmoffset, freqInterval, regressionInterval
    type(ESMF_Alarm)        :: alarm

    integer :: rc, use_dt
    integer :: toffset, ring_day, ring_hour
    integer :: rcnt, step
    integer, parameter :: max_steps = 200   ! fixed everywhere -- covers 100h at the default dt=1800s;
                                              ! only dt varies per-case to change effective coverage
    logical :: rang, primary_ok, secondary_ok

    ierr = 0
    errmsg = ''

    use_dt = 1800
    if (present(dt)) use_dt = dt

    cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, rc=rc)
    call esmf_err(rc, subname,  "ESMF_CalendarCreate")
    call ESMF_TimeSet(startTime, yy=base_yy, mm=base_mm, dd=base_dd, h=start_hour, calendar=cal, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeSet(startTime)")
    call ESMF_TimeIntervalSet(timeStep, s=use_dt, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeIntervalSet(timeStep)")

    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, rc=rc)
    call esmf_err(rc, subname,  "ESMF_ClockCreate")
    call ESMF_TimeIntervalSet(tincrement, m=1, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeIntervalSet(tincrement)")

    if (mod(start_hour,6) /= 0) then
       toffset = start_hour - 6
    else
       toffset = 0
    endif

    if (freq >= 6) then
       alarmoffset = toffset*60*tincrement
    else
       alarmoffset = 0*tincrement
    endif
    refTime = startTime + alarmoffset

    call AlarmInit(clock,      &
         alarm     = alarm,    &
         option    = 'nhours', &
         opt_n     = freq,     &
         opt_ymd   = -999,     &
         RefTime   = refTime,  &
         alarmname = 'test_alarm', rc=rc)
    call esmf_err(rc, subname,  "AlarmInit")

    ! Find first ring
    rang = .false.
    do step = 1, max_steps
       call ESMF_ClockAdvance(clock, ringingAlarmCount=rcnt, rc=rc)
       call esmf_err(rc, subname,  "ESMF_ClockAdvance")
       if (rcnt > 0) then
          rang = .true.
          exit
       endif
    end do
    if (.not. rang) then
       ierr = 1
       errmsg = 'ERROR: alarm never rang within '//itoa(max_steps)//' steps (dt='//itoa(use_dt)//'s)'
       return
    end if

    if (verbose) then
       print '(A,I0,A)', "  alarm first rang after ", step, " steps of stepping the clock forward"
       print '(A,I0,A,/)', "  RefTime passed to AlarmInit is shifted by that same ", toffset, "h"
    endif

    call ESMF_ClockGet(clock, currTime=ringTime, rc=rc)
    call esmf_err(rc, subname,  "ESMF_ClockGet(currTime at ringTime)")

    call ESMF_TimeGet(ringTime, dd=ring_day, h=ring_hour, rc=rc)
    call esmf_err(rc, subname,  "ESMF_TimeGet(ringTime)")

    ! --- PRIMARY: independent structural check (the code's own stated intent).
    ! freq>=6 can only be checked as an integer property (any multiple of 6
    ! is valid -- there's no single expected instant to compare against).
    ! freq<6 IS a single expected instant, so it's compared via ESMF's own
    ! Time equality directly -- day rollover (e.g. start=21,freq=3) is
    ! handled natively, no modulo arithmetic needed at the call site or here.
    if (freq >= 6) then
       primary_ok = (mod(ring_hour,6) == 0)
       if (.not. primary_ok) then
          errmsg = trim(errmsg)//'PRIMARY FAIL: ring hour '//itoa(ring_hour)//' is not a multiple of 6. '
       endif
    else
       call ESMF_TimeIntervalSet(freqInterval, h=freq, rc=rc)
       call esmf_err(rc, subname, "ESMF_TimeIntervalSet(freqInterval)")
       expectedTime = startTime + freqInterval
       primary_ok = (ringTime == expectedTime)
       if (.not. primary_ok) then
          errmsg = trim(errmsg)//'PRIMARY FAIL: ring did not occur at exactly start+freq. '
       endif
    endif

    ! --- SECONDARY: regression check only -- re-derives AlarmInit's own
    ! rewind-then-advance loop independently and compares the FULL resulting
    ! time (via ESMF's == operator, not just the hour) against what actually
    ! rang. Carries real oracle-mirroring risk, unlike the primary check.
    call ESMF_TimeIntervalSet(regressionInterval, h=predicted_ring_offset(freq, toffset), rc=rc)
    call esmf_err(rc, subname, "ESMF_TimeIntervalSet(regressionInterval)")
    regressionTime = startTime + regressionInterval
    secondary_ok = (ringTime == regressionTime)
    if (.not. secondary_ok) then
       errmsg = trim(errmsg)//'SECONDARY FAIL: ring time did not match the hand-derived regression value. '
    endif

    if (.not. (primary_ok .and. secondary_ok)) ierr = 1
  end subroutine run_case

  !> Independent re-derivation of AlarmInit's rewind-then-advance loop, for
  !! the regression check only. Returns the UNWRAPPED elapsed-hours offset
  !! from startTime (not an hour-of-day) so the caller can build a proper
  !! ESMF_TimeInterval and compare full ESMF_Time objects -- preserves day
  !! precision, unlike returning a modulo-24 hour value would.
  function predicted_ring_offset(freq, toffset) result(val)
    integer, intent(in) :: freq, toffset
    integer :: val
    integer :: eff_offset

    eff_offset = merge(toffset, 0, freq >= 6)
    val = eff_offset - freq
    do while (val <= 0)
       val = val + freq
    end do
  end function predicted_ring_offset
end program test_alarminit
