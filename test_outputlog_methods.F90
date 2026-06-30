program test_outputlog_methods

  use mom_outputlog_methods, only : setrequest, settype, setrootname

  implicit none

  integer, parameter :: nfreq = 4
  integer :: validfreqs(nfreq) = (/1, 3, 6, 24/)
  integer :: mock_fh(nfreq)
  character(len=32) :: mock_type(nfreq)

  logical :: requested(nfreq)
  logical :: avgtype(nfreq)

  ! Scoreboard tracking
  integer :: n_pass = 0
  integer :: n_fail = 0

  character(len=128) :: testmsg
  character(len=256) :: errmsg
  integer            :: i,ierr

  nt = 0
  !
  ! test setrequest
  !
  nt = nt + 1
  mock_fh = (/0, 0, 0, 0/)
  write(testname,'(A,I2.2,A)','test ',nt,' setrequest: outputfh==0 disables logging'
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  is_passing = (ierr == 0 .and. .not. any(requested)))
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' pass'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' fail'
  endif

  nt = nt + 1
  mock_fh = (/6, 0, 0, 0/)
  write(testname,'(A,I2,2,A)','test ',nt,' setrequest: map request to canonical order'
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  is_passing = (ierr == 0 .and. requested(3) .and. .not. any(requested((/1,2,4/))))
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' pass'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' fail'
  endif

  nt = nt + 1
  mock_fh = (/0, 24, 0, 1/)
  write(testname,'(A,I2,2,A)','test ',nt,' setrequest: map request to canonical order'
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  is_passing = (ierr == 0 .and. requested(1) .and. (requested(4) .and. .not. any(requested((/2,3/))))
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' pass'
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' fail'
  endif


  nt = nt + 1
  mock_fh = (/18, 0, 0, 0/)
  write(testname,'(A,I2,2,A)','test ',nt,' setrequest: invalid frequency blocked'
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' pass '
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' should have failed '
  endif

  nt = nt + 1
  mock_fh = (/24, 24, 0, 0/)
  write(testname,'(A,I2,2,A)','test ',nt,' setrequest: duplicate frequencies blocked'
  requested = setrequest(validfreqs, mock_fh, errmsg, ierr)

  is_passing = (ierr /= 0)
  if (is_passing) then
     npass = npass + 1
     msg(nt) = trim(testname)//' pass '
  else
     nfail = nfail + 1
     msg(nt) = trim(testname)//' should have failed '
  endif


  !
  ! test settype
  !
  testmsg = 'settype: standard lower-case strings map correctly'
  mock_fh = (/1, 6/)
  requested = (/.true., .true., .false., .false./)
  mock_type = (/ character(len=32) :: 'none', 'average', '', '' /)

  avgtype = settype(validfreqs, requested, mock_fh, mock_type, errmsg, ierr)

  if (ierr == 0 .and. trim(avgtype(1)) == 'none' .and. trim(avgtype(2)) == 'average') then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
  endif



  ! test NONE and AVERAGE; need to add tolower to code
  !

  testmsg = 'settype: invalid/typo '
  mock_fh = (/3,24/)
  mock_type = (/ character(len=32) :: 'snapshot', 'average', '', '' /)
  requested = (/.false., .true., .false., .true./)
  avgtype = settype(validfreqs, requested, mock_fh, mock_type, errmsg, ierr)

  if (ierr /= 0) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
  endif

  testmsg = 'settype: empty string on an active frequency defaults to average'
  mock_fh = (/1,6/)
  requested = (/ .true., .false., .true., .false. /)
  mock_type = (/ character(len=32) :: 'none', '', '', '' /)

  requested = settype(requested, mock_type, errmsg, ierr)

  if (ierr == 0 .and. trim(avgtype(1)) == 'none' .and. trim(avgtype(2)) == 'average') then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> Default fallback failed. Res=", avgtype
  endif

  testmsg = 'settype: mis-aligned active requests and types fail strictly'
  requested = (/ .true., .false., .false., .true. /)
  mock_type = (/ character(len=32) :: 'none', 'none', '', '' /)

  avgtype = settype(requested, mock_type, errmsg, ierr)

  ! Verification: ierr MUST be non-zero because an active slot was left unconfigured
  if (ierr /= 0) then
    n_pass = n_pass + 1
    print '(A, A)', " [PASS]: ", trim(testmsg)
  else
    n_fail = n_fail + 1
    print '(A, A)', " [FAIL]: ", trim(testmsg)
    print *, "      -> CRITICAL: Allowed misaligned array to pass without error!"
    print *, "         Returned Types: ", (/ (trim(avgtype(i)), i=1,nfreq) /)
  endif

  print *, "-------------------------------------------------------"
  print '(A, I4)', " TOTAL TESTS PASSED: ", n_pass
  print '(A, I4)', " TOTAL TESTS FAILED: ", n_fail
  print *, "-------------------------------------------------------"

  if (n_fail > 0) then
    stop 1
  else
    print *, "ALL TESTS PASSED SUCCESSFULLY."
 endif

end program test_outputlog_methods
