program testopts

  implicit none

  integer :: nprocs, lstride, lroot, liotasks
  integer :: stride, root, iotasks
  character(len=1) :: ans

100 continue
  !nprocs=720
  print *,'enter values for nprocs, stride, root, iotasks'
  read(*,*),nprocs,stride,root,iotasks

  ! nprocs = get_num_procs()
  lstride = 4
  lroot = min(1,nprocs-1)
  liotasks = max(1,(nprocs-lroot)/lstride)

  print '(4(a,i6))','DEFAULTS SET: nprocs = ',nprocs,' stride = ',lstride,' root = ',lroot,' iotasks = ',liotasks

  ! if (present(iotasks)) then
  if (iotasks /= -99) liotasks=iotasks
  ! endif
  ! if (present(root)) then
  if (root /= -99) lroot=root
  ! endif
  ! if (present(stride)) then
  if (stride /= -99) lstride=stride
  ! endif

  ! if (liotasks < 1 .or. lroot < 0 .or. lstride < 1) then
  !    call abort_ice(subname//' ERROR: iotasks, root, stride incorrect ', &
  !       file=__FILE__, line=__LINE__)
  ! endif

  ! ! adjust to fit in nprocs, preserve root and stride as much as possible
  lroot = min(lroot,nprocs-1)   ! lroot <= nprocs-1
  ! ! tcraig, should work better but aborts in pio2
  ! !liotasks = min(liotasks, 1 + (nprocs-lroot-1)/lstride)
  if (lroot + (liotasks-1)*lstride > nprocs-1) then
     liotasks = max(1,(nprocs-lroot)/lstride)
  endif

  print '(4(a,i6))','NEW VALS: nprocs = ',nprocs,' stride = ',lstride,' root = ',lroot,' iotasks = ',liotasks

  print *,'continue?'
  read(*,*)ans

  if (ans .eq. 'y') then
     goto 100
  else
     stop
  end if

end program testopts
