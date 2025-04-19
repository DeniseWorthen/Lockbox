program testnml

  implicit none

  integer, parameter :: maxatmres = 10
  character(len=6) :: atmreslist(maxatmres)
  !character(len=4), allocatable :: catm(:)
  integer, allocatable :: catm(:)
  integer :: iounit, i, ii, rc, nar

  namelist /grid_nml/ atmreslist

  atmreslist(:) = ' '

  open (action='read', file='test.nml', iostat=rc, newunit=iounit)
  read (nml=grid_nml, iostat=rc, unit=iounit)
  close(iounit)

  nar = 0
  do i = 1,size(atmreslist)
     if (len_trim(atmreslist(i)) > 0) nar = nar+1
  end do
  print *,nar
  allocate(catm(nar))

  do i = 1,nar
     read(atmreslist(i),'(i4)')catm(i)
     print *,i,catm(i)
  end do
  !print *,'X ',catm

end program testnml
