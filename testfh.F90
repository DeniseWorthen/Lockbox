program testfh

  implicit none

  real :: tcpl = 4800.0
  real :: tmom = 2400.0
  real :: t0, ts, tc

  real, dimension(3) :: fh = (/2.0, 4.0, 8.0/)

  integer :: ntimes = 20
  integer :: i

  t0 = 12.0*3600.0
  fh = fh*3600.0
  do i = 1,size(fh)
     print *,i,fh(i),t0+fh(i)
  end do
  print *

  do i = 1,ntimes
     ts = t0 + (i-1)*tmom
     tc = t0 + (i-1)*tcpl
     print *,i,ts,ts/3600.0,tc,tc/3600.0
  end do
end program testfh
