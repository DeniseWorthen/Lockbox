program jnk
  implicit none

  integer :: n
  integer :: fh, fhinit, toff

  integer, dimension(3) :: freq = (/3,6,24/)

  fhinit = 12

  !do n = 0,23
  !   print *,n,mod(n,6)
  !end do

  do n = 1,size(freq)
     if (mod(fhinit,freq(n)) .ne. 0) then
        toff = fhinit-freq(n)
     else
        toff = 0
     end if
     print *,n,freq(n),toff
  end do
end program jnk
