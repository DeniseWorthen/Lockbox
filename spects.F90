program test
  implicit none

  integer, parameter :: nk = 50
  integer, parameter :: nth = 36
  !integer, parameter :: nth = 36
  !integer, parameter :: nk = 33
  integer :: ith, ik, kk

  kk = 0
  do ik = 1,nk
     do ith = 1,nth
        kk = kk + 1
        !print '(3(a,i5))','frq ',ik,' dir ',ith,' nspec ',kk
        print '(3(a,i5))','dir ',ith,' frq ',ik,' nspec ',kk
     end do
  end do


  ! integer, parameter :: naproc=121

  ! character(len=3) :: caproc
  ! integer :: i

  ! do i = 1,naproc
  !    write(caproc,'(i3.3)')i
  !    print *,trim(caproc)
  ! end do

  ! print *,17*8570 + ((55-18)+1)*8569 + ((121-56)+1)*8568
end program test
