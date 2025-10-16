program tasks

  implicit none

  integer, parameter :: res=1152
  integer, parameter :: tocn = 240, tice = 240, twav = 4000

  !integer, parameter :: inpes=16, jnpes=24, res=1152
  !integer, parameter :: wrtg=1, wrtt=120
  !integer, parameter :: blocksize=32

  integer ::  tatm, twgc
  integer :: lbatm, ubatm, lbice, ubice, lbocn, ubocn, lbwav, ubwav

100 continue

  print *,'enter inpes,jnpes,blocksize'
  read(*,*)inpes,jnpes,blocksize

  blocks = (res/inpes)*(res/jnpes)
  if ( mod(blocks/blocksize) .ne. 0) then
     print *,'blocksize not perfect'
     go to 100
  else
     print *,'enter wtg groups, tasks and atm threads'
     read(*,*)wrtg,wrtt,atmthrd

     tatm = inpes*jnpes*6
     twgc = wrtg*wrtt

     atm = (tatm + twgc)*atmthrd

     lbatm = 0
     ubatm = atm - 1
     print '(a,2i6)','atm pelist : ',lbatm,ubatm

     lbice = ubatm+1
     ubice = lbice+(tice-1)
     print '(a,2i6)','ice pelist : ',lbice,ubice

     lbocn = ubice
     ubocn = lbocn+(tocn-1)
     print '(a,2i6)','ocn pelist : ',lbocn,ubocn

     lbwav = ubocn+1
     ubwav = lbwav+(twav-1)
     print '(a,2i6)','wav pelist : ',lbwav,ubwav

  end if

end program tasks
