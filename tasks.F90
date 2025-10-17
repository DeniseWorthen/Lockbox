program tasks

  implicit none

  integer, parameter :: res=1152
  !integer, parameter :: tocn = 240, tice = 240, twav = 4000
  integer, parameter :: tice = 80, tocn = 120, twav = 1200
  integer, parameter :: medmax = 1200

  integer :: inpes, jnpes, blocksize, blocks, atmthrd
  integer :: nt128, nt192

  !integer, parameter :: inpes=16, jnpes=24, res=1152
  !integer, parameter :: wrtg=1, wrtt=120
  !integer, parameter :: blocksize=32

  integer ::  tatm, twgc, atm, wrtg, wrtt, tmed
  integer :: lbatm, ubatm, lbmed, ubmed, lbice, ubice, lbocn, ubocn, lbwav, ubwav

100 continue

  print *,'enter inpes,jnpes,blocksize'
  read(*,*)inpes,jnpes,blocksize

  blocks = (res/inpes)*(res/jnpes)
  if ( mod(blocks,blocksize) .ne. 0) then
     print *,'blocksize not perfect'
     go to 100
  else
     print *,'enter wtg groups, tasks and atm threads'
     read(*,*)wrtg,wrtt,atmthrd

     tatm = inpes*jnpes*6
     twgc = wrtg*wrtt
     atm = (tatm + twgc)*atmthrd
     print '(a,2i6)',' atm, wtg tasks: ',tatm,twgc

     lbatm = 0
     ubatm = atm - 1
     print '(a,2i6)','ATM_petlist_bounds: ',lbatm,ubatm
     print '(a,i6)','ATM_omp_num_threads: ',atmthrd

     lbmed = 0
     if (tatm > medmax)tmed = medmax*atmthrd
     ubmed = lbmed+(tmed-1)
     print '(a,2i6)','MED_petlist_bounds: ',lbmed,ubmed
     print '(a,i6)','MED_omp_num_threads: ',atmthrd

     lbocn = ubatm+1
     ubocn = lbocn+(tocn-1)
     print '(a,2i6)','OCN_petlist_bounds: ',lbocn,ubocn

     lbice = ubocn+1
     ubice = lbice+(tice-1)
     print '(a,2i6)','ICE_petlist_bounds: ',lbice,ubice

     lbwav = ubice+1
     ubwav = lbwav+(twav-1)
     print '(a,2i6)','WAV_petlist_bounds: ',lbwav,ubwav

     nt128 = 1+(1+ubwav)/128
     nt192 = 1+(1+ubwav)/192

     print '(a,2i6)','total nodes @128 ',nt128,nt128*128
     print '(a,2i6)','total nodes @192 ',nt192,nt192*192
  end if

end program tasks
