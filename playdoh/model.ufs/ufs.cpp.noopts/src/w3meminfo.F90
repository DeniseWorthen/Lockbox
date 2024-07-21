module mallocinfo_m
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |                                   |
  !/                  | aron roland (bgs it&e gmbh)       |
  !/                  | mathieu dutour-sikiric (irb)      |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :        01-june-2018 |
  !/                  +-----------------------------------+
  !/
  !/    01-june-2018 : origination.                        ( version 6.04 )
  !/
  !  1. purpose : init pdlib part
  !  2. method :
  !  3. parameters :
  !
  !     parameter list
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. subroutines used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. called by :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  6. error messages :
  !  7. remarks
  !  8. structure :
  !  9. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  ! 10. source code :
  !
  !/ ------------------------------------------------------------------- /
  !
  use :: iso_c_binding
  implicit none
  !/
  !/ ------------------------------------------------------------------- /
  !/ parameter list
  !/
  !/ ------------------------------------------------------------------- /
  !/ local parameters
  !/
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> this structure type is used to return information about the dynamic memory allocator.
  type, bind(c) :: mallinfo_t
    !> this is the total size of memory allocated with sbrk by malloc, in bytes.
    integer(c_int) :: arena
    !> this is the number of chunks not in use. (the memory allocator internally gets chunks of memory from the operating system, and then carves them up to satisfy individual malloc requests; see efficiency and malloc.)
    integer(c_int) :: ordblks
    !> this field is unused.
    integer(c_int) :: smblks
    !> this is the total number of chunks allocated with mmap.
    integer(c_int) :: hblks
    !> this is the total size of memory allocated with mmap, in bytes.
    integer(c_int) :: hblkhd
    !> this field is unused.
    integer(c_int) :: usmblks
    !> this field is unused.
    integer(c_int) :: fsmblks
    !> this is the total size of memory occupied by chunks handed out by malloc.
    integer(c_int) :: uordblks
    !> this is the total size of memory occupied by free (not in use) chunks.
    integer(c_int) :: fordblks
    !> this is the size of the top-most releasable chunk that normally borders the end of the heap (i.e., the high end of the virtual address space’s data segment).
    integer(c_int) :: keepcost
  end type mallinfo_t
  interface
    function mallinfo() bind(c, name="mallinfo") result(data)
      use :: iso_c_binding
      implicit none
      type, bind(c) :: mallinfo_t
        integer(c_int) :: arena
        integer(c_int) :: ordblks
        integer(c_int) :: smblks
        integer(c_int) :: hblks
        integer(c_int) :: hblkhd
        integer(c_int) :: usmblks
        integer(c_int) :: fsmblks
        integer(c_int) :: uordblks
        integer(c_int) :: fordblks
        integer(c_int) :: keepcost
      end type mallinfo_t
      type(mallinfo_t) :: data
    end function mallinfo
  end interface
contains
  subroutine getmallocinfo(malinfo)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | thomas huxhorn (bgs it&e gmbh     |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : init pdlib part
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    type(mallinfo_t), intent(out) :: malinfo
    malinfo = mallinfo()
  end subroutine getmallocinfo
  subroutine printmallinfo(ihdnl,malinfo)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : init pdlib part
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    real :: ib2m
    integer(8) :: vmsize, vmrss
    integer, intent(in)          :: ihdnl
    type(mallinfo_t), intent(in) :: malinfo
    if (ihdnl .lt. 1) stop 'ihndl not set'
    ib2m=1./real(1024**2)
    vmsize = getvmsize()
    vmrss  = getvmrss()
    !write(*,'(a72,2f20.10)') "total size of memory allocated with sbrk by malloc in mbyte.  ", malinfo%arena*ib2m
    !write(*,'(a72,2f20.10)') "total size of memory allocated with mmap, in mbytes.          ", malinfo%hblkhd*ib2m
    !write(*,'(a72,2f20.10)') "total size of memory occupied by chunks handed out by malloc.", malinfo%uordblks*ib2m
    !write(*,'(a72,i10)') "total number of chunks allocated with mmap.                  ", malinfo%hblks
    !write(*,'(a72,i10)') "number of chunks not in use.                                 ", malinfo%ordblks
    !write(*,'(a72,2f20.10)') "total size of memory occupied by free (not in use) chunks.   ", malinfo%fordblks*ib2m
    !write(*,'(a72,2f20.10)') "size of the top-most releasable chunk borders end of the heap", malinfo%keepcost*ib2m
    write(ihdnl,'(a72,2f20.10)') "vm size in proc ", vmsize/1024.
    write(ihdnl,'(a72,2f20.10)') "rss size in prof ", vmrss/1024.
    call flush(ihdnl)
  end subroutine printmallinfo
  !vmpeak: peak virtual memory usage
  !vmsize: current virtual memory usage
  !vmlck:	current mlocked memory
  !vmhwm:	peak resident set size
  !vmrss:	resident set size
  !vmdata: size of "data" segment
  !vmstk:	size of stack
  !vmexe:	size of "text" segment
  !vmlib:	shared library usage
  !vmpte:	pagetable entries size
  !vmswap: swap space used
  function getvmsize() result(vmsize)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : init pdlib part
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer(8) :: vmsize
    character(len=80) :: stat_key, stat_value
    !
    vmsize = 0
    open(unit=1000, file="/proc/self/status", status='old', err=99)
    do while (.true.)
      read(unit=1000, fmt=*, err=88) stat_key, stat_value
      if (stat_key == 'vmsize:') then
        read(stat_value, *) vmsize
        exit
      end if
    end do
88  close(unit=1000)
    if (vmsize == 0) goto 99
    return
    !
99  print *, 'error: procfs not mounted or not compatible'
    vmsize = -1
  end function getvmsize
  function getvmrss() result(vmrss)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : init pdlib part
    !  2. method :
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer(8) :: vmrss
    character(len=80) :: stat_key, stat_value
    !
    vmrss = 0
    open(unit=1000, file="/proc/self/status", status='old', err=99)
    do while (.true.)
      read(unit=1000, fmt=*, err=88) stat_key, stat_value
      if (stat_key == 'vmrss:') then
        read(stat_value, *) vmrss
        exit
      end if
    end do
88  close(unit=1000)
    if (vmrss == 0) goto 99
    return
    !
99  print *, 'error: procfs not mounted or not compatible'
    vmrss = -1
  end function getvmrss
end module mallocinfo_m
!program test
!  use mallocinfo_m
!  implicit none
!  type(mallinfo_t) :: mallinfos(10000)
!  integer :: i, ninfos
!  integer, allocatable :: data(:)
!
!  allocate(data(0))
!  ninfos = 0
!  do i=1, 10
!    write(*,*) "iteration",i
!    deallocate(data)
!    allocate(data(i*100000))
!    ninfos = ninfos+1
!    call getmallocinfo(mallinfos(ninfos))
!    call printmallinfo(iaproc,mallinfos(ninfos))
!    call sleep(1)
!  end do
!   do i=10, 1, -1
!    write(*,*) "iteration",i
!    deallocate(data)
!    allocate(data(i*100000))
!    ninfos = ninfos+1
!    call getmallocinfo(mallinfos(ninfos))
!    call printmallinfo(iaproc,mallinfos(ninfos))
!    call sleep(1)
!  end do
!  write(*,*) "total size of memory allocated with sbrk. min, mean, max", minval(mallinfos(1:ninfos)%arena), sum(mallinfos(1:ninfos)%arena)/ninfos, maxval(mallinfos(1:ninfos)%arena)
!end program
