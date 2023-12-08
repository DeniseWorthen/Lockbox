program testvars

  use vartypedefs
  use netcdf

  implicit none

  ! example list of 'tags' set in shel/multi.inp
  integer, parameter :: ntags = 9
  integer, parameter :: nogrp = 10
  integer, parameter :: maxvars = 24
  real, parameter :: undef = 1.e35

  ! avail from ww3
  integer, parameter :: nx = 10, ny = 7
  !integer, parameter :: len_s = 3 + 1
  !integer, parameter :: len_b = 3 ! currently hardwired to 3 bedforms
  !integer, parameter :: len_m = P2MSF(3)-P2MSF(2) + 1
  !integer, parameter :: len_p = usspf(2)
  !integer, parameter :: len_k = e3df(3,1) - e3df(2,1) + 1
  integer, parameter :: len_s = 3 + 1
  integer, parameter :: len_b = 3 ! currently hardwired to 3 bedforms
  integer, parameter :: len_m = 15
  integer, parameter :: len_p = 3
  integer, parameter :: len_k = 25
 
  integer    ,target            :: dimid3(3)
  integer    ,target            :: dimid4(4)
  integer    ,pointer           :: dimid(:)

  real, dimension(nx*ny) :: dw
  real, dimension(nx*ny,0:noswll) :: phs

  interface writevar
     module procedure write_var
     module procedure write_var_s
  end interface

  ! dims: x,y = nx,ny
  !         s = noswll
  !         b = 'bedforms', currently 3
  !         m = 'p2sms', currently 15 (P2MSF(2):P2MSF(3))
  !         p = partitions, currently 3 (eg 1:USSPF(2),NK+1:NK+USSPF(2))
  !         k = nk, number of freqs (NK, also eg E3DF(2,1):E3DF(3,1))
  !         t = time
 
  integer :: ncid, ierr, xtid, ytid, stid, btid, mtid, ptid, ktid, timid, varid
  integer :: i,j,n,nt,ii,nout
  logical :: s_axis = .false., b_axis = .false., m_axis = .false., p_axis = .false., k_axis = .false.

  character(len=4), dimension(ntags) :: inptags = (/'WND ', 'CUR ', 'HS  ', 'FP  ', 'DP  ', 'PHS ', 'PTP ', 'PDIR', 'LAN '/)
  character(len=7) :: fname = 'test.nc'

  type(varatts), dimension(:), allocatable :: outvars

  call initialize_gridout

   ! debug check (maxvars<=n)
   do j = 1,nogrp
    n = 0
    do i = 1,maxvars
     !print '(4a,l)',trim(gridoutdefs(j,i)%tag)
     if(len_trim(gridoutdefs(j,i)%tag) > 0)then
       n = n+1
       !print *,trim(gridoutdefs(j,i)%tag),trim(gridoutdefs(j,i)%var_name), trim(gridoutdefs(j,i)%long_name)
     end if
    enddo
    print *,'group ',j,' variables ',n
   end do

   ! determine which variables are tagged for output
   do j = 1,nogrp
    do i = 1,maxvars
     if(len_trim(gridoutdefs(j,i)%tag) > 0)then
      do nt = 1,ntags
       if(trim(gridoutdefs(j,i)%tag) == trim(inptags(nt)))gridoutdefs(j,i)%validout = .true.
      end do
     end if
    end do
   end do

   ! determine number of output variables (not the same as the number of tags)
   n = 0
   do j = 1,nogrp
    do i = 1,maxvars
     if(gridoutdefs(j,i)%validout)n = n+1
    end do
   end do
   nout = n
   allocate(outvars(1:nout))

   ! subset variables requested
   n = 0
   do j = 1,nogrp
    do i = 1,maxvars
      if(gridoutdefs(j,i)%validout) then
        n = n+1
        outvars(n) = gridoutdefs(j,i)
      end if
    enddo
   end do
   ! check
   nout = n
   print *,'number of requested output variables ',nout
   do n = 1,nout
    print *,n,'  ',trim(outvars(n)%tag),' ',trim(outvars(n)%var_name),'  ',trim(outvars(n)%dims)
   end do
   
   ! define the dimensions required
   do n = 1,nout
     if(outvars(n)%validout) then
       if(scan(trim(outvars(n)%dims),'s') > 0)s_axis = .true.
       if(scan(trim(outvars(n)%dims),'b') > 0)b_axis = .true.
       if(scan(trim(outvars(n)%dims),'m') > 0)m_axis = .true.
       if(scan(trim(outvars(n)%dims),'p') > 0)p_axis = .true.
       if(scan(trim(outvars(n)%dims),'k') > 0)k_axis = .true.
     end if
    end do

   ierr = nf90_create(trim(fname),nf90_clobber,ncid)
   ierr = nf90_def_dim(ncid,'nx',nx,xtid)
   ierr = nf90_def_dim(ncid,'ny',ny,ytid)
   ierr = nf90_def_dim(ncid,'time', nf90_unlimited, timid)

   if(s_axis)ierr = nf90_def_dim(ncid,'noswll',len_s,stid)
   if(b_axis)ierr = nf90_def_dim(ncid,'nb'    ,len_b,btid)
   if(m_axis)ierr = nf90_def_dim(ncid,'nm'    ,len_m,mtid)
   if(p_axis)ierr = nf90_def_dim(ncid,'np'    ,len_p,ptid)
   if(k_axis)ierr = nf90_def_dim(ncid,'freq'  ,len_k,ktid)

   ! define time axis
   ierr = nf90_put_att(ncid, timid, 'units'   , trim(time_origin))
   ierr = nf90_put_att(ncid, timid, 'calendar', trim(calendar_name))

   ! define the variables
   dimid3(1:2) = (/xtid, ytid/)
   dimid4(1:2) = (/xtid, ytid/)
   do n = 1,nout
      if(scan(trim(outvars(n)%dims),'s') > 0) then
       dimid4(3:4) = (/stid, timid/)
       dimid => dimid4
      else if(scan(trim(outvars(n)%dims),'b') > 0) then
       dimid4(3:4) = (/btid, timid/)
       dimid => dimid4
      else if(scan(trim(outvars(n)%dims),'m') > 0) then
       dimid4(3:4) = (/mtid, timid/)
       dimid => dimid4
      else if(scan(trim(outvars(n)%dims),'p') > 0) then
       dimid4(3:4) = (/ptid, timid/)
       dimid => dimid4
      else if(scan(trim(outvars(n)%dims),'k') > 0) then
       dimid4(3:4) = (/ktid, timid/)
       dimid => dimid4
      else
       dimid3(3) = timid
       dimid => dimid3
      end if

      ierr = nf90_def_var(ncid, trim(outvars(n)%var_name), nf90_float, dimid, varid)
      ierr = nf90_put_att(ncid, varid, 'units'     , trim(outvars(n)%unit_name))
      ierr = nf90_put_att(ncid, varid, 'long_name' , trim(outvars(n)%long_name))
      ierr = nf90_put_att(ncid, varid, '_FillValue', undef)
    end do
      ierr = nf90_enddef(ncid)

    ! write the variables
    ierr = nf90_inq_varid(ncid, 'time', timid)
    ierr = nf90_put_var(ncid, timid, elapsed_secs)
    do n = 1,nout
      vname = trim(outvars(n)%var_name)
      if(vname .eq. 'DW')call write_var(trim(fname), vname, DW)

      if(vname .eq. 'PHS')call write_var(trim(fname), vname, PHS)
    end do
    ierr = nf90_close(ncid)

    subroutine write_var(fname, vname, var)

    character(len=*), intent(in) :: fname
    real, dimension(nsea), intent(in) :: var
    character(len=*), intent(in) :: vname

    real, dimension(nx,ny) :: var2d
    var2d = undef
    do i = 1,nsea
     var2d(mapsta....) = var(i)
    end do

    ierr = nf90_open(trim(fname), nc_write, ncid)
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    ierr = nf90_put_varid(ncid, varid, var2d)
    ierr = nf90_close(ncid)

    end subroutine write_var

    subroutine write_var_s(fname, vname, var)

    character(len=*), intent(in) :: fname
    real, dimension(nsea,0:noswll), intent(in) :: var
    character(len=*), intent(in) :: vname

    real, dimension(nx,ny,0:nswll) :: var3d
    var2d = undef
    do i = 1,nsea
     var3d(mapsta....) = var(i)
    end do

    ierr = nf90_open(trim(fname), nc_write, ncid)
    ierr = nf90_inq_varid(ncid, trim(vname), varid)
    ierr = nf90_put_varid(ncid, varid, var3d)
    ierr = nf90_close(ncid)

    end subroutine write_var
 

end program testvars
