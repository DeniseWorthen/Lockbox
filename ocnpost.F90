program ocnpost

  use utils_mod
  use ocnvars
  use netcdf

  implicit none

  character(len=240) :: filesrc, filedst, wgtsfile, fout, dstgrid
  character(len=120) :: wgtsdir = '/scratch1/NCEPDEV/climate/Denise.Worthen/grids-test-202210/'

  integer, parameter :: nxt = 1440, nyt = 1080, nlevs = 40  !tripole dimensions, 1/4deg
  integer, parameter :: nxr = 1440, nyr =  721              !regular grid dimensions, 1/4deg

  real, allocatable, dimension(:,:)   :: bilin2d, consd2d
  real, allocatable, dimension(:,:,:) :: bilin3d

  type(vardefs), allocatable :: b2d(:)
  type(vardefs), allocatable :: c2d(:)
  type(vardefs), allocatable :: b3d(:)

  real, allocatable :: tmp3d(:,:,:)   !debug use

  ! source grid fields
  real, dimension(nxt*nyt,nlevs) :: mask3d
  real, dimension(nxt*nyt)       :: cosrot, sinrot
  real, dimension(nlevs)         :: z_l
  real, dimension(0:nlevs)       :: z_i

  ! destination grid fields
  real, dimension(nxr*nyr,nlevs) :: rgmask3d
  real, dimension(nxr*nyr)       :: out1d
  real, dimension(nxr,nyr)       :: dstlon, dstlat, out2d
  real, dimension(nxr,nyr,nlevs) :: out3d
  real, allocatable, dimension(:,:)   :: rgb2d, rgc2d
  real, allocatable, dimension(:,:,:) :: rgb3d

  real(kind=8) :: timestamp

  integer :: i,j,k,n,nn,nvalid
  integer :: rc,ncid,varid,dimid
  integer :: nbilin2d, nbilin3d, nconsd2d
  integer :: indx_to, indx_spd

  character(len= 40) :: timeunit, timecal
  character(len= 20) :: vname, vunit
  character(len=120) :: vlong
  real    :: vfill
  integer :: idimid, jdimid, kdimid, edimid, timid

  logical :: debug = .true.

  ! initialize the variable type
  call ovars_typedefine

  ! --------------------------------
  ! open the source file and obtain the units and long name,
  ! rotation angles, vertical grid and time axis
  ! --------------------------------

  filesrc = 'ocn_2013_04_01_03.nc'

  nvalid = 0
  rc = nf90_open(trim(filesrc), nf90_nowrite, ncid)
  do i = 1,maxvars
     if (len_trim(ovars(i)%input_var_name) > 0 ) then
        rc = nf90_inq_varid(ncid, trim(ovars(i)%input_var_name), varid)
        rc = nf90_get_att(ncid, varid, 'long_name', ovars(i)%long_name)
        rc = nf90_get_att(ncid, varid, 'units', ovars(i)%units)
        rc = nf90_get_att(ncid, varid, '_FillValue', ovars(i)%var_fillvalue)
        nvalid = nvalid+1
        if (trim(ovars(i)%input_var_name) == 'temp')vfill = ovars(i)%var_fillvalue
     end if
  end do
  print *,' vfill = ',vfill

  ! timestamp
  rc = nf90_inq_varid(ncid, 'time', varid)
  rc = nf90_get_var(ncid, varid, timestamp)
  rc = nf90_get_att(ncid, varid, 'units', timeunit)
  rc = nf90_get_att(ncid, varid, 'calendar', timecal)
  ! cell centers
  rc = nf90_inq_varid(ncid, 'z_l', varid)
  rc = nf90_get_var(ncid, varid, z_l)
  ! cell edges
  rc = nf90_inq_varid(ncid, 'z_i', varid)
  rc = nf90_get_var(ncid, varid, z_i)
  ! rotation angles
  call getfield(trim(filesrc), 'cos_rot', dims=(/nxt,nyt/), field=cosrot)
  call getfield(trim(filesrc), 'sin_rot', dims=(/nxt,nyt/), field=sinrot)
  rc = nf90_close(ncid)
  !print *,z_l
  !print *,z_i
  print *,timestamp

  ! --------------------------------
  ! count numbers of fields to remapped for each
  ! mapping type; these can be remapped as packed arrays
  ! --------------------------------

  nbilin2d = 0; nbilin3d = 0; nconsd2d = 0
  do n = 1,nvalid
     if (trim(ovars(n)%var_remapmethod)  == 'bilinear') then
        if (ovars(n)%var_dimen == 2) nbilin2d = nbilin2d + 1
        if (ovars(n)%var_dimen == 3) nbilin3d = nbilin3d + 1
     end if
     if (trim(ovars(n)%var_remapmethod)  == 'conserve')nconsd2d = nconsd2d + 1  !no 3d variables w/ conservative mapping
  end do
  if (debug) print '(3(a,i4))','bilin 2d ',nbilin2d,' bilin 3d ',nbilin3d,' conserv 2d ',nconsd2d

  allocate(bilin2d(nxt*nyt,nbilin2d))
  allocate(bilin3d(nxt*nyt,nlevs,nbilin3d))
  allocate(consd2d(nxt*nyt,nconsd2d))

  allocate(b2d(1:nbilin2d))
  allocate(c2d(1:nconsd2d))
  allocate(b3d(1:nbilin3d))

  ! --------------------------------
  ! create types for each packed array
  ! --------------------------------

  i = 0; j = 0; k = 0
  do n = 1,nvalid
     if (trim(ovars(n)%var_remapmethod)  == 'bilinear') then
        if (ovars(n)%var_dimen == 2) then
           i = i+1; b2d(i) = ovars(n)
        end if
        if (ovars(n)%var_dimen == 3) then
           j = j+1; b3d(j) = ovars(n)
        end if
     end if
     if (trim(ovars(n)%var_remapmethod)  == 'conserve') then
        k = k+1; c2d(k) = ovars(n)
     end if
  end do

  if (debug) then
     print *,'2D fields mapped bilin'
     do n = 1,nbilin2d
        print '(i6,4(a,a))',n,'  ',trim(b2d(n)%input_var_name),'  ',trim(b2d(n)%var_grid), &
             '  ',trim(b2d(n)%var_pair),'  ', trim(b2d(n)%var_pair_grid)
     end do
     print *,'3D fields mapped bilin'
     do n = 1,nbilin3d
        print '(i6,4(a,a))',n,'  ',trim(b3d(n)%input_var_name),'  ',trim(b3d(n)%var_grid), &
             '  ',trim(b3d(n)%var_pair),'  ', trim(b3d(n)%var_pair_grid)
     end do
     print *,'2D fields mapped conserv'
     do n = 1,nconsd2d
        print '(i6,4(a,a))',n,'  ',trim(c2d(n)%input_var_name),'  ',trim(c2d(n)%var_grid), &
             '  ',trim(c2d(n)%var_pair),'  ', trim(c2d(n)%var_pair_grid)
     end do
  end if

  ! --------------------------------
  ! create packed arrays for mapping
  ! --------------------------------

  ! 2D bilin
  call packarrays(trim(filesrc), trim(wgtsdir), cosrot, sinrot, b2d, dims=(/nxt,nyt/), nflds=nbilin2d, fields=bilin2d)
  ! 2D conserv
  call packarrays(trim(filesrc), trim(wgtsdir), cosrot, sinrot, c2d, dims=(/nxt,nyt/), nflds=nconsd2d, fields=consd2d)
  ! 3D bilin
  call packarrays(trim(filesrc), trim(wgtsdir), cosrot, sinrot, b3d, dims=(/nxt,nyt,nlevs/), nflds=nbilin3d, fields=bilin3d)

  if (debug) call dumpnc('bilin2d.nc', 'bilin2d', dims=(/nxt,nyt/)      , nflds=nbilin2d, field=bilin2d)
  if (debug) call dumpnc('consd.nc'  ,   'consd', dims=(/nxt,nyt/)      , nflds=nconsd2d, field=consd2d)
  if (debug) call dumpnc('bilin3d.nc', 'bilin3d', dims=(/nxt,nyt,nlevs/), nflds=nbilin3d, field=bilin3d)

  ! --------------------------------
  ! remap packed arrays. this is the first
  ! place where the destination grid is used
  ! --------------------------------

  dstgrid = '0p25'
  allocate(rgb2d(nxr*nyr,nbilin2d))
  allocate(rgc2d(nxr*nyr,nconsd2d))
  allocate(rgb3d(nxr*nyr,nlevs,nbilin3d))

  wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
  print '(a)','remapping 2D fields bilinear with '//trim(wgtsfile)
  call remap(trim(wgtsfile), dim3=nbilin2d, src_field=bilin2d, dst_field=rgb2d)

  ! if (debug) then
  !    fout = 'rgbilin2d.nc'
  !    rc = nf90_create(trim(fout), nf90_clobber, ncid)
  !    rc = nf90_def_dim(ncid, 'nx', nxr,     idimid)
  !    rc = nf90_def_dim(ncid, 'ny', nyr,     jdimid)
  !    rc = nf90_def_dim(ncid, 'nk', nbilin2d, kdimid)
  !    rc = nf90_def_var(ncid, 'bilin2d', nf90_float, (/idimid,jdimid,kdimid/), varid)
  !    rc = nf90_enddef(ncid)
  !    allocate(tmp3d(nxr,nyr,nbilin2d))
  !    tmp3d(:,:,:) =  reshape(bilin2d,(/nxt,nyt,nbilin2d/))
  !    rc = nf90_put_var(ncid, varid, tmp3d)
  !    rc = nf90_close(ncid)
  !    print *,'wrote ',trim(fout)
  !    deallocate(tmp3d)
  ! end if

  wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.conserve.nc'
  print '(a)','remapping 2D fields conserv with '//trim(wgtsfile)
  call remap(trim(wgtsfile), dim3=nconsd2d, src_field=consd2d, dst_field=rgc2d)

  wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
  print '(a)','remapping 3D fields bilinear with '//trim(wgtsfile)
  call remap(trim(wgtsfile), nk=nlevs, nflds=nbilin3d, src_field=bilin3d, dst_field=rgb3d)

  if (debug) call dumpnc('rgbilin2d.nc', 'rgbilin2d', dims=(/nxr,nyr/)      , nflds=nbilin2d, field=rgb2d)
  if (debug) call dumpnc('rgconsd2d.nc', 'rgconsd2d', dims=(/nxr,nyr/)      , nflds=nconsd2d, field=rgc2d)
  if (debug) call dumpnc('rgbilin3d.nc', 'rgbilin3d', dims=(/nxt,nyt,nlevs/), nflds=nbilin3d, field=rgb3d)

  ! if (debug) then
  !    fout = 'rgcons2d.nc'
  !    rc = nf90_create(trim(fout), nf90_clobber, ncid)
  !    rc = nf90_def_dim(ncid, 'nx', nxr,     idimid)
  !    rc = nf90_def_dim(ncid, 'ny', nyr,     jdimid)
  !    rc = nf90_def_dim(ncid, 'nk', nconsd, kdimid)
  !    rc = nf90_def_var(ncid, 'consd', nf90_float, (/idimid,jdimid,kdimid/), varid)
  !    rc = nf90_enddef(ncid)
  !    allocate(tmp3d(nxr,nyr,nconsd))
  !    tmp3d(:,:,:) =  reshape(consd2d,(/nxt,nyt,nconsd/))
  !    rc = nf90_put_var(ncid, varid, tmp3d)
  !    rc = nf90_close(ncid)
  !    print *,'wrote ',trim(fout)
  !    deallocate(tmp3d)
  ! end if

  !wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
  !print '(a)','remapping 3D fields bilinear with '//trim(wgtsfile)
  !call remap(trim(wgtsfile), nk=nlevs, nflds=nbilin3d, src_field=bilin3d, dst_field=rgb3d)

#ifdef test
  ! --------------------------------
  ! regrid the 3D mask to obtain the interpolation mask.
  ! mask3d contain 1's on land and 0's at valid points.
  ! when remapped, any mask value > 0 identifies land values that
  ! have crept into the field. remapped model fields are then
  ! masked with this interpolation mask
  ! --------------------------------

  call getfield(trim(filesrc), 'temp', dims=(/nxt,nyt,nlevs/), field=mask3d)
  print *,minval(mask3d),maxval(mask3d)
  ! set mask to 0 on ocean, 1 on land on source grid
  where(mask3d .eq. vfill)mask3d = 1.0
  where(mask3d .ne.   1.0)mask3d = 0.0
  print *,minval(mask3d),maxval(mask3d)

  if (debug) then
     fout = 'mask3d.nc'
     rc = nf90_create(trim(fout), nf90_clobber, ncid)
     rc = nf90_def_dim(ncid, 'nx', nxt,     idimid)
     rc = nf90_def_dim(ncid, 'ny', nyt,     jdimid)
     rc = nf90_def_dim(ncid, 'nk', nlevs,   kdimid)
     rc = nf90_def_var(ncid, 'mask3d', nf90_float, (/idimid,jdimid,kdimid/), varid)
     rc = nf90_enddef(ncid)
     allocate(tmp3d(nxt,nyt,nlevs))
     tmp3d(:,:,:) =  reshape(mask3d,(/nxt,nyt,nlevs/))
     rc = nf90_put_var(ncid, varid, tmp3d)
     rc = nf90_close(ncid)
     print *,'wrote ',trim(fout)
     deallocate(tmp3d)
  end if

  rgmask3d = 0.0
  !call remap(trim(wgtsfile),nk=nlevs,src_field=mask3d,dst_field=rgmask3d)
  print *,minval(rgmask3d),maxval(rgmask3d)
  ! set interpolation mask 0.0 on land, 1.0 on ocean on destination grids
  where(rgmask3d > 0.0)rgmask3d = vfill
  where(rgmask3d /= vfill)rgmask3d = 1.0

  if (debug) then
     fout = 'rgmask3d.nc'
     rc = nf90_create(trim(fout), nf90_clobber, ncid)
     rc = nf90_def_dim(ncid, 'nx', nxr,     idimid)
     rc = nf90_def_dim(ncid, 'ny', nyr,     jdimid)
     rc = nf90_def_dim(ncid, 'nk', nlevs,   kdimid)
     rc = nf90_def_var(ncid, 'mask3d', nf90_float, (/idimid,jdimid,kdimid/), varid)
     rc = nf90_enddef(ncid)
     out3d(:,:,:) = reshape(rgmask3d,(/nxr,nyr,nlevs/))
     rc = nf90_put_var(ncid, varid, out3d)
     rc = nf90_close(ncid)
     print *,'wrote ',trim(fout)
  end if
  !stop

  ! lat,lon of destination grid can be obtained from xc_b,yc_b in wgtsfile
  call getfield(trim(wgtsfile), 'xc_b', dims=(/nxr*nyr,1/), field=out1d)
  dstlon = reshape(out1d,(/nxr,nyr/))
  call getfield(trim(wgtsfile), 'yc_b', dims=(/nxr*nyr,1/), field=out1d)
  dstlat = reshape(out1d,(/nxr,nyr/))

  ! --------------------------------
  ! mask the mapped fields
  ! --------------------------------

  do n = 1,nbilin2d
     !where(rgmask3d(:,1) .eq. vfill)rgb2d(:,n) = vfill
  end do
  do n = 1,nconsd
     !where(rgmask3d(:,1) .eq. vfill)rgc2d(:,n) = vfill
  end do
  do n = 1,nbilin3d
     !where(rgmask3d(:,:) .eq. vfill)rgb3d(:,:,n) = vfill
  end do

  ! --------------------------------
  ! write the mapped fields
  ! --------------------------------

  fout = 'test.nc'
  print *,timestamp

  rc = nf90_create(trim(fout), nf90_clobber, ncid)
  rc = nf90_def_dim(ncid, 'longitude', nxr, idimid)
  rc = nf90_def_dim(ncid, 'latitude',  nyr, jdimid)
  rc = nf90_def_dim(ncid,  'z_l',    nlevs, kdimid)
  rc = nf90_def_dim(ncid,  'z_i',  nlevs+1, edimid)
  rc = nf90_def_dim(ncid, 'time', nf90_unlimited, timid)

  ! define the time variable
  rc = nf90_def_var(ncid, 'time', nf90_double, (/timid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', trim(timeunit))
  rc= nf90_put_att(ncid, varid, 'calendar', trim(timecal))

  rc = nf90_def_var(ncid, 'longitude', nf90_float,  (/idimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'degrees_east')

  rc = nf90_def_var(ncid, 'latitude', nf90_float,  (/jdimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'degrees_north')

  rc = nf90_def_var(ncid, 'z_l', nf90_float,  (/kdimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'm')
  rc = nf90_put_att(ncid, varid, 'postive', 'down')

  rc = nf90_def_var(ncid, 'z_i', nf90_float,  (/edimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'm')
  rc = nf90_put_att(ncid, varid, 'postive', 'down')

  do n = 1,nbilin2d
    vname = trim(b2d(n)%output_var_name)
    vunit = trim(b2d(n)%units)
    vlong = trim(b2d(n)%long_name)
    vfill = b2d(n)%var_fillvalue
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,timid/), varid)
    rc = nf90_put_att(ncid, varid,     'units',  vunit)
    rc = nf90_put_att(ncid, varid, 'long_name',  vlong)
    rc = nf90_put_att(ncid, varid, '_FillValue', vfill)
 enddo
#ifdef test
 do n = 1,nconsd
    vname = trim(c2d(n)%output_var_name)
    vunit = trim(c2d(n)%units)
    vlong = trim(c2d(n)%long_name)
    vfill = c2d(n)%var_fillvalue
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,timid/), varid)
    rc = nf90_put_att(ncid, varid,     'units',  vunit)
    rc = nf90_put_att(ncid, varid, 'long_name',  vlong)
    rc = nf90_put_att(ncid, varid, '_FillValue', vfill)
 enddo
  do n = 1,nbilin3d
    vname = trim(b3d(n)%output_var_name)
    vunit = trim(b3d(n)%units)
    vlong = trim(b3d(n)%long_name)
    vfill = b3d(n)%var_fillvalue
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid,timid/), varid)
    rc = nf90_put_att(ncid, varid,     'units',  vunit)
    rc = nf90_put_att(ncid, varid, 'long_name',  vlong)
    rc = nf90_put_att(ncid, varid, '_FillValue', vfill)
 enddo
#endif
 rc = nf90_enddef(ncid)

 ! dimensions
 rc = nf90_inq_varid(ncid, 'z_l', varid)
 rc = nf90_put_var(ncid,   varid,   z_l)
 rc = nf90_inq_varid(ncid, 'z_i', varid)
 rc = nf90_put_var(ncid,   varid,   z_i)
 rc = nf90_inq_varid(ncid, 'longitude', varid)
 rc = nf90_put_var(ncid,   varid, dstlon(:,1))
 rc = nf90_inq_varid(ncid,  'latitude', varid)
 rc = nf90_put_var(ncid,   varid, dstlat(1,:))
 ! time
 rc = nf90_inq_varid(ncid, 'time', varid)
 rc = nf90_put_var(ncid, varid, timestamp)

 do n = 1,nbilin2d
    out2d(:,:) = reshape(rgb2d(:,n), (/nxr,nyr/))
    vname = trim(b2d(n)%output_var_name)
    rc = nf90_inq_varid(ncid, vname, varid)
    rc = nf90_put_var(ncid,  varid, out2d)
 end do
#ifdef test
 do n = 1,nconsd
    out2d(:,:) = reshape(rgc2d(:,n), (/nxr,nyr/))
    vname = trim(c2d(n)%output_var_name)
    rc = nf90_inq_varid(ncid, vname, varid)
    rc = nf90_put_var(ncid,  varid, out2d)
 end do
 do n = 1,nbilin3d
    out3d(:,:,:) = reshape(rgb3d(:,:,n), (/nxr,nyr,nlevs/))
    vname = trim(b3d(n)%output_var_name)
    rc = nf90_inq_varid(ncid, vname, varid)
    rc = nf90_put_var(ncid,  varid, out3d)
 end do
#endif
 rc = nf90_close(ncid)
#endif
 print *,'all done!'

end program ocnpost
