program ocnpost

  use utils_mod
  use vartypedefs
  use netcdf

  implicit none

  character(len=240) :: filesrc, filedst, wgtsfile, fout
  !character(len=120) :: wgtsdir = '/scratch1/NCEPDEV/climate/Denise.Worthen/grids-test-202210/'
  character(len=120) :: wgtsdir = '/work/noaa/nems/dworthen/grids-test-202210/'

  integer, parameter :: nxt = 1440, nyt = 1080, nlevs = 40  !tripole dimensions, 1/4deg
  integer, parameter :: nxr = 1440, nyr =  721              !regular grid dimensions, 1/4deg
  real   , parameter :: fval = -1.e+34                      !need to get

  integer :: i,j,k,nn,nvalid
  integer :: rc,ncid,varid,dimid
  integer :: nbilin2d, nbilin3d, nconsd
  character(len=20) :: vname, vlong, vunit
  integer :: idimid, jdimid, kdimid
  logical :: vpair(maxvars)

  real, dimension(nxt*nyt)       :: cosrot, sinrot
  real, dimension(nlevs)         :: zlevs

  real, allocatable, dimension(:,:)   :: vecpair
  real, allocatable, dimension(:,:,:) :: vecpair2d
  real, allocatable, dimension(:,:)   :: bilin2d, consd2d
  real, allocatable, dimension(:,:,:) :: bilin3d

  real, dimension(nxt*nyt,nlevs) :: mask3d
  real, dimension(nxr*nyr,nlevs) :: rgmask3d
  real, dimension(nxr,nyr) :: dstlon
  real, dimension(nxr,nyr) :: dstlat

  real, dimension(nxr*nyr)       :: out1d
  real, dimension(nxr,nyr,nlevs) :: out3d

  logical :: debug = .true.

  filesrc = 'ocn_2013_04_01_03.nc'
  call ocnvars_typedefine

  ! open the source file and obtain the units and long name and rotation angles
  nvalid = 0
  vpair(:) = .false.
  do i = 1,maxvars
     if (len_trim(ocnvars(i)%input_var_name) > 0 ) then
        rc = nf90_open(trim(filesrc), nf90_nowrite, ncid)
        rc = nf90_inq_varid(ncid, trim(ocnvars(i)%input_var_name), varid)
        rc = nf90_get_att(ncid, varid, "long_name", ocnvars(i)%long_name)
        rc = nf90_get_att(ncid, varid, "units", ocnvars(i)%units)
        if (len_trim(ocnvars(i)%var_pair) > 0) vpair(i) = .true.
        nvalid = nvalid+1
     end if
  end do
  call getfield(trim(filesrc), 'cos_rot', dims=(/nxt,nyt/), field=cosrot)
  call getfield(trim(filesrc), 'sin_rot', dims=(/nxt,nyt/), field=sinrot)
  call getfield(trim(filesrc), 'z_l', dims=(/nlevs,1/), field=zlevs)
  print *,zlevs

  ! count numbers of fields to remapped for each mapping type; these can be remapped as packed arrays
  nbilin2d = 0; nbilin3d = 0; nconsd = 0
  do i = 1,nvalid
     if (trim(ocnvars(i)%var_remapmethod)  == 'bilinear') then
        if (ocnvars(i)%var_dimen == 2) nbilin2d = nbilin2d + 1
        if (ocnvars(i)%var_dimen == 3) nbilin3d = nbilin3d + 1
     end if
     if (trim(ocnvars(i)%var_remapmethod)  == 'conserve')nconsd = nconsd + 1  !no 3d variables w/ conservative mapping
     !print *,trim(ocnvars(i)%var_remapmethod),trim(ocnvars(i)%input_var_name),nbilin2d,nbilin3d,nconsd
  end do
  print *,'bilin 2d ',nbilin2d,' bilin 3d ',nbilin3d,' conserv 2d ',nconsd
  allocate(bilin2d(nxt*nyt,nbilin2d))
  allocate(bilin3d(nxt*nyt,nlevs,nbilin3d))
  allocate(consd2d(nxt*nyt,nconsd))

  ! --------------------------------
  ! create packed arrays for mapping
  ! --------------------------------
#ifdef test
  ! obtain any 2D vector pairs on Ct grid for bilinear mapping
  do i = 1,nvalid
     if (trim(ocnvars(i)%var_remapmethod)  == 'bilinear' .and. ocnvars(i)%var_dimen == 2) then
        if (vpair(i)) then
           allocate(vecpair(nxt*nyt,2))
           call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot, &
                trim(ocnvars(i)%input_var_name), trim(ocnvars(i)%var_grid),&
                trim(ocnvars(i)%var_pair), trim(ocnvars(i)%var_pair_grid), &
                dims=(/nxt,nyt/), vecpair=vecpair)
           vpair(i+1) = .false.
        end if
     end if
  end do

  nn = 0
  do i = 1,nvalid
     if (trim(ocnvars(i)%var_remapmethod)  == 'bilinear' .and. ocnvars(i)%var_dimen == 2) then
        if (len_trim(ocnvars(i)%var_pair) == 0) then
           nn = nn + 1
           vname = trim(ocnvars(i)%input_var_name)
           call getfield(trim(filesrc), vname, dims=(/nxt,nyt/), field=bilin2d(:,nn))
        else ! fill with vector pairs
           nn = nn+1
           if (trim(ocnvars(i)%var_grid) == 'Cu')bilin2d(:,nn) = vecpair(:,1)
           if (trim(ocnvars(i)%var_grid) == 'Cv')bilin2d(:,nn) = vecpair(:,2)
        end if
     end if
  end do
  print *,'packed 2d bilin ',nn

  ! obtain any 2D vector pairs on Ct grid for conserv mapping
  do i = 1,nvalid
     if (trim(ocnvars(i)%var_remapmethod)  == 'conserv' .and. ocnvars(i)%var_dimen == 2) then
        if (vpair(i)) then
           allocate(vecpair(nxt*nyt,2))
           call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot, &
                trim(ocnvars(i)%input_var_name), trim(ocnvars(i)%var_grid),&
                trim(ocnvars(i)%var_pair), trim(ocnvars(i)%var_pair_grid), &
                dims=(/nxt,nyt/), vecpair=vecpair)
           vpair(i+1) = .false.
        end if
     end if
  end do

  nn = 0
  do i = 1,nvalid
     print *,i
     if (trim(ocnvars(i)%var_remapmethod)  == 'conserv' .and. ocnvars(i)%var_dimen == 2) then
        if (len_trim(ocnvars(i)%var_pair) == 0) then
           nn = nn + 1
           vname = trim(ocnvars(i)%input_var_name)
           call getfield(trim(filesrc), vname, dims=(/nxt,nyt/), field=consd2d(:,nn))
        else ! fill with vector pairs
           nn = nn+1
           if (trim(ocnvars(i)%var_grid) == 'Cu')consd2d(:,nn) = vecpair(:,1)
           if (trim(ocnvars(i)%var_grid) == 'Cv')consd2d(:,nn) = vecpair(:,2)
        end if
     end if
  end do
  print *,'packed 2d conserv ',nn

  ! obtain any 3D vector pairs on Ct grid for bilinear mapping
  do i = 1,nvalid
     if (trim(ocnvars(i)%var_remapmethod)  == 'bilinear' .and. ocnvars(i)%var_dimen == 3) then
        if (vpair(i)) then
           allocate(vecpair2d(nxt*nyt,nlevs,2))
           print *,'call getvecpair 2d'
           call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot, &
                trim(ocnvars(i)%input_var_name), trim(ocnvars(i)%var_grid),&
                trim(ocnvars(i)%var_pair), trim(ocnvars(i)%var_pair_grid), &
                dims=(/nxt,nyt,nlevs/), vecpair=vecpair2d)
           vpair(i+1) = .false.
        end if
     end if
  end do
  print *,'done vecpair3d'

  nn = 0
  do i = 1,nvalid
     if (trim(ocnvars(i)%var_remapmethod)  == 'bilinear' .and. ocnvars(i)%var_dimen == 3) then
        if (len_trim(ocnvars(i)%var_pair) == 0) then
           nn = nn + 1
           vname = trim(ocnvars(i)%input_var_name)
           call getfield(trim(filesrc), vname, dims=(/nxt,nyt,nlevs/), field=bilin3d(:,:,nn))
        else ! fill with vector pairs
           nn = nn+1
           if (trim(ocnvars(i)%var_grid) == 'Cu')bilin3d(:,:,nn) = vecpair2d(:,:,1)
           if (trim(ocnvars(i)%var_grid) == 'Cv')bilin3d(:,:,nn) = vecpair2d(:,:,2)
        end if
     end if
  end do
  print *,'packed 3d bilin ',nn
#endif
  !
  ! remap packed array
  !
  ! make remapped mask first using 3-d temp array
  call getfield(trim(filesrc), 'temp', dims=(/nxt,nyt,nlevs/), field=mask3d)
  print *,size(mask3d,1),size(mask3d,2)

  print *,minval(mask3d),maxval(mask3d)
  ! set mask to 0 on ocean, 1 on land
  where(mask3d .eq. fval)mask3d = 1.0
  where(mask3d .ne.  1.0)mask3d = 0.0
  print *,minval(mask3d),maxval(mask3d)

  wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.0p25.bilinear.nc'
  print '(a)',trim(wgtsfile)
  ! lat,lon of destination grid can be obtained from xc_b,yc_b in wgtsfile
  call getfield(trim(wgtsfile), 'xc_b', dims=(/nxr*nyr,1/), field=out1d)
  dstlon = reshape(out1d,(/nxr,nyr/))

  call getfield(trim(wgtsfile), 'yc_b', dims=(/nxr*nyr,1/), field=out1d)
  dstlat = reshape(out1d,(/nxr,nyr/))

  print *,'lon ',dstlon(:,1)
  print *,'lat ',dstlat(1,:)

  print *,'dstlon ',size(dstlon)
  print *,'dstlat ',size(dstlat)

  rgmask3d = 0.0
  call remap(trim(wgtsfile),nk=nlevs,src_field=mask3d,dst_field=rgmask3d)
  print *,minval(rgmask3d),maxval(rgmask3d)
  out3d(:,:,:) = reshape(rgmask3d,(/nxr,nyr,nlevs/))

  fout = 'test.nc'

  rc = nf90_create(trim(fout), nf90_clobber, ncid)
  rc = nf90_def_dim(ncid, 'longitude',  nxr, idimid)
  rc = nf90_def_dim(ncid, 'latitude',  nyr, jdimid)
  rc = nf90_def_dim(ncid, 'depth',  nlevs, kdimid)

  rc = nf90_def_var(ncid, 'longitude', nf90_float,  (/idimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'degrees_east')

  rc = nf90_def_var(ncid, 'latitude', nf90_float,  (/jdimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'degrees_north')

  rc = nf90_def_var(ncid, 'depth', nf90_float,  (/kdimid/), varid)
  rc = nf90_put_att(ncid, varid, 'units', 'm')
  rc = nf90_put_att(ncid, varid, 'postive', 'down')
  print *,trim(nf90_strerror(rc))

  rc = nf90_def_var(ncid, 'mask', nf90_float, (/idimid,jdimid,kdimid/), varid)
  !do i = 1,nvalid
  !  vname = trim(ocnvars%output_var_name(i))
  !  vunit = trim(ocnvars%units(i))
  !  vlong = trim(ocnvars%long_name(i))
  !  rc = nf90_def_var(ncid, vname, nf90_float, dim3, varid)
  !  rc = nf90_put_att(ncid, varid,     'units', vunit)
  !  rc = nf90_put_att(ncid, varid, 'long_name', vlong)
  !  rc = nf90_put_att(ncid, varid, 'missing_value', fval)    !need to obtain
  !  rc = nf90_put_att(ncid, varid,    '_FillValue', fval)
  ! enddo
    rc = nf90_enddef(ncid)

    rc = nf90_inq_varid(ncid,     'depth', varid)
    rc = nf90_put_var(ncid,         varid, zlevs)

    rc = nf90_inq_varid(ncid, 'longitude', varid)
    rc = nf90_put_var(ncid,         varid, dstlon(:,1))

    rc = nf90_inq_varid(ncid,  'latitude', varid)
    rc = nf90_put_var(ncid,         varid, dstlat(1,:))

    rc = nf90_inq_varid(ncid,      'mask', varid)
    rc = nf90_put_var(ncid,         varid, out3d)

    rc = nf90_close(ncid)


end program ocnpost
