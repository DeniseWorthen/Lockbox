
  !---------------------------------------------------------------------
  ! create needed AtmMeshes with added mask
  !---------------------------------------------------------------------

  allocate(atmMesh(size(catm)))
  do n = 1,size(catm)
     npx = catm(n)
     if (npx < 100) then
        write(atmres,'(a,i2)')'C',npx
     elseif (npx < 1000) then
        write(atmres,'(a,i3)')'C',npx
     else
        write(atmres,'(a,i4)')'C',npx
     end if

     fsrc = trim(fv3dir)//'/'//trim(atmres)//'/'//trim(atmres)//'_mosaic.nc'
     logmsg = 'creating AtmGrid from '//trim(fsrc)
     if (maintask) print '(a)',trim(logmsg)

     atmGrid = ESMF_GridCreateMosaic(filename=trim(fsrc),    &
          tileFilePath=trim(fv3dir)//'/'//trim(atmres)//'/', &
          staggerLocList = (/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     fsrc = trim(dirout)//'/'//trim(atmres)//'.mx'//trim(res)//'.tile*.nc'
     logmsg = 'adding land_frac from  '//trim(fsrc)//' to grid'
     if (maintask) print '(a)',trim(logmsg)
     call addmask2grid(trim(fsrc), 'land_frac', atmGrid)
     ! create needed atmMeshes
     atmMesh(n) = ESMF_MeshCreate(atmGrid, trim(atmres)//'_mesh', rc=rc)
  end do

  call create_weights4rhs()
  ! do n = 1,size(catm)
  !    npx = catm(n)
  !    if (npx < 100) then
  !       write(atmres,'(a,i2)')'C',npx
  !    elseif (npx < 1000) then
  !       write(atmres,'(a,i3)')'C',npx
  !    else
  !       write(atmres,'(a,i4)')'C',npx
  !    end if

  !    meshatm = atmMesh(n)

  !    ! ocn/ice
  !    meshname = '/scratch3/NCEPDEV/global/role.glopara/fix/cice/20240416/100/mesh.mx100.nc'
  !    if (maintask) print '(a)', trim(meshname)
  !    meshocn = ESMF_MeshCreate(filename=trim(meshname), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
  !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !    ! wave
  !    wavres = 'global_270k'
  !    meshname = &
  !         '/scratch4/NAGAPE/epic/role-epic/UFS-WM_RT/NEMSfv3gfs/input-data-20250507/' &
  !         //'WW3_input_data_20250807/mesh.'//trim(wavres)//'.nc'
  !    if (maintask) print '(a)',trim(meshname)
  !    meshwav = ESMF_MeshCreate(filename=trim(meshname), fileformat=ESMF_FILEFORMAT_ESMFMESH,rc=rc)
  !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !    ! src:dst:method

  !    !a->o
  !    ftag = trim(atmres)//'.to.mx'//trim(res)
  !    do nn = 1,na2omaps
  !       maptype = trim(a2omaps(nn))
  !       fwgt = trim(dirout)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
  !       if (maintask) print '(a)','XXX '//trim(fwgt)
  !       !call create_weights(meshatm, meshocn, masksrc=ispval, maskdst=0, method, fwgt)
  !    end do

  !    !a->w
  !    ftag = trim(atmres)//'.to.'//trim(wavres)
  !    do nn = 1,na2wmaps
  !       maptype = trim(a2wmaps(nn))
  !       fwgt = trim(dirout)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
  !       if (maintask) print '(a)','XXX '//trim(fwgt)
  !       !call create_weights(meshsrc, meshdst, masksrc=ispval, maskdst=0, method, fwgt)
  !    end do

  !    !o->a
  !    ftag = 'mx'//trim(res)//'.to.'//trim(atmres)
  !    do nn = 1,no2amaps
  !       maptype = trim(o2amaps(nn))
  !       fwgt = trim(dirout)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
  !       if (maintask) print '(a)','XXX '//trim(fwgt)
  ! 	!call create_weights(meshocn, meshatm, masksrc=0, maskdst=1, method, fwgt)
  !    end do

  !    !o->w
  !    ftag = 'mx'//trim(res)//'.to.'//trim(wavres)
  !    do nn = 1,no2wmaps
  !       maptype = trim(o2wmaps(nn))
  !       fwgt = trim(dirout)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
  !       if (maintask) print '(a)','XXX '//trim(fwgt)
  !       !call create_weights(meshsrc, meshdst, masksrc=0, maskdst=0, method, fwgt)
  !    end do

  !    !w->a
  !    ftag = trim(wavres)//'.to.'//trim(atmres)
  !    do nn = 1,nw2amaps
  !       maptype = trim(w2amaps(nn))
  !       fwgt = trim(dirout)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
  !       if (maintask) print '(a)','XXX '//trim(fwgt)
  !       !call create_weights(meshsrc, meshdst, masksrc=0, maskdst=1, method, fwgt)
  !    end do

  !    !w->o
  !    ftag = trim(wavres)//'.to.'//'mx'//trim(res)
  !    do nn = 1,no2wmaps
  !       maptype = trim(w2omaps(nn))
  !       fwgt = trim(dirout)//'/'//trim(ftag)//'.'//trim(maptype)//'.nc'
  !       if (maintask) print '(a)','XXX '//trim(fwgt)
  !       !call create_weights(meshsrc, meshdst, masksrc=0, maskdst=0, method, fwgt)
  !    end do
  ! end do
