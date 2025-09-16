module weights4rhs

  use ESMF

  implicit none

  !private
  public
  !public :: addmask2grid

  character(len=*) , parameter :: u_FILE_u = __FILE__

  ! from ATM to ocn/ice and wav
  integer, parameter :: na2omaps = 4
  character(len=12), dimension(na2omaps) :: a2omaps = (/ &
       'bilnr       ',&
       'consf       ',&
       'consf_uv3d  ',&
       'patch_uv3d  '/)

  integer, parameter :: na2wmaps = 1
  character(len=12), dimension(na2wmaps) :: a2wmaps = (/ &
       'bilnr       '/)

  ! from OCN/ICE to atm and wav
  integer, parameter :: no2amaps = 2
  character(len=12), dimension(no2amaps) :: o2amaps = (/ &
       'consf       ', &
       'consd       '/)

  integer, parameter :: no2wmaps = 1
  character(len=12), dimension(no2wmaps) :: o2wmaps = (/ &
       'bilnr_nstod '/)

  ! from WAV to atm and ocn/ice
  integer, parameter :: nw2amaps = 1
  character(len=12), dimension(nw2amaps) :: w2amaps = (/ &
       'bilnr_nstod '/)

  integer, parameter :: nw2omaps = 1
  character(len=12), dimension(nw2omaps) :: w2omaps = (/ &
       'bilnr_nstod '/)
contains

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

  subroutine addmask2grid(fname, fldname, atmgrid)

    character(len=*), intent(in)    :: fname
    character(len=*), intent(in)    :: fldname
    type(ESMF_Grid),  intent(inout) :: atmgrid

    ! local variable
    type(ESMF_Field)                    :: gridfld
    type(ESMF_Field)                    :: maskfld
    real(kind=ESMF_KIND_R8), pointer    :: ptr2dr8(:,:)
    integer(kind=ESMF_KIND_I4), pointer :: maskptr(:,:)
    type(ESMF_ArraySpec)                :: arraySpec
    integer                             :: i,j,rc
    ! from ocean_merge
    real(kind=ESMF_KIND_R8)             :: tmpland
    real(kind=ESMF_KIND_R8), parameter  :: min_land = 1.0e-4

    !---------------------------------------------------------------------
    ! obtain land_frac from tile file to create the mask
    !---------------------------------------------------------------------

    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    gridfld = ESMF_FieldCreate(atmgrid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(gridfld, farrayPtr=ptr2dr8, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldRead(gridfld, filename=trim(fname), variableName=trim(fldname), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    maskfld = ESMF_FieldCreate(atmgrid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(maskfld, farrayPtr=maskptr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !---------------------------------------------------------------------
    ! add the mask to the grid
    !---------------------------------------------------------------------

    ! add the mask to the grid
    call ESMF_GridAddItem(atmgrid, itemflag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
         staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridGetItem(atmgrid, itemflag=ESMF_GRIDITEM_MASK, staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=maskPtr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! replicate ocean_merge; the land_frac variable from the file is the mapped ocean fraction
    maskptr = 0
    do j = lbound(maskptr,2),ubound(maskptr,2)
       do i = lbound(maskptr,1),ubound(maskptr,1)
          tmpland = 1.0 - ptr2dr8(i,j)
          if (tmpland <       min_land) maskptr(i,j) = 0
          if (tmpland > 1.0 - min_land) maskptr(i,j) = 1
       end do
    end do
  end subroutine addmask2grid


  logical function ChkErr(rc, line, file)

    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    integer :: lrc

    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       ChkErr = .true.
    endif
  end function ChkErr

end module weights4rhs
