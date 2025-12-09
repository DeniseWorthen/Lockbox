module atmmesh

  use ESMF
  use netcdf
  use charstrings, only: logmsg, fv3dir

  implicit none

contains

  subroutine write_atmmesh(fsrc,atmres,npx,atmmask)

    character(len=*), intent(in) :: fsrc
    character(len=*), intent(in) :: atmres
    integer         , intent(in) :: npx
    integer         , intent(in) :: atmmask(:)

    type(ESMF_Grid)      :: atmGrid
    type(ESMF_Mesh)      :: atmMesh
    type(ESMF_Array)     :: elemMaskArray
    type(ESMF_DistGrid)  :: Distgrid
    integer(i4), pointer :: meshmask(:)

    atmGrid = ESMF_GridCreateMosaic(filename=trim(fsrc),    &
         tileFilePath=trim(fv3dir)//'/'//trim(atmres)//'/', &
         staggerLocList = (/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! add the mask to the grid
    call ESMF_GridAddItem(atmGrid, itemflag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
         staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    logmsg = 'creating AtmGrid from '//trim(fsrc)
    print '(a)',trim(logmsg)

    atmMesh = ESMF_MeshCreate(atmGrid, trim(atmres)//'_mesh', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! fill mesh mask
    call ESMF_MeshGet(atmMesh, elementDistgrid=Distgrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_DistGridGet(Distgrid, localDe=0, elementCount=ncnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    allocate(meshmask(ncnt))
    elemMaskArray = ESMF_ArrayCreate(Distgrid, farrayPtr=meshmask, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_MeshGet(atmMesh, elemMaskArray=elemMaskArray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    meshmask(:) = atmmask(:)
    call ESMF_MeshSet(mesh=atmMesh, elementMask=meshmask, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_MeshWrite(atmMesh,'test.nc',rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine write_atmmesh
end module atmmesh
