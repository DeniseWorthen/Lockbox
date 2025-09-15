module weights4rhs

  use ESMF

  implicit none

  private

  public :: addmask2grid

  character(len=*) , parameter :: u_FILE_u = __FILE__

contains

  subroutine create_weights(srcmesh, dstmesh, srcmask, dstmask, method, fname, polemethod)

    fldsrc = ESMF_FieldGet(srcmesh,....
    fdd
    !characer(len=*), intent(in), optional :: polemethod
    !type(ESMF_PoleMethod_Flag), intent(in), optional :: polemethod

    logical :: lpole
    if (present (polemethod)) then
       lpolemethod = polemethod
    else
       lpolemethod = polemethodall
    end if
    !if (method == 'consf') then...
    !if (method == 'consd') then...
    !if (method == 'bilnr') then
    !if (method == 'bilnr_nstod) then
    !if (method == 'patch_uv3d')then


  end subroutine create_weights

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
    !print *,'XXX bounds 2dr8 ',lbound(ptr2dr8,1),ubound(ptr2dr8,1),lbound(ptr2dr8,2),ubound(ptr2dr8,2)

    call ESMF_FieldRead(gridfld, filename=trim(fname), variableName=trim(fldname), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !print *,'XXX ',minval(ptr2dr8), maxval(ptr2dr8)

    call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_I4, rank=2, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    maskfld = ESMF_FieldCreate(atmgrid, arraySpec, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_FieldGet(maskfld, farrayPtr=maskptr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    !print *,'XXX bounds maskptr ',lbound(maskptr,1),ubound(maskptr,1),lbound(maskptr,2),ubound(maskptr,2)

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

    ! replicate ocean_merge
    maskptr = 0
    do j = lbound(maskptr,2),ubound(maskptr,2)
       do i = lbound(maskptr,1),ubound(maskptr,1)
          tmpland = 1.0 - ptr2dr8(i,j)                     ! the land_frac variable from the file is the mapped ocean fraction
          if (tmpland <       min_land) maskptr(i,j) = 0
          if (tmpland > 1.0 - min_land) maskptr(i,j) = 1
       end do
    end do
    !print *,'XXX maskptr ',minval(maskptr), maxval(maskptr)
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
