subroutine med_map_routehandles_initfrom_field(n1, n2, fldsrc, flddst, mapindex, routehandles, &
     mapfile, dstatfield, rc)

  use ESMF                  , only : ESMF_RouteHandle, ESMF_RouteHandlePrint, ESMF_Field, ESMF_MAXSTR
  use ESMF                  , only : ESMF_PoleMethod_Flag, ESMF_POLEMETHOD_ALLAVG, ESMF_POLEMETHOD_NONE
  use ESMF                  , only : ESMF_FieldSMMStore, ESMF_FieldRedistStore, ESMF_FieldRegridStore
  use ESMF                  , only : ESMF_RouteHandleIsCreated, ESMF_RouteHandleCreate, ESMF_RouteHandleWrite
  use ESMF                  , only : ESMF_REGRIDMETHOD_BILINEAR, ESMF_REGRIDMETHOD_PATCH
  use ESMF                  , only : ESMF_REGRIDMETHOD_CONSERVE, ESMF_NORMTYPE_DSTAREA, ESMF_NORMTYPE_FRACAREA
  use ESMF                  , only : ESMF_UNMAPPEDACTION_IGNORE, ESMF_REGRIDMETHOD_NEAREST_STOD
  use ESMF                  , only : ESMF_EXTRAPMETHOD_NEAREST_STOD, ESMF_FAILURE
  use ESMF                  , only : ESMF_Mesh, ESMF_MeshLoc, ESMF_MESHLOC_ELEMENT, ESMF_TYPEKIND_I4
  use ESMF                  , only : ESMF_MeshGet, ESMF_DistGridGet, ESMF_DistGrid, ESMF_TYPEKIND_R8
  use ESMF                  , only : ESMF_FieldGet, ESMF_FieldCreate, ESMF_FieldIsCreated, ESMF_FieldDestroy
  use med_internalstate_mod , only : mapbilnr, mapconsf, mapconsd, mappatch, mappatch_uv3d, mapbilnr_uv3d, mapfcopy
  use med_internalstate_mod , only : mapunset, mapnames, nmappers
  use med_internalstate_mod , only : mapnstod, mapnstod_consd, mapnstod_consf, mapnstod_consd
  use med_internalstate_mod , only : mapfillv_bilnr, mapbilnr_nstod, mapconsf_aofrac, mapconsf_uv3d
  use med_internalstate_mod , only : compocn, compwav, complnd, compname, compatm
  use med_internalstate_mod , only : coupling_mode, write_dststatus, rw_routehandles
  use med_internalstate_mod , only : defaultMasks
  use med_constants_mod     , only : ispval_mask => med_constants_ispval_mask

  ! input/output variables
  integer                    , intent(in)    :: n1
  integer                    , intent(in)    :: n2
  type(ESMF_Field)           , intent(inout) :: fldsrc
  type(ESMF_Field)           , intent(inout) :: flddst
  integer                    , intent(in)    :: mapindex
  type(ESMF_RouteHandle)     , intent(inout) :: routehandles(:)
  character(len=*), optional , intent(in)    :: mapfile
  type(ESMF_Field), optional , intent(out)   :: dstatfield
  integer                    , intent(out)   :: rc

  ! local variables
  type(ESMF_Mesh)            :: mesh_dst
  type(ESMF_Field)           :: lfield
  character(len=CS)          :: string
  character(len=CS)          :: mapname
  character(len=CS)          :: dstatname, rhname
  integer                    :: srcMaskValue
  integer                    :: dstMaskValue
  real(R8), pointer          :: r8ptr(:)
  integer(I4), pointer       :: i4ptr(:)
  character(len=ESMF_MAXSTR) :: lmapfile
  character(len=ESMF_MAXSTR) :: rh_filename =''
  logical                    :: rhprint = .false.
  logical                    :: rh_file_exists
  integer                    :: srcTermProcessing_Value = 0
  type(ESMF_PoleMethod_Flag) :: polemethod
  character(len=*), parameter :: subname=' (module_med_map: med_map_routehandles_initfrom_field) '
  !---------------------------------------------

  rc = ESMF_SUCCESS
  lmapfile = 'unset'
  if (present(mapfile)) then
     lmapfile = trim(mapfile)
  end if

  mapname = trim(mapnames(mapindex))
  call ESMF_LogWrite(trim(subname)//": mapname "//trim(mapname), ESMF_LOGMSG_INFO)

  ! create a name for the dststatus field and/or saved RH file
  ! set saved_name(n1,n2,mapname)
  !if (mapindex == mapnstod_consd) then
  !    dstatname = trim(compname(n1))//'_'//trim(compname(n2))//'_consd'
  ! else if (mapindex == mapnstod_consf) then
  !    dstatname = trim(compname(n1))//'_'//trim(compname(n2))//'_consf'
  ! else
  !    dstatname = trim(compname(n1))//'_'//trim(compname(n2))//'_'//mapname
  !end if
  ! mapindex is map_nstodconsf etc to
  if (maintask) print *,'XXX0 '//trim(dstatname)//'   '//trim(mapname)

  call ESMF_FieldGet(flddst, mesh=mesh_dst, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  lfield = ESMF_FieldCreate(mesh_dst, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(dstatname), rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  if (rw_routehandles) then
     if ((mapindex == mapnstod_consd) .or  (mapindex == mapnstod_consf)) then
        call rhfiles(compname(n1),compname(n2),'nstod',routehandles(mapnstod), 'read', isCreated)
        if (isCreated) then
           if (mapindex == mapnstod_consd) then
              call rhfiles(compname(n1),compname(n2),'consd',routehandles(mapconsd), 'read', isCreated)
              if (isCreated) return
           else if (mapindex == mapnstod_consf) then
              call rhfiles(compname(n1),compname(n2),'consf',routehandles(mapconsf), 'read', isCreated)
              if (isCreated) return
           end if
        end if
     else
        call rhfiles(compname(n1),compname(2),trim(mapname),routehandles(mapindex), 'read', isCreated)
        if (isCreated) return
     endif
  end if


  !    rh_filename = 'cmeps.rh_'//trim(dstatname)
  !    inquire(FILE=trim(rh_filename), EXIST=rh_file_exists)
  !    if (rh_file_exists) then
  !       if (maintask) write(logunit,'(A)') trim(subname)//' Reading RH from file: '//trim(rh_filename)
  !       if (write_dststatus) then
  !          if (maintask) write(logunit,'(A)') 'ERROR: dststatus not available when RHs are read from file'
  !          rc = ESMF_FAILURE
  !          return
  !       end if
  !       routehandles(mapindex) = ESMF_RouteHandleCreate(fileName=trim(rh_filename), rc=rc)
  !       if (chkerr(rc,__LINE__,u_FILE_u)) return
  !       return
  !    end if
  ! end if

  ! Set masking

  ! Create route handle
  if (mapindex == mapfcopy) then
  end if
  !if (maintask) print *,'XXX done w/ RH creation '//trim(dstatname)

  ! Save route handle to file if requested; for conserv+nstod methods, need to
  ! first save the conservative RH
  if (rw_routehandles) then
     if (mapindex == mapnstod_consd) then
        call rhfiles(compname(n1),compname(n2),'consd',routehandles(mapconsd), 'write')
     else if (mapindex == mapnstod_consf) then
        call rhfiles(compname(n1),compname(n2),'consf',routehandles(mapconsf), 'write')
     else
        call rhfiles(compname(n1),compname(n2),trim(mapname),routehandles(mapindex), 'write')
     end if
  end if


  !    rh_filename = 'cmeps.rh_'//trim(dstatname)
  !    if (maintask) then
  !       write(logunit,'(a)') trim(subname)//": saving  RH for "//trim(dstatname)
  !    end if
  !    if (mapindex == mapnstod_consd) then
  !       !rh_name = 'consd'
  !       call ESMF_RouteHandleWrite(routehandles(mapconsd), fileName=trim(rh_filename), rc=rc)
  !       if (chkerr(rc,__LINE__,u_FILE_u)) return
  !    else if (mapindex == mapnstod_consf) then
  !       call ESMF_RouteHandleWrite(routehandles(mapconsf), fileName=trim(rh_filename), rc=rc)
  !       if (chkerr(rc,__LINE__,u_FILE_u)) return
  !    else
  !       call ESMF_RouteHandleWrite(routehandles(mapindex), fileName=trim(rh_filename), rc=rc)
  !       if (chkerr(rc,__LINE__,u_FILE_u)) return
  !    end if
  ! endif
  ! if(maintask) print *,'XXX done w/ RH write '//trim(dstatname)

  ! Copy dstatus field R8 values into a returned field; for nstod+conservative this will just be the conservative
  if (present(dstatfield)) then
     ! reset dstatname == mapindex; this will write consf part of nstodconsf to that name, which will
     ! be a bit confusing, but I"m the only one using this and I can add a note in med write to clarify?
     ! or I could actually combine the fields in med writing?
     ! dstatname == mapindex
     !
     dstatfield = ESMF_FieldCreate(mesh_dst, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
          name=trim(dstatname), rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
     call ESMF_FieldGet(lfield, farrayPtr=i4ptr, rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
     call ESMF_FieldGet(dstatfield, farrayPtr=r8ptr, rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
     r8ptr = real(i4ptr,R8)
     if (maintask) print *,'XXX done w/ dstat field '//trim(dstatname)
  end if

  ! conservative+nstod methods requires a second routehandle
  if (mapindex == mapnstod .or. mapindex == mapnstod_consd .or. mapindex == mapnstod_consf) then
     if(maintask) print *,'XXX1 '//trim(dstatname)
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapnstod), &
          srcMaskValues=(/srcMaskValue/),              &
          dstMaskValues=(/dstMaskValue/),              &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
          srcTermProcessing=srcTermProcessing_Value,   &
          ignoreDegenerate=.true.,                     &
          dstStatusField=lfield,                       &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE,   &
          rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return

     ! Save nstod route handle to file if requested
     if (rw_routehandles) then
        call rhfiles(compname(n1),compname(n2),'nstod',routehandles(mapnstod), 'write')
     end if

     !    rh_filename = 'cmeps.rh_'//trim(dstatname)
     !    if (maintask) print *,'XXX2 '//trim(rh_filename)
     !    if (maintask) then
     !       write(logunit,'(a)') trim(subname)//": saving  RH for "//trim(dstatname)
     !    end if
     !     dstatname = trim(compname(n1))//'_'//trim(compname(n2))//'_nstod'
     !    call ESMF_RouteHandleWrite(routehandles(mapnstod), fileName=trim(rh_filename), rc=rc)
     !    if (chkerr(rc,__LINE__,u_FILE_u)) return
     ! endif

     ! Copy dstatus field R8 values into a returned field
     if (present(dstatfield)) then
        !dstatname = ...
        dstatfield = ESMF_FieldCreate(mesh_dst, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
             name=trim(dstatname), rc=rc)
        if (chkerr(rc,__LINE__,u_FILE_u)) return
        call ESMF_FieldGet(lfield, farrayPtr=i4ptr, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        call ESMF_FieldGet(dstatfield, farrayPtr=r8ptr, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        r8ptr = real(i4ptr,R8)
     end if
  end if

  ! Output route handle to file if requested
  if (rhprint) then
     if (maintask) then
        write(logunit,'(a)') trim(subname)//trim(string)//": printing  RH for "//trim(mapname)
     end if
     call ESMF_RouteHandlePrint(routehandles(mapindex), rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  endif

  if (ESMF_FieldIsCreated(lfield, rc=rc)) then
     call ESMF_FieldDestroy(lfield, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  end if

end subroutine med_map_routehandles_initfrom_field
