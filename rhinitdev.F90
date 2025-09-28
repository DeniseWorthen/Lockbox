subroutine med_map_routehandles_initfrom_field(n1, n2, fldsrc, flddst, mapindex, routehandles, &
     mapfile, dstatfield, rc)

  use ESMF                  , only : ESMF_RouteHandle, ESMF_RouteHandlePrint, ESMF_Field, ESMF_MAXSTR
  use ESMF                  , only : ESMF_PoleMethod_Flag, ESMF_POLEMETHOD_ALLAVG, ESMF_POLEMETHOD_NONE
  use ESMF                  , only : ESMF_FieldSMMStore, ESMF_FieldRedistStore, ESMF_FieldRegridStore
  use ESMF                  , only : ESMF_RouteHandleIsCreated, ESMF_RouteHandleCreate
  use ESMF                  , only : ESMF_REGRIDMETHOD_BILINEAR, ESMF_REGRIDMETHOD_PATCH
  use ESMF                  , only : ESMF_REGRIDMETHOD_CONSERVE, ESMF_NORMTYPE_DSTAREA, ESMF_NORMTYPE_FRACAREA
  use ESMF                  , only : ESMF_UNMAPPEDACTION_IGNORE, ESMF_REGRIDMETHOD_NEAREST_STOD
  use ESMF                  , only : ESMF_EXTRAPMETHOD_NEAREST_STOD
  use ESMF                  , only : ESMF_Mesh, ESMF_MeshLoc, ESMF_MESHLOC_ELEMENT, ESMF_TYPEKIND_I4
  use ESMF                  , only : ESMF_MeshGet, ESMF_DistGridGet, ESMF_DistGrid, ESMF_TYPEKIND_R8
  use ESMF                  , only : ESMF_FieldGet, ESMF_FieldCreate, ESMF_FieldDestroy
  use med_internalstate_mod , only : mapbilnr, mapconsf, mapconsd, mappatch, mappatch_uv3d, mapbilnr_uv3d, mapfcopy
  use med_internalstate_mod , only : mapunset, mapnames, nmappers
  use med_internalstate_mod , only : mapnstod, mapnstod_consd, mapnstod_consf, mapnstod_consd
  use med_internalstate_mod , only : mapfillv_bilnr, mapbilnr_nstod, mapconsf_aofrac, mapconsf_uv3d
  use med_internalstate_mod , only : compocn, compwav, complnd, compname, compatm
  use med_internalstate_mod , only : coupling_mode
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
  character(len=CS)          :: dstatname
  integer                    :: srcMaskValue
  integer                    :: dstMaskValue
  real(R8), pointer          :: r8ptr(:)
  integer(I4), pointer       :: i4ptr(:)
  character(len=ESMF_MAXSTR) :: lmapfile
  logical                    :: rhprint = .false.
  integer                    :: srcTermProcessing_Value = 0
  type(ESMF_PoleMethod_Flag) :: polemethod
  character(len=*), parameter :: subname=' (module_med_map: med_map_routehandles_initfrom_field) '
  !---------------------------------------------

  lmapfile = 'unset'
  if (present(mapfile)) then
     lmapfile = trim(mapfile)
  end if

  mapname = trim(mapnames(mapindex))

  ! create a field to retrieve the dststatus field
  if (mapindex == mapnstod .or. mapindex == mapnstod_consd .or. mapindex == mapnstod_consf) then
     dstatname = trim(compname(n1))//'_'//trim(compname(n2))//'_nstod'
  else
     dstatname = trim(compname(n1))//'_'//trim(compname(n2))//'_'//mapname
  end if
  call ESMF_FieldGet(flddst, mesh=mesh_dst, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  lfield = ESMF_FieldCreate(mesh_dst, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(dstatname), rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  ! probably never true for CESM
  if (rw_routehandles) then
     !if (coupling_mode(1:3) == 'ufs') then
        rh_filename = 'cmeps.rh_'//trim(dstatname)
        inquire(FILE=trim(rh_filename), EXIST=rh_file_exist)
        if (rh_file_exist) then
           if (maintask) then
              write(logunit,'(A)') trim(subname)//' Reading RH from file: '//trim(rh_filename)
           end if
           routehandles(mapindex) = ESMF_RouteHandleCreate(fileName=trim(rh_filename), rc=rc)
           if (chkerr(rc,__LINE__,u_FILE_u)) return
           return
        end if
     !end if
  end if

  ! set src and dst masking using defaults
  srcMaskValue = defaultMasks(n1,1)
  dstMaskValue = defaultMasks(n2,2)

  ! override defaults for specific cases
  if (trim(coupling_mode) == 'cesm') then
     if (n1 == compwav .and. n2 == compocn) then
        srcMaskValue = 0
        dstMaskValue = ispval_mask
     endif
  end if
  if (coupling_mode(1:3) == 'ufs') then
     if (n1 == compatm .and. n2 == complnd) then
        srcMaskValue = ispval_mask
        dstMaskValue = ispval_mask
     end if
     if (n1 == complnd .and. n2 == compatm) then
        srcMaskValue = ispval_mask
        dstMaskValue = ispval_mask
     end if
  end if
  if (coupling_mode(1:4) == 'hafs') then
     if (n1 == compatm .and. n2 == compwav) then
        srcMaskValue = ispval_mask
     end if
  end if
  write(string,'(a,i10,a,i10)') trim(compname(n1))//' to '//trim(compname(n2))//' srcMask = ', &
       srcMaskValue,' dstMask = ',dstMaskValue
  call ESMF_LogWrite(trim(string), ESMF_LOGMSG_INFO)

  polemethod=ESMF_POLEMETHOD_ALLAVG
  if (trim(coupling_mode) == 'cesm' .or. coupling_mode(1:3) == 'ufs') then
     if (n1 == compwav .or. n2 == compwav) then
        polemethod = ESMF_POLEMETHOD_NONE ! todo: remove this when ESMF tripolar mapping fix is in place.
     endif
  end if
  if (trim(coupling_mode) == 'hafs.mom6') then
     polemethod = ESMF_POLEMETHOD_NONE
  endif

  ! Create route handle
  if (mapindex == mapfcopy) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH redist for '//trim(string)
     end if
     call ESMF_FieldRedistStore(fldsrc, flddst, routehandle=routehandles(mapfcopy), &
          ignoreUnmatchedIndices = .true., rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (lmapfile /= 'unset') then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//&
             ' via input file '//trim(mapfile)//' for '//trim(string)
     end if
     call ESMF_FieldSMMStore(fldsrc, flddst, mapfile, routehandle=routehandles(mapindex), &
          ignoreUnmatchedIndices=.true., &
          srcTermProcessing=srcTermProcessing_Value, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapbilnr ) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapbilnr), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR,   &
          polemethod=polemethod,                     &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapbilnr_uv3d ) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapbilnr_uv3d), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR,   &
          polemethod=polemethod,                     &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapfillv_bilnr) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapfillv_bilnr), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR,   &
          polemethod=polemethod,                     &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapbilnr_nstod) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapbilnr_nstod), &
          srcMaskValues=(/srcMaskValue/),              &
          dstMaskValues=(/dstMaskValue/),              &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR,     &
          extrapMethod=ESMF_EXTRAPMETHOD_NEAREST_STOD, &
          polemethod=polemethod,                       &
          srcTermProcessing=srcTermProcessing_Value,   &
          ignoreDegenerate=.true.,                     &
          dstStatusField=lfield,                       &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapconsf .or. mapindex == mapnstod_consf) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapconsf), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE,   &
          normType=ESMF_NORMTYPE_FRACAREA,           &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapconsf_aofrac) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapconsf_aofrac), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE,   &
          normType=ESMF_NORMTYPE_FRACAREA,           &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapconsf_uv3d) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapconsf_uv3d), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE,   &
          normType=ESMF_NORMTYPE_FRACAREA,           &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mapconsd .or. mapindex == mapnstod_consd) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapconsd), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE,   &
          normType=ESMF_NORMTYPE_DSTAREA,            &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mappatch ) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mappatch), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_PATCH,      &
          polemethod=polemethod,                     &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else if (mapindex == mappatch_uv3d ) then
     if (maintask) then
        write(logunit,'(A)') trim(subname)//' creating RH '//trim(mapname)//' for '//trim(string)
     end if
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mappatch_uv3d), &
          srcMaskValues=(/srcMaskValue/),            &
          dstMaskValues=(/dstMaskValue/),            &
          regridmethod=ESMF_REGRIDMETHOD_PATCH,      &
          polemethod=polemethod,                     &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true.,                   &
          dstStatusField=lfield,                     &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  else
     call shr_log_error(trim(subname)//' mapindex '//trim(mapname)//' not supported for '//trim(string), &
          line=__LINE__, file=u_FILE_u, rc=rc)
     return
  end if

  ! consd_nstod method requires a second routehandle
  if (mapindex == mapnstod .or. mapindex == mapnstod_consd .or. mapindex == mapnstod_consf) then
     call ESMF_FieldRegridStore(fldsrc, flddst, routehandle=routehandles(mapnstod), &
          srcMaskValues=(/srcMaskValue/), &
          dstMaskValues=(/dstMaskValue/), &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
          srcTermProcessing=srcTermProcessing_Value, &
          ignoreDegenerate=.true., &
          dstStatusField=lfield, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  end if

  ! Output route handle to file if requested
  if (rhprint) then
     if (maintask) then
        write(logunit,'(a)') trim(subname)//trim(string)//": printing  RH for "//trim(mapname)
     end if
     call ESMF_RouteHandlePrint(routehandles(mapindex), rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  endif

  ! Save route handle to file if requested
  !use_saved_routehandles = .true.  ! FIXME retrieve this from some config file
  if (rw_routehandles) then
     rh_filename = 'cmeps.rh_'//trim(dstatname)
     if (maintask) then
        write(logunit,'(a)') trim(subname)//trim(string)//": saving  RH for "//trim(dstatname)
     end if
     call ESMF_RouteHandleWrite(routehandles(mapindex), fileName=trim(rh_filename), rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  endif

  ! Copy R8 values into a returned field; for nstod/consf_nstod/consd_nstod this will be the nstod map
  if (present(dstatfield)) then
     dstatfield = ESMF_FieldCreate(mesh_dst, ESMF_TYPEKIND_R8, meshloc=ESMF_MESHLOC_ELEMENT, &
          name=trim(dstatname), rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
     call ESMF_FieldGet(lfield, farrayPtr=i4ptr, rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
     call ESMF_FieldGet(dstatfield, farrayPtr=r8ptr, rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
     r8ptr = real(i4ptr,R8)
     call ESMF_FieldDestroy(lfield, rc=rc)
     if (chkerr(rc,__LINE__,u_FILE_u)) return
  end if

end subroutine med_map_routehandles_initfrom_field
