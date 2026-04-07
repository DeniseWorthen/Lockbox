.  subroutine assign_importdata(atmtime,atmtimestep,isregional,rc)

  use module_cplfields,  only: importFields, nImportFields, queryImportFields, &
       importFieldsValid
  use ESMF
  !
  implicit none
  type(time_type), intent(in) :: atmtime, atmtimestep
  logical, intent(in) :: isregional
  integer, intent(out) :: rc

  !--- local variables
  integer :: n, j, i, k, ix, nb, im, isc, iec, jsc, jec, nk, dimCount, findex
  integer :: iyear, imonth, iday, ihour, iminute, isecond
  integer :: sphum, liq_wat, ice_wat, o3mr
  character(len=128) :: impfield_name, fldname
  type(ESMF_TypeKind_Flag)                           :: datatype
  real(kind=ESMF_KIND_R8),  dimension(:,:), pointer  :: datar82d
  real(kind=ESMF_KIND_R8),  dimension(:,:,:), pointer:: datar83d
  real(kind=GFS_kind_phys), dimension(:,:), pointer  :: dataptr
  real(kind=ESMF_KIND_R8),  dimension(:,:), pointer  :: dbgptr
  logical,                  dimension(:,:), pointer  :: mergeflg
  real(kind=GFS_kind_phys)                           :: tem, ofrac
  logical :: found, isFieldCreated, lcpl_fice
  real(ESMF_KIND_R8), parameter :: missing_value = 9.99e20_ESMF_KIND_R8
  type(ESMF_FieldBundle) :: FBcpl2phys
  type(ESMF_Field) :: dbgField
  character(19)    :: timestring
  character(len=128), allocatable :: fieldlist(:)
  integer :: nfields
  real (kind=GFS_kind_phys), parameter :: z0ice=1.0    !  (in cm)

  !
  !     real(kind=GFS_kind_phys), parameter :: himax = 8.0      !< maximum ice thickness allowed
  !     real(kind=GFS_kind_phys), parameter :: himin = 0.1      !< minimum ice thickness required
  !     real(kind=GFS_kind_phys), parameter :: hsmax = 100.0    !< maximum snow depth (m) allowed
  real(kind=GFS_kind_phys), parameter :: himax = 1.0e12   !< maximum ice thickness allowed
  real(kind=GFS_kind_phys), parameter :: hsmax = 1.0e12   !< maximum snow depth (m) allowed
  real(kind=GFS_kind_phys), parameter :: con_sbc = 5.670400e-8_GFS_kind_phys !< stefan-boltzmann
  !
  !------------------------------------------------------------------------------
  !
  rc  = -999

  ! set up local dimension
  isc = GFS_control%isc
  iec = GFS_control%isc+GFS_control%nx-1
  jsc = GFS_control%jsc
  jec = GFS_control%jsc+GFS_control%ny-1
  nk  = Atm_block%npz
  lcpl_fice = .false.

  allocate(dataptr(isc:iec,jsc:jec))
  allocate(mergeflg(isc:iec,jsc:jec))
  if (GFS_control%cpl_imp_dbg) then
     allocate(dbgptr(isc:iec,jsc:jec), source=missing_value)
     FBcpl2phys = ESMF_FieldBundleCreate(rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
  end if

  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,dim=',isc,iec,jsc,jec
  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,GFS_data, size', size(GFS_data)
  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,tsfc, size', size(GFS_data(1)%sfcprop%tsfc)
  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,tsfc, min_seaice', GFS_control%min_seaice

  do n=1,nImportFields ! Each import field is only available if it was connected in the import state.

     found = .false.
     firstfound = .false.
     add2dFB = .false.
     isFieldCreated = ESMF_FieldIsCreated(importFields(n), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

     if (isFieldCreated) then ! put the data from local cubed sphere grid to column grid for phys

        dataptr = -99999.0
        mergeflg = .false.
        call ESMF_FieldGet(importFields(n), dimCount=dimCount ,typekind=datatype, &
             name=impfield_name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if ( dimCount == 2) then
           if ( datatype == ESMF_TYPEKIND_R8) then
              call ESMF_FieldGet(importFields(n),farrayPtr=datar82d,localDE=0, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              dataptr = datar82d
              if (GFS_control%cpl_imp_mrg) then
                 mergeflg(:,:) = datar82d(:,:).eq.missing_value
              endif
              if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplIMP,atmos gets ',trim(impfield_name),' dataptr=', &
                   dataptr(isc,jsc), maxval(dataptr), minval(dataptr)
              found = .true.
           endif

        else if( dimCount == 3) then
           if ( datatype == ESMF_TYPEKIND_R8) then
              call ESMF_FieldGet(importFields(n),farrayPtr=datar83d,localDE=0, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              found = .true.
           endif
        endif
        !
        if (found) then
           if (GFS_control%cpl_imp_dbg .and. .not. firstfound) then
              call ESMF_FieldGet(importFields(n), grid=grid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              firstfound = .true.
           end if

           if(GFS_control%cplwav2atm) then
              ! get sea-state dependent surface roughness
              !----------------------------
              fldname = 'wave_z0_roughness_length'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    add2FB = .true.
                    call copy2block(GFS_Sfcprop%zorlwav, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'get wave roughness from mediator'
                 endif
              endif
           endif ! GFS_control%cplwav2atm

           if (GFS_control%cplocn2atm) then
              ! get sst:  sst needs to be adjusted by land sea mask before passing to fv3
              !--------------------------------------------------------------------------
              fldname = 'sea_surface_temperature'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    add2FB = .true.
                    call copy2block(GFS_Sfcprop%tsfco, dataptr, mask=GFS_Sfcprop%oceanfrac, validmin=150.0_GFS_kind_phys, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (GFS_control%cpl_imp_mrg) then
                       call merge_importfield(GFS_Sfcprop%tsfco, GFS_Sfcprop%tsfc, mergeflg, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    end if
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'get sst from mediator'
                 endif
              end if

              ! get zonal ocean current:
              !--------------------------------------------------------------------------
              fldname = 'ocn_current_zonal'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    add2FB = .true.
                    call copy2block(GFS_Sfcprop%usfco, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (GFS_control%cpl_imp_mrg) then
                       call merge_importfield(GFS_Sfcprop%usfco, zero, mergeflg, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    end if
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'get usfco from mediator'
                 end if
              end if

              ! get meridional ocean current:
              !--------------------------------------------------------------------------
              fldname = 'ocn_current_merid'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    add2FB = .true.
                    call copy2block(GFS_Sfcprop%vsfco, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (GFS_control%cpl_imp_mrg) then
                       call merge_importfield(GFS_Sfcprop%vsfco, zero, mergeflg, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    end if
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'get vsfco from mediator'
                 end if
              end if
           end if ! GFS_control%cplocn2atm

           ! get sea ice fraction:  fice or sea ice concentration from the mediator
           !-----------------------------------------------------------------------
           fldname = 'ice_fraction'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 lcpl_fice = .true.
                 add2FB = .true.
                 call copy2block(GFS_Sfcprop%fice, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get fice from mediator'
              endif
           endif
           ! get upward LW flux: for sea ice covered area
           !----------------------------------------------
           fldname = 'lwup_flx_ice'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 add2FB = .true.
                 call copy2block(GFS_Coupling%ulwsfcin_cpl, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get lwflx from mediator'
              endif
           endif
           ! get latent heat flux: for sea ice covered area
           !------------------------------------------------
           fldname = 'laten_heat_flx_atm_into_ice'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 call copy2block(GFS_Coupling%dqsfcin_cpl, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get laten_heat from mediator'
              endif
           endif
           ! get sensible heat flux: for sea ice covered area
           !--------------------------------------------------
           fldname = 'sensi_heat_flx_atm_into_ice'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 call copy2block(GFS_Coupling%dtsfcin_cpl, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sensi_heat from mediator'
              endif
           endif
           ! get zonal compt of momentum flux: for sea ice covered area
           !------------------------------------------------------------
           fldname = 'stress_on_air_ice_zonal'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 call copy2block(GFS_Coupling%dusfcin_cpl, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get zonal_moment_flx from mediator'
              endif
           endif
           ! get meridional compt of momentum flux: for sea ice covered area
           !-----------------------------------------------------------------
           fldname = 'stress_on_air_ice_merid'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 call copy2block(GFS_Coupling%dvsfcin_cpl, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get merid_moment_flx from mediator'
              endif
           endif
           ! get sea ice volume: for sea ice covered area
           !----------------------------------------------
           fldname = 'sea_ice_volume'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 add2FB = .true.
                 call copy2block(GFS_Sfcprop%hice, dataptr, mask=GFS_Sfcprop%oceanfrac, validmax=himax, rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug) print *,'fv3 assign_import: get ice_volume from mediator'
              endif
           endif

           ! get snow volume: for sea ice covered area
           !-------------------------------------------
           fldname = 'snow_volume_on_sea_ice'
           if (trim(impfield_name) == trim(fldname)) then
              if (importFieldsValid(queryImportFields(fldname))) then
                 add2FB = .true.
                 call copy2block(GFS_Coupling%hsnoin_cpl, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                 if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                 if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get snow_volume from mediator'
              endif
           endif
           if (GFS_control%use_cice_alb) then
              ! get instantaneous near IR albedo for diffuse radiation: for sea ice covered area
              !---------------------------------------------------------------------------------
              fldname = 'inst_ice_ir_dif_albedo'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Sfcprop%albdifnir_ice, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sfc_alb_nir_dif_cpl from mediator'
                 endif
              endif
              ! get instantaneous near IR albedo for direct radiation: for sea ice covered area
              !---------------------------------------------------------------------------------
              fldname = 'inst_ice_ir_dir_albedo'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Sfcprop%albdirnir_ice, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sfc_alb_nir_dir_cpl from mediator'
                 endif
              endif
              ! get instantaneous visible albedo for diffuse radiation: for sea ice covered area
              !---------------------------------------------------------------------------------
              fldname = 'inst_ice_vis_dif_albedo'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Sfcprop%albdifvis_ice, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sfc_alb_vis_dif_cpl from mediator'
                 endif
              endif
              ! get instantaneous visible IR albedo for direct radiation: for sea ice covered area
              !---------------------------------------------------------------------------------
              fldname = 'inst_ice_vis_dir_albedo'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Sfcprop%albdirvis_ice, dataptr, mask=GFS_Sfcprop%oceanfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get inst_ice_vis_dir_albedo from mediator'
                 endif
              endif
           endif ! GFS_control%use_cice_alb

           if (GFS_control%use_med_flux) then
              ! get upward LW flux: for open ocean
              !----------------------------------------------
              fldname = 'lwup_flx_ocn'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%ulwsfcin_med, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get lwflx for open ocean from mediator'
                 endif
              endif
              ! get latent heat flux: for open ocean
              !------------------------------------------------
              fldname = 'laten_heat_flx_atm_into_ocn'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%dqsfcin_med, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get laten_heat for open ocean from mediator'
                 endif
              endif
              ! get sensible heat flux: for open ocean
              !--------------------------------------------------
              fldname = 'sensi_heat_flx_atm_into_ocn'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%dtsfcin_med, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sensi_heat for open ocean from mediator'
                 endif
              endif
              ! get zonal compt of momentum flux: for open ocean
              !------------------------------------------------------------
              fldname = 'stress_on_air_ocn_zonal'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%dusfcin_med, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get zonal_moment_flx for open ocean from mediator'
                 endif
              endif
              ! get meridional compt of momentum flux: for open ocean
              !-----------------------------------------------------------------
              fldname = 'stress_on_air_ocn_merid'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%dvsfcin_med, dataptr, mask=GFS_Sfcprop%oceanfrac, flipsign=.true., rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get merid_moment_flx for open ocean from mediator'
                 endif
              endif
           end if ! GFS_control%use_med_flux

           if (GFS_control%cpllnd .and. GFS_control%cpllnd2atm) then
              ! get surface snow area fraction: over land
              !------------------------------------------------
              fldname = 'inst_snow_area_fraction_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%sncovr1_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get snow area fraction from land'
                 endif
              endif
              ! get latent heat flux: over land
              !------------------------------------------------
              fldname = 'inst_laten_heat_flx_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%evap_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get latent heat flux from land'
                 endif
              endif
              ! get sensible heat flux: over land
              !--------------------------------------------------
              fldname = 'inst_sensi_heat_flx_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%hflx_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sensible heat flux from land'
                 endif
              endif
              ! get surface upward potential latent heat flux: over land
              !------------------------------------------------
              fldname = 'inst_potential_laten_heat_flx_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%ep_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get potential latent heat flux from land'
                 endif
              endif
              ! get 2m air temperature: over land
              !------------------------------------------------
              fldname = 'inst_temp_height2m_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%t2mmp_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get temperature at 2m from land'
                 endif
              endif
              ! get 2m specific humidity: over land
              !------------------------------------------------
              fldname = 'inst_spec_humid_height2m_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%q2mp_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get specific humidity at 2m from land'
                 endif
              endif
              ! get specific humidity: over land
              !------------------------------------------------
              fldname = 'inst_spec_humid_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%qsurf_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get specific humidity from land'
                 endif
              endif
              ! get upward heat flux in soil
              !------------------------------------------------
              fldname = 'inst_upward_heat_flux_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%gflux_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get upward heat flux from land'
                 endif
              endif
              ! get surface runoff in soil
              !------------------------------------------------
              fldname = 'inst_runoff_rate_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%runoff_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get surface runoff from land'
                 endif
              endif
              ! get subsurface runoff in soil
              !------------------------------------------------
              fldname = 'inst_subsurface_runoff_rate_lnd'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%drain_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get subsurface runoff from land'
                 endif
              endif
              ! get momentum exchange coefficient
              !------------------------------------------------
              fldname = 'inst_drag_wind_speed_for_momentum'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%cmm_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get drag wind speed for momentum from land'
                 endif
              endif
              ! get thermal exchange coefficient
              !------------------------------------------------
              fldname = 'inst_drag_mass_flux_for_heat_and_moisture'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%chh_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get thermal exchange coefficient form land'
                 endif
              endif
              ! get function of surface roughness length and green vegetation fraction
              !------------------------------------------------
              fldname = 'inst_func_of_roughness_length_and_vfrac'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_Coupling%zvfun_lnd, dataptr, mask=GFS_Sfcprop%landfrac, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get func. of roughness length and vfrac form land'
                 endif
              endif
           endif ! GFS_control%cpllnd .and. GFS_control%cpllnd2atm

           if (GFS_control%cpl_fire) then
              fldname = 'hflx_fire'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_sfcprop%hflx_fire, dataptr, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get hflx_fire from FBH model'
                 endif
              endif
              fldname = 'evap_fire'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_sfcprop%evap_fire, dataptr, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get evap_fire from FBH model'
                 endif
              endif
              fldname = 'smoke_fire'
              if (trim(impfield_name) == trim(fldname)) then
                 if (importFieldsValid(queryImportFields(fldname))) then
                    call copy2block(GFS_sfcprop%smoke_fire, dataptr, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get smoke_fire from FBH model'
                 endif
              endif
           endif ! (GFS_control%cpl_fire)

           if (GFS_control%cpl_imp_dbg .and. add2FB) then
              dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name=trim(fldname), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
           endif
        endif ! if (found) then
     endif ! if (isFieldCreated) then
  enddo
  !
  deallocate(mergeflg)
  deallocate(dataptr)

  ! add fields not present in importstate to FB
  if (GFS_control%cpl_imp_dbg) then
    dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='ocean_fraction', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='slimskin_cpl', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='slmsk', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='zorlw', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
  endif

  !$omp parallel do default(shared) private(i,j,nb,ix,tem,im,ofrac)
  do j=jsc,jec
     do i=isc,iec
        nb = Atm_block%blkno(i,j)
        ix = Atm_block%ixp(i,j)
        im = GFS_control%chunk_begin(nb)+ix-1

        if (GFS_control%cplwav2atm) then
           if (GFS_Sfcprop%oceanfrac(im) > zero .and. GFS_Sfcprop%zorlwav(im) > zorlmin) then
              tem = 100.0_GFS_kind_phys * min(0.1_GFS_kind_phys, GFS_Sfcprop%zorlwav(im))
              GFS_Sfcprop%zorlwav(im)      = tem
              GFS_Sfcprop%zorlw(im)        = tem
           else
              GFS_Sfcprop%zorlwav(im) = -999.0_GFS_kind_phys
           endif
        endif

        if (lcpl_fice) then
           GFS_Coupling%slimskin_cpl(im) = GFS_Sfcprop%slmsk(im)
           ofrac = GFS_Sfcprop%oceanfrac(im)

           if (ofrac > zero) then
              GFS_Sfcprop%fice(im) = max(zero, min(one, GFS_Sfcprop%fice(im)/ofrac)) !LHS: ice frac wrt water area
              if (GFS_Sfcprop%fice(im) >= GFS_control%min_seaice) then
                 if (GFS_Sfcprop%fice(im) > one-epsln) GFS_Sfcprop%fice(im) = one
                 ! slmsk
                 if (abs(one-ofrac) < epsln) GFS_Sfcprop%slmsk(im) = 2.0_GFS_kind_phys !slmsk=2 crashes in gcycle on partial land points
                 GFS_Coupling%slimskin_cpl(im) = 4.0_GFS_kind_phys
                 !hsnow and z0
                 GFS_Coupling%hsnoin_cpl(im) = min(hsmax, GFS_Coupling%hsnoin_cpl(im) / GFS_Sfcprop%fice(im))
                 GFS_Sfcprop%zorli(im)       = z0ice
                 ! ulw
                 tem = GFS_Sfcprop%tisfc(im) * GFS_Sfcprop%tisfc(im)
                 tem = con_sbc * tem * tem
                 if (GFS_Coupling%ulwsfcin_cpl(im) > zero) then
                    GFS_Sfcprop%emis_ice(im) = GFS_Coupling%ulwsfcin_cpl(im) / tem
                    GFS_Sfcprop%emis_ice(im) = max(0.9, min(one, GFS_Sfcprop%emis_ice(im)))
                 else
                    GFS_Sfcprop%emis_ice(im) = 0.96
                 endif
                 GFS_Coupling%ulwsfcin_cpl(im) = tem * GFS_Sfcprop%emis_ice(im)
              else
                 GFS_Sfcprop%tisfc(im)       = GFS_Sfcprop%tsfco(im)
                 GFS_Sfcprop%fice(im)        = zero
                 GFS_Sfcprop%hice(im)        = zero
                 GFS_Coupling%hsnoin_cpl(im) = zero
                 !
                 GFS_Coupling%dtsfcin_cpl(im)  = -99999.0 ! over open water - should not be used in ATM
                 GFS_Coupling%dqsfcin_cpl(im)  = -99999.0 !                 ,,
                 GFS_Coupling%dusfcin_cpl(im)  = -99999.0 !                 ,,
                 GFS_Coupling%dvsfcin_cpl(im)  = -99999.0 !                 ,,
                 GFS_Coupling%dtsfcin_cpl(im)  = -99999.0 !                 ,,
                 GFS_Coupling%ulwsfcin_cpl(im) = -99999.0 !                 ,,
                 if (abs(one-GFS_Sfcprop%oceanfrac(im)) < epsln) then !  100% open water
                    GFS_Coupling%slimskin_cpl(im) = zero
                    GFS_Sfcprop%slmsk(im)         = zero
                 endif
              endif ! GFS_Sfcprop%fice(im) >= GFS_control%min_seaice
           endif ! GFS_Sfcprop%oceanfrac(im) > zero
        endif ! lcpl_fice
     enddo
  enddo

  if (GFS_control%cpl_imp_dbg) then
     call get_date(atmtime+atmtimestep,iyear,imonth,iday,ihour,iminute,isecond)
     write(timestring, "(I4.4,'-',I2.2,'-',I2.2,'T',I2.2,':',I2.2,':',I2.2)") iyear,imonth,iday,ihour,iminute,isecond
     ! dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='slimskin_cpl', rc=rc)
     ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
     ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! do nb = 1, Atm_block%nblks
     !    call block_data_copy(dbgptr, GFS_Coupling%slimskin_cpl, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! enddo
     ! dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='wave_z0_roughness_length', rc=rc)
     ! call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
     ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! do nb = 1, Atm_block%nblks
     !    call block_data_copy(dbgptr, GFS_Sfcprop%zorlwav, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! enddo
     ! dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=dbgptr, name='ice_fraction', rc=rc)
     ! call ESMF_FieldBundleAdd(FBcpl2phys, (/dbgField/), rc=rc)
     ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! do nb = 1, Atm_block%nblks
     !    call block_data_copy(dbgptr,  GFS_Sfcprop%fice, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     ! enddo

     call ESMF_FieldBundleGet(FBcpl2phys, fieldCount=nfields, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     allocate(fieldlist(1:nfields))
     call ESMF_FieldBundleGet(FBcpl2phys, fieldNameList=fieldList, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     do n = 1,nfields
        call ESMF_FieldBundleGet(FBcpl2phys, fieldName=trim(fieldlist(n)), field=dbgField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldGet(dbgField, farrayPtr=dbgptr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        select case(trim(fieldlist(n)))
        case('wave_z0_roughness_length')
           do nb = 1, Atm_block%nblks
              call block_data_copy(dbgptr, GFS_Sfcprop%zorlwav, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
           enddo
     !    case('ice_fraction')
     !       do nb = 1, Atm_block%nblks
     !          call block_data_copy(dbgptr, GFS_Sfcprop%fice, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     !       enddo
     !    case('sea_ice_surface_temperature')
     !       do nb = 1, Atm_block%nblks
     !          call block_data_copy(dbgptr, GFS_Sfcprop%tisfc, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     !       enddo
     !       if (GFS_control%cpl_imp_mrg) then
     !          select case(trim(fieldlist(n)))
     !          case('sea_surface_temperature')
     !             do nb = 1, Atm_block%nblks
     !                call block_data_copy(dbgptr, GFS_Sfcprop%tsfco, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     !             enddo
     !          case('ocn_current_zonal')
     !             do nb = 1, Atm_block%nblks
     !                call block_data_copy(dbgptr, GFS_Sfcprop%usfco, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     !             enddo
     !          case('ocn_current_merid')
     !             do nb = 1, Atm_block%nblks
     !                call block_data_copy(dbgptr, GFS_Sfcprop%vsfco, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     !             enddo
     !          end select
     !       end if
     !    case('lwup_flx_ice')
     !       do nb = 1, Atm_block%nblks
     !          call block_data_copy(dbgptr, GFS_Coupling%ulwsfcin_cpl, Atm_block, nb, offset=GFS_Control%chunk_begin(nb), rc=rc)
     !          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     !       enddo
        end select
     enddo

     if (isregional) then
        call ESMF_FieldBundleWrite(FBcpl2phys, fileName='fv3_merge_'//trim(timestring)//'.nc', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
     else
        call ESMF_FieldBundleWrite(FBcpl2phys, fileName='fv3_merge_'//trim(timestring)//'.tile*.nc', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      endif
      do n = 1,nfields
        call ESMF_FieldBundleGet(FBcpl2phys, fieldName=trim(fieldlist(n)), field=dbgField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldDestroy(dbgField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      enddo
      call ESMF_FieldBundleDestroy(FBcpl2phys, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      if (associated(dbgptr)) then
        deallocate(dbgptr)
        nullify(dbgptr) ! Good practice to prevent future accidental access
      endif
     deallocate(dbgptr)
  endif
  rc=0
  !
end subroutine assign_importdata
