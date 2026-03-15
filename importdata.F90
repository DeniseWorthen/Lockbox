subroutine assign_importdata(jdat, rc)

  use module_cplfields,  only: importFields, nImportFields, queryImportFields, &
       importFieldsValid
  use ESMF
  !
  implicit none
  integer, intent(in)  :: jdat(8)
  integer, intent(out) :: rc

  !--- local variables
  integer :: n, j, i, k, ix, nb, im, isc, iec, jsc, jec, nk, dimCount, findex
  integer :: sphum, liq_wat, ice_wat, o3mr
  character(len=128) :: impfield_name, fldname
  type(ESMF_TypeKind_Flag)                           :: datatype
  real(kind=ESMF_KIND_R8),  dimension(:,:), pointer  :: datar82d
  real(kind=ESMF_KIND_R8),  dimension(:,:,:), pointer:: datar83d
  real(kind=GFS_kind_phys), dimension(:,:), pointer  :: datar8
  logical,                  dimension(:,:), pointer  :: mergeflg
  real(kind=GFS_kind_phys)                           :: tem, ofrac
  logical found, isFieldCreated, lcpl_fice
  real(ESMF_KIND_R8), parameter :: missing_value = 9.99e20_ESMF_KIND_R8
  type(ESMF_Grid)  :: grid
  type(ESMF_Field) :: dbgField
  character(19)    :: currtimestring
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

  allocate(datar8(isc:iec,jsc:jec))
  allocate(mergeflg(isc:iec,jsc:jec))

  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,dim=',isc,iec,jsc,jec
  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,GFS_data, size', size(GFS_data)
  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,tsfc, size', size(GFS_data(1)%sfcprop%tsfc)
  !   if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplImp,tsfc, min_seaice', GFS_control%min_seaice

  do n=1,nImportFields ! Each import field is only available if it was connected in the import state.

     found = .false.

     isFieldCreated = ESMF_FieldIsCreated(importFields(n), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

     if (isFieldCreated) then ! put the data from local cubed sphere grid to column grid for phys

        datar8 = -99999.0
        mergeflg = .false.
        call ESMF_FieldGet(importFields(n), dimCount=dimCount ,typekind=datatype, &
             name=impfield_name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        if ( dimCount == 2) then
           if ( datatype == ESMF_TYPEKIND_R8) then
              call ESMF_FieldGet(importFields(n),farrayPtr=datar82d,localDE=0, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              datar8 = datar82d
              if (GFS_control%cpl_imp_mrg) then
                 mergeflg(:,:) = datar82d(:,:).eq.missing_value
              endif
              if (mpp_pe() == mpp_root_pe() .and. debug) print *,'in cplIMP,atmos gets ',trim(impfield_name),' datar8=', &
                   datar8(isc,jsc), maxval(datar8), minval(datar8)
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
           if (datar8(isc,jsc) > -99998.0) then

              ! get sea-state dependent surface roughness (if cplwav2atm=true)
              !----------------------------
              fldname = 'wave_z0_roughness_length'
              if (trim(impfield_name) == trim(fldname)) then
                 findex = queryImportFields(fldname)
                 if (importFieldsValid(findex) .and. GFS_control%cplwav2atm) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im,tem)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero .and.  datar8(i,j) > zorlmin) then
                             tem = 100.0_GFS_kind_phys * min(0.1_GFS_kind_phys, datar8(i,j))
                             !                   GFS_Coupling%zorlwav_cpl(im) = tem
                             GFS_Sfcprop%zorlwav(im)      = tem
                             GFS_Sfcprop%zorlw(im)        = tem
                          else
                             GFS_Sfcprop%zorlwav(im) = -999.0_GFS_kind_phys
                          endif
                       enddo
                    enddo
                 endif
              endif

              ! get sea ice surface temperature
              !--------------------------------
              fldname = 'sea_ice_surface_temperature'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero .and.  datar8(i,j) > 150.0) then
                             !                   GFS_Coupling%tisfcin_cpl(im) = datar8(i,j)
                             GFS_Sfcprop%tisfc(im)       = datar8(i,j)
                          endif
                       enddo
                    enddo
                 endif
              endif

              ! get sst:  sst needs to be adjusted by land sea mask before passing to fv3
              !--------------------------------------------------------------------------
              fldname = 'sea_surface_temperature'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex) .and. GFS_control%cplocn2atm) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero .and. datar8(i,j) > 150.0) then
                             if(mergeflg(i,j)) then
                                !                     GFS_Coupling%tseain_cpl(im) = GFS_Sfcprop%tsfc(im)
                                GFS_Sfcprop%tsfco(im) = GFS_Sfcprop%tsfc(im)
                                datar8(i,j) = GFS_Sfcprop%tsfc(im)
                             else
                                !                     GFS_Coupling%tseain_cpl(im) = datar8(i,j)
                                GFS_Sfcprop%tsfco(im)       = datar8(i,j)
                             endif
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'get sst from mediator'
                 endif
              endif

              ! get sea ice fraction:  fice or sea ice concentration from the mediator
              !-----------------------------------------------------------------------
              fldname = 'ice_fraction'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    lcpl_fice = .true.
                    !$omp parallel do default(shared) private(i,j,nb,ix,im,ofrac)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          GFS_Coupling%slimskin_cpl(im) = GFS_Sfcprop%slmsk(im)
                          ofrac = GFS_Sfcprop%oceanfrac(im)
                          if (ofrac > zero) then
                             GFS_Sfcprop%fice(im) = max(zero, min(one, datar8(i,j)/ofrac)) !LHS: ice frac wrt water area
                             if (GFS_Sfcprop%fice(im) >= GFS_control%min_seaice) then
                                if (GFS_Sfcprop%fice(im) > one-epsln) GFS_Sfcprop%fice(im) = one
                                if (abs(one-ofrac) < epsln) GFS_Sfcprop%slmsk(im) = 2.0_GFS_kind_phys !slmsk=2 crashes in gcycle on partial land points
                                !                     GFS_Sfcprop%slmsk(im)         = 2.0_GFS_kind_phys
                                GFS_Coupling%slimskin_cpl(im) = 4.0_GFS_kind_phys
                             else
                                GFS_Sfcprop%fice(im) = zero
                                if (abs(one-ofrac) < epsln) then
                                   GFS_Sfcprop%slmsk(im)         = zero
                                   GFS_Coupling%slimskin_cpl(im) = zero
                                endif
                             endif
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get fice from mediator'
                 endif
              endif

              ! get upward LW flux:  for sea ice covered area
              !----------------------------------------------
              fldname = 'lwup_flx_ice'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Coupling%ulwsfcin_cpl(im) = -datar8(i,j)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get lwflx from mediator'
                 endif
              endif

              ! get latent heat flux:  for sea ice covered area
              !------------------------------------------------
              fldname = 'laten_heat_flx_atm_into_ice'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Coupling%dqsfcin_cpl(im) = -datar8(i,j)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get laten_heat from mediator'
                 endif
              endif

              ! get sensible heat flux:  for sea ice covered area
              !--------------------------------------------------
              fldname = 'sensi_heat_flx_atm_into_ice'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Coupling%dtsfcin_cpl(im) = -datar8(i,j)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sensi_heat from mediator'
                 endif
              endif

              ! get zonal compt of momentum flux:  for sea ice covered area
              !------------------------------------------------------------
              fldname = 'stress_on_air_ice_zonal'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Coupling%dusfcin_cpl(im) = -datar8(i,j)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get zonal_moment_flx from mediator'
                 endif
              endif

              ! get meridional compt of momentum flux:  for sea ice covered area
              !-----------------------------------------------------------------
              fldname = 'stress_on_air_ice_merid'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Coupling%dvsfcin_cpl(im) = -datar8(i,j)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get merid_moment_flx from mediator'
                 endif
              endif

              ! get sea ice volume:  for sea ice covered area
              !----------------------------------------------
              fldname = 'sea_ice_volume'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Sfcprop%hice(im)        = min(datar8(i,j), himax)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug) print *,'fv3 assign_import: get ice_volume from mediator'
                 endif
              endif

              ! get snow volume:  for sea ice covered area
              !-------------------------------------------
              fldname = 'snow_volume_on_sea_ice'
              if (trim(impfield_name) == trim(fldname)) then
                 findex  = queryImportFields(fldname)
                 if (importFieldsValid(findex)) then
                    !$omp parallel do default(shared) private(i,j,nb,ix,im)
                    do j=jsc,jec
                       do i=isc,iec
                          nb = Atm_block%blkno(i,j)
                          ix = Atm_block%ixp(i,j)
                          im = GFS_control%chunk_begin(nb)+ix-1
                          if (GFS_Sfcprop%oceanfrac(im) > zero) then
                             GFS_Coupling%hsnoin_cpl(im) = datar8(i,j)
                          endif
                       enddo
                    enddo
                    if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get snow_volume from mediator'
                 endif
              endif

              if (GFS_control%use_cice_alb) then
                 !
                 ! get instantaneous near IR albedo for diffuse radiation: for sea ice covered area
                 !---------------------------------------------------------------------------------
                 fldname = 'inst_ice_ir_dif_albedo'
                 if (trim(impfield_name) == trim(fldname)) then
                    findex  = queryImportFields(fldname)
                    if (importFieldsValid(findex)) then
                       !$omp parallel do default(shared) private(i,j,nb,ix,im)
                       do j=jsc,jec
                          do i=isc,iec
                             nb = Atm_block%blkno(i,j)
                             ix = Atm_block%ixp(i,j)
                             im = GFS_control%chunk_begin(nb)+ix-1
                             if (GFS_Sfcprop%oceanfrac(im) > zero) then
                                GFS_Sfcprop%albdifnir_ice(im) = datar8(i,j)
                             endif
                          enddo
                       enddo
                       if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sfc_alb_nir_dif_cpl from mediator'
                    endif
                 endif
                 !
                 ! get instantaneous near IR albedo for direct radiation: for sea ice covered area
                 !---------------------------------------------------------------------------------
                 fldname = 'inst_ice_ir_dir_albedo'
                 if (trim(impfield_name) == trim(fldname)) then
                    findex  = queryImportFields(fldname)
                    if (importFieldsValid(findex)) then
                       !$omp parallel do default(shared) private(i,j,nb,ix,im)
                       do j=jsc,jec
                          do i=isc,iec
                             nb = Atm_block%blkno(i,j)
                             ix = Atm_block%ixp(i,j)
                             im = GFS_control%chunk_begin(nb)+ix-1
                             if (GFS_Sfcprop%oceanfrac(im) > zero) then
                                GFS_Sfcprop%albdirnir_ice(im) = datar8(i,j)
                             endif
                          enddo
                       enddo
                       if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sfc_alb_nir_dir_cpl from mediator'
                    endif
                 endif
                 !
                 ! get instantaneous visible albedo for diffuse radiation: for sea ice covered area
                 !---------------------------------------------------------------------------------
                 fldname = 'inst_ice_vis_dif_albedo'
                 if (trim(impfield_name) == trim(fldname)) then
                    findex  = queryImportFields(fldname)
                    if (importFieldsValid(findex)) then
                       !$omp parallel do default(shared) private(i,j,nb,ix,im)
                       do j=jsc,jec
                          do i=isc,iec
                             nb = Atm_block%blkno(i,j)
                             ix = Atm_block%ixp(i,j)
                             im = GFS_control%chunk_begin(nb)+ix-1
                             if (GFS_Sfcprop%oceanfrac(im) > zero) then
                                GFS_Sfcprop%albdifvis_ice(im) = datar8(i,j)
                             endif
                          enddo
                       enddo
                       if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get sfc_alb_vis_dif_cpl from mediator'
                    endif
                 endif

                 !
                 ! get instantaneous visible IR albedo for direct radiation: for sea ice covered area
                 !---------------------------------------------------------------------------------
                 fldname = 'inst_ice_vis_dir_albedo'
                 if (trim(impfield_name) == trim(fldname)) then
                    findex  = queryImportFields(fldname)
                    if (importFieldsValid(findex)) then
                       !$omp parallel do default(shared) private(i,j,nb,ix,im)
                       do j=jsc,jec
                          do i=isc,iec
                             nb = Atm_block%blkno(i,j)
                             ix = Atm_block%ixp(i,j)
                             im = GFS_control%chunk_begin(nb)+ix-1
                             if (GFS_Sfcprop%oceanfrac(im) > zero) then
                                GFS_Sfcprop%albdirvis_ice(im) = datar8(i,j)
                             endif
                          enddo
                       enddo
                       if (mpp_pe() == mpp_root_pe() .and. debug)  print *,'fv3 assign_import: get inst_ice_vis_dir_albedo from mediator'
                    endif
                 endif
              endif
           endif ! if (datar8(isc,jsc) > -99999.0) then

           ! write post merge import data to NetCDF file.
           if (GFS_control%cpl_imp_dbg) then
              call ESMF_FieldGet(importFields(n), grid=grid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

              dbgField = ESMF_FieldCreate(grid=grid, farrayPtr=datar8, name=impfield_name, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

              write (currtimestring, "(I4.4,'-',I2.2,'-',I2.2,'T',I2.2,':',I2.2,':',I2.2)") &
                   jdat(1), jdat(2), jdat(3), jdat(5), jdat(6), jdat(7)
              call ESMF_FieldWrite(dbgField, fileName='fv3_merge_'//trim(impfield_name)//'_'// &
                   trim(currtimestring)//'.nc', rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

              call ESMF_FieldDestroy(dbgField, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
           endif

        endif ! if (found) then
     endif   ! if (isFieldCreated) then
  enddo
  !
  deallocate(mergeflg)
  deallocate(datar8)

  ! update sea ice related fields:
  if( lcpl_fice ) then
     !$omp parallel do default(shared) private(i,j,nb,ix,tem,im)
     do j=jsc,jec
        do i=isc,iec
           nb = Atm_block%blkno(i,j)
           ix = Atm_block%ixp(i,j)
           im = GFS_control%chunk_begin(nb)+ix-1
           if (GFS_Sfcprop%oceanfrac(im) > zero) then
              if (GFS_Sfcprop%fice(im) >= GFS_control%min_seaice) then

                 GFS_Coupling%hsnoin_cpl(im) = min(hsmax, GFS_Coupling%hsnoin_cpl(im) &
                      / GFS_Sfcprop%fice(im))
                 GFS_Sfcprop%zorli(im)       = z0ice
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
              endif
           endif
        enddo
     enddo
  endif

  rc=0
  !
end subroutine assign_importdata
!
