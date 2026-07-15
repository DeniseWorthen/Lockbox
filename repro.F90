program repro

  use ESMF
  use netCDF

  implicit none

  type(ESMF_RegridMethod_Flag) :: method
  type(ESMF_VM):: vm

  integer :: rc, mype, numpe
  character(len=256) :: fsrc, fdst, fwgt
  character(len=5) :: catm
  character(len=5) :: cocn

  call ESMF_Initialize(vm=vm, logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_VMGet(vm=vm, localPet=mype, peCount=numpe, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  method=ESMF_REGRIDMETHOD_CONSERVE

  !! 1/12 ocean + C-ATM
  catm = 'C1152'; cocn = 'mx008'
  !catm = 'C768 '; cocn = 'mx008'
  !catm = 'C384 '; cocn = 'mx008'
  !catm = 'C192 '; cocn = 'mx008'

  !! 1/4 ocean + C-ATM
  !catm = 'C1152'; cocn = 'mx025'
  !catm = 'C768'; cocn = 'mx025'
  !catm = 'C384'; cocn = 'mx025'

  fdst = trim(catm)//"_mosaic.nc"
  fsrc = "Ct."//trim(cocn)//"_SCRIP_land.nc"
  fwgt = "Ct."//trim(cocn)//".to."//trim(catm)//".nc"

  if (mype == 0) print '(A,i6)','fsrc = '//trim(fsrc)//', fdst= '//trim(fdst)//', fwgt= '//trim(fwgt)//' npe = ',numpe

  call ESMF_RegridWeightGen(srcFile=trim(fsrc),dstFile=trim(fdst),         &
       weightFile=trim(fwgt), regridmethod=method,                         &
       unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, ignoreDegenerate=.true., &
       netcdf4fileFlag=.true., tileFilePath="./", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

end program repro
