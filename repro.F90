program repro

  use ESMF
  use netCDF

  implicit none

  type(ESMF_RegridMethod_Flag) :: method
  type(ESMF_VM):: vm

  integer :: rc
  character(len=256) :: fsrc, fdst, fwgt

  call ESMF_Initialize(vm=vm, logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  method=ESMF_REGRIDMETHOD_CONSERVE
  fsrc = "Ct.mx008_SCRIP_land.nc"
  fdst = "C1152_mosaic.nc"
  fwgt = "Ct.mx008.to.C1152.nc"

  call ESMF_RegridWeightGen(srcFile=trim(fsrc),dstFile=trim(fdst),         &
       weightFile=trim(fwgt), regridmethod=method,                         &
       unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, ignoreDegenerate=.true., &
       netcdf4fileFlag=.true., tileFilePath="./", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

end program repro
