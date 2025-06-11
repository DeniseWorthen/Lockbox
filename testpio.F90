program testpio

  use netcdf
  !use pio

  implicit none

  !type(iosystem_desc_t) :: ice_pio_subsystem
  !type(file_desc_t)     :: File

  integer :: nmode, nmode0, pio_iotype, status
  integer :: ierror

  integer :: rc, ncid

  integer :: ni = 1440, nj = 1080

  integer :: i2,j2
  integer :: maxi, maxj
  integer :: status

  character(len=120) :: fname
  character(len=3) :: cname
  i2 = 1
  write(cname,'(i3.3)')i2
  print *,trim(cname)





#ifdef test
  print *,'PIO_64BIT_OFFSET = ',PIO_64BIT_OFFSET ! cdf2,pnetcdf2
  print *,'PIO_64BIT_DATA = ',PIO_64BIT_DATA ! cdf5, pnetcdf5
  print *,'PIO_NOCLOBBER = ',PIO_NOCLOBBER
  print *,'PIO_CLOBBER = ',PIO_CLOBBER

  nmode=PIO_64BIT_OFFSET
  print *,'ior(PIO_NOCLOBBER,PIO_64BIT_OFFSET) = ',ior(PIO_NOCLOBBER,nmode)
  nmode=PIO_64BIT_DATA
  print *,'ior(PIO_NOCLOBBER,PIO_64BIT_DATA) = ',ior(PIO_NOCLOBBER,nmode)

  nmode=PIO_64BIT_OFFSET
  print *,'ior(PIO_CLOBBER,PIO_64BIT_OFFSET) = ',ior(PIO_CLOBBER,nmode)
  nmode=PIO_64BIT_DATA
  print *,'ior(PIO_CLOBBER,PIO_64BIT_DATA) = ',ior(PIO_CLOBBER,nmode)

  print *,'PIO_IOTYPE_NETCDF4P ',PIO_IOTYPE_NETCDF4P
  print *,'NF90_NETCDF4 = ',NF90_NETCDF4
  print *,'NF90_64BIT_OFFSET = ',NF90_64BIT_OFFSET

  print *,'pnetcdf ',pio_iotype_pnetcdf
  print *,'netcdf ',PIO_iotype_netcdf
  print *,'netcdf4c ',PIO_iotype_netcdf4c
  print *,'netcdf4p ',PIO_iotype_netcdf4p

  !nmode = pio_noclobber
  !print *,ior(nmode,PIO_IOTYPE_PNETCDF)
  !print *,ior(nmode,PIO_IOTYPE_NETCDF)

  nmode0 = 0
  nmode = pio_noclobber
  print *,ior(nmode0,nmode)
  !if(pio_iotype == PIO_IOTYPE_NETCDF .or. pio_iotype == PIO_IOTYPE_PNETCDF) then
  !   nmode = ior(nmode,pio_ioformat)
  !endif

  !elseif (fformat == 'cdf5' .or. fformat == 'pnetcdf5') then
  ! nmode0 = PIO_64BIT_DATA

  rc = nf90_create('test.nc',NF90_NETCDF4, ncid)
  print *,trim(nf90_strerror(rc))
#endif
end program testpio
