program ocnpost

  use vartypedefs

  implicit none

  character(len=240) :: filesrc, filedst, wgtsfile
  character(len=120) :: wgtsdir

  integer, parameter :: nxt = 1440; nyt = 1080; nlevs = 40  !tripole dimensions, 1/4deg
  integer, parameter :: nxr = 1000; nyr =  720              !regular grid dimensions, 1/4deg

  real, dimension(nxt,nyt) :: cosrot, sinrot
  real, dimension(nxt,nyt) :: invar2d
  real, dimension(nxt*nyt) :: invar1d
  real, dimension(nxr*nyr) :: outvar1d
  real, dimension(nxr,nyr) :: outvar2d

  ! mask arrays. the tripole mask has 1s on land and 0s at valid points
  ! when remapped, any desmask value > 0 identifies land values that have
  ! crept into the field. the mapped fields will be masked using this masked array
  !real, dimension(nxt,nyt,nlevs) :: tripolemask3d
  !real, dimension(nxr,nyr,nlevs) :: destmask3d

  ! read 3D temp variable to make mapped mask and rotation variables to rotate vectors
  ! filesrt = ....
  ! get temp;  mask3d=1 where temp=missing else 0

  ! call ocnvars_typedefine
  ! open the source file and obtain the units and long names
  ! set up output file
  ! read cosrot,sinrot and temp for rgmask

  ! make 3dmask

  ! loop over variables and get a 2d, unpaired
  ! loop over variables and get a 3d, upaired
  ! get 2d paired
  ! get 3d paired

  ! at this point have either a 2 or 3dim variable or variable pair
  ! reshape to 1d (nxt*nyt) or 2d (nxt*nyt,nlevs)
  ! unstagger if grid ne ct
  ! if part of pair, rotate
  ! map
  ! reshape 1d(nxr*nyr) to (nxr,nyr)

  ! subroutine unstagger(varin, wgtsfile, varout)
  ! end subroutine unstagger

end program ocnpost
