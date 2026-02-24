!> Unit test for reshape_staggers routine
!!
!! This test checks the reshaping of staggered grid points for both a global domain
!! and an extracted subdomain
!!
!! @author Denise.Worthen@noaa.gov
program ftst_reshape_staggers

  use assertion_mod, only: assert_equal
  use gengrid_kinds, only: dbl_kind, int_kind, CL
  use gengrid_utils, only: reshape_staggers
  use grdvars      , only: nv

  implicit none

  integer, parameter :: nx = 5, ny = 4
  integer, parameter :: ngrids = 2
  integer, parameter :: maxtests = 50, nresults = ngrids*maxtests

  logical           :: ispassing(nresults)
  character(len=8)  :: gridname(ngrids) = (/'Global  ','Regional'/)
  character(len=CL) :: testmsg(nresults) = ' '

  integer           :: iind(2), jind(2)
  ! test data
  real(dbl_kind)    :: lon(nx, ny), lat(nx, ny)
  integer(int_kind) :: mask(nx, ny)
  real(dbl_kind)    :: lonvert(nx, ny, nv), latvert(nx, ny, nv)
  ! result data
  real(dbl_kind), allocatable    :: cnlons(:), cnlats(:)
  integer(int_kind), allocatable :: cnmask(:)
  real(dbl_kind), allocatable    :: crlons(:, :), crlats(:, :)

  integer :: ng, nt, ntests
  integer :: i, j, n
  integer :: ib, ie, jb, je, idim, jdim
  integer :: idx, jdx, idx1

  character(len=CL) :: msg, msg_out
  logical :: status

  ! Initialize global test data; coordinate encoded
  do j = 1, ny
     do i = 1, nx
        lon(i,j) =  10.0_dbl_kind * i + j
        lat(i,j) = -10.0_dbl_kind * i - j
     end do
  end do

  !Bu=>staggers for value at lon,lat
  lon = lon+0.5
  lat = lat+0.5

  xlon(:) =  lon(:,1)
  xlat(:) = -lat(:,1)

  ! lat and lon of Ct_verts are the But lat,lon w/ offsets
  !call fill_vertices(iVertCt, jVertCt, latBu, lonBu, xlatBu, xlonBu, latCt_vert, lonCt_vert, 0)


  assert_equal latvert(3,2,:) = (/lat(3,2),lat(2,3),lat(2,2),lat(3,1)/)

  assert_equal lonvert(3,2,:) = (/lon(3,2),lon(2,3),lon(2,2),lon(3,1)/)

  assert_equal lonvert(1,1,:) = (/lon(1,1),lon(nx,1),xlon(nx),xlon(1)/)


  !Ct=>staggers for lon+0.5,lat+0.5
  ! need xlon,xlat for 'cross-pole' values, do these need to make sense? "ipole" = 3 here, make 'reverse values'?

end program ftst_reshape_staggers
