!> Unit test for reshape_staggers routine
!!
!! This test checks the filling of vertex values for each stagger
!! location. This test relies on the known Arakawa C-grid (NE) staggering.
!!
!!          Bu----Cv(i,j)---Bu (i,j)
!!           |              |
!!          Cu    Ct(i,j)   Cu (i,j)
!!           |              |
!! (i-1,j-1) Bu----Cv-------Bu

!! @author Denise.Worthen@noaa.gov
program ftst_fill_vertices

  use assertion_mod, only: assert_equal
  use gengrid_kinds, only: dbl_kind, int_kind, CL, CS
  use vertices     , only: fill_vertices
  use grdvars      , only: nv, iVertCt, jVertCt

  implicit none

  integer, parameter     :: nx = 5, ny = 4
  integer, parameter     :: maxtests = 50, nresults = maxtests

  logical                :: ispassing(nresults)
  character(len=CL)      :: testmsg(nresults) = ' '

  integer                :: iind(2), jind(2)
  ! static test data
  real(dbl_kind)         :: testlon(nx, ny), testlat(nx, ny)
  real(dbl_kind)         :: xtestlon(nx), xtestlat(nx)
  ! lon,lat for a stagger loc
  real(dbl_kind)         :: lon(nx, ny), lat(nx, ny)
  integer, dimension(nv) :: iVert, jVert
  real(dbl_kind)         :: xlon(nx), xlat(nx)
  real(dbl_kind)         :: xoff, yoff
  ! result data
  real(dbl_kind) :: lonvert(nx, ny, nv), latvert(nx, ny, nv)

  integer :: nt, ntests
  integer :: i, j, n
  character(len=CS) :: stagger
  character(len=CL) :: msg, msg_out
  logical :: status

  ! Initialize global test data; coordinate encoded
  ! stagger = 'Ct'
  ! xoff = 0.0; yoff = 0.0
  ! if (stagger == 'Bu') then
  !    xoff = 0.5; yoff = 0.5
  ! end if
  ! if (stagger == 'Cu') then
  !    xoff = 0.5; yoff = 0.0
  ! end if
  ! if (stagger == 'Cv') then
  !    xoff = 0.5; yoff = 0.5
  ! end if

  do j = 1, ny
     do i = 1, nx
        testlon(i,j) =  10.0_dbl_kind * i + j
        testlat(i,j) =  10.0_dbl_kind * i - j
     end do
  end do
  do i = 1,nx
     xtestlon(i) = 90.0_dbl_kind + i
     xtestlat(i) = 90.0_dbl_kind + i
  end do


  do j = ny,1,-1
     print '(i3,5f8.2)',j,(testlon(i,j),i=1,nx)
  end do
  print *
  do j = ny,1,-1
     print '(i3,5f8.2)',j,(testlat(i,j),i=1,nx)
  end do
  print *


  !iVertCu = iVertCt + 1; jVertCu = jVertCt + 0
  !iVertCv = iVertCt + 0; jVertCv = jVertCt + 1
  !iVertBu = iVertCt + 1; jVertBu = jVertCt + 1
  !call fill_vertices(iVertCt, jVertCt, latBu, lonBu, xlatBu, xlonBu, latCt_vert, lonCt_vert, 0)
  !call fill_vertices(iVertCu, jVertCu, latCv, lonCv, xlatCv, xlonCv, latCu_vert, lonCu_vert, 0)
  !call fill_vertices(iVertCv, jVertCv, latCu, lonCu, xlatCu, xlonCu, latCv_vert, lonCv_vert)
  !call fill_vertices(iVertBu, jVertBu, latCt, lonCt, xlatCt, xlonCt, latBu_vert, lonBu_vert)


  ! if (stagger == 'Bu') then
  !    xoff = 0.5; yoff = 0.5
  ! end if
  ! if (stagger == 'Cu') then
  !    xoff = 0.5; yoff = 0.0
  ! end if
  ! if (stagger == 'Cv') then
  !    xoff = 0.5; yoff = 0.5
  ! end if
  ! assert_equal(actual,expected..

  nt = 0
#ifdef test
  ! vertices for Ct and Cu require "j=0" values; the Bu grid -> Ct vertices
  !
  ! input data for Bu grid; Bu grid points are at +0.5 in i,j; the Bu grid -> Ct vertices
  xoff = 0.5; yoff = 0.5
  lon = testlon + xoff
  lat = testlat + yoff
  xlon = xtestlon + xoff
  xlat = xtestlat + yoff

  iVert = iVertCt; jVert = jVertCt
  call fill_vertices(iVert, jVert, lat, lon, xlat, xlon, latvert, lonvert, 0)

  nt = nt + 1; msg = 'Ct lat vertices from Bu grid at 3,2'
  call assert_equal(latvert(3,2,:),(/lat(3,2),lat(2,2),lat(2,1),lat(3,1)/),0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)

  nt = nt + 1; msg = 'Ct lon vertices from Bu grid at 4,2'
  call assert_equal(lonvert(4,2,:),(/lon(4,2),lon(3,2),lon(3,1),lon(4,1)/),0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)

  nt = nt + 1; msg = 'Ct lon vertices from Bu grid, bottom '
  call assert_equal(lonvert(1,1,:),(/lon(1,1),lon(5,1),xlon(5),xlon(1)/),0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)

  nt = nt + 1; msg = 'Ct lat vertices from Bu grid, top '
  call assert_equal(latvert(3,4,:),(/lat(3,4),lat(2,4),lat(2,3),lat(3,3)/),0.0_dbl_kind,msg,status,msg_out)

  !print *,lonvert(1,1,:)
  !print *,lon(1,1),lon(5,1),xlon(5),xlon(1)
  print *,status,trim(msg_out)
#endif
  ! input data for Cv grid; Cv grid points are at +0.5 in j; the Cv grid -> Cu vertices
  xoff = 0.0; yoff = 0.5
  lon = testlon + xoff
  lat = testlat + yoff
  xlon = xtestlon + xoff
  xlat = xtestlat + yoff
  do j = ny,1,-1
     print '(i3,5f8.2)',j,(lon(i,j),i=1,nx)
  end do
  print *
  do j = ny,1,-1
     print '(i3,5f8.2)',j,(lat(i,j),i=1,nx)
  end do
  print *
  print *,lon(3,2),lat(3,2)

  iVert = iVertCt + 1; jVert = jVertCt + 0
  print '(a8,4i6)','iVert ',(iVert(i),i=1,4)
  print '(a8,4i6)','jVert ',(jVert(i),i=1,4)
  call fill_vertices(iVert, jVert, lat, lon, xlat, xlon, latvert, lonvert, 0)

  nt = nt + 1; msg = 'Cu lon vertices from Cv grid'
  call assert_equal(lonvert(3,2,:), &
       (/                           &
       lon(3+ivert(1),2+jvert(1)),  &
       lon(3+ivert(2),2+jvert(2)),  &
       lon(3+ivert(3),2+jvert(3)),  &
       lon(3+ivert(4),2+jvert(4))   &
       /),                          &
       0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)
  print '(4f8.2)',lon(3,2),lon(2,2),lon(2,1),lon(3,1)
  do n = 1,nv
     print '(i3,f8.2)',n,lonvert(3,2,n)
  end do
  !call assert_equal(lonvert(1,1,:),(/lon(1,1),lon(5,1),xlon(5),xlon(1)/),0.0_dbl_kind,msg,status,msg_out)
  !print *,status,trim(msg_out)

#ifdef test
  ! vertices for Cv and Bu require j=ny+1 values; the Ct grid -> Bu vertices; Cu grid -> Cv vertices

  ! test for Bu vertices
  xoff = 0.0; yoff = 0.0
  lon = testlon + xoff
  lat = testlat + xoff

  iVertBu = iVertCt + 1; jVertBu = jVertCt + 1
  call fill_vertices(iVertBu, jVertBu, lat, lon, xlat, xlon, latvert, lonvert)

  nt = nt+1; msg = 'Bu vertex lat'
  call assert_equal(latvert(3,2,:),(/lat(3,2),lat(2,2),lat(2,1),lat(3,1)/),0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)



  ! vertices for Ct and Cu require "j=0" values; the Bu grid -> Ct vertices; Cv grid -> Cu vertices


  xoff = 0.5; yoff = 0.5
  ! input data for Bu grid
  lon = lon + xoff
  lat = lat + yoff

  call fill_vertices(iVertCt, jVertCt, lat, lon, xlat, xlon, latvert, lonvert, 0)

  call assert_equal(lonvert(3,2,:),(/lon(3,2),lon(2,2),lon(2,1),lon(3,1)/),0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)

  call assert_equal(lonvert(1,1,:),(/lon(1,1),lon(5,1),xlon(5),xlon(1)/),0.0_dbl_kind,msg,status,msg_out)
  print *,status,trim(msg_out)



  ! Cu grid is used to find vertices of lat,lon of Cv points
  xoff = 0.5; yoff = 0.0
  ! input data for Cu grid
  lon = lon + xoff
  lat = lat + yoff

  !call assert_equal(latvert(3,4,1),lat(3,4),0.0_dbl_kind,msg,status,msg_out)
  !print *,status
  !nt = nt+1; msg = 'lower left Ct vertex lat'
  !call assert_equal(latvert(3,4,3),lat(2,3),0.0_dbl_kind,msg,status,msg_out)
  !print *,status
#endif
end program ftst_fill_vertices
