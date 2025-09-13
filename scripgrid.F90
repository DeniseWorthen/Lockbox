!> @file
!! @brief Write a SCRIP format file
!! @author Denise.Worthen@noaa.gov
!!
!> This module writes a SCRIP format file
!! @author Denise.Worthen@noaa.gov

module scripgrid

  use gengrid_kinds, only: dbl_kind,int_kind,CM
  use charstrings,   only: logmsg
  use vartypedefs,   only: maxvars, scripvars, scripvars_typedefine
  use netcdf

  implicit none
  private

  public :: reshade_staggers
  public :: write_scripgrid

contains
  !> Write a SCRIP grid file
  !!
  !! @param[in]  fname  the file name to write
  !! @param[in]  cstagger  the name of the stagger location
  !! @param[in]  imask (optional)  the land mask values
  !!
  !! @author Denise.Worthen@noaa.gov

  subroutine write_scripgrid(fname,idim,jdim,cnlons,cnlats,crlons,crlats,imask)

    character(len=*) , intent(in) :: fname
    character(len=*) , intent(in) :: cstagger
    integer(int_kind), intent(in) :: idim,jdim
    real(dbl_kind)   , intent(in) :: cnlons(:),cnlats(:)
    real(dbl_kind)   , intent(in) :: cnlons(:,:),cnlats(:)
    integer(int_kind), optional, intent(in) :: imask(:)

    ! local variables
    integer, parameter :: grid_rank = 2
    integer :: ii,n,id,rc,ncid,dim2(2),dim1(1)
    integer :: idimid,jdimid,kdimid

    integer, dimension(grid_rank) :: gdims
    integer(int_kind), allocatable, dimension(:) :: cnmask

    character(len=2)  :: vtype
    character(len=CM) :: vname
    character(len=CM) :: vunit

    !---------------------------------------------------------------------
    !
    !---------------------------------------------------------------------

    ! define the output variables and file name
    call scripvars_typedefine

    gdims(:) = (/idim,jdim/)

    allocate(cnmask(idim*jdim))
    if(present(imask))then
       cnmask = imask
    else
       cnmask = 1
    end if

    !---------------------------------------------------------------------
    ! create the netcdf file
    !---------------------------------------------------------------------
    ! create the file
    ! 64_bit offset reqd for 008 grid
    ! produces b4b results for smaller grids
    rc = nf90_create(trim(fname), nf90_64bit_offset, ncid)
    logmsg = '==> writing SCRIP grid to '//trim(fname)
    print '(a)',trim(logmsg)
    if(rc .ne. 0)print '(a)', 'nf90_create = '//trim(nf90_strerror(rc))

    rc = nf90_def_dim(ncid, 'grid_size', idim*jdim, idimid)
    rc = nf90_def_dim(ncid, 'grid_corners',      4, jdimid)
    rc = nf90_def_dim(ncid, 'grid_rank', grid_rank, kdimid)

    !grid_dims
    dim1(:) = (/kdimid/)
    rc = nf90_def_var(ncid, 'grid_dims', nf90_int, dim1, id)
    ! mask
    dim1(:) = (/idimid/)
    rc = nf90_def_var(ncid, 'grid_imask', nf90_int, dim1, id)
    rc = nf90_put_att(ncid, id,     'units',      'unitless')

    ! centers
    do ii = 1,2
       vname = trim(scripvars(ii)%var_name)
       vunit = trim(scripvars(ii)%unit_name)
       vtype = trim(scripvars(ii)%var_type)
       dim1(:) =  (/idimid/)
       if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim1, id)
       if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim1, id)
       if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim1, id)
       rc = nf90_put_att(ncid, id,     'units', vunit)
    enddo

    ! corners
    do ii = 3,4
       vname = trim(scripvars(ii)%var_name)
       vunit = trim(scripvars(ii)%unit_name)
       vtype = trim(scripvars(ii)%var_type)
       dim2(:) =  (/jdimid,idimid/)
       if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
       if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
       if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
       rc = nf90_put_att(ncid, id,     'units', vunit)
    enddo
    rc = nf90_enddef(ncid)

    rc = nf90_inq_varid(ncid,  'grid_dims',        id)
    rc = nf90_put_var(ncid,             id,     gdims)
    rc = nf90_inq_varid(ncid, 'grid_imask',        id)
    rc = nf90_put_var(ncid,             id,    cnmask)

    rc = nf90_inq_varid(ncid,  'grid_center_lon',        id)
    rc = nf90_put_var(ncid,                   id,    cnlons)
    rc = nf90_inq_varid(ncid,  'grid_center_lat',        id)
    rc = nf90_put_var(ncid,                   id,    cnlats)

    rc = nf90_inq_varid(ncid,  'grid_corner_lon',        id)
    rc = nf90_put_var(ncid,                   id,    crlons)
    rc = nf90_inq_varid(ncid,  'grid_corner_lat',        id)
    rc = nf90_put_var(ncid,                   id,    crlats)

    rc = nf90_close(ncid)

  end subroutine write_scripgrid


  subroutine reshape_staggers(lons,lats,vlons,vlats,cnlons,cnlats,crlons,crlats)

    real(dbl_kind), dimension(:,:),   intent(in) :: lons, lats
    real(dbl_kind), dimension(:,:,:), intent(in) :: vlons, vlats

    real(dbl_kind), dimension(:),     intent(out) :: cnlons, cnlats
    real(dbl_kind), dimension(:,:),   intent(out) :: crlons, crlats

    integer :: idim, jdim, kdim
    real(dbl_kind), allocatable, dimension(:,:) :: tmp

    !---------------------------------------------------------------------
    !
    !---------------------------------------------------------------------

    idim = size(cnlons,1)
    jdim = size(cnlons,2)
    kdim = size(crlons,3)

    allocate(tmp(1:idim,1:jdim))

    cnlons = 0.0
    cnlats = 0.0
    crlons = 0.0
    crlats = 0.0
    tmp = 0.0

    cnlons = reshape(lons, (/idim*jdim/))
    cnlats = reshape(lats, (/idim*jdim/))
    do n = 1,kdim
       tmp(:,:) = vlons(:,:,n)
       crlons(n,:) = reshape(tmp, (/idim*jdim/))
       tmp(:,:) = vlats(:,:,n)
       crlats(n,:) = reshape(tmp, (/idim*jdim/))
    end do
    return

end module scripgrid
