module utils_mod

  use netcdf

  implicit none

  private

  interface getfield
     module procedure getfield2d
     module procedure getfield3d
  end interface getfield

  interface remap
     module procedure remap2d
     module procedure remap3d
  end interface remap

  interface getvecpair
     module procedure getvecpair2d
     module procedure getvecpair3d
  end interface getvecpair

  public getfield
  public getvecpair
  public remap

  logical :: debug = .true.

  contains

  subroutine getvecpair2d(fname, wdir, cosrot, sinrot, vname1, vgrid1, &
       vname2, vgrid2, dims, vecpair)

    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: wdir
    real,             intent(in)  :: cosrot(:), sinrot(:)
    character(len=*), intent(in)  :: vname1, vgrid1, vname2, vgrid2
    integer,          intent(in)  :: dims(:)
    real,             intent(out) :: vecpair(:,:)

    ! local variables
    integer :: ii
    real, dimension(dims(1)*dims(2)) :: urot, vrot
    character(len=240) :: wgtsfile
    character(len=20) :: subname = 'getvecpair2d'

    if (debug)print *,'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,1), wgts=trim(wgtsfile))
    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid2//'.to.Ct.bilinear.nc'
    call getfield(fname, vname2, dims=dims, field=vecpair(:,2), wgts=trim(wgtsfile))

    urot = 0.0; vrot = 0.0
    do ii = 1,dims(1)*dims(2)
       urot(ii) = vecpair(ii,1)*cosrot(ii) + vecpair(ii,2)*sinrot(ii)
       vrot(ii) = vecpair(ii,2)*cosrot(ii) - vecpair(ii,1)*sinrot(ii)
    end do
    vecpair(:,1) = urot(:)
    vecpair(:,2) = vrot(:)

    if (debug) print *,'exit '//trim(subname)

  end subroutine getvecpair2d

  subroutine getvecpair3d(fname, wdir, cosrot, sinrot, vname1, vgrid1, &
       vname2, vgrid2, dims, vecpair)

    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: wdir
    real,             intent(in)  :: cosrot(:), sinrot(:)
    character(len=*), intent(in)  :: vname1, vgrid1, vname2, vgrid2
    integer,          intent(in)  :: dims(:)
    real,             intent(out) :: vecpair(:,:,:)

    ! local variables
    integer :: ii,k
    real, dimension(dims(1)*dims(2)) :: urot, vrot
    character(len=240) :: wgtsfile
    character(len=20) :: subname = 'getfield3d'

    if (debug)print *,'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,:,1), wgts=trim(wgtsfile))
    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid2//'.to.Ct.bilinear.nc'
    call getfield(fname, vname2, dims=dims, field=vecpair(:,:,2), wgts=trim(wgtsfile))

    do k = 1,dims(3)
       urot = 0.0; vrot = 0.0
       do ii = 1,dims(1)*dims(2)
          urot(ii)= vecpair(ii,k,1)*cosrot(ii) + vecpair(ii,k,2)*sinrot(ii)
          vrot(ii)= vecpair(ii,k,2)*cosrot(ii) - vecpair(ii,k,1)*sinrot(ii)
       end do
       vecpair(:,k,1) = urot(:)
       vecpair(:,k,2) = vrot(:)
    end do
    if (debug) print *,'exit '//trim(subname)

  end subroutine getvecpair3d

  subroutine getfield2d(fname, vname, dims, field, wgts)
    character(len=*),           intent(in)  :: fname, vname
    integer,                    intent(in)  :: dims(:)
    real,                       intent(out) :: field(:)
    character(len=*), optional, intent(in)  :: wgts

    ! local variable
    integer           :: ncid, varid, rc
    real              :: fval
    real, allocatable :: a2d(:,:)
    real, allocatable :: atmp(:)
    character(len=20) :: subname = 'getfield2d'

    if (debug)print *,'enter '//trim(subname)//' variable '//vname

    allocate(a2d(dims(1),dims(2)))
    allocate(atmp(dims(1)*dims(2)))

    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    rc = nf90_inq_varid(ncid, trim(vname), varid)
    rc = nf90_get_var(ncid, varid, a2d)
    rc = nf90_get_att(ncid, varid, '_FillValue', fval)
    rc = nf90_close(ncid)

    atmp(:) = reshape(a2d, (/dims(1)*dims(2)/))
    if(present(wgts)) then
       where(atmp .eq. fval)atmp = 0.0
       call remap(trim(wgts), src_field=atmp, dst_field=field)
    else
       field = atmp
    end if
    if (debug) print *,'exit '//trim(subname)//' variable '//vname

  end subroutine getfield2d

  subroutine getfield3d(fname, vname, dims, field, wgts)
    character(len=*),           intent(in)  :: fname, vname
    integer,                    intent(in)  :: dims(:)
    real,                       intent(out) :: field(:,:)
    character(len=*), optional, intent(in)  :: wgts

    ! local variable
    integer           :: ncid, varid, rc
    real              :: fval
    real, allocatable :: a3d(:,:,:)
    real, allocatable :: atmp(:,:)
    character(len=20) :: subname = 'getfield3d'

    if (debug)print *,'enter '//trim(subname)//' variable '//vname

    allocate(a3d(dims(1),dims(2),dims(3)))
    allocate(atmp(dims(1)*dims(2),dims(3)))

    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    rc = nf90_inq_varid(ncid, trim(vname), varid)
    rc = nf90_get_var(ncid, varid, a3d)
    rc = nf90_get_att(ncid, varid, '_FillValue', fval)
    rc = nf90_close(ncid)

    atmp(:,:) = reshape(a3d, (/dims(1)*dims(2),dims(3)/))
    if(present(wgts)) then
       where(atmp .eq. fval)atmp = 0.0
       call remap(trim(wgts), nk=dims(3), src_field=atmp, dst_field=field)
    else
       field = atmp
    end if
    if (debug) print *,'exit '//trim(subname)//' variable '//vname

  end subroutine getfield3d

  subroutine remap2d(fname, src_field, dst_field)

    character(len=*),   intent(in)  :: fname
    real, dimension(:), intent(in)  :: src_field
    real, dimension(:), intent(out) :: dst_field

    ! local variables
    integer :: ncid, rc, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    ! not real, but get_var(ncid, id, col) seg faults if col is int
    ! see https://github.com/Unidata/netcdf-fortran/issues/413
    real(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8), allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap2d'

    if (debug)print *,'enter '//trim(subname)

    ! retrieve the weights
    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    rc = nf90_inq_dimid(ncid, 'n_s', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_s)
    rc = nf90_inq_dimid(ncid, 'n_a', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_a)
    rc = nf90_inq_dimid(ncid, 'n_b', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_b)

    allocate(col(1:n_s))
    allocate(row(1:n_s))
    allocate(  S(1:n_s))

    rc = nf90_inq_varid(ncid, 'col', id)
    rc = nf90_get_var(ncid,     id, col)
    rc = nf90_inq_varid(ncid, 'row', id)
    rc = nf90_get_var(ncid,     id, row)
    rc = nf90_inq_varid(ncid,   'S', id)
    rc = nf90_get_var(ncid,      id,  S)
    rc = nf90_close(ncid)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(ii) = dst_field(ii) + S(i)*src_field(jj)
    enddo
    deallocate(col,row,S)

    if (debug) print *,'exit '//trim(subname)

  end subroutine remap2d

  subroutine remap3d(fname, nk, src_field, dst_field)
    character(len=*),     intent(in)  :: fname
    integer,              intent(in)  :: nk
    real, dimension(:,:), intent(in)  :: src_field
    real, dimension(:,:), intent(out) :: dst_field

    ! local variables
    integer :: ncid, rc, id
    integer :: i,ii,jj,k
    integer :: n_a, n_b, n_s
    ! not real, but get_var(ncid, id, col) seg faults if col is int
    ! see https://github.com/Unidata/netcdf-fortran/issues/413
    real(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8), allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap3d'

    if (debug)print *,'enter '//trim(subname)//' weights = '//trim(fname)
    !print *,size(src_field,1),size(src_field,2)
    !print *,size(dst_field,1),size(dst_field,2)

    ! retrieve the weights
    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    rc = nf90_inq_dimid(ncid, 'n_s', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_s)
    rc = nf90_inq_dimid(ncid, 'n_a', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_a)
    rc = nf90_inq_dimid(ncid, 'n_b', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_b)

    allocate(col(1:n_s))
    allocate(row(1:n_s))
    allocate(  S(1:n_s))
    !print *,'n_s = ',n_s,' n_a = ',n_a,' n_b = ',n_b

    rc = nf90_inq_varid(ncid, 'col', id)
    rc = nf90_get_var(ncid,     id, col)
    rc = nf90_inq_varid(ncid, 'row', id)
    rc = nf90_get_var(ncid,     id, row)
    rc = nf90_inq_varid(ncid,   'S', id)
    rc = nf90_get_var(ncid,      id,  S)
    rc = nf90_close(ncid)

    dst_field = 0.0
    do k = 1,nk
       do i = 1,n_s
          ii = row(i); jj = col(i)
          dst_field(ii,k) = dst_field(ii,k) + S(i)*src_field(jj,k)
       enddo
    end do
    deallocate(col,row,S)

    if (debug) print *,'exit '//trim(subname)
  end subroutine remap3d
end module utils_mod
