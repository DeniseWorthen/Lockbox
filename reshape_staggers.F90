
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
