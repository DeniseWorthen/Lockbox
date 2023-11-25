  subroutine get_dtzm_2d(xt,xz,dt_cool,zc,z1,z2,dtm)
  ! subroutine get_dtzm_2d(xt,xz,dt_cool,zc,wet,z1,z2,nx,ny,nth,dtm)
  !subroutine get_dtzm_2d(xt,xz,dt_cool,zc,wet,icy,z1,z2,nx,ny,dtm)
  ! ===================================================================== !
  !                                                                       !
  !  description:  get dtm = mean of dT(z) (z1 - z2) with NSST dT(z)      !
  !                dT(z) = (1-z/xz)*dt_warm - (1-z/zc)*dt_cool            !
  !                                                                       !
  !  usage:                                                               !
  !                                                                       !
  !    call get_dtzm_2d                                                   !
  !                                                                       !
  !       inputs:                                                         !
  !          (xt,xz,dt_cool,zc,z1,z2,                                     !
  !       outputs:                                                        !
  !          dtm)                                                         !
  !                                                                       !
  !  program history log:                                                 !
  !                                                                       !
  !         2015  -- xu li       createad original code                   !
  !  inputs:                                                              !
  !     xt      - real, heat content in dtl                            1  !
  !     xz      - real, dtl thickness                                  1  !
  !     dt_cool - real, sub-layer cooling amount                       1  !
  !     zc      - sub-layer cooling thickness                          1  !
  !     wet     - logical, flag for wet point (ocean or lake)          1  !
  !     icy     - logical, flag for ice point (ocean or lake)          1  !
  !     nx      - integer, dimension in x-direction (zonal)            1  !
  !     ny      - integer, dimension in y-direction (meridional)       1  !
  !     z1      - lower bound of depth of sea temperature              1  !
  !     z2      - upper bound of depth of sea temperature              1  !
  !     nth     - integer, num of openmp thread                        1  !
  !  outputs:                                                             !
  !     dtm   - mean of dT(z)  (z1 to z2)                              1  !
  !
  use machine , only : kind_phys

  implicit none

  !integer, intent(in) :: nx,ny, nth
 !real (kind=kind_phys), dimension(n x,ny), intent(in)  :: xt,xz,dt_cool,zc
  !logical, dimension(nx,ny), intent(in)  :: wet
  ! logical, dimension(nx,ny), intent(in)  :: wet,icy
  real (kind=kind_phys), intent(in)  :: z1,z2
  !real (kind=kind_phys), dimension(nx,ny), intent(out) :: dtm
  ! Local variables
  integer :: i,j
  real (kind=kind_phys) :: dt_warm, dtw, dtc, xzi
  real (kind=kind_phys), parameter :: zero=0.0, half=0.5, one=1.0


  !$omp parallel do num_threads (nth) private(j,i,dtw,dtc,xzi)
!  do j = 1, ny
!     do i= 1, nx

        dtm(i,j) = zero      ! initialize dtm

!        if ( wet(i,j) ) then
           !
           !       get the mean warming in the range of z=z1 to z=z2
           !
           dtw = zero
           if ( xt(i,j) > zero ) then
              xzi = one / xz(i,j)
              dt_warm = (xt(i,j)+xt(i,j)) * xzi      ! Tw(0)
              if (z1 < z2) then
                 if ( z2 < xz(i,j) ) then
                    dtw = dt_warm * (one-half*(z1+z2)*xzi)
                 elseif (z1 < xz(i,j) .and. z2 >= xz(i,j) ) then
                    dtw = half*(one-z1*xzi)*dt_warm*(xz(i,j)-z1)/(z2-z1)
                 endif
              elseif (z1 == z2 ) then
                 if (z1 < xz(i,j) ) then
                    dtw = dt_warm * (one-z1*xzi)
                 endif
              endif
           endif
           !
           !       get the mean cooling in the range of z=0 to z=zsea
           !
           dtc = zero
           if ( zc(i,j) > zero ) then
              if ( z1 < z2) then
                 if ( z2 < zc(i,j) ) then
                    dtc = dt_cool(i,j) * (one-(z1+z2)/(zc(i,j)+zc(i,j)))
                 elseif ( z1 < zc(i,j) .and. z2 >= zc(i,j) ) then
                    dtc = half*(one-z1/zc(i,j))*dt_cool(i,j)*(zc(i,j)-z1)/(z2-z1)
                 endif
              elseif ( z1 == z2 ) then
                 if ( z1 < zc(i,j) ) then
                    dtc = dt_cool(i,j) * (one-z1/zc(i,j))
                 endif
              endif
           endif
           ! get the mean T departure from Tf in the range of z=z1 to z=z2
           dtm(i,j) = dtw - dtc
!        endif        ! if ( wet(i,j)) then
     enddo
  enddo
  !

end subroutine get_dtzm_2d
