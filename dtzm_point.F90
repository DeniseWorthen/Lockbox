  subroutine get_dtzm_point(xt,xz,dt_cool,zc,z1,z2,dtm)
    ! ===================================================================== !
    !                                                                       !
    !  description:  get dtm = mean of dT(z) (z1 - z2) with NSST dT(z)      !
    !                dT(z) = (1-z/xz)*dt_warm - (1-z/zc)*dt_cool            !
    !                                                                       !
    !  usage:                                                               !
    !                                                                       !
    !    call get_dtm12                                                     !
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
    !     z1      - lower bound of depth of sea temperature              1  !
    !     z2      - upper bound of depth of sea temperature              1  !
    !  outputs:                                                             !
    !     dtm   - mean of dT(z)  (z1 to z2)                              1  !
    !
    use machine , only : kind_phys

    implicit none

    real (kind=kind_phys), intent(in)  :: xt,xz,dt_cool,zc,z1,z2
    real (kind=kind_phys), intent(out) :: dtm
    ! Local variables
    real (kind=kind_phys) :: dt_warm,dtw,dtc

    !
    ! get the mean warming in the range of z=z1 to z=z2
    !
    dtw = 0.0
    if ( xt > 0.0 ) then
       dt_warm = (xt+xt)/xz      ! Tw(0)
       if ( z1 < z2) then
          if ( z2 < xz ) then
             dtw = dt_warm*(1.0-(z1+z2)/(xz+xz))
          elseif ( z1 < xz .and. z2 >= xz ) then
             dtw = 0.5*(1.0-z1/xz)*dt_warm*(xz-z1)/(z2-z1)
          endif
       elseif ( z1 == z2 ) then
          if ( z1 < xz ) then
             dtw = dt_warm*(1.0-z1/xz)
          endif
       endif
    endif
    !
    ! get the mean cooling in the range of z=z1 to z=z2
    !
    dtc = 0.0
    if ( zc > 0.0 ) then
       if ( z1 < z2) then
          if ( z2 < zc ) then
             dtc = dt_cool*(1.0-(z1+z2)/(zc+zc))
          elseif ( z1 < zc .and. z2 >= zc ) then
             dtc = 0.5*(1.0-z1/zc)*dt_cool*(zc-z1)/(z2-z1)
          endif
       elseif ( z1 == z2 ) then
          if ( z1 < zc ) then
             dtc = dt_cool*(1.0-z1/zc)
          endif
       endif
    endif

    !
    ! get the mean T departure from Tf in the range of z=z1 to z=z2
    !
    dtm = dtw - dtc

  end subroutine get_dtzm_point
