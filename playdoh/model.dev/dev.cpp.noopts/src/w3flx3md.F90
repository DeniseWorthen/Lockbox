!> @file
!> @brief flux/stress computations according tolman and chalikov (1996).
!>
!> @author h. l. tolman
!> @date   20-apr-2010
!>
!/ ------------------------------------------------------------------- /
!/    preprocessing macros
!/
!/                  +-----------------------------------+
!/                  | wavewatch iii           noaa/ncep |
!/                  |        t. j. campbell, nrl        |
!/                  |                               cpp |
!/                  | last update :         26-oct-2015 |
!/                  +-----------------------------------+
!/
!/    10-dec-2014 : origination.                     ( version 5.04 )
!/    26-oct-2015 : replace c style comments with fortran
!/                  style comments.                  ( version 5.09 )
!/
!/ 1. purpose :
!/
!/    define preprocessor macros for ww3 ftn source code.
!/
!/ 2. method :
!/
!/ 3. parameters :
!/
!/ 4. subroutines used :
!/
!/ 5. called by :
!/
!/ 6. error messages :
!/
!/ 7. remarks :
!/
!/    this file uses fortran style comments, and hence, can only be
!/    included in the fortran (ftn) source files.  the fortran style
!/    comments are used because not all fortran pre-processors recognize
!/    the c style comments.
!/
!/    the "src/w3macros.h" and 36 macros are defined by cpp.
!/
!/ 8. structure :
!/
!/    see source code.
!/
!/ 9. source code :
!/
!/ ------------------------------------------------------------------- /
!/
!/ macros to wrap checking allocate/deallocate status
!/
!/
!/ end of w3macros.h ------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!>
!> @brief flux/stress computations according tolman and chalikov (1996).
!>
!> @details cap on flux added compared to w3flx2.
!>
!> @author h. l. tolman
!> @date   20-apr-2010
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3flx3md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         20-apr-2010 |
  !/                  +-----------------------------------+
  !/
  !/    05-jul-2006 : origination.                        ( version 3.09 )
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    20-apr-2010 : fix intent of ust.                ( version 3.14.1 )
  !/
  !/    copyright 2009-2010 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     flux/stress computations according tolman and chalikov (1996).
  !     cap on flux added compared to w3flx2.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3flx3    subr. public   stresses according to tc (1996).
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !     - originally used with source term !/st2.
  !
  !  6. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief flux/stress computations according tolman and chalikov (1996).
  !>
  !> @details cap on flux added compared to w3flx2.
  !>
  !> @param[in]    zwind height of wind.
  !> @param[in]    depth depth.
  !> @param[in]    fp    peak frequency.
  !> @param[in]    u     wind speed.
  !> @param[in]    udir  wind direction.
  !> @param[inout] ust   friction velocity.
  !> @param[out]   ustd  direction of friction velocity.
  !> @param[out]   z0    z0 in profile law.
  !> @param[out]   cd    drag coefficient.
  !>
  !> @author h. l. tolman
  !> @date   10-jan-2014
  !>
  subroutine w3flx3 ( zwind, depth, fp, u, udir, ust, ustd, z0, cd )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-jan-2014 |
    !/                  +-----------------------------------+
    !/
    !/    05-jul-2006 : origination.                        ( version 3.09 )
    !/    20-apr-2010 : fix intent of ust.                ( version 3.14.1 )
    !/    10-jan-2014 : add max on division by ust          ( version 4.18 )
    !/                  (this was already done for w3flx2 on 16 sep 2011)
    !/    10-jan-2014 : add a min value for fp              ( version 4.18 )
    !/
    !  1. purpose :
    !
    !     flux/stress computations according tolman and chalikov (1996).
    !     cap on flux added compared to w3flx2.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       zwind   real   i   hight of wind.
    !       depth   real   i   depth.
    !       fp      real   i   peak frequency.
    !       u       real   i   wind speed.
    !       udir    real   i   wind direction.
    !       ust     real   o   friction velocity.
    !       ustd    real   0   direction of friction velocity.
    !       z0      real   o   z0 in profile law.
    !       cd      real   o   drag coefficient.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants
    use w3gdatmd, only: nittin, cinxsi, cd_max, cap_id
    use w3odatmd, only: ndse, iaproc, naperr
    use w3servmd, only: extcde
    use w3dispmd, only: dsie, n1max, ewn1
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: zwind, depth, fp, u, udir
    real, intent(inout)     :: ust
    real, intent(out)       :: ustd, z0, cd
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: i1, itt
    real                    :: sqrth, six, r1, wnp, cp, unz, alpha, &
         rdch, afp
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  peak phase velocity -------------------------------------------- *
    !
    ! ----- start of inlined and reduced wavnu1 -----
    !
    afp    = tpi * max ( fp, 0.001)
    !
    sqrth  = sqrt ( depth )
    six    = afp * sqrth
    i1     = int ( six / dsie )
    if (i1.le.n1max) then
      r1 = six/dsie - real(i1)
      wnp    = ( (1.-r1)*ewn1(i1) + r1*ewn1(i1+1) ) / depth
    else
      wnp    = afp * afp / grav
    end if
    !
    ! -----  end of inlined and reduced wavnu1  -----
    !
    cp     = afp / wnp
    !
    ! 2.  itterative stress computation ---------------------------------- *
    !
    unz    = max ( 0.01 , u )
    ustd   = udir
    !
    do itt=1, nittin
      alpha  = 0.57 / ( cp / max (ust,0.0001) )**(1.5)
      rdch   = max ( 0. ,                                           &
           log ( ( zwind * grav) / ( cinxsi * sqrt(alpha) * unz**2) ) )
      cd     = 0.001 * ( 0.021 + 10.4 / (rdch**1.23+1.85) )
      ust    = sqrt(cd) * unz
      z0    = zwind * exp ( -0.4 / sqrt(cd) )
    end do
    !
    ! 3.  apply limit to drag coefficient -------------------------------- *
    !
    if ( cap_id .eq. 0 ) then
      cd     = min ( cd_max, cd )
    else
      cd     = cd_max * tanh ( cd / cd_max )
    end if
    !
    ust    = sqrt(cd) * unz
    z0     = zwind * exp ( -0.4 / sqrt(cd) )
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3flx3 ----------------------------------------------------- /
    !/
  end subroutine w3flx3
  !/
  !/ end of module w3flx3md -------------------------------------------- /
  !/
end module w3flx3md
