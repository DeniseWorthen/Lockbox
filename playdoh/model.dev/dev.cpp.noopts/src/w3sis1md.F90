!> @file
!> @brief diffusion source term.
!>
!> @author s. zieger
!> @date   20-dec-2013
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
!> @brief diffusion source term.
!>
!> @author s. zieger
!> @date   20-dec-2013
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3sis1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           s. zieger               |
  !/                  |                        fortran 90 |
  !/                  | last update :         20-dec-2013 |
  !/                  +-----------------------------------+
  !/
  !/    for updates see w3sid1 documentation.
  !/
  !  1. purpose :
  !
  !     diffusion source term.
  !
  !  2. variables and types :
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sis1    subr. public   ice scattering term.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !     see subroutine documentation.
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !     see subroutine documentation.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public :: w3sis1
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief spectral reflection due to ice.
  !>
  !> @param[in]  a    action density spectrum (1-d).
  !> @param[in]  ice  sea ice concentration.
  !> @param[out] s    source term (1-d version).
  !>
  !> @author s. zieger
  !> @date
  !>
  subroutine w3sis1 (a, ice, s)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           s. zieger               |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-dec-2013 |
    !/                  +-----------------------------------+
    !/
    !/    16-nov-2012 : origination.                        ( version 4.14 )
    !/                                                        (s. zieger)
    !  1. purpose :
    !     spectral reflection due to ice.
    !
    !/ ------------------------------------------------------------------- /
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d)
    !       ice     real  i   sea ice concentration.
    !       s       r.a.  o   source term (1-d version).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    ascii point output post-processor.
    !      w3exnc    subr.   n/a    netcdf point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !     none.
    !
    !  7. remarks :
    !
    !     if ice parameter 1 is zero, no calculations are made.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !     !/t   enable general test output.
    !           2-d print plot of source term.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use w3gdatmd, only: nk, nth, nspec, sig, sig2, dden2
    use w3gdatmd, only: dtmin, th, dth, ecos, dtmin
    use w3gdatmd, only: is1c1, is1c2
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    real, intent(in)        :: a(nspec), ice
    real, intent(out)       :: s(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ik, ith, ith2, is, is2
    real                    :: alpha
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    s     = 0.
    !
    !     calculate scattering coefficient (linear transfer function) ---- *
    alpha = max(0., is1c1 * ice + is1c2)
    !
    if (alpha.gt.0. .and. ice.gt.0.) then
      ! 1. calculate the derivative ---------------------------------------- *
      do ik = 1,nk
        do ith = 1,nth
          is  = ith+(ik-1)*nth
          if (a(is).ge.0.) then
            s(is)   = s(is)  -  alpha * a(is)
            do ith2 = 1,nth
              is2 = ith2+(ik-1)*nth
              if (is2.ne.is) then
                s(is2) = s(is2)  +  alpha * a(is) / real(nth-1)
              end if
            end do
          end if
        end do
      end do
      !
      s = s / dtmin
      !
      !
    end if
    ! formats
8000 format (' test w3sis1 : alpha :',e10.3)
    !
    !/
    !/ end of w3sis1 ----------------------------------------------------- /
    !/
  end subroutine w3sis1
  !/
  !/ end of module w3sis1md -------------------------------------------- /
  !/
end module w3sis1md
