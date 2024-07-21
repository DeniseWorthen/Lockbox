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
module w3sbt9md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa      |
  !/                  |           m. orzech     nrl       |
  !/                  |           w. e. rogers  nrl       |
  !/                  |                        fortran 90 |
  !/                  | last update :         21-nov-2013 |
  !/                  +-----------------------------------+
  !/
  !/    28-jul-2011 : origination.                        ( version 4.01 )
  !/    21-nov-2013 : preparing distribution version.     ( version 4.11 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     contains routines for computing dissipation by viscous fluid mud using
  !     ng (2000)
  !
  !  2. variables and types :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3sbt9    subr. public   fluid mud dissipation (ng 2000)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !      csinh     subr.   ??     complex sinh function
  !      ccosh     subr.   ??     complex cosh function
  !      z_wnumb   subr.   ??     compute wave number from freq & depth
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !     historical information:
  !        this started as some equations (the "b" parameter equations
  !        in subroutine "ng" below) in a standalone fortran
  !        code written by jim kaihatu, december 2004. these were adapted by
  !        erick rogers for a simple model based on governing equation
  !        similar to swan, and installed in a full version of swan in
  !        march 2005 with an informal report in may 2005. kaihatu provided
  !        a "patch" for the b equations may 2006. mud code in swan v40.41a was
  !        finalized june 2006, and v40.51 august 2007. the code was applied
  !        to cassino beach ~sep 2006. this work was presented at a conference
  !        in brazil nov 2006, and later published in rogers and holland
  !        (csr 2009). the code was adapted for ww3 by mark orzech in nov 2012
  !        (he had installed the d&l routines as bt8 in july 2011).
  !
  !     reference: ng, c.o.,2000. water waves over a muddy bed:
  !                a two-layer stokes’ boundary layer model.
  !                coastal engineering 40(3),221–242.
  !
  !  6. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3sbt9(ac,h_wdepth,s,d,ix,iy)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa      |
    !/                  |           m. orzech     nrl       |
    !/                  |           w. e. rogers  nrl       |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-nov-2013 |
    !/                  +-----------------------------------+
    !/    28-jul-2011 : origination.                        ( version 4.01 )
    !/    21-nov-2013 : preparing distribution version.     ( version 4.11 )
    !/
    !  1. purpose :
    !
    !     compute dissipation by viscous fluid mud using ng (2000)
    !     (adapted from erick rogers code by mark orzech, nrl).
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       h_wdepth  real  i   mean water depth.
    !       s         r.a.  o   source term (1-d version).
    !       d         r.a.  o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      calc_nd
    !      ng
    !     ----------------------------------------------------------------
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3srce    subr. w3srcemd source term integration.
    !      w3expo    subr.   n/a    point output post-processor.
    !      gxexpo    subr.   n/a    grads point output post-processor.
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !
    !     cg_mud calculation could be improved by using dsigma/dk instead
    !        of n*c. the latter is a "naive" method and its accuracy has
    !        not been confirmed.
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
    use w3gdatmd, only: nk,sig,nspec,mapwn
    use w3idatmd, only: mudt, mudv, mudd, inflags1
    use constants, only: pi,grav,dwat,nu_water
    use w3servmd, only: extcde
    use w3odatmd, only: ndse
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real,    intent(in)  :: h_wdepth  ! water depth, denoted "h" in ng (m)
    real,    intent(in)  :: ac(nspec) ! action density
    integer, intent(in)  :: ix, iy
    real,    intent(out) :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !  local variables
    real :: dmw(nk)
    real :: rootdg
    real :: snd
    real :: snd2
    real :: wgd
    real :: cwave
    real :: kd_rock
    real :: cg_mud
    real :: k_mud
    real :: nwave_mud
    real :: nd_mud
    real :: smudwd(nk) !  dissipation due to mud
    real :: cg_rock
    real :: k_rock
    real :: nwave_rock
    real :: nd_rock
    real :: kinvism       ! := the kinematic viscosity of the mud
    real :: kinvisw       ! := kinematic viscosity of water
    real :: rhow          ! := density of water
    real :: rhom          ! := density of mud
    real :: dm            ! := depth of mud layer
    real :: zeta          ! := this is zeta as used in ng pg. 238. it is the
    !    ratio of stokes' boundary layer thicknesses,
    !    or delta_m/delta_w
    real :: gamma         ! := this is the gamma used in ng pg. 238. this is
    !  density(water)/density(mud)
    real :: sbltw         ! := a function of viscosity and freq
    real :: sbltm         ! := a function of viscosity and freq
    real :: dtilde        ! := normalized mud depth = mud depth / delta_m,
    !  delta is the sblt= sqrt(2*visc/sigma)
    real :: ztmp
    real :: kdcutoff
    real :: kd
    integer :: is
    logical :: inan
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    !     ng (2000), waves over soft muds.
    !     based on code for swan created by erick rogers.
    !     adapted for ww3 by mark orzech, nov 2012.
    ! initialize properties from mud fields if available
    if (inflags1(-2))then
      rhom = mudd(ix,iy)
    else
      write(ndse,*)'rhom not set'
      call extcde ( 1 )
    endif
    if (inflags1(-1)) then
      dm = mudt(ix,iy)
    else
      write(ndse,*)'dm not set'
      call extcde ( 2 )
    endif
    if (inflags1(0)) then
      kinvism = mudv(ix,iy)
    else
      write(ndse,*)'kinvism not set'
      call extcde ( 3 )
    endif
    rootdg = sqrt(h_wdepth/grav)
    wgd    = rootdg*grav
    do is = 1, nk
      !       snd is dimensionless frequency
      snd = sig(is) * rootdg
      if (snd .ge. 2.5) then
        !       ******* deep water *******
        k_rock  = sig(is) * sig(is) / grav
        cg_rock = 0.5 * grav / sig(is)
        nwave_rock  = 0.5
        nd_rock = 0.
      else if (snd.lt.1.e-6) then
        !       *** very shallow water ***
        k_rock  = snd/h_wdepth
        cg_rock = wgd
        nwave_rock  = 1.
        nd_rock = 0.
      else
        snd2  = snd*snd
        cwave     = sqrt(grav*h_wdepth/(snd2+1./(1.+0.666*snd2 &
             +0.445*snd2**2 -0.105*snd2**3+0.272*snd2**4)))
        k_rock = sig(is)/cwave
        call calc_nd(k_rock,h_wdepth,snd2,nd_rock)
        nwave_rock = 0.5*(1.0+2.0*k_rock*h_wdepth/sinh(2.0*k_rock*h_wdepth))
        cg_rock= nwave_rock*cwave
        snd2=0
        cwave=0
      endif
      kdcutoff = 10.0  ! hardwired (same as w3sbt8md)
      ! now that kh is known, we can use a definition of "deep" that is
      ! consistent with the definition used in sbot
      k_mud=0.0
      dmw(is)=0.0
      kd_rock = k_rock * h_wdepth
      ! kd_rock is used to determine whether we make the mud calculation
      if((kd_rock.lt.kdcutoff).and.(dm.gt.1.0e-5))then
        kinvisw=nu_water
        rhow=dwat
        zeta=sqrt(kinvism/kinvisw)
        gamma=rhow/rhom
        sbltw=sqrt(2.0*kinvisw/sig(is))
        sbltm=sqrt(2.0*kinvism/sig(is))
        dtilde=dm/sbltm
        call ng(sig(is),h_wdepth,dtilde,zeta,sbltm,gamma,k_rock,k_mud, &
             dmw(is))
      else  !     if ( kd_rock .lt. kdcutoff ) then
        k_mud=k_rock
      end if !     if ( kd_rock .lt. kdcutoff ) then
      !    calculate  cg_mud, nwave_mud here
      cwave=sig(is)/k_mud
      ztmp=2.0*k_mud*h_wdepth
      if(ztmp.lt.70)then
        ztmp=sinh(ztmp)
      else
        ztmp=1.0e+30
      endif
      nwave_mud=0.5*(1.0+2.0*k_mud*h_wdepth/ztmp)
      cg_mud=nwave_mud*cwave
      snd2  = snd*snd
      call calc_nd(k_mud,h_wdepth,snd2,nd_mud)
      snd2=0
      cwave=0
      ! if we wanted to include the effects of mud on the real part of the
      ! wavnumber (as we do in swan), this is where we would do it.
      ! set output variables k_out, cg_out, nwave_out, nd_out, dmw.
      !kinematics       if(mud)then !
      !kinematics          k_out(is)    =k_mud
      !kinematics          cg_out(is)   =cg_mud
      !kinematics          nwave_out(is)=nwave_mud
      !kinematics          nd_out(is)   =nd_mud
      !kinematics       else ! use rocky wavenumber,etc.
      !kinematics          k_out(is)    =k_rock
      !kinematics          cg_out(is)   =cg_rock
      !kinematics          nwave_out(is)=nwave_rock
      !kinematics          nd_out(is)   =nd_rock
      !kinematics          dmw(is)=0.0
      !kinematics       endif
      kd = k_mud * h_wdepth
      if ( kd .lt. kdcutoff ) then
        ! note that "is" here is for the 1d spectrum
        smudwd(is)=2.0*dmw(is)*cg_mud
      end if
      ! nan check:
      inan = .not. ( dmw(is) .ge. -huge(dmw(is)) .and. dmw(is) &
           .le. huge(dmw(is)) )
      if (inan) then
        write(*,'(/1a/)') 'w3sbt9 error -- dmw(is) is nan'
        write(*,*)'w3sbt9: rhom, dm, kinvism = ',rhom, dm, kinvism
        write(*,*)'w3sbt9: is,nk = ',is,nk
        write(*,*)'w3sbt9: h_wdepth,kd,kdcutoff = ',h_wdepth,kd, kdcutoff
        write(*,*)'w3sbt9: k_mud,cg_mud,nwave_mud = ',k_mud,cg_mud,nwave_mud
        call extcde (1)
      end if
    end do !  do is = 1, nk
    !    *** store the results in the diagonal arrays d and s ***
    do is = 1,nspec
      ! note that "is" here is for the directional spectrum (2d)
      d(is) = -smudwd(mapwn(is))
    end do
    s = d * ac
    return
  end subroutine w3sbt9
  !/ ------------------------------------------------------------------- /
  subroutine ng(sigma,h_wdepth,dtilde,zeta,sbltm,gamma,wk,wkdr,diss)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |    e. rogers and m. orzech        |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-nov-2013 |
    !/                  +-----------------------------------+
    !
    !/    28-jul-2011 : origination.                        ( version 4.01 )
    !/    21-nov-2013 : preparing distribution version.     ( version 4.11 )
    !/
    !  1. purpose :
    !
    !     compute dissipation by viscous fluid mud using ng (2000)
    !     (adapted from erick rogers code by mark orzech, nrl).
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       sigma     real  i  radian frequency (rad)
    !       h_wdepth  real  i  water depth
    !       dtilde    real  i  normalized mud depth
    !       zeta      real  i  zeta as used in ng
    !       sbltm     real  i  mud stokes boundary layer thickness
    !       gamma     real  i  gamma as used in ng
    !       wk        real  i  wavenumber w/out mud
    !       wkdr      real  o  wavenumber w/mud
    !       diss      real  o  dissipation rate
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      none.
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      w3sbt9    subr. w3sbt9md main routine (all freqs)
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !     calculations for the "b coefficients" came from a code by jim kaihatu
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       none.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ ------------------------------------------------------------------- /
    !/
    !
    implicit none
    ! input variables :
    real, intent(in)  ::  sigma   ! radian frequency (rad)
    real, intent(in)  ::  h_wdepth! water depth, denoted "h" in ng (m)
    real, intent(in)  ::  dtilde  ! normalized mud depth = mud depth / sbltm,
    ! delta is the sblt= sqrt(2*visc/sigma)
    real, intent(in)  ::  zeta    ! this is zeta as used in ng pg. 238. it is
    ! the ratio of stokes' boundary layer
    ! thicknesses, or sbltm/delta_w
    real, intent(in)  ::  gamma   ! this is the gamma used in ng pg. 238.
    ! this is density(water)/density(mud)
    real, intent(in)  ::  sbltm   ! sbltm is what you get if you calculate
    ! sblt using the viscosity of the mud,
    ! sbltm=sqrt(2*visc_m/sigma)
    ! .....also delta_m
    real, intent(in)  :: wk       ! unmuddy wavenumber
    ! output variables :
    real, intent(out)  :: wkdr    ! muddy wavenumber
    real, intent(out)  :: diss    ! dissipation rate
    ! local variables :
    real    :: b1  !  an ng coefficient
    real    :: b2  !  an ng coefficient
    real    :: b3  !  an ng coefficient
    real    :: br  !  an ng coefficient
    real    :: bi  !  an ng coefficient
    real    :: brp !  an ng coefficient
    real    :: bip !  an ng coefficient
    real    :: dm !  mud depth, added june 2 2006
    dm=dtilde*sbltm !  dtilde=dm/sbltm
    !   now calculate ng's b coefficients : see ng pg 238
    b1=gamma*(-2.0*gamma**2+2.0*gamma-1.-zeta**2)*sinh(dtilde)*   &
         cosh(dtilde)-gamma**2*zeta*((cosh(dtilde))**2+             &
         (sinh(dtilde))**2)-(gamma-1.)**2*zeta*((cosh(dtilde))**2   &
         *(cos(dtilde))**2+(sinh(dtilde))**2*(sin(dtilde))**2)-2.0  &
         *gamma*(1.-gamma)*(zeta*cosh(dtilde)+gamma*sinh(dtilde))   &
         *cos(dtilde)
    b2=gamma*(-2.0*gamma**2+2.0*gamma-1.+zeta**2)*sin(dtilde)*    &
         cos(dtilde) -2.0*gamma*(1.-gamma)*(zeta*sinh(dtilde)+gamma &
         *cosh(dtilde))*sin(dtilde)
    b3=(zeta*cosh(dtilde)+gamma*sinh(dtilde))**2*(cos(dtilde))**2 &
         +(zeta*sinh(dtilde)+gamma*cosh(dtilde))**2*(sin(dtilde))**2
    br=wk*sbltm*(b1-b2)/(2.0*b3)+gamma*wk*dm
    bi=wk*sbltm*(b1+b2)/(2.0*b3)
    brp=b1/b3  ! "b_r prime"
    bip=b2/b3  ! "b_i prime"
    !  now calculate dissipation rate and wavenumber
    diss=-sbltm*(brp+bip)*wk**2/(sinh(2.0*wk*h_wdepth)+2.0*wk*h_wdepth)
    wkdr=wk-br*wk/(sinh(wk*h_wdepth)*cosh(wk*h_wdepth)+wk*h_wdepth)
    return
  end subroutine ng
  !/ ------------------------------------------------------------------- /
  subroutine calc_nd(kwave,h_wdepth,snd2,nd)
    !/ ------------------------------------------------------------------- /
    implicit none
    real, intent(in)  ::  kwave
    real, intent(in)  ::  h_wdepth
    real, intent(in)  ::  snd2
    real, intent(out) ::  nd
    real    :: fac1       ! local
    real    :: fac2       ! local
    real    :: fac3       ! local
    real    :: knd        ! local
    knd   = kwave*h_wdepth
    fac1  = 2.*knd/sinh(2.*knd)
    fac2  = snd2/knd
    fac3  = 2.*fac2/(1.+fac2*fac2)
    nd= fac1*(0.5/h_wdepth - kwave/fac3)
  end subroutine calc_nd
  !/ ------------------------------------------------------------------- /
  !/
end module w3sbt9md
