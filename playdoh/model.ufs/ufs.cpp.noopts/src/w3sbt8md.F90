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
module w3sbt8md
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
  !     dalrymple and liu (1978) "thin model".
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
  !      w3sbt8    subr. public   fluid mud dissipation (dalrymple & liu, 1978)
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !      strace    subr. w3servmd subroutine tracing.
  !      csinh     subr.   ??     complex sinh function
  !      ccosh     subr.   ??     complex cosh function
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !     historical information:
  !        this started as a matlab script provided to erick rogers by tony
  !        dalrympyle sep 2006. erick rogers converted to fortran and put
  !        it into the swan model may 2007. mark orzech adapted the code for
  !        ww3 and added it to nrl code repository july-dec 2011.
  !        erick rogers brought it over to the ncep repository may 2013
  !        and has been updating and maintaining it there.
  !
  !     reference: dalrymple, r.a., liu,p.l.-f.,1978:
  !                waves over soft muds :a 2-layer fluid model.
  !                journal of physical oceanography, 8, 1121–1131.
  !
  !  6. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3sbt8(ac,h_wdepth,s,d,ix,iy)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa      |
    !/                  |           m. orzech     nrl       |
    !/                  |           w. e. rogers  nrl       |
    !/                  |                        fortran 90 |
    !/                  | last update :         21-nov-2013 |
    !/                  +-----------------------------------+
    !/
    !/    20-dec-2004 : origination.                        ( version 3.06
    !/    23-jun-2006 : formatted for submitting code for   ( version 3.09 )
    !/                  inclusion in wavewatch iii.
    !/
    !  1. purpose :
    !
    !     compute dissipation by viscous fluid mud using dalrymple and liu (1978)
    !     "thin model" (adapted from erick rogers code by mark orzech, nrl).
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ac        r.a.  i   action density spectrum (1-d)
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
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use w3dispmd, only: wavnu1
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)    :: h_wdepth ! water depth
    real, intent(in)    :: ac(nspec) ! action density
    integer, intent(in) :: ix, iy
    real, intent(out)   :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    complex    :: k
    complex    :: shh
    complex    :: chh
    complex    :: shd
    complex    :: chd
    complex    :: lam1
    complex    :: lam2
    complex    :: chlam2
    complex    :: shlam2
    complex    :: a1
    complex    :: a2
    complex    :: a3
    complex    :: b1
    complex    :: b2
    complex    :: b3
    complex    :: b4
    complex    :: c0
    complex    :: testalf1
    complex    :: testalf2
    complex    :: alf1
    complex    :: alf2
    complex    :: psi1
    complex    :: psi2
    complex    :: m1
    complex    :: m0
    complex    :: c(4,3)
    complex    :: c41a
    complex    :: c42a
    complex    :: c43a
    complex    :: b(4)
    complex    :: cd
    complex    :: hh
    complex    :: dd
    complex    :: gg
    complex    :: fm1
    complex    :: km1
    complex    :: fp
    complex    :: i
    complex    :: f
    complex    :: kmud
    real    :: bet0
    real    :: kinvisw
    real    :: rhow
    real    :: exph
    real    :: a
    real    :: k_unmud
    real    :: sigma       ! radian frequency (rad)
    real    :: smudwd(nk)  ! dissipation due to mud
    real    :: kmimag(nk)  ! imag part of kmud
    real    :: kd
    real    :: kdcutoff
    real    :: cwave
    real    :: ztmp
    real    :: nwave_mud
    real    :: cg_mud
    real    :: kcheck
    real    :: kthreshold
    real    :: rhom
    real    :: kinvism
    real    :: thickm
    real    :: cg_unmud
    integer :: icount
    integer :: ik
    integer :: is
    parameter (i=(0.,1.))
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    !     dalrymple and liu, waves over soft muds:  1978.
    !     thin layer solution.
    !     matlab code provided by tony dalrymple
    !     converted to fortran by erick rogers
    ! initialize properties from mud fields if available
    if (inflags1(-2))then
      rhom = mudd(ix,iy)
    else
      write(ndse,*)'rhom not set'
      call extcde ( 1 )
    endif
    if (inflags1(-1)) then
      thickm = mudt(ix,iy)
    else
      write(ndse,*)'thickm not set'
      call extcde ( 2 )
    endif
    if (inflags1(0)) then
      kinvism = mudv(ix,iy)
    else
      write(ndse,*)'kinvism not set'
      call extcde ( 3 )
    endif
    rhow=dwat        ! density of seawater
    kinvisw=nu_water
    kdcutoff = 10.0
    kthreshold=1.0e-9
    a=1.0
    ! initialize matrix diagonal contributions
    d = 0.0
    s = 0.0
    if ( thickm>0.0 .and. rhom>0.0 .and. kinvism>0.0 ) then
      smudwd = 0.0
      ! *** loop over frequencies
      do ik = 1,nk
        sigma = sig(ik)
        !     un-muddy wave number, to start things off
        call wavnu1(sigma,h_wdepth,k_unmud,cg_unmud)
        k=k_unmud
        !     start iterative loop
        do icount=1,20  ! *** may need more ***
          call csinh(k*h_wdepth,shh)
          call ccosh(k*h_wdepth,chh)
          call csinh(k*thickm,shd)
          call ccosh(k*thickm,chd)
          !   define lambdas
          lam1=sqrt(k*k-i*sigma/kinvisw)
          lam2=sqrt(k*k-i*sigma/kinvism)
          !   define hyperbolics on lamda2, lamda1
          call ccosh(lam2*thickm,chlam2)
          call csinh(lam2*thickm,shlam2)
          !   define exp decay
          exph=exp(-lam1*h_wdepth)
          !   define a1, a2, a3
          a1=-lam2*shd/k+shlam2
          a2=-chd+chlam2
          a3=shd-lam2*shlam2/k
          !   define b1, b2, b3, b4
          b1=lam1*shh/k-chh
          b2=lam2*a2*shh/k+a1*chh
          b3=-a3*shh+a2*chh
          b4=lam1*shh/k+chh
          ! define c0
          c0=b4*exph-(lam1*lam1+k*k)/(2*k*k)
          ! define beta0
          bet0=-exph/c0
          ! define alfa1, alfa2
          testalf1=-rhom*kinvism*(lam2*lam2+k*k)*(-lam2/k)*chd/k  &
               -2*rhom*kinvism*lam2*chlam2-(rhom-rhow)*grav*(i*(a1)/sigma)
          testalf2=-rhom*kinvism*(lam2*lam2+k*k)*(-1)*shd/k  &
               -2*rhom*kinvism*lam2*shlam2-(rhom-rhow)*grav*(i*(a2)/sigma)
          alf1=-testalf1
          alf2=-testalf2
          ! define psi1, psi2
          psi1=2*k*(-lam2/k)*shd+(lam2*lam2+k*k)*shlam2/k
          psi2=2*k*(-1)*chd+(lam2*lam2+k*k)*chlam2/k
          ! define m1, mo
          m1=i*rhow*sigma/k-2*rhow*kinvisw*k
          m0=b1+(lam1*lam1+k*k)*exph/(k*k)
          ! matrix coefficients (eq. 22)
          c(1,1)=lam1*(bet0*m0+1)*shh/k+(bet0*m0-1)*chh+m0/c0+exph
          c(1,2)=(lam1*bet0*b2+lam2*a2)*shh/k+(bet0*b2+a1)*chh+b2/c0
          c(1,3)=(lam1*bet0*b3/k-a3)*shh+(bet0*b3+a2)*chh+b3/c0
          ! matrix coefficients (eq. 23)
          c(2,1)=lam1*(bet0*m0+1)*m1*chh/k+(bet0*m0-1)*m1*shh  &
               -2*rhow*kinvisw*lam1*m0/c0+2*rhow*kinvisw*lam1*exph
          c(2,2)=(lam1*bet0*b2+lam2*a2)*m1*chh/k+(bet0*b2+a1)*m1*shh &
               -2*rhow*kinvisw*lam1*b2/c0
          c(2,3)=(lam1*bet0*b3/k-a3)*m1*chh+(bet0*b3+a2)*m1*shh  &
               -2*rhow*kinvisw*lam1*b3/c0
          ! matrix coefficients (eq. 21)
          c(3,1)=2*k*rhow*kinvisw*(bet0*m0-1)+rhow*kinvisw  &
               *(lam1*lam1+k*k)*(1-bet0*m0)/k
          c(3,2)=2*k*rhow*kinvisw*(bet0*b2+a1)-rhow*kinvisw  &
               *(lam1*lam1+k*k)*bet0*b2/k-i*rhom*kinvism*psi1
          c(3,3)=2*k*rhow*kinvisw*(bet0*b3+a2)-rhow*kinvisw  &
               *(lam1*lam1+k*k)*bet0*b3/k-i*rhom*kinvism*psi2
          ! matrix coefficients (eq.19)
          c41a=lam1*m1*(bet0*m0+1)/k+2*rhow*kinvisw*lam1  &
               +2*rhow*kinvisw*lam1*bet0*m0
          c42a=m1*(lam1*bet0*b2+lam2*a2)/k  &
               +2*rhow*kinvisw*lam1*bet0*b2+alf1
          c43a=m1*(lam1*bet0*b3/k-a3)+2*rhow*kinvisw*lam1*bet0*b3+alf2
          ! method 1
          c(4,1)=c41a*c(3,1)/c41a-c(3,1)
          c(4,2)=c42a*c(3,1)/c41a-c(3,2)
          c(4,3)=c43a*c(3,1)/c41a-c(3,3)
          ! force terms......righthand side
          b(1)=-i*sigma*a
          b(2)=rhow*grav*a
          b(3)=0
          b(4)=0
          !  coefficients
          cd=-(c(3,3)-c(3,2)*c(4,3)/c(4,2))/c(3,1)
          hh=b(2)/(c(2,1)*cd -c(2,2)*(c(4,3)/c(4,2))+c(2,3))
          dd=cd*hh
          gg=-c(4,3)*hh/c(4,2)
          !  find k
          f=c(1,1)*dd+c(1,2)*gg+c(1,3)*hh-b(1)
          if(icount.eq.1)then
            fm1=f
            km1=k
            k=k*(.995)+.001*i
            kcheck=100.0
          else
            kcheck=abs(imag(k)-imag(km1))
            if((f.eq.fm1).or.(k.eq.km1).or.(kcheck<kthreshold))then
              ! notes: i have noticed that if iterations are not stopped early enough, nans result
              kmud=k
              exit
            end if
            fp=(f-fm1)/(k-km1)
            km1=k
            fm1=f
            k=k-0.8*f/fp
          endif
        end do
        kmud=k
        ! kd calc: not that important: just used to determine whether we make the mud calculation
        kd = real(kmud) * h_wdepth
        if ( kd .lt. kdcutoff ) then
          ! notes: it would be better to have cg_mud stored for each freq.
          cwave=sigma/real(kmud)
          ztmp=2.0*real(kmud)*h_wdepth
          if(ztmp.lt.70)then
            ztmp=sinh(ztmp)
          else
            ztmp=1.0e+30
          endif
          nwave_mud=0.5*(1.0+2.0*real(kmud)*h_wdepth/ztmp)
          cg_mud=nwave_mud*cwave
          !        --- compute fluid mud-induced wave dissipation
          smudwd(ik)=2.0*imag(kmud)*cg_mud
          !        --- store imaginary part of kmud
          kmimag(ik)=imag(kmud)
        end if !     if ( kd .lt. kdcutoff ) then
      end do ! ik
      !     *** store the results in the diagonal array d ***
      do is = 1,nspec
        d(is) = -smudwd(mapwn(is))
      end do
    end if !   if ( thickm>0.0 & rhom>0.0 & kinvism>0.0 ) then
    s = d * ac
    return
    !/
    !/ end of w3sbt8 ----------------------------------------------------- /
    !/
  end subroutine w3sbt8
  !/ ------------------------------------------------------------------- /
  subroutine csinh(c,cs)
    complex, intent(in) ::  c
    complex, intent(out) :: cs
    x = real(c)
    y = aimag(c)
    cs = cmplx(sinh(x) * cos(y), sin(y) * cosh(x))
    return
  end subroutine csinh
  !/ ------------------------------------------------------------------- /
  subroutine ccosh(c,cc)
    complex, intent(in) ::  c
    complex, intent(out) :: cc
    x = real(c)
    y = aimag(c)
    cc = cmplx(cosh(x) * cos(y), sin(y) * sinh(x))
    return
  end subroutine ccosh
  !/ ------------------------------------------------------------------- /
  !/
end module w3sbt8md
