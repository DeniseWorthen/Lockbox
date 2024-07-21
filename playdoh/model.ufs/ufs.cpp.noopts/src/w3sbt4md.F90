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
module w3sbt4md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |    f. ardhuin and j. lepesqueur   |
  !/                  |                        fortran 90 |
  !/                  | last update :         14-mar-2012 |
  !/                  +-----------------------------------+
  !/
  !/    20-dec-2004 : origination.                        ( version 3.06 )
  !/    23-jun-2006 : formatted for submitting code for   ( version 3.09 )
  !/                  inclusion in wavewatch iii.
  !/    29-may-2009 : preparing distribution version.     ( version 3.14 )
  !/    14-mar-2012 : preparing distribution version.     ( version 4.05 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     showex bottom friction source term (ardhuin et al. 2003),
  !     using a subgrid depth parameterization based on tolman (ce 1995).
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
  !      w3sbt4    subr. public   showex bottom friction (movable bed)
  !      insbt4    subr. public   corresponding initialization routine.
  !      tabu_erf  subr. public   tabulation of erf function
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
  !     wavewatch iii is designed as a highly plug-compatible code.
  !     source term modules can be included as self-contained modules,
  !     with limited changes needed to the interface of routine calls
  !     in w3srce, and in the point postprocessing programs only.
  !     codes submitted for inclusion in wavewatch iii should be
  !     self-contained in the way described below, and might be
  !     provided with distributions fully integrated in the data
  !     structure, or as an optional version of this module to be
  !     included by the user.
  !
  !     rules for preparing a module to be included in or distributed
  !     with wavewatch iii :
  !
  !      - fully document the code following the outline given in this
  !        file, and according to all other wavewatch iii routines.
  !      - provide a file with necessary modifications to w3srce and
  !        all other routines that require modification.
  !      - provide a test case with expected results.
  !      - it is strongly recommended that the programming style used
  !        in wavewatch iii is followed, in particular
  !          a) for readability, write as if in fixed fortran format
  !             regarding column use, even though all files are f90
  !             free format.
  !          b) i prefer upper case programming for permanent code,
  !             as i use lower case in debugging and temporary code.
  !
  !     this module needs to be self-contained in the following way.
  !
  !      a) all saved variables connected with this source term need
  !         to be declared in the module header. upon acceptance as
  !         permanent code, they will be converted to the wavewatch iii
  !         dynamic data structure.
  !      b) provide a separate computation and initialization routine.
  !         in the submission, the initialization should be called
  !         from the computation routine upon the first call to the
  !         routine. upon acceptance as permanent code, the
  !         initialization routine will be moved to a more appropriate
  !         location in the code (i.e., being absorbed in ww3_grid or
  !         being moved to w3iogr).
  !
  !     see notes in the file below where to add these elements.
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
  !
  ! parameters for erf function
  !
  integer, parameter      :: sizeerftable=300
  real                    :: erftable(0:sizeerftable)
  real                    :: delxerf
  real,    parameter      :: xerfmax =  4. ! number of stdev
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine insbt4
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                         shom      |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         14-mar-2012 |
    !/                  +-----------------------------------+
    !/
    !/    14-mar-2012 : origination.                        ( version 4.05 )
    !
    !  1. purpose :
    !
    !     initialization for bottom friction source term routine.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      w3sbt4    subr. w3src3md corresponding source term.
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
    !
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !      none
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  .... ----------------------------------------------------------- *
    !
    call tabu_erf   !tabulates erf function
    !/
    !/ end of insbt4 ----------------------------------------------------- /
    !/
  end subroutine insbt4
  ! ----------------------------------------------------------------------
  subroutine tabu_erf
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |        j. lepesqueur              |
    !/                  |                        fortran 90 |
    !/                  | last update :         14-mar-2012 |
    !/                  +-----------------------------------+
    !/
    !/    14-mar-2012 : origination.                        ( version 3.13 )
    !/
    !  1. purpose :
    !     tabulation of erf function, which is used in bottom friction subgrid modelling
    !
    !     initialization for source term routine.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
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
    !      w3sin3    subr. w3src3md corresponding source term.
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
    implicit none
    integer :: i
    real :: x,y
    delxerf   = (2*xerfmax)/real(sizeerftable)
    do i=0,sizeerftable
      x=-1.*xerfmax+i*delxerf
      if(x.lt.0.)then
        y=2**(1/2)*(1-abs(erf(x)))/2
      else
        y=2**(1/2)*(1+erf(x))/2
      end if
      erftable(i)=y
    end do
    return
    !/ ------------------------------------------------------------------- /
  end subroutine tabu_erf
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine w3sbt4 (a, cg, wn, depth, d50, psic, taubbl, bedform, s, d, ix, iy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  !            j. lepesqueur          !
    !/                  |                        fortran 90 |
    !/                  | last update :         15-mar-2012 |
    !/                  +-----------------------------------+
    !/
    !/    23-jun-2011 : origination.                        ( version 4.04 )
    !/    04-jul-2011 : adding momentum flux taubbl         ( version 4.05 )
    !/    15-mar-2012 : adding subgrid treatment for depth  ( version 4.05 )
    !/
    !  1. purpose :
    !
    !     computes the showex bottom friction with movable bed effects
    !
    !  2. method :
    !     uses a gaussian distribution for friction factors, and estimates
    !     the contribution of rippled and non-rippled fractions based on
    !     the bayesian approach of tolman (1995).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum.
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       depth   real  i   water depth.
    !       d50     real  i   median grain size.
    !       psic    real  i   critical shields parameter
    !       beforms real i/o  ripple parameters (roughness and wavelength).
    !       taubbl  real  o   components of stress leaking to the bottom.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative.             *)
    !       ix,iy   int. i   spatial grid indices
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
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    use w3gdatmd, only: nk, nth, nspec, sig, dden,  &
         sbtcx, ecos, esin, dth
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    logical, save           :: first = .true.
    real, intent(in)        :: cg(nk), wn(nk), depth, a(nspec), d50
    real, intent(in)        :: psic
    integer, intent(in)     :: ix, iy
    real, intent(out)       :: s(nspec), d(nspec), taubbl(2)
    real, intent(inout)     :: bedform(3)
    real                    :: cbeta(nk)
    real :: uorb2,uorb,aorb, ebx, eby, ax, ay, lx, ly
    real :: const2, temp2
    real :: fw, ksubn, ksubs, ksubr, minadim
    real :: shields(3), psi, deli1, deli2, eb, xi, varu, dd50
    integer :: ik, ith, is, ind, inde, isub
    real :: krr, dsub
    real dsum(nk)
    ! these are the 3-point gauss-hermitte quadrature coefficients
    real, parameter :: wsub(3) = (/ 0.1666667,   0.1666666  , 0.6666667/)
    real, parameter :: xsub(3) = (/ -0.001,  0.001 , 0. /)
    real :: proba1, proba2, psix, psixt, psin2, dpsi , factor
    real :: background
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    if ( first ) then
      call insbt4
      first  = .false.
    end if
    !
    ! 1.  min / max settings for grain size d50---------------------------- *
    !
    dd50=max(d50,1e-5)
    dd50=min(dd50,1.)
    !
    ! 1.1 set background roughness when ripples are not active
    !
    background=max(sbtcx(6),sbtcx(7)*dd50)
    !
    ! 2. subgrid loop
    !
    dsum(:)=0.
    taubbl(:)=0.
    !
    do isub=1,3
      !
      ! 2.a  computes bulk parameters : e, uorb, aorb------------------------- *
      !
      dsub=depth*(1.+xsub(isub))
      uorb=0.
      aorb=0.
      ax  =0.
      ay  =0.
      do ik=1, nk
        if ( wn(ik)*dsub .lt. 6. ) then
          eb  = 0.
          ebx = 0.
          eby = 0.
          do ith=1, nth
            is=ith+(ik-1)*nth
            eb  = eb  + a(is)
            ebx = ebx +a(is)*ecos(ith)
            eby = eby +a(is)*esin(ith)
          end do
          !
          ! u_bot=sigma * zeta / sinh(kd)  and cbeta = 0.5*sigma^2 /(g*sinh^(kd))
          ! therefore variance(u_bot)= variance(elevation)*2*cbeta/d
          !
          !            cbeta(ik) = max(0., (cg(ik)*wn(ik)/sig(ik)-0.5) )/dsub
          cbeta(ik) = 0.5*sig(ik)**2 /(grav*(sinh(wn(ik)*dsub))**2)
          !  n.b.:  could also include shoaling effect on eb ...
          factor= (dden(ik) / cg(ik))*2*cbeta(ik)*grav
          varu= eb * factor
          uorb = uorb + varu
          aorb = aorb + varu/(sig(ik)**2)
          ax   = ax   + (ebx * factor)
          ay   = ay   + (eby * factor)
        else
          cbeta(ik) = 0.
        end if
      end do
      !
      ! computes rms orbital amplitudes
      !
      uorb2 = 2*uorb
      uorb = sqrt(max(1.0e-7,uorb2))
      aorb = sqrt(max(1.0e-7,2*aorb))
      !
      ! computes potential ripple wavelength, 1.7 = 2 * sqrt(2) * 0.6
      ! based on ardhuin et al. (jgr 2002): lambda = 0.6 * d_1/3
      !
      lx = aorb*1.7*ax/sqrt(ax**2+ay**2+1e-12)
      ly = aorb*1.7*ay/sqrt(ax**2+ay**2+1e-12)
      !
      ! 2.b first use of fwtable to get skin roughness and estimate shields parameter
      !
      xi=max((alog10(max(aorb/dd50,0.3))-abmin)/delab,1.)
      ind  = min (sizefwtable-1, int(xi))
      deli1= min (1. ,xi-float(ind))
      deli2= 1. - deli1
      fw =fwtable(ind)*deli2+fwtable(ind+1)*deli1
      psi=fw*uorb2/(2.*grav*(sed_sg-1)*dd50)
      !
      ! normalized shields parameter
      !
      shields(isub)=psi/psic
      !
    end do ! end of loop on isub
    dpsi=(shields(2)-shields(1))/(xsub(2)-xsub(1))*sbtcx(5)
    !
    ! tests if the variation in psi is large enough to use subgrid
    !
    if (abs(dpsi).lt.0.0001*shields(3).or.abs(dpsi).lt.1.e-8) then
      !
      ! no subgrid in this case
      !
      if(shields(3).gt.sbtcx(3)) then
        !  ripple roughness, see ardhuin et al. (2003)
        ksubr=aorb*sbtcx(1)*shields(3)**sbtcx(2)
        !  sheet flow roughness, see wilson (1989)
        ksubs=aorb*0.0655*(uorb2/((sed_sg-1)*grav*aorb))**1.4
        ksubn = ksubr + ksubs
        bedform(2)=lx
        bedform(3)=ly
      else
        !  relict roughness, see ardhuin et al. (2003)
        ksubn=max(background,aorb*sbtcx(4))
        bedform(2)=-lx
        bedform(3)=-ly
      end if
      bedform(1)=ksubn
    else
      !
      ! subgrid in this case
      !
      psix=(sbtcx(3)-shields(3))/dpsi
      psixt=max((psix + xerfmax)/delxerf,0.)
      inde  = max(min (sizeerftable-1, int(psixt)),0)
      deli1  = min (1. ,psixt-float(inde))
      deli2  = 1. - deli1
      proba2=max(min(erftable(inde)*deli2+erftable(inde+1)*deli1,1.),0.)
      proba1 = 1. - proba2
      ! mean psi with ripples (tolman 1995, eq. xx)
      psin2=max(shields(3)+exp(-(0.5*psix**2))/sqrt(tpi)*dpsi/(proba2+0.0001),sbtcx(3))
      ! sum of relict, ripple and sheet flow roughnesses
      ksubn = proba1*max(background,aorb*sbtcx(4))       &
           +proba2*aorb*(sbtcx(1)*psin2**sbtcx(2)+                   &
           0.0655*(uorb2/((sed_sg-1)*grav*aorb))**1.4)
      !
      if (proba2.gt.0.5) then
        bedform(2)=lx
        bedform(3)=ly
      else
        bedform(2)=-lx
        bedform(3)=-ly
      end if
      !
    end if
    bedform(1)=ksubn
    !
    ! 2.c second use of fwtable to get fw from the full roughness
    !
    xi=max((alog10(max(aorb/ksubn,0.3))-abmin)/delab,1.)
    ind  = min (sizefwtable-1, int(xi))
    deli1= min (1. ,xi-float(ind))
    deli2= 1. - deli1
    fw =fwtable(ind)*deli2+fwtable(ind+1)*deli1
    !
    ! 5. fills output arrays and estimates the energy and momentum loss
    !
    do ik=1, nk
      const2=dden(ik)/cg(ik) &         !jacobian to get energy in band
           *grav/(sig(ik)/wn(ik))    ! coefficient to get momentum
      dsum(ik)=-fw*uorb*cbeta(ik)      !*wsub(isub)
      do ith=1,nth
        is=ith+(ik-1)*nth
        d(is)=dsum(ik)
        temp2=const2*d(is)*a(is)
        taubbl(1) = taubbl(1) - temp2*ecos(is)
        taubbl(2) = taubbl(2) - temp2*esin(is)
        s(is)=d(is)*a(is)
      end do
    end do
    !
    return
    !/
    !/ end of w3sbt4 ----------------------------------------------------- /
    !/
  end subroutine w3sbt4
  !/ ------------------------------------------------------------------- /
end module w3sbt4md
