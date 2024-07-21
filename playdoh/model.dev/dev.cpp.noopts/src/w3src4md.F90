!> @file
!> @brief the 'shom/ifremer' source terms based on p.a.e.m.
!>
!> @author f. ardhuin
!> @date   13-nov-2013
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
!> @brief the 'shom/ifremer' source terms based on p.a.e.m.
!>
!> @details janssen's wind input
!>  and dissipation functions by ardhuin et al. (2009,2010)
!>  and filipot & ardhuin (2010)
!>  the wind input is converted from the original
!>  wam codes, courtesy of p.a.e.m. janssen and j. bidlot
!>
!> @author f. ardhuin
!> @date   13-nov-2013
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3src4md
  !/
  public
  !/
  !/ public variables
  !/
  !air kinematic viscosity (used in wam)
  integer, parameter      :: itaumax=200,jumax=200
  integer, parameter      :: iustar=100,ialpha=200, ilevtail=50
  ! tables for total stress and h.f. stress as a function of 2 or 3 variables
  real, allocatable       :: taut(:,:),tauhft(:,:),tauhft2(:,:,:)
  real                    :: delust, delalp,deltauw, delu
  ! table for swell damping
  real                    :: deltail
  real,    parameter      :: umax    = 50.
  real,    parameter      :: tauwmax = 2.2361 !sqrt(5.)
  integer                 :: dikcumul
  !  size of wave height table for integrating the pdf of wave heights
  integer,    parameter      :: nkhi=100, fac_kd2=1000
  real,    parameter      :: fac_kd1=1.01, khsmax=2., khmax=2.
  real,    parameter      ::kdmax=200000.
  !/
contains
  !/ ------------------------------------------------------------------- /
!>
!> @brief calculate mean wave parameters for the use in the source term
!>  routines.
!>
!> @param[in]    a         action density spectrum.
!> @param[in]    cg        group velocities.
!> @param[in]    wn        wavenumbers.
!> @param[out]   emean     energy.
!> @param[out]   fmean     mean frequency for determination of tail.
!> @param[out]   fmean1    mean frequency (fm0,-1) used for reflection.
!> @param[out]   wnmean    mean wavenumber.
!> @param[out]   amax      maximum of action spectrum.
!> @param[in]    u         wind speed.
!> @param[in]    udir      wind direction.
!> @param[in]    taua      atm total stress.
!> @param[in]    tauadir   atm total stress direction.
!> @param[in]    dair      air density.
!> @param[inout] ustar     friction velocity.
!> @param[inout] usdir     wind stress direction.
!> @param[in]    tauwx     component of wave-supported stress.
!> @param[in]    tauwy     component of wave-supported stress.
!> @param[out]   cd        drag coefficient at wind level zwnd.
!> @param[out]   z0        corresponding z0.
!> @param[out]   charn     corresponding charnock coefficient.
!> @param[in]    llws      wind sea true/false array for each component.
!> @param[out]   fmeanws   mean frequency of wind sea, used for tail.
!> @param[out]   dlwmean   mean long wave direction  (l. romero 2019).
!>
!> @author f. ardhuin
!> @author h. l. tolman
!> @date   22-feb-2020
!>
  subroutine w3spr4 (a, cg, wn, emean, fmean, fmean1, wnmean,     &
       amax, u, udir,                                    &
       ustar, usdir,                                     &
       tauwx, tauwy, cd, z0, charn, llws, fmeanws, dlwmean)
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only: iaproc
    use constants, only: tpiinv, grav, nu_air
    use w3gdatmd, only: nk, nth, nspec, sig, dth, dden, wwnmeanp, &
         wwnmeanptail, fte, ftf, sstxftf, sstxftwn,&
         sstxftftail, sswellf, esin, ecos, aaircmin, &
         aairgb, aalpha, zzwnd, ssdsc
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nth,nk), cg(nk), wn(nk), u, udir
    real, intent(in)        :: tauwx, tauwy
    logical, intent(in)     :: llws(nspec)
    real, intent(inout)     :: ustar ,usdir
    real, intent(out)       :: emean, fmean, fmean1, wnmean, amax,  &
         cd, z0, charn, fmeanws, dlwmean
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, ik, ith
    real                    :: tauw, eband, emeanws,unz,            &
         eb(nk),eb2(nk),elcs, elsn, sigfac
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    unz    = max ( 0.01 , u )
    ustar  = max ( 0.0001 , ustar )
    !
    emean  = 0.
    emeanws= 0.
    fmeanws= 0.
    fmean  = 0.
    fmean1 = 0.
    wnmean = 0.
    amax   = 0.
    dlwmean =0.
    elcs =0.
    elsn =0.
    !
    ! 1.  integral over directions and maximum --------------------------- *
    !
    do ik=1, nk
      eb(ik)  = 0.
      eb2(ik) = 0.
      sigfac=sig(ik)**ssdsc(12) * dden(ik) / cg(ik)
      do ith=1, nth
        is=ith+(ik-1)*nth
        eb(ik) = eb(ik) + a(ith,ik)
        elcs = elcs + a(ith,ik)*ecos(is)*sigfac
        elsn = elsn + a(ith,ik)*esin(is)*sigfac
        if (llws(is)) eb2(ik) = eb2(ik) + a(ith,ik)
        amax   = max ( amax , a(ith,ik) )
      end do
    end do
    !
    dlwmean=atan2(elsn,elcs)
    !
    ! 2.  integrate over directions -------------------------------------- *
    !
    do ik=1, nk
      eb(ik)   = eb(ik) * dden(ik) / cg(ik)
      eb2(ik)   = eb2(ik) * dden(ik) / cg(ik)
      emean    = emean  + eb(ik)
      fmean    = fmean  + eb(ik) /sig(ik)
      fmean1   = fmean1 + eb(ik) *(sig(ik)**(2.*wwnmeanptail))
      wnmean   = wnmean + eb(ik) *(wn(ik)**wwnmeanp)
      emeanws  = emeanws+ eb2(ik)
      fmeanws  = fmeanws+ eb2(ik)*(sig(ik)**(2.*wwnmeanptail))
    end do
    !
    ! 3.  add tail beyond discrete spectrum and get mean pars ------------ *
    !     ( dth * sig absorbed in ftxx )
    !
    eband  = eb(nk) / dden(nk)
    emean  = emean  + eband * fte
    fmean  = fmean  + eband * ftf
    fmean1 = fmean1 + eband * sstxftftail
    wnmean = wnmean + eband * sstxftwn
    eband  = eb2(nk) / dden(nk)
    emeanws = emeanws + eband * fte
    fmeanws = fmeanws + eband * sstxftftail
    !
    ! 4.  final processing
    !
    fmean  = tpiinv * emean / max ( 1.e-7 , fmean )
    if (fmean1.lt.1.e-7) then
      fmean1=tpiinv * sig(nk)
    else
      fmean1  = tpiinv *( max ( 1.e-7 , fmean1 )                       &
           / max ( 1.e-7 , emean ))**(1/(2.*wwnmeanptail))
    endif
    wnmean = ( max ( 1.e-7 , wnmean )                              &
         / max ( 1.e-7 , emean ) )**(1/wwnmeanp)
    if (fmeanws.lt.1.e-7.or.emeanws.lt.1.e-7) then
      fmeanws=tpiinv * sig(nk)
    else
      fmeanws  = tpiinv *( max ( 1.e-7 , fmeanws )                       &
           / max ( 1.e-7 , emeanws ))**(1/(2.*wwnmeanptail))
    end if
    !
    ! 5.  cd and z0 ----------------------------------------------- *
    !
    tauw = sqrt(tauwx**2+tauwy**2)
    !
    call calc_ustar(u,tauw,ustar,z0,charn)
    unz    = max ( 0.01 , u )
    cd     = (ustar/unz)**2
    usdir = udir
    !
    ! 6.  final test output ---------------------------------------------- *
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3spr4 ----------------------------------------------------- /
    !/
  end subroutine w3spr4
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate diagonal and input source term for wam4+ approach.
  !>
  !> @verbatim
  !>       wam-4     : janssen et al.
  !>       wam-"4.5" : gustiness effect (cavaleri et al. )
  !>       sat       : high-frequency input reduction for balance with
  !>                   saturation dissipation (ardhuin et al., 2008)
  !>       swell     : negative wind input (ardhuin et al. 2008)
  !> @endverbatim
  !>
  !> @param[in]  a         action density spectrum (1-d).
  !> @param[in]  cg        group speed.
  !> @param[in]  k         wavenumber for entire spectrum.
  !> @param[in]  u         wind speed.
  !> @param[in]  ustar     friction velocity.
  !> @param[in]  drat      air/water density ratio.
  !> @param[in]  as        air-sea temperature difference.
  !> @param[in]  usdir     wind stress direction.
  !> @param[in]  z0        air-sea roughness length.
  !> @param[in]  cd        wind drag coefficient.
  !> @param[out] tauwx     component of the wave-supported stress.
  !> @param[out] tauwy     component of the wave-supported stress.
  !> @param[out] tauwnx    component of the negative wave-supported stress.
  !> @param[out] tauwny    component of the negative wave-supported stress.
  !> @param[out] s         source term (1-d version).
  !> @param[out] d         diagonal term of derivative.
  !> @param[out] llws
  !> @param[in]  ix
  !> @param[in]  iy
  !> @param[in]  brlambda
  !>
  !> @author f. ardhuin
  !> @author h. l. tolman
  !> @date   05-dec-2013
  !>
  subroutine w3sin4 (a, cg, k, u, ustar, drat, as, usdir, z0, cd,    &
       tauwx, tauwy, tauwnx, tauwny, s, d, llws,       &
       ix, iy, brlambda)
    use constants, only: grav,nu_air,kappa,tpi,fwtable,sizefwtable, &
         delab,abmin
    use w3gdatmd, only: nk, nth, nspec, dden, sig, sig2, th,         &
         esin, ecos, ec2, zzwnd, aalpha, bbeta, zzalp,&
         ttauwshelter, sswellf, dden2, dth, ssinthp,  &
         zz0rat, ssinbr, sintailpar
    use w3odatmd, only: iaproc
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), brlambda(nspec)
    real, intent(in)        :: cg(nk), k(nspec),z0,u, cd
    real, intent(in)        :: ustar, usdir, as, drat
    real, intent(out)       :: s(nspec), d(nspec), tauwx, tauwy, tauwnx, tauwny
    logical, intent(out)    :: llws(nspec)
    integer, intent(in)     :: ix, iy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is,ik,ith
    real                    :: facln1, facln2, lambda
    real                    :: cosu, sinu, taux, tauy, usdirp, ustp
    real                    :: taupx, taupy, ust2, tauw, tauwb
    real   , parameter      :: eps1 = 0.00001, eps2 = 0.000001
    real                    :: usigma           !standard deviation of u due to gustiness
    real                    :: ustarsigma       !standard deviation of ustar due to gustiness
    real                    :: cm,ucn,zcn, &
         z0visc, z0noz, eb,  &
         ebx, eby, aorb, aorb1, fw, uorb, th2, &
         re, fu, fud, swellcoefv, swellcoeft
    real                   ::  pturb, pvisc, smooth
    real xi,deli1,deli2
    real xj,delj1,delj2
    real xk,delk1,delk2
    real                    :: const, const0, const2, tau1, tau1nt, zinf, tensk
    real x,zarg,zlog,ust
    real                    :: coswind, xstress, ystress, tauhf
    real temp, temp2
    integer ind,j,i,istab
    real dstab(3,nspec), dvisc, dturb
    real stressstab(3,2),stressstabn(3,2)
    !
    integer, parameter      :: jtot=50
    real   , parameter      :: km=363.,cmm=0.2325  ! k and c at phase speed minimum in rad/m
    real                    :: omegacc, omega, zz0, zx, zbeta, ustr, taur,  &
         const1, levtail0, x0, y, dely, yc, zmu,      &
         levtail, cgtail, alpham, fm, alphat, fmean
    real, allocatable       :: w(:)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  preparations
    !
    !jdm: initializing values to zero, they shouldn't be used unless
    !set in another place, but seems to solve some bugs with certain
    !compilers.
    dstab =0.
    stressstab =0.
    stressstabn =0.
    !
    ! coupling coefficient times density ratio drat
    !
    const1=bbeta/kappa**2  ! needed for the tail
    const0=const1*drat     ! needed for the resolved spectrum
    !
    ! 1.a  estimation of surface roughness parameters
    !
    z0visc = 0.1*nu_air/max(ustar,0.0001)
    z0noz = max(z0visc,zz0rat*z0)
    facln1 = u / log(zzwnd/z0noz)
    facln2 = log(z0noz)
    !
    ! 1.b  estimation of surface orbital velocity and displacement
    !
    uorb=0.
    aorb=0.
    do ik=1, nk
      eb  = 0.
      ebx = 0.
      eby = 0.
      do ith=1, nth
        is=ith+(ik-1)*nth
        eb  = eb  + a(is)
      end do
      !
      !  at this point uorb and aorb are the variances of the orbital velocity and surface elevation
      !
      uorb = uorb + eb *sig(ik)**2 * dden(ik) / cg(ik)
      aorb = aorb + eb             * dden(ik) / cg(ik)  !correct for deep water only
    end do
    !      fmean = sqrt((uorb+1e-6)/(aorb+1e-6))
    uorb  = 2*sqrt(uorb)                  ! significant orbital amplitude
    aorb1 = 2*aorb**(1-0.5*sswellf(6))    ! half the significant wave height ... if swellf(6)=1
    re = 4*uorb*aorb1 / nu_air           ! reynolds number
    !
    ! defines the swell dissipation based on the "reynolds number"
    !
    if (sswellf(4).gt.0) then
      if (sswellf(7).gt.0.) then
        smooth = 0.5*tanh((re-sswellf(4))/sswellf(7))
        pturb=(0.5+smooth)
        pvisc=(0.5-smooth)
      else
        if (re.le.sswellf(4)) then
          pturb =  0.
          pvisc =  1.
        else
          pturb =  1.
          pvisc =  0.
        end if
      end if
    else
      pturb=1.
      pvisc=1.
    end if
    !
    if (sswellf(2).eq.0) then
      fw=max(abs(sswellf(3)),0.)
      fu=0.
      fud=0.
    else
      fu=abs(sswellf(3))
      fud=sswellf(2)
      aorb=2*sqrt(aorb)
      xi=(alog10(max(aorb/z0noz,3.))-abmin)/delab
      ind  = min (sizefwtable-1, int(xi))
      deli1= min (1. ,xi-float(ind))
      deli2= 1. - deli1
      fw =fwtable(ind)*deli2+fwtable(ind+1)*deli1
    end if
    !
    ! 2.  diagonal
    !
    ! here as is the air-sea temperature difference in degrees. expression given by
    ! abdalla & cavaleri, jgr 2002 for usigma. for ustarsigma ... i do not see where
    ! i got it from, maybe just made up from drag law ...
    !
    ust=ustar
    istab=3
      taux = ust**2* cos(usdir)
      tauy = ust**2* sin(usdir)
      !
      ! loop over the resolved part of the spectrum
      !
      stressstab(istab,:)=0.
      stressstabn(istab,:)=0.
      !
      do ik=1, nk
        taupx=taux-abs(ttauwshelter)*stressstab(istab,1)
        taupy=tauy-abs(ttauwshelter)*stressstab(istab,2)
        ! with min and max the bug should disappear.... but where did it come from?
        ustp=min((taupx**2+taupy**2)**0.25,max(ust,0.3))
        usdirp=atan2(taupy,taupx)
        cosu   = cos(usdirp)
        sinu   = sin(usdirp)
        is=1+(ik-1)*nth
        cm=k(is)/sig2(is) !inverse of phase speed
        ucn=ustp*cm+zzalp  !this is the inverse wave age
        ! the stress is the real stress (n/m^2) divided by
        ! rho_a, and thus comparable to ustar**2
        ! it is the integral of rho_w g sin/c /rho_a
        ! (air-> waves momentum flux)
        const2=dden2(is)/cg(ik) &        !jacobian to get energy in band
             *grav/(sig(ik)/k(is)*drat) ! coefficient to get momentum
        const=sig2(is)*const0
        ! cm parameter is 1 / c_phi
        ! z0 corresponds to z0+z1 of the janssen eq. 14
        zcn=alog(k(is)*z0)
        !
        ! precomputes swell factors
        !
        swellcoefv=-sswellf(5)*drat*2*k(is)*sqrt(2*nu_air*sig2(is))
        swellcoeft=-drat*sswellf(1)*16*sig2(is)**2/grav
        !
        do ith=1,nth
          is=ith+(ik-1)*nth
          coswind=(ecos(is)*cosu+esin(is)*sinu)
          if (coswind.gt.0.01) then
            x=coswind*ucn
            ! this zarg term is the argument of the exponential
            ! in janssen 1991 eq. 16.
            zarg=kappa/x
            ! zlog is alog(mu) where mu is defined by janssen 1991 eq. 15
            ! mu=
            zlog=zcn+zarg
            if (zlog.lt.0.) then
              ! the source term sp is beta * omega * x**2
              ! as given by janssen 1991 eq. 19
              ! note that this is slightly diffent from ecwam code cy45r2 where zlog is replaced by ??
              dstab(istab,is) = const*exp(zlog)*zlog**4*ucn*ucn*coswind**ssinthp
              ! below is an example with breaking probability feeding back to the input...
              !dstab(istab,is) = const*exp(zlog)*zlog**4  &
              !                  *ucn*ucn*coswind**ssinthp *(1+brlambda(is)*20*ssinbr)
              llws(is)=.true.
            else
              dstab(istab,is) = 0.
              llws(is)=.false.
            end if
            !
            !  added for consistency with ecwam implsch.f
            !
            if (28.*cm*ustar*coswind.ge.1) then
              llws(is)=.true.
            end if
          else  ! (coswind.le.0.01)
            dstab(istab,is) = 0.
            llws(is)=.false.
          end if
          !
          if ((sswellf(1).ne.0.and.dstab(istab,is).lt.1e-7*sig2(is)) &
               .or.sswellf(3).gt.0) then
            !
            dvisc=swellcoefv
            dturb=swellcoeft*(fw*uorb+(fu+fud*coswind)*ustp)
            !
            dstab(istab,is) = dstab(istab,is) + pturb*dturb +  pvisc*dvisc
          end if
          !
          ! sums up the wave-supported stress
          !
          ! wave direction is "direction to"
          ! therefore there is a plus sign for the stress
          temp2=const2*dstab(istab,is)*a(is)
          if (dstab(istab,is).lt.0) then
            stressstabn(istab,1)=stressstabn(istab,1)+temp2*ecos(is)
            stressstabn(istab,2)=stressstabn(istab,2)+temp2*esin(is)
          else
            stressstab(istab,1)=stressstab(istab,1)+temp2*ecos(is)
            stressstab(istab,2)=stressstab(istab,2)+temp2*esin(is)
          end if
        end do
      end do
      !
      d(:)=dstab(3,:)
      xstress=stressstab (3,1)
      ystress=stressstab (3,2)
      tauwnx =stressstabn(3,1)
      tauwny =stressstabn(3,2)
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    taupx=taux-abs(ttauwshelter)*xstress
    taupy=tauy-abs(ttauwshelter)*ystress
    ustp=(taupx**2+taupy**2)**0.25
    usdirp=atan2(taupy,taupx)
    ust=ustp
    !
    ! computes hf tail
    !
    ! computes the high-frequency contribution
    ! the difference in spectal density (kx,ky) to (f,theta)
    ! is integrated in this modified const0
    const0=dth*sig(nk)**5/((grav**2)*tpi) &
         *tpi*sig(nk) / cg(nk)  !conversion wam (e(f,theta) to ww3 a(k,theta)
    temp=0.
    do ith=1,nth
      is=ith+(nk-1)*nth
      coswind=(ecos(is)*cosu+esin(is)*sinu)
      temp=temp+a(is)*(max(coswind,0.))**3
    end do
    !
    levtail0= const0*temp  ! levtail is sum over theta of a(k,theta)*cos^3(theta-wind)*dth*sig^5/(g^2*2pi)*2*pi*sig/cg
                           !  which is the same as sum of e(f,theta)*cos^3(theta-wind)*dth*sig^5/(g^2*2pi)
                           ! reminder:  sum of e(f,theta)*dth*sig^5/(g^2*2pi) is 2*k^3*e(k)
!
! computation of stress supported by tail: uses table if sintailpar(1)=1 , correspoding to sintable = 1
!
    if (sintailpar(1).lt.0.5) then
      allocate(w(jtot))
      w(2:jtot-1)=1.
      w(1)=0.5
      w(jtot)=0.5
      x0 = 0.05
      !
      ustr= ust
      zz0=z0
      omegacc  = max(sig(nk),x0*grav/ust)
      yc       = omegacc*sqrt(zz0/grav)
      ! dely     = max((1.-yc)/real(jtot),0.)
      ! changed integration variable from y to log(y) and to log(k)
      !zinf      = log(yc)
      !dely     = max((1.-zinf)/real(jtot),0.)
      zinf = log(sig(nk)**2/grav)
      dely = (log(tpi/0.005)-zinf)/real(jtot)
      taur=ust**2
      tau1=0.
      ! integration loop over the tail wavenumbers or frequencies ...
      do j=1,jtot
        !y        = yc+real(j-1)*dely
        !omega    = y*sqrt(grav/zz0)
        !omega    = sqrt(grav*y)
        ! this is the deep water phase speed... no surface tension !!
        !cm       = grav/omega
        ! with this form, y is the wavenumber in the tail;
        y= exp(zinf+real(j-1)*dely)
        tensk    =1+(y/km)**2
        omega    = sqrt(grav*y*tensk)
        cm       = sqrt(grav*tensk/y)
        cgtail   = 0.5*(3*(y/km)**2+1)*sqrt(grav/(y*tensk))
        !this is the inverse wave age, shifted by zzalp (tuning)
        zx       = ustr/cm +zzalp
        zarg     = min(kappa/zx,20.)
        ! zmu corresponds to exp(zcn)
        zmu      = min(grav*zz0/cm**2*exp(zarg),1.)
        zlog     = min(alog(zmu),0.)
        zbeta        = const1*zmu*zlog**4
        !
        ! optional addition of capillary wave peak if sintail2=1
        !
        if (sintailpar(3).gt.0) then
          if (ustr.lt.cm) then
            alpham=max(0.,0.01*(1.+alog(ustr/cm)))
          else
            alpham=0.01*(1+3.*alog(ustr/cm))
          end if
          fm=exp(-0.25*(y/km-1)**2)
          alphat=alpham*(cmm/cm)*fm  ! equivalent to 2*bh in elfouhaily et al.
          levtail=levtail0*0.5*(1-tanh((y-20)/5))+sintailpar(3)*0.5*(1+tanh((y-20)/5))*alphat
        else
          levtail=levtail0
        end if
        ! write(991,*) 'tail??',sintailpar(3),levtail0,levtail,alphat,y,y/km,omega/(tpi)
        !tau1=tau1+w(j)*zbeta*(ustr/ust)**2/y*dely              ! integration over log(y)
        tau1=tau1+w(j)*zbeta*ustr**2*levtail*dely*cgtail/cm       ! integration over log(k)
        ! nb: the factor abs(ttauwshelter) was forgotten in the tauhft2 table
        !taur=taur-w(j)*abs(ttauwshelter)*ustr**2*zbeta*levtail/y*dely
        !taur=taur-w(j)*ustr**2*zbeta*levtail*dely    ! integration over log(y)
        taur=taur-w(j)*sintailpar(2)*ustr**2*zbeta*levtail*dely*cgtail/cm   ! dk/k*cg/c = d omega / omega
        ustr=sqrt(max(taur,0.))
      end do
      deallocate(w)
      tau1nt=tau1
      tauhf = tau1
      !
      ! in this case, uses tables for high frequency contribution to tauw.
      !
    else
      ! finds the values in the tabulated stress tauhft
      xi=ust/delust
      ind  = max(1,min (iustar-1, int(xi)))
      deli1= max(min (1. ,xi-float(ind)),0.)
      deli2= 1. - deli1
      xj=max(0.,(grav*z0/max(ust,0.00001)**2-aalpha) / delalp)
      j    = max(1 ,min (ialpha-1, int(xj)))
      delj1= max(0.,min (1.      , xj-float(j)))
      delj2=1. - delj1
      if (ttauwshelter.gt.0) then
        xk = levtail0/ deltail
        i = min (ilevtail-1, int(xk))
        delk1= min (1. ,xk-float(i))
        delk2=1. - delk1
        tau1 =((tauhft2(ind,j,i)*deli2+tauhft2(ind+1,j,i)*deli1 )*delj2 &
             +(tauhft2(ind,j+1,i)*deli2+tauhft2(ind+1,j+1,i)*deli1)*delj1)*delk2 &
             +((tauhft2(ind,j,i+1)*deli2+tauhft2(ind+1,j,i+1)*deli1 )*delj2 &
             +(tauhft2(ind,j+1,i+1)*deli2+tauhft2(ind+1,j+1,i+1)*deli1)*delj1)*delk1
      else
        tau1 =(tauhft(ind,j)*deli2+tauhft(ind+1,j)*deli1 )*delj2 &
             +(tauhft(ind,j+1)*deli2+tauhft(ind+1,j+1)*deli1)*delj1
      end if
      !
      tauhf = levtail0*ust**2*tau1
    end if ! end of test on use of table
    tauwx = xstress+tauhf*cos(usdirp)
    tauwy = ystress+tauhf*sin(usdirp)
    !
    ! reduces tail effect to make sure that wave-supported stress
    ! is less than total stress, this is borrowed from ecwam stresso.f
    !
    tauw = sqrt(tauwx**2+tauwy**2)
    ust2   = max(ustar,eps2)**2
    tauwb = min(tauw,max(ust2-eps1,eps2**2))
    if (tauwb.lt.tauw) then
      tauwx=tauwx*tauwb/tauw
      tauwy=tauwy*tauwb/tauw
    end if
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3sin4 ----------------------------------------------------- /
    !/
  end subroutine w3sin4
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialization for source term routine.
  !>
  !> @param[in] fltabs
  !>
  !> @author f. ardhuin
  !> @date   30-aug-2010
  !>
  subroutine insin4(fltabs)
    use constants, only: tpiinv, rade, grav
    use w3odatmd,  only: ndse
    use w3servmd,  only: extcde
    use w3dispmd,  only: wavnu2
    use w3gdatmd,  only: sig, dsip, nk, nth, ttauwshelter,             &
         ssdsdth, ssdscos, th, dth, xfr, ecos, esin,   &
         ssdsc,  ssdsbrf1, ssdsbck, ssdsbint, ssdspbk, &
         ssdsabk, ssdshck, iktab, dcki, satindices,    &
         satweights, cumulw, nkhs, nkd, ndtab, qbi,    &
         sintailpar
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    logical, intent(in)     :: fltabs
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer  sdsnth, ith, i_int, j_int, ik, ik2, ith2 , is, is2
    integer  ikl, id, icon, ikd, ikhs, ikh, toto
    real     c, c2
    real     diff1, diff2, binf, bsup, cgg, prof
    real     kik, dhs, kd, khs, kh, xt, gam, dkh, pr, w, eps
    real     dkd
    real, dimension(:,:)   , allocatable :: sigtab
    real, dimension(:,:)   , allocatable :: k1, k2
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  initializations ------------------------------------------------ *
    !
    !
    ! these precomputed tables are written in mod_def.ww3
    !
    if (sintailpar(1).gt.0.5) then
      if (.not. allocated(taut)) allocate(taut(0:itaumax,0:jumax))
      if (.not. allocated(tauhft)) allocate(tauhft(0:iustar,0:ialpha))
      if (fltabs) then
        call tabu_stress
        call tabu_tauhf(sig(nk) )      !tabulate high-frequency stress: 2d table
      end if
      if (ttauwshelter.gt.0) then
        if (.not. allocated(tauhft2)) allocate(tauhft2(0:iustar,0:ialpha,0:ilevtail))
        if (fltabs) call tabu_tauhf2(sig(nk) )   !tabulate high-frequency stress: 3d table
      end if
    end if
    !
    ! 2.  spontaneous breaking
    ! 2.a precomputes the indices for integrating the spectrum to get saturation (test 4xx )
    !
    if (ssdsdth.lt.180) then
      sdsnth  = min(nint(ssdsdth/(dth*rade)),nth/2-1)
      satindices(:,:)=1
      satweights(:,:)=0.
      do ith=1,nth
        do i_int=ith-sdsnth, ith+sdsnth
          j_int=i_int
          if (i_int.lt.1)  j_int=i_int+nth
          if (i_int.gt.nth) j_int=i_int-nth
          satindices(i_int-(ith-sdsnth)+1,ith)=j_int
          satweights(i_int-(ith-sdsnth)+1,ith)=          &
               cos(th(ith)-th(j_int))**ssdscos
        end do
      end do
    else
      satindices(:,:)=1
      satweights(:,:)=1.
    end if
    !/ ------------------------------------------------------------------- /
    !
    ! precomputes qbi and dcki (test 500)
    !
    if (ssdsbck.gt.0) then
      !
      ! precomputes the indices for integrating the spectrum over frequency bandwidth
      !
      binf=(1-ssdsbint) ! banner et al 2002: hp=4*sqrt(int_0.7^1.3fp e df), ssdsbint=0.3
      bsup=(1+ssdsbint)
      kik=0.
      !
      ! high frequency tail for convolution calculation
      !
      allocate(k1(nk,ndtab))
      allocate(k2(nk,ndtab))
      allocate(sigtab(nk,ndtab))
      sigtab=0. !contains frequency for upper windows boundaries
      iktab=0  ! contains indices for upper windows boundaries
      do id=1,ndtab
        toto=0
        prof=real(id)
        do ikl=1,nk ! last window starts at ik=nk
          call wavnu2(sig(ikl), prof, kik, cgg, 1e-7, 15, icon)
          k1(ikl,id)=kik  ! wavenumber lower boundary (is directly related to the frequency indices, ik)
          k2(ikl,id)=((bsup/binf)**2.)*k1(ikl,id)! wavenumber upper boundary
          sigtab(ikl,id)=sqrt(grav*k2(ikl,id)*tanh(k2(ikl,id)*id)) ! corresponding frequency upper boundary
          if(sigtab(ikl,id) .le. sig(1)) then
            iktab(ikl,id)=1
          end if
          if(sigtab(ikl,id) .gt. sig(nk)) then
            iktab(ikl,id)=nk+toto       ! in w3sds4 only windows with iksup<=nk will be kept
            toto=1
          end if
          do ik=1,nk-1
            diff1=0.
            diff2=0.
            if(sig(ik)<sigtab(ikl,id) .and. sig(ik+1)>=sigtab(ikl,id)) then
              diff1=sigtab(ikl,id)-sig(ik)   ! seeks the indices of the upper boundary
              diff2=sig(ik+1)-sigtab(ikl,id)! the indices of lower boudary = ik
              if (diff1<diff2) then
                iktab(ikl,id)=ik
              else
                iktab(ikl,id)=ik+1
              end if
            end if
          end do
        end do
      end do
      !
      ! tabulates dcki and qbi
      !
      dhs=khsmax/nkhs ! max value of khs=khsmax
      dkh=khmax/nkhi  ! max value of kh=khmax
      dkd=kdmax/nkd
      allocate(dcki(nkhs,nkd))
      allocate(qbi(nkhs,nkd))
      dcki=0.
      qbi =0.
      do ikd=1,nkd
        khs=0.
        kd=(fac_kd1**(ikd-fac_kd2))
        xt=tanh(kd)
        gam=1.0314*(xt**3)-1.9958*(xt**2)+1.5522*xt+0.1885
        gam=gam/2.15
        do ikhs=1,nkhs  ! max value of khs=1.
          kh=0.
          khs=khs+dhs
          do ikh=1,nkhi
            kh=kh+dkh
            pr=(4.*kh/(khs**2.))*exp(-(2*((kh/khs)**2.)))
            !              w=1.5*(((khs)/(sqrt(2.)*gam*xt))**2.)*(1-exp(-(((kh)/(gam*xt))**4.))) !ck2002 parameterization
            w=ssdsabk*(((khs)/(sqrt(2.)*gam*xt))**2.)*(1-exp(-(((kh)/(gam*xt))**ssdspbk)))
            eps=-((((ssdsbck/(xt**ssdshck))*kh)**3.)/4)*sqrt(grav/xt)
            dcki(ikhs, ikd)= dcki(ikhs, ikd)+pr*w*eps*dkh
            qbi(ikhs, ikd) = qbi(ikhs, ikd) +pr*w*    dkh
          end do
        end do
      end do
      where ( qbi .gt. 1. )
        qbi = 1.
      end where
      deallocate(k1,k2)
      deallocate(sigtab)
    else
      iktab(:,:)=1
      dcki(:,:) =0.
      qbi(:,:)  =0.
    end if
    !
    !/ ------------------------------------------------------------------- /
    !                        cumulative effect
    !/ ------------------------------------------------------------------- /
    !
    ! precomputes the weights for the cumulative effect (test 441 and 500)
    !
    dikcumul = 0
    if (ssdsc(3).lt.0.) then
      !       dikcumul is the integer difference in frequency bands
      !       between the "large breakers" and short "wiped-out waves"
      dikcumul = nint(ssdsbrf1/(xfr-1.))
      !        write(6,*) 'insin4b:',dikcumul
      cumulw(:,:)=0.
      do ik=1,nk
        c = grav/sig(ik)   ! valid in deep water only
        !c = sig(ik)/k(ik) ! valid in all water depth ???
        do ith=1,nth
          is=ith+(ik-1)*nth
          do ik2=1,ik-dikcumul
            c2 = grav/sig(ik2) ! valid in deep water only
            !c2 = sig(ik2)/k(ik2) ! valid in all water depth ???
            do ith2=1,nth
              is2=ith2+(ik2-1)*nth
              cumulw(is2,is)=sqrt(c**2+c2**2-2*c*c2*ecos(1+abs(ith2-ith))) & ! = deltac
                   *dsip(ik2)/(0.5*c2) * dth                   ! = dk*dtheta (valid in deep water only)
            end do
          end do
        end do
      end do
    else
      cumulw(:,:)=0.
    end if
    !/
    !/ end of insin4 ----------------------------------------------------- /
    !/
  end subroutine insin4
  ! ----------------------------------------------------------------------
  !>
  !> @brief to generate friction velocity table taut(tauw,u10)=sqrt(tau).
  !>
  !> @author f. ardhuin
  !> @date   17-oct-2007
  !>
  subroutine tabu_stress
    use constants, only: kappa, grav
    use w3gdatmd, only: zzwnd, aalpha, zz0max
    implicit none
    integer, parameter      :: niter=10
    real   , parameter      :: xm=0.50, eps1=0.00001
    !     variable.   type.     purpose.
    !      *xm*        real      power of tauw/tau in roughness length.
    !      *xnu*       real      kinematic viscosity of air.
    !      *niter*     integer   number of iterations to obtain total stress
    !      *eps1*      real      small number to make sure that a solution
    !                            is obtained in iteration with tau>tauw.
    ! ----------------------------------------------------------------------
    integer i,j,iter
    real ztauw,utop,cdrag,wcd,ustold,tauold
    real x,ust,zz0,f,delf,zz00
    !
    !
    delu    = umax/float(jumax)
    deltauw = tauwmax/float(itaumax)
    do i=0,itaumax
      ztauw   = (real(i)*deltauw)**2
      do j=0,jumax
        utop    = float(j)*delu
        cdrag   = 0.0012875
        wcd     = sqrt(cdrag)
        ustold  = utop*wcd
        tauold  = max(ustold**2, ztauw+eps1)
        do iter=1,niter
          x   = ztauw/tauold
          ust = sqrt(tauold)
          zz00=aalpha*tauold/grav
          if (zz0max.ne.0) zz00=min(zz00,zz0max)
          ! corrects roughness zz00 for quasi-linear effect
          zz0 = zz00/(1.-x)**xm
          !znu = 0.1*nu_air/ust  ! this was removed by bidlot in 1996
          !zz0 = max(znu,zz0)
          f   = ust-kappa*utop/(alog(zzwnd/zz0))
          delf= 1.-kappa*utop/(alog(zzwnd/zz0))**2*2./ust &
               *(1.-(xm+1)*x)/(1.-x)
          ust = ust-f/delf
          tauold= max(ust**2., ztauw+eps1)
        end do
        taut(i,j)  = sqrt(tauold)
      end do
    end do
    i=itaumax
    j=jumax
    !
    !  force zero wind to have zero stress (bidlot 1996)
    !
    do i=0,itaumax
      taut(i,0)=0.0
    end do
    return
  end subroutine tabu_stress
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief tabulation of the high-frequency wave-supported stress.
  !>
  !> @details see reference for wave stress calculation.
  !>       for quasilinear effect see peter a.e.m. janssen,1990.
  !>     see tech. memo ecmwf 03 december 2003 by bidlot & janssen.
  !>
  !>
  !> @param[in] sigmax  maximum frequency * tpi.
  !>
  !> @author f. ardhuin
  !> @date   14-aug-2006
  !>
  subroutine tabu_tauhf(sigmax)
    !/ ------------------------------------------------------------------- /
    use constants, only: kappa, grav
    use w3gdatmd, only: aalpha, bbeta, zzalp, fachfe, zz0max
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in) :: sigmax  !  maximum frequency
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !       ustarm  r.a.  maximum friction velocity
    !       alpham  r.a.  maximum charnock coefficient
    !       wlv     r.a.  water levels.
    !       ua      r.a.  absolute wind speeds.
    !       ud      r.a.  absolute wind direction.
    !       u10     r.a.  wind speed used.
    !       u10d    r.a.  wind direction used.
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    real                    :: ustarm, alpham
    real                    :: const1, omega, omegac
    real                    :: ust, zz0,omegacc, cm
    integer, parameter      :: jtot=250
    real, allocatable       :: w(:)
    real                    :: zx,zarg,zmu,zlog,zz00,zbeta
    real                    :: y,yc,dely
    integer                 :: j,k,l
    real                    :: x0
    !
    !
    ustarm = 5.
    alpham = 20.*aalpha
    delust = ustarm/real(iustar)
    delalp = alpham/real(ialpha)
    const1 = bbeta/kappa**2
    omegac = sigmax
    !
    tauhft(0:iustar,0:ialpha)=0. !table initialization
    !
    allocate(w(jtot))
    w(2:jtot-1)=1.
    w(1)=0.5
    w(jtot)=0.5
    x0 = 0.05
    !
    do l=0,ialpha
      do k=0,iustar
        ust      = max(real(k)*delust,0.000001)
        zz00       = ust**2*aalpha/grav
        if (zz0max.ne.0) zz00=min(zz00,zz0max)
        zz0       = zz00*(1+float(l)*delalp/aalpha)
        omegacc  = max(omegac,x0*grav/ust)
        yc       = omegacc*sqrt(zz0/grav)
        dely     = max((1.-yc)/real(jtot),0.)
        ! for a given value of ust and alpha,
        ! the wave-supported stress is integrated all the way
        ! to 0.05*g/ust
        do j=1,jtot
          y        = yc+real(j-1)*dely
          omega    = y*sqrt(grav/zz0)
          ! this is the deep water phase speed
          cm       = grav/omega
          !this is the inverse wave age, shifted by zzalp (tuning)
          zx       = ust/cm +zzalp
          zarg     = min(kappa/zx,20.)
          zmu      = min(grav*zz0/cm**2*exp(zarg),1.)
          zlog     = min(alog(zmu),0.)
          zbeta        = const1*zmu*zlog**4
          ! power of y in denominator should be fachfe-4  tail applied here
          tauhft(k,l)  = tauhft(k,l)+w(j)*zbeta/y*dely
        end do
      end do
    end do
    deallocate(w)
    return
  end subroutine tabu_tauhf
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief tabulation of the high-frequency wave-supported stress as
  !>  a function of ustar, alpha (modified charnock), and tail energy
  !>  level.
  !>
  !> @details see reference for wave stress calculation.
  !>  for quasilinear effect see peter a.e.m. janssen,1990.
  !>  see tech. memo ecmwf 03 december 2003 by bidlot & janssen
  !>
  !> @param[in] sigmax  maximum frequency*tpi.
  !>
  !> @author f. ardhuin
  !> @date   24-jan-2013
  !>
  subroutine tabu_tauhf2(sigmax)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update 2006/08/14            |
    !/                  | last update 2013/01/24            |
    !/                  +-----------------------------------+
    !/
    !/    15-may-2007 : origination in ww3                  ( version 3.10.shom )
    !/    24-jan-2013 : allows to read in table             ( version 4.08 )
    !
    !  1. purpose :
    !
    !     tabulation of the high-frequency wave-supported stress as a function of
    !     ustar, alpha (modified charnock), and tail energy level
    !
    !  2. method :
    !
    !       see reference for wave stress calculation.
    !       for quasilinear effect see peter a.e.m. janssen,1990.
    !     see tech. memo ecmwf 03 december 2003 by bidlot & janssen
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       sigmax   real  i   maximum frequency*tpi
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3sin3   wind input source term routine.
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !       !/t      enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: kappa, grav, file_endian
    use w3gdatmd, only: aalpha, bbeta, zzalp, fachfe,  &
         ttauwshelter, zz0max
    use w3odatmd, only: ndse
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in) :: sigmax  !  maximum frequency * tpi
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !       ustarm  r.a.  maximum friction velocity
    !       alpham  r.a.  maximum charnock coefficient
    !       wlv     r.a.  water levels.
    !       ua      r.a.  absolute wind speeds.
    !       ud      r.a.  absolute wind direction.
    !       u10     r.a.  wind speed used.
    !       u10d    r.a.  wind direction used.
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    real                    :: ustarm, alpham, levtailm
    real                    :: const1, omega, omegac, levtail
    real                    :: ust, ust0, zz0,omegacc, cm
    real                    :: tauw, tauw0
    integer, parameter      :: jtot=250
    real, allocatable       :: w(:)
    real                    :: zx,zarg,zmu,zlog,zbeta
    real                    :: y,yc,dely
    integer                 :: i, j, k, l
    real                    :: x0, insigmax, inaalpha, inbbeta, inzzalp, inkappa, ingrav
    integer                 :: iniustar, inialpha, inilevtail, ierr
    character(160)          :: fnametab
    logical                 :: nofile
    character(len=10), parameter :: vergrd = '2018-06-08'
    character(len=35), parameter :: idstr = 'wavewatch iii st4 table for stress '
    character(len=10)       :: vertst=' '
    character(len=35)       :: idtst=' '
    !
    !
    fnametab='st4tabuhf2.bin'
    nofile=.true.
    open (993,file=fnametab,form='unformatted', convert=file_endian,iostat=ierr,status='old')
    if (ierr.eq.0) then
      read(993,iostat=ierr) idtst, vertst, insigmax, inaalpha, inbbeta, iniustar,  &
           inialpha, inilevtail, inzzalp, inkappa, ingrav
      if (vertst.eq.vergrd.and.idtst.eq.idstr.and.ierr.eq.0             &
           .and.insigmax.eq.sigmax.and.inaalpha.eq.aalpha.and.inbbeta.eq.bbeta) then
        if (iniustar.eq.iustar.and.inialpha.eq.ialpha.and.inilevtail.eq.ilevtail.and. &
             inzzalp.eq.zzalp.and.ingrav.eq.grav.and.inkappa.eq.kappa) then
          nofile=.false.
        else
          close(993)
        end if
      end if
    end if
    !
    ustarm = 5.
    alpham = 20.*aalpha
    levtailm = 0.05
    delust  = ustarm/real(iustar)
    delalp  = alpham/real(ialpha)
    deltail = alpham/real(ilevtail)
    const1  = bbeta/kappa**2
    omegac  = sigmax
800 continue
    if ( nofile ) then
      write(ndse,*) 'filling 3d look-up table for sin4. please wait'
      write(ndse,*)  idstr, vergrd, sigmax, aalpha, bbeta, iustar, ialpha,  &
           ilevtail, zzalp, kappa, grav
      !
      tauhft(0:iustar,0:ialpha)=0.  !table initialization
      !
      allocate(w(jtot))
      w(2:jtot-1)=1.
      w(1)=0.5
      w(jtot)=0.5
      x0 = 0.05
      !
      do k=0,iustar
        ust0      = max(real(k)*delust,0.000001)
        do l=0,ialpha
          ust=ust0
          zz0       = ust0**2*(aalpha+float(l)*delalp)/grav
          omegacc  = max(omegac,x0*grav/ust)
          yc       = omegacc*sqrt(zz0/grav)
          dely     = max((1.-yc)/real(jtot),0.)
          ! for a given value of ust and alpha,
          ! the wave-supported stress is integrated all the way
          ! to 0.05*g/ust
          do i=0,ilevtail
            levtail=real(i)*deltail
            tauhft(k,l)=0.
            tauhft2(k,l,i)=0.
            tauw0=ust0**2
            tauw=tauw0
            do j=1,jtot
              y        = yc+real(j-1)*dely
              omega    = y*sqrt(grav/zz0)
              ! this is the deep water phase speed
              cm       = grav/omega
              !this is the inverse wave age, shifted by zzalp (tuning)
              zx       = ust0/cm +zzalp
              zarg     = min(kappa/zx,20.)
              zmu      = min(grav*zz0/cm**2*exp(zarg),1.)
              zlog     = min(alog(zmu),0.)
              zbeta        = const1*zmu*zlog**4
              ! power of y in denominator should be fachfe-4
              tauhft(k,l)  = tauhft(k,l)+w(j)*zbeta/y*dely
              zx       = ust/cm +zzalp
              zarg     = min(kappa/zx,20.)
              zmu      = min(grav*zz0/cm**2*exp(zarg),1.)
              zlog     = min(alog(zmu),0.)
              zbeta        = const1*zmu*zlog**4
              ! power of y in denominator should be fachfe-4
              tauhft2(k,l,i)  = tauhft2(k,l,i)+w(j)*zbeta*(ust/ust0)**2/y*dely
              tauw=tauw-w(j)*ust**2*zbeta*levtail/y*dely
              ust=sqrt(max(tauw,0.))
            end do
          end do
        end do
      end do
      deallocate(w)
      open (993,file=fnametab,form='unformatted', convert=file_endian,iostat=ierr,status='unknown')
      write(993) idstr, vergrd, sigmax, aalpha, bbeta, iustar, ialpha, ilevtail, zzalp, kappa, grav
      write(993) tauhft(0:iustar,0:ialpha)
      write(993) tauhft2
      close(993)
      !do k=0,iustar
      !  do l=0,ialpha
      !    do i=0,ilevtail
      !      write(995,*) k,l,i,max(real(k)*delust,0.000001),aalpha+float(l)*delalp,real(i)*deltail,tauhft(k,l),tauhft2(k,l,i)
      !      end do
      !    end do
      !  end do
      !
    else
      write(ndse,*) 'reading 3d look-up table for sin4 from file.'
      read(993,err=2000,iostat=ierr ) tauhft(0:iustar,0:ialpha)
      read(993,err=2000,iostat=ierr ) tauhft2
      close(993)
    end if
    !
    goto 2001
2000 nofile=.true.
    goto 800
2001 continue
    return
  end subroutine tabu_tauhf2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief compute friction velocity based on wind speed u10.
  !>
  !> @details computation of u* based on quasi-linear theory.
  !>
  !> @param[in]  windspeed  10-m wind speed -- should be neutral.
  !> @param[in]  tauw       wave-supported stress.
  !> @param[out] ustar      friction velocity.
  !> @param[out] z0         air-side roughness length.
  !> @param[out] charn      charnock.
  !>
  !> @author f. ardhuin
  !> @date   14-aug-2006
  !>
  subroutine calc_ustar(windspeed,tauw,ustar,z0,charn)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update 2006/08/14            |
    !/                  +-----------------------------------+
    !/
    !/    27-feb-2004 : origination in ww3                  ( version 2.22-shom )
    !/     the resulting table was checked to be identical to the original f77 result
    !/    14-aug-2006 : modified following bidlot           ( version 2.22-shom )
    !/    18-aug-2006 : ported to version 3.09
    !/    03-apr-2010 : adding output of charnock parameter ( version 3.14-ifremer )
    !
    !  1. purpose :
    !
    !     compute friction velocity based on wind speed u10
    !
    !  2. method :
    !
    !     computation of u* based on quasi-linear theory
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       u10,tauw,ustar,z0
    !     ----------------------------------------------------------------
    !       windspeed real  i   10-m wind speed ... should be neutral
    !       tauw      real  i   wave-supported stress
    !       ustar     real  o   friction velocity.
    !       z0        real  o   air-side roughness length
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3sin3   wind input source term routine.
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !       !/s      enable subroutine tracing.
    !       !/t      enable test output.
    !
    ! 10. source code :
    !-----------------------------------------------------------------------------!
    use constants, only: grav, kappa, nu_air
    use w3gdatmd,  only: zzwnd, aalpha, zz0max, sintailpar, capchnk
    implicit none
    real, intent(in) :: windspeed,tauw
    real, intent(out) :: ustar, z0, charn
    ! local variables
    real             :: sqrtcdm1
    real             :: xi,deli1,deli2,xj,delj1,delj2  ! used for table version
    integer          :: ind,j
    real             :: tauw_local
    real             :: tauold,cdrag,wcd,ustold,x,ust,zz0,znu,zz00,f,delf
    real             :: chath, xmin ! used for reduction of high winds
    integer, parameter      :: niter=10
    real   , parameter      :: xm=0.50, eps1=0.00001
    integer                 :: iter
    !     variable.   type.     purpose.
    !      *xm*        real      power of tauw/tau in roughness length.
    !      *xnu*       real      kinematic viscosity of air.
    !      *niter*     integer   number of iterations to obtain total stress
    !      *eps1*      real      small number to make sure that a solution
    !                            is obtained in iteration with tau>tauw.
    chath = aalpha
    !
    if (sintailpar(1).gt.0.5) then
      tauw_local=max(min(tauw,tauwmax),0.)
      xi      = sqrt(tauw_local)/deltauw
      ind     = min ( itaumax-1, int(xi)) ! index for stress table
      deli1   = min(1.,xi - real(ind))  !interpolation coefficient for stress table
      deli2   = 1. - deli1
      xj      = windspeed/delu
      j       = min ( jumax-1, int(xj) )
      delj1   = min(1.,xj - real(j))
      delj2   = 1. - delj1
      ustar=(taut(ind,j)*deli2+taut(ind+1,j  )*deli1)*delj2 &
           + (taut(ind,j+1)*deli2+taut(ind+1,j+1)*deli1)*delj1
    else
      if (capchnk(1).eq.1.) then
        ! computation of sea surface roughness and charnock coefficient based
        ! on donelan (2018). determines minimum charnock; reduction for winds
        ! above a particular threshold
        chath  = capchnk(2) + 0.5 * (capchnk(3) - capchnk(2)) * (1 &
                 - tanh((windspeed-capchnk(4))/capchnk(5)))
        xmin   = 0.15 * (capchnk(3)-chath)
      else
        xmin  = 0.
      end if
      ! this max is for comparison ... to be removed later
      !        tauw_local=max(min(tauw,tauwmax),0.)
      tauw_local=tauw
      cdrag   = 0.0012875
      wcd     = sqrt(cdrag)
      ustold  = windspeed*wcd
      tauold  = max(ustold**2, tauw_local+eps1)
      ! newton method to solve for ustar in u=ustar*log(z/z0)
      do iter=1,niter
        x   = max(tauw_local/tauold, xmin)
        ust = sqrt(tauold)
        zz00 = chath*tauold/grav
        if (zz0max.ne.0) zz00=min(zz00,zz0max)
        ! corrects roughness zz00 for quasi-linear effect
        zz0 = zz00/(1.-x)**xm
        znu = 0.11*nu_air/max(ust,1e-6)
        zz0 = sintailpar(5)*znu+zz0
        f   = ust-kappa*windspeed/(alog(zzwnd/zz0))
        delf= 1.-kappa*windspeed/(alog(zzwnd/zz0))**2*2./ust &
             *(1.-(xm+1)*x)/(1.-x)
        ust = ust-f/delf
        tauold= max(ust**2., tauw_local+eps1)
      end do
      ustar=ust
    end if
    !
    ! determines roughness length
    !
    if (ustar.gt.0.001) then
      sqrtcdm1  = min(windspeed/ustar,100.0)
      z0  = zzwnd*exp(-kappa*sqrtcdm1)
      charn = grav*z0/ustar**2
    else
      if (ustar.gt.0) then
        sqrtcdm1  = min(windspeed/ustar,100.0)
        z0  = zzwnd*exp(-kappa*sqrtcdm1)
      else
        z0 = chath*0.001*0.001/grav
      end if
      charn = chath
    end if
    if(capchnk(1) .eq. 1) then
      ! problem with large values of charn for low winds
      charn = min( 0.09 , charn )
      if(charn.lt.chath) charn = chath
    endif
    !  write(6,*) 'calc_ustar:',windspeed,tauw,aalpha,charn,z0,ustar
    !
    return
  end subroutine calc_ustar
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate whitecapping source term and diagonal term of derivative.
  !>
  !> @details this codes does either one or the other of
  !>          ardhuin et al. (jpo 2010)
  !>          filipot & ardhuin (jgr 2012)
  !>          romero (grl 2009)
  !>          the choice depends on sdsbchoice
  !>
  !> @param[in]  a          action density spectrum (1-d).
  !> @param[in]  k          wavenumber for entire spectrum.
  !> @param[in]  cg         group velocity.
  !> @param[in]  ustar      friction velocity.
  !> @param[in]  usdir      wind stress direction.
  !> @param[in]  depth      water depth.
  !> @param[in]  dair       air density.
  !> @param[out] srhs
  !> @param[out] ddiag
  !> @param[in]  ix         grid index.
  !> @param[in]  iy         grid index.
  !> @param[out] brlambda   phillips' lambdas.
  !> @param[out] whitecap
  !> @param[in]  dlwmean
  !>
  !> @author f. ardhuin
  !> @author f. leckler
  !> @author l. romero
  !> @date   13-aug-2021
  !>
  subroutine w3sds4 (a, k, cg, ustar, usdir, depth, dair, srhs,    &
       ddiag, ix, iy, brlambda, whitecap, dlwmean )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  ! f. ardhuin, f. leckler, l. romero !
    !/                  |                        fortran 90 |
    !/                  | last update :         13-aug-2021 |
    !/                  +-----------------------------------+
    !/
    !/    30-aug-2010 : clean up from common st3-st4 routine( version 3.14-ifremer )
    !/    23-jan-2012 : add output of lambdas to be used in sin
    !/    13-nov-2013 : reduced frequency range with ig1 switch
    !/    06-jun-2018 : add optional debugsrc              ( version 6.04 )
    !/    22-feb-2020 : option to use romero (grl 2019)    ( version 7.06 )
    !/    13-aug-2021 : consider dair a variable           ( version 7.14 )
    !/
    !  1. purpose :
    !
    !     calculate whitecapping source term and diagonal term of derivative.
    !
    !  2. method :
    !
    !       this codes does either one or the other of
    !       ardhuin et al. (jpo 2010)
    !       filipot & ardhuin (jgr 2012)
    !       romero (grl 2009)
    !       the choice depends on sdsbchoice
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       ix, iy    int   i   grid index
    !       a         r.a.  i   action density spectrum (1-d).
    !       k         r.a.  i   wavenumber for entire spectrum.          *)
    !       ustar     real  i   friction velocity.
    !       usdir     real  i   wind stress direction.
    !       depth     real  i   water depth.
    !       dair      real  i   air density
    !       s         r.a.  o   source term (1-d version).
    !       d         r.a.  o   diagonal term of derivative.             *)
    !       brlambda  r.a.  o   phillips' lambdas
    !     ----------------------------------------------------------------
    !                         *) stored in 1-d array with dimension nth*nk
    !
    !  4. subroutines used :
    !
    !       strace    subroutine tracing.                 ( !/s switch )
    !       prt2ds    print plot of spectrum.             ( !/t0 switch )
    !       outmat    print out matrix.                   ( !/t1 switch )
    !
    !  5. called by :
    !
    !       w3srce   source term integration.
    !       w3expo   point output program.
    !       gxexpo   grads point output program.
    !
    !  6. error messages :
    !
    !  7. remarks :
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s   enable subroutine tracing.
    !     !/t   enable general test output.
    !     !/t0  2-d print plot of source term.
    !     !/t1  print arrays.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants,only: grav, dwat, pi, tpi, rade, debug_node
    use w3gdatmd, only: nspec, nth, nk, ssdsbr, ssdsbt, dden,      &
         ssdsc, ec2, es2, esc,                      &
         sig, ssdsp, ecos, esin, dth, aairgb,       &
         ssdsiso, ssdsdth, ssdsbm, aaircmin,        &
         ssdsbrfdf, ssdsbck, iktab, dcki,           &
         satindices, satweights, cumulw, nkhs, nkd, &
         ndtab, qbi, dsip, ssdsbrf1,xfr
    use w3odatmd, only: flogrd
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, optional, intent(in) :: ix, iy
    real, intent(in)        :: a(nspec), k(nk), cg(nk),            &
         depth, dair, ustar, usdir, dlwmean
    real, intent(out)       :: srhs(nspec), ddiag(nspec), brlambda(nspec)
    real, intent(out)       :: whitecap(1:4)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, is2, is0, ikl, ikc, id, nkl
    integer                 :: ik, ik1, ith, ik2, jth, ith2,             &
         ikhs, ikd, sdsnth, it, ikm, nkm
    integer                 :: nsmooth(nk)
    real                    :: c, c2, cumulwiso, coswind, asum, sdiagiso
    real                    :: coef1, coef2, coef4(nk),      &
         coef5(nk)
    real                    :: facturb, facturb2, dturb, dvisc, diag2, breakfraction
    real                    :: renewalfreq, epsr
    real                    :: s1(nk), e1(nk)
    integer                 :: ntimes(nk)
    real                    :: gam, xt
    real                    :: dk(nk), hs(nk), kbar(nk), dck(nk)
    real                    :: efdf(nk)     ! energy integrated over a spectral band
    integer                 :: iksup(nk)
    real                    :: facsat, dkhs, facstrainb, facstrainl
    real                    :: bth0(nk)     !saturation spectrum
    real                    :: bth(nspec)   !saturation spectrum
    real                    :: msssum(nk,5),  fachf
    real                    :: msslong
    real                    :: msspcs, msspc2, mssps2, mssp, mssd, mssth
    real                    :: miche, x, kloc
    real                    :: qb(nk), s2(nk)
    real                    :: tstr, tmax, dt, t, mft, dirforcum
    real                    :: pb(nspec), pb2(nspec), brm12(nk), btover
    real                    :: ko, lmodulation(nth)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    !----------------------------------------------------------------------
    !
    ! 0.  pre-initialization to zero out arrays. all arrays should be reset
    !     within the computation, but these are helping with some bugs
    !     found in certain compilers
    nsmooth=0
    s1=0.; e1=0.
    ntimes=0;iksup=0
    dk=0.; hs=0.; kbar=0.; dck=0.; efdf=0.
    bth0=0.; bth=0.; ddiag=0.; srhs=0.; pb=0.
    msssum(:,:)=0.
    qb=0.; s2=0.;pb=0.; pb2=0.
    brm12(:)=0.
    !
    ! 1.  initialization and numerical factors
    !
    facturb=ssdsc(5)*ustar**2/grav*dair/dwat
    dikcumul = nint(ssdsbrf1/(xfr-1.))
    breakfraction=0.
    renewalfreq=0.
    ik1=1
    !
    ! 1.b mss parameters used for modulation factors for lambda (romero )
    !
    if (ssdsc(8).gt.0.or.ssdsc(11).gt.0.or.ssdsc(18).gt.0) then
      do ik=1,nk
        mssp   = 0.
        msspc2 = 0.
        mssps2 = 0.
        msspcs = 0.
        !
        ! sums the contributions to the directional mss for all angles
        !
        do ith=1,nth
          is=ith+(ik-1)*nth
          msslong  = k(ik)**ssdsc(20) * a(is) * dden(ik) / cg(ik) ! contribution to mss
          msspc2 = msspc2 +msslong*ec2(ith)
          mssps2 = mssps2 +msslong*es2(ith)
          msspcs = msspcs +msslong*esc(ith)
          mssp   = mssp   +msslong
        end do
        msssum  (ik:nk,1) = msssum (ik:nk,1) +mssp
        msssum  (ik:nk,3) = msssum (ik:nk,3) +msspc2
        msssum  (ik:nk,4) = msssum (ik:nk,4) +mssps2
        msssum  (ik:nk,5) = msssum (ik:nk,5) +msspcs
        !
        ! direction of long wave mss summed up to ik
        !
        mssd=0.5*(atan2(2*msssum(ik,5),msssum(ik,3)-msssum(ik,4)))
        if (mssd.lt.0) mssd = mssd + pi
        msssum  (ik,2)  =  mssd
      end do
    end if ! ssdsc(8).gt.0) then
    !
    ! 2.   estimation of spontaneous breaking from local saturation
    !
    !############################################################################################"
    select case (nint(ssdsc(1)))
    case (1)
      !
      ! 2.a  case of a direction-dependent breaking term following ardhuin et al. 2010
      !
      epsr = sqrt(ssdsbr)
      !
      ! 2.a.1 computes saturation
      !
      bth(:) = 0.
      do  ik=ik1, nk
        facsat=sig(ik)*k(ik)**3*dth
        is0=(ik-1)*nth
        bth(is0+1)=0.
        asum = sum(a(is0+1:is0+nth))
        bth0(ik)=asum*facsat
        !
        if (ssdsdth.ge.180) then  ! integrates around full circle
          bth(is0+1:is0+nth)=bth0(ik)
        else
          do ith=1,nth            ! partial integration
            is=ith+(ik-1)*nth
            bth(is)=dot_product(satweights(:,ith),  a(is0+satindices(:,ith)) ) &
                 *facsat
          end do
          bth0(ik)=maxval(bth(is0+1:is0+nth))
        end if
        !
      end do !ik=nk
      !
      !  2.a.2  computes spontaneous breaking dissipation rate
      !
      do  ik=ik1, nk
        !
        !  correction of saturation level for shallow-water kinematics
        !
        if (ssdsbm(0).eq.1) then
          miche=1.
        else
          x=tanh(min(k(ik)*depth,10.))
          ! correction of saturation threshold for shallow-water kinematics
          miche=(x*(ssdsbm(1)+x*(ssdsbm(2)+x*(ssdsbm(3)+x*ssdsbm(4)))))**2
        end if
        coef1=(ssdsbr*miche)
        !
        !  computes isotropic part
        !
        sdiagiso = ssdsc(2) * sig(ik)*ssdsc(6)*(max(0.,bth0(ik)/coef1-1.))**2
        !
        !  computes anisotropic part and sums isotropic part
        !
        coef2=ssdsc(2) * sig(ik)*(1-ssdsc(6))/(coef1*coef1)
        ddiag((ik-1)*nth+1:ik*nth) = sdiagiso + &
             coef2*((max(0.,bth((ik-1)*nth+1:ik*nth)-coef1))**ssdsp)
      end do
      !
      ! computes breaking probability
      !
      pb = (max(sqrt(bth)-epsr,0.))**2
      !
      ! multiplies by 28.16 = 22.0 * 1.6² * 1/2 with
      !  22.0 (banner & al. 2000, figure 6)
      !  1.6  the coefficient that transforms  sqrt(b) to banner et al. (2000)'s epsilon
      !  1/2  factor to correct overestimation of banner et al. (2000)'s breaking probability due to zero-crossing analysis
      !
      pb = pb * 28.16
      ! compute lambda = pb* l(k,th)
      ! with l(k,th)=1/(2*pi²)= the breaking crest density
      brlambda = pb / (2.*pi**2.)
      srhs = ddiag * a
      !############################################################################################"
    case(2)
      !
      ! 2.b             computes spontaneous breaking for t500 (filipot et al. jgr 2010)
      !
      e1 = 0.
      hs = 0.
      srhs  = 0.
      ddiag = 0.
      pb2  = 0.
      !
      ! computes wavenumber spectrum e1 integrated over direction and computes dk
      !
      do ik=ik1, nk
        e1(ik)=0.
        do ith=1,nth
          is=ith+(ik-1)*nth
          e1(ik)=e1(ik)+(a(is)*sig(ik))*dth
        end do
        dk(ik)=dden(ik)/(dth*sig(ik)*cg(ik))
      end do
      !
      ! gets windows indices of iktab
      !
      id=min(nint(depth),ndtab)
      if (id < 1) then
        id = 1
      else if(id > ndtab) then
        id = ndtab
      end if
      !
      ! loop over wave scales
      !
      hs=0.
      efdf=0.
      kbar=0.
      nkl=0. !number of windows
      do ikl=1,nk
        iksup(ikl)=iktab(ikl,id)
        if (iksup(ikl) .le. nk) then
          efdf(ikl) = dot_product(e1(ikl:iksup(ikl)-1),dk(ikl:iksup(ikl)-1))
          if (efdf(ikl) .ne. 0) then
            kbar(ikl) = dot_product(k(ikl:iksup(ikl)-1)*e1(ikl:iksup(ikl)-1), &
                 dk(ikl:iksup(ikl)-1)) / efdf(ikl)
          else
            kbar(ikl)=0.
          end if
          ! estimation of significant wave height of a given scale
          hs(ikl) = 4*sqrt(efdf(ikl))
          nkl = nkl+1
        end if
      end do
      !
      ! computes dissipation and breaking probability in each scale
      !
      dck=0.
      qb =0.
      dkhs = khsmax/nkhs
      do ikl=1, nkl
        if (hs(ikl) .ne. 0. .and. kbar(ikl) .ne. 0.)  then
          ! gets indices for tabulated dissipation dcki and breaking probability qbi
          !
          ikd = fac_kd2+anint(log(kbar(ikl)*depth)/log(fac_kd1))
          ikhs= 1+anint(kbar(ikl)*hs(ikl)/dkhs)
          if (ikd > nkd) then    ! deep water
            ikd = nkd
          else if (ikd < 1) then ! shallow water
            ikd = 1
          end if
          if (ikhs > nkhs) then
            ikhs = nkhs
          else if (ikhs < 1) then
            ikhs = 1
          end if
          xt = tanh(kbar(ikl)*depth)
          !
          ! gamma corrected for water depth
          !
          gam=1.0314*(xt**3)-1.9958*(xt**2)+1.5522*xt+0.1885
          !
          ! computes the energy dissipated for the scale ikl
          ! using dcki which is tabulated in insin4
          !
          dck(ikl)=((kbar(ikl)**(-2.5))*(kbar(ikl)/(2*pi)))*dcki(ikhs,ikd)
          !
          ! get the breaking probability for the scale ikl
          !
          qb(ikl) = qbi(ikhs,ikd) ! qbi is tabulated in insin4
        else
          dck(ikl)=0.
          qb(ikl) =0.
        end if
      end do
      !
      ! distributes scale dissipation over the frequency spectrum
      !
      s1 = 0.
      s2 = 0.
      ntimes = 0
      do ikl=1, nkl
        if (efdf(ikl) .gt. 0.) then
          s1(ikl:iksup(ikl))    = s1(ikl:iksup(ikl)) + &
               dck(ikl)*e1(ikl:iksup(ikl)) / efdf(ikl)
          s2(ikl:iksup(ikl))    = s2(ikl:iksup(ikl)) + &
               qb(ikl) *e1(ikl:iksup(ikl)) / efdf(ikl)
          ntimes(ikl:iksup(ikl)) = ntimes(ikl:iksup(ikl)) + 1
        end if
      end do
      !
      ! finish the average
      !
      where (ntimes .gt. 0)
        s1 = s1 / ntimes
        s2 = s2 / ntimes
      elsewhere
        s1 = 0.
        s2 = 0.
      end where
      ! goes back to action for dissipation source term
      s1(1:nk) = s1(1:nk) / sig(1:nk)
      !
      ! makes isotropic distribution
      !
      asum = 0.
      do ik = 1, nk
        asum = (sum(a(((ik-1)*nth+1):(ik*nth)))*dth)
        if (asum.gt.1.e-8) then
          forall (is=1+(ik-1)*nth:ik*nth) ddiag(is)  = s1(ik)/asum
          forall (is=1+(ik-1)*nth:ik*nth) pb2(is) = s2(ik)/asum
        else
          forall (is=1+(ik-1)*nth:ik*nth) ddiag(is)  = 0.
          forall (is=1+(ik-1)*nth:ik*nth) pb2(is) = 0.
        end if
        if (pb2(1+(ik-1)*nth).gt.0.001) then
          bth0(ik) = 2.*ssdsbr
        else
          bth0(ik) = 0.
        end if
      end do
      !
      pb = (1-ssdsc(1))*pb2*a + ssdsc(1)*pb
      ! compute lambda = pb* l(k,th)
      ! with l(k,th)=1/(2*pi²)= the breaking crest density
      brlambda = pb / (2.*pi**2.)
      srhs = ddiag * a
      !############################################################################################"
    case(3)
      !
      ! 2c romero (grl 2019)
      !
      ! directional saturation i
      ! integrate in azimuth
      ko=(grav/(1e-6+ustar**2))/(28./ssdsc(16))**2
      do ik=1,nk
        is0=(ik-1)*nth
        kloc=k(ik)**(2-ssdsc(20)) ! local wavenumber factor, if mss not used.
        bth(1:nth)=max(a(is0+1:is0+nth)*sig(ik)*k(ik)**3,.00000000000001)
        !
        dirforcum=dlwmean
        if (ssdsc(11).gt.0) dirforcum=msssum(ik,2)
        c=sig(ik)/k(ik)
        bth0(ik)=sum(bth(1:nth)*dth)
        if (ssdsc(18).gt.0) then ! applies modulation factor on lambda
          do ith=1,nth
            facstrainl=1.+ssdsc(18)*((msssum(ik,1)*kloc)**ssdsc(14) *      &   ! romero
                 (ecos(ith)*cos(dirforcum)+esin(ith)*sin(dirforcum))**2)
            lmodulation(ith)= facstrainl**ssdsc(19)
          end do
        else
          lmodulation(:)= 1.
        end if
        ! lambda
        brlambda(is0+1:is0+nth)=ssdsc(9)*exp(-ssdsbr/bth(1:nth))              &
             *( 1.0+ssdsc(13)*max(1.,(k(ik)/ko))**ssdsc(15) ) &
             /(ssdsc(13)+1)*lmodulation(1:nth)
        ! breaking strength : generalisation of duncan's b parameter
        btover = sqrt(bth0(ik))-sqrt(ssdsbt)
        brm12(ik)=ssdsc(2)*(max(0.,btover))**(2.5)/sig(ik)  ! not function of direction
        !  for consistency set brlambda set to zero if b is zero
        brlambda(is0+1:is0+nth)= max(0.,sign(brlambda(is0+1:is0+nth),btover))
        !  source term / sig2  (action dissipation)
        srhs(is0+1:is0+nth)= brm12(ik)/grav**2*brlambda(is0+1:is0+nth)*c**5
        ! diagonal
        ddiag(is0+1:is0+nth) = srhs(is0+1:is0+nth)*ssdsbr/max(1.e-20,bth(1:nth))/max(1e-20,a(is0+1:is0+nth))  !
      end do
      !   breaking probability (is actually the breaking rate)
      pb = brlambda *c
      !
    end select
    !############################################################################################"
    !
    !
    !/ ------------------------------------------------------------------- /
    !             wave-turbulence interaction and cumulative effect
    !/ ------------------------------------------------------------------- /
    !
    !
    ! loop over spectrum
    !
    if ( (ssdsc(3).ne.0.) .or. (ssdsc(5).ne.0.) .or. (ssdsc(21).ne.0.) ) then
      do  ik=ik1, nk
        renewalfreq = 0.
        facturb2=-2.*sig(ik)*k(ik)*facturb
        dvisc=-4.*ssdsc(21)*k(ik)*k(ik)
        c = sig(ik)/k(ik) ! phase speed
        !
        if (ssdsc(3).gt.0 .and. ik.gt.dikcumul) then
          ! this is the cheap isotropic version
          do ik2=ik1,ik-dikcumul
            c2 = sig(ik2)/k(ik2)
            is2=(ik2-1)*nth
            cumulwiso=abs(c2-c)*dsip(ik2)/(0.5*c2) * dth
            renewalfreq=renewalfreq-cumulwiso*sum(brlambda(is2+1:is2+nth))
          end do
        end if
        do ith=1,nth
          is=ith+(ik-1)*nth
          !
          ! computes cumulative effect from breaking probability
          !
          if (ssdsc(3).lt.0 .and. ik.gt.dikcumul) then
            renewalfreq = 0.
            ! this is the expensive and largely useless version
            do ik2=ik1,ik-dikcumul
              if (bth0(ik2).gt.ssdsbr) then
                is2=(ik2-1)*nth
                renewalfreq=renewalfreq+dot_product(cumulw(is2+1:is2+nth,is),brlambda(is2+1:is2+nth))
              end if
            end do
          end if
          !
          ! computes wave turbulence interaction
          !
          coswind=(ecos(ith)*cos(usdir)+esin(ith)*sin(usdir))
          dturb=facturb2*max(0.,coswind)  ! theory -> stress direction
          !
          ! add effects
          !
          diag2 = (ssdsc(3)*renewalfreq+dturb+dvisc)
          ddiag(is) = ddiag(is) + diag2
          srhs(is)  = srhs(is)  + a(is)* diag2
        end do
      end do
    end if
    !
    !  computes whitecap parameters
    !
    if ( .not. (flogrd(5,7).or.flogrd(5,8) ) ) then
      return
    end if
    !
    whitecap(1:4) = 0.
    !
    ! precomputes integration of lambda over direction
    ! times wavelength times a (a=5 in reul&chapron jgr 2003) times dk
    !
    do ik=1,min(floor(aaircmin),nk)
      c=sig(ik)/k(ik)
      is0=(ik-1)*nth
      coef4(ik) = c*c*sum(brlambda(is0+1:is0+nth))                          &
           *2.*pi/grav*ssdsc(7) * dden(ik)/(sig(ik)*cg(ik))
      coef5(ik) = c**3*sum(brlambda(is0+1:is0+nth)                           &
           *brm12(ik))                       	       &
           *aairgb/grav * dden(ik)/(sig(ik)*cg(ik))
      !        coef4(ik) = sum(brlambda((ik-1)*nth+1:ik*nth) * dth) *(2*pi/k(ik)) *  &
      !                    ssdsc(7) * dden(ik)/(dth*sig(ik)*cg(ik))
      !                   nb: ssdsc(7) is whitecapwidth
    end do
    ! need to extrapolate above nk if necessary ... to be added later.
    do ik=min(floor(aaircmin),nk),nk
      coef4(ik)=0.
      coef5(ik)=0.
    end do
    !/
    if ( flogrd(5,7) ) then
      !
      ! computes the total whitecap coverage (a=5. ; reul and chapron, 2003)
      !
      do ik=ik1,min(floor(aaircmin),nk)
        whitecap(1) = whitecap(1) + coef4(ik) * (1-whitecap(1))
        whitecap(4) = whitecap(4) + coef5(ik)
      end do
    end if
    !/
    if ( flogrd(5,8) ) then
      !
      ! calculates the mean foam thickness for component k(ik) => fig.3, reul and chapron, 2003
      ! ( copied from st4 - not yet tested/validated with romero 2019 (lambda model)
      !
      do ik=ik1,nk
        !    duration of active breaking (tau*)
        tstr = 0.8 * 2*pi/sig(ik)
        !    time persistence of foam (a=5.)
        tmax = 5.  * 2*pi/sig(ik)
        dt   = tmax / 50
        mft  = 0.
        do it = 1, 50
          ! integration over time of foam persistance
          t = float(it) * dt
          ! eq. 5 and 6 of reul and chapron, 2003
          if ( t .lt. tstr ) then
            mft = mft + 0.4 / (k(ik)*tstr) * t * dt
          else
            mft = mft + 0.4 / k(ik) * exp(-1*(t-tstr)/3.8) * dt
          end if
        end do
        mft = mft / tmax
        !
        ! computes foam-layer thickness (reul and chapron, 2003)
        !
        whitecap(2) = whitecap(2) + coef4(ik) * mft
      end do
    end if
    !
    ! end of output computing
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3sds4 ----------------------------------------------------- /
    !/
  end subroutine w3sds4
end module w3src4md
