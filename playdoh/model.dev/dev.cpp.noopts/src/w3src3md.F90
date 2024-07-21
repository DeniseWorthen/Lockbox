!> @file
!> @brief the 'wam4+' source terms based on p.a.e.m. janssen's work, with
!>  extensions by him and by j.-r. bidlot.
!>
!> @author f. ardhuin
!> @author h. l. tolman
!> @date   02-sep-2012
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
!> @brief the 'wam4+' source terms based on p.a.e.m. janssen's work, with
!>  extensions by him and by j.-r. bidlot.
!>
!> @details converted from the original wam codes by f. ardhuin,
!>  with further extensions to adapt to a saturation-based breaking
!>  and observation-based swell dissipation.
!>
!> @author f. ardhuin
!> @author h. l. tolman
!> @date   02-sep-2012
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3src3md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii                shom |
  !/                  !            f. ardhuin             !
  !/                  |           h. l. tolman            |
  !/                  |                        fortran 90 |
  !/                  | last update :         02-sep-2012 |
  !/                  +-----------------------------------+
  !/
  !/    09-oct-2007 : origination.                        ( version 3.13 )
  !/    10-oct-2010 : adding janssen-style swell damping  ( version 3.14)
  !/    02-sep-2012 : clean up test output t, t0, t1      ( version 4.07 )
  !/
  !  1. purpose :
  !
  !     the 'wam4+' source terms based on p.a.e.m. janssen's work, with
  !     extensions by him and by j.-r. bidlot. converted from the original
  !     wam codes by f. ardhuin, with further extensions to adapt to a
  !     saturation-based breaking and observation-based swell dissipation
  !
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
  !      w3spr3    subr. public   mean parameters from spectrum.
  !      w3sin3    subr. public   wam4+ input source term.
  !      insin3    subr. public   corresponding initialization routine.
  !      tabu_stress, tabu_tauhf, tabu_tauhf2
  !                subr. public   populate various tables.
  !      calc_ustar
  !                subr. public   compute stresses.
  !      w3sds3    subr. public   user supplied dissipation.
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !
  !  6. switches :
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  use constants, only: kappa, nu_air
  implicit none
  public
  !/
  !/ public variables
  !/
  !air kinematic viscosity (used in wam)
  integer, parameter      :: itaumax=200,jumax=200
  integer, parameter      :: iustar=100,ialpha=200, ilevtail=50
  real                    :: taut(0:itaumax,0:jumax), deltauw, delu
  ! table for h.f. stress as a function of 2 variables
  real                    :: tauhft(0:iustar,0:ialpha), delust, delalp
  real,    parameter      :: umax    = 50.
  real,    parameter      :: tauwmax = 2.2361 !sqrt(5.)
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate mean wave parameters for the use in the source term
  !>  routines.
  !>
  !> @param[in]    a       action density spectrum.
  !> @param[in]    cg      group velocities.
  !> @param[in]    wn      wavenumbers.
  !> @param[out]   emean   energy.
  !> @param[out]   fmean   mean frequency for determination of tail.
  !> @param[out]   fmeans  mean frequency for dissipation source term.
  !> @param[out]   wnmean  mean wavenumber.
  !> @param[out]   amax    maximum of action spectrum.
  !> @param[in]    u       wind speed.
  !> @param[in]    udir    wind direction.
  !> @param[inout] ustar   friction velocity
  !> @param[inout] usdir   wind stress direction.
  !> @param[in]    tauwx   component of wave-supported stress.
  !> @param[in]    tauwy   component of wave-supported stress.
  !> @param[out]   cd      drag coefficient at wind level zwnd.
  !> @param[out]   z0      corresponding z0.
  !> @param[out]   charn
  !> @param[in]    llws    wind sea true/false array for each component.
  !> @param[out]   fmeanws mean frequency of wind sea, used for tail.
  !>
  !> @author f. ardhuin
  !> @author h. l. tolman
  !> @date   17-oct-2007
  !>
  subroutine w3spr3 (a, cg, wn, emean, fmean, fmeans, wnmean,     &
       amax, u, udir, ustar, usdir, tauwx, tauwy, cd, z0,&
       charn, llws, fmeanws)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                shom |
    !/                  !            f. ardhuin             !
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-oct-2007 |
    !/                  +-----------------------------------+
    !/
    !/    03-oct-2007 : origination.                        ( version 3.13 )
    !/
    !  1. purpose :
    !
    !     calculate mean wave parameters for the use in the source term
    !     routines.
    !
    !  2. method :
    !
    !     see source term routines.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum.
    !       cg      r.a.  i   group velocities.
    !       wn      r.a.  i   wavenumbers.
    !       emean   real  o   energy
    !       fmean   real  o   mean  frequency for determination of tail
    !       fmeans  real  o   mean  frequency for dissipation source term
    !       wnmean  real  o   mean wavenumber.
    !       amax    real  o   maximum of action spectrum.
    !       u       real  i   wind speed.
    !       udir    real  i   wind direction.
    !       ustar   real i/o  friction velocity.
    !       usdir   real i/o  wind stress direction.
    !       tauwx-y real  i   components of wave-supported stress.
    !       cd      real  o   drag coefficient at wind level zwnd.
    !       z0      real  o   corresponding z0.
    !       llws    l.a.  i   wind sea true/false array for each component
    !       fmeanws real  o   mean frequency of wind sea, used for tail
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !       strace   service routine.
    !
    !  5. called by :
    !
    !       w3srce   source term integration routine.
    !       w3outp   point output program.
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
    !       !/s      enable subroutine tracing.
    !       !/t      enable test output.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only: tpiinv
    use w3gdatmd, only: nk, nth, nspec, sig, dth, dden, wwnmeanp, &
         wwnmeanptail, fte, ftf, sstxftf, sstxftwn,&
         sstxftftail, sswellf
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
    real, intent(out)       :: emean, fmean, fmeans, wnmean, amax,  &
         cd, z0, charn, fmeanws
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, ik, ith
    real                    :: tauw, eband, emeanws, unz,       &
         eb(nk),eb2(nk),alfa(nk)
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
    fmeans = 0.
    wnmean = 0.
    amax   = 0.
    !
    ! 1.  integral over directions and maximum --------------------------- *
    !
    do ik=1, nk
      eb(ik)  = 0.
      eb2(ik) = 0.
      do ith=1, nth
        is=ith+(ik-1)*nth
        eb(ik) = eb(ik) + a(ith,ik)
        if (llws(is)) eb2(ik) = eb2(ik) + a(ith,ik)
        amax   = max ( amax , a(ith,ik) )
      end do
    end do
    !
    ! 2.  integrate over directions -------------------------------------- *
    !
    do ik=1, nk
      alfa(ik) = 2. * dth * sig(ik) * eb(ik) * wn(ik)**3
      eb(ik)   = eb(ik) * dden(ik) / cg(ik)
      eb2(ik)   = eb2(ik) * dden(ik) / cg(ik)
      emean    = emean  + eb(ik)
      fmean    = fmean  + eb(ik) *(sig(ik)**(2.*wwnmeanptail))
      fmeans   = fmeans + eb(ik) *(sig(ik)**(2.*wwnmeanp))
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
    fmean  = fmean  + eband * sstxftftail
    fmeans = fmeans + eband * sstxftf
    wnmean = wnmean + eband * sstxftwn
    eband  = eb2(nk) / dden(nk)
    emeanws = emeanws + eband * fte
    fmeanws = fmeanws + eband * sstxftftail
    !
    ! 4.  final processing
    !
    if (fmean.lt.1.e-7) then
      fmean=tpiinv * sig(nk)
    else
      fmean  = tpiinv *( max ( 1.e-7 , fmean )                       &
           / max ( 1.e-7 , emean ))**(1/(2.*wwnmeanptail))
    end if
    if (fmeans.lt.1.e-7) then
      fmeans=tpiinv * sig(nk)
    else
      fmeans = tpiinv *( max ( 1.e-7 , fmeans )                      &
           / max ( 1.e-7 , emean ))**(1/(2.*wwnmeanp))
    end if
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
    z0=0.
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
    !/ end of w3spr3 ----------------------------------------------------- /
    !/
  end subroutine w3spr3
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate diagonal and input source term for wam4+ approach.
  !>
  !> @verbatim
  !>       wam-4 : janssen et al.
  !>       wam-"4.5" : gustiness effect (cavaleri et al. )
  !>       swellf: damping coefficient (=1) for janssen (2004) theory
  !> @endverbatim
  !>
  !> @param[in]  a       action density spectrum (1-d).
  !> @param[in]  cg      group speed.
  !> @param[in]  k       wavenumber for entire spectrum.
  !> @param[in]  u       wind speed.
  !> @param[in]  ustar   friction velocity.
  !> @param[in]  drat    air/water density ratio.
  !> @param[in]  as      air-sea temperature difference.
  !> @param[in]  usdir   wind stress direction.
  !> @param[in]  z0      air-sea roughness length.
  !> @param[in]  cd      wind drag coefficient.
  !> @param[out] tauwx   component of the wave-supported stress.
  !> @param[out] tauwy   component of the wave-supported stress.
  !> @param[out] tauwnx  component of the negative wave-supported stress.
  !> @param[out] tauwny  component of the negative wave-supported stress.
  !> @param[in]  ice     sea ice fraction.
  !> @param[out] s       source term (1-d version).
  !> @param[out] d       diagonal term of derivative.
  !> @param[out] llws    wind sea true/false array for each component.
  !> @param[in]  ix
  !> @param[in]  iy
  !>
  !> @author f. ardhuin
  !> @author h. l. tolman
  !> @date   02-sep-2012
  !>
  subroutine w3sin3 (a, cg, k, u, ustar, drat, as, usdir, z0, cd,    &
       tauwx, tauwy, tauwnx, tauwny, ice, s, d, llws, ix, iy)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                shom |
    !/                  !            f. ardhuin             !
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         02-sep-2012 |
    !/                  +-----------------------------------+
    !/
    !/    09-oct-2007 : origination.                        ( version 3.13 )
    !/    16-may-2010 : adding sea ice                      ( version 3.14_ifremer )
    !/    02-sep-2012 : clean up test output t, t0, t1      ( version 4.07 )
    !/
    !  1. purpose :
    !
    !     calculate diagonal and input source term for wam4+ approach.
    !
    !  2. method :
    !
    !       wam-4 : janssen et al.
    !       wam-"4.5" : gustiness effect (cavaleri et al. )
    !       swellf: damping coefficient (=1) for janssen (2004) theory
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       cg      r.a.  i   group speed                              *)
    !       k       r.a.  i   wavenumber for entire spectrum.          *)
    !       u       real  i   wind speed
    !       ustar   real  i   friction velocity.
    !       drat    real  i   air/water density ratio.
    !       as      real  i   air-sea temperature difference
    !       usdir   real  i   wind stress direction
    !       z0      real  i   air-side roughness lengh.
    !       cd      real  i   wind drag coefficient.
    !       usdir   real  i   direction of friction velocity
    !       tauwx-y real  i   components of the wave-supported stress.
    !       tauwnx  real  i   component of the negative wave-supported stress.
    !       tauwny  real  i   component of the negative wave-supported stress.
    !       ice     real  i   sea ice fraction.                        !/stefan: ice is dummy argument; remove later.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative.             *)
    !       llws    l.a.  o   wind sea true/false array for each component
    !     ----------------------------------------------------------------
    !                         *) stored as 1-d array with dimension nth*nk
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
    use constants, only: grav, tpi
    use w3gdatmd, only: nk, nth, nspec, xfr, dden, sig, sig2, th,   &
         esin, ecos, ec2, zzwnd, aalpha, bbeta, zzalp,&
         sswellf,                                     &
         dden2, dth, ssinthp,zz0rat
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec)
    real, intent(in)        :: cg(nk), k(nspec),z0,u, cd
    real, intent(in)        :: ustar, usdir, as, drat, ice       !/stefan: ice is dummy argument; remove later.
    real, intent(out)       :: s(nspec), d(nspec), tauwx, tauwy, tauwnx, tauwny
    logical, intent(out)    :: llws(nspec)
    integer, intent(in)     :: ix, iy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is,ik,ith
    real                    :: cosu, sinu, taux, tauy
    real                    :: ust2, tauw, tauwb
    real   , parameter      :: eps1 = 0.00001, eps2 = 0.000001
    real                    :: cm,uco,ucn,zcn, &
         z0visc
    real xi,deli1,deli2
    real xj,delj1,delj2
    real                    :: const, const0, const2, const3,  tau1
    real                    :: x,zarg,zlog,zbeta,ust
    real coswind,xstress,ystress,tauhf
    real temp, temp2
    integer ind,j,istab
    real dstab(3,nspec)
    real stressstab(3,2),stressstabn(3,2)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !
    ! 1.  preparations
    !
    !      jdm: initializing arrays (shouldn't change answers)
    dstab=0.; stressstab=0. ;stressstabn=0.
    !
    ! 1.a  estimation of surface roughness parameters
    !
    z0visc = 0.1*nu_air/max(ustar,0.0001)
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
      const0=bbeta*drat/(kappa**2)
      ! sswellf(1) is idamping in ecmwam
      const3 = sswellf(1)*2.*kappa*drat
      !
      cosu   = cos(usdir)
      sinu   = sin(usdir)
      do ik=1, nk
        is=1+(ik-1)*nth
        cm=k(is)/sig2(is) !inverse of phase speed
        ucn=ust*cm+zzalp  !this is the inverse wave age
        !
        ! the stress is the real stress (n/m^2) divided by
        ! rho_a, and thus comparable to ustar**2
        ! it is the integral of rho_w g sin/c /rho_a
        ! (air-> waves momentum flux)
        !
        const2=dden2(is)/cg(ik) &        !jacobian to get energy in band
             *grav/(sig(ik)/k(is)*drat) ! coefficient to get momentum
        const=sig2(is)*const0
        ! this cm parameter is 1 / c_phi
        ! this is the "correct" shallow-water expression
        ! here z0 corresponds to z0+z1 of the janssen eq. 14
        zcn=alog(k(is)*z0)
        ! commented below is the original wam version (ok for deep water)
        !       zcn=alog(g*z0b(i)*cm(i)**2)
        do ith=1,nth
          is=ith+(ik-1)*nth
          coswind=(ecos(is)*cosu+esin(is)*sinu)
          if (coswind.gt.0.01) then
            x=coswind*ucn
            ! this zarg term is the argument of the exponential
            ! in janssen 1991 eq. 16.
            zarg=kappa/x
            ! zlog is alog(mu) where mu is defined by janssen 1991 eq. 15
            zlog=zcn+zarg
            zbeta = const3*(coswind+kappa/(zcn*ust*cm))*ucn**2
            if (zlog.lt.0.) then
              ! the source term sp is beta * omega * x**2
              ! as given by janssen 1991 eq. 19
              dstab(istab,is) = const*exp(zlog)*zlog**4*ucn**2*coswind**ssinthp
              llws(is)=.true.
            else
              dstab(istab,is) = zbeta
              llws(is)=.true.
            end if
          else
            zbeta = const3*(coswind+kappa/(zcn*ust*cm))*ucn**2
            dstab(istab,is) = zbeta
            llws(is)=.false.
          end if
          !
          temp2=const2*dstab(istab,is)*a(is)
          if (dstab(istab,is).lt.0) then
            stressstabn(istab,1)=stressstabn(istab,1)+temp2*ecos(is)
            stressstabn(istab,2)=stressstabn(istab,2)+temp2*esin(is)
          end if
          stressstab(istab,1)=stressstab(istab,1)+temp2*ecos(is)
          stressstab(istab,2)=stressstab(istab,2)+temp2*esin(is)
        end do
      end do
      !
      d(:)=dstab(3,:)
      xstress=stressstab (3,1)
      ystress=stressstab (3,2)
      tauwnx =stressstabn(3,1)
      tauwny =stressstabn(3,2)
      !             write(995,'(a,11g14.5)') 'negstress:    ',tauwnx,tauwny,fw*uorb**3
    s = d * a
    !
    ! ... test output of arrays
    !
    !
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
    ! finds the values in the tabulated stress tauhft
    !
    xi=ust/delust
    ind  = max(1,min (iustar-1, int(xi)))
    deli1= max(min (1. ,xi-float(ind)),0.)
    deli2= 1. - deli1
    xj=max(0.,(grav*z0/max(ust,0.00001)**2-aalpha) / delalp)
    j    = max(1 ,min (ialpha-1, int(xj)))
    delj1= max(0.,min (1.      , xj-float(j)))
    delj2=1. - delj1
    tau1 =(tauhft(ind,j)*deli2+tauhft(ind+1,j)*deli1 )*delj2 &
         +(tauhft(ind,j+1)*deli2+tauhft(ind+1,j+1)*deli1)*delj1
    tauhf = const0*temp*ust**2*tau1
    tauwx = xstress+tauhf*cos(usdir)
    tauwy = ystress+tauhf*sin(usdir)
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
    return
    !
    ! formats
    !
    !/
    !/ end of w3sin3 ----------------------------------------------------- /
    !/
  end subroutine w3sin3
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialization for source term routine.
  !>
  !> @author f. ardhuin
  !> @date   23-jul-2009
  !>
  subroutine insin3
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                         shom      |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-jul-2009 |
    !/                  +-----------------------------------+
    !/
    !/    23-jun-2006 : origination.                        ( version 3.09 )
    !/    23-jul-2007 : cleaning up convolutions            ( version 3.14-shom)
    !
    !  1. purpose :
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
    !/ ------------------------------------------------------------------- /
    use constants, only: tpiinv
    use w3gdatmd,  only: sig, nk
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
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
    call tabu_stress
    call tabu_tauhf(sig(nk) * tpiinv)   !tabulate high-frequency stress
    !/
    !/ end of insin3 ----------------------------------------------------- /
    !/
  end subroutine insin3
  ! ----------------------------------------------------------------------
  !>
  !> @brief to generate the friction velocity table, taut(tauw,u10)=sqrt(tau).
  !>
  !> @author f. ardhuin
  !> @date   17-oct-2007
  !>
  subroutine tabu_stress
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-oct-2007 |
    !/                  +-----------------------------------+
    !/
    !/    23-jun-2006 : origination.                        ( version 3.13 )
    !/     adapted from wam, original:p.a.e.m. janssen    knmi august 1990
    !/     adapted version (subr. stress): j. bidlot    ecmwf october 2004
    !/     table values were checkes against the original f90 result and found to
    !/     be identical (at least at 0.001 m/s accuracy)
    !/
    !  1. purpose :
    !     to generate friction velocity table taut(tauw,u10)=sqrt(tau).
    !     method.
    !       a steady state wind profile is assumed.
    !       the wind stress is computed using the roughnesslength
    !                  z1=z0/sqrt(1-tauw/tau)
    !       where z0 is the charnock relation , tauw is the wave-
    !       induced stress and tau is the total stress.
    !       we search for steady-state solutions for which tauw/tau < 1.
    !       for quasilinear effect see peter a.e.m. janssen,1990.
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
    !/ ------------------------------------------------------------------- /
    use constants, only: grav
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
    real x,ust,zz0,znu,f,delf,zz00
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
  !>  for quasilinear effect see peter a.e.m. janssen,1990.
  !>  see tech. memo ecmwf 03 december 2003 by bidlot & janssen.
  !>
  !> @param[in] frmax  maximum frequency.
  !>
  !> @author f. ardhuin
  !> @date   14-aug-2006
  !>
  subroutine tabu_tauhf(frmax)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update 2006/08/14            |
    !/                  +-----------------------------------+
    !/
    !/    27-feb-2004 : origination in ww3                  ( version 2.22.shom )
    !/     the resulting table was checked to be identical to the original f77 result
    !/    14-aug-2006 : modified following bidlot           ( version 2.22.shom )
    !/    18-aug-2006 : ported to version 3.09
    !
    !  1. purpose :
    !
    !     tabulation of the high-frequency wave-supported stress
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
    !       frmax   real  i   maximum frequency.
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
    use constants, only: grav, tpi
    use w3gdatmd, only: aalpha, bbeta, zzalp, xfr, fachfe, zz0max
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in) :: frmax  !  maximum frequency
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
    integer                 :: i,j,k,l
    real                    :: x0
    !
    ustarm = 5.
    alpham = 20.*aalpha
    delust = ustarm/real(iustar)
    delalp = alpham/real(ialpha)
    const1 = bbeta/kappa**2
    omegac = tpi*frmax
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
          ! power of y in denominator should be fachfe-4
          tauhft(k,l)  = tauhft(k,l)+w(j)*zbeta/y*dely
        end do
        !if (mod(k,5).eq.0.and.mod(l,5).eq.0) &
        !write(102,'(2i4,3g16.8)') l,k,ust,aalpha+float(l)*delalp,tauhft(k,l)
      end do
    end do
    deallocate(w)
    !      do l=0,ialpha
    !         do k=0,iustar
    !write(101,'(a,2i4,g16.8)') 'l,k,tauhft(k,l):',l,k,tauhft(k,l)
    !         end do
    !        end do
    !write(101,*) 'tauhft:',frmax,bbeta,aalpha,const1,omegac,tpi
    !write(101,'(20g16.8)') tauhft
    return
  end subroutine tabu_tauhf
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief compute friction velocity based on wind speed u10.
  !>
  !> @details computation of u* based on quasi-linear theory.
  !>
  !> @param[in]  windspeed  10-m wind speed ... should be neutral
  !> @param[in]  tauw       wave-supported stress.
  !> @param[out] ustar      friction velocity.
  !> @param[out] z0         air-side roughness length.
  !> @param[out] charn
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
    use constants, only: grav
    use w3gdatmd,  only: zzwnd, aalpha
    implicit none
    real, intent(in) :: windspeed,tauw
    real, intent(out) :: ustar, z0, charn
    ! local variables
    real sqrtcdm1
    real x,xi,deli1,deli2,xj,delj1,delj2
    real ust,deltold,tauw_local
    integer ind,j
    !
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
    !
    ! determines roughness length
    !
    sqrtcdm1  = min(windspeed/ustar,100.0)
    z0  = zzwnd*exp(-kappa*sqrtcdm1)
    if (ustar.gt.0.001) then
      charn = grav*z0/ustar**2
    else
      charn = aalpha
    end if
    !
    return
  end subroutine calc_ustar
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate whitecapping source term and diagonal term of derivative.
  !>
  !> @details wam-cycle 4 and following.
  !>  the last update (09-may-2005) follows the redefinition of
  !>  the mean wavenumber as in bidlot et al. (2005).
  !>
  !> @param[in] a       action density spectrum (1-d).
  !> @param[in] k       wavenumber for entire spectrum.
  !> @param[in] cg
  !> @param[in] emean   mean wave energy.
  !> @param[in] fmean   mean wave frequency.
  !> @param[in] wnmean  mean wavenumber.
  !> @param[in] ustar   friction velocity.
  !> @param[in] usdir   wind stress direction.
  !> @param[in] depth   water depth.
  !> @param[out] s      source term (1-d version).
  !> @param[out] d      diagonal term of derivative.
  !> @param[in] ix
  !> @param[in] iy
  !>
  !> @author f. ardhuin
  !> @date   23-jul-2009
  !>
  subroutine w3sds3 (a, k, cg, emean, fmean, wnmean, ustar, usdir,    &
       depth, s, d, ix, iy)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  !            f. ardhuin             !
    !/                  |                        fortran 90 |
    !/                  | last update :         23-jul-2009 |
    !/                  +-----------------------------------+
    !/
    !/    05-dec-1996 : final fortran 77                    ( version 1.18 )
    !/    08-dec-1999 : upgrade to fortran 90               ( version 2.00 )
    !/    14-aug-2006 : generic wam4+ dissipation term      ( version 2.22shom )
    !/    23-jul-2009 : addition of filipot &al convolution ( version 3.14-shom )
    !/
    !  1. purpose :
    !
    !     calculate whitecapping source term and diagonal term of derivative.
    !
    !  2. method :
    !
    !       wam-cycle 4 and following.
    !       the last update (09-may-2005) follows the redefinition of
    !       the mean wavenumber as in bidlot et al. (2005).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a       r.a.  i   action density spectrum (1-d).
    !       k       r.a.  i   wavenumber for entire spectrum.          *)
    !       emean   real  i   mean wave energy.
    !       fmean   real  i   mean wave frequency.
    !       wnmean  real  i   mean wavenumber.
    !       ustar   real  i   friction velocity.
    !       usdir   real  i   wind stress direction.
    !       depth   real  i   water depth.
    !       s       r.a.  o   source term (1-d version).
    !       d       r.a.  o   diagonal term of derivative.             *)
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
    use constants, only: grav, tpi
    use w3gdatmd, only: nspec, nth, nk, ddelta1, ddelta2,   &
         ssdsc1
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), k(nk), cg(nk),                     &
         depth, ustar, usdir, emean, fmean, wnmean
    integer, intent(in)     :: ix, iy
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, ik, ith
    real                    :: factor, factor2
    real                    :: alfamean, wnmean2
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  pre-initialization of arrays, should be set before being used
    !     but this is helping with bit reproducibility
    d=0.
    ! 1.  common factor
    !
    wnmean2 = max( 1.e-10 , wnmean  )
    alfamean=wnmean**2*emean
    factor = ssdsc1 * tpi*fmean * alfamean**2
    !
    !
    !----------------------------------------------------------------------
    !
    ! 2.  source term
    !
    do  ik=1, nk
      !
      !    original wam4/wam4+ dissipation term
      !
      factor2=factor*(ddelta1*k(ik)/wnmean2 + ddelta2*(k(ik)/wnmean2)**2)
      do ith=1,nth
        is=ith+(ik-1)*nth
        d(is)=  factor2
      end do
    end do
    !
    s = d * a
    !
    ! ... test output of arrays
    !
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3sds3 ----------------------------------------------------- /
    !/
  end subroutine w3sds3
end module w3src3md
