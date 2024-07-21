!> @file
!> @brief source term integration routine.
!>
!> @author h. l. tolman
!> @author f. ardhuin
!> @date   22-mar-2021
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
!> @brief source term integration routine.
!>
!> @author h. l. tolman
!> @author f. ardhuin
!> @date   22-mar-2021
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3srcemd
  real, parameter, private:: offset = 1.
  !/
contains
  subroutine w3srce ( srce_call, it, isea, jsea, ix, iy, imod,          &
       specold, spec, vsio, vdio, shaveio,         &
       alpha, wn1, cg1, clatsl,                    &
       d_inp, u10abs, u10dir,                      &
       as, ustar, ustdir,                          &
       cx, cy,  ice, iceh, icef, icedmax,          &
       reflec, refled, delx, dely, dela, trnx,     &
       trny, berg, fpi, dtdyn, fcut, dtg, tauwx,   &
       tauwy, tauox, tauoy, tauwix, tauwiy, tauwnx,&
       tauwny, phiaw, charn, tws, phioc, whitecap, &
       d50, psic, bedform , phibbl, taubbl, tauice,&
       phice, tauocx, tauocy, wnmean, dair, coef)
    use constants, only: dwat, srce_imp_post, srce_imp_pre,         &
         srce_direct, grav, tpi, tpiinv
    use w3gdatmd, only: nk, nth, nspec, sig, th, dmin, dtmax,       &
         dtmin, facti1, facti2, facsd, fachfa, facp, &
         xfc, xflt, xrel, xft, fxfm, fxpm, dden,     &
         fte, ftf, fhmax, ecos, esin, iicedisp,      &
         icescales, iicesmooth
    use w3wdatmd, only: time
    use w3odatmd, only: ndse, ndst, iaproc
    use w3idatmd, only: inflags2
    use w3dispmd
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: srce_call, it, isea, jsea, ix, iy, imod
    real, intent(in)        :: specold(nspec), clatsl
    real, intent(out)       :: vsio(nspec), vdio(nspec)
    logical, intent(out)    :: shaveio
    real, intent(in)        :: d_inp, u10abs,     &
         u10dir, as, cx, cy, dtg, d50,psic,   &
         ice, iceh
    integer, intent(in)     :: refled(6)
    real, intent(in)        :: reflec(4), delx, dely, dela,         &
         trnx, trny, berg, icedmax, dair
    real, intent(inout)     :: wn1(nk), cg1(nk), &
         spec(nspec), alpha(nk), ustar,       &
         ustdir, fpi, tauox, tauoy,           &
         tauwx, tauwy, phiaw, phioc, phice,   &
         charn, tws, bedform(3), phibbl,      &
         taubbl(2), tauice(2), whitecap(4),   &
         tauwix, tauwiy, tauwnx, tauwny,      &
         icef, tauocx, tauocy, wnmean
    real, intent(out)       :: dtdyn, fcut
    real, intent(in)        :: coef
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer :: ik, ith, is, is0, nsteps, nkh, nkh1, &
         iks1, is1, nspech, idt, ierr, isp
    real :: dttot, fhigh, dt, afilt, damax, afac, &
         hdt, zwnd, fp, depth, tauscx, tauscy, fhigi
    ! scaling factor for sin, sds, snl
    real :: icescaleln, icescalein, icescalenl, icescaleds
    real :: emean, fmean, amax, cd, z0, scat,    &
         smooth_icedisp
    real :: wn_r(nk), cg_ice(nk), alpha_liu(nk), icecoef2, r(nk)
    double precision :: att, iso
    real :: eband, diff, efinish, hstot, phinl,       &
         fmean1, fmeanws, &
         factor, factor2, drat, tauwax, tauway,    &
         mwxfinish, mwyfinish, a1band, b1band,     &
         cosi(2)
    real :: specinit(nspec), spec2(nspec), frlocal, jac2
    real :: dam (nspec), dam2(nspec), wn2(nspec),  &
         vsln(nspec),                         &
         vsin(nspec), vdin(nspec),            &
         vsnl(nspec), vdnl(nspec),            &
         vsds(nspec), vdds(nspec),            &
         vsbt(nspec), vdbt(nspec)
    real :: vs(nspec), vd(nspec), eb(nk)
    logical :: shave
    logical :: lbreak
    logical, save :: first = .true.
    logical :: printdeltasmda
    real :: einc1, einc2, evs, evd, jac
    real :: deltasrc(nspec)
    real :: fout(nk,nth), sout(nk,nth), dout(nk,nth)
    real, save :: taunux, taunuy
    logical, save :: fltest = .false., flagnn = .true.
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters dependent on compile switch
    !/
    !
    !/ -- end of variable delclarations
    !
    !
    iks1 = 1
    is1=(iks1-1)*nth+1
    !! initialise source term arrays:
    vd   = 0.
    vs   = 0.
    vdio = 0.
    vsio = 0.
    vsbt = 0.
    vdbt = 0.
    !
    ! 1.  preparations --------------------------------------------------- *
    !
    depth = max ( dmin , d_inp )
    drat = dair / dwat
    icescaleln = max(0.,min(1.,1.-ice*icescales(1)))
    icescalein = max(0.,min(1.,1.-ice*icescales(2)))
    icescalenl = max(0.,min(1.,1.-ice*icescales(3)))
    icescaleds = max(0.,min(1.,1.-ice*icescales(4)))
    ! 1.a set maximum change and wavenumber arrays.
    !
    !xp     = 0.15
    !facp   = xp / pi * 0.62e-3 * tpi**4 / grav**2
    !
    do ik=1, nk
      dam(1+(ik-1)*nth) = facp / ( sig(ik) * wn1(ik)**3 )
      wn2(1+(ik-1)*nth) = wn1(ik)
    end do
    !
    do ik=1, nk
      is0    = (ik-1)*nth
      do ith=2, nth
        dam(ith+is0) = dam(1+is0)
        wn2(ith+is0) = wn2(1+is0)
      end do
    end do
    !
    ! 1.b prepare dynamic time stepping
    !
    dtdyn  = 0.
    dttot  = 0.
    nsteps = 0
    phiaw  = 0.
    charn  = 0.
    tws    = 0.
    phinl  = 0.
    phibbl = 0.
    tauwix = 0.
    tauwiy = 0.
    tauwnx = 0.
    tauwny = 0.
    tauwax = 0.
    tauway = 0.
    tauscx = 0.
    tauscy = 0.
    taubbl = 0.
    tauice = 0.
    phice  = 0.
    tauocx = 0.
    tauocy = 0.
    wnmean = 0.
    !
    ! time is updated in w3wavemd prior to the call of w3scre, we should
    ! move 'time' one time step backward (ql)
    !
    !
    ! 1.c set mean parameters
    !
    !
    ! 1.c2 stores the initial data
    !
    specinit = spec
    !
    ! 1.d stresses
    !
    !
    ! 1.e prepare cut-off beyond which the tail is imposed with a power law
    !
    !
    ! 1.f prepare output file for !/nnt option
    !
    !
    ! ... branch point dynamic integration - - - - - - - - - - - - - - - -
    !
    do
      !
      nsteps = nsteps + 1
      !
      !
      ! 2.  calculate source terms ----------------------------------------- *
      !
      ! 2.a input.
      !
      !
      !
      ! 2.b nonlinear interactions.
      !
      !
      !
      ! 2.c dissipation... except for st4
      ! 2.c1   as in source term package
      !
      !
      !
      ! 2.c2   optional dissipation parameterisations
      !
      !
      ! 2.d bottom interactions.
      !
      !
      !
      ! 2.e unresolved obstacles source term
      !
      !
      ! 2.g dump training data if necessary
      !
      !
      ! 3.  set frequency cut-off ------------------------------------------ *
      !
      nkh    = min ( nk , int(facti2+facti1*log(max(1.e-7,fhigh))) )
      nkh1   = min ( nk , nkh+1 )
      nspech = nkh1*nth
      !
      ! 4.  summation of source terms and diagonal term and time step ------ *
      !
      dt     = min ( dtg-dttot , dtmax )
      afilt  = max ( dam(nspec) , xflt*amax )
      !
      !     for input and dissipation calculate the fraction of the ice-free
      !     surface. in the presence of ice, the effective water surface
      !     is reduce to a fraction of the cell size free from ice, and so is
      !     input :
      !             sin = (1-ice)**iscalein*sin and sds=(1-ice)**iscaleds*sds ------------------ *
      !     inflags2(4) is true if ice concentration was ever read during
      !             this simulation
      if ( inflags2(4) ) then
        vsnl(1:nspech) = icescalenl * vsnl(1:nspech)
        vdnl(1:nspech) = icescalenl * vdnl(1:nspech)
        vsln(1:nspech) = icescaleln * vsln(1:nspech)
        vsin(1:nspech) = icescalein * vsin(1:nspech)
        vdin(1:nspech) = icescalein * vdin(1:nspech)
        vsds(1:nspech) = icescaleds * vsds(1:nspech)
        vdds(1:nspech) = icescaleds * vdds(1:nspech)
      end if
      !
      do is=is1, nspech
        vs(is) = vsln(is) + vsin(is) + vsnl(is)  &
             + vsds(is) + vsbt(is)
        vd(is) =  vdin(is) + vdnl(is)  &
             + vdds(is) + vdbt(is)
        damax = min ( dam(is) , max ( xrel*specinit(is) , afilt ) )
        afac = 1. / max( 1.e-10 , abs(vs(is)/damax) )
          dt = min ( dt , afac / ( max ( 1.e-10,                  &
               1. + offset*afac*min(0.,vd(is)) ) ) )
      end do  ! end of loop on is
      !
      dt     = max ( 0.5, dt ) ! the hardcoded min. dt is a problem for certain cases e.g. laborotary scale problems.
      !
      dtdyn  = dtdyn + dt
      idt = 1 + int ( 0.99*(dtg-dttot)/dt ) ! number of iterations
      dt = (dtg-dttot)/real(idt)           ! actualy time step
      shave = dt.lt.dtmin .and. dt.lt.dtg-dttot   ! limiter check ...
      shaveio = shave
      dt = max ( dt , min (dtmin,dtg-dttot) ) ! override dt with input time step or last time step if it is bigger ... anyway the limiter is on!
      !
      if (srce_call .eq. srce_imp_post) dt = dtg  ! for implicit part
        hdt    = offset * dt
      dttot  = dttot + dt
      !
      !
      ! 5.  increment spectrum --------------------------------------------- *
      !
      if (srce_call .eq. srce_direct) then
        if ( shave ) then
          do is=is1, nspech
            einc1 = vs(is) * dt / max ( 1. , (1.-hdt*vd(is)))
            einc2 = sign ( min (dam(is),abs(einc1)) , einc1 )
            spec(is) = max ( 0. , spec(is)+einc2 )
          end do
        else
          !
          do is=is1, nspech
            einc1 = vs(is) * dt / max ( 1. , (1.-hdt*vd(is)))
            spec(is) = max ( 0. , spec(is)+einc1 )
          end do
        end if
        !
      end if ! srce_call .eq. srce_direct
      !
      ! 5.b  computes
      !              atmos->wave flux phiaw-------------------------------- *
      !              wave ->bbl  flux phibbl------------------------------- *
      !              wave ->ice  flux phice ------------------------------- *
      !
      whitecap(3)=0.
      hstot=0.
      do ik=iks1, nk
        factor = dden(ik)/cg1(ik)                    !jacobian to get energy in band
        factor2= factor*grav*wn1(ik)/sig(ik)         ! coefficient to get momentum
        ! wave direction is "direction to"
        ! therefore there is a plus sign for the stress
        do ith=1, nth
          is   = (ik-1)*nth + ith
          cosi(1)=ecos(is)
          cosi(2)=esin(is)
          phiaw = phiaw + (vsin(is))* dt * factor                    &
               / max ( 1. , (1.-hdt*vdin(is))) ! semi-implict integration scheme
          phibbl= phibbl- (vsbt(is))* dt * factor                    &
               / max ( 1. , (1.-hdt*vdbt(is))) ! semi-implict integration scheme
          phinl = phinl + vsnl(is)* dt * factor                      &
               / max ( 1. , (1.-hdt*vdnl(is))) ! semi-implict integration scheme
          if (vsin(is).gt.0.) whitecap(3) = whitecap(3) + spec(is)  * factor
          hstot = hstot + spec(is) * factor
        end do
      end do
      whitecap(3) = 4. * sqrt(whitecap(3))
      hstot =4.*sqrt(hstot)
      tauwix = tauwix + tauwx * drat * dt
      tauwiy = tauwiy + tauwy * drat * dt
      tauwnx = tauwnx + tauwax * drat * dt
      tauwny  = tauwny + tauway * drat * dt
      ! missing: tail to be added ?
      !
      !
      ! 6.  add tail ------------------------------------------------------- *
      !   a mean parameters
      !
      !
      !
      !
      !
      !
      !
      !
      !
      ! 6.b limiter for shallow water or miche style criterion
      !     last time step only !
      !     uses true depth (d_inp) instead of limited depth
      !
      !
      ! 6.c seeding of spectrum
      !     alpha = 0.005 , 0.5 in eq., 0.25 for directional distribution
      !
      !
      ! 6.d add tail
      !
      do ik=nkh+1, nk
        do ith=1, nth
          spec(ith+(ik-1)*nth) = spec(ith+(ik-2)*nth) * fachfa         &
               + 0.
        end do
      end do
      !
      ! 6.e  update wave-supported stress----------------------------------- *
      !
      !
      ! 7.  check if integration complete ---------------------------------- *
      !
      ! update qi5tstart (q. liu)
      if (srce_call .eq. srce_imp_post) then
        exit
      endif
      if ( dttot .ge. 0.9999*dtg ) then
        ! if (ix == debug_node) write(*,*) 'dttot, dtg', dttot, dtg
        exit
      endif
    end do ! integration loop
    !
    ! ... end point dynamic integration - - - - - - - - - - - - - - - - - -
    !
    ! 8.  save integration data ------------------------------------------ *
    !
    dtdyn  = dtdyn / real(max(1,nsteps))
    fcut   = fhigh * tpiinv
    !
    goto 888
    !
    ! error escape locations
    !
    !
888 continue
    !
    ! 9.a  computes phioc------------------------------------------ *
    !     the wave to ocean flux is the difference between initial energy
    !     and final energy, plus wind input plus the snl flux to high freq.,
    !     minus the energy lost to the bottom boundary layer (bbl)
    !
    efinish  = 0.
    mwxfinish  = 0.
    mwyfinish  = 0.
    do ik=1, nk
      eband = 0.
      a1band = 0.
      b1band = 0.
      do ith=1, nth
        diff = specinit(ith+(ik-1)*nth)-spec(ith+(ik-1)*nth)
        eband = eband + diff
        a1band = a1band + diff*ecos(ith)
        b1band = b1band + diff*esin(ith)
      end do
      efinish  = efinish  + eband * dden(ik) / cg1(ik)
      mwxfinish  = mwxfinish  + a1band * dden(ik) / cg1(ik)        &
           * wn1(ik)/sig(ik)
      mwyfinish  = mwyfinish  + b1band * dden(ik) / cg1(ik)        &
           * wn1(ik)/sig(ik)
    end do
    !
    ! transformation in momentum flux in m^2 / s^2
    !
    tauox=(grav*mwxfinish+tauwix-taubbl(1))/dtg
    tauoy=(grav*mwyfinish+tauwiy-taubbl(2))/dtg
    tauwix=tauwix/dtg
    tauwiy=tauwiy/dtg
    tauwnx=tauwnx/dtg
    tauwny=tauwny/dtg
    taubbl(:)=taubbl(:)/dtg
    tauocx=dair*coef*coef*ustar*ustar*cos(ustdir) + dwat*(tauox-tauwix)
    tauocy=dair*coef*coef*ustar*ustar*sin(ustdir) + dwat*(tauoy-tauwiy)
    !
    ! transformation in wave energy flux in w/m^2=kg / s^3
    !
    phioc =dwat*grav*(efinish+phiaw-phibbl)/dtg
    phiaw =dwat*grav*phiaw /dtg
    phinl =dwat*grav*phinl /dtg
    phibbl=dwat*grav*phibbl/dtg
    !
    ! 10.1  adds ice scattering and dissipation: implicit integration---------------- *
    !     inflags2(4) is true if ice concentration was ever read during
    !             this simulation
    !
    if ( inflags2(4).and.ice.gt.0 ) then
      if (iicedisp) then
        icecoef2 = 1e-6
        call liu_forward_dispersion (iceh,icecoef2,depth, &
             sig,wn_r,cg_ice,alpha_liu)
        !
        if (iicesmooth) then
        end if
      else
        wn_r=wn1
        cg_ice=cg1
      end if
      !
      r(:)=1 ! in case ic2 is defined but not is2
      !
      !
      spec2 = spec
      !
      tauice(:) = 0.
      phice = 0.
      do ik=1,nk
        is = 1+(ik-1)*nth
        !
        ! first part of ice term integration: dissipation part
        !
        att=1.
        spec(1+(ik-1)*nth:nth+(ik-1)*nth) = att*spec2(1+(ik-1)*nth:nth+(ik-1)*nth)
        !
        ! second part of ice term integration: scattering including re-distribution in directions
        !
        !
        ! 10.2  fluxes of energy and momentum due to ice effects
        !
        factor = dden(ik)/cg1(ik)                    !jacobian to get energy in band
        factor2= factor*grav*wn1(ik)/sig(ik)         ! coefficient to get momentum
        do ith = 1,nth
          is = ith+(ik-1)*nth
          phice = phice + (spec(is)-spec2(is)) * factor
          cosi(1)=ecos(is)
          cosi(2)=esin(is)
          tauice(:) = tauice(:) - (spec(is)-spec2(is))*factor2*cosi(:)
        end do
      end do
      phice =-1.*dwat*grav*phice /dtg
      tauice(:)=tauice(:)/dtg
    else
    end if
    !
    !
    ! - - - - - - - - - - - - - - - - - - - - - -
    ! 11. sea state dependent stress routine calls
    ! - - - - - - - - - - - - - - - - - - - - - -
    !note the sea-state dependent stress calculations are primarily for high-wind
    !conditions (>10 m/s).  it is not recommended to use these at lower wind
    !in their current state.
    !
    ! fld1/2 requires the calculation of fpi:
    !
    !
    ! 12. includes shoreline reflection --------------------------------------------- *
    !
    !
    first  = .false.
    if (it.eq.0) spec = specinit
    spec = max(0., spec)
    !
    return
    !
    ! formats
    !
    !
    !
    !
    !/
    !/ end of w3srce ----------------------------------------------------- /
    !/
  end subroutine w3srce
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief calculate equivalent peak frequency.
  !>
  !> @details tolman and chalikov (1996), equivalent peak frequency from source.
  !>
  !> @param[in]  a    action density spectrum (1-d).
  !> @param[in]  cg   group velocities for k-axis of spectrum.
  !> @param[out]  fpi  input 'peak' frequency.
  !> @param[in] s    source term (1-d version).
  !>
  !> @author jessica meixner
  !> @date   06-jun-2018
  !>
  subroutine calc_fpi( a, cg, fpi, s )
    use constants
    use w3gdatmd, only: nk, nth, nspec, xfr, dden, sig,fte, fttr
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: a(nspec), cg(nk), s(nspec)
    real, intent(out)       :: fpi
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: is, ik
    real                    ::  m0, m1, sin1a(nk)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !     calculate fpi: equivalent peak frequncy from wind source term
    !     input
    !
    do ik=1, nk
      sin1a(ik) = 0.
      do is=(ik-1)*nth+1, ik*nth
        sin1a(ik) = sin1a(ik) + max ( 0. , s(is) )
      end do
    end do
    !
    m0     = 0.
    m1     = 0.
    do ik=1, nk
      sin1a(ik) = sin1a(ik) * dden(ik) / ( cg(ik) * sig(ik)**3 )
      m0        = m0 + sin1a(ik)
      m1        = m1 + sin1a(ik)/sig(ik)
    end do
    !
    sin1a(nk) = sin1a(nk) / dden(nk)
    m0        = m0 + sin1a(nk) * fte
    m1        = m1 + sin1a(nk) * fttr
    if ( m1 .lt. 1e-20 ) then
      fpi    = xfr * sig(nk)
    else
      fpi    = m0 / m1
    end if
  end subroutine calc_fpi
  !/ ------------------------------------------------------------------- /!
  !>
  !> @brief put source term in matrix same as done always.
  !>
  !> @param[in]    spec
  !> @param[inout] vs
  !> @param[inout] vd
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine sign_vsd_semi_implicit_ww3(spec, vs, vd)
    !
    use w3gdatmd, only : nth, nk, nspec
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
    integer             :: isp, ith, ik, is
    real, intent(in)    :: spec(nspec)
    real, intent(inout) :: vs(nspec), vd(nspec)
    do is=1,nspec
      vd(is) = min(0., vd(is))
    end do
  end subroutine sign_vsd_semi_implicit_ww3
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief put source term in matrix patankar style (experimental).
  !>
  !> @param[in]    spec
  !> @param[inout] vs
  !> @param[inout] vd
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine sign_vsd_patankar_ww3(spec, vs, vd)
    !
    use w3gdatmd, only : nth, nk, nspec
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
    integer             :: isp, ith, ik, is
    real, intent(in)    :: spec(nspec)
    real, intent(inout) :: vs(nspec), vd(nspec)
    do is=1,nspec
      vd(is) = min(0., vd(is))
      vs(is) = max(0., vs(is))
    end do
  end subroutine sign_vsd_patankar_ww3
  !/
  !/ end of module w3srcemd -------------------------------------------- /
  !/
end module w3srcemd
