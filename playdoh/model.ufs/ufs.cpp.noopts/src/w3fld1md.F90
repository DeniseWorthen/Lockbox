!/ ------------------------------------------------------------------- /
module w3fld1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii      noaa/ncep/nopp |
  !/                  |           b. g. reichl            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    01-jul-2013 : origination.                        ( version 4.xx )
  !/    18-mar-2015 : clean-up/prepare for distribution   ( version 5.12 )
  !/    15-jan-2016 : updates                             ( version 5.12 )
  !/                                                      ( b. g. reichl )
  !/    27-jul-2016 : added charnock output  (j.meixner)  ( version 5.12 )
  !/    22-jun-2018 : updated sig2wn subroutine (x.chen)  ( version 6.06 )
  !/                  modified the range of wind profile computation;
  !/                  corrected direction of the shortest waves
  !/    22-mar-2021 : consider dair a variable            ( version 7.13 )
  !/
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     this module contains routines to compute the wind stress vector
  !     from the wave spectrum, the wind vector, and the lower atmospheric
  !     stability (the form included here is for neutral conditions, but
  !     the structure needed to include stability is included in comments).
  !     the stress calculated via this subroutine is
  !     intended for coupling to serve as the boundary condition
  !     between the ocean and atmosphere, and (for now)
  !     and has no impact on the wave spectrum calculated.
  !     the calculation in w3fld1 is based on the method
  !     presented in reichl, hara, and ginis (2014), "sea state dependence
  !     of the wind stress under hurricane winds."
  !
  !  2. variables and types :
  !
  !     not applicable.
  !
  !  3. subroutines and functions :
  !
  !      name       type  scope    description
  !     ----------------------------------------------------------------
  !      w3fld1     subr. public   reichl et al. 2014 stress calculation
  !      infld1     subr. public   corresponding initialization routine.
  !      appendtail subr. public   modification of tail for calculation
  !      sig2wn     subr. public   depth-dependent dispersion relation
  !      wnd2z0m    subr. public   wind to roughness length
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
  !  6. switches :
  !
  !     !/s  enable subroutine tracing.
  !     !/
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !
  public
  ! tail_choice: chose the method to determine the level of the tail
  integer, save :: tail_choice
  real, save    :: tail_level !if tail_choice=0, tail is constant
  real, save    :: tail_transition_ratio1! freq/fpi where tail
  !  adjustment begins
  real, save    :: tail_transition_ratio2! freq/fpi where tail
  !  adjustment ends
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3fld1( aspc, fpi, wndx,wndy, zwnd,               &
       depth, rib, dair, ust, ustd, z0,               &
       taunux, taunuy, charn)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii      noaa/ncep/nopp |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    01-jul-2013 : origination.                        ( version 4.xx )
    !/    18-mar-2015 : prepare for submission              ( version 5.12 )
    !/    22-mar-2021 : consider dair a variable            ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     diagnostic stress vector calculation from wave spectrum, lower
    !     atmosphere stability, and wind vector (at some given height).
    !     the height of wind vector is assumed to be within the constant
    !     stress layer.  these parameterizations are meant to be performed
    !     at wind speeds > 10 m/s, and may not converge for extremely young
    !     seas (i.e. starting from flat sea conditions).
    !
    !  2. method :
    !     see reichl et al. (2014).
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       aspc    real   i   1-d wave action spectrum.
    !       fpi     real   i   peak input frequency.
    !       wndx    real   i   x-dir wind (assumed referenced to current)
    !       wndy    real   i   y-dir wind (assumed referenced to current)
    !       zwnd    real   i   wind height.
    !       depth   real   i   water depth.
    !       rib     real   i   bulk richardson in lower atmosphere
    !                          (for determining stability in abl to get
    !                          10 m neutral wind)
    !       dair    real   i   air density
    !       taunux  real   0   x-dir viscous stress (guessed from prev.)
    !       taunuy  real   0   y-dir viscous stress (guessed from prev.)
    !       ust     real   o   friction velocity.
    !       ustd    real   o   direction of friction velocity.
    !       z0      real   o   surface roughness length
    !       charn   real   o,optional    charnock parameter
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
    !      w3asim    subr. w3asimmd air-sea interface module.
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
    use constants, only: grav, dwat, tpi, pi, kappa
    use w3gdatmd, only: nk, nth, nspec, sig, dth, xfr, th
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: aspc(nspec), wndx, wndy,  &
         zwnd, depth, rib, dair, fpi
    real, intent(out)       :: ust, ustd, z0
    real, intent(out), optional :: charn
    real, intent(inout)     :: taunux, taunuy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real, parameter         ::  nu=0.105/10000.0
    real, parameter         ::  delta=0.03
    ! commonly used parameters
    real                    ::  wnd_in_mag, wnd_in_dir
    !for calculating tail
    real                    ::  kmax, ktaila, ktailb, ktailc
    real                    ::  sat, z01, z02, u10
    logical                 ::  iterflag
    integer                 ::  count
    !for iterations
    real                    ::  dtx, dty, iter_thresh, &
         ustsm, z0sm, z1
    !for stress calculation
    real                    ::  wage, cbeta, bp, cd,       &
         ustrb, angdif, ustar, znu, &
         taut, taux, tauy, betag, taudir, &
         taudirb
    !for wind profile calculation
    real                    ::  uprofv, vprofv
    !for wind profile iteration
    real                    ::  wnd_1x, wnd_1y, &
         wnd_2x, wnd_2y, &
         wnd_3x, wnd_3y, &
         difu10xx, difu10yx, difu10xy, difu10yy, &
         fd_a, fd_b, fd_c, fd_d, &
         dwndx, dwndy, &
         apar, ch,uitv, vitv,ustl,&
         ck
    !for adding stability to wind profile
    real                    ::  wnd_top, ang_top, wnd_pa, wnd_pe,   &
         wnd_pex, wnd_pey, wnd_pax, wnd_pay, &
         cdm
    integer                 ::  nkt, k, t, z2, iter, zi, zii, &
         i, ctr, iteration, ka1, ka2, &
         ka3, kb
    ! for defining extended spectrum with appended tail.
    real, allocatable, dimension(:)   :: wn, dwn, cp,sig2
    real, allocatable, dimension(:,:) :: spc2
    real, allocatable, dimension(:)   :: tltn, tlte, taud, &
         tltnd, &
         tlted, zofk, uprof, vprof, &
         ftilde, up1, vp1, up, vp, &
         tltna, tltea
    logical                 :: fsfl1,fsfl2, crit1, crit2
    logical                 :: it_flag1, it_flag2
    logical, save           :: first = .true.
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    !     **********************************************************
    !     ***    the initialization routine should include all   ***
    !     *** initialization, including reading data from files. ***
    !     **********************************************************
    !
    if ( first ) then
      call infld
      first  = .false.
    end if
    wnd_in_mag = sqrt( wndx**2 + wndy**2 )
    wnd_in_dir = atan2(wndy, wndx)
    !----------------------------------------------------------+
    ! assume wind input is neutral 10 m wind.  if wind input   +
    ! is not 10 m, tail level will need to be calculated based +
    ! on esimation of 10 m wind.                               +
    !----------------------------------------------------------+
    u10 = wnd_in_mag
    ! - get tail level
    if (tail_choice.eq.0) then
      sat=tail_level
    elseif (tail_choice.eq.1) then
      call wnd2sat(u10,sat)
    endif
    !
    ! 1.  attach tail ---------------------------------------------------- *
    !
    ! if the depth remains constant, the allocation could be limited to the
    !   first time step.  since this code is designed for coupled
    !   implementation where the water level can change, i keep it the
    !   allocation on every time step.  when computational efficiency is
    !   important, this process may be rethought.
    !
    ! i. find maximum wavenumber of input spectrum
    call sig2wn(sig(nk),depth,kmax)
    nkt = nk
    ! ii. find additional wavenumber bins to extended to cm scale waves
    do while ( kmax .lt. 366.0 )
      nkt = nkt + 1
      kmax = ( kmax * xfr**2 )
    enddo!k<366
    ! iii. allocate new "extended" spectrum
    allocate( wn(nkt), dwn(nkt), cp(nkt), sig2(nkt),spc2(nkt,nth), &
         tltn(nkt), tlte(nkt), taud(nkt), &
         tltnd(nkt), tlted(nkt), zofk(nkt), uprof(nkt+1),&
         vprof(nkt+1), ftilde(nkt), up1(nkt+1),vp1(nkt+1), &
         up(nkt+1), vp(nkt+1), tltna(nkt),tltea(nkt))
    !
    ! 1a. build discrete wavenumbers for defining extended spectrum on---- *
    !
    !i. copy existing sig to extended sig2, calculate phase speed.
    do k = 1, nk !existing spectrum
      call sig2wn(sig(k),depth,wn(k))
      cp(k) = ( sig(k) / wn(k) )
      sig2(k) = sig(k)
    enddo!k
    !ii. calculate extended sig2 and phase speed.
    do k = ( nk + 1 ), ( nkt) !extension
      sig2(k) = sig2(k-1) *xfr
      call sig2wn(sig2(k),depth,wn(k))
      cp(k) = sig2(k) / wn(k)
    enddo!k
    !iii. calculate dk's for integrations.
    do k = 1, nkt-1
      dwn(k) = wn(k+1) - wn(k)
    enddo
    dwn(nkt) = wn(nkt)*xfr**2 - wn(nkt)
    !
    ! 1b. attach initial tail--------------------------------------------- *
    !
    !i. convert action spectrum to variance spectrum
    !   spc(k,theta) = a(k,theta) * sig(k)
    ! this could be redone for computational efficiency
    i=0
    do k=1, nk
      do t=1, nth
        i = i + 1
        spc2(k,t) = aspc(i)  * sig(k)
      enddo!t
    enddo!k
    !ii. extend k^-3 tail to extended spectrum
    do k=nk+1, nkt
      do t=1, nth
        spc2(k,t)=spc2(nk,t)*wn(nk)**3.0/wn(k)**(3.0)
      enddo!t
    enddo!k
    !
    ! 1c. calculate transitions for new (constant saturation ) tail ------ *
    !
    !
    !i. find wavenumber for beginning spc level transition to tail
    call sig2wn (fpi*tpi*tail_transition_ratio1,depth,ktaila )
    !ii. find wavenumber for ending spc level transition to tail
    call sig2wn (fpi*tpi*tail_transition_ratio2,depth,ktailb )
    !iii. find wavenumber for ending spc direction transition to tail
    ktailc= ktailb * 2.0
    !iv. find corresponding indices of wavenumber transitions
    ka1 = 2     ! do not modify 1st wavenumber bin
    do while ( ( ktaila .ge. wn(ka1) ) .and. (ka1 .lt. nkt-6) )
      ka1 = ka1 + 1
    enddo
    ka2 = ka1+2
    do while ( ( ktailb .ge. wn(ka2) ) .and. (ka2 .lt. nkt-4) )
      ka2 = ka2 + 1
    enddo
    ka3 = ka2+2
    do while ( ( ktailc .ge. wn(ka3)) .and. (ka3 .lt. nkt-2) )
      ka3 = ka3 + 1
    enddo
    !v. call subroutine to perform actually tail truncation
    ! only if there is some energy in spectrum
    call appendtail(spc2, wn, nkt, ka1, ka2, ka3,&
         wnd_in_dir, sat)
    ! spectrum is now set for stress integration
    !
    ! 2.  prepare for iterative calculation of wave-form stress----------- *
    !
    dtx = 0.00005
    dty = 0.00005
    iter_thresh = 0.001
    !
    ! 2a. calculate initial guess for viscous stress from smooth-law------ *
    ! (would be preferable to use prev. step)
    !
    z0sm = 0.001  !guess
    it_flag1 = .true.
    iteration = 0
    do while( it_flag1 )
      iteration = iteration + 1
      z1 = z0sm
      ustsm = kappa * wnd_in_mag / ( log( zwnd / z1 ) )
      z0sm = 0.132 * nu / ustsm
      if ( (abs( z0sm - z1 ) .lt. 10.0**(-6)) .or.&
           ( iteration .gt. 5 )) then
        it_flag1 = .false.
      endif
    enddo
    iteration = 1
    ! guessed values of viscous stress
    taunux = ustsm**2 * dair * wndx / wnd_in_mag
    taunuy = ustsm**2 * dair * wndy / wnd_in_mag
    !
    ! 3.  enter iterative calculation of wave form/skin stress----------  *
    !
    it_flag1 = .true.
    do while (it_flag1)
      do iter=1, 3 !3 loops for taunu iteration
        z2 = nkt
        ! first : taunux + dx
        if (iter .eq. 1) then
          taunux = taunux + dtx
          ! second : taunuy + dy
        elseif (iter .eq. 2) then
          taunux = taunux - dtx
          taunuy = taunuy + dty
          ! third : unmodified
        elseif (iter .eq. 3) then
          taunuy = taunuy - dty
        endif
        ! near surface turbulent stress = taunu
        tltn(1) = taunuy
        tlte(1) = taunux
        call appendtail(spc2, wn, nkt, ka1, ka2, ka3,&
             atan2(taunuy,taunux), sat)
        !|---------------------------------------------------------------------|
        !|-----calculate first guess at growth rate and local turbulent stress-|
        !|-----for integration as a function of wavedirection------------------|
        !|---------------------------------------------------------------------|
        do zi = 2, nkt
          ustl=0.0
          tltnd(zi)=0.0
          tlted(zi)=0.0
          z2 = z2 - 1
          ! use value of prev. wavenumber/height
          taud(zi) = atan2( tltn(zi-1), tlte(zi-1))
          ustl = sqrt( sqrt( tltn(zi-1)**2 + tlte(zi-1)**2 )/ dair )
          do t = 1, nth
            angdif=taud(zi)-th(t) !stress/wave angle
            if ( cos( angdif ) .ge. 0.0 ) then !waves aligned
              wage = cp(z2) / (ustl)
              ! first, waves much slower than wind.
              if ( wage .lt. 10. ) then
                cbeta = 25.0
                ! transition from waves slower than wind to faster
              elseif ( ( wage .ge. 10.0 ) .and. &
                   ( wage .le. 25.0 ) ) then
                cbeta = 10.0 + 15.0 * cos( pi * ( wage - 10.0 ) &
                     / 15.0 )
                ! waves faster than wind
              elseif ( wage .gt. 25.0 ) then
                cbeta = -5.0
              endif
              ! waves opposing wind
            else
              cbeta = -25.0
            endif
            !integrate turbulent stress
            tltnd(zi) =tltnd(zi)+( sin( th(t) ) * cos( angdif )**2)&
                 * cbeta * spc2(z2,t) * &
                 sqrt( tlte(zi-1)**2 + tltn(zi-1)**2.0 ) &
                 * ( wn(z2)**2.0 )*dth
            tlted(zi) = tlted(zi)+(cos( th(t) ) * cos( angdif )**2)&
                 * cbeta * spc2(z2,t) * &
                 sqrt( tlte(zi-1)**2 + tltn(zi-1)**2.0 ) &
                 * ( wn(z2)**2.0 )*dth
          enddo
          !|---------------------------------------------------------------------|
          !|-----complete the integrations---------------------------------------|
          !|---------------------------------------------------------------------|
          if (zi .eq. 2) then
            !first turbulent stress bin above taunu
            tltna(zi) = tltnd(zi) * dwn(z2) * 0.5
            tltea(zi) = tlted(zi) * dwn(z2) * 0.5
          else
            tltna(zi)=(tltnd(zi)+tltnd(zi-1))*0.5*dwn(z2)
            tltea(zi)=(tlted(zi)+tlted(zi-1))*0.5*dwn(z2)
          endif
          tltn(zi)=tltn(zi-1)+tltna(zi)
          tlte(zi)=tlte(zi-1)+tltea(zi)
        enddo
        tauy=tltn(nkt)
        taux=tlte(nkt)
        ! this is the first guess at the stress.
        !|---------------------------------------------------------------------|
        !|----iterate til convergence------------------------------------------|
        !|---------------------------------------------------------------------|
        ustrb=sqrt(sqrt(tauy**2.0+taux**2.0)/dair)
        taudirb=atan2(tauy,taux)
        it_flag2 = .true.
        ctr=1
        do while ( (it_flag2) .and. ( ctr .lt. 10 ) )
          z2=nkt+1
          do zi=1, nkt
            z2=z2-1
            ustl=0.0
            tlted(zi)=0.0
            tltnd(zi)=0.0
            ftilde(z2)=0.0
            taud(zi) = atan2(tltn(zi),tlte(zi))
            ustl = sqrt(sqrt(tltn(zi)**2+tlte(zi)**2)/dair)
            do t=1, nth
              betag=0.0
              angdif = taud(zi)-th(t)
              if ( cos( angdif ) .ge. 0.0 ) then
                wage = cp(z2)  / (ustl)
                if ( wage .lt. 10 ) then
                  cbeta = 25.0
                elseif ( ( wage .ge. 10.0 ) .and. &
                     ( wage .le. 25.0 ) ) then
                  cbeta = 10.0 + 15.0 * cos( pi * ( wage - 10.0 ) &
                       / 15.0 )
                elseif ( wage .gt. 25.0 ) then
                  cbeta = -5.0
                endif
              else
                cbeta = -25.0
              endif
              bp = sqrt( (cos( th(t) ) * cos( angdif )**2.0)**2.0 &
                   + (sin( th(t) ) * cos( angdif )**2.0)**2.0 )
              betag=bp*cbeta*sqrt(tlte(zi)**2.0+tltn(zi)**2.0) &
                   /(dwat)*sig2(z2)/cp(z2)**2
              ftilde(z2) = ftilde(z2) + betag * dwat * grav &
                   * spc2(z2,t) * dth
              tltnd(zi) =tltnd(zi)+ (sin( th(t) ) * cos( angdif )**2.0)&
                   * cbeta * spc2(z2,t) * sqrt( &
                   tlte(zi)**2.0 + tltn(zi)**2.0 ) * &
                   ( wn(z2)**2.0 )*dth
              tlted(zi) = tlted(zi)+(cos( th(t) ) * cos( angdif )**2.0)&
                   * cbeta * spc2(z2,t) * sqrt( &
                   tlte(zi)**2.0 + tltn(zi)**2.0 ) * &
                   ( wn(z2)**2.0 )*dth
            enddo
            if (zi .eq. 1) then
              tltna(zi)=tltnd(zi)*dwn(z2)*0.5
              tltea(zi)=tlted(zi)*dwn(z2)*0.5
            else
              tltna(zi)=(tltnd(zi)+tltnd(zi-1))*0.5*dwn(z2)
              tltea(zi)=(tlted(zi)+tlted(zi-1))*0.5*dwn(z2)
            endif
            if (zi.gt.1) then
              tltn(zi)=tltn(zi-1)+tltna(zi)
              tlte(zi)=tlte(zi-1)+tltea(zi)
            else
              tltn(zi)=taunuy+tltna(zi)
              tlte(zi)=taunux+tltea(zi)
            endif
          enddo
          tauy=tltn(nkt) !by nkt full stress is entirely
          taux=tlte(nkt) !from turbulent stress
          taut=sqrt(tauy**2.0+taux**2.0)
          ustar=sqrt(sqrt(tauy**2.0+taux**2.0)/dair)
          taudir=atan2(tauy, taux)
          ! note: add another criterion (stress direction) for iteration.
          crit1=(abs(ustar-ustrb)*100.0)/((ustar+ustrb)*0.5) .gt. 0.1
          crit2=(abs(taudir-taudirb)*100.0/(taudir+taudirb)*0.5) .gt. 0.1
          if (crit1 .or. crit2) then
            !            if ((abs(ustar-ustrb)*100.0)/((ustar+ustrb)*0.5) .gt. 0.1) then
            ustrb=ustar
            taudirb=taudir
            ctr=ctr+1
          else
            it_flag2 = .false.
          endif
        enddo
        ! note: search for the top of wbl from top to bottom (avoid problems
        ! caused by  for very long swell)
        kb=nkt
        do while(((tltn(kb)**2+tlte(kb)**2)/(taux**2+tauy**2)).gt. &
             .99)
          kb=kb-1
        enddo
        kb=kb+1
        !|---------------------------------------------------------------------|
        !|----now begin work on wind profile-----------------------------------|
        !|---------------------------------------------------------------------|
        do i=1,nkt
          zofk(i)=delta/wn(i)
        enddo
        znu=0.1 * 1.45e-5 / sqrt(sqrt(taunux**2.0+taunuy**2.0)/dair)
        uprof(1:nkt+1)=0.0
        vprof(1:nkt+1)=0.0
        uprofv=0.0
        vprofv=0.0
        zi=1
        z2=nkt
        up1(zi) = ( ( ( wn(z2)**2 / delta ) * ftilde(z2) ) + &
             ( dair / ( zofk(z2) * kappa ) ) * ( sqrt( &
             tltn(zi)**2 + tlte(zi)**2 ) / dair )**(3/2) ) &
             * ( tlte(zi) ) / ( tlte(zi) * taux &
             + tltn(zi) * tauy )
        vp1(zi) = ( ( ( wn(z2)**2 / delta ) * ftilde(z2) ) + &
             ( dair / ( zofk(z2) * kappa ) ) * ( sqrt ( &
             tltn(zi)**2 + tlte(zi)**2 ) / dair )**(3/2) ) &
             * ( tltn(zi) ) / ( tlte(zi) * taux &
             + tltn(zi) * tauy )
        up(zi) = up1(zi)
        vp(zi) = vp1(zi)
        uprof(zi) = dair / kappa * ( sqrt( taunux**2.0 + taunuy**2.0 ) &
             / dair )**(1.5) * ( taunux / ( taux * &
             taunux + tauy * taunuy ) ) * log( &
             zofk(z2) / znu )
        vprof(zi) = dair / kappa * ( sqrt( taunux**2.0 + taunuy**2.0 ) &
             / dair )**(1.5) * ( taunuy / ( taux * &
             taunux + tauy * taunuy ) ) * log( &
             zofk(z2) / znu )
        !noted: wind profile computed till the inner layer height of the longest
        !wave, not just to the top of wave boundary layer (previous)
        do zi=2, nkt
          z2 = z2 - 1
          up1(zi) = ( ( ( wn(z2)**2.0 / delta ) * ftilde(z2) ) + &
               ( dair / ( zofk(z2) * kappa ) ) * ( sqrt( &
               tltn(zi)**2.0 + tlte(zi)**2.0 ) / dair )**(1.5) ) &
               * ( tlte(zi) ) / ( tlte(zi) * taux + &
               tltn(zi) * tauy )
          vp1(zi) = ( ( ( wn(z2)**2.0 / delta ) * ftilde(z2) ) + &
               ( dair / ( zofk(z2) * kappa ) ) * ( sqrt( &
               tltn(zi)**2.0 + tlte(zi)**2.0 ) / dair )**(1.5) ) &
               * ( tltn(zi) ) / ( tlte(zi) * taux + &
               tltn(zi) * tauy )
          up(zi) = up1(zi) * 0.5 + up1(zi-1) * 0.5
          vp(zi) = vp1(zi) * 0.5 + vp1(zi-1) * 0.5
          uprof(zi) = uprof(zi-1) + up(zi) * ( zofk(z2) - zofk(z2+1) )
          vprof(zi) = vprof(zi-1) + vp(zi) * ( zofk(z2) - zofk(z2+1) )
        enddo
        !|---------------------------------------------------------------------|
        !|----iteration completion/checks--------------------------------------|
        !|---------------------------------------------------------------------|
        !zi = ( kb + 1 )
        ! now solving for 'zwnd' height wind
        uprof(nkt+1) = uprof(nkt) + ( sqrt( sqrt( tauy**2.0 + &
             taux**2.0 ) / dair ) ) / kappa * taux &
             / sqrt( tauy**2.0 +taux**2.0 ) * log( zwnd &
             / zofk(z2) )
        vprof(nkt+1) = vprof(nkt) + ( sqrt( sqrt( tauy**2.0 + &
             taux**2.0 ) / dair ) ) / kappa * tauy &
             / sqrt( tauy**2.0 +taux**2.0 ) * log( zwnd &
             / zofk(z2) )
        if (iter .eq. 3) then
          wnd_1x = uprof(nkt+1)
          wnd_1y = vprof(nkt+1)
        elseif (iter .eq. 2) then
          wnd_2x = uprof(nkt+1)
          wnd_2y = vprof(nkt+1)
        elseif (iter .eq. 1) then
          wnd_3x = uprof(nkt+1)
          wnd_3y = vprof(nkt+1)
        endif
        !-------------------------------------+
        !  guide for adding stability effects +
        !-------------------------------------+
        !get wind at top of wave boundary layer
        ! wnd_top=sqrt(uprof(kb)**2+vprof(kb)**2)
        ! get wind angle at top of wave boundary layer
        ! ang_top=atan2(vprof(kb),uprof(kb))
        ! stress and direction
        ! ustd = atan2(tauy,taux)
        ! ust = sqrt( sqrt( taux**2 + tauy**2 ) / dair)
        ! calclate along (pa) and across (pe) wind components
        ! wnd_pa=wnd_top*cos(ang_top-ustd)
        ! wnd_pe=wnd_top*sin(ang_top-ustd)
        ! calculate cartesian aligned wind
        ! wnd_pax=wnd_pa*cos(ustd)
        ! wnd_pay=wnd_pa*sin(ustd)
        !calculate cartesion across wind
        ! wnd_pex=wnd_pe*cos(ustd+pi/2.)
        ! wnd_pey=wnd_pe*sin(ustd+pi/2.)
        !----------------------------------------------------+
        ! if a non-neutral profile is used the effective z0  +
        ! should be computed.  this z0 can then be used      +
        ! with stability information to derive a cd, which   +
        ! can be used to project the along-stress wind to    +
        ! the given height.                                  +
        ! i.e.: assume neutral inside wbl calculate z0       +
        ! z0=zofk(z2)*exp(-wnd_pa*kappa/ust)                 +
        ! wnd_pa=ust/sqrt(cdm)                               +
        !----------------------------------------------------+
        ! wnd_pax=wnd_pa*cos(ustd)
        ! wnd_pay=wnd_pa*sin(ustd)
        ! if (iter .eq. 3) then
        !   wnd_1x = wnd_pax+wnd_pex
        !   wnd_1y = wnd_pay+wnd_pey
        ! elseif (iter .eq. 2) then
        !   wnd_2x = wnd_pax+wnd_pex
        !   wnd_2y = wnd_pay+wnd_pey
        ! elseif (iter .eq. 1) then
        !   wnd_3x = wnd_pax+wnd_pex
        !   wnd_3y = wnd_pay+wnd_pey
        ! endif
      enddo
      iteration = iteration + 1
      difu10xx = wnd_3x - wnd_1x
      difu10yx = wnd_3y - wnd_1y
      difu10xy = wnd_2x - wnd_1x
      difu10yy = wnd_2y - wnd_1y
      fd_a = difu10xx / dtx
      fd_b = difu10xy / dty
      fd_c = difu10yx / dtx
      fd_d = difu10yy / dty
      dwndx = - wndx + wnd_1x
      dwndy = - wndy + wnd_1y
      uitv = abs( dwndx )
      vitv = abs( dwndy )
      ch = sqrt( uitv**2.0 + vitv**2.0 )
      if (ch .gt. 15.) then
        apar = 0.5 / ( fd_a * fd_d - fd_b * fd_c )
      else
        apar = 1.0 / ( fd_a * fd_d - fd_b * fd_c )
      endif
      ck=4.
      if (((vitv/max(abs(wndy),ck) .gt. iter_thresh) .or. &
           (uitv/max(abs(wndx),ck) .gt. iter_thresh)) .and. &
           (iteration .lt. 2)) then
        taunux = taunux - apar * ( fd_d * dwndx - fd_b * dwndy )
        taunuy = taunuy - apar * ( -fd_c * dwndx +fd_a * dwndy )
      elseif (((vitv/max(abs(wndy),ck) .gt. iter_thresh) .or. &
           (uitv/max(abs(wndx),ck) .gt. iter_thresh)) .and. &
           (iteration .lt. 24)) then
        iter_thresh = 0.001
        taunux = taunux - apar * ( fd_d * dwndx - fd_b * dwndy )
        taunuy = taunuy - apar * ( -fd_c * dwndx +fd_a * dwndy )
      elseif (((vitv/max(abs(wndy),ck) .gt. iter_thresh) .or. &
           (uitv/max(abs(wndx),ck) .gt. iter_thresh)) .and. &
           (iteration .lt. 26)) then
        iter_thresh = 0.01
        taunux = taunux - apar * ( fd_d * dwndx - fd_b * dwndy )
        taunuy = taunuy - apar * ( -fd_c * dwndx +fd_a * dwndy )
      elseif (((vitv/max(abs(wndy),ck) .gt. iter_thresh) .or. &
           (uitv/max(abs(wndx),ck) .gt. iter_thresh)) .and. &
           (iteration .lt. 30)) then
        iter_thresh = 0.05
        taunux = taunux - apar * ( fd_d * dwndx - fd_b * dwndy )
        taunuy = taunuy - apar * ( -fd_c * dwndx +fd_a * dwndy )
      elseif (iteration .ge. 30) then
        write(*,*)'attn: w3fld1 not converged.'
        write(*,*)'      wind (x/y): ',wndx,wndy
        it_flag1 = .false.
        ust=-999
        taunux=0.
        taunuy=0.
      elseif (((vitv/max(abs(wndy),ck) .lt. iter_thresh) .and.&
           (uitv/max(abs(wndx),ck) .lt. iter_thresh)) .and. &
           (iteration .ge. 2)) then
        it_flag1 = .false.
      endif
      ! if taunu iteration is unstable try to reset with new guess...
      if (.not.(cos(wnd_in_dir-atan2(taunuy,taunux)).ge.0.0)) then
        taunux = ustsm**2 * dair * wndx / wnd_in_mag*.95
        taunuy = ustsm**2 * dair * wndy / wnd_in_mag*.95
      endif
    enddo
    !|---------------------------------------------------------------------|
    !|----finish-----------------------------------------------------------|
    !|---------------------------------------------------------------------|
    ustd = atan2(tauy,taux)
    ust = sqrt( sqrt( taux**2 + tauy**2 ) / dair)
    ! get z0 from aligned wind
    wnd_pa=wnd_in_mag*cos(wnd_in_dir-ustd)
    z0 = zwnd/exp(wnd_pa*kappa/ust)
    cd = ust**2 / wnd_in_mag**2
    if (present(charn)) then
      charn = 0.01/sqrt(sqrt( taunux**2 + taunuy**2 )/(ust**2))
    endif
    fsfl1=.not.((cd .lt. 0.01).and.(cd .gt. 0.0001))
    fsfl2=.not.(cos(wnd_in_dir-ustd).gt.0.9)
    if (fsfl1 .or. fsfl2) then
      !fail safe to bulk
      write(*,*)'attn: w3fld1 failed, will output bulk...'
      call wnd2z0m(wnd_in_mag,z0)
      ust = wnd_in_mag*kappa/log(zwnd/z0)
      ustd = wnd_in_dir
      cd = ust**2 / wnd_in_mag**2
    endif
    deallocate(wn, dwn, cp,sig2, spc2, tltn, tlte, taud, &
         tltnd, tlted, zofk, uprof, &
         vprof, ftilde, up1, vp1, up, vp, tltna, tltea)
    !/ end of w3fld1 ----------------------------------------------------- /
    !/
    return
    !
  end subroutine w3fld1
  !/ ------------------------------------------------------------------- /
  subroutine infld
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-2016 : origination.                        ( version 5.12 )
    !/
    !  1. purpose :
    !
    !     initialization for w3fld1 (also used by w3fld2)
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
    !      w3fldx    subr. w3fldxmd corresponding source term.
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
    use w3odatmd, only: ndse
    use w3gdatmd, only: tail_id, tail_lev, tail_tran1, tail_tran2
    use w3servmd, only: extcde
    !/
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
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  .... ----------------------------------------------------------- *
    !
    tail_choice=tail_id
    tail_level=tail_lev
    tail_transition_ratio1 = tail_tran1
    tail_transition_ratio2 = tail_tran2
    !
    return
    !
    ! formats
    !
    !/
    !/ end of infld1 ----------------------------------------------------- /
    !/
  end subroutine infld
  !/
  !/ ------------------------------------------------------------------- /
  subroutine appendtail(inspc, wn2, nkt, ka1, ka2, ka3, wnddir,sat)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         15-jan-2016 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-2016 : origination.                        ( version 5.12 )
    !/
    !  1. purpose :
    !
    !     set tail for stress calculation.
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
    !      w3fld1    subr. w3fld1md corresponding source term.
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
    use constants, only: tpi, pi
    use w3gdatmd, only: nth, th, dth
    use w3odatmd, only: ndse
    use w3servmd, only: extcde
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in) :: nkt, ka1, ka2, ka3
    real, intent(in)    :: wn2(nkt), wnddir,sat
    real, intent(inout)   :: inspc(nkt,nth)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real                :: bt(nkt), ic, angle2, ang(nkt),&
         normspc(nth), avg, angdif, m, maxang, &
         maxan, minan
    integer             :: mai, i, k, t
    real, allocatable, dimension(:)  :: angle1
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 1.  .... ----------------------------------------------------------- *
    !
    !|###############################################################|
    !|##1. get the level of the saturation spectrum in transition
    !|##   region a
    !|###############################################################|
    !-------------------------------------------
    ! 1a, get saturation level at ka1 (1.25xfpi)
    !-------------------------------------------
    bt(ka1) = 0
    ang = 0.0
    do t=1, nth
      bt(ka1)=bt(ka1)+inspc(ka1,t)*wn2(ka1)**3.0*dth
    enddo
    !-----------------------------------------------
    ! 1b, set saturation level at ka2 (3xfpi) to sat
    !-----------------------------------------------
    bt(ka2) = sat
    !-------------------------------------------------------------
    ! 1c, find slope of saturation spectrum in transition region a
    !-------------------------------------------------------------
    m = ( bt(ka2) - bt(ka1) ) / ( wn2(ka2) - wn2(ka1) )
    !----------------------------------------------------------------
    ! 1d, find intercept of saturation spectrum in transition region
    !     a
    !----------------------------------------------------------------
    ic = bt(ka1) - m * wn2(ka1)
    !------------------------------------------------------
    ! 1e, calculate saturation level for all wavenumbers in
    !     transition region a
    !------------------------------------------------------
    do k=ka1,ka2
      bt(k)=m*wn2(k)+ic
    enddo
    !|###############################################################|
    !|##2. determine the directionality at each wavenumber in
    !|##   transition region b
    !|###############################################################|
    !-----------------------------------------------
    ! 2a, find angle of spectral peak at ka2 (3xfpi)
    !-----------------------------------------------
    maxang = 0.0
    do t=1, nth
      if (inspc(ka2,t) .gt. maxang) then
        maxang=inspc(ka2,t)
      endif
    enddo
    !-------------------------------
    ! 2b, check if peak spans 2 bins
    !-------------------------------
    !mai = total number of angles of peak (if it spans more than 1)
    mai = 0
    do t=1, nth
      if (maxang .eq. inspc(ka2,t)) then
        mai = mai+1
      endif
    enddo
    !angle1 = angles that correspond to peak (array)
    mai = max(1,mai)
    allocate(angle1(mai))
    !-----------------------------------------------------
    ! 2c, if peak spans 2 or more bins it must be averaged
    !-----------------------------------------------------
    k=1
    do t=1, nth
      if (maxang .eq. inspc(ka2,t)) then
        angle1(k) = th(t)
        k=k+1
      endif
    enddo
    do k=1, mai
      do while (angle1(k) .lt. 0.0)
        angle1(k) = angle1(k) + tpi
      enddo
      do while (angle1(k) .ge. tpi)
        angle1(k) = angle1(k) - tpi
      enddo
    enddo
    if (mai .gt. 1) then
      maxan = angle1(1)
      minan = angle1(1)
      do i=2, mai
        if (maxan .lt. angle1(i) )then
          maxan = angle1(i)
        endif
        if (minan .gt. angle1(i) )then
          minan = angle1(i)
        endif
      enddo
      !------------------------------------------------------
      !  need to distinguish if mean cross the origin (0/2pi)
      !------------------------------------------------------
      if (maxan-minan .gt. pi) then
        do i=1, mai
          if (maxan - angle1(i) .gt. pi) then
            angle1(i) = angle1(i) + tpi
          endif
        enddo
        angle2=sum(angle1)/max(real(mai),1.)
      else
        angle2=sum(angle1)/max(real(mai),1.)
      endif
    else
      angle2=angle1(1)
    endif
    do while (angle2 .lt. 0.0)
      angle2 = angle2 + tpi
    enddo
    do while (angle2 .ge. tpi)
      angle2 = angle2 - tpi
    enddo
    !
    !---------------------------------------------------
    ! this deals with angles that are less than 90
    !---------------------------------------------------
    if (cos(angle2-wnddir) .ge. 0.) then  !less than 90
      m=asin(sin(wnddir-angle2))/(wn2(ka3)-wn2(ka2))
      ic=angle2
      do k=ka2, ka3
        ang(k)=ic +m*(wn2(k)-wn2(ka2))
      enddo
    else
      !----------------------------------------------------
      !  this deals with angels that turn clockwise
      !----------------------------------------------------
      if (sin(wnddir-angle2).ge.0) then
        m=acos(cos(wnddir-angle2))/(wn2(ka3)-wn2(ka2))
        ic=angle2
        do k=ka2, ka3
          ang(k)=ic+m*(wn2(k)-wn2(ka2))
        enddo
      else
        !-----------------------------------------------------
        !  this deals with angels that cross counter-clockwise
        !-----------------------------------------------------
        m=acos(cos(wnddir-angle2))/(wn2(ka3)-wn2(ka2))
        ic=angle2
        do k=ka2, ka3
          ang(k)=ic-m*(wn2(k)-wn2(ka2))
        enddo
      endif
    endif
    !----------------------------------------------
    ! region a, saturation level decreased linearly
    ! while direction is maintained
    !----------------------------------------------
    do k=ka1, ka2-1
      avg=sum(inspc(k,:))/max(real(nth),1.)
      do t=1,nth
        if (avg /= 0.0) then
          inspc(k,t)=bt(k)*inspc(k,t)/tpi/(wn2(k)**3.0)/avg
        else
          inspc(k,t) = 0.0
        end if
      enddo
    enddo
    !-----------------------------------------------------------
    ! region b, saturation level left flat while spectrum turned
    ! to direction of wind
    !-----------------------------------------------------------
    do k = ka2, ka3
      do t=1, nth
        angdif=th(t)-ang(k)
        if (cos(angdif) .gt. 0.0) then
          normspc(t) = cos(angdif)**2.0
        else
          normspc(t)=0.0
        endif
      enddo
      avg=sum(normspc)/max(real(nth),1.)
      do t=1, nth
        if (avg /= 0.0) then
          inspc(k,t) = sat * normspc(t)/tpi/(wn2(k)**3.0)/avg
        else
          inspc(k,t) = 0.0
        end if
      enddo
    enddo
    do t=1, nth
      angdif=th(t)-wnddir
      if (cos(angdif) .gt. 0.0) then
        normspc(t) = cos(angdif)**2.0
      else
        normspc(t) = 0.0
      endif
    enddo
    avg=sum(normspc)/max(real(nth),1.)!1./4.
    do k=ka3+1, nkt
      do t=1, nth
        if (avg /= 0.0) then
          inspc(k,t)=normspc(t)*(sat)/tpi/(wn2(k)**3.0)/avg
        else
          inspc(k,t) = 0.0
        end if
      enddo
    enddo
    deallocate(angle1)
    !
    ! formats
    !
    !/
    !/ end of appendtail ----------------------------------------------------- /
    !/
    return
    !
  end subroutine appendtail
  !/ ------------------------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine sig2wn(sig,depth,wn)
    !/ ------------------------------------------------------------------- /
    !author: brandon reichl (gso/uri)
    !origination  : 2013
    !update       : march - 18 - 2015
    !             : june -22 -2018  (xyc)
    !puropse      : convert from angular frequency to wavenumber
    !               using full gravity wave dispersion relation
    !               if tanh(kh)<0.99, otherwise uses deep-water
    !               approximation.
    !note: may be a better version internal to ww3 that can replace this.
    !      improved by using newton's method for iteration.(2018)
    !/ ------------------------------------------------------------------- /
    !/
    use constants, only: grav
    !/
    implicit none
    !/
    real,intent(in)    :: sig,depth
    real,intent(out)   :: wn
    !/
    real    :: wn1,wn2 !,sig1,sig2,dsigdk
    real    :: fk, fk_slp
    integer :: i
    logical :: switch
    !/ ------------------------------------------------------------------- /
    wn1=sig**2/grav
    switch=.true.
    !/ updated code with newton's method by xyc:
    if (tanh(wn1*depth) .lt. 0.99) then
      do while (switch)
        fk=grav*wn1*tanh(wn1*depth) - sig**2
        fk_slp = grav*tanh(wn1*depth) + grav*wn1*depth/(cosh(wn1*depth))**2
        wn2=wn1 - fk/fk_slp
        if (abs(wn2-wn1)/wn1 .lt. 0.0001 ) then
          switch = .false.
        else
          wn1=wn2
        endif
      enddo
    else
      wn2=wn1
    endif
    wn=wn2
    !/ end of update
    !/
    !/ previous code by br:
    !/ ------------------------------------------------------------------- /
    !        wn1=sig**2/grav
    !        wn2=wn1+0.00001
    !        switch=.true.
    !/ ------------------------------------------------------------------- /
    !        if (tanh(wn1*depth).lt.0.99) then
    !           do i=1,5
    !              if (switch) then
    !                 sig1=sqrt(grav*wn1*tanh(wn1*depth))
    !                 sig2=sqrt(grav*wn2*tanh(wn2*depth))
    !                 if (sig1.lt.sig*.99999.or.sig1.gt.sig*1.00001) then
    !                    dsigdk=(sig2-sig1)/(wn2-wn1)
    !                    wn1=wn1+(sig2-sig1)/dsigdk
    !                    wn2=wn1+wn1*0.00001
    !                 else
    !                    switch = .false.
    !                 endif
    !              endif
    !           enddo
    !        endif
    !/
    !        wn=wn1
    !/
    return
  end subroutine sig2wn
  !/ ------------------------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  subroutine wnd2z0m( w10m , znotm )
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-aug-2016 |
    !/                  +-----------------------------------+
    !/
    !/    09-apr-2014 : last update.                        ( version 5.12 )
    !/    15-aug-2016 : updated for 2016 hwrf z0            ( j. meixner   )
    !/
    !  1. purpose :
    !
    !     get bulk momentum z0 from 10-m wind.
    !          bulk stress corresponds to 2015 gfdl hurricane model
    !          not published yet, but contact brandon reichl or
    !          isaac ginis (univ. of rhode island) for further info
    !
    !  2. method :
    !          this has now been updated for 2016 hwrf z0 using routines
    !          from hwrf  znot_m_v1, biju thomas, 02/07/2014
    !           and       znot_wind10m weiguo wang, 02/24/2016
    !
    !  3. parameters :
    !       name  unit  type      description
    !     ----------------------------------------------------------------
    !       w10m   m/s  input    10 m neutral wind [m/s]
    !       znotm  m    output   roughness scale for momentum
    !     ----------------------------------------------------------------
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
    !      w3fld1    subr. w3fld1md corresponding source term.
    !      w3fld2    subr. w3fld2md corresponding source term.
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
    !/
    implicit none
    real, intent(in) :: w10m
    real, intent(out):: znotm
    !parameters from znot_m_v1
    real, parameter :: bs0 = -8.367276172397277e-12
    real, parameter :: bs1 = 1.7398510865876079e-09
    real, parameter :: bs2 = -1.331896578363359e-07
    real, parameter :: bs3 = 4.507055294438727e-06
    real, parameter :: bs4 = -6.508676881906914e-05
    real, parameter :: bs5 = 0.00044745137674732834
    real, parameter :: bs6 = -0.0010745704660847233
    real, parameter :: cf0 = 2.1151080765239772e-13
    real, parameter :: cf1 = -3.2260663894433345e-11
    real, parameter :: cf2 = -3.329705958751961e-10
    real, parameter :: cf3 = 1.7648562021709124e-07
    real, parameter :: cf4 = 7.107636825694182e-06
    real, parameter :: cf5 = -0.0013914681964973246
    real, parameter :: cf6 = 0.0406766967657759
    !variables from znot_wind10m
    real            :: z10, u10,aaa,tmp
    !values as set in znot_wind10m
    z10=10.0
    u10=w10m
    if (u10 > 85.0) u10=85.0
    !calculation of z0 as in znot_m_v1
    if ( u10 .le. 5.0 ) then
      znotm = (0.0185 / 9.8*(7.59e-4*u10**2+2.46e-2*u10)**2)
    elseif (u10 .gt. 5.0 .and. u10 .lt. 10.0) then
      znotm =.00000235*(u10**2 - 25 ) + 3.805129199617346e-05
    elseif ( u10 .ge. 10.0  .and. u10 .lt. 60.0) then
      znotm = bs6 + bs5*u10 + bs4*u10**2 + bs3*u10**3 + bs2*u10**4 +&
           bs1*u10**5 + bs0*u10**6
    else
      znotm = cf6 + cf5*u10 + cf4*u10**2 + cf3*u10**3 + cf2*u10**4 +&
           cf1*u10**5 + cf0*u10**6
    end if
    !modifications as in znot_wind10m for icoef_sf=4
    !for wind<20, cd similar to icoef=2 at 10m, then reduced
    tmp=0.4*0.4/(alog(10.0/znotm))**2   ! cd at zlev
    aaa=0.75
    if (u10 < 20) then
      aaa=0.99
    elseif(u10 < 45.0) then
      aaa=0.99+(u10-20)*(0.75-0.99)/(45.0-20.0)
    end if
    znotm=z10/exp( sqrt(0.4*0.4/(tmp*aaa)) )
  end subroutine wnd2z0m
  !/ ------------------------------------------------------------------- /
  subroutine wnd2sat(wnd10,sat)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-aug-2016 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-2016 : origination.                        ( version 5.12 )
    !/    04-aug-2016 : updated for 2016 hwrf cd/u10 curve  ( j. meixner   )
    !/
    !  1. purpose :
    !
    !     gives level of saturation spectrum to produce
    !         equivalent cd as in wnd2z0m (for neutral 10m wind)
    !         tuned for method of reichl et al. 2014
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !input: wnd10 - 10 m neutral wind [m/s]
    !output: sat  - level 1-d saturation spectrum in tail [non-dim]
    !     ----------------------------------------------------------------
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
    !      w3fld1    subr. w3fld1md corresponding source term.
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
    !/
    implicit none
    !/
    real, intent(in) :: wnd10
    real, intent(out) :: sat
    !/ old hwrf 2015 and st2
    !        sat =0.000000000001237 * wnd10**6 +&
    !             -0.000000000364155 * wnd10**5 +&
    !             0.000000037435015 * wnd10**4 +&
    !             -0.000001424719473 * wnd10**3 +&
    !             0.000000471570975 * wnd10**2 +&
    !             0.000778467452178 * wnd10**1 +&
    !             0.002962335390055
    !
    !     sat values based on
    !     hwrf 2016 cd curve, created with  fetch limited cases st4 physics
    if (wnd10<20.0) then
      sat = -0.000018541921682*wnd10**2  &
           +0.000751515452434*wnd10     &
           +0.002466529381004
    elseif (wnd10<45) then
      sat = -0.000000009060349*wnd10**4  &
           +0.000001276678367*wnd10**3  &
           -0.000068274393789*wnd10**2  &
           +0.001418180888868*wnd10     &
           +0.000262277682984
    else
      sat = -0.000155976275073*wnd10     &
           +0.012027763023184
    endif
    sat = min(max(sat,0.002),0.014)
  end subroutine wnd2sat
  !
  !/ end of module w3fld1md -------------------------------------------- /
  !/
end module w3fld1md
