!/ ------------------------------------------------------------------- /
module w3fld2md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii      noaa/ncep/nopp |
  !/                  |           b. g. reichl            |
  !/                  |                        fortran 90 |
  !/                  | last update :         22-mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    01-jul-2013 : origination                         (version 3.14)
  !/    16-may-2014 : finalizing                          (version 4.18)
  !/    19-mar-2015 : extending for non-10 m winds        (version 5.12)
  !/    27-jul-2016 : added charnock output (j.meixner)   (version 5.12)
  !/    22-jun-2018 : minor modification for application in shallow water.
  !/                                        (x.chen)      (version 6.06)
  !/    22-mar-2021 : consider dair a variable            ( version 7.13 )
  !/
  !/    copyright 2009 national weather service (nws),
  !/       national oceanic and atmospheric administration.  all rights
  !/       reserved.  wavewatch iii is a trademark of the nws.
  !/       no unauthorized use without permission.
  !/
  !  1. purpose :
  !
  !     this section of code has been designed to compute the wind
  !     stress vector from the wave spectrum, the wind speed
  !     vector, and the lower atmosphere stability.
  !     this code is based on the 2012 jgr paper, "modeling waves
  !     and wind stress" by donelan, curcic, chen, and magnusson.
  !
  !  2. variables and types :
  !
  !     not applicable
  !
  !  3. subroutines and functions :
  !
  !      name      type  scope    description
  !     ----------------------------------------------------------------
  !      w3fld2    subr. public   donelan et al. 2012 stress calculation
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name       type  module    description
  !     ----------------------------------------------------------------
  !      strace     subr. w3servmd  subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
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
  subroutine w3fld2(  aspc,fpi, wndx,wndy, zwnd,                 &
       depth, rib, dair, ust, ustd, z0, taunux,taunuy,charn)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii      noaa/ncep/nopp |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         22-mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    01-jul-2013 : origination                         (version 3.14)
    !/    19-mar-2015 : clean-up for submission             (version 5.12)
    !/    22-mar-2021 : consider dair a variable            ( version 7.13 )
    !/
    !  1. purpose :
    !
    !     wind stress vector calculation from wave spectrum and
    !        n-meter wind speed vector.
    !
    !  2. method :
    !     see donelan et al. (2012).
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
    !       rib     real   i   bulk richardson number in lower atm
    !       dair    real   i   air density
    !       taunux  real   0   x-dir viscous stress (guessed from prev.)
    !       taunuy  real   0   y-dir viscous stress (guessed from prev.)
    !       ust     real   o   friction velocity.
    !       ustd    real   o   direction of friction velocity.
    !       z0      real   o   surface roughness length
    !       charn   real   o,optional   charnock parameter
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      strace    subr. w3servmd subroutine tracing.
    !      appendtail subr. w3fld1md  modification of tail for calculation
    !      sig2wn     subr. w3fld1md  depth-dependent dispersion relation
    !      mflux      subr. w3fld1md  mo stability correction
    !      wnd2z0m    subr. w3fld1md  bulk z0 from wind
    !      calc_fpi   subr. w3fld1md  calculate peak frequency
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
    use constants, only: dwat, grav, tpi, pi, kappa
    use w3gdatmd,  only: nk, nth, nspec, sig, dth, xfr, th
    use w3odatmd,  only: ndse
    use w3servmd,  only: extcde
    use w3fld1md,  only: appendtail,sig2wn,wnd2z0m,infld,tail_choice,&
         tail_level, tail_transition_ratio1,         &
         tail_transition_ratio2
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: aspc(nspec), wndx, wndy, &
         zwnd, depth, rib, dair, fpi
    real, intent(out)       :: ust, ustd, z0
    real, intent(out),optional :: charn
    real, intent(inout)     :: taunux, taunuy
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !-parameters
    real, parameter  :: nu=0.105/10000.0
    !-commonly used values
    real :: uref, urefd
    !-tail
    real :: sat
    real :: kmax, ktaila, ktailb, ktailc
    integer :: ka1, ka2, ka3, nkt
    !-extended spectrum
    real, allocatable, dimension(:)   :: wn, dwn, cp, sig2,tauintx, tauinty
    real, allocatable, dimension(:,:) :: spc2
    !-stress calculation
    integer :: k, t, its
    real :: tauxw, tauyw, taux, tauy
    real :: ustra, ustrb, ustsm
    real :: a1, scin
    real :: cd, cdf, cds
    real :: wnd_z, wnd_z_mag, wnd_z_proj, wnd_effect
    ! stress iteration
    real :: b1, b2
    real :: ustri1, ustrf1, ustri2, ustrf2
    real :: ustgra, slo
    logical :: ust_it_flg(2)
    !-z0 iteration
    real :: z01,z02
    !-wind iteration
    real :: wnd_10_x, wnd_10_y, wnd_10_mag, wnd_10_dir
    real :: u35_1, v35_1, u35_2, v35_2, u35_3, v35_3
    real :: difu10xx, difu10yx, difu10xy, difu10yy
    real :: fd_a, fd_b, fd_c, fd_d
    real :: du, dv, uitv, vitv, ch
    real :: apar, dtx(3), dty(3), dt
    logical :: wiflg, wnd_it_flg
    !-mo stability correction
    logical :: heightflg
    integer :: wi_count, wi
    real :: wnd_ref_al,wnd_ref_ax
    real :: wndpa, wndpax, wndpay, wndpe,wndpex, wndpey
    logical :: no_err
    logical :: iterflag
    integer :: ittot
    integer :: count
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
    !----------------------------|
    ! calculate reference height |
    !  wind magnitude            |
    !----------------------------|
    uref=sqrt(wndx**2+wndy**2)
    urefd=atan2(wndy,wndx)
    !----------------------------------------------|
    ! check if wind height not equal to 10 m       |
    !----------------------------------------------|
    !heightflg = (abs(zwnd-10.).gt.0.1) ! true if not 10m
    !----------------------------------------------|
    ! assume bulk and calculate 10 m wind guess for|
    ! defining tail level                          |
    !----------------------------------------------|
    call wnd2z0m(uref,z01)  ! first guess at z0
    wnd_10_mag=uref
    ittot=1
    ! if input wind is not 10m, solve for approx 10 m
    !-------------------------------------------------|
    ! if height != 10 m, then iterate to get 10 m wind|
    ! (assuming neutral -- this is just a guess)      |
    !-------------------------------------------------|
    !if (heightflg) then
    !   iterflag=.true.
    !   count = 1 !count is now counting iteration over z0
    !   do while(iterflag)
    !      wnd_10_mag=uref*log(10./z01)/log(zwnd/z01)
    !      call wnd2z0m(wnd_10_mag,z02)
    !      if ( (abs(z01/z02-1.).gt.0.001) .and. &
    !           (count.lt.10))then
    !         z01 = z02
    !      else
    !         iterflag = .false.
    !      endif
    !      count = count + 1
    !   enddo
    !   ittot = 3 !extra iterations for 10m wind
    !else
    !   wnd_10_mag = uref
    !   ittot = 1 !no iteration needed
    !endif
    if (tail_choice.eq.0) then
      sat=tail_level
    elseif (tail_choice.eq.1) then
      call wnd2sat(wnd_10_mag,sat)
    endif
    ! now you have the guess at 10 m wind mag. and z01
    !/
    !--------------------------|
    ! get first guess at ustar |
    !--------------------------|
    ustra = uref*kappa/log(zwnd/z01)
    ustd = urefd
    wnd_10_dir = urefd
    wnd_10_x=wnd_10_mag*cos(wnd_10_dir)
    wnd_10_y=wnd_10_mag*sin(wnd_10_dir)
    !
    ! 1.  attach tail ---------------------------------------------------- *
    !
    call sig2wn ( sig(nk),depth,kmax)
    nkt = nk
    do while ( kmax .lt. 366. )
      nkt = nkt + 1
      kmax = ( kmax * xfr**2 )
    enddo
    allocate( wn(nkt), dwn(nkt), cp(nkt),sig2(nkt), spc2(nkt,nth), &
         tauintx(nkt),tauinty(nkt))
    !|--------------------------------------------------------------------|
    !|----build discrete wavenumbers for defining spectrum on-------------|
    !|--------------------------------------------------------------------|
    do k = 1, nk
      call sig2wn(sig(k),depth,wn(k))
      cp(k) = sig(k) / wn(k)
      sig2(k) = sig(k)
    enddo
    do k = ( nk + 1 ), ( nkt)
      sig2(k)=sig2(k-1)*xfr
      call sig2wn(sig2(k),depth,wn(k))
      cp(k)=sig2(k)/wn(k)
    enddo
    do k = 2, nkt-1
      dwn(k) = (wn(k+1) - wn(k-1)) / 2.0
    enddo
    dwn(1) = ( wn(2)- ( wn(1) / (xfr **2.0) ) ) / 2.0
    dwn(nkt) = ( wn(nkt)*(xfr**2.0) -  wn(nkt-1)) / 2.0
    !|---------------------------------------------------------------------|
    !|---attach initial tail-----------------------------------------------|
    !|---------------------------------------------------------------------|
    count=0 !count is now counting step through 1-d spectrum
    do k=1, nk
      do t=1, nth
        count = count + 1
        spc2(k,t) = aspc(count)  * sig(k)
      enddo
    enddo
    do k=nk+1, nkt
      do t=1, nth
        spc2(k,t)=spc2(nk,t)*wn(nk)**3.0/wn(k)**(3.0)
      enddo
    enddo
    !
    ! 1c. calculate transitions for new (constant saturation ) tail ------ *
    !
    !-----wavenumber for beginning of (spectrum level) transition to tail- *
    call sig2wn (fpi*tpi*tail_transition_ratio1,depth,ktaila )
    !-----wavenumber for end of (spectrum level) transition to tail------- *
    call sig2wn (fpi*tpi*tail_transition_ratio2,depth,ktailb )
    !-----wavenumber for end of (spectrum direction) transition to tail--- *
    ktailc= ktailb * 2.0
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
    call appendtail(spc2,wn,nkt,ka1,ka2,ka3,atan2(wndy,wndx),sat)
    ! now the spectrum is set w/ tail level sat
    !
    ! 2. enter iteration ------------------------------------------------- *
    !
    ! add new iteration for wind
    !
    ! wind perturbations for iteration
    !dt = 1.e-04
    !dtx = (/ 1. , -1. , 0. /)
    !dty = (/ 0. , 1. , -1. /)
    !/
    heightflg=.false.!not set-up for non-10 m winds
    wiflg = .true.   !this kicks out when wind iteration complete
    no_err = .true.  !this kicks out when there is an error
    wi_count = 1     !count is now counting wind iterations
    ! - start of wind iteration (if applicable)
    do while ( wiflg .and. no_err )  !wind iteration
      !/
      do wi = 1, ittot   !newton-raphson solve for derivatives if zwnd not 10 m.
        ! if iterating over 10 m wind need to adjust guesses to get slopes
        if (heightflg) then
          wnd_10_x = wnd_10_x + dtx(wi)*dt
          wnd_10_y = wnd_10_y + dty(wi)*dt
          wnd_10_mag = sqrt(wnd_10_x**2+wnd_10_y**2)
          wnd_10_dir = atan2(wnd_10_y,wnd_10_x)
        endif
        !
        ! stress iteration (inside wind iteration solve for stress)
        its = 1 !its is counting stress iteration
        ust_it_flg(1)=.true.
        ust_it_flg(2)=.true.
        do while ((ust_it_flg(1) .and. ust_it_flg(2)) .and. no_err)
          !get z0 from (guessed) stress and wind magnitude
          z0  = 10. / ( exp( kappa * wnd_10_mag / ustra ) )
          tauintx(1:nkt) = 0.0
          tauinty(1:nkt) = 0.0
          do k = 1, nkt
            !waves 'feel' wind at height related to wavelength
            wnd_z = min( pi / wn(k), 20.0 )
            wnd_z_mag = ( ustra / kappa ) * (log(wnd_z/z0))
            do t = 1, nth
              !projected component of wind in wave direction
              wnd_z_proj = wnd_z_mag * cos( wnd_10_dir-th(t) )
              if (wnd_z_proj .gt. cp(k)) then
                !waves slower than wind
                a1 = 0.11
              elseif (( wnd_z_proj .ge. 0 ) .and. ( wnd_z_proj .le. cp(k) )) then
                !wave faster than wind
                a1 = 0.01
              elseif (wnd_z_proj .lt. 0) then
                !waves opposed to wind
                a1 = 0.1
              endif
              wnd_effect = wnd_z_proj - cp(k)
              scin = a1 * wnd_effect * abs( wnd_effect ) * dair / dwat * &
                   wn(k) / cp(k)
              ! -- original version assumed g/cp = sig,(a.k.a in deep water.)
              !   tauintx(k) = tauintx(k) + spc2(k,t) * scin &
              !        * cos( th(t) ) / cp(k) * dth
              !   tauinty(k) = tauinty(k) + spc2(k,t) * scin &
              !        * sin( th(t) ) / cp(k) * dth
              tauintx(k) = tauintx(k) + spc2(k,t) * scin &
                   * cos( th(t) ) *sig2(k) * dth
              tauinty(k) = tauinty(k) + spc2(k,t) * scin &
                   * sin( th(t) ) *sig2(k) * dth
            enddo
          enddo
          tauyw = 0.0
          tauxw = 0.0
          do k = 1, nkt
            !  tauxw = tauxw + dwat * grav * dwn(k) * tauintx(k)
            !  tauyw = tauyw + dwat * grav * dwn(k) * tauinty(k)
            tauxw = tauxw + dwat * dwn(k) * tauintx(k)
            tauyw = tauyw + dwat * dwn(k) * tauinty(k)
          enddo
          cdf = ( sqrt(tauxw**2.0+tauyw**2.0) / dair ) / wnd_10_mag**2.0
          !|---------------------------------------------------------------------|
          !|----solve for the smooth drag coefficient to use as initial guess----|
          !|----for the viscous stress-------------------------------------------|
          !|---------------------------------------------------------------------|
          if (uref .lt. 0.01) then
            ustsm = 0.0
            iterflag = .false.
          else
            z02 = 0.001
            iterflag = .true.
          endif
          count = 1
          ! finding smooth z0 to get smooth drag
          do while( (iterflag ) .and. (count .lt. 10) )
            z01 = z02
            ustsm = kappa * wnd_10_mag / ( log( 10. / z01 ) )
            z02 = 0.132 * nu / ustsm
            if (abs( z02/z01-1.0) .lt. 10.0**(-4)) then
              iterflag = .false.
            else
              iterflag = .true.
            endif
            count = count + 1
          enddo
          cds = ustsm**2.0 / wnd_10_mag**2.0
          ! smooth drag adjustment based on full drag
          cds = cds / 3.0 * ( 1.0 + 2.0 * cds / ( cds + cdf ) )
          !-----solve for viscous stress from smooth cd
          taunux = dair * cds * wnd_10_mag**2.0 * cos( wnd_10_dir )
          taunuy = dair * cds * wnd_10_mag**2.0 * sin( wnd_10_dir )
          !-----sum drag components
          taux = taunux + tauxw
          tauy = taunuy + tauyw
          !-----calculate ustar
          ustrb = sqrt( sqrt( tauy**2.0 + taux**2.0) / dair )
          !-----calculate stress direction
          ustd = atan2(tauy,taux)
          !checking ustar. ustra=guess. ustrb=found.
          b1 = ( ustra - ustrb )
          b2 = ( ustra + ustrb ) / 2.0
          its = its + 1
          !check for convergence
          ust_it_flg(1)=( abs(b1*100.0/b2) .ge. 0.01)
          !if not converged after 20 iterations, quit.
          ust_it_flg(2)=( its .lt. 20 )
          if ( ust_it_flg(1) .and. ust_it_flg(2)) then
            ! toyed with methods for improving iteration.
            ! ultimately this was sufficient.
            ! may be imporved upon in future...
            ustra = ustrb*.5 + ustrb*.5
          elseif (abs(b1*100.0/b2) .ge. 5.) then
            !after 20 iterations, >5% from converged
            ust_it_flg(1) = .false.
            ust_it_flg(2) = .false.
            print*,'attn: stress not converged for windspeed: ',uref
            ust = -999.
            no_err = .false.
          endif
        enddo
        !if (heightflg) then
        !   ! get along stress wind at top wave boundary layer (10m)
        !   wndpa=wnd_10_mag*cos(wnd_10_dir-ustd)
        !   wndpe=wnd_10_mag*sin(wnd_10_dir-ustd)
        !   ! calculate cartesian of across wind
        !   wndpex=wndpe*cos(ustd+pi/2.)! add pi/2 since referenced
        !   wndpey=wndpe*sin(ustd+pi/2.)! to right of stress angle
        !   !approx as neutral inside 10 m (wbl) calculate z0
        !   wnd_ref_al=uref*cos(urefd-ustd)
        !   z0=10. / exp( wnd_10_mag * kappa / ustra )
        !   ! use that z0 to calculate stability
        !   ! cd to ref height (based on input wind)
        ! below is subroutine for computing stability effects
        !   call mflux(wnd_ref_al,zwnd,z0,rib,cd)
        ! 2. get cd with stability
        ! 3. get new 35-m wind based on calculated stress cd from mo
        !   wndpa=ustra/sqrt(cd)
        !   wndpax=wndpa*cos(ustd)
        !   wndpay=wndpa*sin(ustd)
        !   if (wi.eq.3) then
        !      u35_1=wndpax+wndpex
        !      v35_1=wndpay+wndpey
        !   elseif (wi.eq.2) then
        !      u35_2=wndpax+wndpex
        !      v35_2=wndpay+wndpey
        !   elseif (wi.eq.1) then
        !      u35_3=wndpax+wndpex
        !      v35_3=wndpay+wndpey
        !   endif
        !endif
      enddo
      !if (heightflg) then
      !   difu10xx= u35_3-u35_1
      !   difu10yx= v35_3-v35_1
      !   difu10xy= u35_2-u35_1
      !   difu10yy= v35_2-v35_1
      !   fd_a = difu10xx / dt
      !   fd_b = difu10xy / dt
      !   fd_c = difu10yx / dt
      !   fd_d = difu10yy / dt
      !   du = -wndx+u35_1
      !   dv = -wndy+v35_1
      !   uitv= abs(du)
      !   vitv=abs(dv)
      !   ch=sqrt(uitv*uitv+vitv*vitv)
      !   if (ch.gt.10) then
      !      apar=0.5/(fd_a*fd_d-fd_b*fd_c)
      !   else
      !      apar=1.0/(fd_a*fd_d-fd_b*fd_c)
      !   endif
      ! check for wind convergence
      !   wnd_it_flg = (((du**2+dv**2)/(wndx**2+wndy**2)).gt.0.001)
      !
      !   if ( wnd_it_flg .and. wi_count.lt.10 ) then
      !      ! new guesses
      !      wnd_10_x=wnd_10_x-apar*( fd_d * du - fd_b * dv )
      !      wnd_10_y=wnd_10_y-apar*( -fd_c * du +fd_a * dv )
      !   else
      !      wiflg = .false.
      !      if (wi_count .gt. 10 .and. &
      !           ((du**2+dv**2)/(wndx**2+wndy**2)).gt.0.05 ) then
      !         print*,'attn: w3fld2 wind error gt 5%'
      !         !print*,'  wind y/error: ',wndy,dv
      !         !print*,'  wind x/error: ',wndx,du
      !         no_err = .false.
      !      endif
      !   endif
      !   wi_count = wi_count + 1
      !else
      wiflg=.false. ! if already 10 m wind then complete.
      !endif
    enddo
    ust = ustrb
    ustd = atan2(tauy, taux)
    cd = ust**2 / uref**2
    if (present(charn)) then
      charn = 0.01/sqrt(sqrt( taunux**2 + taunuy**2 )/(ust**2))
    endif
    if (.not.((cd .lt. 0.01).and.(cd .gt. 0.0005)).or. .not.(no_err)) then
      !fail safe to bulk
      print*,'attn: w3fld2 failed, using bulk stress'
      print*,'calculated wind/cd: ',uref,cd,ust
      call wnd2z0m(uref,z0)
      ust = uref * kappa / log(zwnd/z0)
      cd = ust**2/uref**2
      ustd = urefd
    endif
    deallocate( tauinty , tauintx , &
         spc2, sig2, cp , dwn , wn )
    !stop
    !/ end of w3fld2 ----------------------------------------------------- /
    !/
    return
    !
  end subroutine w3fld2
  subroutine wnd2sat(wnd10,sat)
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           b. g. reichl            |
    !/                  |                        fortran 90 |
    !/                  | last update :         04-aug-2016 |
    !/                  +-----------------------------------+
    !/
    !/    15-jan-2016 : origination.                        ( version 5.12 )
    !/    05-aug-2016 : updated for 2016 hwrf cd/u10 curve  ( j. meixner   )
    !/
    !  1. purpose :
    !
    !     gives level of saturation spectrum to produce
    !         equivalent cd as in wnd2z0m (for neutral 10m wind)
    !         tuned for method of donelan et al. 2012
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
    !
    implicit none
    real, intent(in) :: wnd10
    real, intent(out) :: sat
    !
    ! st2, previous hwrf relationship:
    !        sat =0.000000000000349* wnd10**6 +&
    !             -0.000000000250547* wnd10**5 +&
    !             0.000000039543565* wnd10**4 +&
    !             -0.000002229206185* wnd10**3 +&
    !             0.000034922624204* wnd10**2 +&
    !             0.000339117617027* wnd10**1 +&
    !             0.003521314236550
    !     sat values based on
    !     hwrf 2016 cd curve, created with  fetch limited cases st4 physics
    if (wnd10<20) then
      sat = -0.022919753482426e-3* wnd10**2 &
           +0.960758623686446e-3* wnd10    &
           -0.084461041915030e-3
    elseif (wnd10<45) then
      sat = -0.000000006585745* wnd10**4 &
           +0.000001058147954* wnd10**3 &
           -0.000065829151883* wnd10**2 &
           +0.001587028483595* wnd10    &
           -0.002857112191889
    else
      sat = -0.000178498197241* wnd10    &
           +0.012706067280674
    endif
    sat = min(max(sat,0.002),0.014)
  end subroutine wnd2sat
  !/ ------------------------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module c3fld2md -------------------------------------------- /
  !/
end module w3fld2md
