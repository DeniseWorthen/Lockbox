!> @file
!> @brief calculation of the second order correction to the surface
!>  gravity wave spectrum.
!>
!> @author p.a.e.m. janssen
!> @date   21-aug-2014
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
!> @brief calculation of the second order correction to the surface
!>  gravity wave spectrum.
!>
!> @author p.a.e.m. janssen
!> @date   21-aug-2014
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3canomd
  !/
  !/                  +-----------------------------------+
  !/                  |                                   |
  !/                  |   p.a.e.m. janssen                |
  !/                  |                        fortran 90 |
  !/                  | last update :         21-aug-2014 |
  !/                  +-----------------------------------+
  !/
  !/    xx-jul-2010 : origination by  paem janssen
  !/    18-oct-2012 : adapted to wavewatch iii: f. ardhuin( version 4.07 )
  !/    21-aug-2014 : bug corrected: only first call wasok( version 5.01 )
  !/
  !  0. note by f. ardhuin:
  !     in adapting the orginal program to be a wavewatch module, i
  !     have so far strived to keep the original code. as a result
  !     some routines are unnecessarily duplicated (e.g. the calculation of
  !     the group velocity ...). but this can improve the traceability of
  !     the code.
  !     the first spectrum (jonswap) has been removed from the code
  !
  !  1. purpose :
  !
  !
  !     calculation of the second order correction to the surface gravity
  !     wave spectrum
  !
  !     documentation.
  !     -------------
  !
  !     presently, the software is set up to do for a giving first-order
  !     spectrum at a given depth the determination of the second order
  !     correction (including second-harmonics, wave set down and doppler shift
  !     owing to the stokes frequency correction.
  !
  !     evaluation of the interaction coefficients for arbritrary depth would
  !     be very time consuming. therefore, the approach in the wam model is
  !     followed, where tables are generated for a logarithmic depth table
  !
  !               d(jd) = deptha*depthd**(jd-1)
  !
  !     with jd an integer. in the present operational version of ecwam jd
  !     ranges from 1 to ndepth = 74, while deptha = 1. and depthd = 1.1
  !
  !     finally, this is a very time-consuming calculation, at least for an
  !     operational model. i have therefore introduced the option that the second-order
  !     spectrum is calculated on a lower resolution grid (typically half the
  !     resolution) while the information contained in the first-order spectrum
  !     is kept on the original spectral grid.
  !
  ! ----------------------------------------------------------------------
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
  !      w3sref    subr. public   reflection of waves (shorline, islands...)
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
  !/
  !/ public variables
  !/
  real                     :: g, pi, zpi, rad, deg
  integer                  :: ndepth
  real                     :: deptha         ! first depth in table
  real, save , private, allocatable   :: omega(:)
  integer, save , private             :: counter = 0
  ! tables for non-linear coefficients ...
  real, save , private, allocatable   :: ta(:,:,:,:),tb(:,:,:,:),tc_ql(:,:,:,:),&
       tt_4m(:,:,:,:),tt_4p(:,:,:,:),tfakh(:,:),    &
       tfak(:,:)
  integer, save, private, allocatable :: im_p(:,:),im_m(:,:)
  !
  !/
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief adds second order spectrum on top of first order spectrum.
  !>
  !> @param[inout] e        energy density spectrum (1-d), f-theta.
  !> @param[in]    depth    mean water depth.
  !> @param[in]    wn       wavenumbers.
  !> @param[in]    cg       group velocities.
  !> @param[in]    iaction  action density spectrum (1-d).
  !>
  !> @author f. ardhuin
  !> @date   19-oct-2012
  !>
  subroutine w3add2ndorder(e,depth,wn,cg,iaction)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         19-oct-2012 |
    !/                  +-----------------------------------+
    !/
    !/    19-oct-2012 : origination                         ( version 4.08 )
    !/
    !  1. purpose :
    !
    !     adds second order spectrum on top of first order spectrum
    !
    !  2. method :
    !
    !     uses p. janssen's code for the inverse canonical transform
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       a         r.a.  i   action density spectrum (1-d)
    !       cg        r.a.  i   group velocities.
    !       wn        r.a.  i   wavenumbers.
    !       depth     real  i   mean water depth.
    !       s         r.a.  o   source term (1-d version).
    !       d         r.a.  o   diagonal term of derivative (1-d version).
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !       e         r.a. i/o   energy density spectrum (1-d), f-theta
    !       depth     real i     water depth
    !       wn        r.a.       wavenumbers
    !       cg        r.a.       group velocities
    !       iaction   int  i     switch to specify if the input spectrum
    !                            is e(f,theta) or a(k,theta)
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
    !      w3sref    subr. w3ref1md shoreline reflection source term
    !      w3expo    subr.   n/a    point output post-processor.
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
    use w3dispmd
    use w3gdatmd, only: nk, nth, nspec, sig, th, dth, igpars
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(inout)     :: e(nspec)
    real, intent(in)        :: depth
    real, intent(in)        :: wn(nk)
    real, intent(in)        :: cg(nk)
    integer, intent(in)     :: iaction
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer           :: ispec, ik, ith, m
    real              :: co1, atoe, dpth
    logical, save     :: first = .true.
    real, allocatable, save  :: fr(:), dfim(:)
    real, allocatable, save  :: f1(:,:), f3(:,:)
    integer, save     ::  nfre, nang
    integer, save     ::  nfreh, nangh
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    if (first) then
      first=.false.
      nfre=nk
      nang=nth
      nfreh=nk
      nangh=nth
      g=grav
      pi = 4.*atan(1.)
      zpi=2*pi
      rad = pi/180.
      deg = 180./pi
      allocate(fr(nfre), dfim(nfre))
      fr(1:nfre)=sig(1:nk)/zpi
      ! the following can be replaced using dsip from wwatch
      co1 = 0.5*dth
      dfim(1)= co1*(fr(2)-fr(1))
      do m=2,nfre-1
        dfim(m)=co1*(fr(m+1)-fr(m-1))
      enddo
      dfim(nfre)=co1*(fr(nfre)-fr(nfre-1))
      !
      allocate(f1(nang,nfre), f3(nang,nfre))
      ndepth=igpars(6)
      deptha=igpars(7)
    end if
    dpth = depth
    do ik=1,nk
      if (iaction.eq.0) then
        atoe=1
      else
        atoe=sig(ik)*zpi / cg(ik)
      end if
      do ith=1,nth
        ispec=ith+(ik-1)*nth
        f1(ith,ik)=e(ispec)*atoe
      end do
      !write(100,'(100g16.8)') sig(ik)*zpi,(f1(ith,ik),ith=1,nth)
    end do
    !
    ! 1. determine second-order spectrum.
    !
    call cal_sec_order_spec(f1,f3,nfre,nang,fr,dfim,th,   &
         dth,dpth,+1., nfreh, nangh)
    !
    ! 2. adds 2nd order spectrum to 1st order
    !
    do ik=1,nk
      if (iaction.eq.0) then
        atoe=1
      else
        atoe=sig(ik)*zpi / cg(ik)
      end if
      do ith=1,nth
        ispec=ith+(ik-1)*nth
        e(ispec)=f3(ith,ik)/atoe
      end do
      !write(101,'(i3,100g16.8)') sig(ik)*zpi,(f3(ith,ik),ith=1,nth)
    end do
    return
  end subroutine w3add2ndorder
  !/ ------------------------------------------------------------------- /
  !-----------------------------------------------------------------------
  !
  !>
  !> @brief determines second order spectrum.
  !>
  !> @param[in]  f1     2-d free wave spectrum
  !> @param[out] f3     2-d spectrum including 2nd-order correction
  !> @param[in]  nfre   number of frequencies
  !> @param[in]  nang   number of directions
  !> @param[in]  fr     frequencies
  !> @param[in]  dfim   frequency increment
  !> @param[in]  th     directional array
  !> @param[in]  delth  directional increment
  !> @param[in]  dpth   depth array
  !> @param[in]  sigm   mapping indicator
  !> @param[in]  nfreh
  !> @param[in]  nangh
  !>
  !> @author peter janssen
  !> @date   na
  !>
  subroutine cal_sec_order_spec(f1,f3,nfre,nang,fr,dfim,th,delth, &
       dpth,sigm, nfreh, nangh)
    !
    !***  *cal_sec_order_spec*   determines second_order spectrum
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              determination of second-order spectrum
    !
    !     interface.
    !     ----------
    !              *call*  *cal_sec_order_spec(f1,f3,nfre,nang,fr,
    !                                dfim,th,delth,dpth,sigm)*
    !
    !                       input:
    !                            *f1*    - 2-d free wave spectrum
    !                            *nfre*  - number of frequencies
    !                            *nang*  - number of directions
    !                            *fr*    - frequencies
    !                            *dfim*  - frequency increment
    !                            *th*    - directional array
    !                            *delth* - directional increment
    !                            *dpth*  - depth array
    !                            *sigm*   - for sigm = 1 forward mapping
    !                                      while for sigm = -1 inverse
    !                                      mapping.
    !
    !                       output:
    !                            *f3*    - 2-d spectrum including second-order
    !                                      correction
    !
    !     method.
    !     -------
    !              is described in janssen (2009), jfm, 637, 1-44.
    !
    !     externals.
    !     ----------
    !              none
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    real, intent(in)        :: f1(nang,nfre)
    real, intent(out)       :: f3(nang,nfre)
    integer, intent(in)     :: nfre,nang,nfreh, nangh
    real, intent(in)        :: dfim(nfre),fr(nfre), th(nang), delth
    real, intent(in)        :: dpth, sigm
    logical frstime,doublep
    integer mdw,m,k, k0,m0,mp,kp,mm,km,kl,kll,ml,jd
    integer, save :: mr, ma,nmax
    !      parameter (nfreh=32,nangh=36)
    integer, save            :: indep
    real,allocatable         :: pf1(:,:),pf3(:,:)
    real depth,alpha,gam_j,depthd
    real om0,aa1,bb1,&
         f,epsmin,delff,spec1,sqrtk
    real frac,del,delf,d1,d2,d3,d4,c1,&
         c2,xm,xk
    real, save :: omstart
    real, save :: xmr,xma, delthh, co1
    real :: f13(nfreh,nangh)
    real :: sum0,akmean
    real :: delom(nfreh),thh(nangh),dfdth(nfreh)
    data frstime/.true./
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    !
    !***  2. determine second order correction to the spectrum
    !     ----------------------------------------------------
    !
    doublep = .true.
    !
    !***  2.1 set up of low-resolution calculation grid.
    !     ---------------------------------------------
    !
    epsmin = 1.0e-4
    frac = 0.1
    omstart = zpi*fr(1)
    mr = max(1,nfre/nfreh)
    xmr = 1./float(mr)
    ma = nang/nangh
    xma = 1./float(ma)
    delthh = float(ma)*delth
    if (frstime) then
      ! if (counter.gt.0) then
      !  deallocate(omega,tfak,ta,tb,tc_ql,tt_4m,tt_4p,im_p,im_m,tfakh)
      ! endif
      allocate(omega(nfreh))
      allocate(tfak(nfre,ndepth))
      allocate(ta(nangh,nfreh,nfreh,ndepth))
      allocate(tb(nangh,nfreh,nfreh,ndepth))
      allocate(tc_ql(nangh,nfreh,nfreh,ndepth))
      allocate(tt_4m(nangh,nfreh,nfreh,ndepth))
      allocate(tt_4p(nangh,nfreh,nfreh,ndepth))
      allocate(im_p(nfreh,nfreh))
      allocate(im_m(nfreh,nfreh))
      allocate(tfakh(nfreh,ndepth))
      do m=1,nfreh
        omega(m) = zpi*fr(mr*m)
      enddo
      do k=1,nangh
        k0 = ma*k+1
        if (k0.gt.nang) k0 = k0-nang
        thh(k) = th(k0)
      enddo
      co1   = 1./2.*delthh
      delom(1) = co1*(omega(2)-omega(1))
      do m=2,nfreh-1
        delom(m)=co1*(omega(m+1)-omega(m-1))
      enddo
      delom(nfreh)=co1*(omega(nfreh)-omega(nfreh-1))
      !
      dfdth = delom/zpi
      !
      !***  2.2 initialise tables
      !     ---------------------
      !
      nmax = xmr*(1+nint(log(2.*omega(nfreh)/omstart)/log(1.+frac)))
      nmax = nmax+1
      depthd = 1.1
      do jd=1,ndepth
        depth = deptha*depthd**(jd-1)
        do m=1,nfre
          om0 = zpi*fr(m)
          tfak(m,jd) = aki(om0,depth)
        enddo
      enddo
      indep = 1+nint(log(dpth/deptha)/log(depthd))
      indep = min(ndepth,indep)
      indep = max(1,indep)
      call tables_2nd(nfreh,nangh,ndepth,deptha,omstart,frac,xmr,&
           dfdth,omega,thh)
      print*, '2nd order tables generated:',ndepth,deptha, delthh
      frstime = .false.
    endif ! end of test on frstime
    !
    counter=counter+1
    !
    !***  determine some moments.
    !     ----------------------
    !
    sum0 = 0.
    akmean = 0.
    do m=1,nfre
      do k=1,nang
        sqrtk=sqrt(tfak(m,indep))
        sum0 = sum0+f1(k,m)*dfim(m)
        akmean = akmean+f1(k,m)*dfim(m)/sqrtk
      enddo
    enddo
    !
    ! nb: akmean is the mean wavenumber corresponding to tm0,-1 in deep water
    !
    akmean = (sum0/akmean)**2
    !
    !***  2.2 interpolation or not.
    !     ------------------------
    !
    if (mr.eq.1 .and. ma.eq.1) then
      !
      !***     2.21 no interpolation.
      !        ----------------------
      !
      call secspom(f1,f3,nfre,nang,nmax,ndepth,&
           deptha,depthd,omstart,frac,mr,dfdth,omega,&
           dpth,akmean,ta,tb,tc_ql,tt_4m,tt_4p,&
           im_p,im_m,counter)
      do m=1,nfre
        do k=1,nang
          delf = f3(k,m)
          f3(k,m)=max(0.00000001,f1(k,m)+sigm*delf)
        enddo
      enddo
    else
      !
      !***     2.22 energy conserving interpolation scheme
      !        -------------------------------------------
      !
      print*,' !thinning and interpolation!'
      allocate(pf1(nangh,nfreh))
      allocate(pf3(nangh,nfreh))
      pf1 = 0.
      do m=1,nfreh
        do k=1,nangh
          m0 = mr*m
          mp = m0+1
          mp = min(nfre,mp)
          mm = m0-1
          k0 = ma*k+1
          kp = k0+1
          km = k0-1
          delff = 0.
          do kl = km,kp
            kll = kl
            if (kll.gt.nang) kll = kll-nang
            if (kll.lt.1) kll = kll+nang
            do ml = mm,mp
              del = dfim(ml)
              delff = delff+del
              spec1 = f1(kll,ml)
              pf1(k,m)=pf1(k,m)+spec1*del
            enddo
          enddo
          pf1(k,m) =pf1(k,m)/delff
        enddo
      enddo
      !
      !***     2.23 determine second-order spec
      !        --------------------------------
      !
      call secspom(pf1,pf3,nfreh,nangh,nmax,ndepth,&
           deptha,depthd,omstart,frac,mr,dfdth,omega,&
           dpth,akmean,ta,tb,tc_ql,tt_4m,tt_4p,&
           im_p,im_m,counter)
      !
      !***     2.24 interpolate towards high-res grid
      !        --------------------------------------
      !
      do m=1,nfre
        do k=1,nang
          xm = real(m/mr)
          xk = real((k-1)/ma)
          m0 = max(1,int(xm))
          k0 = int(xk)
          d1 = real(m)/real(mr)-xm
          d2 = 1.-d1
          d3 = real(k-1)/real(ma)-xk
          d4 = 1.-d3
          if (k0.lt.1) k0 = k0+nangh
          mp = min(nfreh,m0+1)
          kp = k0+1
          if (kp.gt.nangh) kp = kp-nangh
          c1 = pf3(k0,m0)*d4+pf3(kp,m0)*d3
          c2 = pf3(kp,mp)*d3+pf3(k0,mp)*d4
          delf = c1*d2+c2*d1
          f3(k,m)=max(0.00000001,f1(k,m)+sigm*delf)
        enddo
      enddo
    endif
    if (mr.gt.1 .or. ma.gt.1 ) then
      do m=1,nfreh
        aa1 = 0.
        do k=1,nangh
          aa1 = aa1+pf1(k,m)*delthh
        enddo
        aa1 = max(aa1,epsmin)
        bb1 = 0.
        do k=1,nangh
          bb1 = bb1+(pf1(k,m)+pf3(k,m))*delthh
        enddo
        bb1 = max(bb1,epsmin)
        f   = omega(m)/zpi
      enddo
      do m=1,nfreh
        do k=1,nangh
          f13(m,k)=pf1(k,m)+pf3(k,m)
        enddo
      enddo
    endif
    !
    !
    return
  end subroutine cal_sec_order_spec
  !
  !--------------------------------------------------------------------
  !
  !>
  !> @brief computes tables for second order spectrum in frequency space.
  !>
  !> @param nfre     number of frequencies
  !> @param nang     number of directions
  !> @param ndepth   number of entries in the depth table
  !> @param deptha
  !> @param omstart  start frequency
  !> @param frac     fractional increase in frequency space
  !> @param xmr      inverse of thinning factor in frequency space
  !> @param dfdth    product of increment in frequency and direction
  !> @param omega    angular frequency array
  !> @param th       direction array
  !>
  !> @author na
  !> @date   na
  !>
  subroutine tables_2nd(nfre,nang,ndepth,deptha,omstart,frac,xmr,&
       dfdth,omega,th)
    !
    !--------------------------------------------------------------------
    !
    !*****tables** computes tables for second order spectrum in frequency space.
    !
    !     p.janssen december 2008
    !
    !     purpose
    !     -------
    !             determines tables, based on janssen (2008)
    !             there are three corrections:
    !                   1) generation of second-harmonics
    !                   2) quasi-linear effect
    !                   3) shift of spectrum because of stokes frequency
    !                      correction.
    !
    !     interface
    !     ---------
    !             *call* *tables(nfre,nang,ndepth,omstart,frac,xmr,
    !                            omega,ta,tb,tc_ql,tt_4m,tt_4p,im_p,im_m,
    !                            tfak)*
    !
    !
    !     parameter   type      purpose.
    !     ---------   ----      -------
    !
    !       nfre      integer   number of frequencies
    !       nang      integer   number of directions
    !       ndepth    integer   number of entries in the depth table
    !       omstart   real      start frequency
    !       frac      real      fractional increase in frequency space
    !       xmr       real      inverse of thinning factor in frequency space
    !       omega     real      angular frequency array
    !       dfdth     real      product of increment in frequency and direction
    !       th        real      direction array
    !       ta        real      table for minus interactions
    !       tb        real      table for plus interactions
    !       tc_ql     real      table for quasi-linear interactions
    !       tt_4m     real      table for stokes frequency correction
    !       tt_4p     real      table for stokes frequency correction
    !       im_p      integer   table for wavenumber m2 plus
    !       im_m      integer   table for wavenumber m2 min
    !       tfak      real      wavenumber table
    !
    !
    !     method
    !     ------
    !
    !     externals
    !     ---------
    !             none
    !
    !     references
    !     ----------
    !             v.e. zakharov, hamiltonian approach (1968)
    !             m.a. srokosz, j.g.r.,91,995-1006 (1986)
    !             p.a.e.m. janssen, ecmwf tech memo (2008),jfm paper (2009)
    !
    !
    !--------------------------------------------------------------------
    !
    !
    !
    implicit none
    integer nfre,nang,ndepth,mdw,jd,m,k,m1,k1,mp,mm,l
    real depth,alpha,gam_j,deptha,depthd
    real om0,th0,xk0,om1,th1,xk1,om2,xk2,om0p,xk0p,om0m,xk0m,omstart,&
         frac,xmr,xm2,fac
    real omega(nfre),th(nang),dfdth(nfre)
    common/const/depth,alpha,mdw,gam_j,depthd
    !
    !     1. computation of wavenumber array tfak
    !     ---------------------------------------
    !
    !
    do jd=1,ndepth
      depth = deptha*depthd**(jd-1)
      do m=1,nfre
        om0 = omega(m)
        tfak(m,jd) = aki(om0,depth)
      enddo
      write(6,*) 'generating tables for depth:',jd,depth,deptha,ndepth
      !
      !     2. computation of the 2nd order coefficients.
      !     ---------------------------------------------
      !
      !
      k1 = 0
      th1 = th(nang)
      do m=1,nfre
        om0 = omega(m)
        xk0 = tfak(m,jd)
        mp   = min(m+1,nfre)
        om0p = omega(mp)
        xk0p = tfak(mp,jd)
        mm   = max(m-1,1)
        om0m = omega(mm)
        xk0m = tfak(mm,jd)
        do m1=1,nfre
          om1 = omega(m1)
          do l=1,nang
            !
            !              xk0-xk1 case
            !
            k = k1+l
            th0 = th(k)
            om2 = om0-om1
            if (abs(om1).lt.om0/2.) then
              xm2  = log(om2/omstart)/log(1.+frac)
              im_m(m1,m) = nint(xmr*(xm2+1.))
              xk1 = tfak(m1,jd)
              xk2 = aki(om2,depth)
              ta(l,m1,m,jd) = dfdth(m1)*a(xk1,xk2,th1,th0)**2
            else
              ta(l,m1,m,jd) = 0.
              im_m(m1,m) = 1
            endif
            !
            !              xk1+xk0 case
            !
            om2 = om1+om0
            xm2  = log(om2/omstart)/log(1.+frac)
            im_p(m1,m) = nint(xmr*(xm2+1.))
            xk1 = tfak(m1,jd)
            xk2 = aki(om2,depth)
            tb(l,m1,m,jd) = dfdth(m1)*b(xk1,xk2,th1,th0)**2
            !
            !              quasi-linear effect
            !
            !
            tc_ql(l,m1,m,jd) = dfdth(m1)*c_ql(xk0,xk1,th0,th1)
            !
            !              stokes-frequency correction
            !
            !
            fac = 2.*g/om1*dfdth(m1)
            tt_4m(l,m1,m,jd) = &
                 fac*(w2(xk0m,xk1,xk1,xk0m,th0,th1,th1,th0)+&
                 v2(xk0m,xk1,xk1,xk0m,th0,th1,th1,th0))
            tt_4p(l,m1,m,jd) = &
                 fac*(w2(xk0p,xk1,xk1,xk0p,th0,th1,th1,th0)+&
                 v2(xk0p,xk1,xk1,xk0p,th0,th1,th1,th0))
            ! table identical to janssen: verified.
            !              if (jd.eq.1) write(998,'(f4.1,3i3,5g11.3)') depth,m,m1,l, tb(l,m1,m,jd),  &
            !                           tc_ql(l,m1,m,jd) , fac, tt_4m(l,m1,m,jd), tt_4p(l,m1,m,jd)
          enddo
        enddo
      enddo
    enddo
    !
    !
    !--------------------------------------------------------------------
    !
    return
  end subroutine tables_2nd
  !
  !--------------------------------------------------------------------
  !
  !>
  !> @brief computes second order spectrum in frequency space.
  !>
  !> @param f1       2d free wave spectrum (input)
  !> @param f3       bound waves spectrum (output)
  !> @param nfre     number of frequencies
  !> @param nang     number of directions
  !> @param nmax     maximum index corresponds to twice the cut-off frequency
  !>
  !> @param ndepth   number of entries in depth table
  !> @param deptha   start value depth array
  !> @param depthd   increment depth array
  !> @param omstart  start value angular frequency array
  !> @param frac     fractional increase in frequency space
  !> @param mr       thinning factor in frequency space
  !> @param omega    angular frequency array
  !> @param depth    depth array
  !> @param akmean   mean wavenumber array
  !> @param ta       table for minus interactions
  !> @param tb       table for plus interactions
  !> @param tc_ql    table for quasi-linear interactions
  !> @param tt_4m    table for stokes frequency correction
  !> @param tt_4p    table for stokes frequency correction
  !> @param im_p     table for wavenumber m2 plus
  !> @param im_m     table for wavenumber m2 min
  !>
  !> @author na
  !> @date   na
  !>
  subroutine secspom(f1,f3,nfre,nang,nmax,ndepth,&
       deptha,depthd,omstart,frac,mr,dfdth,omega,&
       depth,akmean,ta,tb,tc_ql,tt_4m,tt_4p,&
       im_p,im_m,counter)
    !
    !--------------------------------------------------------------------
    !
    !*****secspom** computes second order spectrum in frequency space.
    !
    !     p.janssen july 2008
    !
    !     purpose
    !     -------
    !             determines second-order spectrum, based on janssen (2008)
    !             there are three corrections:
    !                   1) generation of second-harmonics
    !                   2) quasi-linear effect
    !                   3) shift of spectrum because of stokes frequency
    !                      correction.
    !
    !     interface
    !     ---------
    !             *call* *secspom(f1,f3,nfre,nang,nmax,ndepth,
    !                             deptha,depthd,omstart,frac,mr,dfdth,omega,
    !                             depth,akmean,ta,tb,tc_ql,tt_4m,tt_4p,
    !                             im_p,im_m)*
    !
    !
    !     parameter   type      purpose.
    !     ---------   ----      -------
    !
    !       f1        real      2d free wave spectrum (input)
    !       f3        real      bound waves spectrum (output)
    !       nfre      integer   number of frequencies
    !       nang      integer   number of directions
    !       nmax      integer   maximum index corresponds to twice the cut-off
    !                           frequency
    !       ndepth    integer   number of entries in depth table
    !       deptha    real      start value depth array
    !       depthd    real      increment depth array
    !       omstart   real      start value ang. frequency array
    !       frac      real      fractional increase in frequency space
    !       mr        integer   thinning factor in frequency space
    !       omega     real      angular frequency array
    !       depth     real      depth array
    !       akmean    real      mean wavenumber array
    !       ta        real      table for minus interactions
    !       tb        real      table for plus interactions
    !       tc_ql     real      table for quasi-linear interactions
    !       tt_4m     real      table for stokes frequency correction
    !       tt_4p     real      table for stokes frequency correction
    !       im_p      integer   table for wavenumber m2 plus
    !       im_m      integer   table for wavenumber m2 min
    !
    !
    !
    !     method
    !     ------
    !             evaluate second order spectrum in frequency based on
    !             krasitskii's canonical transformation.
    !
    !     externals
    !     ---------
    !             none
    !
    !     references
    !     ----------
    !             v.e. zakharov, hamiltonian approach (1968)
    !             m.a. srokosz, j.g.r.,91,995-1006 (1986)
    !             p.a.e.m. janssen, jfm (2009)
    !
    !
    !--------------------------------------------------------------------
    !
    !
    !
    use w3gdatmd, only: igpars
    implicit none
    integer nfre,nang,ndepth,m,k,m1,k1,m2_m,m2_p,k2,mp,&
         mm,l,mr,nmax,jd,counter
    integer im_p(nfre,nfre),im_m(nfre,nfre),il(nang,nang)
    real om0,om0h,om1,om0p,om0m,&
         omstart,frac,xincr1,xincr2,xincr3,xincr4,fac1,fac2,&
         fac3,t_4m,t_4p,f2k,f2kp,f2km,f2k1,f2k2,delm1,deptha,depthd,&
         xd,x_min
    real omega(nfre), dfdth(nfre), omegahf(nfre+1:nmax)
    real ta(nang,nfre,nfre,ndepth),tb(nang,nfre,nfre,ndepth),&
         tc_ql(nang,nfre,nfre,ndepth),tt_4m(nang,nfre,nfre,ndepth),&
         tt_4p(nang,nfre,nfre,ndepth)
    real f1(nang,nfre),f3(nang,nfre),depth
    real akmean
    real g1(nang,nmax),g3(nang,nfre)
    logical :: ll2h
    !
    !***  1. computation of tail of the spectrum and index jd
    !     ---------------------------------------------------
    !
    !
    x_min = igpars(9)   ! this was 1.1 in janssen's original code
    do m=nfre+1,nmax
      omegahf(m) = omstart*(1.+frac)**(mr*m-1)
    enddo
    do k=1,nang
      do k1=1,nang
        l = k-k1
        if (l.gt.nang) l=l-nang
        if (l.lt.1) l=l+nang
        il(k,k1) = l
      enddo
    enddo
    !   this was janssen's version ... limited to kd > x_min ... (here set to 1.1)
    xd = max(x_min/akmean,depth)    ! note by fa: why do we have x_min/akmean??!
    xd = depth
    xd = log(xd/deptha)/log(depthd)+1.
    jd = nint(xd)
    jd = max(jd,1)
    jd = min(jd,ndepth)
    do m=1,nfre
      do k=1,nang
        g1(k,m) = f1(k,m)
        g3(k,m) = 0.
      enddo
    enddo
    do m=nfre+1,nmax
      do k=1,nang
        g1(k,m) = omega(nfre)**5*g1(k,nfre)/omegahf(m)**5
      enddo
    enddo
    !
    !
    !
    !
    !***  2. computation of the 2nd order frequency spectrum.
    !     ---------------------------------------------------
    !
    !
    do m=1,nfre
      om0 = omega(m)
      om0h = om0/2.
      mp   = min(m+1,nfre)
      om0p = omega(mp)
      mm   = max(m-1,1)
      om0m = omega(mm)
      delm1 = 1./(om0p-om0m)
      do k=1,nang
        k2 = k
        f2k = g1(k,m)
        f2kp = g1(k,mp)
        f2km = g1(k,mm)
        do m1=1,nfre
          om1 = omega(m1)
          ll2h = (abs(om1).lt.om0h)
          m2_m = im_m(m1,m)
          m2_p = im_p(m1,m)
          do k1=1,nang
            f2k1 = g1(k1,m1)
            l = il(k,k1)
            !
            !                   2.1 om0-om1 case: second harmonics
            !                   om2 = om0-om1
            !
            if (ll2h) then
              f2k2 = g1(k2,m2_m)
              fac1 = ta(l,m1,m,jd)
              fac2 = f2k1*f2k2+g1(k2,m1)*g1(k1,m2_m)
              xincr1 = fac1*fac2
              g3(k,m) = g3(k,m)+xincr1
            endif
            !
            !                   2.2 om1+om0 case: infra-gravity waves
            !                    om2 = om1+om0
            !
            f2k2 = g1(k2,m2_p)
            fac3 = 2.*tb(l,m1,m,jd)
            xincr2 = fac3*f2k2
            !
            !                   2.3 quasi-linear effect
            !
            xincr3 = tc_ql(l,m1,m,jd)*f2k
            !
            !                   2.4 stokes-frequency correction
            !
            t_4m = tt_4m(l,m1,m,jd)
            t_4p = tt_4p(l,m1,m,jd)
            xincr4 = -(f2kp*t_4p-f2km*t_4m)*delm1
            g3(k,m) = g3(k,m)+f2k1*(xincr2+xincr3+xincr4)
          enddo
        enddo
      enddo
    enddo
    !
    do m=1,nfre
      do k=1,nang
        f3(k,m) = g3(k,m)
      enddo
    enddo
    !
    !--------------------------------------------------------------------
    !
    return
  end subroutine secspom
  !
  !>
  !> @brief gives nonlinear transfer coefficient for three wave interactions
  !>  interactions of gravity waves in the ideal case of no current. determines
  !>  the minus interaction coefficients.
  !>
  !> @param   xi   wave number
  !> @param   xj   wave number
  !> @param   thi
  !> @param   thj
  !> @returns a
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function a(xi,xj,thi,thj)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *a(xi,xj,thi,thj)
    !
    !-----------------------------------------------------------------------
    !
    !***  *a*  determines the minus interactions.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *a(xi,xj)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real ri,rj,rk,xi,xj,thi,thj,thk,oi,oj,ok,fi,fj,fk
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    ri = xi
    rj = xj
    rk  = vabs(ri,rj,thi,thj)
    thk = vdir(ri,rj,thi,thj)
    oi=omeg(ri)
    oj=omeg(rj)
    ok=omeg(rk)
    fi = sqrt(oi/(2.*g))
    fj = sqrt(oj/(2.*g))
    fk = sqrt(ok/(2.*g))
    a = fk/(fi*fj)*(a1(rk,ri,rj,thk,thi,thj)+&
         a3(rk,ri,rj,thk-pi,thi,thj))
    return
  end function a
  !
  !>
  !> @brief gives nonlinear transfer coefficient for three wave interactions
  !>  interactions of gravity waves in the ideal case of no current. determines
  !>  the plus interaction coefficients.
  !>
  !> @param   xi   wave number
  !> @param   xj   wave number
  !> @param   thi
  !> @param   thj
  !> @returns b
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function b(xi,xj,thi,thj)
    !***  *real function* *b(xi,xj,thi,thj)
    !
    !-----------------------------------------------------------------------
    !
    !***  *b*  determines the plus interaction coefficients.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *b(xi,xj)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del,ri,rj,rk,xi,xj,thi,thj,thk,oi,oj,ok,fi,fj,fk
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    del = 0.
    ri = xi
    rj = xj
    rk  = vabs(rj,ri,thj,thi-pi)
    thk = vdir(rj,ri,thj,thi-pi)
    oi=omeg(ri)+del
    oj=omeg(rj)+del
    ok=omeg(rk)+del
    fi = sqrt(oi/(2.*g))
    fj = sqrt(oj/(2.*g))
    fk = sqrt(ok/(2.*g))
    b = 0.5*fk/(fi*fj)*(a2(rk,ri,rj,thk,thi,thj)+&
         a2(rk,rj,ri,thk-pi,thj,thi))
    return
  end function b
  !
  !>
  !> @brief determine contribution by quasi-linear terms.
  !>
  !> @param   xk0
  !> @param   xk1
  !> @param   th0
  !> @param   th1
  !> @returns c_ql
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function c_ql(xk0,xk1,th0,th1)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *c_ql(xk0,xk1,th0,th1)
    !
    !-----------------------------------------------------------------------
    !
    !***  *a*  determines the quasi-linear term.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              determine contribution by quasi-linear terms
    !
    !     interface.
    !     ----------
    !              *c_ql(xk0,xk1)*
    !                        *xk0*  - wave number
    !                        *xk1*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real xk0,xk1,th0,th1,om1,f1
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    om1 = omeg(xk1)
    f1  = sqrt(om1/(2.*g))
    c_ql = 2./f1**2*(b2(xk0,xk1,xk1,xk0,th0,th1,th1,th0)+&
         b3(xk0,xk0,xk1,xk1,th0-pi,th0,th1,th1))
    return
  end function c_ql
  !
  !
  !>
  !> @brief determines the second-order transfer coefficient
  !>        for three wave interactions of gravity waves.
  !>
  !> @param   xi    wave numbers
  !> @param   xj    wave numbers
  !> @param   xk    wave numbers
  !> @param   thi   wave direction
  !> @param   thj   wave direction
  !> @param   thk   wave direction
  !> @returns vplus
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function vplus(xi,xj,xk,thi,thj,thk)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *vplus(xi,xj,xk,thi,thj,thk)
    !
    !-----------------------------------------------------------------------
    !
    !***  *vplus*  determines the second-order transfer coefficient
    !              for three wave interactions of gravity waves.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *vplus(xi,xj,xk)*
    !                      *xi*   - wave number
    !                      *xj*   - wave number
    !                      *xk*   - wave number
    !                      *thi*  - wave direction
    !                      *thj*  - wave direction
    !                      *thk*  - wave direction
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,ri,rj,rk,xi,xj,xk,thi,thj,thk,oi,oj,ok,qi,qj,qk,&
         rij,rik,rjk,sqijk,sqikj,sqjki,zconst
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    del1 = 10.**(-12)
    zconst=1./(4*sqrt(2.))
    ri = xi
    rj = xj
    rk = xk
    oi=omeg(ri)+del1
    oj=omeg(rj)+del1
    ok=omeg(rk)+del1
    qi=oi**2/g
    qj=oj**2/g
    qk=ok**2/g
    rij = ri*rj*cos(thj-thi)
    rik = ri*rk*cos(thk-thi)
    rjk = rj*rk*cos(thk-thj)
    sqijk=sqrt(g*ok/(oi*oj))
    sqikj=sqrt(g*oj/(oi*ok))
    sqjki=sqrt(g*oi/(oj*ok))
    vplus=zconst*( (rij+qi*qj)*sqijk + (rik+qi*qk)*sqikj&
         + (rjk+qj*qk)*sqjki )
    return
  end function vplus
  !
  !>
  !> @brief determines the second-order transfer coefficient for
  !>        three wave interactions of gravity waves.
  !>
  !> @param xi  wave number
  !> @param xj  wave number
  !> @param xk  wave number
  !> @param thi wave direction
  !> @param thj wave direction
  !> @param thk wave direction
  !> @returns vmin
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function vmin(xi,xj,xk,thi,thj,thk)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *vmin(xi,xj,xk,thi,thj,thk)
    !
    !-----------------------------------------------------------------------
    !
    !***  *vmin*  determines the second-order transfer coefficient for
    !             three wave interactions of gravity waves.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *vmin(xi,xj,xk)*
    !                      *xi*   - wave number
    !                      *xj*   - wave number
    !                      *xk*   - wave number
    !                      *thi*  - wave direction
    !                      *thj*  - wave direction
    !                      *thk*  - wave direction
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,ri,rj,rk,xi,xj,xk,thi,thj,thk,oi,oj,ok,qi,qj,qk,&
         rij,rik,rjk,sqijk,sqikj,sqjki,zconst
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    del1 = 10.**(-12)
    zconst=1./(4*sqrt(2.))
    ri = xi
    rj = xj
    rk = xk
    oi=omeg(ri)+del1
    oj=omeg(rj)+del1
    ok=omeg(rk)+del1
    qi=oi**2/g
    qj=oj**2/g
    qk=ok**2/g
    rij = ri*rj*cos(thj-thi)
    rik = ri*rk*cos(thk-thi)
    rjk = rj*rk*cos(thk-thj)
    sqijk=sqrt(g*ok/(oi*oj))
    sqikj=sqrt(g*oj/(oi*ok))
    sqjki=sqrt(g*oi/(oj*ok))
    vmin=zconst*( (rij-qi*qj)*sqijk + (rik-qi*qk)*sqikj&
         + (rjk+qj*qk)*sqjki )
    return
  end function vmin
  !
  !>
  !> @brief determines the third-order transfer coefficient for four
  !>        wave interactions of gravity waves.
  !>
  !> @param xi   wave number
  !> @param xj   wave number
  !> @param xk   wave number
  !> @param xl   wave number
  !> @param thi
  !> @param thj
  !> @param thk
  !> @param thl
  !> @returns u
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function u(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *u(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *u*  determines the third-order transfer coefficient for four
    !              wave interactions of gravity waves.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *u(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real xi,xj,xk,xl,thi,thj,thk,thl,oi,oj,ok,ol,xik,xjk,xil,xjl,&
         oik,ojk,oil,ojl,qi,qj,qik,qjk,qil,qjl,sqijkl,zconst
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    zconst=1./(16.)
    oi=omeg(xi)
    oj=omeg(xj)
    ok=omeg(xk)
    ol=omeg(xl)
    xik = vabs(xi,xk,thi,thk)
    xjk = vabs(xj,xk,thj,thk)
    xil = vabs(xi,xl,thi,thl)
    xjl = vabs(xj,xl,thj,thl)
    oik=omeg(xik)
    ojk=omeg(xjk)
    oil=omeg(xil)
    ojl=omeg(xjl)
    qi=oi**2/g
    qj=oj**2/g
    qik=oik**2/g
    qjk=ojk**2/g
    qil=oil**2/g
    qjl=ojl**2/g
    sqijkl=sqrt(ok*ol/(oi*oj))
    u = zconst*sqijkl*( 2.*(xi**2*qj+xj**2*qi)-qi*qj*(&
         qik+qjk+qil+qjl) )
    return
  end function u
  !
  !>
  !> @brief determines the contribution of the direct four-wave
  !>        interactions of gravity waves of the type a_2^*a_3a_4.
  !>
  !> @param   xi   wave number
  !> @param   xj   wave number
  !> @param   xk   wave number
  !> @param   xl   wave number
  !> @param   thi
  !> @param   thj
  !> @param   thk
  !> @param   thl
  !> @returns w2
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function w2(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *w2(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *w2*  determines the contribution of the direct four-wave
    !              interactions of gravity waves of the type
    !              a_2^*a_3a_4.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *w(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    real xi,xj,xk,xl,thi,thj,thk,thl
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    w2= u(xi,xj,xk,xl,thi-pi,thj-pi,thk,thl)+&
         u(xk,xl,xi,xj,thk,thl,thi-pi,thj-pi)-&
         u(xk,xj,xi,xl,thk,thj-pi,thi-pi,thl)-&
         u(xi,xk,xj,xl,thi-pi,thk,thj-pi,thl)-&
         u(xi,xl,xk,xj,thi-pi,thl,thk,thj-pi)-&
         u(xl,xj,xk,xi,thl,thj-pi,thk,thi-pi)
    return
  end function w2
  !
  !>
  !> @brief determines the contribution of the virtual
  !>        four-wave interactions of gravity waves.
  !>
  !> @param    xi    wave number
  !> @param    xj    wave number
  !> @param    xk    wave number
  !> @param    xl    wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  v2
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function v2(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *v2(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *v2*  determines the contribution of the virtual
    !           four-wave interactions of gravity waves.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity waves in the
    !              ideal case of no current. (cf.zakharov,and
    !              crawford et al)
    !
    !     interface.
    !     ----------
    !              *v2(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    logical doublep
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,xi,xj,xk,xl,thi,thj,thk,thl,oi,oj,ok,ol,ri,rj,rk,rl,&
         rij,rik,rli,rjl,rjk,rkl,thij,thik,thli,thjl,thjk,thkl,oij,&
         oik,ojl,ojk,oli,okl,xnik,xnjl,xnjk,xnil,ynil,ynjk,ynjl,ynik,&
         znij,znkl,zpij,zpkl,thlj,thil,thkj,thki,thji,thlk
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    if (doublep) then
      del1=10.**(-5)
    else
      del1=10.**(-2)
    endif
    ri=xi+del1
    rj=xj+del1/2.
    rk=xk+del1/3.
    rl=xl+del1*(1.+1./2.-1./3.)
    oi=omeg(ri)
    oj=omeg(rj)
    ok=omeg(rk)
    ol=omeg(rl)
    rij  = vabs(ri,rj,thi,thj)
    thij = vdir(ri,rj,thi,thj)
    rik  = vabs(ri,rk,thi,thk-pi)
    thik = vdir(ri,rk,thi,thk-pi)
    rli  = vabs(rl,ri,thl,thi-pi)
    thli = vdir(xl,xi,thl,thi-pi)
    rjl  = vabs(rj,rl,thj,thl-pi)
    thjl = vdir(rj,rl,thj,thl-pi)
    rjk  = vabs(rj,rk,thj,thk-pi)
    thjk = vdir(rj,rk,thj,thk-pi)
    rkl  = vabs(rk,rl,thk,thl)
    thkl = vdir(rk,rl,thk,thl)
    oij=omeg(rij)
    oik=omeg(rik)
    ojl=omeg(rjl)
    ojk=omeg(rjk)
    oli=omeg(rli)
    okl=omeg(rkl)
    xnik = ok+oik-oi
    xnjl = oj+ojl-ol
    xnjk = ok+ojk-oj
    xnil = oi+oli-ol
    ynil = ol+oli-oi
    ynjk = oj+ojk-ok
    ynjl = ol+ojl-oj
    ynik = oi+oik-ok
    znij = oij-oi-oj
    znkl = okl-ok-ol
    zpij = oij+oi+oj
    zpkl = okl+ok+ol
    thlj = thjl-pi
    thil = thli-pi
    thkj = thjk-pi
    thki = thik-pi
    thji = thij-pi
    thlk = thkl-pi
    v2= vmin(ri,rk,rik,thi,thk,thik)*vmin(rl,rj,rjl,thl,thj,thlj)*&
         (1./xnik+1./xnjl)&
         +vmin(rj,rk,rjk,thj,thk,thjk)*vmin(rl,ri,rli,thl,thi,thli)*&
         (1./xnjk+1./xnil)&
         +vmin(ri,rl,rli,thi,thl,thil)*vmin(rk,rj,rjk,thk,thj,thkj)*&
         (1./ynil+1./ynjk)&
         +vmin(rj,rl,rjl,thj,thl,thjl)*vmin(rk,ri,rik,thk,thi,thki)*&
         (1./ynjl+1./ynik)&
         +vmin(rij,ri,rj,thij,thi,thj)*vmin(rkl,rk,rl,thkl,thk,thl)*&
         (1./znij+1./znkl)&
         +vplus(rij,ri,rj,thji,thi,thj)*vplus(rkl,rk,rl,thlk,thk,thl)*&
         (1./zpij+1./zpkl)
    v2 = -v2
    return
  end function v2
  !
  !>
  !> @brief determines the nonlinear transfer coefficient for four wave
  !>        interactions of gravity waves of the type a_2a_3a_4.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    xl   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  w1
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function w1(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *w1(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *w1*  determines the nonlinear transfer coefficient for four
    !              wave interactions of gravity waves of the type
    !              a_2a_3a_4.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *w1(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real xi,xj,xk,xl,thi,thj,thk,thl
    !
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    w1= -u(xi,xj,xk,xl,thi-pi,thj,thk,thl)-&
         u(xi,xk,xj,xl,thi-pi,thk,thj,thl)-&
         u(xi,xl,xj,xk,thi-pi,thl,thj,thk)+&
         u(xj,xk,xi,xl,thj,thk,thi-pi,thl)+&
         u(xj,xl,xi,xk,thj,thl,thi-pi,thk)+&
         u(xk,xl,xi,xj,thk,thl,thi-pi,thj)
    w1=w1/3.
    return
  end function w1
  !
  !>
  !> @brief determines the nonlinear transfer coefficient for four wave
  !>        interactions of gravity waves of the type a_^*a_3^*a_4^*.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    xl   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  w4
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function w4(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *w4(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *w4*  determines the nonlinear transfer coefficient for four
    !              wave interactions of gravity waves of the type
    !              a_^*a_3^*a_4^*.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *w4(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real xi,xj,xk,xl,thi,thj,thk,thl
    !
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    w4= u(xi,xj,xk,xl,thi,thj,thk,thl)+&
         u(xi,xk,xj,xl,thi,thk,thj,thl)+&
         u(xi,xl,xj,xk,thi,thl,thj,thk)+&
         u(xj,xk,xi,xl,thj,thk,thi,thl)+&
         u(xj,xl,xi,xk,thj,thl,thi,thk)+&
         u(xk,xl,xi,xj,thk,thl,thi,thj)
    w4=w4/3.
    return
  end function w4
  !>
  !> @brief weights of the a_2^*a_3^*a_4 part of the canonical transformation.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    xl   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  b3
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function b3(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *b3(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *b3*  weights of the a_2^*a_3^*a_4 part of the
    !           canonical transformation.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *b3(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    logical doublep
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,xi,xj,xk,xl,thi,thj,thk,thl,oi,oj,ok,ol,ri,rj,rk,rl,&
         rij,rji,rik,rki,rlj,rjl,rjk,rkj,rli,ril,rlk,rkl,thij,thji,&
         thik,thki,thlj,thjl,thjk,thkj,thli,thil,thlk,thkl,zijkl
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    if (doublep) then
      del1=10.**(-5)
    else
      del1=0.01
    endif
    ri=xi
    rj=xj
    rk=xk
    rl=xl
    oi=omeg(ri)+del1
    oj=omeg(rj)+del1
    ok=omeg(rk)+del1
    ol=omeg(rl)+del1
    rij  = vabs(ri,rj,thi,thj)
    thij = vdir(ri,rj,thi,thj)
    rji  = vabs(rj,ri,thj,thi)
    thji = vdir(rj,ri,thj,thi)
    rik  = vabs(ri,rk,thi,thk)
    thik = vdir(ri,rk,thi,thk)
    rki  = vabs(rk,ri,thk,thi)
    thki = vdir(rk,ri,thk,thi)
    rlj  = vabs(rl,rj,thl,thj-pi)
    thlj = vdir(rl,rj,thl,thj-pi)
    rjl  = vabs(rj,rl,thj,thl-pi)
    thjl = vdir(rj,rl,thj,thl-pi)
    rjk  = vabs(rj,rk,thj,thk)
    thjk = vdir(rj,rk,thj,thk)
    rkj  = vabs(rk,rj,thk,thj)
    thkj = vdir(rk,rj,thk,thj)
    rli  = vabs(rl,ri,thl,thi-pi)
    thli = vdir(rl,ri,thl,thi-pi)
    ril  = vabs(ri,rl,thi,thl-pi)
    thil = vdir(ri,rl,thi,thl-pi)
    rlk  = vabs(rl,rk,thl,thk-pi)
    thlk = vdir(rl,rk,thl,thk-pi)
    rkl  = vabs(rk,rl,thk,thl-pi)
    thkl = vdir(rk,rl,thk,thl-pi)
    zijkl = oi+oj+ok-ol
    b3= -1./zijkl*(2.*( &
         vmin(rl,ri,rli,thl,thi,thli)*a1(rjk,rj,rk,thjk,thj,thk)&
         -vmin(rij,ri,rj,thij,thi,thj)*a1(rl,rk,rlk,thl,thk,thlk)&
         -vmin(rik,ri,rk,thik,thi,thk)*a1(rl,rj,rlj,thl,thj,thlj)&
         -vplus(rj,ri,rji,thj,thi,thji-pi)*a1(rk,rl,rkl,thk,thl,thkl)&
         -vplus(rk,ri,rki,thk,thi,thki-pi)*a1(rj,rl,rjl,thj,thl,thjl)&
         +vmin(ri,rl,ril,thi,thl,thil)*a3(rj,rk,rjk,thj,thk,thjk-pi))&
         +3.*w1(rl,rk,rj,ri,thl,thk,thj,thi) )
    return
  end function b3
  !
  !>
  !> @brief weights of the a_2^*a_3^*a_4^* part of the canonical
  !>        transformation.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    xl   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  b4
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function b4(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *b4(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *b4*  weights of the a_2^*a_3^*a_4^* part of the canonical
    !           transformation.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *b4(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    logical doublep
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,xi,xj,xk,xl,thi,thj,thk,thl,oi,oj,ok,ol,ri,rj,rk,rl,&
         rij,rik,ril,rjl,rjk,rkl,thij,thik,thil,thjl,thjk,thlk,thkl,&
         zijkl
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    ri=xi
    rj=xj
    rk=xk
    rl=xl
    oi=omeg(ri)
    oj=omeg(rj)
    ok=omeg(rk)
    ol=omeg(rl)
    rij  = vabs(ri,rj,thi,thj)
    thij = vdir(ri,rj,thi,thj)
    rik  = vabs(ri,rk,thi,thk)
    thik = vdir(ri,rk,thi,thk)
    ril  = vabs(ri,rl,thi,thl)
    thil = vdir(ri,rl,thi,thl)
    rjl  = vabs(rj,rl,thj,thl)
    thjl = vdir(rj,rl,thj,thl)
    rjk  = vabs(rj,rk,thj,thk)
    thjk = vdir(rj,rk,thj,thk)
    rkl  = vabs(rk,rl,thk,thl)
    thkl = vdir(rk,rl,thk,thl)
    zijkl = oi+oj+ok+ol
    b4= -1./zijkl*(2./3.*(    &
         vplus(rij,ri,rj,thij-pi,thi,thj)*a1(rkl,rk,rl,thkl,thk,thl)&
         +vplus(rik,ri,rk,thik-pi,thi,thk)*a1(rjl,rj,rl,thjl,thj,thl)&
         +vplus(ril,ri,rl,thil-pi,thi,thl)*a1(rjk,rj,rk,thjk,thj,thk)&
         +vmin(rik,ri,rk,thik,thi,thk)*a3(rjl,rj,rl,thjl-pi,thj,thl)&
         +vmin(ril,ri,rl,thil,thi,thl)*a3(rjk,rj,rk,thjk-pi,thj,thk)&
         +vmin(rij,ri,rj,thij,thi,thj)*a3(rkl,rk,rl,thkl-pi,thk,thl) )&
         +w4(ri,rj,rk,rl,thi,thj,thk,thl) )
    return
  end function b4
  !
  !>
  !> @brief weights of the a_2a_3a_4 part of the canonical
  !>        transformation.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    xl   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  b1
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function b1(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *b1(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *b1*  weights of the a_2a_3a_4 part of the canonical
    !           transformation.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *b1(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    logical doublep
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,xi,xj,xk,xl,thi,thj,thk,thl,oi,oj,ok,ol,ri,rj,rk,rl,&
         rij,rji,rik,rki,rjl,rjk,rli,ril,rkl,thij,thji,&
         thik,thki,thjl,thjk,thli,thil,thkl,zijkl
    !
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    ri=xi
    rj=xj
    rk=xk
    rl=xl
    oi=omeg(ri)
    oj=omeg(rj)
    ok=omeg(rk)
    ol=omeg(rl)
    rij  = vabs(ri,rj,thi,thj-pi)
    thij = vdir(ri,rj,thi,thj-pi)
    rji  = vabs(rj,ri,thj,thi-pi)
    thji = vdir(rj,ri,thj,thi-pi)
    rik  = vabs(ri,rk,thi,thk-pi)
    thik = vdir(ri,rk,thi,thk-pi)
    rki  = vabs(rk,ri,thk,thi-pi)
    thki = vdir(rk,ri,thk,thi-pi)
    ril  = vabs(ri,rl,thi,thl-pi)
    thil = vdir(ri,rl,thi,thl-pi)
    rli  = vabs(rl,ri,thl,thi-pi)
    thli = vdir(rl,ri,thl,thi-pi)
    rjl  = vabs(rj,rl,thj,thl)
    thjl = vdir(rj,rl,thj,thl)
    rjk  = vabs(rj,rk,thj,thk)
    thjk = vdir(rj,rk,thj,thk)
    rkl  = vabs(rk,rl,thk,thl)
    thkl = vdir(rk,rl,thk,thl)
    zijkl = oi-oj-ok-ol
    b1= -1./zijkl*(2./3.*(    &
         min(ri,rj,rij,thi,thj,thij)*a1(rkl,rk,rl,thkl,thk,thl)&
         +vmin(ri,rk,rik,thi,thk,thik)*a1(rjl,rj,rl,thjl,thj,thl)&
         +vmin(ri,rl,ril,thi,thl,thil)*a1(rjk,rj,rk,thjk,thj,thk)&
         +vmin(rk,ri,rki,thk,thi,thki)*a3(rjl,rj,rl,thjl-pi,thj,thl)&
         +vmin(rl,ri,rli,thl,thi,thli)*a3(rjk,rj,rk,thjk-pi,thj,thk)&
         +vmin(rj,ri,rji,thj,thi,thji)*a3(rkl,rk,rl,thkl-pi,thk,thl) &
         ) +w1(ri,rj,rk,rl,thi,thj,thk,thl) )
    return
  end function b1
  !
  !>
  !> @brief weights of the a_2^*a_3a_4 part of the canonical
  !>        transformation.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    xl   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @param    thl
  !> @returns  b2
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function b2(xi,xj,xk,xl,thi,thj,thk,thl)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *b2(xi,xj,xk,xl,thi,thj,thk,thl)
    !
    !-----------------------------------------------------------------------
    !
    !***  *b2*  weights of the a_2^*a_3a_4 part of the canonical
    !           transformation.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for four
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov,and crawford et al)
    !
    !     interface.
    !     ----------
    !              *b2(xi,xj,xk,xl)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !                      *xl*  - wave number
    !     method.
    !     -------
    !              none
    !
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    logical doublep
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,xi,xj,xk,xl,thi,thj,thk,thl,oi,oj,ok,ol,ri,rj,rk,rl,&
         rij,rik,rki,rjl,rlj,rjk,rkj,rli,ril,rkl,thij,&
         thik,thki,thjl,thlj,thjk,thkj,thli,thil,thkl,zijkl
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    ri=xi
    rj=xj
    rk=xk
    rl=xl
    rij  = vabs(ri,rj,thi,thj)
    thij = vdir(ri,rj,thi,thj)
    rik  = vabs(ri,rk,thi,thk-pi)
    thik = vdir(ri,rk,thi,thk-pi)
    rki  = vabs(rk,ri,thk,thi-pi)
    thki = vdir(rk,ri,thk,thi-pi)
    ril  = vabs(ri,rl,thi,thl-pi)
    thil = vdir(ri,rl,thi,thl-pi)
    rli  = vabs(rl,ri,thl,thi-pi)
    thli = vdir(rl,ri,thl,thi-pi)
    rjl  = vabs(rj,rl,thj,thl-pi)
    thjl = vdir(rj,rl,thj,thl-pi)
    rlj  = vabs(rl,rj,thl,thj-pi)
    thlj = vdir(rl,rj,thl,thj-pi)
    rjk  = vabs(rj,rk,thj,thk-pi)
    thjk = vdir(rj,rk,thj,thk-pi)
    rkj  = vabs(rk,rj,thk,thj-pi)
    thkj = vdir(rk,rj,thk,thj-pi)
    rkl  = vabs(rk,rl,thk,thl)
    thkl = vdir(rk,rl,thk,thl)
    b2=  a3(ri,rj,rij,thi,thj,thij-pi)*a3(rk,rl,rkl,thk,thl,thkl-pi)&
         +a1(rj,rk,rjk,thj,thk,thjk)*a1(rl,ri,rli,thl,thi,thli)&
         +a1(rj,rl,rjl,thj,thl,thjl)*a1(rk,ri,rki,thk,thi,thki)&
         -a1(rij,ri,rj,thij,thi,thj)*a1(rkl,rk,rl,thkl,thk,thl)&
         -a1(ri,rk,rik,thi,thk,thik)*a1(rl,rj,rlj,thl,thj,thlj)&
         -a1(ri,rl,ril,thi,thl,thil)*a1(rk,rj,rkj,thk,thj,thkj)
    return
  end function b2
  !
  !>
  !> @brief auxiliary second-order coefficient.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @returns  a1
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function a1(xi,xj,xk,thi,thj,thk)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *a1(xi,xj,xk,thi,thj,thk)
    !
    !-----------------------------------------------------------------------
    !
    !***  *a1*  auxiliary second-order coefficient.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *vmin(xi,xj,xk)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    common/precis/doublep
    logical doublep
    integer mdw
    real depth,alpha,gam_j,deptha,depthd
    real del1,xi,xj,xk,thi,thj,thk,oi,oj,ok
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    if (doublep) then
      del1 = 10.**(-8)
    else
      del1 = 10.**(-4)
    endif
    oi=omeg(xi)+del1
    oj=omeg(xj)+del1
    ok=omeg(xk)+del1
    a1 = -vmin(xi,xj,xk,thi,thj,thk)/(oi-oj-ok)
    return
  end function a1
  !
  !>
  !> @brief auxiliary second-order function.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @returns  a2
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function a2(xi,xj,xk,thi,thj,thk)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *a2(xi,xj,xk,thi,thj,thk)
    !
    !-----------------------------------------------------------------------
    !
    !***  *a2*  auxiliary second-order function.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *vmin(xi,xj,xk)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    real del1,xi,xj,xk,thi,thj,thk
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    a2 = -2.*a1(xk,xj,xi,thk,thj,thi)
    return
  end function a2
  !
  !>
  !> @brief auxiliary second-order function.
  !>
  !> @param    xi   wave number
  !> @param    xj   wave number
  !> @param    xk   wave number
  !> @param    thi
  !> @param    thj
  !> @param    thk
  !> @returns  a3
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function a3(xi,xj,xk,thi,thj,thk)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *a3(xi,xj,xk,thi,thj,thk)
    !
    !-----------------------------------------------------------------------
    !
    !***  *a3*  auxiliary second-order function.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives nonlinear transfer coefficient for three
    !              wave interactions of gravity-capillary waves in the
    !              ideal case of no current. (cf.zakharov)
    !
    !     interface.
    !     ----------
    !              *vmin(xi,xj,xk)*
    !                      *xi*  - wave number
    !                      *xj*  - wave number
    !                      *xk*  - wave number
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/precis/doublep
    logical doublep
    real del1,oi,oj,ok,xi,xj,xk,thi,thj,thk
    !
    !***  1. determine nonlinear transfer.
    !     --------------------------------
    !
    if (doublep) then
      del1 = 10.**(-8)
    else
      del1 = 10.**(-4)
    endif
    oi=omeg(xi)+del1
    oj=omeg(xj)+del1
    ok=omeg(xk)+del1
    a3 = -vplus(xi,xj,xk,thi,thj,thk)/(oi+oj+ok)
    return
  end function a3
  !
  !>
  !> @brief determines the dispersion relation for gravity
  !>        waves.
  !>
  !> @param    x     wave number
  !> @returns  omeg
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function omeg(x)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *omeg(x)*
    !
    !-----------------------------------------------------------------------
    !
    !
    !***  *omeg*   determines the dispersion relation for gravity
    !              waves.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives dispersion relation for gravity-
    !              waves in the ideal case of no current.
    !
    !     interface.
    !     ----------
    !              *omeg(x)*
    !                      *x*  - wave number
    !
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,depthd
    real d,xk,x,t
    d = depth
    xk = abs(x)
    t = tanh(xk*d)
    omeg=sqrt(g*xk*t)
    return
  end function omeg
  !
  !>
  !> @brief determines the group velocity for gravity- waves.
  !>
  !> @param    x   wave number
  !> @returns  vg
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function vg(x)
    !-----------------------------------------------------------------------
    !
    !***  *real function* *vg(x)*
    !
    !-----------------------------------------------------------------------
    !
    !***  *vg*   determines the group velocity for gravity- waves.
    !
    !     peter janssen
    !
    !     purpose.
    !     --------
    !
    !              gives group velocity for gravity-
    !              waves in the ideal case of no current.
    !
    !     interface.
    !     ----------
    !              *vg(x)*
    !              *x*  - wave number
    !
    !     method.
    !     -------
    !              none
    !
    !     externals.
    !     ----------
    !              none.
    !
    !-----------------------------------------------------------------------
    !
    implicit none
    common/const/depth,alpha,mdw,gam_j,depthd
    integer mdw
    real depth,alpha,gam_j,depthd
    real d,xk,x,xd
    d = depth
    xk = abs(x)
    xd = xk*depth
    vg = 0.5*sqrt(g*tanh(xd)/xk)*(1.+2.*xd/sinh(2.*xd))
    return
  end function vg
  !---------------------------------------------------------------------
  !>
  !> @brief gives the wavenumber.
  !>
  !> @param   om
  !> @param   beta
  !> @returns aki
  !>
  !> @author peter janssen
  !> @date   na
  !>
  real function aki(om,beta)
    ! this function gives the wavenumber ...
    !---------------------------------------------------------------------
    !
    implicit none
    real om,beta,g,ebs,akm1,akm2,ao,akp,bo,th,sth
    g =9.806
    ebs=0.0001
    akm1=om**2/(4.*g )
    akm2=om/(2.*sqrt(g*beta))
    ao=max(akm1,akm2)
10  continue
    akp=ao
    bo=beta*ao
    !     if (bo.gt.10) go to 20
    if (bo.gt.20.) go to 20
    th=g*ao*tanh(bo)
    sth=sqrt(th)
    ao=ao+(om-sth)*sth*2./(th/ao+g*bo/cosh(bo)**2)
    if (abs(akp-ao).gt.ebs*ao) go to 10
    aki=ao
    return
20  continue
    aki=om**2/g
    return
  end function aki
  !
  !>
  !> @brief na.
  !>
  !> @param   xi
  !> @param   xj
  !> @param   thi
  !> @param   thj
  !> @returns vabs
  !>
  !> @author na
  !> @date   na
  !>
  real function vabs(xi,xj,thi,thj)
    !
    !---------------------------------------------------------------------
    !
    implicit none
    real xi,xj,thi,thj,arg
    arg = xi**2+xj**2+2.*xi*xj*cos(thi-thj)
    if (arg.le.0.) then
      vabs = 0.
    else
      vabs = sqrt(arg)
    endif
    return
  end function vabs
  !
  !>
  !> @brief na.
  !>
  !> @param   xi
  !> @param   xj
  !> @param   thi
  !> @param   thj
  !> @returns vdir
  !>
  !> @author na
  !> @date   na
  !>
  real function vdir(xi,xj,thi,thj)
    !
    !---------------------------------------------------------------------
    !
    implicit none
    real xi,xj,thi,thj,eps,y,x
    eps = 0.
    y = xj*sin(thj-thi)
    x = xi+xj*cos(thj-thi)+eps
    vdir = atan2(y,x)+thi
    if (x.eq.0.) vdir = 0.
    return
  end function vdir
  !/
  !/ end of module w3canomd -------------------------------------------- /
  !/
end module w3canomd
