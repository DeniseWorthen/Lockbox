!> @file
!> @brief computes scattering term.
!>
!> @author f. ardhuin
!> @date   14-nov-2010
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
!>
!> @brief this module computes a scattering term
!>  based on the theory by ardhuin and magne (jfm 2007).
!>
!> @author f. ardhuin
!> @date   14-nov-2010
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
!/ ------------------------------------------------------------------- /
module w3sbs1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii                shom |
  !/                  |            f. ardhuin             |
  !/                  |                        fortran 90 |
  !/                  | last update :         14-nov-2010 |
  !/                  +-----------------------------------+
  !/
  !/    15-jul-2005 : origination.                        ( version 3.07 )
  !/    23-jun-2006 : formatted for submitting code for   ( version 3.09 )
  !/                  inclusion in wavewatch iii.
  !/    10-may-2007 : adapt from version 2.22.shom        ( version 3.10.shom )
  !/    14-nov-2010 : include scaling factor and clean up ( version 3.14 )
  !/
  !  1. purpose :
  !
  !     this module computes a scattering term
  !     based on the theory by ardhuin and magne (jfm 2007)
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
  !      w3sbs1    subr. public   bottom scattering
  !      insbs1    subr. public   corresponding initialization routine.
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
  public
  !/
  !/ public variables
  !/
  real, dimension(:,:), allocatable   :: botspec
  integer, parameter :: nkscat = 30                 !number of wavenumbers
  double precision  ,dimension(:,:,:) ,  allocatable :: scatmatv !scattering matrices
  double precision  ,dimension(:,:,:) ,  allocatable :: scatmata !original matrix
  double precision  ,dimension(:,:)   ,  allocatable :: scatmatd
  character(len=10)                   :: botspec_indicator
  integer            :: nkbx, nkby
  real               :: dkbx, dkby, kwmin, kwmax
  real, parameter    :: scattcutoff=0.
  real               :: curtx, curty
  !/
contains
  !>
  !> @brief bottom scattering source term.
  !>
  !> @details without current, goes through a diagonalization of the matrix
  !>  problem  s(f,:) = m(f,:,:)**e(f,:).
  !>  with current, integrates the source term along the resonant locus.
  !> @param[in] a         action density spectrum (1-d)
  !> @param[in] cg        group velocities
  !> @param[in] wn        wavenumbers
  !> @param[in] depth     mean water depth
  !> @param[in] cx1       current components at isea
  !> @param[in] cy1       current components at isea
  !> @param[out] tauscx   change of wave momentum due to scattering
  !> @param[out] tauscy   change of wave momentum due to scattering
  !> @param[out] s        source term (1-d version)
  !> @param[out] d        diagonal term of derivative (1-d version)
  !>
  !> @author f. ardhuin
  !> @date   23-jun-2006
  !>
  subroutine w3sbs1(a, cg, wn, depth, cx1, cy1,      &
       tauscx, tauscy, s, d)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    15-jul-2005 : origination.                        ( version 3.07 )
    !/    23-jun-2006 : formatted for submitting code for   ( version 3.09 )
    !/                  inclusion in wavewatch iii.
    !/
    !  1. purpose :
    !
    !     bottom scattering source term
    !
    !  2. method :
    !
    !     without current, goes through a diagonalization of the matrix
    !     problem  s(f,:) = m(f,:,:)**e(f,:)
    !     with current, integrates the source term along the resonant locus
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !  a         r.a.  i   action density spectrum (1-d)
    !       cg        r.a.  i   group velocities.
    !       wn        r.a.  i   wavenumbers.
    !       depth     real  i   mean water depth.
    !       s         r.a.  o   source term (1-d version).
    !       d         r.a.  o   diagonal term of derivative (1-d version).
    !       cx1-y1    r.a.  i   current components at isea.
    !       tauscx-y  r.a.  i   change of wave momentum due to scattering
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
    use w3gdatmd, only: nk, nth, nspec, sig, dth, dden, &
         ecos, esin, ec2, mapth, mapwn, &
         sig2, dsii
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: cg(nk), wn(nk), depth
    real, intent(in)        :: a(nth,nk)
    real, intent(in)        :: cx1, cy1
    real, intent(out)       :: tauscx, tauscy
    real, intent(out)       :: s(nspec), d(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer         :: ispec, ik, nscut, ith, ith2, i, j,iajust,iajust2
    logical, save   :: first = .true.
    integer         :: matrices = 0
    real            :: r1, r2, r3
    real            :: wn2(nspec, nth), ka(nspec),        &
         kb(nspec, nth), wnbot(nspec, nth), &
         b(nspec, nth)
    real            :: kbotxi, kbotyi, xbk,   &
         ybk,integral, kbotx, kboty, count,count2
    integer         :: ibk, jbk, ik2
    real            ::  sigp,ku, kpu, cgk, cgpk, wn2i, xk2, ap, kcutoff, ecc2, &
         variance , integral1,integral1b,integral2, sb(nk,nth), integral3,&
         ajust,absajust,aa,bb,lnorm,udotl,kdotkp,mbandc
    real            :: kd, kfactor, kscaled, kmod , checksum, etot
    real            :: smatrix(nth,nth),smatrix2(nth,nth)
    double precision :: avect(nth)
    curtx=cx1
    curty=cy1
    count=0
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
      call insbs1( 1 )
      first  = .false.
    end if
    if (( (abs(cx1)+abs(cy1)).eq.0.).and.(matrices.eq.0) ) then
      kwmin=max(max(dkbx,dkby),sig(1)**2/grav)
      kwmax=min(nkbx*dkbx,nkby*dkby)*0.25
      write(*,*) 'k range:',kwmin,kwmax,sig(1)**2/grav
      call insbs1( 2 )
      matrices  = 1
    end if
    !
    ! 1.  sets scattering term to zero
    !
    d = 0.
    s = 0.
    tauscx=0.
    tauscy=0.
    !
    ! 3.  bottom scattering ================================================== *
    !
    if ( depth*wn(1) .le. 6 ) then
      !
      ! 3.a ardhuin and herbers jfm 2000: no current
      !
      if ((abs(cx1)+abs(cy1).eq.0.).and.(matrices.eq.1)) then
        do ik=1,nk
          kd=wn(ik)*depth
          if ( kd .le. 6 .and.wn(ik).lt.kwmax ) then
            ! test on kwmax means that scattering is not computed if interaction goes beyond the shortest resolved
            ! bottom component. this should probably be replaced by a warning...
            kfactor=(wn(ik)**4)*sig(ik)*pi*4.             &
                 /(sinh(2*kd)*(2*kd+sinh(2*kd)))
            kscaled=(nkscat-2)*(wn(ik)-kwmin)/(kwmax-kwmin)
            avect=dble(a(:,ik))
            if (kscaled.lt.0) then
              ibk=0
              kmod=0.
            else
              ibk=int(kscaled)
              kmod=mod(kscaled,1.0)
            end if
            s((ik-1)*nth+1:ik*nth)                              &
                 =real(matmul(scatmatv(ibk,:,:),kfactor*scatmatd(ibk,:) &
                 *matmul(transpose(scatmatv(ibk,:,:)),avect))*(1.-kmod))
            s((ik-1)*nth+1:ik*nth)                              &
                 =s((ik-1)*nth+1:ik*nth)                           &
                 +real(matmul(scatmatv(ibk+1,:,:),kfactor*scatmatd(ibk+1,:) &
                 *matmul(transpose(scatmatv(ibk+1,:,:)),avect))*kmod)
            checksum=abs(sum(s((ik-1)*nth+1:ik*nth) ))
            etot=sum(a(:,ik))
            if (checksum.gt.0.01*etot) write(*,*)         &
                 'energy not conserved:',ik,depth,checksum,etot
          else
            s((ik-1)*nth+1:ik*nth)=0.
          end if
        end do
      else
        ! 3.b
        !  case with current (ardhuin and magne jfm 2007)
        ! compute k' (wn2) from k (wn) and u (cx1, cy1)
        ! using : k'=(cg+k.u/k)/(cg+k'.u/k')
        !
        do ith2=1, nth
          do ispec=1, nspec
            ku=cx1 * ecos(mapth(ispec))+cy1 * esin(mapth(ispec))
            kpu=cx1 * ecos(ith2)+  cy1 * esin(ith2)
            cgk=cg(mapwn(ispec))
            if ((cgk+kpu).lt.0.1*cgk) kpu=-0.9*cgk
            if ((cgk+ku).lt.0.1*cgk)  ku=-0.9*cg(mapwn(ispec))
            wn2(ispec,ith2)= wn(mapwn(ispec))*(cgk+ku)/(cgk+kpu)
          end do
        end do
        !
        ! 3.c compute the coupling coefficient as a product of two terms
        !
        !   k=0.5*pi k'^2 * m(k,k')^2 / [sig*sig' *(k'*cg'+k'.u)]
        !                                      (magne and ardhuin jfm 2007)
        !
        !   k=ka(k)*kb(k,k',theta')
        !
        !   ka = ...
        !   here mc is neglected
        !
        do ispec=1, nspec
          ka(ispec)= 4*pi*sig2(ispec) * wn(mapwn(ispec))  /    &
               sinh(min(2*wn(mapwn(ispec))*depth,20.))
          do ith2=1, nth
            ku=cx1 * ecos(mapth(ispec))+cy1 * esin(mapth(ispec))
            kpu=cx1 * ecos(ith2)+  cy1 * esin(ith2)
            sigp=sqrt(grav*wn2(ispec,ith2)*tanh(wn2(ispec,ith2)*depth))
            cgpk=sigp*(0.5+wn2(ispec,ith2)*depth &
                 /sinh(min(2*wn2(ispec,ith2)*depth,20.)))/wn2(ispec, ith2)
            kb(ispec, ith2)= wn2(ispec, ith2)**3      &
                 *ec2(1+abs(mapth(ispec)-ith2)) /        &
                 (                                      &
                 2*wn2(ispec, ith2)*depth +             &
                 sinh(min(2*wn2(ispec,ith2)*depth,20.)) &
                 *(1+wn2(ispec,ith2)*kpu*2/sigp)  &
                 )
            !
            !  other option for computing also mc
            !
            !  udotl=wn(mapwn(ispec))*ku-kpu*wn2(ispec,ith2)
            !  kdotkp=ec(1+abs(mapth(ispec)-ith2))*wn2(ispec,ith2)*wn(mapwn(ispec))
            !  lnorm=sqrt(wn(mapwn(ispec))**2+wn2(ispec, ith2)**2-2*kdotkp)
            !  mbandc=grav*kdotkp &
            !        /(cosh(min(wn2(ispec,ith2)*depth,20.))*cosh(min(wn(mapwn(ispec))*depth,20.)
            !        +(udotl*(sigp*(wn(mapwn(ispec))**2-kdotkp)+sig2(ispec)*(kdotkp-wn2(ispec, ith2)**2)) &
            !           - udotl**2*(kdotkp-sigp*sig2(ispec)*(sigp*sig2(ispec)+udotl**2)/grav**2)) &
            !           /(lnorm*(udotl**2/(grav*lnorm)-tanh(min(lnorm*depth,20.)))*cosh(min(lnorm*depth,20.)))
            !  kb(ispec,ith2)= wn2(ispec, ith2)**2
            !                   /((sig2(ispec)*sigp*wn2(ispec, ith2)*(cgpk+kpu)) &
            !                  *mbandc**2
            !
          end do
        end do
        !
        ! 3.a bilinear interpolation of the bottom spectrum botspec
        !     along the locus -> b(ispec,ith2)
        !
        b(:,:)=0
        do ispec=1, nspec
          kcutoff=scattcutoff*wn(mapwn(ispec))
          do ith2=1,nth
            kbotx=wn(mapwn(ispec))*ecos(mapth(ispec)) - &
                 wn2(ispec, ith2) * ecos(ith2)
            kboty=wn(mapwn(ispec))*esin(mapth(ispec)) - &
                 wn2(ispec, ith2) * esin(ith2)
            !
            ! 3.a.1 test if the bottom wavenumber is larger than the cutoff
            !        otherwise the interaction is set to zero
            if ((kbotx**2+kboty**2)>(kcutoff**2)) then
              kbotxi=real(nkbx-mod(nkbx,2))/2.+1.+kbotx/dkbx   ! the mod(nkbx,2) is either 1 or 0
              kbotyi=real(nkby-mod(nkby,2))/2.+1.+kboty/dkby   ! k=0 is at ik=(nkbx-1)/2+1 if kkbx is odd
              ibk=max(min(int(kbotxi),nkbx-1),1)
              xbk=mod(kbotxi,1.0)
              jbk=max(min(int(kbotyi),nkby-1),1)
              ybk=mod(kbotyi,1.0)
              b(ispec,ith2)=(                &
                   (botspec(ibk,jbk)*(1-ybk)+      &
                   botspec(ibk,jbk+1)*ybk)*(1-xbk) &
                   +               &
                   (botspec(ibk+1,jbk)*(1-ybk)+    &
                   botspec(ibk+1,jbk+1)*ybk)*xbk   &
                   )
            end if
          end do
        end do
        !
        ! 4. compute sbscat
        ! 4.a linear interpolation of a(k', theta') -> ap
        !
        !  4.b computation of the source term
        integral2=0.
        integral3=0.
        smatrix(:,:)=0.
        do ispec=1, nspec
          integral=0
          do ith2=1, nth
            iajust=1
            do i=2,nk
              if(wn2(ispec,ith2).ge.wn(i)) iajust=i
            end do
            iajust=max(iajust,1)
            iajust2=min(iajust+1,nk)
            if (iajust.eq.iajust2) then
              ap=a(ith2,iajust)
            else
              bb=(wn2(ispec,ith2)-wn(iajust))/(wn(iajust2)-wn(iajust))
              aa=(wn(iajust2)-wn2(ispec,ith2))/(wn(iajust2)-wn(iajust))
              ap=(a(ith2,iajust)*aa+a(ith2,iajust2)*bb)
            end if
            integral=integral + ka(ispec)*kb(ispec, ith2)*b(ispec,ith2)*               &
                 ( ap*wn(mapwn(ispec))/wn2(ispec,ith2)- a(mapth(ispec),mapwn(ispec))) *dth
            ! the factor wn/wn2 accounts for the fact that n(k) and n(k')
            ! have different jacobian transforms from kx,ky to k,theta
            integral1=integral1+kb(ispec, ith2)*b(ispec,ith2)*ap*wn(mapwn(ispec))/wn2(ispec,ith2)*dth &
                 *dth*dsii(mapwn(ispec))/cg(mapwn(ispec))
            integral1b=integral1b+kb(ispec, ith2)*b(ispec,ith2)*a(mapth(ispec),mapwn(ispec))*dth  &
                 *dth*dsii(mapwn(ispec))/cg(mapwn(ispec))
          end do
          s(ispec)=s(ispec)+integral
          integral2=integral2+s(ispec)*dth*dsii(mapwn(ispec))/cg(mapwn(ispec))
          integral3=integral3+abs(s(ispec))*dth*dsii(mapwn(ispec))/cg(mapwn(ispec))
        end do
      end if
    end if
    !/
    !/ end of w3sbs1 ----------------------------------------------------- /
    !/
  end subroutine w3sbs1
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief initialization for bottom scattering source term routine.
  !>
  !> @param[in] inistep
  !>
  !> @author f. ardhuin
  !> @date   23-jun-2006
  !>
  subroutine insbs1( inistep )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         23-jun-2006 |
    !/                  +-----------------------------------+
    !/
    !/    23-jun-2006 : origination.                        ( version 3.09 )
    !/
    !  1. purpose :
    !
    !     initialization for bottom scattering source term routine.
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
    !      w3sbs1    subr. w3sbs1md corresponding source term.
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
    use w3gdatmd, only: nk, nth, nspec, sig, dth, dden, ecos, esin
    use w3servmd, only: diagonalize
    !/
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)        :: inistep
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer         :: i, j, k1, k2, ik, jk, nrot
    real            :: kbotx, kboty, kcurr, kcutoff, variance
    real            :: kbotxi, kbotyi, xk, yk
    double precision, allocatable,dimension(:,:) :: amat, v
    double precision, allocatable,dimension(:)   :: d
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    if (inistep.eq.1) then
      !
      ! 1.  reads bottom spectrum
      !
      open(183,file= 'bottomspectrum.inp', status='old')
      read(183,*) nkbx, nkby
      read(183,*) dkbx, dkby
      write(*,*) 'bottom spec. dim.:', nkbx, nkby, dkbx, dkby
      allocate(botspec(nkbx, nkby))
      do i=1, nkbx
        read(183,*) botspec(i,:)
      end do
      close(183)
      variance=0
      do i=1,nkbx
        do j=1,nkby
          variance=variance+botspec(i,j)*dkbx*dkby
        end do
      end do
      write(*,*) 'bottom variance:', variance
      !
    else
      !
      ! 2.  precomputed the scatering matrices for zero current
      !
      ! the scattering source term is expressed as a matrix problem for
      ! a list of wavenumbers k0
      ! in the range of wavenumbers used in the model.
      ! i.e. s(k0,theta)=kfactor*scatmata ** transpose (e(k0,theta))
      !
      ! in which
      !
      ! kfactor is a scalar computed in calcsource as
      !        kfactor=tailfactor*(kp(i,j)**4)*2.*pi*freq(j)*pi*4./(sinh(hnd)*(hnd+sinh(hnd)))
      !
      ! scatmata is a square matrix of size nth*nth
      !
      ! s(k0,theta) and e(k0,theta) are the vectors giving the directional source term
      !   and spectrum at a fixed wavenumber
      !
      allocate(scatmata(0:nkscat-1,1:nth,1:nth))
      allocate(amat(nth,nth))
      do i=0,nkscat-1
        ! kcurr is the current surface wavenumber for which
        ! the scattering matrices are evaluated
        kcurr=kwmin+i*(kwmax-kwmin)/(nkscat-2)
        kcutoff=scattcutoff*kcurr
        do k1=1,nth
          do k2=1,nth
            kbotx=-kcurr*(ecos(k2)-ecos(k1))
            kboty=-kcurr*(esin(k2)-esin(k1))
            amat(k1,k2)=0.
            ! tests if the bottom wavenumber is larger than the cutoff
            ! otherwise the interaction is set to zero
            if ((kbotx**2+kboty**2) > (kcutoff**2)) then
              !warning : there may be a bug : spectrum not symmetric when
              ! nkbx is odd !!
              kbotxi=real(nkbx)/2.+1.+kbotx/dkbx
              kbotyi=real(nkby)/2.+1.+kboty/dkby
              !write(6,*) 'bottom wavenumber i:',kbotxi,kbotyi
              ik=int(kbotxi)
              xk=mod(kbotxi,1.0)
              jk=int(kbotyi)
              yk=mod(kbotyi,1.0)
              if (ik.ge.nkbx) ik=nkbx-1
              if (jk.ge.nkby) jk=nkby-1
              if (ik.lt.1) ik=1
              if (jk.lt.1) jk=1
              ! bilinear interpolation of the bottom spectrum
              amat(k1,k2)=((botspec(ik,jk  )  *(1-yk)           &
                   +botspec(ik,jk+1)  *yk    )*(1-xk)   &
                   +(botspec(ik+1,jk)  *(1-yk)           &
                   +botspec(ik+1,jk+1)*yk)    *xk)      &
                   *(ecos(k1)*ecos(k2)+esin(k1)*esin(k2))**2
            end if
          end do
          amat(k1,k1)=amat(k1,k1)-sum(amat(k1,:))
        end do
        amat(:,:)=dth*(amat(:,:)+transpose(amat(:,:)))*0.5
        !makes sure the matrix is exactly symmetric
        !which should already be the case if the bottom
        ! spectrum is really symmetric
        scatmata(i,:,:)=amat(:,:)
      end do
      allocate(scatmatd(0:nkscat-1,nth))
      allocate(scatmatv(0:nkscat-1,nth,nth))
      allocate(v(nth,nth))
      allocate(d(nth))
      do i=0,nkscat-1
        amat(:,:)=scatmata(i,:,:)
        !
        !diagonalizes the matrix a
        !d is a vector with the eigenvalues, v is the matrix made of the
        !eigenvectors so that vd2vt=a  with d2(i,j)=delta(i,j)d(i)
        !and vvt=id, so that exp(a)=vexp(d2)vt
        !
        call diagonalize(amat,d,v,nrot)
        scatmatd(i,:)=d(:)    !eigen values
        scatmatv(i,:,:)=v(:,:) !eigen vectors
        kcurr=kwmin+i*(kwmax-kwmin)/(nkscat-2)
        write(*,*) 'scattering matrix diagonalized for k=  ',kcurr,',',i+1,'out of ',nkscat
      end do
    end if
    !/
    !/ end of insbs1 ----------------------------------------------------- /
    !/
  end subroutine insbs1
  !/
  !/ end of module insbs1md -------------------------------------------- /
  !/
end module w3sbs1md
