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
module w3gig1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii                     |
  !/                  |     a. rawat and  f. ardhuin      |
  !/                  |                        fortran 90 |
  !/                  | last update :         05-jul-2012 |
  !/                  +-----------------------------------+
  !/
  !/    31-mar-2010 : origination.                        ( version 4.07 )
  !/
  !  1. purpose :
  !
  !     this module computes :
  !        - the second order spectrum, in particular for infragravity waves
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
  public
  !/
  !/ public variables
  !/
  !
  !/
contains
  !/ ------------------------------------------------------------------- /
  function df1f2theta(s1,s2,wn1,wn2,theta,depth)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           h. l. tolman            |
    !/                  |                        fortran 90 |
    !/                  | last update :         29-nov-1999 |
    !/                  +-----------------------------------+
    !/                                based on incymd of the gla gcm.
    !/
    !/    18-oct-1998 : final fortran 77                    ( version 1.18 )
    !/    29-nov-1999 : upgrade to fortran 90               ( version 2.00 )
    !/
    !  1. purpose :
    !
    !      computes the coupling coefficient between waves of frequencies f1 and f2
    !      and an angle theta.
    !       this is for the surface elevation variance
    !      see okihiro et al. 1992
    !       code adapted from matlab by arshad rawat, 2012.
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       nymd    int.   i   old date in yymmdd format.
    !       m       int.   i   +/- 1 (day adjustment)
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
    !     any subroutine.
    !
    !  8. structure :
    !
    !     see source code.
    !
    !  9. switches :
    !
    !     !/s  enable subroutine tracing using strace.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use constants
    implicit none
    real, intent(in)        :: s1,s2,theta,depth
    real                    :: df1f2theta,wn1,wn2
    real                    :: k1,k2,co,cok1,cok2,k3,c1,c2,c3,c4
    real                    :: c1b,s3,sk2,g2,g
    k1=wn1
    k2=wn2
    co=cos(theta)
    g2=grav**2
    s3=s1+s2
    k3=sqrt(k1**2+k2**2+2*k1*k2*co)
    g=grav
    sk2=g*k3*tanh(k3*depth)
    c1=-(k1*k2*co)/(s1*s2)
    c1b=(s3**2-s1*s2)/g2
    c2=s3
    c3=(s3**2-sk2)*s1*s2
    ! c4 is hasselmann's d times i
    c4=s3*(k1*k2*co-((s1*s2)**2)/g2)+0.5*(s1*k2**2+s2*k1**2-s1*s2*(s2**3+s1**3)/g2)
    df1f2theta=g*(0.5*(c1+c1b)+(c2*c4/c3));
    return
  end function df1f2theta
  !/ ------------------------------------------------------------------- /
  subroutine w3addig(e,depth,wn,cg,iaction)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii                     |
    !/                  |     a. rawat and  f. ardhuin      |
    !/                  |                        fortran 90 |
    !/                  | last update :         05-jul-2012 |
    !/                  +-----------------------------------+
    !/
    !/    31-mar-2010 : origination.                        ( version 4.07 )
    !/
    !  1. purpose :
    !
    !     this subroutine computes :
    !        - the second order spectrum, in particular for infragravity waves
    !  2. method :
    !     uses 2nd order coupling coefficient (biesel 1952, hasselmann 1962)
    !
    !  3. parameters :
    !
    !     parameter list
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
    use constants
    use w3dispmd
    use w3gdatmd, only: nk, nth, nspec, sig, th, dth, dden,  &
         ecos, esin, ec2, mapth, mapwn, &
         dsip, iobpd, gtype, ungtype, igpars
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
    !*****************************************************************************
    ! computes the "second order spectrum" (only difference interaction, not sum)
    !*****************************************************************************
    ! reads in the wave frequency-directional spectrum
    !
    integer     :: nkig,iloc,nspecig
    integer     :: i,iig,ifr,ik,ith,ith1,ith2,itime,i2, isp1, isp2, isp3
    integer , dimension(:,:),      allocatable :: ifr2c
    real :: d,deltaf,dfig,cg2
    real :: wn1,k1,k2,dkx,dky,eadd,thetaig,memo
    real   , dimension(:),        allocatable :: df,fig,ii,efmall
    real   , dimension(:,:),      allocatable :: wfr1,efth
    real   , dimension(:),        allocatable :: efthig
    real   , dimension(:,:,:,:),  allocatable :: dd
    real   , dimension(nspec)                 :: espec
    character(120) ::path,filename,filename2
    ! defines the spectral domain for the ig computation
    nkig=igpars(5)
    nspecig=nkig*nth
    allocate(dd(nkig,nk,nth,nth))
    allocate(wfr1(nkig,nk))
    allocate(ifr2c(nkig,nk))
    allocate(efthig(nspecig))
    efthig(:)=0.
    !  write(*,*) 'computing coupling coefficient for surface elevation'
    if (iaction.eq.0) then
      espec=e
    else
      do ik = 1,nk
        do ith = 1, nth
          isp1=ith+(ik-1)*nth
          espec(isp1)=e(isp1)*sig(ik)*tpi / cg(ik)
        end do
      end do
    end if
    !
    do iig=1,nkig
      do ifr=1,nk
        call wavnu1 (sig(ifr)+sig(iig),depth,wn1,cg2)
        do ith1=1,nth
          do ith2=1,nth
            !
            ! this is the coupling coefficient for the surface elevation. see .e.g. forristall (jpo 2000)
            !
            dd(iig,ifr,ith1,ith2)=(df1f2theta(sig(ifr)+sig(iig),-sig(ifr), wn1,wn(ifr), &
                 (abs(th(ith1)-th(ith2))+pi),depth))**2
          end do
        end do
        !
        ! weights
        !
        wfr1(iig,ifr)=dble(dsip(ifr))*dth
        !
        ! computes indices for a proper integration over the spectral domain using rectangle's rule
        ! since we integrate e(f)*e(f+fig)*df  for a fixed fig
        iloc=1
        if (sig(iig) < 0.5*dsip(ifr))then
          ifr2c(iig,ifr)=ifr
        else
          iloc=minloc(abs((sig(1:nk)-dsip(1:nk))-(sig(iig)+sig(ifr))), 1)
          !find(f-df< (fig(iig)+f(ifr)))
          if (iloc /= 0) then
            ifr2c(iig,ifr)=iloc  ! index of frequency f+fig
          else
            ifr2c(iig,ifr)=nk
          end if
          !wfr1(iig,ifr)=0.0
        end if
      end do
    end do
    do iig=1,nkig
      do ifr = 1,nk-1
        ! ar calculating k1 and k2 before loops on th1 and th2
        k1=wn(ifr)
        k2=wn(ifr2c(iig,ifr))
        do ith1 = 1,nth
          do ith2 = 1,nth
            ! adds the effect of interaction of frequency f(ifr), theta(ith1) with f(ifr)+fig(:), theta(ith2)
            isp1 = ith1 + (ifr2c(iig,ifr)-1)*nth
            isp2 = ith2 + (ifr-1)*nth
            eadd=dd(iig,ifr,ith1,ith2)*wfr1(iig,ifr) &
                 *espec(isp1)*espec(isp2) ! rectangle rule by ar
            dkx=k2*cos(dble(dth*ith2))- k1*cos(dble(dth*ith1))
            dky=k2*sin(dble(dth*ith2))- k1*sin(dble(dth*ith1))
            thetaig=atan2(dky,dkx)
            if (thetaig.lt.0) thetaig=2*pi+thetaig
            ! finding corresponding index of theta ig in theta array
            !i=int((thetaig/(2*pi))*nth)
            i=minloc(abs(thetaig-th), 1)-1
            if (i==0) i=nth
            isp3 = i + (iig-1)*nth
            !            memo=efthig(isp3)
            efthig(isp3)= efthig(isp3)+eadd;
            !              if (efthig(isp3).ne.efthig(isp3).and.eadd.ne.0) write(6,*) 'efthig:',iig, ifr, ith1,ith2,isp3, &
            !                                                                        efthig(isp3),eadd,memo
          end do
        end do
      end do
    end do
    !   espec(1:nspecig)=espec(1:nspecig)+efthig(:)
    espec(1:nspecig)=efthig(:)
    if (iaction.eq.0) then
      do isp1=1,nspecig
        e(isp1)=espec(isp1)
      end do
    else
      do ik = 1,nkig
        do ith = 1, nth
          isp1=ith+(ik-1)*nth
          e(isp1)=espec(isp1)*cg(ik)/(sig(ik)*tpi)
        end do
      end do
    end if
    ! open(5555,file='testos.dat',status='unknown')
    ! write(5555,*) e,efthig !f,fig,tet!ifr2c !efth, !!, efth,
    !/
    !/ end of w3addig ----------------------------------------------------- /
    !/
  end subroutine w3addig
  !/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3gig1md -------------------------------------------- /
  !/
end module w3gig1md
