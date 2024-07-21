!/ ------------------------------------------------------------------- /
module w3ref1md
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii                     |
  !/                  |            f. ardhuin             |
  !/                  |                        fortran 90 |
  !/                  | last update :         27-jun-2014 |
  !/                  +-----------------------------------+
  !/
  !/    31-mar-2010 : origination.                        ( version 3.14.ifremer )
  !/    03-sep-2010 : clean up                            ( version 3.14.ifremer )
  !/    31-may-2011 : adding variable reflections         ( version 4.01 )
  !/    02-nov-2011 : compatibility with unst. grids      ( version 4.04 )
  !/    24-fev-2012 : correction of angle in fluxes       ( version 4.05 )
  !/    27-jul-2013 : adding free infragravity waves      ( version 4.11 )
  !/    11-nov-2013 : extends ig energy into main band    ( version 4.13 )
  !/    11-jun-2014 : put reflection by subgrids back     ( version 5.01 )
  !/    27-jun-2014 : modifies subgrid reflection of ig   ( version 5.01 )
  !/
  !  1. purpose :
  !
  !     this module computes :
  !        - shoreline reflection
  !        - unresolved islands and iceberg reflections
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
  subroutine w3sref(a, cg, wn, emean, fmean, depth, cx1, cy1, reflc, refld,     &
       trnx, trny, berg, dt, ix, iy, jsea, s)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            f. ardhuin             |
    !/                  |                        fortran 90 |
    !/                  | last update :         11-jun-2014 |
    !/                  +-----------------------------------+
    !/
    !/    06-may-2010 : origination.                          ( version 3.14-ifremer )
    !/    31-may-2011 : introduction of amplitude-dependent r ( version 4.05 )
    !/    27-jul-2013 : adding free infragravity waves        ( version 4.11 )
    !/    11-nov-2013 : expands ig energy frequency range     ( version 4.13 )
    !/    05-mar-2014 : fixing bug with icalc = 1 and ig1     ( version 4.18 )
    !/    11-jun-2014 : put reflection by subgrids back       ( version 5.01 )
    !/
    !  1. purpose :
    !
    !     computes coastal and iceberg/island reflections and adds free ig energy
    !
    !  2. method :
    !
    !     adds the reflected components from 2 types of sources:
    !        shoreline reflection, subgrid obstruction and icebergs
    !
    !     in the case where the ig switch is present, there are two passes:
    !        - icalc = 1, only the wind sea and swell are reflected (no ig added)
    !        - icalc = 2, ig energy is added into all frequency bands
    !
    !
    !     when ig energy is put in the entire spectrum ( nint(igpars(4)).eq.2 )
    !        two passes are done: the first for the reflection of windsea and swell
    !                             the second for the addition of ig and ig reflection alone
    !
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
    use w3gdatmd, only: nk, nth, nspec, sig, dth, dden,  smctype, &
         refpars, ecos, esin, ec2, mapth, mapwn, flagll, &
         sig2, dsii, iobpd, gtype, ungtype, mapfs, clgtype, rlgtype
    use w3gdatmd, only : clats, hpfac, hqfac, sx, sy, si
    !/
    !
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    real, intent(in)        :: cg(nk), wn(nk), depth, emean, fmean
    real, intent(inout)     :: a(nspec)
    real, intent(in)        :: cx1, cy1, dt
    integer, intent(in)     :: refld(6), ix, iy, jsea
    real, intent(in)        :: reflc(4), trnx, &
         trny, berg
    real, intent(out)       :: s(nspec)
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer         :: ispeci, ispec, ik, ith, ith2, ith3, ith2x, ith2y, &
         nrs, ik1
    integer         :: isea, icalc, iobpdip(nth)
    logical         :: igbcoverwrite, igswellmax
    real            :: r1, r2, r3, r4, r2x, r2y, depthig
    real            :: dela, delx, dely, facx
    real            :: fac1, fac2, fac3, fac4, ramp0, ramp, &
         ramp1, ramp2, ramp4, michefac, slope
    real             :: hs, hig, hig1, hig2, eb, sb, emeana, fmean2,   &
         fmeana, freqig, efig, efig1, sqrth, smeana
    !
    ! 0.  initializations ------------------------------------------------ *
    !
    emeana  = 0.
    fmeana  = 0.
    fmean2  = 0.
    delx=1.
    dely=1.
    ! set facx for all grid types
    if (flagll) then
      facx   =  1./(dera * radius)
    else
      facx   =  1.
    end if
    isea = mapfs (iy,ix)
    !!li  smctype shares info with rlgtype.  jgli12oct2020
    if (gtype.eq.rlgtype .or. gtype.eq.smctype) then
      delx=sx*clats(isea)/facx
      dely=sy/facx
    end if
    if (gtype.eq.clgtype) then
      ! maybe what follows works also for rlgtype ... to be verified
      delx=hpfac(iy,ix)/ facx
      dely=hqfac(iy,ix)/ facx
    end if
    if (gtype.eq.ungtype) then
      if (lpdlib) then
      else
        delx=5.*sqrt(si(ix))*(dera * radius)    ! first approximation ...
        dely=5.*sqrt(si(ix))*(dera * radius)    ! first approximation ...
      endif
    end if
    ik1=1
    do ik=ik1, nk
      eb  = 0.
      do ith=1, nth
        eb = eb + a(ith+(ik-1)*nth)
      end do
      eb   = eb * dden(ik) / cg(ik)
      emeana    = emeana  + eb
      fmean2    = fmean2  + eb /sig(ik)**2
      fmeana    = fmeana  + eb /sig(ik)
    end do
    fmeana  = tpiinv * (emeana / max ( 1.e-7 , fmeana ))
    fmean2  = tpiinv * sqrt(emeana / max ( 1.e-7 , fmean2 ))
    fmeana  = max(fmeana,sig(1))
    !
    ! 1.  sets reflection term to zero
    !
    icalc=1
    hs=4.*sqrt(emeana)
      s = 0.
      !
      nrs=nint(refpars(8))
      if (refpars(6).gt.0) then
        !
        ! this is the miche parameter for a beach slope of reflc(3)
        !
        if(reflc(3)/=reflc(3)) then ! isnan test
          slope=0.001
        else
          slope=max(0.001,reflc(3))
        end if
        michefac=0.0001*grav**2*(slope**5)  &
             /(max(emeana,1e-4)*max(fmeana,0.001)**4)
        ramp0=max(0.07*(alog10(michefac)+4.5)+1.5*michefac,0.005)   ! if reflc(1)=1, 0.07 should be 0.007
        ! nb: these constants are adjusted for reflc(1) = 0.1. if  reflc(1)=1, 0.07 should be 0.007
      else
        ramp0=1.
      endif
      !
      ! 2.  shoreline reflection =============================================== *
      !
      if (reflc(1).gt.0) then
        fac1=1/(0.5*real(nth))
        fac2=1.57/(0.5*real(nth))
        !           fac3=2.6/(0.5*real(nth))  ! this is for nrs=4
        fac3=2./sum(abs(ecos(1:nth))**nrs)
        fac4=1.
        !
        do ik=1, nk
          !
          ! includes frequency dependence (see elgar et al. jpo 1994)
          !
          if (refpars(6).gt.0) then
            ramp=(max((0.75*tpi*fmeana/sig(ik)),1.)**refpars(10))*ramp0
            ramp1=min(refpars(9),reflc(1)*ramp)
            ramp2=min(refpars(9),reflc(2)*ramp)
          else
            ramp1=ramp0*reflc(1)
            ramp2=ramp0*reflc(2)
          end if
          !
          ! special treatment for unstructured grids when not using source term
          !
          if (gtype.eq.ungtype.and.refpars(3).lt.0.5) then
            if (lpdlib) then
            else
              iobpdip = iobpd(:,ix)
            endif
            do ith=1, nth
              ispeci=ith+(ik-1)*nth
              a(ispeci)=a(ispeci)*iobpdip(ith) !puts to zero the energy not going to coast
            end do
            !
            do ith=1, nth
              r1=ecos(1+mod(abs(ith-refld(1)),nth))
              r1=iobpdip(ith)
              ispeci=ith+(ik-1)*nth
              r2=ramp1*a(ispeci)
              if (r1.gt.0.and.r2.gt.0) then
                !
                ! determines direction of specular reflection: th3=pi+2*n-th1
                !
                ith3=1+mod(nth/2+nth+2*refld(1)-ith-1,nth)
                do ith2=1,nth
                  !
                  !  adds energy into reflected directions (ith2)
                  !
                  ispec=ith2+(ik-1)*nth
                  r3=ecos(1+mod(abs(ith2-refld(1)),nth))
                  if (r3.lt.0) then
                    r4=ecos(1+mod(abs(ith2-ith3),nth))*(1-iobpdip(ith2))
                    if (r4.gt.0.) then
                      !
                      ! tests the type of shoreline geometry
                      !
                      select case (refld(2))
                      case (0)
                        ! sharp corner: broad reflection
                        s(ispec)=s(ispec)+r2*fac1/dt
                        ! fa: analog to following lines to be swapped in if reflection method changed
                        ! rect case:
                        !  s(ispec)=s(ispec)+    &
                        !    real(refld(3))*r2*cg(ik)*abs(ecos(ith2x)/delx)*fac1   &
                        !   +real(refld(4))*r2*cg(ik)*abs(esin(ith2y)/dely)*fac1
                      case (1)
                        ! mild corner: average reflection
                        s(ispec)=s(ispec)+r2*abs(r4)*fac2/dt
                      case (2)
                        ! straight coast: narrow reflection
                        !     if(ith3.eq.ith2) s(ispec)=s(ispec)+r2/dt  ! this is for specular ref.
                        s(ispec)=s(ispec)+r2*(r4**nrs) *fac3/dt
                      end select
                    end if  ! (r4.gt.0.)
                  end if  ! (r3.lt.0)
                end do ! ith2=1,nth
              end if  ! (r1.gt.0.and.r2.gt.0)
            end do  ! ith=1, nth
          else ! (gtype.ne.ungtype)
            !
            ! this is for structured grids ....
            !
            !
            !  loop on  incident wave direction (ith)
            !
            do ith=1, nth
              r1=ecos(1+mod(abs(ith-refld(1)),nth))
              r2=ramp1*a(ith+(ik-1)*nth)
              if (r1.gt.0.and.r2.gt.0) then
                do ith2=1,nth
                  !
                  !  adds energy into reflected directions (ith2)
                  !
                  ispec=ith2+(ik-1)*nth
                  ith2x=1+mod(nth+ith2-refld(5)-1,nth)
                  ith2y=1+mod(nth+ith2-refld(6)-1,nth)
                  r3=ecos(1+mod(abs(ith2-refld(1)),nth))
                  if (r3.lt.0) then
                    !
                    ! determines direction of specular reflection: th3=pi+2*n-th1
                    !
                    ith3=1+mod(nth/2+nth+2*refld(1)-ith-1,nth)
                    r4=ecos(1+mod(abs(ith2-ith3),nth))
                    if (r4.gt.0.) then
                      !
                      ! tests the type of shoreline geometry
                      ! nb: refld(3) or refld(4) is equal to 1 if the reflection is applied (real land neighbor)
                      select case (refld(2))
                      case (0)
                        ! sharp corner: broad reflection
                        s(ispec)=s(ispec)+    &
                             real(refld(3))*r2*cg(ik)*abs(ecos(ith2x)/delx)*fac1   &
                             +real(refld(4))*r2*cg(ik)*abs(esin(ith2y)/dely)*fac1
                      case (1)
                        ! mild corner: average reflection
                        !
                        s(ispec)=s(ispec)+    &
                             real(refld(3))*r2*cg(ik)*abs(ecos(ith2x)/delx)*abs(r4)*fac2 &
                             + real(refld(4))*r2*cg(ik)*abs(esin(ith2y)/dely)*abs(r4)*fac2
                      case (2)
                        ! straight coast: narrow reflection
                        ! specular for tests
                        !                   s(ispec)=s(ispec)+real(refld(3))*r2*cg(ik)*abs(ecos(ith2)/delx)  &
                        !                                    +real(refld(4))*r2*cg(ik)*abs(esin(ith2)/dely)
                        !
                        s(ispec)=s(ispec)+real(refld(3))*r2*cg(ik)*abs(ecos(ith2x)/delx) &
                             *(r4**nrs) *fac3                          &
                             +real(refld(4))*r2*cg(ik)*abs(esin(ith2y)/dely) &
                             *(r4**nrs) *fac3
                      end select
                    end if ! (r4.gt.0.)
                  end if  ! (r3.lt.0)
                end do  ! ith2=1,nth
              end if  ! (r1.gt.0.and.r2.gt.0)
            end do  ! ith=1,nth
          end if  ! ungtype
        end do ! loop on ik
      end if   ! end of test on reflc(1)
      !
      !  add diffuse reflection due to subgrid islands and icebergs
      !  at present this feature is not supported for unstructured grids.
      !
      if (    ((refpars(2).gt.0.).and.((trnx+trny).lt.2))     &
           .or.((refpars(4).gt.0.).and.(berg.gt.0)       )   ) then
        !
        ! includes frequency dependence (see elgar et al. jpo 1994)
        !
        if (refpars(6).gt.0) then
          ramp=(max((0.75*tpi*fmeana/sig(ik)),1.)**refpars(10))*ramp0
          ramp2=min(refpars(9),reflc(2)*ramp)
          !
          ! recomputes coefficients for iceberg slope given by reflc(4)
          !
          slope=max(0.001,reflc(4))
          michefac=0.0001*grav**2*(slope**5)  &
               /(max(emeana,1e-4)*max(fmeana,0.001)**4)
          ramp0=max(0.007*(alog10(michefac)+4.5)+1.5*michefac,0.005)   ! if reflc(1)=1, 0.07 should be 0.007
          ramp=(max((0.75*tpi*fmeana/sig(ik)),1.)**refpars(10))*ramp0
          ramp4=min(refpars(9),ramp)
        else
          ramp2=ramp0*reflc(2)
          ramp4=ramp0*reflc(4)
        end if
        !
        !
        r2x=  ramp2*refpars(2)*max(0.,min(1.,(1-trnx)))  &
             + ramp4*refpars(4)*max(0.,min(1.,(1-exp(-berg*delx*0.0001))))
        r2y=  ramp2*refpars(2)*max(0.,min(1.,(1-trny)))  &
             + ramp4*refpars(4)*max(0.,min(1.,(1-exp(-berg*dely*0.0001))))
        fac1=1/(0.5*real(nth))
        do ik=1, nk
          do ith=1, nth
            r2=a(ith+(ik-1)*nth)
            if (r2.gt.0.) then
              do ith2=1,nth
                ispec=ith2+(ik-1)*nth
                r3=ecos(1+mod(nth+ith2-ith,nth))
                if (r3.lt.0) then
                  s(ispec)=s(ispec)+ &
                       cg(ik)*r2x*r2*abs(ecos(ith2)/delx)*fac1 &
                       + cg(ik)*r2y*r2*abs(esin(ith2)/dely)*fac1
                end if
              end do
            end if
          end do
        end do
      end if
    !/
    !/ end of w3sref ----------------------------------------------------- /
    !/
  end subroutine w3sref!/ ------------------------------------------------------------------- /
  !/
  !/ end of module w3ref1md -------------------------------------------- /
  !/
end module w3ref1md
