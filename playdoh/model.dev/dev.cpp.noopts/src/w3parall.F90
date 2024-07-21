!> @file
!> @brief parallel routines for implicit solver.
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date   01-jun-2018
!>
!/ ------------------------------------------------------------------- /
!>
!> @brief parallel routines for implicit solver.
!>
!> @author aron roland
!> @author mathieu dutour-sikiric
!> @date   01-jun-2018
!>
!> @copyright copyright 2009-2022 national weather service (nws),
!>       national oceanic and atmospheric administration.  all rights
!>       reserved.  wavewatch iii is a trademark of the nws.
!>       no unauthorized use without permission.
!>
module w3parall
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |                                   |
  !/                  | aron roland (bgs it&e gmbh)       |
  !/                  | mathieu dutour-sikiric (irb)      |
  !/                  |                                   |
  !/                  |                        fortran 90 |
  !/                  | last update :        01-june-2018 |
  !/                  +-----------------------------------+
  !/
  !/   01-june-2018 : origination.                        ( version 6.04 )
  !/
  !  1. purpose : parallel routines for implicit solver
  !  2. method :
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
  !     ----------------------------------------------------------------
  !
  !  6. error messages :
  !  7. remarks
  !  8. structure :
  !  9. switches :
  !
  !     !/s  enable subroutine tracing.
  !
  ! 10. source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ ------------------------------------------------------------------- /
  !/ parameter list
  !/
  !/ ------------------------------------------------------------------- /
  !/ local parameters
  !/
  !
  integer, allocatable :: listispnextdir(:), listispprevdir(:)
  integer, allocatable :: listispnextfreq(:), listispprevfreq(:)
  logical, parameter   :: lsloc = .true.
  integer, parameter   :: imem = 1
  real,  parameter     :: onesixth  = 1.0d0/6.0d0
  real,  parameter     :: onethird  = 1.0d0/3.0d0
  real,  parameter     :: zero      = 0.0d0
  real*8,  parameter     :: thr8      = tiny(1.d0)
  real,  parameter     :: thr       = tiny(1.0)
contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief na
  !>
  !> @param[out] etime
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine wav_my_wtime(etime)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose :
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    implicit none
    integer mpimode
    real(8), intent(out) :: etime
    real(8) mpi_wtime
    mpimode=0
    mpimode=1
    etime=mpi_wtime()
    if (mpimode .eq. 0) then
      call cpu_time(etime)
    end if
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine wav_my_wtime
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief print timings.
  !>
  !> @param[in] string
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine print_my_time(string)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : print timings
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3odatmd, only : iaproc
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
    !
    character(*), intent(in) :: string
    real(8) :: etime
    call wav_my_wtime(etime)
    write(740+iaproc,*) 'timing time=', etime, ' at step ', string
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine print_my_time
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief compute refraction part in matrix.
  !>
  !> @param[in]  isea
  !> @param[in]  dtg
  !> @param[out] cad
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine prop_refraction_pr1(isea,dtg, cad)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : compute refraction part in matrix
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3gdatmd, only: nk, nk2, nth, nspec, sig, dsip, ecos, esin, &
         ec2, esc, es2, fachfa, mapwn, flcth, flck,  &
         ctmax, dmin, dth, cthg0s, mapsf
    use w3adatmd, only: cg, wn, dcxdx, dcxdy, dcydx, dcydy, dddx,   &
         dddy, dw
    use w3idatmd, only: flcur
    use w3odatmd, only : iaproc
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
    !/
    real, intent(out) :: cad(nspec)
    integer, intent(in) :: isea
    real, intent(in) :: dtg
    integer :: isp, ik, ith, ix, iy
    real :: frk(nk), frg(nk), dsdd(0:nk+1)
    real :: facth, dcxy, dcyx, dcxxyy, dttst
    real :: edcxdx, edcxdy, edcydx, edcydy, edddx, edddy, ecthg0
    real :: vcflt(nspec), depth, fdg
    real :: fddmax
    ix=mapsf(isea,1)
    iy=mapsf(isea,2)
    edddx=dddx(iy,ix)
    edddy=dddy(iy,ix)
    ecthg0 = cthg0s(isea)
    facth  = dtg / dth
    !
    fdg    = facth * ecthg0
    depth  = max ( dmin , dw(isea) )
    do ik=0, nk+1
      if ( depth*wn(ik,isea) .lt. 5. ) then
        dsdd(ik) = max ( 0. , cg(ik,isea)*wn(ik,isea)-0.5*sig(ik) ) / depth
      else
        dsdd(ik) = 0.
      end if
    end do
    fddmax=0
    do ith=1, nth
      fddmax = max ( fddmax , abs(esin(ith)*edddx - ecos(ith)*edddy ) )
    end do
    do ik=1, nk
      frk(ik) = facth * dsdd(ik) / wn(ik,isea)
      !frk(ik) = frk(ik) / max ( 1. , frk(ik)*fddmax/ctmax )
      frg(ik) = fdg * cg(ik,isea)
    end do
    do isp=1, nspec
      vcflt(isp) = frg(mapwn(isp)) * ecos(isp) +                     &
           frk(mapwn(isp)) * ( esin(isp)*edddx - ecos(isp)*edddy )
    end do
    !
    !
    if ( flcur ) then
      edcxdx=dcxdx(iy,ix)
      edcxdy=dcxdy(iy,ix)
      edcydx=dcydx(iy,ix)
      edcydy=dcydy(iy,ix)
      dcyx   = facth *   edcydx
      dcxxyy = facth * ( edcxdx - edcydy )
      dcxy   = facth *   edcxdy
      do isp=1, nspec
        vcflt(isp) = vcflt(isp) + es2(isp)*dcyx  + esc(isp)*dcxxyy - ec2(isp)*dcxy
      end do
    end if
    do isp=1,nspec
      cad(isp)=dble(vcflt(isp))
    end do
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine prop_refraction_pr1
  !/ ------------------------------------------------------------------- /
  !
  !>
  !> @brief compute refraction part in matrix alternative approach.
  !>
  !> @param[in]  ip
  !> @param[in]  isea
  !> @param[in]  dtg
  !> @param[out] cad
  !> @param[in]  dolimiter
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine prop_refraction_pr3(ip, isea, dtg, cad, dolimiter)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : compute refraction part in matrix alternative approach
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only : lpdlib
    use w3gdatmd, only: nk, nk2, nth, nspec, sig, dsip, ecos, esin, &
         ec2, esc, es2, fachfa, mapwn, flcth, flck,  &
         ctmax, dmin, dth, cthg0s, mapsf, sig
    use w3adatmd, only: cg, wn, dcxdx, dcxdy, dcydx, dcydy, dddx,   &
         dddy, dw
    use w3idatmd, only: flcur
    use w3odatmd, only : iaproc
    implicit none
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real, intent(out) :: cad(nspec)
    integer, intent(in) :: isea, ip
    real, intent(in) :: dtg
    logical, intent(in) :: dolimiter
    integer :: isp, ik, ith, ix, iy
    real :: frk(nk), frg(nk), dsdd(0:nk+1)
    real :: facth, dcxy, dcyx, dcxxyy, dttst
    real :: edcxdx, edcxdy, edcydx, edcydy, edddx, edddy, ecthg0
    real :: vcflt(nspec), depth, fdg, cg1(0:nk+1), wn1(0:nk+1)
    real :: fddmax, cflthmax, velnofilt, ctmax_eff
    ix = mapsf(isea,1)
    iy = mapsf(isea,2)
    edddx=dddx(1,ip)
    edddy=dddy(1,ip)
    ecthg0 = cthg0s(isea)
    facth  = 1.0 / dth
    !
    fdg    = facth * ecthg0
    depth  = max ( dmin , dw(isea) )
    do ik=0, nk+1
      if ( depth*wn(ik,isea) .lt. 5. ) then
        dsdd(ik) = max ( 0. , cg(ik,isea)*wn(ik,isea)-0.5*sig(ik) ) / depth
      else
        dsdd(ik) = 0.
      end if
    end do
    do ik=1, nk
      frk(ik) = facth * dsdd(ik) / wn(ik,isea)
      frg(ik) = fdg * cg(ik,isea)
    end do
    if (flcur) then
      edcxdx = dcxdx(1,ip)
      edcxdy = dcxdy(1,ip)
      edcydx = dcydx(1,ip)
      edcydy = dcydy(1,ip)
      dcyx   = facth *   edcydx
      dcxxyy = facth * ( edcxdx - edcydy )
      dcxy   = facth *   edcxdy
      do isp=1, nspec
        vcflt(isp) = es2(isp)*dcyx  + esc(isp)*dcxxyy - ec2(isp)*dcxy
      end do
    else
      vcflt=0
    end if
    !
    !
    ctmax_eff=ctmax/dtg
    do isp=1, nspec
      velnofilt = vcflt(isp)                                       &
           + frg(mapwn(isp)) * ecos(isp)                             &
           + frk(mapwn(isp)) * (esin(isp)*edddx - ecos(isp)*edddy)
      !
      ! puts filtering on total velocity (including currents and great circle effects)
      ! the filtering limits vcflt to be less than ctmax
      ! this modification was proposed by f. ardhuin 2011/03/06
      !
      if (dolimiter) then
        vcflt(isp)=sign(min(abs(velnofilt),ctmax_eff),velnofilt)
      else
        vcflt(isp)=velnofilt
      end if
    end do
    do isp=1,nspec
      cad(isp)=dble(vcflt(isp))
    end do
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine prop_refraction_pr3
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief compute frequency shift in matrix.
  !>
  !> @param[in]  ip
  !> @param[in]  isea
  !> @param[out] cas
  !> @param[out] dmm
  !> @param[in]  dtg
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine prop_freq_shift(ip, isea, cas, dmm, dtg)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : compute freq. shift in matrix
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    use constants, only : lpdlib
    use w3gdatmd, only: nk, nk2, nth, nspec, sig, dsip, ecos, esin, &
         ec2, esc, es2, fachfa, mapwn, flcth, flck,  &
         ctmax, dmin, dth, mapsf
    use w3adatmd, only: cg, wn, dcxdx, dcxdy, dcydx, dcydy, cx, cy, dddx, dddy, dw
    use w3odatmd, only : iaproc
    implicit none
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, intent(in) :: isea, ip
    real, intent(out) :: dmm(0:nk2)
    real, intent(in) :: dtg
    real, intent(out) :: cas(nspec)
    real :: db(nk2), dsdd(0:nk+1)
    real :: edcxdx, edcxdy, edcydx, edcydy, ecx, ecy, edddx, edddy
    real :: dcxx, dcxyyx, dcyy, fkd, fack
    real :: velnofilt, velfac, depth
    real :: cflk(nk2,nth), fkc(nth), fkd0
    integer :: ik, ith, isp, iy, ix
    !
    if (lpdlib) then
      edcxdx = dcxdx(1,ip)
      edcxdy = dcxdy(1,ip)
      edcydx = dcydx(1,ip)
      edcydy = dcydy(1,ip)
      edddx  = dddx(1,ip)
      edddy  = dddy(1,ip)
    else
      ix=mapsf(isea,1)
      iy=mapsf(isea,2)
      edcxdx=dcxdx(iy,ix)
      edcxdy=dcxdy(iy,ix)
      edcydx=dcydx(iy,ix)
      edcydy=dcydy(iy,ix)
      edddx=dddx(iy,ix)
      edddy=dddy(iy,ix)
    endif
    ecx=cx(isea)
    ecy=cy(isea)
    dcxx   =  -   edcxdx
    dcxyyx =  - ( edcxdy + edcydx )
    dcyy   =  -   edcydy
    fkd    =    ( ecx*edddx + ecy*edddy )
    fack = dtg
    do ith=1, nth
      fkc(ith) = ec2(ith)*dcxx + esc(ith)*dcxyyx + es2(ith)*dcyy
    end do
    do ik=0, nk
      db(ik+1) = dsip(ik) / cg(ik,isea)
      dmm(ik+1) = dble(wn(ik+1,isea) - wn(ik,isea))
    end do
    db(nk+2) = dsip(nk+1) / cg(nk+1,isea)
    dmm(nk+2) = zero
    dmm(0)=dmm(1)
    !
    depth  = max ( dmin , dw(isea) )
    do ik=0, nk+1
      if ( depth*wn(ik,isea) .lt. 5. ) then
        dsdd(ik) = max ( 0. , cg(ik,isea)*wn(ik,isea)-0.5*sig(ik) ) / depth
      else
        dsdd(ik) = 0.
      end if
    end do
    do ik=0, nk+1
      fkd0   = fkd / cg(ik,isea) * dsdd(ik)
      velfac =  fack/db(ik+1)
      do ith=1, nth
        velnofilt = ( fkd0 + wn(ik,isea)*fkc(ith) ) * velfac
        cflk(ik+1,ith) = velnofilt/velfac
      end do
    end do
    do ik=1,nk
      do ith=1,nth
        isp=ith + (ik-1)*nth
        cas(isp)=dble(cflk(ik,ith))
      end do
    end do
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine prop_freq_shift
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief compute frequency shift alternative approach.
  !>
  !> @param[in]  ip
  !> @param[in]  isea
  !> @param[out] cwnb_m2
  !> @param[out] dwni_m2
  !> @param[in]  dtg
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine prop_freq_shift_m2(ip, isea, cwnb_m2, dwni_m2, dtg)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : compute freq. shift alternative approach
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use constants, only : lpdlib
    use w3gdatmd, only: nk, nk2, nth, nspec, sig, dsip, ecos, esin, &
         ec2, esc, es2, fachfa, mapwn, flcth, flck,  &
         ctmax, dmin, dth, mapsf
    use w3adatmd, only: cg, wn, dcxdx, dcxdy, dcydx, dcydy, cx, cy, dddx, dddy, dw
    use w3odatmd, only : iaproc
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer, intent(in) :: isea, ip
    real, intent(out) :: cwnb_m2(1-nth:nspec)
    real, intent(out) :: dwni_m2(nk)
    real, intent(in) :: dtg
    !
    real :: edcxdx, edcxdy, edcydx, edcydy, ecx, ecy, edddx, edddy
    real :: dcxx, dcxyyx, dcyy, fkd, fack
    real :: depth
    real :: fkc(nth), fkd0
    real :: vcwn(1-nth:nspec+nth)
    real :: dsdd(0:nk+1)
    real :: sumdiff, sumdiff1, sumdiff2, sumdiff3
    real :: sumdiff0, sumdiff4, sumdiff5
    integer :: ik, ith, isp, iy, ix
    !/ ------------------------------------------------------------------- /
    if (lpdlib) then
      edcxdx = dcxdx(1,ip)
      edcxdy = dcxdy(1,ip)
      edcydx = dcydx(1,ip)
      edcydy = dcydy(1,ip)
      edddx  = dddx(1,ip)
      edddy  = dddy(1,ip)
    else
      ix=mapsf(isea,1)
      iy=mapsf(isea,2)
      edcxdx=dcxdx(iy,ix)
      edcxdy=dcxdy(iy,ix)
      edcydx=dcydx(iy,ix)
      edcydy=dcydy(iy,ix)
      edddx=dddx(iy,ix)
      edddy=dddy(iy,ix)
    endif
    ecx = cx(isea)
    ecy = cy(isea)
    fack = dtg
    dcxx   =  - fack *   edcxdx
    dcxyyx =  - fack * ( edcxdy + edcydx )
    dcyy   =  - fack *   edcydy
    fkd    =    fack * ( ecx*edddx + ecy*edddy )
    do ith=1, nth
      fkc(ith) = ec2(ith)*dcxx + esc(ith)*dcxyyx + es2(ith)*dcyy
    end do
    !
    depth  = max ( dmin , dw(isea) )
    do ik=0, nk+1
      if ( depth*wn(ik,isea) .lt. 5. ) then
        dsdd(ik) = max ( 0. , cg(ik,isea)*wn(ik,isea)-0.5*sig(ik) ) / depth
      else
        dsdd(ik) = 0.
      end if
    end do
    isp = -nth
    do ik=0, nk+1
      fkd0   = fkd / cg(ik,isea) * dsdd(ik)
      do ith=1, nth
        isp = isp + 1
        vcwn(isp) = fkd0 + wn(ik,isea)*fkc(ith)
      end do
    end do
    sumdiff=0
    do isp=1-nth,nspec
      cwnb_m2(isp) = dble(0.5 * ( vcwn(isp) + vcwn(isp+nth) ))
      sumdiff = sumdiff + max(cwnb_m2(isp), zero)
    end do
    do ik=1,nk
      dwni_m2(ik) = dble( cg(ik,isea) / dsip(ik) )
    end do
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine prop_freq_shift_m2
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief sync global local arrays.
  !>
  !> @param[in] imod
  !> @param[in] ismulti
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine synchronize_ipgl_etc_array(imod, ismulti)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : sync global local arrays
    !  2. method :
    ! 		all the process need to have ipgl_tot and ipgl_to_proc
    ! 		this is especially the case for the output process.
    ! 		so we need some painful exportation business
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    implicit none
    integer, intent(in) :: imod
    logical, intent(in) :: ismulti
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine synchronize_ipgl_etc_array
  !/ ....................----------------------------------------------- /
  !>
  !> @brief setup nseal, nsealm in context of pdlib.
  !>
  !> @param[out] nsealout
  !> @param[out] nsealmout
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine set_up_nseal_nsealm(nsealout, nsealmout)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : setup nseal, nsealm in contect of pdlib
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    use w3adatmd, only: mpi_comm_wave, mpi_comm_wcmp
    use constants, only : lpdlib
    use w3gdatmd, only: nsea
    use w3odatmd, only: ntproc, naproc, iaproc
    implicit none
    integer, intent(out) :: nsealout, nsealmout
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine set_up_nseal_nsealm
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set jsea for all schemes.
  !>
  !> @param[in]  isea
  !> @param[out] jsea
  !> @param[out] isproc
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine init_get_jsea_isproc(isea, jsea, isproc)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : set jsea for all schemes
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3odatmd, only: outpts, iaproc, naproc
    use w3gdatmd, only: gtype, ungtype, mapsf
    use constants, only : lpdlib
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    integer, intent(in) :: isea
    integer, intent(out) :: jsea, isproc
    integer ip_glob
      jsea   = 1 + (isea-1)/naproc
      isproc = isea - (jsea-1)*naproc
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine init_get_jsea_isproc
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set belongings of jsea in context of pdlib.
  !>
  !> @param[in]  isea
  !> @param[out] jsea
  !> @param[out] ibelong
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine get_jsea_ibelong(isea, jsea, ibelong)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : set belongings of jsea in context of pdlib
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3odatmd, only: outpts, iaproc, naproc
    use w3gdatmd, only: gtype, ungtype, mapsf
    use constants, only : lpdlib
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    integer, intent(in) :: isea
    integer, intent(out) :: jsea, ibelong
    integer isproc, ix, jx
    if (.not. lpdlib) then
      jsea   = 1 + (isea-1)/naproc
      isproc = isea - (jsea-1)*naproc
      if (isproc .eq. iaproc) then
        ibelong=1
      else
        ibelong=0
      end if
    else
    endif
    !/
    !/ end of init_get_isea ---------------------------------------------- /
    !/
  end subroutine get_jsea_ibelong
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief set isea for all schemes.
  !>
  !> @param[out] isea
  !> @param[in]  jsea
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine init_get_isea(isea, jsea)
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : set isea for all schemes
    !  2. method :
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    use w3odatmd, only: outpts, iaproc, naproc
    use w3gdatmd, only: gtype, ungtype
    use constants, only : lpdlib
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !
    use w3odatmd, only: outpts, iaproc, naproc
    use w3gdatmd, only: gtype, ungtype
    use constants, only : lpdlib
    implicit none
    integer, intent(in) :: jsea
    integer, intent(out) :: isea
    !/
    !/ end of init_get_isea ------------------------------------------------ /
    !/
  end subroutine init_get_isea
  !>
  !> @brief sync global array in context of pdlib.
  !>
  !> @details an array of size (nsea) is send but only the (1:nseal) values
  !>          are correct. the program synchonizes everything on all nodes.
  !>
  !> @param[inout] thevar
  !>
  !> @author aron roland
  !> @author mathieu dutour-sikiric
  !> @date   01-jun-2018
  !>
  subroutine synchronize_global_array(thevar)
    !/ ------------------------------------------------------------------- /
    !**********************************************************************
    !*  an array of size (nsea) is send but only the (1:nseal) values     *
    !*  are correct. the program synchonizes everything on all nodes.     *
    !**********************************************************************
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :        01-june-2018 |
    !/                  +-----------------------------------+
    !/
    !/   01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : sync global array in context of pdlib
    !  2. method :
    !			an array of size (nsea) is send but only the (1:nseal) values
    ! 			are correct. the program synchonizes everything on all nodes.
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
    !     ----------------------------------------------------------------
    !
    !  6. error messages :
    !  7. remarks
    !  8. structure :
    !  9. switches :
    !
    !     !/s  enable subroutine tracing.
    !
    ! 10. source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    use w3gdatmd, only: nseal, nsea, nx
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    include "mpif.h"
    integer isea, jsea, status(nx), rstatus(nx)
    integer iproc, i, ierr, ip, ix, ip_glob
    double precision, intent(inout) :: thevar(nx)
    double precision                ::  rvect(nx)
    status=0
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine synchronize_global_array
  !/ ------------------------------------------------------------------- /
end module w3parall
!/ ------------------------------------------------------------------- /
