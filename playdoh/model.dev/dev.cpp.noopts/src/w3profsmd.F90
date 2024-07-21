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
module w3profsmd
  !/
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |           aron roland             |
  !/                  |         fabrice ardhuin           |
  !/                  |                        fortran 90 |
  !/                  | last update :         15-apr-2020 |
  !/                  +-----------------------------------+
  !/
  !/    xx-nov-2007 : origination.                        ( version 3.10 )
  !/    03-nov-2011 : adding shoreline reflection         ( version 4.04 )
  !/    03-jun-2013 : removed assign statements           ( version 4.10 )
  !/    20-jun-2013 : update test output for time steps   ( version 4.10 )
  !/    17-oct-2013 : removes boundary nodes from cfl     ( version 4.12 )
  !/    15-dec-2013 : bug fix for implicit scheme         ( version 4.16 )
  !/    18-aug-2016 : corrected boundary treatment        ( version 4.16 )
  !/    15-apr-2020 : adds optional opt-out for cfl on bc ( version 7.08 )
  !
  !  1. purpose :
  !
  !     propagation schemes for unstructured grids using fluctuation splitting
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
  !      w3xypug       subr. public   generic fluctuation splitting operations
  !      w3xypfsn2     subr. public   advection with n scheme (csik et al. 2002)
  !      w3xypfspsi    subr. public   advection with fct scheme
  !      w3xypfsfct2   subr. public   advection with fct scheme
  !     ----------------------------------------------------------------
  !
  !  4. subroutines and functions used :
  !
  !      name      type  module   description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  5. remarks :
  !     for a detailed description of the schemes and their properties, see
  !     roland (2008), ph.d. thesis, t. u. darmstadt.
  !
  !  6. switches :
  !
  !  7. source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  public
  !/
contains
  !/ ------------------------------------------------------------------- /
  subroutine w3xypug ( isp, facx, facy, dtg, vq, vgx, vgy, lcalc )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |            aron roland            |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-jan-2011 |
    !/                  +-----------------------------------+
    !/
    !/    10-jan-2008 : origination.                        ( version 3.13 )
    !/    10-jan-2011 : addition of implicit scheme         ( version 3.14.4 )
    !/
    !  1. purpose :
    !
    !     propagation in physical space for a given spectral component.
    !     gives the choice of scheme on unstructured grid
    !
    !  2. method :
    !
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isp     int.   i   number of spectral bin (ik-1)*nth+ith
    !       facx/y  real   i   factor in propagation velocity.
    !                          ( 1 or 0 * dt / dx )
    !       dtg     real   i   total time step.
    !       vq      r.a.  i/o  field to propagate.
    !       vgx/y   real   i   speed of grid.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       vcfl0x  r.a.  local courant numbers for absolute group vel.
    !                     using local x-grid step.
    !       vcfl0y  r.a.  id. in y.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       w3wave   wave model routine.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !              make the interface between the wavewatch and the wwm code.
    !
    !  8. structure :
    !
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !
    !
    ! 10. source code :
    !/ ------------------------------------------------------------------- /
    !/
    !
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nx, ny, nsea, mapsf, mapfs, dtcfl, clats,   &
         flcx, flcy, nk, nth, dth, xfr,              &
         ecos, esin, sig,  pfmove,ien,               &
         ntri, trigp, ccon ,                         &
         ie_cell, pos_cell, iobp, iobpd, iobdp,      &
         fsn, fspsi, fsfct, fsnimp, gtype, ungtype
    use w3wdatmd, only: time
    use w3odatmd, only: tbpi0, tbpin, flbpi
    use w3adatmd, only: cg, cx, cy, atrnx, atrny, itime, cflxymax, dw
    use w3idatmd, only: flcur
    !      use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin,       &
    !                          isbpi, bbpi0, bbpin
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isp
    real, intent(in)        :: facx, facy, dtg, vgx, vgy
    real, intent(inout)     :: vq(1-ny:ny*(nx+2))
    logical, intent(in)     :: lcalc
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, isea, ixy
    integer                 :: ix
    real                    :: ccos, csin, ccurx, ccury
    real                    :: c(nx,2)
    real                    :: rd1, rd2
    !/
    !/ automatic work arrays
    !/
    real                    :: vlcflx((nx+1)*ny), vlcfly(nx*ny)
    double precision        :: ac(nx)
    !/ ------------------------------------------------------------------- /
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a set constants
    !
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    ccos   = facx * ecos(ith)
    csin   = facy * esin(ith)
    ccurx  = facx
    ccury  = facy
    !
    ! 1.b initialize arrays
    !
    vlcflx = 0.
    vlcfly = 0.
    !
    ! 1.c set depth
    !
    call setdepth
    !
    !
    ! 2.  calculate velocities ---------------- *
    !
    do isea = 1, nsea
      ixy         = mapsf(isea,3)
      vq(ixy)     = vq(ixy) / cg(ik,isea) * clats(isea)
      vlcflx(ixy) = ccos * cg(ik,isea) / clats(isea)
      vlcfly(ixy) = csin * cg(ik,isea)
    end do
    if ( flcur ) then
      do isea=1, nsea
        ixy =  mapsf(isea,3)
        !
        ! currents are not included on coastal boundaries (iobp(ixy).eq.0)
        !
        if (iobp(ixy) .eq. 1) then
          vlcflx(ixy) = vlcflx(ixy) + ccurx*cx(isea)/clats(isea)
          vlcfly(ixy) = vlcfly(ixy) + ccury*cy(isea)
        end if
      end do
    end if
    !
    ! 3. initialize fluctuation splitting arrays ( to fit with wwm notations)
    !
    ac(1:nx)   = dble(vq(1:nx)) * iobdp(1:nx)
    c(1:nx,1)  = vlcflx(1:nx)   * iobdp(1:nx)
    c(1:nx,2)  = vlcfly(1:nx)   * iobdp(1:nx)
    !
    ! 4. prepares boundary update
    !
    if ( flbpi ) then
      rd1    = dsec21 ( tbpi0, time )
      rd2    = dsec21 ( tbpi0, tbpin )
    else
      rd1=1.
      rd2=0.
    end if
    !
    ! 4. propagate using the selected scheme
    !
    if (fsn) then
      call w3xypfsn2 (isp, c, lcalc, rd1, rd2, dtg, ac)
    else if (fspsi) then
      call w3xypfspsi2 (isp, c, lcalc, rd1, rd2, dtg, ac)
    else if (fsfct) then
      call w3xypfsfct2 (isp, c, lcalc, rd1, rd2, dtg, ac)
    else if (fsnimp) then
      call w3xypfsnimp(isp, c, lcalc, rd1, rd2, dtg, ac)
    endif
    !
    do ix=1,nx
      isea=mapfs(1,ix)
      vq(ix)=real(ac(ix))
    enddo
    ! 6.  store results in vq in proper format --------------------------- *
    !
    do isea=1, nsea
      ixy   = mapsf(isea,3)
      vq(ixy) =  max ( 0. , cg(ik,isea)/clats(isea)*vq(ixy) )
    end do
  end subroutine w3xypug
  !/ ------------------------------------------------------------------- /
  subroutine w3cflug ( isea, nkcfl, facx, facy, dt, mapfs, cflxymax,    &
       vgx, vgy )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |           fabrice ardhuin         |
    !/                  |                        fortran 90 |
    !/                  | last update :         20-jun-2013 |
    !/                  +-----------------------------------+
    !/
    !/    01-mar-2011 : origination.                        ( version 3.14 )
    !/    20-jun-2013 : computes only up to nkcfl for tests ( version 4.10 )
    !/     1-jun-2017 : rewrite routine for performance     ( version 5.xx )
    !/
    !  1. purpose :
    !
    !     computes the max cfl number for output purposes
    !
    !  2. method :
    !
    !
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !       isea     int.   i   index of sea point
    !       nkcfl    int.   i   maximum frequency index
    !       facx/y   real   i   factor in propagation velocity.
    !                          ( 1 or 0 * dt / dx )
    !       dt       real   i   time step.
    !       mapfs    i.a.   i   storage map.
    !       cflxymax real       maxmimum cfl for spatial advection
    !       vgx/y    real   i   speed of grid.
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !       vcfl0x  r.a.  local courant numbers for absolute group vel.
    !                     using local x-grid step.
    !       vcfl0y  r.a.  id. in y.
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !       w3wave   wave model routine.
    !
    !  6. error messages :
    !
    !       none.
    !
    !  7. remarks :
    !              make the interface between the wavewatch and the wwm code.
    !
    !  8. structure :
    !
    !
    !  9. switches :
    !
    !       !/s     enable subroutine tracing.
    !
    !
    ! 10. source code :
    !/ ------------------------------------------------------------------- /
    !/
    !
    use constants
    !
    use w3timemd, only: dsec21
    !
    use w3gdatmd, only: nx, ny, nsea, mapsf, dtcfl, clats,          &
         flcx, flcy, nk, nth, dth, xfr,              &
         ecos, esin, sig,  pfmove,ien, index_cell,   &
         ntri, trigp, ccon ,                         &
         ie_cell, pos_cell, countri, si, iobp
    use w3adatmd, only: cg, cx, cy, atrnx, atrny, itime, dw
    use w3idatmd, only: flcur
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isea, nkcfl, mapfs(ny*nx)
    real, intent(in)        :: facx, facy, dt, vgx, vgy
    real, intent(inout)     :: cflxymax
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik
    integer                 :: ip, ip2, isea2, i, j, ie, iv, i1, i2, i3
    real                    :: ccos, csin, ccurx, ccury
    real                    :: c(nx,2)
    real*8                  :: kelem(3), ktmp(3), lambda(2)
    real*8                  :: kksum, dtmaxexp
    !/
    !/  velocities
    !/
    real*8,  parameter :: onesixth  = 1.0d0/6.0d0
    real*8,  parameter :: thr8      = tiny(1.0d0)
    real,    parameter :: thr       = tiny(1.0)
    !/ ------------------------------------------------------------------- /
    !
    ! 1.  preparations --------------------------------------------------- *
    ! 1.a set constants
    !
    cflxymax=1e-10
    ip = mapsf(isea,3)
    !
    ! cfl no important on boundary
    !
    if (iobp(ip).eq.1) then
      ccurx  = facx
      ccury  = facy
      !
      ! loop over spectral components
      !
      do ik=1,nkcfl
        do ith=1,nth
          ccos   = facx * ecos(ith)
          csin   = facy * esin(ith)
          c(:,:)=0.
          !
          ! 2.  calculate advection velocities: group speed ---------------- *
          !
          !ar: needs to be rewritten for speed ... single node computation is costly ...
          !ma: you are right but now it is only called if cfx and unst for cfl profiling
          do i = index_cell(ip), index_cell(ip+1)-1
            ie=ie_cell(i)       ! trigp(iv,ie)=ip  with iv=pos_cell(i)
            do j=1,3
              ip2=trigp(j,ie)
              isea2=mapfs(ip2)
              c(ip2,1) = ccos * cg(ik,isea2) / clats(isea2)
              c(ip2,2) = csin * cg(ik,isea2)
              if ( flcur ) then
                if (iobp(ip2) .eq. 1) then
                  c(ip2,1) = c(ip2,1) + ccurx*cx(isea2)/clats(isea2)
                  c(ip2,2) = c(ip2,2) + ccury*cy(isea2)
                end if
              end if  ! end of ( flcur )
            end do
          end do
          !
          !3.     calculate k-values and contour based quantities ...
          !
          kksum = 0.d0
          do i = index_cell(ip), index_cell(ip+1)-1
            ie=ie_cell(i)       ! trigp(iv,ie)=ip
            iv=pos_cell(i)
            i1 = trigp(1,ie)
            i2 = trigp(2,ie)
            i3 = trigp(3,ie)
            lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! advection speed in x direction
            lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2)) ! advection speed in y direction
            kelem(1) = lambda(1) * ien(ie,1) + lambda(2) * ien(ie,2) ! k-values - so called flux jacobians
            kelem(2) = lambda(1) * ien(ie,3) + lambda(2) * ien(ie,4) ! k-values - so called flux jacobians
            kelem(3) = lambda(1) * ien(ie,5) + lambda(2) * ien(ie,6) ! k-values - so called flux jacobians
            ktmp        = kelem ! copy
            kelem       = max(0.d0,ktmp)
            kksum = kksum  +  kelem(iv)
          end do ! countri
          !
          dtmaxexp = si(ip)/max(dble(10.e-10),kksum)
          cflxymax = max(dble(dt)/dtmaxexp,dble(cflxymax))
        end do
      end do
    end if
    !
    return
  end subroutine w3cflug
  !/ ------------------------------------------------------------------- /
  subroutine w3xypfsn2 ( isp, c, lcalc, rd10, rd20, dt, ac)
    !/
    !/
    !/                  +-----------------------------------+
    !/                  |  wwiii version of the wwm fs code |
    !/                  |  by aron roland                   |
    !/                  |     and fabrice  ardhuin          |
    !/                  |  for use in wwiii                 |
    !/                  |  gpl license                      |
    !/                  |  last update :        15-apr-2020 |
    !/                  +-----------------------------------+
    !/
    !/    19-dec-2007 : origination.                        ( version 3.13 )
    !/    25-aug-2011 : change of method for iobpd          ( version 4.04 )
    !/    03-nov-2011 : addition of shoreline reflection    ( version 4.04 )
    !/    15-apr-2020 : adds optional opt-out for cfl on bc ( version 7.08 )
    !/
    !/
    !  1. purpose :
    !     advection of a scalar in a arbitary velocity field on unstructured meshes
    !     for the conservative hyperbolic equation n,t + (c*n),xy = 0 in spatial space
    !     this is the standard explicit n-scheme from roe as formulated in abgrall
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
    !       strace   subroutine tracing (!/s switch)
    !
    !  5. called by :
    !
    !       w3xypug   routine for advection on unstructured grid
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
    use w3gdatmd, only : nk, nth, ntri, nx, ccon, ie_cell,pos_cell, si, &
         ien, trigp, clats, mapsf, iobpd, iobp, iobdp,  &
         iobpa, fsbccfl
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter, dw
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    implicit none
    integer, intent(in)    :: isp                   ! actual frequency/wavenumber, actual wave direction
    real,    intent(in)    :: dt                    ! time intervall for which the advection should be computed for the given velocity field
    real,    intent(in)    :: c(:,:)                ! velocity field in it's x- and y- components,
    double precision, intent(inout):: ac(:)         ! wave action before and after advection
    real,    intent(in)    :: rd10, rd20            ! time interpolation coefficients for boundary conditions
    logical, intent(in)    :: lcalc                 ! switch for the calculation of the max. global time step
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real*8,  parameter :: onesixth  = 1.0d0/6.0d0
    real*8,  parameter :: thr8      = tiny(1.0d0)
    real,    parameter :: thr       = tiny(1.0)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! local integer
    !
    integer :: ip, ie, it, i1, i2, i3, ith, ik
    integer :: ibi, ni(3)
    !
    ! local real
    !
    real    :: rd1, rd2
    !
    ! local double
    !
    real*8  :: utilde, boundary_forcing
    real*8  :: cflxy
    real*8  :: fl11, fl12, fl21, fl22, fl31, fl32
    real*8  :: fl111, fl112, fl211, fl212, fl311, fl312
    real*8  :: dtsi(nx), u(nx)
    real*8  :: dtmaxgl, dtmaxexp, rest
    real*8  :: lambda(2), ktmp(3), cloc(2,3)
    real*8  :: kelem(3,ntri), flall(3,ntri)
    real*8  :: kksum(nx), st(nx)
    real*8  :: nm(ntri)
    ! 1. initialisation
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmaxgl = dble(10.e10)
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ntri ! i precacalculate this arrays below as i assume that current velocity  changes continusly ...
      i1 = trigp(1,ie) ! index of the element nodes (trigp)
      i2 = trigp(2,ie)
      i3 = trigp(3,ie)
      ni = trigp(:,ie)
      lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! linearized advection speed in x and y direction
      lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2))
      kelem(1,ie) = lambda(1) * ien(ie,1) + lambda(2) * ien(ie,2) ! k-values - so called flux jacobians
      kelem(2,ie) = lambda(1) * ien(ie,3) + lambda(2) * ien(ie,4)
      kelem(3,ie) = lambda(1) * ien(ie,5) + lambda(2) * ien(ie,6)
      !
      ktmp        = kelem(:,ie) ! copy
      nm(ie)      = - 1.d0/min(-thr8,sum(min(0.d0,ktmp))) ! n-values
      kelem(:,ie) = max(0.d0,ktmp)
      fl11  = c(i2,1) * ien(ie,1) + c(i2,2) * ien(ie,2) ! weights for simpson integration
      fl12  = c(i3,1) * ien(ie,1) + c(i3,2) * ien(ie,2)
      fl21  = c(i3,1) * ien(ie,3) + c(i3,2) * ien(ie,4)
      fl22  = c(i1,1) * ien(ie,3) + c(i1,2) * ien(ie,4)
      fl31  = c(i1,1) * ien(ie,5) + c(i1,2) * ien(ie,6)
      fl32  = c(i2,1) * ien(ie,5) + c(i2,2) * ien(ie,6)
      fl111 = 2.d0*fl11+fl12
      fl112 = 2.d0*fl12+fl11
      fl211 = 2.d0*fl21+fl22
      fl212 = 2.d0*fl22+fl21
      fl311 = 2.d0*fl31+fl32
      fl312 = 2.d0*fl32+fl31
      flall(1,ie) = (fl311 + fl212) * onesixth + kelem(1,ie)
      flall(2,ie) = (fl111 + fl312) * onesixth + kelem(2,ie)
      flall(3,ie) = (fl211 + fl112) * onesixth + kelem(3,ie)
      ! if (i1.eq.1.or.i2.eq.1.or.i3.eq.1) write(6,*) 'test n1 :',ik,ith,ip,ie,kelem(:,ie),'##',lambda
    end do ! ntri
    if (lcalc) then ! if the current field or water level changes estimate the iteration number based on the new flow field and the cfl number of the scheme
      kksum = 0.d0
      do ie = 1, ntri
        ni = trigp(:,ie)
        kksum(ni) = kksum(ni) + kelem(:,ie)
      end do ! ie
      dtmaxexp = 1e10 ! initialize to large number
      do ip = 1, nx
        if (iobp(ip) .eq. 1 .or. fsbccfl) then
          dtmaxexp = si(ip)/max(dble(10.e-10),kksum(ip)*iobdp(ip))
          dtmaxgl  = min( dtmaxgl, dtmaxexp)
        end if
      end do ! ip
      cflxy = dble(dt)/dtmaxgl
      rest  = abs(mod(cflxy,1.0d0))
      if (rest .lt. thr8) then
        iter(ik,ith) = abs(nint(cflxy))
      else if (rest .gt. thr8 .and. rest .lt. 0.5d0) then
        iter(ik,ith) = abs(nint(cflxy)) + 1
      else
        iter(ik,ith) = abs(nint(cflxy))
      end if
    end if ! lcalc
    do ip = 1, nx
      dtsi(ip) = dble(dt)/dble(iter(ik,ith))/si(ip) ! some precalculations for the time integration.
    end do
    do it = 1, iter(ik,ith)
      u = ac
      st = 0.d0
      do ie = 1, ntri
        ni     = trigp(:,ie)
        utilde = nm(ie) * (dot_product(flall(:,ie),u(ni)))
        st(ni) = st(ni) + kelem(:,ie) * (u(ni) - utilde) ! the 2nd term are the theta values of each node ...
      end do ! ie
      do ip = 1, nx
        !
        ! iobpd=0  : waves coming from land (or outside grid)
        ! possibly set flux to zero by multiplying st by iobpd but also in utilde multiply u(ni) by iobpd ...
        !
        u(ip) = max(0.d0,u(ip)-dtsi(ip)*st(ip)*(1-iobpa(ip)))*dble(iobpd(ith,ip))
      end do
      ! update spectrum
      ac = u
      !
      ! 4 update boundaries: performs interpolation in time as done in rect grids (e.g. w3pro1md.ftn)
      !
      if ( flbpi ) then
        !
        ! 4.1 in this case the boundary is read from the nest.ww3 file
        !
        rd1=rd10 - dt * real(iter(ik,ith)-it)/real(iter(ik,ith))
        rd2=rd20
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        !
        ! overwrites only the incoming energy ( iobpd(ith,ip) = 0)
        !
        do ibi=1, nbi
          ip = mapsf(isbpi(ibi),1)
          ac(ip) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
               / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
        end do
      endif
      !
    end do ! end of loop on time steps
    !        call extcde ( 99 )
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine w3xypfsn2
  !/ ------------------------------------------------------------------- /
  subroutine w3xypfspsi2 ( isp, c, lcalc, rd10, rd20, dt, ac)
    !/
    !/
    !/                  +-----------------------------------+
    !/                  |  wwiii version of the wwm fs code |
    !/                  |  by aron roland                   |
    !/                  |  for use in wwiii                 |
    !/                  |  gpl license                      |
    !/                  |  last update :        19-dec-2007 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !     advection of a scalar in a arbitary velocity field on unstructured meshes
    !     for the conservative hyperbolic equation n,t + (c*n),xy = 0 in spatial space
    !     this is the standard explicit n-scheme from roe as formulated in abgrall
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
    !       strace   subroutine tracing (!/s switch)
    !
    !  5. called by :
    !
    !       w3xypug   routine for advection on unstructured grid
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
    use w3gdatmd, only : nk, nth, ntri, nx, ccon, ie_cell,pos_cell, si, &
         ien, trigp, clats, mapsf, iobpa, iobpd, iobp, nnz, iobdp
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    implicit none
    integer, intent(in)    :: isp                   ! actual frequency/wavenumber, actual wave direction
    real,    intent(in)    :: dt                    ! time intervall for which the advection should be computed for the given velocity field
    real,    intent(in)    :: c(:,:)              ! velocity field in it's x- and y- components,
    double precision,intent(inout) :: ac(:)         ! wave action before and after advection
    real,    intent(in)    :: rd10, rd20            ! time interpolation coefficients for boundary conditions
    logical, intent(in)    :: lcalc                 ! switch for the calculation of the max. global time step
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real*8,  parameter :: onesixth  = 1.0d0/6.0d0
    real*8,  parameter :: thr8      = tiny(1.0d0)
    real,    parameter :: thr       = tiny(1.0)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! local integer
    !
    integer :: ip, ie, it, i1, i2, i3, ith, ik
    integer :: ibi, ni(3)
    !
    ! local real
    !
    real    :: rd1, rd2
    !:
    ! local double
    !
    real*8  :: utilde, boundary_forcing
    real*8  :: ft, cflxy
    real*8  :: fl11, fl12, fl21, fl22, fl31, fl32
    real*8  :: fl111, fl112, fl211, fl212, fl311, fl312
    real*8  :: dtsi(nx), u(nx)
    real*8  :: dtmaxgl, dtmaxexp, rest
    real*8  :: lambda(2), ktmp(3), tmp(3)
    real*8  :: theta_l(3), bet1(3), betahat(3)
    real*8  :: kelem(3,ntri), flall(3,ntri)
    real*8  :: kksum(nx), st(nx)
    real*8  :: nm(ntri)
    ! 1. initialisation
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmaxgl = dble(10.e10)
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ntri ! i precacalculate this arrays below as i assume that current velocity  changes continusly ...
      i1 = trigp(1,ie) ! index of the element nodes (trigp)
      i2 = trigp(2,ie)
      i3 = trigp(3,ie)
      lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! linearized advection speed in x and y direction
      lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2))
      kelem(1,ie) = lambda(1) * ien(ie,1) + lambda(2) * ien(ie,2) ! k-values - so called flux jacobians
      kelem(2,ie) = lambda(1) * ien(ie,3) + lambda(2) * ien(ie,4)
      kelem(3,ie) = lambda(1) * ien(ie,5) + lambda(2) * ien(ie,6)
      ktmp        = kelem(:,ie) ! copy
      nm(ie)      = - 1.d0/min(-thr8,sum(min(0.d0,ktmp))) ! n-values
      kelem(:,ie) = max(0.d0,ktmp)
      fl11  = c(i2,1) * ien(ie,1) + c(i2,2) * ien(ie,2) ! weights for simpson integration
      fl12  = c(i3,1) * ien(ie,1) + c(i3,2) * ien(ie,2)
      fl21  = c(i3,1) * ien(ie,3) + c(i3,2) * ien(ie,4)
      fl22  = c(i1,1) * ien(ie,3) + c(i1,2) * ien(ie,4)
      fl31  = c(i1,1) * ien(ie,5) + c(i1,2) * ien(ie,6)
      fl32  = c(i2,1) * ien(ie,5) + c(i2,2) * ien(ie,6)
      fl111 = 2.d0*fl11+fl12
      fl112 = 2.d0*fl12+fl11
      fl211 = 2.d0*fl21+fl22
      fl212 = 2.d0*fl22+fl21
      fl311 = 2.d0*fl31+fl32
      fl312 = 2.d0*fl32+fl31
      flall(1,ie) = (fl311 + fl212)! * onesixth + kelem(1,ie)
      flall(2,ie) = (fl111 + fl312)! * onesixth + kelem(2,ie)
      flall(3,ie) = (fl211 + fl112)! * onesixth + kelem(3,ie)
    end do ! ntri
    if (lcalc) then ! if the current field or water level changes estimate the iteration number based on the new flow field and the cfl number of the scheme
      kksum = 0.d0
      do ie = 1, ntri
        ni = trigp(:,ie)
        kksum(ni) = kksum(ni) + kelem(:,ie)
      end do ! ie
      dtmaxexp = 1e10 ! initialize to large number
      do ip = 1, nx
        dtmaxexp = si(ip)/max(dble(10.e-10),kksum(ip)*iobdp(ip))
        dtmaxgl  = min( dtmaxgl, dtmaxexp)
      end do ! ip
      cflxy = dble(dt)/dtmaxgl
      rest  = abs(mod(cflxy,1.0d0))
      if (rest .lt. thr8) then
        iter(ik,ith) = abs(nint(cflxy))
      else if (rest .gt. thr8 .and. rest .lt. 0.5d0) then
        iter(ik,ith) = abs(nint(cflxy)) + 1
      else
        iter(ik,ith) = abs(nint(cflxy))
      end if
    end if ! lcalc
    do ip = 1, nx
      dtsi(ip) = dble(dt)/dble(iter(ik,ith))/si(ip) ! some precalculations for the time integration.
    end do
    do it = 1, iter(ik,ith)
      u = ac
      st = 0.d0
      do ie = 1, ntri
        ni   = trigp(:,ie)
        ft     =-onesixth*dot_product(u(ni),flall(:,ie))
        utilde = nm(ie) * ( dot_product(kelem(:,ie),u(ni)) - ft )
        theta_l(:) = kelem(:,ie) * (u(ni) - utilde)
        if (abs(ft) .gt. 0.0d0) then
          bet1(:) = theta_l(:)/ft
          if (any( bet1 .lt. 0.0d0) ) then
            betahat(1)    = bet1(1) + 0.5d0 * bet1(2)
            betahat(2)    = bet1(2) + 0.5d0 * bet1(3)
            betahat(3)    = bet1(3) + 0.5d0 * bet1(1)
            bet1(1)       = max(0.d0,min(betahat(1),1.d0-betahat(2),1.d0))
            bet1(2)       = max(0.d0,min(betahat(2),1.d0-betahat(3),1.d0))
            bet1(3)       = max(0.d0,min(betahat(3),1.d0-betahat(1),1.d0))
            theta_l(:) = ft * bet1
          end if
        else
          theta_l(:) = 0.d0
        end if
        st(ni) = st(ni) + theta_l ! the 2nd term are the theta values of each node ...
      end do
      do ip = 1, nx
        u(ip) = max(0.d0,u(ip)-dtsi(ip)*st(ip)*(1-iobpa(ip)))*dble(iobpd(ith,ip))
      end do
      ! update spectrum
      ac = u
      !
      ! 4 update boundaries: performs interpolation in time as done in rect grids (e.g. w3pro1md.ftn)
      !
      if ( flbpi ) then
        !
        ! 4.1 in this case the boundary is read from the nest.ww3 file
        !
        rd1=rd10 - dt * real(iter(ik,ith)-it)/real(iter(ik,ith))
        rd2=rd20
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        !
        ! overwrites only the incoming energy ( iobpd(ith,ip) = 0)
        !
        do ibi=1, nbi
          ip = mapsf(isbpi(ibi),1)
          ac(ip) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
               / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
        end do
      endif
      !
    end do ! end of loop on time steps
    !      call extcde ( 99 )
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine w3xypfspsi2
  !/ ------------------------------------------------------------------- /
  subroutine w3xypfsnimp ( isp, c, lcalc, rd10, rd20, dt, ac)
    !/
    !/
    !/                  +-----------------------------------+
    !/                  |  wwiii version of the wwm fs code |
    !/                  |  by aron roland                   |
    !/                  |  for use in wwiii                 |
    !/                  |  gpl license                      |
    !/                  |  last update :        15-dec-2013 |
    !/                  +-----------------------------------+
    !/
    !/    15-dec-2013 : bug fix for implicit scheme         ( version 4.16 )
    !/
    !  1. purpose :
    !     advection of a scalar in a arbitary velocity field on unstructured meshes
    !     for the conservative hyperbolic equation n,t + (c*n),xy = 0 in spatial space
    !     this is the standard explicit n-scheme from roe as formulated in abgrall
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
    !       strace   subroutine tracing (!/s switch)
    !
    !  5. called by :
    !
    !       w3xypug   routine for advection on unstructured grid
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
    use w3gdatmd, only : nk, nth, ntri, nx, ccon, ie_cell,pos_cell, si, &
         ien, trigp, clats, mapsf, iobpd, iobpa, iobp, iaa, jaa, posi, &
         tria, nnz
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    implicit none
    integer, intent(in)    :: isp                   ! actual frequency/wavenumber, actual wave direction
    real,    intent(in)    :: dt                    ! time intervall for which the advection should be computed for the given velocity field
    real,    intent(in)    :: c(:,:)              ! velocity field in it's x- and y- components,
    double precision,intent(inout) :: ac(:)         ! wave action before and after advection
    real,    intent(in)    :: rd10, rd20            ! time interpolation coefficients for boundary conditions
    logical, intent(in)    :: lcalc                 ! switch for the calculation of the max. global time step
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real*8,  parameter :: onesixth  = 1.0d0/6.0d0
    real*8,  parameter :: onethird  = 1.0d0/3.0d0
    real*8,  parameter :: thr8      = tiny(1.0d0)
    real,    parameter :: thr       = tiny(1.0)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! local integer
    !
    integer :: ip, ie, pos, i1, i2, i3, i, j, ith, ik
    integer :: ibi
    !
    ! local real
    !
    real    :: rd1, rd2
    !:
    ! local double
    !
    real*8  :: boundary_forcing
    real*8  :: fl11, fl12, fl21, fl22, fl31, fl32
    real*8  :: u(nx)
    real*8  :: dtmaxgl
    real*8  :: lambda(2)
    real*8  :: kp(3,ntri)
    real*8  :: k1, dtk, tmp3, km(3), k(3)
    real*8  :: nm(ntri), crfs(3), deltal(3,ntri)
    real*8  :: b(nx), x(nx)
    real*8  :: aspar(nnz)
    integer :: iwksp( 20*nx )
    integer :: flju(nx)
    integer :: fljau(nnz+1)
    integer :: pos_trick(3,2)
    integer :: ipar(16)
    integer :: ierror ! iwk                             ! error indicator and work array size,
    integer :: jau(nnz+1), ju(nx)
    real*8  :: fpar(16)  ! droptol
    real*8  :: wksp( 8 * nx ) ! real workspaces
    real*8  :: au(nnz+1)
    real*8  :: iniu(nx)
    external bcgstab
    pos_trick(1,1) = 2
    pos_trick(1,2) = 3
    pos_trick(2,1) = 3
    pos_trick(2,2) = 1
    pos_trick(3,1) = 1
    pos_trick(3,2) = 2
    ! 1. initialisation
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmaxgl = dble(10.e10)
    if (.false.) then
      write(*,*) 'nnz', nnz
      write(*,*) 'minval iaa,jaa', minval(iaa), minval(jaa)
      write(*,*) 'minval iaa,jaa', maxval(iaa), maxval(jaa)
      write(*,*) 'max/min posi',   maxval(posi), minval(posi)
      write(*,*) 'ac, aq', sum(ac)
    end if
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ntri ! i precacalculate this arrays below as i assume that current velocity  changes continusly ...
      i1 = trigp(1,ie) ! index of the element nodes (trigp)
      i2 = trigp(2,ie)
      i3 = trigp(3,ie)
      lambda(1) = onesixth * (c(i1,1)+c(i2,1)+c(i3,1))
      lambda(2) = onesixth * (c(i1,2)+c(i2,2)+c(i3,2))
      k(1)  = lambda(1) * ien(ie,1) + lambda(2) * ien(ie,2)
      k(2)  = lambda(1) * ien(ie,3) + lambda(2) * ien(ie,4)
      k(3)  = lambda(1) * ien(ie,5) + lambda(2) * ien(ie,6)
      kp(1,ie) = max(0.d0,k(1))
      kp(2,ie) = max(0.d0,k(2))
      kp(3,ie) = max(0.d0,k(3))
      km(1) = min(0.d0,k(1))
      km(2) = min(0.d0,k(2))
      km(3) = min(0.d0,k(3))
      fl11 = c(i2,1)*ien(ie,1)+c(i2,2)*ien(ie,2)
      fl12 = c(i3,1)*ien(ie,1)+c(i3,2)*ien(ie,2)
      fl21 = c(i3,1)*ien(ie,3)+c(i3,2)*ien(ie,4)
      fl22 = c(i1,1)*ien(ie,3)+c(i1,2)*ien(ie,4)
      fl31 = c(i1,1)*ien(ie,5)+c(i1,2)*ien(ie,6)
      fl32 = c(i2,1)*ien(ie,5)+c(i2,2)*ien(ie,6)
      crfs(1) =  - onesixth *  (2.0d0 *fl31 + fl32 + fl21 + 2.0d0 * fl22 )
      crfs(2) =  - onesixth *  (2.0d0 *fl32 + 2.0d0 * fl11 + fl12 + fl31 )
      crfs(3) =  - onesixth *  (2.0d0 *fl12 + 2.0d0 * fl21 + fl22 + fl11 )
      deltal(:,ie) = crfs(:)- kp(:,ie)
      nm(ie)       = 1.d0/min(dble(thr),sum(km(:)))
    end do ! ntri
    u = dble(ac)
    j = 0
    aspar = 0.d0
    b     = 0.d0
    do ip = 1, nx
      do i = 1, ccon(ip)
        j = j + 1
        ie    =  ie_cell(j)
        pos   =  pos_cell(j)
        k1    =  kp(pos,ie) * iobpd(ith,ip)
        if (k1 > 0.) then
          dtk   =  k1 * dt
          tmp3  =  dtk * nm(ie)
          i1    =  posi(1,j)
          i2    =  posi(2,j)
          i3    =  posi(3,j)
          aspar(i1) =  onethird * tria(ie) + dtk - tmp3 * deltal(pos,ie)              + aspar(i1)
          aspar(i2) =                         - tmp3 * deltal(pos_trick(pos,1),ie) + aspar(i2)
          aspar(i3) =                         - tmp3 * deltal(pos_trick(pos,2),ie) + aspar(i3)
          b(ip)     =  b(ip) + onethird * tria(ie) * u(ip)
        else
          i1    =  posi(1,j)
          aspar(i1) =  onethird * tria(ie) + aspar(i1)
          b(ip)     =  b(ip) + onethird * tria(ie) * u(ip)
        end if
      end do ! end loop over connected elements ...
    end do
    !
    !2do setup a semi-implicit integration scheme for source terms only ...
    !
    ipar(1) = 0       ! always 0 to start an iterative solver
    ipar(2) = 1       ! right preconditioning
    ipar(3) = 1       ! use convergence test scheme 1
    ipar(4) = 8*nx  !
    ipar(5) = 15
    ipar(6) = 1000    ! use at most 1000 matvec's
    fpar(1) = dble(1.0e-8)  ! relative tolerance 1.0e-6
    fpar(2) = dble(1.0e-10)  ! absolute tolerance 1.0e-10
    fpar(11) = 0.d0    ! clearing the flops counter
    au    = 0.
    fljau = 0
    flju  = 0
    jau   = 0
    ju    = 0
    call ilu0  (nx, aspar, jaa, iaa, au, fljau, flju, iwksp, ierror)
    !         write(*,*) 'preconditioner', ierror
    !         if (sum(ac) .gt. 0.) then
    if (.false.) then
      write(*,*) sum(ac)
      write(*,*) 'call solver'
      write(*,*) 'write cg', sum(cg)
      write(*,*) 'b, x, ac, u', sum(b), sum(x), sum(ac), sum(u)
      write(*,*) 'ipar, fpar', sum(ipar), sum(fpar)
      write(*,*) 'wksp, iniu', sum(wksp), sum(iniu)
      write(*,*) 'aspar, jaa, iaa',sum(aspar), sum(iaa), sum(jaa)
      write(*,*) 'au, fljau, flju',sum(au), sum(fljau), sum(flju)
    end if
    iniu = u
    x    = 0.d0
    call runrc (nx, b, x, ipar, fpar, wksp, iniu, aspar, jaa, iaa, au, fljau, flju, bcgstab)
    if (.false.) then
      write(*,*) 'solution'
      write(*,*) 'b, x, ac, u', sum(b), sum(x), sum(ac), sum(u)
      write(*,*) 'ipar, fpar', sum(ipar), sum(fpar)
      write(*,*) 'wksp, iniu', sum(wksp), sum(iniu)
      write(*,*) 'aspar, jaa, iaa', sum(aspar), sum(jaa), sum(iaa)
      write(*,*) 'au, fljau, flju', sum(au), sum(fljau), sum(flju)
    end if
    do ip = 1,nx
      u(ip) = max(0.d0,x(ip)*dble(iobpd(ith,ip)))
    end do
    !
    ! update spectrum
    ac = real(u)
    !
    ! 4 update boundaries: performs interpolation in time as done in rect grids (e.g. w3pro1md.ftn)
    !
    if ( flbpi ) then
      rd1=rd10
      rd2=rd20
      if ( rd2 .gt. 0.001 ) then
        rd2    = min(1.,max(0.,rd1/rd2))
        rd1    = 1. - rd2
      else
        rd1    = 0.
        rd2    = 1.
      end if
      !
      ! time interpolation as done in rect grids
      !
      do ibi=1, nbi
        ip    = mapsf(isbpi(ibi),1)
        ac(ip) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
             *iobpa(ip)*iobpd(ith,ip) / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
      end do
    end if
    !      call extcde ( 99 )
    !/
    !/ end of w3xypfsnimp------------------------------------------------- /
    !/
  end subroutine w3xypfsnimp
  !/ ------------------------------------------------------------------- /
  subroutine w3xypfsfct2 ( isp, c, lcalc, rd10, rd20, dt, ac)
    !/
    !/
    !/                  +-----------------------------------+
    !/                  |  wwiii version of the wwm fs code |
    !/                  |  by aron roland                   |
    !/                  |  for use in wwiii                 |
    !/                  |  gpl license                      |
    !/                  |  last update :        19-dec-2007 |
    !/                  +-----------------------------------+
    !/
    !  1. purpose :
    !     advection of a scalar in a arbitary velocity field on unstructured meshes
    !     for the conservative hyperbolic equation n,t + (c*n),xy = 0 in spatial space
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
    !       strace   subroutine tracing (!/s switch)
    !
    !  5. called by :
    !
    !       w3xypug   routine for advection on unstructured grid
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
    use w3gdatmd, only : nk, nth, ntri, nx, ccon, ie_cell,pos_cell, si, &
         ien, trigp, clats, mapsf, iobpd, iobpa, tria, iobdp
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    implicit none
    integer, intent(in)    :: isp                   ! actual frequency/wavenumber, actual wave direction
    real,    intent(in)    :: dt                    ! time intervall for which the advection should be computed for the given velocity field
    real,    intent(in)    :: c(:,:)              ! velocity field in it's x- and y- components,
    double precision,    intent(inout) :: ac(:)               ! wave action before and after advection
    real,    intent(in)    :: rd10, rd20            ! time interpolation coefficients for boundary condition
    logical, intent(in)    :: lcalc                 ! switch for the calculation of the max. global time step
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    real*8,  parameter :: onesixth  = 1.0d0/6.0d0
    real*8,  parameter :: onethird  = 1.0d0/3.0d0
    real*8,  parameter :: thr8      = tiny(1.0d0)
    real,    parameter :: thr       = tiny(1.0)
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    ! local integer
    !
    integer :: ip, ie, it, i1, i2, i3, i, ith, ik
    integer :: ibi, ni(3)
    !
    ! local real
    !
    real    :: rd1, rd2
    !:
    ! local double
    !
    real*8  :: utilde, boundary_forcing
    real*8  :: ft, cflxy
    real*8  :: fl11, fl12, fl21, fl22, fl31, fl32
    real*8  :: fl111, fl112, fl211, fl212, fl311, fl312
    real*8  :: dtsi(nx), u(nx), dt4ai
    real*8  :: dtmaxgl, dtmaxexp, rest
    real*8  :: lambda(2), ktmp(3), tmp(3)
    real*8  :: bet1(3), betahat(3)
    real*8  :: theta_l(3,ntri), theta_h(3,ntri), theta_ace(3,ntri), utmp(3)
    real*8  :: wii(2,nx), ul(nx), ustari(2,nx)
    real*8  :: pm(nx), pp(nx), uim(nx), uip(nx)
    real*8  :: kelem(3,ntri), flall(3,ntri)
    real*8  :: kksum(nx), st(nx), beta
    real*8  :: nm(ntri)
    ! 1. initialisation
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmaxgl = dble(10.e10)
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ntri ! i precacalculate this arrays below as i assume that current velocity  changes continusly ...
      i1 = trigp(1,ie) ! index of the element nodes (trigp)
      i2 = trigp(2,ie)
      i3 = trigp(3,ie)
      lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! linearized advection speed in x and y direction
      lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2))
      kelem(1,ie) = lambda(1) * ien(ie,1) + lambda(2) * ien(ie,2) ! k-values - so called flux jacobians
      kelem(2,ie) = lambda(1) * ien(ie,3) + lambda(2) * ien(ie,4)
      kelem(3,ie) = lambda(1) * ien(ie,5) + lambda(2) * ien(ie,6)
      ktmp        = kelem(:,ie) ! copy
      nm(ie)      = - 1.d0/min(-thr8,sum(min(0.d0,ktmp))) ! n-values
      fl11  = c(i2,1) * ien(ie,1) + c(i2,2) * ien(ie,2) ! weights for simpson integration
      fl12  = c(i3,1) * ien(ie,1) + c(i3,2) * ien(ie,2)
      fl21  = c(i3,1) * ien(ie,3) + c(i3,2) * ien(ie,4)
      fl22  = c(i1,1) * ien(ie,3) + c(i1,2) * ien(ie,4)
      fl31  = c(i1,1) * ien(ie,5) + c(i1,2) * ien(ie,6)
      fl32  = c(i2,1) * ien(ie,5) + c(i2,2) * ien(ie,6)
      fl111 = 2.d0*fl11+fl12
      fl112 = 2.d0*fl12+fl11
      fl211 = 2.d0*fl21+fl22
      fl212 = 2.d0*fl22+fl21
      fl311 = 2.d0*fl31+fl32
      fl312 = 2.d0*fl32+fl31
      flall(1,ie) = (fl311 + fl212)! * onesixth + kelem(1,ie)
      flall(2,ie) = (fl111 + fl312)! * onesixth + kelem(2,ie)
      flall(3,ie) = (fl211 + fl112)! * onesixth + kelem(3,ie)
    end do ! ntri
    if (lcalc) then ! if the current field or water level changes estimate the iteration number based on the new flow field and the cfl number of the scheme
      kksum = 0.d0
      do ie = 1, ntri
        ni = trigp(:,ie)
        kksum(ni) = kksum(ni) + kelem(:,ie)
      end do ! ie
      dtmaxexp = 1e10 ! initialize to large number
      do ip = 1, nx
        dtmaxexp = si(ip)/max(dble(10.e-10),kksum(ip)*iobdp(ip))
        dtmaxgl  = min( dtmaxgl, dtmaxexp)
      end do ! ip
      cflxy = dble(dt)/dtmaxgl
      rest  = abs(mod(cflxy,1.0d0))
      if (rest .lt. thr8) then
        iter(ik,ith) = abs(nint(cflxy))
      else if (rest .gt. thr8 .and. rest .lt. 0.5d0) then
        iter(ik,ith) = abs(nint(cflxy)) + 1
      else
        iter(ik,ith) = abs(nint(cflxy))
      end if
    end if ! lcalc
    dt4ai = dble(dt)/dble(iter(ik,ith))
    dtsi(:)  = dt4ai/si(:) ! some precalculations for the time integration.
    u = ac
    ul = u
    !
    !  now loop on sub-timesteps
    !
    do it = 1, iter(ik,ith)
      st = 0.d0
      do ie = 1, ntri
        ni      = trigp(:,ie)
        utmp    = u(ni)
        ft      =  - onesixth*dot_product(utmp,flall(:,ie))
        tmp     =  max(0.d0,kelem(:,ie))
        utilde  =  nm(ie) * ( dot_product(tmp,utmp) - ft )
        theta_l(:,ie) =  tmp * ( utmp - utilde )
        if (abs(ft) .gt. dble(thr)) then
          bet1(:) = theta_l(:,ie)/ft
          if (any( bet1 .lt. 0.0d0) ) then
            betahat(1)    = bet1(1) + 0.5d0 * bet1(2)
            betahat(2)    = bet1(2) + 0.5d0 * bet1(3)
            betahat(3)    = bet1(3) + 0.5d0 * bet1(1)
            bet1(1)       = max(0.d0,min(betahat(1),1.d0-betahat(2),1.d0))
            bet1(2)       = max(0.d0,min(betahat(2),1.d0-betahat(3),1.d0))
            bet1(3)       = max(0.d0,min(betahat(3),1.d0-betahat(1),1.d0))
            theta_l(:,ie) = ft * bet1
          end if
        else
          theta_l(:,ie) = 0.d0
        end if
        !              theta_h(:,ie) = (onethird+dt4ai/(2.d0*tria(ie)) * kelem(:,ie))*ft ! lax-wendroff
        theta_h(:,ie) = (1./3.+2./3.* kelem(:,ie)/sum(abs(kelem(:,ie))) )*ft ! central scheme
        ! antidiffusive residual according to the higher order nonmonotone scheme
        theta_ace(:,ie) = ((theta_h(:,ie) - theta_l(:,ie))) * dt4ai/si(ni)
        st(ni)          = st(ni) + theta_l(:,ie)*dt4ai/si(ni)
      end do
      !            ul          = max(0.d0,u-st)*dble(iobpd(ith,:))!*dble(iobdp(:)) ... add for iobdp dry/wet flag.
      do ip = 1,nx
        ul(ip) = max(0.d0,u(ip)-st(ip))*dble(iobpd(ith,ip))
      end do
      ustari(1,:) = max(ul,u)
      ustari(2,:) = min(ul,u)
      uip = -thr8
      uim =  thr8
      pp  = 0.d0
      pm  = 0.d0
      do ie = 1, ntri
        ni = trigp(:,ie)
        pp(ni)  = pp(ni) + max(  thr8, -theta_ace(:,ie))
        pm(ni)  = pm(ni) + min( -thr8, -theta_ace(:,ie))
        uip(ni) = max (uip(ni), maxval( ustari(1,ni) ))
        uim(ni) = min (uim(ni), minval( ustari(2,ni) ))
      end do
      wii(1,:) = min(1.0d0,(uip-ul)/max( thr8,pp))
      wii(2,:) = min(1.0d0,(uim-ul)/min(-thr8,pm))
      st = 0.d0
      do ie = 1, ntri
        do i = 1, 3
          ip = trigp(i,ie)
          if (-theta_ace(i,ie) .ge. 0.) then
            tmp(i) = wii(1,ip)
          else
            tmp(i) = wii(2,ip)
          end if
        end do
        beta = minval(tmp)
        ni = trigp(:,ie)
        st(ni) = st(ni) + beta * theta_ace(:,ie)
      end do
      do ip = 1,nx
        !
        ! iobpd is the switch for removing energy coming from the shoreline
        !
        u(ip) = max(0.d0,ul(ip)-st(ip))*dble(iobpd(ith,ip))
      end do
      !
      ! update spectrum
      ac = u
      !
      ! 4 update boundaries: performs interpolation in time as done in rect grids (e.g. w3pro1md.ftn)
      !
      if ( flbpi ) then
        !
        ! 4.1 in this case the boundary is read from the nest.ww3 file
        !
        rd1=rd10 - dt * real(iter(ik,ith)-it)/real(iter(ik,ith))
        rd2=rd20
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        !
        ! overwrites only the incoming energy ( iobpd(ith,ip) = 0)
        !
        do ibi=1, nbi
          ip = mapsf(isbpi(ibi),1)
          ac(ip) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
               / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
        end do
      endif
      !
    end do ! end of loop on time steps
    !      call extcde ( 99 )
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine w3xypfsfct2
  !/ ------------------------------------------------------------------- /
  subroutine setdepth
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
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : init pdlib part
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
    use w3gdatmd, only: mapsf, nseal, dmin, iobdp, mapsta, iobp, mapfs, nx
    use w3adatmd, only: dw
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
    integer :: jsea, isea, ix, ip
    real*8, parameter :: dthr = 10e-6
    iobdp = 1
    do ip=1,nx
      if (dw(ip) .lt. dmin + dthr) iobdp(ip) = 0
      !write(*,*) ip, ip_glob, mapsta(1,ip_glob), iobp(ip_glob), dw(isea), dmin
    end do
  end subroutine setdepth
  !/ ------------------------------------------------------------------- /
end module w3profsmd
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
!------------------iterative solver ---------------------------------------
!----------------------------------------------------------------------c
!                          s p a r s k i t                             c
!----------------------------------------------------------------------c
!         basic iterative solvers with reverse communication           c
!----------------------------------------------------------------------c
!     this file currently has several basic iterative linear system    c
!     solvers. they are:                                               c
!     cg       -- conjugate gradient method                            c
!     cgnr     -- conjugate gradient method (normal residual equation) c
!     bcg      -- bi-conjugate gradient method                         c
!     dbcg     -- bcg with partial pivoting                            c
!     bcgstab  -- bcg stabilized                                       c
!     tfqmr    -- transpose-free quasi-minimum residual method         c
!     fom      -- full orthogonalization method                        c
!     gmres    -- generalized minimum residual method (no longer available) c
!     fgmres   -- flexible version of generalized minimum              c
!                 residual method                                      c
!     dqgmres  -- direct versions of quasi generalize minimum          c
!                 residual method                                      c
!----------------------------------------------------------------------c
!     they all have the following calling sequence:
!      subroutine solver(n, rhs, sol, ipar, fpar, w)
!      integer n, ipar(16)
!      real*8 rhs(n), sol(n), fpar(16), w(*)
!     where
!     (1) 'n' is the size of the linear system,
!     (2) 'rhs' is the right-hand side of the linear system,
!     (3) 'sol' is the solution to the linear system,
!     (4) 'ipar' is an integer parameter array for the reverse
!     communication protocol,
!     (5) 'fpar' is an floating-point parameter array storing
!     information to and from the iterative solvers.
!     (6) 'w' is the work space (size is specified in ipar)
!
!     they are preconditioned iterative solvers with reverse
!     communication. the preconditioners can be applied from either
!     from left or right or both (specified by ipar(2), see below).
!
!     author: kesheng john wu (kewu@mail.cs.umn.edu) 1993
!
!     notes:
!
!     (1) work space required by each of the iterative solver
!     routines is as follows:
!       cg      == 5 * n
!       cgnr    == 5 * n
!       bcg     == 7 * n
!       dbcg    == 11 * n
!       bcgstab == 8 * n
!       tfqmr   == 11 * n
!       fom     == (n+3)*(m+2) + (m+1)*m/2 (m = ipar(5), default m=15)
!       gmres   == (n+3)*(m+2) + (m+1)*m/2 (m = ipar(5), default m=15)
!          gmres is no longer available
!       fgmres  == 2*n*(m+1) + (m+1)*m/2 + 3*m + 2 (m = ipar(5),
!                  default m=15)
!       dqgmres == n + lb * (2*n+4) (lb=ipar(5)+1, default lb = 16)
!
!     (2) all iterative solvers require a user-supplied dot-product
!     routine named ddot. the prototype of ddot is
!
!     real*8 function ddot(n,x,y)
!     integer n, ix, iy
!     real*8 x(1+(n-1)*ix), y(1+(n-1)*iy)
!
!     this interface of ddot is exactly the same as that of
!     ddot (or sdot if real == real*8) from blas-1. it should have
!     same functionality as ddot on a single processor machine. on a
!     parallel/distributed environment, each processor can perform
!     ddot on the data it has, then perform a summation on all the
!     partial results.
!
!     (3) to use this set of routines under spmd/mimd program paradigm,
!     several things are to be noted: (a) 'n' should be the number of
!     vector elements of 'rhs' that is present on the local processor.
!     (b) if rhs(i) is on processor j, it is expected that sol(i)
!     will be on the same processor, i.e. the vectors are distributed
!     to each processor in the same way. (c) the preconditioning and
!     stopping criteria specifications have to be the same on all
!     processor involved, ipar and fpar have to be the same on each
!     processor. (d) ddot should be replaced by a distributed
!     dot-product function.
!
!     ..................................................................
!     reverse communication protocols
!
!     when a reverse-communication routine returns, it could be either
!     that the routine has terminated or it simply requires the caller
!     to perform one matrix-vector multiplication. the possible matrices
!     that involve in the matrix-vector multiplications are:
!     a       (the matrix of the linear system),
!     a^t     (a transposed),
!     ml^{-1} (inverse of the left preconditioner),
!     ml^{-t} (inverse of the left preconditioner transposed),
!     mr^{-1} (inverse of the right preconditioner),
!     mr^{-t} (inverse of the right preconditioner transposed).
!     for all the matrix vector multiplication, v = a u. the input and
!     output vectors are supposed to be part of the work space 'w', and
!     the starting positions of them are stored in ipar(8:9), see below.
!
!     the array 'ipar' is used to store the information about the solver.
!     here is the list of what each element represents:
!
!     ipar(1) -- status of the call/return.
!     a call to the solver with ipar(1) == 0 will initialize the
!     iterative solver. on return from the iterative solver, ipar(1)
!     carries the status flag which indicates the condition of the
!     return. the status information is divided into two categories,
!     (1) a positive value indicates the solver requires a matrix-vector
!     multiplication,
!     (2) a non-positive value indicates termination of the solver.
!     here is the current definition:
!       1 == request a matvec with a,
!       2 == request a matvec with a^t,
!       3 == request a left preconditioner solve (ml^{-1}),
!       4 == request a left preconditioner transposed solve (ml^{-t}),
!       5 == request a right preconditioner solve (mr^{-1}),
!       6 == request a right preconditioner transposed solve (mr^{-t}),
!      10 == request the caller to perform stopping test,
!       0 == normal termination of the solver, satisfied the stopping
!            criteria,
!      -1 == termination because iteration number is greater than the
!            preset limit,
!      -2 == return due to insufficient work space,
!      -3 == return due to anticipated break-down / divide by zero,
!            in the case where arnoldi procedure is used, additional
!            error code can be found in ipar(12), where ipar(12) is
!            the error code of orthogonalization procedure mgsro:
!               -1: zero input vector
!               -2: input vector contains abnormal numbers
!               -3: input vector is a linear combination of others
!               -4: trianguler system in gmres/fom/etc. has nul rank
!      -4 == the values of fpar(1) and fpar(2) are both <= 0, the valid
!            ranges are 0 <= fpar(1) < 1, 0 <= fpar(2), and they can
!            not be zero at the same time
!      -9 == while trying to detect a break-down, an abnormal number is
!            detected.
!     -10 == return due to some non-numerical reasons, e.g. invalid
!            floating-point numbers etc.
!
!     ipar(2) -- status of the preconditioning:
!       0 == no preconditioning
!       1 == left preconditioning only
!       2 == right preconditioning only
!       3 == both left and right preconditioning
!
!     ipar(3) -- stopping criteria (details of this will be
!     discussed later).
!
!     ipar(4) -- number of elements in the array 'w'. if this is less
!     than the desired size, it will be over-written with the minimum
!     requirement. in which case the status flag ipar(1) = -2.
!
!     ipar(5) -- size of the krylov subspace (used by gmres and its
!     variants), e.g. gmres(ipar(5)), fgmres(ipar(5)),
!     dqgmres(ipar(5)).
!
!     ipar(6) -- maximum number of matrix-vector multiplies, if not a
!     positive number the iterative solver will run till convergence
!     test is satisfied.
!
!     ipar(7) -- current number of matrix-vector multiplies. it is
!     incremented after each matrix-vector multiplication. if there
!     is preconditioning, the counter is incremented after the
!     preconditioning associated with each matrix-vector multiplication.
!
!     ipar(8) -- pointer to the input vector to the requested matrix-
!     vector multiplication.
!
!     ipar(9) -- pointer to the output vector of the requested matrix-
!     vector multiplication.
!
!     to perform v = a * u, it is assumed that u is w(ipar(8):ipar(8)+n-1)
!     and v is stored as w(ipar(9):ipar(9)+n-1).
!
!     ipar(10) -- the return address (used to determine where to go to
!     inside the iterative solvers after the caller has performed the
!     requested services).
!
!     ipar(11) -- the result of the external convergence test
!     on final return from the iterative solvers, this value
!     will be reflected by ipar(1) = 0 (details discussed later)
!
!     ipar(12) -- error code of mgsro, it is
!                  1 if the input vector to mgsro is linear combination
!                    of others,
!                  0 if mgsro was successful,
!                 -1 if the input vector to mgsro is zero,
!                 -2 if the input vector contains invalid number.
!
!     ipar(13) -- number of initializations. during each initilization
!                 residual norm is computed directly from m_l(b - a x).
!
!     ipar(14) to ipar(16) are not defined, they are not used by
!     any iterative solver at this time.
!
!     information about the error and tolerance are stored in the array
!     fpar. so are some internal variables that need to be saved from
!     one iteration to the next one. since the internal variables are
!     not the same for each routine, we only define the common ones.
!
!     the first two are input parameters:
!     fpar(1) -- the relative tolerance,
!     fpar(2) -- the absolute tolerance (details discussed later),
!
!     when the iterative solver terminates,
!     fpar(3) -- initial residual/error norm,
!     fpar(4) -- target residual/error norm,
!     fpar(5) -- current residual norm (if available),
!     fpar(6) -- current residual/error norm,
!     fpar(7) -- convergence rate,
!
!     fpar(8:10) are used by some of the iterative solvers to save some
!     internal information.
!
!     fpar(11) -- number of floating-point operations. the iterative
!     solvers will add the number of flops they used to this variable,
!     but they do not initialize it, nor add the number of flops due to
!     matrix-vector multiplications (since matvec is outside of the
!     iterative solvers). to insure the correct flops count, the
!     caller should set fpar(11) = 0 before invoking the iterative
!     solvers and account for the number of flops from matrix-vector
!     multiplications and preconditioners.
!
!     fpar(12:16) are not used in current implementation.
!
!     whether the content of fpar(3), fpar(4) and fpar(6) are residual
!     norms or error norms depends on ipar(3). if the requested
!     convergence test is based on the residual norm, they will be
!     residual norms. if the caller want to test convergence based the
!     error norms (estimated by the norm of the modifications applied
!     to the approximate solution), they will be error norms.
!     convergence rate is defined by (fortran 77 statement)
!     fpar(7) = log10(fpar(3) / fpar(6)) / (ipar(7)-ipar(13))
!     if fpar(7) = 0.5, it means that approximately every 2 (= 1/0.5)
!     steps the residual/error norm decrease by a factor of 10.
!
!     ..................................................................
!     stopping criteria,
!
!     an iterative solver may be terminated due to (1) satisfying
!     convergence test; (2) exceeding iteration limit; (3) insufficient
!     work space; (4) break-down. checking of the work space is
!     only done in the initialization stage, i.e. when it is called with
!     ipar(1) == 0. a complete convergence test is done after each
!     update of the solutions. other conditions are monitored
!     continuously.
!
!     with regard to the number of iteration, when ipar(6) is positive,
!     the current iteration number will be checked against it. if
!     current iteration number is greater the ipar(6) than the solver
!     will return with status -1. if ipar(6) is not positive, the
!     iteration will continue until convergence test is satisfied.
!
!     two things may be used in the convergence tests, one is the
!     residual 2-norm, the other one is 2-norm of the change in the
!     approximate solution. the residual and the change in approximate
!     solution are from the preconditioned system (if preconditioning
!     is applied). the dqgmres and tfqmr use two estimates for the
!     residual norms. the estimates are not accurate, but they are
!     acceptable in most of the cases. generally speaking, the error
!     of the tfqmr's estimate is less accurate.
!
!     the convergence test type is indicated by ipar(3). there are four
!     type convergence tests: (1) tests based on the residual norm;
!     (2) tests based on change in approximate solution; (3) caller
!     does not care, the solver choose one from above two on its own;
!     (4) caller will perform the test, the solver should simply continue.
!     here is the complete definition:
!      -2 == || dx(i) || <= rtol * || rhs || + atol
!      -1 == || dx(i) || <= rtol * || dx(1) || + atol
!       0 == solver will choose test 1 (next)
!       1 == || residual || <= rtol * || initial residual || + atol
!       2 == || residual || <= rtol * || rhs || + atol
!     999 == caller will perform the test
!     where dx(i) denote the change in the solution at the ith update.
!     ||.|| denotes 2-norm. rtol = fpar(1) and atol = fpar(2).
!
!     if the caller is to perform the convergence test, the outcome
!     should be stored in ipar(11).
!     ipar(11) = 0 -- failed the convergence test, iterative solver
!     should continue
!     ipar(11) = 1 -- satisfied convergence test, iterative solver
!     should perform the clean up job and stop.
!
!     upon return with ipar(1) = 10,
!     ipar(8)  points to the starting position of the change in
!              solution sx, where the actual solution of the step is
!              x_j = x_0 + m_r^{-1} sx.
!              exception: ipar(8) < 0, sx = 0. it is mostly used by
!              gmres and variants to indicate (1) sx was not necessary,
!              (2) intermediate result of sx is not computed.
!     ipar(9)  points to the starting position of a work vector that
!              can be used by the caller.
!
!     note: the caller should allow the iterative solver to perform
!     clean up job after the external convergence test is satisfied,
!     since some of the iterative solvers do not directly
!     update the 'sol' array. a typical clean-up stage includes
!     performing the final update of the approximate solution and
!     computing the convergence information (e.g. values of fpar(3:7)).
!
!     note: fpar(4) and fpar(6) are not set by the accelerators (the
!     routines implemented here) if ipar(3) = 999.
!
!     ..................................................................
!     usage:
!
!     to start solving a linear system, the user needs to specify
!     first 6 elements of the ipar, and first 2 elements of fpar.
!     the user may optionally set fpar(11) = 0 if one wants to count
!     the number of floating-point operations. (note: the iterative
!     solvers will only add the floating-point operations inside
!     themselves, the caller will have to add the flops from the
!     matrix-vector multiplication routines and the preconditioning
!     routines in order to account for all the arithmetic operations.)
!
!     here is an example:
!     ipar(1) = 0       ! always 0 to start an iterative solver
!     ipar(2) = 2       ! right preconditioning
!     ipar(3) = 1       ! use convergence test scheme 1
!     ipar(4) = 10000   ! the 'w' has 10,000 elements
!     ipar(5) = 10      ! use *gmres(10) (e.g. fgmres(10))
!     ipar(6) = 100     ! use at most 100 matvec's
!     fpar(1) = 1.0e-6  ! relative tolerance 1.0e-6
!     fpar(2) = 1.0e-10 ! absolute tolerance 1.0e-10
!     fpar(11) = 0.0    ! clearing the flops counter
!
!     after the above specifications, one can start to call an iterative
!     solver, say bcg. here is a piece of pseudo-code showing how it can
!     be done,
!
! 10   call bcg(n,rhs,sol,ipar,fpar,w)
!      if (ipar(1).eq.1) then
!         call amux(n,w(ipar(8)),w(ipar(9)),a,ja,ia)
!         goto 10
!      else if (ipar(1).eq.2) then
!         call atmux(n,w(ipar(8)),w(ipar(9)),a,ja,ia)
!         goto 10
!      else if (ipar(1).eq.3) then
!         left preconditioner solver
!         goto 10
!      else if (ipar(1).eq.4) then
!         left preconditioner transposed solve
!         goto 10
!      else if (ipar(1).eq.5) then
!         right preconditioner solve
!         goto 10
!      else if (ipar(1).eq.6) then
!         right preconditioner transposed solve
!         goto 10
!      else if (ipar(1).eq.10) then
!         call my own stopping test routine
!         goto 10
!      else if (ipar(1).gt.0) then
!         ipar(1) is an unspecified code
!      else
!         the iterative solver terminated with code = ipar(1)
!      endif
!
!     this segment of pseudo-code assumes the matrix is in csr format,
!     amux and atmux are two routines from the sparskit matvec module.
!     they perform matrix-vector multiplications for csr matrices,
!     where w(ipar(8)) is the first element of the input vectors to the
!     two routines, and w(ipar(9)) is the first element of the output
!     vectors from them. for simplicity, we did not show the name of
!     the routine that performs the preconditioning operations or the
!     convergence tests.
!-----------------------------------------------------------------------
subroutine bcgstab(n, rhs, sol, ipar, fpar, w)
  implicit none
  integer n, ipar(16)
  real*8 rhs(n), sol(n), fpar(16), w(n,8)
  !-----------------------------------------------------------------------
  !     bcgstab --- bi conjugate gradient stabilized (bcgstab)
  !     this is an improved bcg routine. (1) no matrix transpose is
  !     involved. (2) the convergence is smoother.
  !
  !
  !     algorithm:
  !     initialization - r = b - a x, r0 = r, p = r, rho = (r0, r),
  !     iterate -
  !     (1) v = a p
  !     (2) alpha = rho / (r0, v)
  !     (3) s = r - alpha v
  !     (4) t = a s
  !     (5) omega = (t, s) / (t, t)
  !     (6) x = x + alpha * p + omega * s
  !     (7) r = s - omega * t
  !     convergence test goes here
  !     (8) beta = rho, rho = (r0, r), beta = rho * alpha / (beta * omega)
  !         p = r + beta * (p - omega * v)
  !
  !     in this routine, before successful return, the fpar's are
  !     fpar(3) == initial (preconditionied-)residual norm
  !     fpar(4) == target (preconditionied-)residual norm
  !     fpar(5) == current (preconditionied-)residual norm
  !     fpar(6) == current residual norm or error
  !     fpar(7) == current rho (rhok = <r, r0>)
  !     fpar(8) == alpha
  !     fpar(9) == omega
  !
  !     usage of the work space w
  !     w(:, 1) = r0, the initial residual vector
  !     w(:, 2) = r, current residual vector
  !     w(:, 3) = s
  !     w(:, 4) = t
  !     w(:, 5) = v
  !     w(:, 6) = p
  !     w(:, 7) = tmp, used in preconditioning, etc.
  !     w(:, 8) = delta x, the correction to the answer is accumulated
  !               here, so that the right-preconditioning may be applied
  !               at the end
  !-----------------------------------------------------------------------
  !     external routines used
  !
  real*8 ddot
  logical stopbis, brkdn
  external ddot, stopbis, brkdn
  !
  real*8 one
  parameter(one=1.0d0)
  !
  !     local variables
  !
  integer i
  real*8 alpha,beta,rho,omega
  logical lp, rp
  save lp, rp
  !
  !     where to go
  !
  if (ipar(1).gt.0) then
    !!goto (10, 20, 40, 50, 60, 70, 80, 90, 100, 110) ipar(10)
    select case (ipar(10))
    case (1)
      goto 10
    case (2)
      goto 20
    case (3)
      goto 40
    case (4)
      goto 50
    case (5)
      goto 60
    case (6)
      goto 70
    case (7)
      goto 80
    case (8)
      goto 90
    case (9)
      goto 100
    case (10)
      goto 110
    end select
  else if (ipar(1).lt.0) then
    goto 900
  endif
  !
  !     call the initialization routine
  !
  call bisinit(ipar,fpar,8*n,1,lp,rp,w)
  if (ipar(1).lt.0) return
  !
  !     perform a matvec to compute the initial residual
  !
  ipar(1) = 1
  ipar(8) = 1
  ipar(9) = 1 + n
  do i = 1, n
    w(i,1) = sol(i)
  enddo
  ipar(10) = 1
  return
10 ipar(7) = ipar(7) + 1
  ipar(13) = ipar(13) + 1
  do i = 1, n
    w(i,1) = rhs(i) - w(i,2)
  enddo
  fpar(11) = fpar(11) + n
  if (lp) then
    ipar(1) = 3
    ipar(10) = 2
    return
  endif
  !
20 if (lp) then
    do i = 1, n
      w(i,1) = w(i,2)
      w(i,6) = w(i,2)
    enddo
  else
    do i = 1, n
      w(i,2) = w(i,1)
      w(i,6) = w(i,1)
    enddo
  endif
  !
  fpar(7) = ddot(n,w,w)
  fpar(11) = fpar(11) + 2 * n
  fpar(5) = sqrt(fpar(7))
  fpar(3) = fpar(5)
  if (abs(ipar(3)).eq.2) then
    fpar(4) = fpar(1) * sqrt(ddot(n,rhs,rhs)) + fpar(2)
    fpar(11) = fpar(11) + 2 * n
  else if (ipar(3).ne.999) then
    fpar(4) = fpar(1) * fpar(3) + fpar(2)
  endif
  if (ipar(3).ge.0) fpar(6) = fpar(5)
  if (ipar(3).ge.0 .and. fpar(5).le.fpar(4) .and. ipar(3).ne.999) then
    goto 900
  endif
  !
  !     beginning of the iterations
  !
  !     step (1), v = a p
30 if (rp) then
    ipar(1) = 5
    ipar(8) = 5*n+1
    if (lp) then
      ipar(9) = 4*n + 1
    else
      ipar(9) = 6*n + 1
    endif
    ipar(10) = 3
    return
  endif
  !
40 ipar(1) = 1
  if (rp) then
    ipar(8) = ipar(9)
  else
    ipar(8) = 5*n+1
  endif
  if (lp) then
    ipar(9) = 6*n + 1
  else
    ipar(9) = 4*n + 1
  endif
  ipar(10) = 4
  return
50 if (lp) then
    ipar(1) = 3
    ipar(8) = ipar(9)
    ipar(9) = 4*n + 1
    ipar(10) = 5
    return
  endif
  !
60 ipar(7) = ipar(7) + 1
  !
  !     step (2)
  alpha = ddot(n,w(1,1),w(1,5))
  fpar(11) = fpar(11) + 2 * n
  if (brkdn(alpha, ipar)) goto 900
  alpha = fpar(7) / alpha
  fpar(8) = alpha
  !
  !     step (3)
  do i = 1, n
    w(i,3) = w(i,2) - alpha * w(i,5)
  enddo
  fpar(11) = fpar(11) + 2 * n
  !
  !     step (4): the second matvec -- t = a s
  !
  if (rp) then
    ipar(1) = 5
    ipar(8) = n+n+1
    if (lp) then
      ipar(9) = ipar(8)+n
    else
      ipar(9) = 6*n + 1
    endif
    ipar(10) = 6
    return
  endif
  !
70 ipar(1) = 1
  if (rp) then
    ipar(8) = ipar(9)
  else
    ipar(8) = n+n+1
  endif
  if (lp) then
    ipar(9) = 6*n + 1
  else
    ipar(9) = 3*n + 1
  endif
  ipar(10) = 7
  return
80 if (lp) then
    ipar(1) = 3
    ipar(8) = ipar(9)
    ipar(9) = 3*n + 1
    ipar(10) = 8
    return
  endif
90 ipar(7) = ipar(7) + 1
  !
  !     step (5)
  omega = ddot(n,w(1,4),w(1,4))
  fpar(11) = fpar(11) + n + n
  if (brkdn(omega,ipar)) goto 900
  omega = ddot(n,w(1,4),w(1,3)) / omega
  fpar(11) = fpar(11) + n + n
  if (brkdn(omega,ipar)) goto 900
  fpar(9) = omega
  alpha = fpar(8)
  !
  !     step (6) and (7)
  do i = 1, n
    w(i,7) = alpha * w(i,6) + omega * w(i,3)
    w(i,8) = w(i,8) + w(i,7)
    w(i,2) = w(i,3) - omega * w(i,4)
  enddo
  fpar(11) = fpar(11) + 6 * n + 1
  !
  !     convergence test
  if (ipar(3).eq.999) then
    ipar(1) = 10
    ipar(8) = 7*n + 1
    ipar(9) = 6*n + 1
    ipar(10) = 9
    return
  endif
  if (stopbis(n,ipar,2,fpar,w(1,2),w(1,7),one))  goto 900
100 if (ipar(3).eq.999.and.ipar(11).eq.1) goto 900
  !
  !     step (8): computing new p and rho
  !
  rho = fpar(7)
  fpar(7) = ddot(n,w(1,2),w(1,1))
  omega = fpar(9)
  beta = fpar(7) * fpar(8) / (fpar(9) * rho)
  do i = 1, n
    w(i,6) = w(i,2) + beta * (w(i,6) - omega * w(i,5))
  enddo
  fpar(11) = fpar(11) + 6 * n + 3
  if (brkdn(fpar(7),ipar)) goto 900
  !
  !     end of an iteration
  !
  goto 30
  !
  !     some clean up job to do
  !
900 if (rp) then
    if (ipar(1).lt.0) ipar(12) = ipar(1)
    ipar(1) = 5
    ipar(8) = 7*n + 1
    ipar(9) = ipar(8) - n
    ipar(10) = 10
    return
  endif
110 if (rp) then
    call tidycg(n,ipar,fpar,sol,w(1,7))
  else
    call tidycg(n,ipar,fpar,sol,w(1,8))
  endif
  !
  return
  !-----end-of-bcgstab
end subroutine bcgstab
!-----------------------------------------------------------------------
subroutine implu(np,umm,beta,ypiv,u,permut,full)
  real*8 umm,beta,ypiv(*),u(*),x, xpiv
  logical full, perm, permut(*)
  integer np,k,npm1
  !-----------------------------------------------------------------------
  !     performs implicitly one step of the lu factorization of a
  !     banded hessenberg matrix.
  !-----------------------------------------------------------------------
  if (np .le. 1) goto 12
  npm1 = np - 1
  !
  !     -- perform  previous step of the factorization-
  !
  do k=1,npm1
    if (.not. permut(k)) goto 5
    x=u(k)
    u(k) = u(k+1)
    u(k+1) = x
5   u(k+1) = u(k+1) - ypiv(k)*u(k)
  end do
  !-----------------------------------------------------------------------
  !     now determine pivotal information to be used in the next call
  !-----------------------------------------------------------------------
12 umm = u(np)
  perm = (beta .gt. abs(umm))
  if (.not. perm) goto 4
  xpiv = umm / beta
  u(np) = beta
  goto 8
4 xpiv = beta/umm
8 permut(np) = perm
  ypiv(np) = xpiv
  if (.not. full) return
  !     shift everything up if full...
  do k=1,npm1
    ypiv(k) = ypiv(k+1)
    permut(k) = permut(k+1)
  end do
  return
  !-----end-of-implu
end subroutine implu
!-----------------------------------------------------------------------
subroutine uppdir(n,p,np,lbp,indp,y,u,usav,flops)
  implicit none
  integer   :: k,np,n,npm1,j,ju,indp,lbp
  real*8    :: p(n,lbp), y(*), u(*), usav(*), x, flops
  !-----------------------------------------------------------------------
  !     updates the conjugate directions p given the upper part of the
  !     banded upper triangular matrix u.  u contains the non zero
  !     elements of the column of the triangular matrix..
  !-----------------------------------------------------------------------
  real*8 zero
  parameter(zero=0.0d0)
  !
  npm1=np-1
  if (np .le. 1) goto 12
  j=indp
  ju = npm1
10 if (j .le. 0) j=lbp
  x = u(ju) /usav(j)
  if (x .eq. zero) goto 115
  do k=1,n
    y(k) = y(k) - x*p(k,j)
  end do
  flops = flops + 2*n
115 j = j-1
  ju = ju -1
  if (ju .ge. 1) goto 10
12 indp = indp + 1
  if (indp .gt. lbp) indp = 1
  usav(indp) = u(np)
  do  k=1,n
    p(k,indp) = y(k)
  end do
  return
  !-----------------------------------------------------------------------
  !-------end-of-uppdir---------------------------------------------------
end subroutine uppdir
subroutine givens(x,y,c,s)
  implicit none
  real*8   :: x,y,c,s
  !-----------------------------------------------------------------------
  !     given x and y, this subroutine generates a givens' rotation c, s.
  !     and apply the rotation on (x,y) ==> (sqrt(x**2 + y**2), 0).
  !     (see p 202 of "matrix computation" by golub and van loan.)
  !-----------------------------------------------------------------------
  real*8   :: t,one,zero
  parameter (zero=0.0d0,one=1.0d0)
  !
  if (x.eq.zero .and. y.eq.zero) then
    c = one
    s = zero
  else if (abs(y).gt.abs(x)) then
    t = x / y
    x = sqrt(one+t*t)
    s = sign(one / x, y)
    c = t*s
  else if (abs(y).le.abs(x)) then
    t = y / x
    y = sqrt(one+t*t)
    c = sign(one / y, x)
    s = t*c
  else
    !
    !     x or y must be an invalid floating-point number, set both to zero
    !
    x = zero
    y = zero
    c = one
    s = zero
  endif
  x = abs(x*y)
  !
  !     end of givens
  !
  return
end subroutine givens
!-----end-of-givens
!-----------------------------------------------------------------------
logical function stopbis(n,ipar,mvpi,fpar,r,delx,sx)
  implicit none
  integer n,mvpi,ipar(16)
  real*8 fpar(16), r(n), delx(n), sx, ddot
  external ddot
  !-----------------------------------------------------------------------
  !     function for determining the stopping criteria. return value of
  !     true if the stopbis criteria is satisfied.
  !-----------------------------------------------------------------------
  if (ipar(11) .eq. 1) then
    stopbis = .true.
  else
    stopbis = .false.
  endif
  if (ipar(6).gt.0 .and. ipar(7).ge.ipar(6)) then
    ipar(1) = -1
    stopbis = .true.
  endif
  if (stopbis) return
  !
  !     computes errors
  !
  fpar(5) = sqrt(ddot(n,r,r))
  fpar(11) = fpar(11) + 2 * n
  if (ipar(3).lt.0) then
    !
    !     compute the change in the solution vector
    !
    fpar(6) = sx * sqrt(ddot(n,delx,delx))
    fpar(11) = fpar(11) + 2 * n
    if (ipar(7).lt.mvpi+mvpi+1) then
      !
      !     if this is the end of the first iteration, set fpar(3:4)
      !
      fpar(3) = fpar(6)
      if (ipar(3).eq.-1) then
        fpar(4) = fpar(1) * fpar(3) + fpar(2)
      endif
    endif
  else
    fpar(6) = fpar(5)
  endif
  !
  !     .. the test is struct this way so that when the value in fpar(6)
  !       is not a valid number, stopbis is set to .true.
  !
  if (fpar(6).gt.fpar(4)) then
    stopbis = .false.
    ipar(11) = 0
  else
    stopbis = .true.
    ipar(11) = 1
  endif
  !
  return
end function stopbis
!-----end-of-stopbis
!-----------------------------------------------------------------------
subroutine tidycg(n,ipar,fpar,sol,delx)
  implicit none
  integer i,n,ipar(16)
  real*8 fpar(16),sol(n),delx(n)
  !-----------------------------------------------------------------------
  !     some common operations required before terminating the cg routines
  !-----------------------------------------------------------------------
  real*8 zero
  parameter(zero=0.0d0)
  !
  if (ipar(12).ne.0) then
    ipar(1) = ipar(12)
  else if (ipar(1).gt.0) then
    if ((ipar(3).eq.999 .and. ipar(11).eq.1) .or. &
         &        fpar(6).le.fpar(4)) then
      ipar(1) = 0
    else if (ipar(7).ge.ipar(6) .and. ipar(6).gt.0) then
      ipar(1) = -1
    else
      ipar(1) = -10
    endif
  endif
  if (fpar(3).gt.zero .and. fpar(6).gt.zero .and. ipar(7).gt.ipar(13)) then
    fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
  else
    fpar(7) = zero
  endif
  do i = 1, n
    sol(i) = sol(i) + delx(i)
  enddo
  return
end subroutine tidycg
!-----end-of-tidycg
!-----------------------------------------------------------------------
logical function brkdn(alpha, ipar)
  implicit none
  integer ipar(16)
  real*8 alpha, beta, zero, one
  parameter (zero=0.0d0, one=1.0d0)
  !-----------------------------------------------------------------------
  !     test whether alpha is zero or an abnormal number, if yes,
  !     this routine will return .true.
  !
  !     if alpha == 0, ipar(1) = -3,
  !     if alpha is an abnormal number, ipar(1) = -9.
  !-----------------------------------------------------------------------
  brkdn = .false.
  if (alpha.gt.zero) then
    beta = one / alpha
    if (.not. beta.gt.zero) then
      brkdn = .true.
      ipar(1) = -9
    endif
  else if (alpha.lt.zero) then
    beta = one / alpha
    if (.not. beta.lt.zero) then
      brkdn = .true.
      ipar(1) = -9
    endif
  else if (alpha.eq.zero) then
    brkdn = .true.
    ipar(1) = -3
  else
    brkdn = .true.
    ipar(1) = -9
  endif
  return
end function brkdn
!-----end-of-brkdn
!-----------------------------------------------------------------------
subroutine bisinit(ipar,fpar,wksize,dsc,lp,rp,wk)
  implicit none
  integer i,ipar(16),wksize,dsc
  logical lp,rp
  real*8  fpar(16),wk(*)
  !-----------------------------------------------------------------------
  !     some common initializations for the iterative solvers
  !-----------------------------------------------------------------------
  real*8 zero, one
  parameter(zero=0.0d0, one=1.0d0)
  !
  !     ipar(1) = -2 inidcate that there are not enough space in the work
  !     array
  !
  if (ipar(4).lt.wksize) then
    ipar(1) = -2
    ipar(4) = wksize
    return
  endif
  !
  if (ipar(2).gt.2) then
    lp = .true.
    rp = .true.
  else if (ipar(2).eq.2) then
    lp = .false.
    rp = .true.
  else if (ipar(2).eq.1) then
    lp = .true.
    rp = .false.
  else
    lp = .false.
    rp = .false.
  endif
  if (ipar(3).eq.0) ipar(3) = dsc
  !     .. clear the ipar elements used
  ipar(7) = 0
  ipar(8) = 0
  ipar(9) = 0
  ipar(10) = 0
  ipar(11) = 0
  ipar(12) = 0
  ipar(13) = 0
  !
  !     fpar(1) must be between (0, 1), fpar(2) must be positive,
  !     fpar(1) and fpar(2) can not both be zero
  !     normally return ipar(1) = -4 to indicate any of above error
  !
  if (fpar(1).lt.zero .or. fpar(1).ge.one .or. fpar(2).lt.zero .or. &
       &     (fpar(1).eq.zero .and. fpar(2).eq.zero)) then
    if (ipar(1).eq.0) then
      ipar(1) = -4
      return
    else
      fpar(1) = 1.0d-6
      fpar(2) = 1.0d-16
    endif
  endif
  !     .. clear the fpar elements
  do i = 3, 10
    fpar(i) = zero
  enddo
  if (fpar(11).lt.zero) fpar(11) = zero
  !     .. clear the used portion of the work array to zero
  do i = 1, wksize
    wk(i) = zero
  enddo
  !
  return
  !-----end-of-bisinit
end subroutine bisinit
!-----------------------------------------------------------------------
subroutine mgsro(full,lda,n,m,ind,ops,vec,hh,ierr)
  implicit none
  logical full
  integer lda,m,n,ind,ierr
  real*8  ops,hh(m),vec(lda,m)
  !-----------------------------------------------------------------------
  !     mgsro  -- modified gram-schmidt procedure with selective re-
  !               orthogonalization
  !     the ind'th vector of vec is orthogonalized against the rest of
  !     the vectors.
  !
  !     the test for performing re-orthogonalization is performed for
  !     each indivadual vectors. if the cosine between the two vectors
  !     is greater than 0.99 (reorth = 0.99**2), re-orthogonalization is
  !     performed. the norm of the 'new' vector is kept in variable nrm0,
  !     and updated after operating with each vector.
  !
  !     full   -- .ture. if it is necessary to orthogonalize the ind'th
  !               against all the vectors vec(:,1:ind-1), vec(:,ind+2:m)
  !               .false. only orthogonalize againt vec(:,1:ind-1)
  !     lda    -- the leading dimension of vec
  !     n      -- length of the vector in vec
  !     m      -- number of vectors can be stored in vec
  !     ind    -- index to the vector to be changed
  !     ops    -- operation counts
  !     vec    -- vector of lda x m storing the vectors
  !     hh     -- coefficient of the orthogonalization
  !     ierr   -- error code
  !               0 : successful return
  !               -1: zero input vector
  !               -2: input vector contains abnormal numbers
  !               -3: input vector is a linear combination of others
  !
  !     external routines used: real*8 ddot
  !-----------------------------------------------------------------------
  integer i,k
  real*8  nrm0, nrm1, fct, thr, ddot, zero, one, reorth
  parameter (zero=0.0d0, one=1.0d0, reorth=0.98d0)
  external ddot
  !
  !     compute the norm of the input vector
  !
  nrm0 = ddot(n,vec(1,ind),vec(1,ind))
  ops = ops + n + n
  thr = nrm0 * reorth
  if (nrm0.le.zero) then
    ierr = - 1
    return
  else if (nrm0.gt.zero .and. one/nrm0.gt.zero) then
    ierr = 0
  else
    ierr = -2
    return
  endif
  !
  !     modified gram-schmidt loop
  !
  if (full) then
    do i = ind+1, m
      fct = ddot(n,vec(1,ind),vec(1,i))
      hh(i) = fct
      do k = 1, n
        vec(k,ind) = vec(k,ind) - fct * vec(k,i)
      end do
      ops = ops + 4 * n + 2
      if (fct*fct.gt.thr) then
        fct = ddot(n,vec(1,ind),vec(1,i))
        hh(i) = hh(i) + fct
        do k = 1, n
          vec(k,ind) = vec(k,ind) - fct * vec(k,i)
        end do
        ops = ops + 4*n + 1
      endif
      nrm0 = nrm0 - hh(i) * hh(i)
      if (nrm0.lt.zero) nrm0 = zero
      thr = nrm0 * reorth
    end do
  endif
  !
  do i = 1, ind-1
    fct = ddot(n,vec(1,ind),vec(1,i))
    hh(i) = fct
    do k = 1, n
      vec(k,ind) = vec(k,ind) - fct * vec(k,i)
    end do
    ops = ops + 4 * n + 2
    if (fct*fct.gt.thr) then
      fct = ddot(n,vec(1,ind),vec(1,i))
      hh(i) = hh(i) + fct
      do k = 1, n
        vec(k,ind) = vec(k,ind) - fct * vec(k,i)
      end do
      ops = ops + 4*n + 1
    endif
    nrm0 = nrm0 - hh(i) * hh(i)
    if (nrm0.lt.zero) nrm0 = zero
    thr = nrm0 * reorth
  end do
  !
  !     test the resulting vector
  !
  nrm1 = sqrt(ddot(n,vec(1,ind),vec(1,ind)))
  ops = ops + n + n
  hh(ind) = nrm1    ! statement label 75
  if (nrm1.le.zero) then
    ierr = -3
    return
  endif
  !
  !     scale the resulting vector
  !
  fct = one / nrm1
  do k = 1, n
    vec(k,ind) = vec(k,ind) * fct
  end do
  ops = ops + n + 1
  !
  !     normal return
  !
  ierr = 0
  return
  !     end subroutine mgsro
end subroutine mgsro
!----------------------------------------------------------------------c
!                          s p a r s k i t                             c
!----------------------------------------------------------------------c
!          basic matrix-vector operations - matvec module              c
!         matrix-vector mulitiplications and triang. solves            c
!----------------------------------------------------------------------c
! contents: (as of nov 18, 1991)                                       c
!----------                                                            c
! 1) matrix-vector products:                                           c
!---------------------------                                           c
! amux  : a times a vector. compressed sparse row (csr) format.        c
! amuxms: a times a vector. modified compress sparse row format.       c
! atmux : transp(a) times a vector. csr format.                        c
! atmuxr: transp(a) times a vector. csr format. a rectangular.         c
! amuxe : a times a vector. ellpack/itpack (ell) format.               c
! amuxd : a times a vector. diagonal (dia) format.                     c
! amuxj : a times a vector. jagged diagonal (jad) format.              c
! vbrmv : sparse matrix-full vector product, in vbr format             c
!                                                                      c
! 2) triangular system solutions:                                      c
!-------------------------------                                       c
! lsol  : unit lower triang. solve. compressed sparse row (csr) format.c
! ldsol : lower triang. solve.  modified sparse row (msr) format.      c
! lsolc : unit lower triang. solve. comp. sparse column (csc) format.  c
! ldsolc: lower triang. solve. modified sparse column (msc) format.    c
! ldsoll: lower triang. solve with level scheduling. msr format.       c
! usol  : unit upper triang. solve. compressed sparse row (csr) format.c
! udsol : upper triang. solve.  modified sparse row (msr) format.      c
! usolc : unit upper triang. solve. comp. sparse column (csc) format.  c
! udsolc: upper triang. solve.  modified sparse column (msc) format.   c
!----------------------------------------------------------------------c
! 1)     m a t r i x    b y    v e c t o r     p r o d u c t s         c
!----------------------------------------------------------------------c
subroutine amux (n, x, y, a,ja,ia)
  real*8  x(*), y(*), a(*)
  integer n, ja(*), ia(*)
  !-----------------------------------------------------------------------
  !         a times a vector
  !-----------------------------------------------------------------------
  ! multiplies a matrix by a vector using the dot product form
  ! matrix a is stored in compressed sparse row storage.
  !
  ! on entry:
  !----------
  ! n     = row dimension of a
  ! x     = real array of length equal to the column dimension of
  !         the a matrix.
  ! a, ja,
  !    ia = input matrix in compressed sparse row format.
  !
  ! on return:
  !-----------
  ! y     = real array of length n, containing the product y=ax
  !
  !-----------------------------------------------------------------------
  ! local variables
  !
  real*8 t
  integer i, k
  !-----------------------------------------------------------------------
  do i = 1,n
    !
    !     compute the inner product of row i with vector x
    !
    t = 0.0d0
    do k=ia(i), ia(i+1)-1
      t = t + a(k)*x(ja(k))
    end do
    !
    !     store result in y(i)
    !
    y(i) = t
  end do
  !
  return
  !---------end-of-amux---------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine amux
!-----------------------------------------------------------------------
subroutine amuxms (n, x, y, a,ja)
  real*8  x(*), y(*), a(*)
  integer n, ja(*)
  !-----------------------------------------------------------------------
  !         a times a vector in msr format
  !-----------------------------------------------------------------------
  ! multiplies a matrix by a vector using the dot product form
  ! matrix a is stored in modified sparse row storage.
  !
  ! on entry:
  !----------
  ! n     = row dimension of a
  ! x     = real array of length equal to the column dimension of
  !         the a matrix.
  ! a, ja,= input matrix in modified compressed sparse row format.
  !
  ! on return:
  !-----------
  ! y     = real array of length n, containing the product y=ax
  !
  !-----------------------------------------------------------------------
  ! local variables
  !
  integer i, k
  !-----------------------------------------------------------------------
  do i=1, n
    y(i) = a(i)*x(i)
  end do
  do i = 1,n
    !
    !     compute the inner product of row i with vector x
    !
    do k=ja(i), ja(i+1)-1
      y(i) = y(i) + a(k) *x(ja(k))
    end do
  end do
  !
  return
  !---------end-of-amuxm--------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine amuxms
!-----------------------------------------------------------------------
subroutine atmux (n, x, y, a, ja, ia)
  real*8 x(*), y(*), a(*)
  integer n, ia(*), ja(*)
  !-----------------------------------------------------------------------
  !         transp( a ) times a vector
  !-----------------------------------------------------------------------
  ! multiplies the transpose of a matrix by a vector when the original
  ! matrix is stored in compressed sparse row storage. can also be
  ! viewed as the product of a matrix by a vector when the original
  ! matrix is stored in the compressed sparse column format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n     = row dimension of a
  ! x     = real array of length equal to the column dimension of
  !         the a matrix.
  ! a, ja,
  !    ia = input matrix in compressed sparse row format.
  !
  ! on return:
  !-----------
  ! y     = real array of length n, containing the product y=transp(a)*x
  !
  !-----------------------------------------------------------------------
  !     local variables
  !
  integer i, k
  !-----------------------------------------------------------------------
  !
  !     zero out output vector
  !
  do i=1,n
    y(i) = 0.0
  end do
  !
  ! loop over the rows
  !
  do i = 1,n
    do  k=ia(i), ia(i+1)-1
      y(ja(k)) = y(ja(k)) + x(i)*a(k)
    end do
  end do
  !
  return
  !-------------end-of-atmux----------------------------------------------
  !-----------------------------------------------------------------------
end subroutine atmux
!-----------------------------------------------------------------------
subroutine atmuxr (m, n, x, y, a, ja, ia)
  real*8 x(*), y(*), a(*)
  integer m, n, ia(*), ja(*)
  !-----------------------------------------------------------------------
  !         transp( a ) times a vector, a can be rectangular
  !-----------------------------------------------------------------------
  ! see also atmux.  the essential difference is how the solution vector
  ! is initially zeroed.  if using this to multiply rectangular csc
  ! matrices by a vector, m number of rows, n is number of columns.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! m     = column dimension of a
  ! n     = row dimension of a
  ! x     = real array of length equal to the column dimension of
  !         the a matrix.
  ! a, ja,
  !    ia = input matrix in compressed sparse row format.
  !
  ! on return:
  !-----------
  ! y     = real array of length n, containing the product y=transp(a)*x
  !
  !-----------------------------------------------------------------------
  !     local variables
  !
  integer i, k
  !-----------------------------------------------------------------------
  !
  !     zero out output vector
  !
  do i=1,m
    y(i) = 0.0
  end do
  !
  ! loop over the rows
  !
  do i = 1,n
    do k=ia(i), ia(i+1)-1
      y(ja(k)) = y(ja(k)) + x(i)*a(k)
    end do
  end do
  !
  return
  !-------------end-of-atmuxr---------------------------------------------
  !-----------------------------------------------------------------------
end subroutine atmuxr
!-----------------------------------------------------------------------
subroutine amuxe (n,x,y,na,ncol,a,ja)
  implicit none
  integer     :: n, na, ncol, ja(na,*)
  real*8      :: x(n), y(n), a(na,*)
  !-----------------------------------------------------------------------
  !        a times a vector in ellpack itpack format (ell)
  !-----------------------------------------------------------------------
  ! multiplies a matrix by a vector when the original matrix is stored
  ! in the ellpack-itpack sparse format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n     = row dimension of a
  ! x     = real array of length equal to the column dimension of
  !         the a matrix.
  ! na    = integer. the first dimension of arrays a and ja
  !         as declared by the calling program.
  ! ncol  = integer. the number of active columns in array a.
  !         (i.e., the number of generalized diagonals in matrix.)
  ! a, ja = the real and integer arrays of the itpack format
  !         (a(i,k),k=1,ncol contains the elements of row i in matrix
  !          ja(i,k),k=1,ncol contains their column numbers)
  !
  ! on return:
  !-----------
  ! y     = real array of length n, containing the product y=y=a*x
  !
  !-----------------------------------------------------------------------
  ! local variables
  !
  integer i, j
  !-----------------------------------------------------------------------
  do i=1, n
    y(i) = 0.0
  end do
  do j=1,ncol
    do i = 1,n
      y(i) = y(i)+a(i,j)*x(ja(i,j))
    end do
  end do
  !
  return
  !--------end-of-amuxe---------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine amuxe
!-----------------------------------------------------------------------
subroutine amuxd (n,x,y,diag,ndiag,idiag,ioff)
  integer n, ndiag, idiag, ioff(idiag)
  real*8 x(n), y(n), diag(ndiag,idiag)
  !-----------------------------------------------------------------------
  !        a times a vector in diagonal storage format (dia)
  !-----------------------------------------------------------------------
  ! multiplies a matrix by a vector when the original matrix is stored
  ! in the diagonal storage format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n     = row dimension of a
  ! x     = real array of length equal to the column dimension of
  !         the a matrix.
  ! ndiag  = integer. the first dimension of array adiag as declared in
  !         the calling program.
  ! idiag  = integer. the number of diagonals in the matrix.
  ! diag   = real array containing the diagonals stored of a.
  ! idiag  = number of diagonals in matrix.
  ! diag   = real array of size (ndiag x idiag) containing the diagonals
  !
  ! ioff   = integer array of length idiag, containing the offsets of the
  !          diagonals of the matrix:
  !          diag(i,k) contains the element a(i,i+ioff(k)) of the matrix.
  !
  ! on return:
  !-----------
  ! y     = real array of length n, containing the product y=a*x
  !
  !-----------------------------------------------------------------------
  !       local variables
  !
  integer j, k, io, i1, i2
  !-----------------------------------------------------------------------
  do j=1, n
    y(j) = 0.0d0
  end do
  do j=1, idiag
    io = ioff(j)
    i1 = max0(1,1-io)
    i2 = min0(n,n-io)
    do k=i1, i2
      y(k) = y(k)+diag(k,j)*x(k+io)
    end do
  end do
  !
  return
  !----------end-of-amuxd-------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine amuxd
!-----------------------------------------------------------------------
subroutine amuxj (n, x, y, jdiag, a, ja, ia)
  integer n, jdiag, ja(*), ia(*)
  real*8 x(n), y(n), a(*)
  !-----------------------------------------------------------------------
  !        a times a vector in jagged-diagonal storage format (jad)
  !-----------------------------------------------------------------------
  ! multiplies a matrix by a vector when the original matrix is stored
  ! in the jagged diagonal storage format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = row dimension of a
  ! x      = real array of length equal to the column dimension of
  !         the a matrix.
  ! jdiag  = integer. the number of jadded-diagonals in the data-structure.
  ! a      = real array containing the jadded diagonals of a stored
  !          in succession (in decreasing lengths)
  ! j      = integer array containing the colum indices of the
  !          corresponding elements in a.
  ! ia     = integer array containing the lengths of the  jagged diagonals
  !
  ! on return:
  !-----------
  ! y      = real array of length n, containing the product y=a*x
  !
  ! note:
  !-------
  ! permutation related to the jad format is not performed.
  ! this can be done by:
  !     call permvec (n,y,y,iperm)
  ! after the call to amuxj, where iperm is the permutation produced
  ! by csrjad.
  !-----------------------------------------------------------------------
  ! local variables
  !
  integer i, ii, k1, ilen, j
  !-----------------------------------------------------------------------
  do i=1, n
    y(i) = 0.0d0
  end do
  do  ii=1, jdiag
    k1 = ia(ii)-1
    ilen = ia(ii+1)-k1-1
    do  j=1,ilen
      y(j)= y(j)+a(k1+j)*x(ja(k1+j))
    end do
  end do
  !
  return
  !----------end-of-amuxj-------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine amuxj
!-----------------------------------------------------------------------
subroutine vbrmv(nr, nc, ia, ja, ka, a, kvstr, kvstc, x, b)
  !-----------------------------------------------------------------------
  integer nr, nc, ia(nr+1), ja(*), ka(*), kvstr(nr+1), kvstc(*)
  real*8  a(*), x(*), b(*)
  !-----------------------------------------------------------------------
  !     sparse matrix-full vector product, in vbr format.
  !-----------------------------------------------------------------------
  !     on entry:
  !--------------
  !     nr, nc  = number of block rows and columns in matrix a
  !     ia,ja,ka,a,kvstr,kvstc = matrix a in variable block row format
  !     x       = multiplier vector in full format
  !
  !     on return:
  !---------------
  !     b = product of matrix a times vector x in full format
  !
  !     algorithm:
  !---------------
  !     perform multiplication by traversing a in order.
  !
  !-----------------------------------------------------------------------
  !-----local variables
  integer n, i, j, ii, jj, k, istart, istop
  real*8  xjj
  !---------------------------------
  n = kvstc(nc+1)-1
  do i = 1, n
    b(i) = 0.d0
  enddo
  !---------------------------------
  k = 1
  do i = 1, nr
    istart = kvstr(i)
    istop  = kvstr(i+1)-1
    do j = ia(i), ia(i+1)-1
      do jj = kvstc(ja(j)), kvstc(ja(j)+1)-1
        xjj = x(jj)
        do ii = istart, istop
          b(ii) = b(ii) + xjj*a(k)
          k = k + 1
        enddo
      enddo
    enddo
  enddo
  !---------------------------------
  return
end subroutine vbrmv
!-----------------------------------------------------------------------
!----------------------end-of-vbrmv-------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------c
! 2)     t r i a n g u l a r    s y s t e m    s o l u t i o n s       c
!----------------------------------------------------------------------c
subroutine lsol (n,x,y,al,jal,ial)
  integer n, jal(*),ial(n+1)
  real*8  x(n), y(n), al(*)
  !-----------------------------------------------------------------------
  !   solves    l x = y ; l = lower unit triang. /  csr format
  !-----------------------------------------------------------------------
  ! solves a unit lower triangular system by standard (sequential )
  ! forward elimination - matrix stored in csr format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real array containg the right side.
  !
  ! al,
  ! jal,
  ! ial,    = lower triangular matrix stored in compressed sparse row
  !          format.
  !
  ! on return:
  !-----------
  !          x  = the solution of  l x  = y.
  !--------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8  t
  !-----------------------------------------------------------------------
  x(1) = y(1)
  do  k = 2, n
    t = y(k)
    do j = ial(k), ial(k+1)-1
      t = t-al(j)*x(jal(j))
    end do
    x(k) = t
  end do
  !
  return
  !----------end-of-lsol--------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine lsol
!-----------------------------------------------------------------------
subroutine ldsol (n,x,y,al,jal)
  integer n, jal(*)
  real*8 x(n), y(n), al(*)
  !-----------------------------------------------------------------------
  !     solves l x = y    l = triangular. msr format
  !-----------------------------------------------------------------------
  ! solves a (non-unit) lower triangular system by standard (sequential)
  ! forward elimination - matrix stored in msr format
  ! with diagonal elements already inverted (otherwise do inversion,
  ! al(1:n) = 1.0/al(1:n),  before calling ldsol).
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real array containg the right hand side.
  !
  ! al,
  ! jal,   = lower triangular matrix stored in modified sparse row
  !          format.
  !
  ! on return:
  !-----------
  !        x = the solution of  l x = y .
  !--------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8 t
  !-----------------------------------------------------------------------
  x(1) = y(1)*al(1)
  do  k = 2, n
    t = y(k)
    do  j = jal(k), jal(k+1)-1
      t = t - al(j)*x(jal(j))
    end do
    x(k) = al(k)*t
  end do
  return
  !----------end-of-ldsol-------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine ldsol
!-----------------------------------------------------------------------
subroutine lsolc (n,x,y,al,jal,ial)
  integer n, jal(*),ial(*)
  real*8  x(n), y(n), al(*)
  !-----------------------------------------------------------------------
  !       solves     l x = y ;    where l = unit lower trang. csc format
  !-----------------------------------------------------------------------
  ! solves a unit lower triangular system by standard (sequential )
  ! forward elimination - matrix stored in csc format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real*8 array containg the right side.
  !
  ! al,
  ! jal,
  ! ial,    = lower triangular matrix stored in compressed sparse column
  !          format.
  !
  ! on return:
  !-----------
  !         x  = the solution of  l x  = y.
  !-----------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8 t
  !-----------------------------------------------------------------------
  do k=1,n
    x(k) = y(k)
  end do
  do k = 1, n-1
    t = x(k)
    do j = ial(k), ial(k+1)-1
      x(jal(j)) = x(jal(j)) - t*al(j)
    end do
  end do
  !
  return
  !----------end-of-lsolc-------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine lsolc
!-----------------------------------------------------------------------
subroutine ldsolc (n,x,y,al,jal)
  integer n, jal(*)
  real*8 x(n), y(n), al(*)
  !-----------------------------------------------------------------------
  !    solves     l x = y ;    l = nonunit low. triang. msc format
  !-----------------------------------------------------------------------
  ! solves a (non-unit) lower triangular system by standard (sequential)
  ! forward elimination - matrix stored in modified sparse column format
  ! with diagonal elements already inverted (otherwise do inversion,
  ! al(1:n) = 1.0/al(1:n),  before calling ldsol).
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real array containg the right hand side.
  !
  ! al,
  ! jal,
  ! ial,    = lower triangular matrix stored in modified sparse column
  !           format.
  !
  ! on return:
  !-----------
  !         x = the solution of  l x = y .
  !--------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8 t
  !-----------------------------------------------------------------------
  do k=1,n
    x(k) = y(k)
  end do
  do k = 1, n
    x(k) = x(k)*al(k)
    t = x(k)
    do j = jal(k), jal(k+1)-1
      x(jal(j)) = x(jal(j)) - t*al(j)
    end do
  end do
  !
  return
  !----------end-of-lsolc------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine ldsolc
!-----------------------------------------------------------------------
subroutine ldsoll (n,x,y,al,jal,nlev,lev,ilev)
  integer n, nlev, jal(*), ilev(nlev+1), lev(n)
  real*8 x(n), y(n), al(*)
  !-----------------------------------------------------------------------
  !    solves l x = y    l = triangular. uses level scheduling/msr format
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real array containg the right hand side.
  !
  ! al,
  ! jal,   = lower triangular matrix stored in modified sparse row
  !          format.
  ! nlev   = number of levels in matrix
  ! lev    = integer array of length n, containing the permutation
  !          that defines the levels in the level scheduling ordering.
  ! ilev   = pointer to beginning of levels in lev.
  !          the numbers lev(i) to lev(i+1)-1 contain the row numbers
  !          that belong to level number i, in the level shcheduling
  !          ordering.
  !
  ! on return:
  !-----------
  !        x = the solution of  l x = y .
  !--------------------------------------------------------------------
  integer ii, jrow, i, k
  real*8 t
  !
  !     outer loop goes through the levels. (sequential loop)
  !
  do ii=1, nlev
    !
    !     next loop executes within the same level. parallel loop
    !
    do i=ilev(ii), ilev(ii+1)-1
      jrow = lev(i)
      !
      ! compute inner product of row jrow with x
      !
      t = y(jrow)
      do k=jal(jrow), jal(jrow+1)-1
        t = t - al(k)*x(jal(k))
      end do
      x(jrow) = t*al(jrow)
    end do
  end do
  return
  !-----------------------------------------------------------------------
end subroutine ldsoll
!-----------------------------------------------------------------------
subroutine usol (n,x,y,au,jau,iau)
  integer n, jau(*),iau(n+1)
  real*8  x(n), y(n), au(*)
  !-----------------------------------------------------------------------
  !             solves   u x = y    u = unit upper triangular.
  !-----------------------------------------------------------------------
  ! solves a unit upper triangular system by standard (sequential )
  ! backward elimination - matrix stored in csr format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real array containg the right side.
  !
  ! au,
  ! jau,
  ! iau,    = lower triangular matrix stored in compressed sparse row
  !          format.
  !
  ! on return:
  !-----------
  !         x = the solution of  u x = y .
  !--------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8  t
  !-----------------------------------------------------------------------
  x(n) = y(n)
  do  k = n-1,1,-1
    t = y(k)
    do j = iau(k), iau(k+1)-1
      t = t - au(j)*x(jau(j))
    end do
    x(k) = t
  end do
  !
  return
  !----------end-of-usol--------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine usol
!-----------------------------------------------------------------------
subroutine udsol (n,x,y,au,jau)
  integer n, jau(*)
  real*8  x(n), y(n),au(*)
  !-----------------------------------------------------------------------
  !             solves   u x = y  ;   u = upper triangular in msr format
  !-----------------------------------------------------------------------
  ! solves a non-unit upper triangular matrix by standard (sequential )
  ! backward elimination - matrix stored in msr format.
  ! with diagonal elements already inverted (otherwise do inversion,
  ! au(1:n) = 1.0/au(1:n),  before calling).
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real array containg the right side.
  !
  ! au,
  ! jau,    = lower triangular matrix stored in modified sparse row
  !          format.
  !
  ! on return:
  !-----------
  !         x = the solution of  u x = y .
  !--------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8 t
  !-----------------------------------------------------------------------
  x(n) = y(n)*au(n)
  do k = n-1,1,-1
    t = y(k)
    do j = jau(k), jau(k+1)-1
      t = t - au(j)*x(jau(j))
    end do
    x(k) = au(k)*t
  end do
  !
  return
  !----------end-of-udsol-------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine udsol
!-----------------------------------------------------------------------
subroutine usolc (n,x,y,au,jau,iau)
  real*8  x(*), y(*), au(*)
  integer n, jau(*),iau(*)
  !-----------------------------------------------------------------------
  !       souves     u x = y ;    where u = unit upper trang. csc format
  !-----------------------------------------------------------------------
  ! solves a unit upper triangular system by standard (sequential )
  ! forward elimination - matrix stored in csc format.
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real*8 array containg the right side.
  !
  ! au,
  ! jau,
  ! iau,    = uower triangular matrix stored in compressed sparse column
  !          format.
  !
  ! on return:
  !-----------
  !         x  = the solution of  u x  = y.
  !-----------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8 t
  !-----------------------------------------------------------------------
  do k=1,n
    x(k) = y(k)
  end do
  do  k = n,1,-1
    t = x(k)
    do j = iau(k), iau(k+1)-1
      x(jau(j)) = x(jau(j)) - t*au(j)
    end do
  end do
  !
  return
  !----------end-of-usolc-------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine usolc
!-----------------------------------------------------------------------
subroutine udsolc (n,x,y,au,jau)
  integer n, jau(*)
  real*8 x(n), y(n), au(*)
  !-----------------------------------------------------------------------
  !    solves     u x = y ;    u = nonunit up. triang. msc format
  !-----------------------------------------------------------------------
  ! solves a (non-unit) upper triangular system by standard (sequential)
  ! forward elimination - matrix stored in modified sparse column format
  ! with diagonal elements already inverted (otherwise do inversion,
  ! auuuul(1:n) = 1.0/au(1:n),  before calling ldsol).
  !-----------------------------------------------------------------------
  !
  ! on entry:
  !----------
  ! n      = integer. dimension of problem.
  ! y      = real*8 array containg the right hand side.
  !
  ! au,
  ! jau,   = upper triangular matrix stored in modified sparse column
  !          format.
  !
  ! on return:
  !-----------
  !         x = the solution of  u x = y .
  !--------------------------------------------------------------------
  ! local variables
  !
  integer k, j
  real*8 t
  !-----------------------------------------------------------------------
  do k=1,n
    x(k) = y(k)
  end do
  do  k = n,1,-1
    x(k) = x(k)*au(k)
    t = x(k)
    do j = jau(k), jau(k+1)-1
      x(jau(j)) = x(jau(j)) - t*au(j)
    end do
  end do
  !
  return
  !----------end-of-udsolc------------------------------------------------
  !-----------------------------------------------------------------------
end subroutine udsolc
!-----------------------------------------------------------------------
subroutine lusol(n, y, x, alu, jlu, ju)
  implicit none
  integer    :: n, jlu(*), ju(*)
  real*8     :: x(n), y(n), alu(*)
  !-----------------------------------------------------------------------
  integer    :: i,k
  !
  ! forward solve
  !
  do i = 1, n
    x(i) = y(i)
    do k=jlu(i),ju(i)-1
      x(i) = x(i) - alu(k)* x(jlu(k))
    end do
  end do
  do i = n, 1, -1
    do k=ju(i),jlu(i+1)-1
      x(i) = x(i) - alu(k)*x(jlu(k))
    end do
    x(i) = alu(i)*x(i)
  end do
  !
  return
  !----------------end of lusol ------------------------------------------
end subroutine lusol
!-----------------------------------------------------------------------
subroutine lutsol(n, y, x, alu, jlu, ju)
  implicit none
  integer     :: n, jlu(*), ju(*)
  real*8      :: x(n), y(n), alu(*)
  !-----------------------------------------------------------------------
  ! local variables
  !
  integer      :: i,k
  !
  do i = 1, n
    x(i) = y(i)
  end do
  !
  ! forward solve (with u^t)
  !
  do i = 1, n
    x(i) = x(i) * alu(i)
    do k=ju(i),jlu(i+1)-1
      x(jlu(k)) = x(jlu(k)) - alu(k)* x(i)
    end do
  end do
  !
  !     backward solve (with l^t)
  !
  do i = n, 1, -1
    do k=jlu(i),ju(i)-1
      x(jlu(k)) = x(jlu(k)) - alu(k)*x(i)
    end do
  end do
  !
  return
  !----------------end of lutsol -----------------------------------------
  !-----------------------------------------------------------------------
end subroutine lutsol
!-----------------------------------------------------------------------
subroutine qsplit(a,ind,n,ncut)
  implicit none
  integer    :: n, ind(n), ncut
  real*8     :: a(n)
  !-----------------------------------------------------------------------
  !     does a quick-sort split of a real array.
  !     on input a(1:n). is a real array
  !     on output a(1:n) is permuted such that its elements satisfy:
  !
  !     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
  !     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
  !
  !     ind(1:n) is an integer array which permuted in the same way as a(*).
  !-----------------------------------------------------------------------
  real*8     :: tmp, abskey
  integer    :: itmp, first, last, j, mid
  !-----
  first = 1
  last = n
  if (ncut .lt. first .or. ncut .gt. last) return
  !
  !     outer loop -- while mid .ne. ncut do
  !
1 mid = first
  abskey = abs(a(mid))
  do j=first+1, last
    if (abs(a(j)) .gt. abskey) then
      mid = mid+1
      !     interchange
      tmp = a(mid)
      itmp = ind(mid)
      a(mid) = a(j)
      ind(mid) = ind(j)
      a(j)  = tmp
      ind(j) = itmp
    endif
  end do
  !
  !     interchange
  !
  tmp = a(mid)
  a(mid) = a(first)
  a(first)  = tmp
  !
  itmp = ind(mid)
  ind(mid) = ind(first)
  ind(first) = itmp
  !
  !     test for while loop
  !
  if (mid .eq. ncut) return
  if (mid .gt. ncut) then
    last = mid-1
  else
    first = mid+1
  endif
  goto 1
  !----------------end-of-qsplit------------------------------------------
  !-----------------------------------------------------------------------
end subroutine qsplit
subroutine runrc(n,rhs,sol,ipar,fpar,wk,guess,a,ja,ia,au,jau,ju,solver)
  implicit none
  integer n,ipar(16),ia(n+1),ja(*),ju(*),jau(*)
  real*8 fpar(16),rhs(n),sol(n),guess(n),wk(*),a(*),au(*)
  external solver
  !-----------------------------------------------------------------------
  !     the actual tester. it starts the iterative linear system solvers
  !     with a initial guess suppied by the user.
  !
  !     the structure {au, jau, ju} is assumed to have the output from
  !     the ilu* routines in ilut.f.
  !
  !-----------------------------------------------------------------------
  !     local variables
  !
  integer       :: i, its
  !      real          :: dtime, dt(2), time
  !     external dtime
  save its
  !
  !     ipar(2) can be 0, 1, 2, please don't use 3
  !
  if (ipar(2).gt.2) then
    write(*,*) 'i can not do both left and right preconditioning.'
    return
  endif
  its = 0
  !
  do i = 1, n
    sol(i) = guess(i)
  enddo
  !
  ipar(1) = 0
  !     time = dtime(dt)
10 call solver(n,rhs,sol,ipar,fpar,wk)
  if (ipar(7).ne.its) then
    its = ipar(7)
  endif
  if (ipar(1).eq.1) then
    call amux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
    goto 10
  else if (ipar(1).eq.2) then
    call atmux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
    goto 10
  else if (ipar(1).eq.3 .or. ipar(1).eq.5) then
    call lusol(n,wk(ipar(8)),wk(ipar(9)),au,jau,ju)
    goto 10
  else if (ipar(1).eq.4 .or. ipar(1).eq.6) then
    call lutsol(n,wk(ipar(8)),wk(ipar(9)),au,jau,ju)
    goto 10
  else if (ipar(1).le.0) then
    if (ipar(1).eq.0) then
      !            write(*,*) 'iterative sovler has satisfied convergence test.'
    else if (ipar(1).eq.-1) then
      write(*,*) 'iterative solver has iterated too many times.'
    else if (ipar(1).eq.-2) then
      write(*,*) 'iterative solver was not given enough work space.'
      write(*,*) 'the work space should at least have ', ipar(4), &
           &           ' elements.'
    else if (ipar(1).eq.-3) then
      write(*,*) 'iterative sovler is facing a break-down.'
    else
      write(*,*) 'iterative solver terminated. code =', ipar(1)
    endif
  endif
end subroutine runrc
!-----end-of-runrc
!----------------------------------------------------------------------c
!                          s p a r s k i t                             c
!----------------------------------------------------------------------c
!                   iterative solvers module                           c
!----------------------------------------------------------------------c
! this version dated: august 13, 1996. warning: meaning of some        c
! ============ arguments have changed w.r.t. earlier versions. some    c
!              calling sequences may also have changed                 c
!
subroutine ilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,iwk,w,jw,ierr)
  !-----------------------------------------------------------------------
  implicit none
  integer n
  real*8 a(*),alu(*),w(n+1),droptol
  integer ja(*),ia(n+1),jlu(*),ju(n),jw(2*n),lfil,iwk,ierr
  !----------------------------------------------------------------------*
  !                      *** ilut preconditioner ***                     *
  !      incomplete lu factorization with dual truncation mechanism      *
  !----------------------------------------------------------------------*
  !     author: yousef saad *may, 5, 1990, latest revision, august 1996  *
  !----------------------------------------------------------------------*
  ! parameters
  !-----------
  !
  ! on entry:
  !==========
  ! n       = integer. the row dimension of the matrix a. the matrix
  !
  ! a,ja,ia = matrix stored in compressed sparse row format.
  !
  ! lfil    = integer. the fill-in parameter. each row of l and each row
  !           of u will have a maximum of lfil elements (excluding the
  !           diagonal element). lfil must be .ge. 0.
  !           ** warning: the meaning of lfil has changed with respect to
  !           earlier versions.
  !
  ! droptol = real*8. sets the threshold for dropping small terms in the
  !           factorization. see below for details on dropping strategy.
  !
  !
  ! iwk     = integer. the lengths of arrays alu and jlu. if the arrays
  !           are not big enough to store the ilu factorizations, ilut
  !           will stop with an error message.
  !
  ! on return:
  !===========
  !
  ! alu,jlu = matrix stored in modified sparse row (msr) format containing
  !           the l and u factors together. the diagonal (stored in
  !           alu(1:n) ) is inverted. each i-th row of the alu,jlu matrix
  !           contains the i-th row of l (excluding the diagonal entry=1)
  !           followed by the i-th row of u.
  !
  ! ju      = integer array of length n containing the pointers to
  !           the beginning of each row of u in the matrix alu,jlu.
  !
  ! ierr    = integer. error message with the following meaning.
  !           ierr  = 0    --> successful return.
  !           ierr .gt. 0  --> zero pivot encountered at step number ierr.
  !           ierr  = -1   --> error. input matrix may be wrong.
  !                            (the elimination process has generated a
  !                            row in l or u whose length is .gt.  n.)
  !           ierr  = -2   --> the matrix l overflows the array al.
  !           ierr  = -3   --> the matrix u overflows the array alu.
  !           ierr  = -4   --> illegal value for lfil.
  !           ierr  = -5   --> zero row encountered.
  !
  ! work arrays:
  !=============
  ! jw      = integer work array of length 2*n.
  ! w       = real work array of length n+1.
  !
  !----------------------------------------------------------------------
  ! w, ju (1:n) store the working array [1:ii-1 = l-part, ii:n = u]
  ! jw(n+1:2n)  stores nonzero indicators
  !
  ! notes:
  ! ------
  ! the diagonal elements of the input matrix must be  nonzero (at least
  ! 'structurally').
  !
  !----------------------------------------------------------------------*
  !---- dual drop strategy works as follows.                             *
  !                                                                      *
  !     1) theresholding in l and u as set by droptol. any element whose *
  !        magnitude is less than some tolerance (relative to the abs    *
  !        value of diagonal element in u) is dropped.                   *
  !                                                                      *
  !     2) keeping only the largest lfil elements in the i-th row of l   *
  !        and the largest lfil elements in the i-th row of u (excluding *
  !        diagonal elements).                                           *
  !                                                                      *
  ! flexibility: one  can use  droptol=0  to get  a strategy  based on   *
  ! keeping  the largest  elements in  each row  of l  and u.   taking   *
  ! droptol .ne.  0 but lfil=n will give  the usual threshold strategy   *
  ! (however, fill-in is then mpredictible).                             *
  !----------------------------------------------------------------------*
  !     locals
  integer ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,lenn
  real*8 tnorm, t, abs, s, fact
  if (lfil .lt. 0) goto 998
  !-----------------------------------------------------------------------
  !     initialize ju0 (points to next element to be added to alu,jlu)
  !     and pointer array.
  !-----------------------------------------------------------------------
  ju0 = n+2
  jlu(1) = ju0
  !
  !     initialize nonzero indicator array.
  !
  do  j=1,n
    jw(n+j)  = 0
  end do
  !-----------------------------------------------------------------------
  !     beginning of main loop.
  !-----------------------------------------------------------------------
  do ii = 1, n
    j1 = ia(ii)
    j2 = ia(ii+1) - 1
    tnorm = 0.0d0
    do k=j1,j2
      tnorm = tnorm+abs(a(k))
    end do
    if (abs(tnorm) .lt. tiny(1.)) goto 999
    tnorm = tnorm/real(j2-j1+1)
    !
    !     unpack l-part and u-part of row of a in arrays w
    !
    lenu = 1
    lenl = 0
    jw(ii) = ii
    w(ii) = 0.0
    jw(n+ii) = ii
    !
    do j = j1, j2
      k = ja(j)
      t = a(j)
      if (k .lt. ii) then
        lenl = lenl+1
        jw(lenl) = k
        w(lenl) = t
        jw(n+k) = lenl
      else if (k .eq. ii) then
        w(ii) = t
      else
        lenu = lenu+1
        jpos = ii+lenu-1
        jw(jpos) = k
        w(jpos) = t
        jw(n+k) = jpos
      endif
    end do
    jj = 0
    lenn = 0
    !
    !     eliminate previous rows
    !
150 jj = jj+1
    if (jj .gt. lenl) goto 160
    !-----------------------------------------------------------------------
    !     in order to do the elimination in the correct order we must select
    !     the smallest column index among jw(k), k=jj+1, ..., lenl.
    !-----------------------------------------------------------------------
    jrow = jw(jj)
    k = jj
    !
    !     determine smallest column index
    !
    do j=jj+1,lenl
      if (jw(j) .lt. jrow) then
        jrow = jw(j)
        k = j
      endif
    end do
    !
    if (k .ne. jj) then
      !     exchange in jw
      j = jw(jj)
      jw(jj) = jw(k)
      jw(k) = j
      !     exchange in jr
      jw(n+jrow) = jj
      jw(n+j) = k
      !     exchange in w
      s = w(jj)
      w(jj) = w(k)
      w(k) = s
    endif
    !
    !     zero out element in row by setting jw(n+jrow) to zero.
    !
    jw(n+jrow) = 0
    !
    !     get the multiplier for row to be eliminated (jrow).
    !
    fact = w(jj)*alu(jrow)
    if (abs(fact) .le. droptol) goto 150
    !
    !     combine current row and row jrow
    !
    do  k = ju(jrow), jlu(jrow+1)-1
      s = fact*alu(k)
      j = jlu(k)
      jpos = jw(n+j)
      if (j .ge. ii) then
        !
        !     dealing with upper part.
        !
        if (jpos .eq. 0) then
          !
          !     this is a fill-in element
          !
          lenu = lenu+1
          if (lenu .gt. n) goto 995
          i = ii+lenu-1
          jw(i) = j
          jw(n+j) = i
          w(i) = - s
        else
          !
          !     this is not a fill-in element
          !
          w(jpos) = w(jpos) - s
        endif
      else
        !
        !     dealing  with lower part.
        !
        if (jpos .eq. 0) then
          !
          !     this is a fill-in element
          !
          lenl = lenl+1
          if (lenl .gt. n) goto 995
          jw(lenl) = j
          jw(n+j) = lenl
          w(lenl) = - s
        else
          !
          !     this is not a fill-in element
          !
          w(jpos) = w(jpos) - s
        endif
      endif
    end do
    !
    !     store this pivot element -- (from left to right -- no danger of
    !     overlap with the working elements in l (pivots).
    !
    lenn = lenn+1
    w(lenn) = fact
    jw(lenn)  = jrow
    goto 150
160 continue
    !
    !     reset double-pointer to zero (u-part)
    !
    do k=1, lenu
      jw(n+jw(ii+k-1)) = 0
    end do
    !
    !     update l-matrix
    !
    lenl = lenn
    lenn = min0(lenl,lfil)
    !
    !     sort by quick-split
    !
    call qsplit (w,jw,lenl,lenn)
    !
    !     store l-part
    !
    do k=1, lenn
      if (ju0 .gt. iwk) goto 996
      alu(ju0) =  w(k)
      jlu(ju0) =  jw(k)
      ju0 = ju0+1
    end do
    !
    !     save pointer to beginning of row ii of u
    !
    ju(ii) = ju0
    !
    !     update u-matrix -- first apply dropping strategy
    !
    lenn = 0
    do k=1, lenu-1
      if (abs(w(ii+k)) .gt. droptol*tnorm) then
        lenn = lenn+1
        w(ii+lenn) = w(ii+k)
        jw(ii+lenn) = jw(ii+k)
      endif
    enddo
    lenu = lenn+1
    lenn = min0(lenu,lfil)
    !
    call qsplit (w(ii+1), jw(ii+1), lenu-1,lenn)
    !
    !     copy
    !
    t = abs(w(ii))
    if (lenn + ju0 .gt. iwk) goto 997
    do k=ii+1,ii+lenn-1
      jlu(ju0) = jw(k)
      alu(ju0) = w(k)
      t = t + abs(w(k) )
      ju0 = ju0+1
    end do
    !
    !     store inverse of diagonal element of u
    !
    !2do check if it works ... after correction ...
    if (abs(w(ii)) .lt.  tiny(1.d0)) w(ii) = (0.0001d0 + droptol)*tnorm
    !
    alu(ii) = 1.0d0/ w(ii)
    !
    !     update pointer to beginning of next row of u.
    !
    jlu(ii+1) = ju0
    !-----------------------------------------------------------------------
    !     end main loop
    !-----------------------------------------------------------------------
  end do
  ierr = 0
  return
  !
  !     incomprehensible error. matrix must be wrong.
  !
995 ierr = -1
  return
  !
  !     insufficient storage in l.
  !
996 ierr = -2
  return
  !
  !     insufficient storage in u.
  !
997 ierr = -3
  return
  !
  !     illegal lfil entered.
  !
998 ierr = -4
  return
  !
  !     zero row encountered
  !
999 ierr = -5
  return
  !----------------end-of-ilut--------------------------------------------
  !-----------------------------------------------------------------------
end subroutine ilut
!----------------------------------------------------------------------
!       subroutine ilu0(n, a, ja, ia, alu, jlu, ju, iw, ipoint1, ipoint2, ierr)
subroutine ilu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
  !implicit real*8 (a-h,o-z)
  real*8 a(*), alu(*), tl
  integer n, ju0, ii, jj, i, j, jcol, js, jf, jm, jrow, jw, ierr
  integer ja(*), ia(*), ju(*), jlu(*), iw(n)
  !
  !-----------------------------------------------------------------------
  ju0 = n+2
  jlu(1) = ju0 !!!
  iw = 0
  do ii = 1, n
    js = ju0
    do j=ia(ii),ia(ii+1)-1
      jcol = ja(j)
      if (jcol .eq. ii) then
        alu(ii) = a(j)
        iw(jcol) = ii
        ju(ii)  = ju0 !!!
      else
        alu(ju0) = a(j)
        jlu(ju0) = ja(j)
        iw(jcol) = ju0
        ju0 = ju0+1
      endif
    end do
    jlu(ii+1) = ju0 !!!
    jf = ju0-1
    jm = ju(ii)-1
    !     exit if diagonal element is reached.
    do j=js, jm
      jrow = jlu(j)
      tl = alu(j)*alu(jrow)
      alu(j) = tl
      !     perform  linear combination
      do jj = ju(jrow), jlu(jrow+1)-1
        jw = iw(jlu(jj))
        if (jw .ne. 0) then
          alu(jw) = alu(jw) - tl*alu(jj)
          !                   write(*,*) ii, jw, jj
        end if
      end do
    end do
    !     invert  and store diagonal element.
    if (abs(alu(ii)) .lt. tiny(1.)) goto 600
    alu(ii) = 1.0d0/alu(ii)
    !     reset pointer iw to zero
    iw(ii) = 0
    do i = js, jf
      iw(jlu(i)) = 0
    end do
  end do
  ierr = 0
  return
  !     zero pivot :
600 ierr = ii
  return
end subroutine ilu0
!-----------------------------------------------------------------------
!       subroutine pgmres(n, im, rhs, sol, eps, maxits, ierr)
!        subroutine pgmres(n, im, rhs, sol, eps, maxits, aspar, ierr)
subroutine pgmres(n, im, rhs, sol, eps, maxits, aspar, nnz, ia, ja, alu, jlu, ju, vv, ierr)
  !-----------------------------------------------------------------------
  !       use datapool, only : nnz, ia, ja, alu, jlu, ju, vv, aspar!, rhs, sol
  implicit none
  integer :: n, im, maxits, ierr, nnz
  integer :: ja(nnz), ia(n+1)
  integer :: jlu(nnz+1), ju(n)
  real*8  :: vv(n,im+1), alu(nnz+1)
  real*8  :: aspar(nnz)
  real*8  :: rhs(*), sol(*)
  real*8  :: eps
  real*8  :: eps1, epsmac, gam, t, ddot, dnrm2, ro, tl
  integer :: i,i1,j,jj,k,k1,iii,ii,ju0
  integer :: its,jrow,jcol,jf,jm,js,jw
  real*8  :: hh(im+1,im), c(im), s(im), rs(im+1)
  real*8  :: iw(n)
  logical :: lblas = .false.      ! use sparskit matvec and external blas libs (true), don't use them (false)
  logical :: lilu  = .true.      ! use simple ilu preconditioner
  data epsmac/1.d-16/
  ! ilu0 preconditioner
  if (lilu) then
    ju0 = n+2
    jlu(1) = ju0 !!!
    iw = 0
    do ii = 1, n
      js = ju0
      do j=ia(ii),ia(ii+1)-1
        jcol = ja(j)
        if (jcol .eq. ii) then
          alu(ii) = aspar(j)
          iw(jcol) = ii
          ju(ii)  = ju0 !!!
        else
          alu(ju0) = aspar(j)
          jlu(ju0) = ja(j)
          iw(jcol) = ju0
          ju0 = ju0+1
        endif
      end do
      jlu(ii+1) = ju0 !!!
      jf = ju0-1
      jm = ju(ii)-1
      ! exit if diagonal element is reached.
      do j=js, jm
        jrow = jlu(j)
        tl = alu(j)*alu(jrow)
        alu(j) = tl
        ! perform  linear combination
        do jj = ju(jrow), jlu(jrow+1)-1
          jw = int(iw(jlu(jj)))
          if (jw .ne. 0) then
            alu(jw) = alu(jw) - tl*alu(jj)
            !                    write(*,*) ii, jw, jj
          end if
        end do
      end do
      !     invert  and store diagonal element.
      if (abs(alu(ii)) .lt. epsmac) then
        write (*,*) 'zero pivot'
        stop
      end if
      alu(ii) = 1.0d0/alu(ii)
      !     reset pointer iw to zero
      iw(ii) = 0
      do i = js, jf
        iw(jlu(i)) = 0
      end do
    end do
    ! end preconditioner
  end if
  !-------------------------------------------------------------
  its = 0
  ! outer loop starts here..
  if (lblas) then
    call amux (n, sol, vv, aspar, ja, ia)
  else
    do iii = 1, n
      t = 0.0d0
      do k = ia(iii), ia(iii+1)-1
        t = t + aspar(k) * sol(ja(k))
      end do
      vv(iii,1) = t
    end do
  end if
  do j=1,n
    vv(j,1) = rhs(j) - vv(j,1)
  end do
20 if (lblas) then
    ro = dnrm2(n, vv)
  else
    ro = sqrt(sum(vv(:,1)*vv(:,1)))
  end if
  if (abs(ro) .lt. epsmac) goto 999
  t = 1.0d0 / ro
  do j=1, n
    vv(j,1) = vv(j,1)*t
  end do
  if (its .eq. 0) eps1=eps*ro
  !      initialize 1-st term  of rhs of hessenberg system..
  rs(1) = ro
  i = 0
4 i=i+1
  its = its + 1
  i1 = i + 1
  if (lblas) then
    call lusol (n, vv(1,i), rhs, alu, jlu, ju)
    call amux (n, rhs, vv(1,i1), aspar, ja, ia)
  else
    do iii = 1, n !- lusol
      rhs(iii) = vv(iii,i)
      do k=jlu(iii),ju(iii)-1
        rhs(iii) = rhs(iii) - alu(k)* rhs(jlu(k))
      end do
    end do
    do iii = n, 1, -1
      do k=ju(iii),jlu(iii+1)-1
        rhs(iii) = rhs(iii) - alu(k)*rhs(jlu(k))
      end do
      rhs(iii) = alu(iii)*rhs(iii)
    end do
    do iii = 1, n !- amux
      t = 0.0d0
      do k = ia(iii), ia(iii+1)-1
        t = t + aspar(k) * rhs(ja(k))
      end do
      vv(iii,i1) = t
    end do
  end if
  !      modified gram - schmidt...
  if (lblas) then
    do j=1, i
      t = ddot(n, vv(1,j),vv(1,i1))
      hh(j,i) = t
      call daxpy(n, -t, vv(1,j), 1, vv(1,i1), 1)
      t = dnrm2(n, vv(1,i1))
    end do
  else
    do j=1, i
      t = 0.d0
      do iii = 1,n
        t = t + vv(iii,j)*vv(iii,i1)
      end do
      hh(j,i) = t
      vv(:,i1) = vv(:,i1) - t * vv(:,j)
      t = sqrt(sum(vv(:,i1)*vv(:,i1)))
    end do
  end if
  hh(i1,i) = t
  if ( abs(t) .lt. epsmac) goto 58
  t = 1.0d0/t
  do k=1,n
    vv(k,i1) = vv(k,i1)*t
  end do
  !     done with modified gram schimd and arnoldi step.. now  update factorization of hh
58 if (i .eq. 1) goto 121
  do k=2,i
    k1 = k-1
    t = hh(k1,i)
    hh(k1,i) = c(k1)*t + s(k1)*hh(k,i)
    hh(k,i) = -s(k1)*t + c(k1)*hh(k,i)
  end do
121 gam = sqrt(hh(i,i)**2 + hh(i1,i)**2)
  if (abs(gam) .lt. epsmac) gam = epsmac
  !      get  next plane rotation
  c(i) = hh(i,i)/gam
  s(i) = hh(i1,i)/gam
  rs(i1) = -s(i)*rs(i)
  rs(i) =  c(i)*rs(i)
  !      detrermine residual norm and test for convergence-
  hh(i,i) = c(i)*hh(i,i) + s(i)*hh(i1,i)
  ro = abs(rs(i1))
  if (i .lt. im .and. (ro .gt. eps1))  goto 4
  !      now compute solution. first solve upper triangular system.
  rs(i) = rs(i)/hh(i,i)
  do ii=2,i
    k=i-ii+1
    k1 = k+1
    t=rs(k)
    do j=k1,i
      t = t-hh(k,j)*rs(j)
    end do
    rs(k) = t/hh(k,k)
  end do
  !      form linear combination of v(*,i)'s to get solution
  t = rs(1)
  do k=1, n
    rhs(k) = vv(k,1)*t
  end do
  do j = 2, i
    t = rs(j)
    do k=1, n
      rhs(k) = rhs(k)+t*vv(k,j)
    end do
  end do
  !      call preconditioner.
  if (lblas) then
    call lusol (n, rhs, rhs, alu, jlu, ju)
  else
    do iii = 1, n
      do k=jlu(iii),ju(iii)-1
        rhs(iii) = rhs(iii) - alu(k)* rhs(jlu(k))
      end do
    end do
    do iii = n, 1, -1
      do k=ju(iii),jlu(iii+1)-1
        rhs(iii) = rhs(iii) - alu(k)*rhs(jlu(k))
      end do
      rhs(iii) = alu(iii)*rhs(iii)
    end do
  end if
  do k=1, n
    sol(k) = sol(k) + rhs(k)
  end do
  !      restart outer loop  when necessary
  if (ro .le. eps1) goto 990
  if (its .ge. maxits) goto 991
  !      else compute residual vector and continue..
  do j=1,i
    jj = i1-j+1
    rs(jj-1) = -s(jj-1)*rs(jj)
    rs(jj) = c(jj-1)*rs(jj)
  end do
  do j=1,i1
    t = rs(j)
    if (j .eq. 1)  t = t-1.0d0
    if (lblas) then
      call daxpy (n, t, vv(1,j), 1,  vv, 1)
    else
      vv(:,j) = vv(:,j) + t * vv(:,1)
    end if
  end do
  ! 199   format('   its =', i4, ' res. norm =', d20.6)
  !      restart outer loop.
  goto 20
990 ierr = 0
  return
991 ierr = 1
  return
999 continue
  ierr = -1
  return
  !---------------------------------------------------------------------
end subroutine pgmres
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     subroutine from blas1.f90
!-----------------------------------------------------------------------
double precision function dnrm2(n,x)
  !     .. scalar arguments ..
  integer n
  !     ..
  !     .. array arguments ..
  double precision x(*)
  !     ..
  !
  !  purpose
  !  =======
  !
  !  dnrm2 returns the euclidean norm of a vector via the function
  !  name, so that
  !
  !     dnrm2 := sqrt( x'*x )
  !
  !  further details
  !  ===============
  !
  !  -- this version written on 25-october-1982.
  !     modified on 14-october-1993 to inline the call to dlassq.
  !     sven hammarling, nag ltd.
  !
  !  =====================================================================
  !
  !     .. parameters ..
  double precision one,zero
  parameter (one=1.0d+0,zero=0.0d+0)
  !     ..
  !     .. local scalars ..
  double precision absxi,norm,scale,ssq
  integer ix
  !     ..
  !     .. intrinsic functions ..
  intrinsic abs,sqrt
  !     ..
  if (n.lt.1 ) then
    norm = zero
  else if (n.eq.1) then
    norm = abs(x(1))
  else
    scale = zero
    ssq = one
    !        the following loop is equivalent to this call to the lapack
    !        auxiliary routine:
    !        call dlassq( n, x, scale, ssq )
    !
    do ix = 1,1 + (n-1)
      if (x(ix).ne.zero) then
        absxi = abs(x(ix))
        if (scale.lt.absxi) then
          ssq = one + ssq* (scale/absxi)**2
          scale = absxi
        else
          ssq = ssq + (absxi/scale)**2
        end if
      end if
    end do
    norm = scale*sqrt(ssq)
  end if
  !
  dnrm2 = norm
  return
  !
  !     end of dnrm2.
  !
end function dnrm2
!-----------------------------------------------------------------------
subroutine dlassq( n, x, scale, sumsq )
  !
  ! -- lapack auxiliary routine (version 3.1) --
  ! univ. of tennessee, univ. of california berkeley and nag ltd..
  ! november 2006
  integer n
  double precision scale, sumsq
  double precision x( * )
  !
  ! dlassq returns the values scl and smsq such that
  !
  ! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
  !
  ! where x( i ) = x( 1 + ( i - 1 )*incx ). the value of sumsq is
  ! assumed to be non-negative and scl returns the value
  !
  ! scl = max( scale, abs( x( i ) ) ).
  !
  ! scale (input/output) double precision
  ! on entry, the value scale in the equation above.
  ! on exit, scale is overwritten with scl , the scaling factor
  ! for the sum of squares.
  double precision zero
  parameter ( zero = 0.0d+0 )
  integer ix
  double precision absxi
  intrinsic abs
  !
  if( n.gt.0 ) then
    do ix = 1, 1 + ( n-1 )
      if( x( ix ).ne.zero ) then
        absxi = abs( x( ix ) )
        if( scale.lt.absxi ) then
          sumsq = 1 + sumsq*( scale / absxi )**2
          scale = absxi
        else
          sumsq = sumsq + ( absxi / scale )**2
        end if
      end if
    end do
  end if
  return
end subroutine dlassq
!-------------------------------------------------------------------------
double precision function ddot(n,dx,dy)
  !
  !     forms the dot product of two vectors.
  !     uses unrolled loops for increments equal to one.
  !     jack dongarra, linpack, 3/11/78.
  !
  double precision dx(*),dy(*),dtemp
  integer i,m,mp1,n
  !
  ddot = 0.0d0
  dtemp = 0.0d0
  if(n.le.0)return
20 m = mod(n,5)
  if( m .eq. 0 ) go to 40
  do i = 1,m
    dtemp = dtemp + dx(i)*dy(i)
  end do
  if( n .lt. 5 ) go to 60
40 mp1 = m + 1
  do i = mp1,n,5
    dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) + &
         &   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
  end do
60 ddot = dtemp
  return
end function ddot
!----------------------------------------------------------------------
subroutine daxpy(n,da,dx,incx,dy,incy)
  !
  !     constant times a vector plus a vector.
  !     uses unrolled loops for increments equal to one.
  !     jack dongarra, linpack, 3/11/78.
  !
  double precision dx(1),dy(1),da
  integer i,incx,incy,ix,iy,m,mp1,n
  !
  if(n.le.0)return
  if (abs(da) .lt. tiny(1.d0)) return
  if(incx.eq.1.and.incy.eq.1)go to 20
  !
  !        code for unequal increments or equal increments
  !          not equal to 1
  !
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do  i = 1,n
    dy(iy) = dy(iy) + da*dx(ix)
    ix = ix + incx
    iy = iy + incy
  end do
  return
  !
  !        code for both increments equal to 1
  !
  !
  !        clean-up loop
  !
20 m = mod(n,4)
  if( m .eq. 0 ) go to 40
  do i = 1,m
    dy(i) = dy(i) + da*dx(i)
  end do
  if( n .lt. 4 ) return
40 mp1 = m + 1
  do i = mp1,n,4
    dy(i) = dy(i) + da*dx(i)
    dy(i + 1) = dy(i + 1) + da*dx(i + 1)
    dy(i + 2) = dy(i + 2) + da*dx(i + 2)
    dy(i + 3) = dy(i + 3) + da*dx(i + 3)
  end do
  return
end subroutine daxpy
