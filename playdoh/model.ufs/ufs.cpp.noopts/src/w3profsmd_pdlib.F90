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
!/
!/ ------------------------------------------------------------------- /
module pdlib_w3profsmd
  !/                  +-----------------------------------+
  !/                  | wavewatch iii           noaa/ncep |
  !/                  |                                   |
  !/                  | aron roland (bgs it&e gmbh)       |
  !/                  | mathieu dutour-sikiric (irb)      |
  !/                  |                                   |
  !/                  |                        fortran 95 |
  !/                  | last update :         1-june-2018 |
  !/                  +-----------------------------------+
  !/
  !/    01-june-2016 : origination                        ( version 6.04 )
  !/
  !  1. purpose : pdlib version of ugtype including fully implicit
  !               discretization. this works is based on the thesis
  !               of roland, 2008 and represents the continues
  !               development of the solution of the wae on unstructured
  !               grids. following the quest since one decade we
  !               continuesly improve the aplicability and robustness of
  !               the source code and the methods. the development and
  !               implementation of the involved schemes was funded over
  !               the past decade by ifremer, shom, usace, ncep/noaa,
  !               bgs it&e gmbh, zanke & partner and roland & partner.
  !               the pdlib (parallel decomposition library) library,
  !               which is used here is courtesy to bgs it&e gmbh and
  !               has it's own license, which is the same as ww3. as of
  !               the origin of the methods, ideas and source code. this
  !               code was 1st developed in the wwm-iii (roland, 2008) and
  !               the ported to ww3. this is true for all source code
  !               related to ugtype.
  !
  !
  !  2. method :  we apply here the framework of residual distributions
  !               schemes for hyperbolic problems for nonlinear propagation
  !               laws based on the work of richiuotto et al. 2005.
  !               we supply the n-scheme, psi-scheme and lax-fct-scheme
  !               as explicit methods ranging from 1st order time space
  !               to most optimal psi method up to 2nd order lax-fct-scheme.
  !               for the implicit implementation we used up to now only
  !               the n-scheme. higher order schemes are up to now rather
  !               a research feature than for practical application. the
  !               reason is given in cavalleri et al. 2018, we do not
  !               resolve enough physics in order to be able to run
  !               2nd or even higher order schemes.
  !               use the numerical schemes with the needed care and
  !               do proper convergence analysis on the time step and
  !               grid size as well as solver threshold depedency.
  !               think about the time and spatial scales of your
  !               u r intending to resolve! multiscale modelling needs
  !               much work on the side of the modeler and much more
  !               time for the grid generation and the validation of the
  !               final model
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
  use w3servmd, only : print_memcheck
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
  !/ ------------------------------------------------------------------- /
  !/
  ! module default
  implicit none
  public
  !/
  !/ public variables
  !/
  logical               :: mapsta_hack = .false.
  real, allocatable     :: aspar_jac(:,:), aspar_diag_sources(:,:), aspar_diag_all(:,:), b_jac(:,:)
  real, allocatable     :: cad_the(:,:), cas_sig(:,:)
  real, allocatable     :: cwnb_sig_m2(:,:)
  real, allocatable     :: u_jac(:,:)
  real, allocatable     :: cofrm4(:)
  real*8, allocatable   :: flall1(:,:,:), kelem1(:,:,:)
  real*8, allocatable   :: flall2(:,:,:), kelem2(:,:,:)
  real*8, allocatable   :: flall3(:,:,:), kelem3(:,:,:)
  real*8, allocatable   :: nm(:,:,:), dtsi(:)
  integer, allocatable  :: iter(:)
  integer, allocatable  :: is0_pdlib(:)
  integer               :: freqshiftmethod = 2
  logical               :: fsgeoadvect
  logical, save         :: linit_output = .true.
  real, save            :: rtime = 0.d0
  integer               :: pos_trick(3,2)
  integer  :: memunit
  !
  !/ ------------------------------------------------------------------- /
  !
contains
  !
  !/ ------------------------------------------------------------------- /
  !
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_init(imod)
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
    use w3gdatmd, only: flcx, flcy
    use constants, only : grav, tpi
    use w3gdatmd, only: xgrd, ygrd, nx, nsea, ntri, trigp, nspec, nseal
    use w3gdatmd, only: mapsta, mapfs, grids, nth, sig, nk
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3gdatmd, only: ccon, countcon, index_cell, ie_cell
    use w3gdatmd, only: iobp, iobpa, iobpd, iobdp, si
    use w3adatmd, only: mpi_comm_wcmp, mpi_comm_wave
    use w3odatmd, only: iaproc, naproc, ntproc
    use yowdatapool, only: istatus
    use yowpdlibmain, only: initfromgriddim
    use yownodepool, only: npa, np, iplg
    use w3parall, only : pdlib_nseal, pdlib_nsealm
    use w3parall, only : jx_to_jsea, isea_to_jsea
    use yowfunction, only : computelistnp_listnpa_listiplg, pdlib_abort
    use w3gdatmd, only: fstotalimp, fstotalexp, fsnimp, fsn, fspsi, fsfct
    use w3gdatmd, only: fsrefraction, fsfreqshift, fssource
    !/
    include "mpif.h"
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
    !!      include "mpif.h"
    integer :: istat
    integer :: i, j, ibnd_map, isea, ip, ix, jsea, nb
    integer :: ip_glob
    integer :: myrank, ierr, iproc
    integer, allocatable :: nseal_arr(:)
    integer :: ierr_mpi
    integer :: iscal(1)
    integer, intent(in) :: imod
    integer :: ik, isp
    integer ik0, isp0, ith
    real :: esig, efr
    real, parameter :: coef4 = 5.0e-7
    pdlib_nseal = 0
    if (iaproc .le. naproc) then
      call mpi_comm_rank(mpi_comm_wcmp, myrank, ierr)
      !
      !
      if (fstotalexp) then
        call initfromgriddim(nx,ntri,trigp,nth,mpi_comm_wcmp)
      else
        call initfromgriddim(nx,ntri,trigp,nspec,mpi_comm_wcmp)
      endif
      !
      !
      !
      ! now the computation of nseal
      !
      !
      do ip = 1, npa
        ix = iplg(ip)
        isea = mapfs(1,ix)
        if (isea .gt. 0) pdlib_nseal = pdlib_nseal + 1
      end do
      allocate(jx_to_jsea(npa), isea_to_jsea(nsea), stat=istat)
      if(istat /= 0) call pdlib_abort(3)
      jsea         = 0
      jx_to_jsea   = 0
      isea_to_jsea = 0
      do ip = 1, npa
        ix = iplg(ip)
        isea = mapfs(1,ix)
        if (isea .gt. 0) then
          jsea=jsea+1
          jx_to_jsea(ip)=jsea
          isea_to_jsea(isea)=jsea
        end if
      end do
      !
      !
      ! map a point in (1:pdlib_nseal) to a point in (1:nsea)
      !
      nb=0
      do ix=1,nx
        if (mapfs(1,ix) .gt. 0) nb = nb + 1
      end do
      if (nb .ne. nsea) then
        write(*,*) 'logical error in computation of nsea / nb'
        write(*,*) 'nb=', nb, ' nsea=', nsea
        stop
      end if
    end if
    fsgeoadvect = .false.
    if ((flcx .eqv. .true.).and.(flcy .eqv. .true.)) then
      fsgeoadvect =.true.
    end if
    !
    ! compute nsealm
    !
    if (iaproc .le. naproc) then
      if (iaproc .eq. 1) then
        allocate(nseal_arr(naproc))
        nseal_arr(1)=pdlib_nseal
        do iproc=2,naproc
          call mpi_recv(iscal,1,mpi_int, iproc-1, 23, mpi_comm_wave, istatus, ierr_mpi)
          nseal_arr(iproc)=iscal(1)
        end do
        pdlib_nsealm=maxval(nseal_arr)
        deallocate(nseal_arr)
      else
        iscal(1)=pdlib_nseal
        call mpi_send(iscal,1,mpi_int, 0, 23, mpi_comm_wave, ierr_mpi)
      end if
    end if
    !
    if (iaproc .eq. 1) then
      iscal(1)=pdlib_nsealm
      do iproc = 2 , ntproc
        call mpi_send(iscal,1,mpi_int, iproc-1, 24, mpi_comm_wave, ierr_mpi)
      end do
    else
      call mpi_recv(iscal,1,mpi_int, 0, 24, mpi_comm_wave, istatus, ierr_mpi)
      pdlib_nsealm=iscal(1)
    end if
    !
    call computelistnp_listnpa_listiplg
    allocate(cofrm4(nk))
    do ik=1,nk
      esig=sig(ik)
      efr=esig/tpi
      cofrm4(ik)=coef4*grav/(efr**4)
    end do
    allocate(is0_pdlib(nspec))
    do isp=1, nspec
      is0_pdlib(isp) = isp - 1
    end do
    do isp=1, nspec, nth
      is0_pdlib(isp) = is0_pdlib(isp) + nth
    end do
    do jsea=1, pdlib_nseal
      ip      = jsea
      ip_glob = iplg(ip)
      isea    = mapfs(1,ip_glob)
      if (isea .ne. ip_glob) then
        write(*,*) jsea, pdlib_nseal, ip, ip_glob, isea
        write(*,*) 'isea .ne. ip_glob'
        call pdlib_abort(20)
      endif
    enddo
    !
    !
    !/
    !/ end of pdlib_init ------------------------------------------- /
    !/
  end subroutine pdlib_init
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_mapsta_init(imod)
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
    !  1. purpose : init mapsta part for pdlib
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
    use w3gdatmd, only : index_map, nbnd_map, nsea, nseal, mapsta, grids, nx, nth
    use w3gdatmd, only : mapsta_loc, nbnd_map, index_map
    use w3odatmd, only : iaproc, naproc
    use yownodepool, only: iplg, npa
    use yowfunction, only: pdlib_abort
    use w3odatmd, only: iaproc
    !/
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
    integer :: ibnd_map, isea, jsea, ix, ip, ip_glob
    integer, intent(in) :: imod
    integer :: status(nx), istat
    real :: rtmp(nseal)
    if (iaproc .gt. naproc) then
      return
    end if
    allocate(grids(imod)%mapsta_loc(npa), stat=istat)
    if(istat /= 0) call pdlib_abort(5)
    mapsta_loc => grids(imod)%mapsta_loc
    nbnd_map => grids(imod)%nbnd_map
    status = 0
    do ip=1,npa
      ip_glob=iplg(ip)
      status(ip_glob)=ip
      mapsta_loc(ip)=mapsta(1,ip_glob)
    end do
    nbnd_map = 0
    do ix=1,nx
      if ((mapsta(1,ix) .lt. 1).and.(status(ix).gt.0)) then
        nbnd_map = nbnd_map + 1
      end if
    end do
    allocate(grids(imod)%index_map(nbnd_map), stat=istat)
    if(istat /= 0) call pdlib_abort(6)
    index_map => grids(imod)%index_map
    ibnd_map = 0
    do ix = 1, nx
      if ((mapsta(1,ix) .lt. 1).and.(status(ix).gt.0)) then
        ibnd_map = ibnd_map + 1
        index_map(ibnd_map) = status(ix)
      end if
    end do
    !/
    !/ end of w3spr4 ----------------------------------------------------- /
    !/
  end subroutine pdlib_mapsta_init
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_iobp_init(imod)
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
    !  1. purpose : init mapsta part for pdlib
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
    use w3gdatmd, only : index_map, nbnd_map, nsea, nseal, grids, nx, nth
    use w3gdatmd, only : iobp, iobdp, iobpa, iobpd, nbnd_map, index_map
    use w3gdatmd, only : iobp_loc, iobpd_loc, iobdp_loc, iobpa_loc
    use w3odatmd, only : iaproc, naproc
    use yownodepool, only: iplg, npa
    use yowfunction, only: pdlib_abort
    use w3odatmd, only: iaproc
    !/
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
    integer             :: ibnd_map, isea, jsea, ix, ip, ip_glob
    integer, intent(in) :: imod
    integer             :: status(nx), istat
    real                :: rtmp(nseal)
    if (iaproc .gt. naproc) then
      return
    end if
    allocate(grids(imod)%iobp_loc(npa), stat=istat)
    if(istat /= 0) call pdlib_abort(7)
    allocate(grids(imod)%iobpd_loc(nth,npa), stat=istat)
    if(istat /= 0) call pdlib_abort(8)
    allocate(grids(imod)%iobdp_loc(npa), stat=istat)
    if(istat /= 0) call pdlib_abort(9)
    allocate(grids(imod)%iobpa_loc(npa), stat=istat)
    if(istat /= 0) call pdlib_abort(9)
    iobp_loc  => grids(imod)%iobp_loc
    iobpa_loc => grids(imod)%iobpa_loc
    iobpd_loc => grids(imod)%iobpd_loc
    iobdp_loc => grids(imod)%iobdp_loc
    do ip = 1, npa
      ip_glob         = iplg(ip)
      iobp_loc(ip)    = iobp(ip_glob)
      iobpd_loc(:,ip) = iobpd(:,ip_glob)
    end do
    iobdp_loc = 0
    iobp => null()
    iobpd => null()
    deallocate(grids(imod)%iobp,grids(imod)%iobpd)
    call set_iobpa_pdlib
    !/
    !/ end of w3spr4 ----------------------------------------------------- /
    !/
  end subroutine pdlib_iobp_init
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_w3xypug ( isp, facx, facy, dtg, vgx, vgy, lcalc )
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  | mathieu dutour-sikiric (irb)      |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :         10-jan-2011 |
    !/                  +-----------------------------------+
    !/
    !/    10-jan-2008 : origination.                        ( version 3.13 )
    !/    10-jan-2011 : addition of implicit scheme         ( version 3.14.4 )
    !/    06-feb-2014 : pdlib parallelization
    !/
    !  1. purpose : explicit advection schemes driver
    !
    !     propagation in physical space for a given spectral component.
    !     gives the choice of scheme on unstructured grid
    !     use the geographical parall algorithms for further speed.
    !
    !  2. method :
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
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
    use w3gdatmd, only: nx, ny, mapfs, clats,        &
         flcx, flcy, nk, nth, dth, xfr,              &
         ecos, esin, sig,  pfmove,                   &
         iobp, iobpd,                                &
         fsn, fspsi, fsfct, fsnimp,                  &
         gtype, ungtype, nbnd_map, index_map
    use yownodepool, only: pdlib_ien, pdlib_tria
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use yownodepool, only: iplg, npa
    use w3wdatmd, only: time, va
    use w3odatmd, only: tbpi0, tbpin, flbpi
    use w3adatmd, only: cg, cx, cy, itime, dw
    use w3idatmd, only: flcur, fllev
    use w3gdatmd, only: nseal
    use w3odatmd, only: iaproc
    use w3dispmd, only : wavnu_local
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    integer, intent(in)     :: isp
    real, intent(in)        :: facx, facy, dtg, vgx, vgy
    logical, intent(in)     :: lcalc
    logical                 :: scheme
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ik, isea
    integer                 :: i, j, ie, ibnd_map
    integer                 :: ip_glob
    real                    :: ccos, csin, ccurx, ccury, wn1, cg1
    real                    :: c(npa,2)
    real                    :: rd1, rd2
    !/
    !/ automatic work arrays
    !/
    real                    :: vlcflx(npa), vlcfly(npa)
    real                    :: ac(npa)
    real                    :: ac_map(nbnd_map)
    integer                 :: jsea, ip
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
    ac     = 0.
    !
    ! 2.  calculate velocities ---------------- *
    !
    do jsea = 1, nseal
      ip      = jsea
      ip_glob = iplg(ip)
      isea    = mapfs(1,ip_glob)
      ac(ip)  = va(isp,jsea) / cg(ik,isea) * clats(isea)
      vlcflx(ip) = ccos * cg(ik,isea) / clats(isea)
      vlcfly(ip) = csin * cg(ik,isea)
    end do
    if ( flcur ) then
      do jsea=1, nseal
        ip      = jsea
        ip_glob = iplg(ip)
        isea    = mapfs(1,ip_glob)
        !
        ! currents are not included on coastal boundaries (countseacon(ixy) .ne. pdlib_ccon(ixy))
        !
        if (iobp_loc(ip) .gt. 0) then
          vlcflx(ip) = vlcflx(ip) + ccurx*cx(isea)/clats(isea)
          vlcfly(ip) = vlcfly(ip) + ccury*cy(isea)
        end if
      end do
    end if
    c(:,1) = vlcflx(:) * iobdp_loc
    c(:,2) = vlcfly(:) * iobdp_loc
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
    ! saving data for mapsta business
    !
    if (mapsta_hack) then
      do ibnd_map=1,nbnd_map
        ip=index_map(ibnd_map)
        ac_map(ibnd_map) = ac(ip)
      end do
    end if
    !
    ! 4. propagate using the selected scheme
    !
    if (fsn) then
      call pdlib_w3xypfsn2(isp, c, lcalc, rd1, rd2, dtg, ac)
    else if (fspsi) then
      call pdlib_w3xypfspsi2(isp, c, lcalc, rd1, rd2, dtg, ac)
    else if (fsfct) then
      call pdlib_w3xypfsfct2(isp, c, lcalc, rd1, rd2, dtg, ac)
    else if (fsnimp) then
      stop 'for pdlib and fsnimp, no function has been programmed yet'
    endif
    !
    if (mapsta_hack) then
      do ibnd_map=1,nbnd_map
        ip=index_map(ibnd_map)
        ac(ip) = ac_map(ibnd_map)
      end do
    end if
    ! 6.  store results in vq in proper format --------------------------- *
    !
    do jsea=1, nseal
      ip      = jsea
      ip_glob = iplg(ip)
      isea=mapfs(1,ip_glob)
      va(isp,jsea) = max ( 0. , cg(ik,isea)/clats(isea)*ac(ip) )
    end do
    !/
    !/ end of w3spr4 ----------------------------------------------------- /
    !/
  end subroutine pdlib_w3xypug
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_w3xypfsn2(isp, c, lcalc, rd10, rd20, dt, ac)
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
    !  1. purpose : explicit n-scheme
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
    use w3gdatmd, only: nk, nth, nx,  ien, clats, mapsf
    use w3gdatmd, only: iobpd_loc, iobp_loc, iobdp_loc, iobpa_loc, fsbccfl
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter, dw , cflxymax, nsealm
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    use w3adatmd, only: mpi_comm_wcmp
    use w3gdatmd, only: nseal, dmin, nsea
    use yownodepool,    only: pdlib_si, pdlib_ien, pdlib_tria, ipgl, iplg, npa, np
    use yowelementpool, only: ne, ine
    use yowdatapool, only: rtype
    use yowexchangemodule, only : pdlib_exchange1dreal
    use w3odatmd, only : iaproc
    use mpi, only : mpi_min
    use w3parall, only : init_get_jsea_isproc
    use w3parall, only : onesixth, zero, thr
    use yowrankmodule, only : ipgl_npa
    integer, intent(in)    :: isp        ! actual frequency/wavenumber,
                                         ! actual wave direction
    real,    intent(in)    :: dt         ! time intervall for which the
                                         ! advection should be computed
                                         ! for the given velocity field
    real,    intent(in)    :: c(npa,2)   ! velocity field in it's
                                         ! x- and y- components,
    real,    intent(inout) :: ac(npa)    ! wave action before and
                                         ! after advection
    real,    intent(in)    :: rd10, rd20 ! time interpolation
                                         ! coefficients for boundary
                                         ! conditions
    logical, intent(in)    :: lcalc      ! switch for the calculation of
                                         ! the max. global time step
    integer :: ip, ie, pos, it, i1, i2, i3, i, j, ith, ik
    integer :: ibi, ni(3)
    integer :: jx
    !
    ! local real
    !
    real    :: rd1, rd2
    !:
    ! local double
    !
    real  :: utilde
    real  :: sumtheta
    real  :: ft, cflxy
    real  :: fl11, fl12, fl21, fl22, fl31, fl32
    real  :: fl111, fl112, fl211, fl212, fl311, fl312
    real  :: dtsi(npa), u(npa)
    real  :: dtmax_gl, dtmax, dtmaxexp, rest
    real  :: lambda(2), ktmp(3)
    real  :: kelem(3,ne), flall(3,ne)
    real  :: kksum(npa), st(npa)
    real  :: nm(ne)
    integer :: isproc, jsea, ip_glob, ierr, ix
    real  :: esumac, sumac, sumbpi0, sumbpin, sumcg, sumclats
    logical :: testwrite
    real  :: fin(1), fout(1)
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmax  = dble(10.e10)
    !
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ne
      i1 = ine(1,ie)
      i2 = ine(2,ie)
      i3 = ine(3,ie)
      lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! linearized advection speed in x and y direction
      lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2))
      kelem(1,ie) = lambda(1) * pdlib_ien(1,ie) + lambda(2) * pdlib_ien(2,ie) ! k-values - so called flux jacobians
      kelem(2,ie) = lambda(1) * pdlib_ien(3,ie) + lambda(2) * pdlib_ien(4,ie)
      kelem(3,ie) = lambda(1) * pdlib_ien(5,ie) + lambda(2) * pdlib_ien(6,ie)
      ktmp        = kelem(:,ie) ! copy
      nm(ie)      = - 1.d0/min(-thr,sum(min(zero,ktmp))) ! n-values
      kelem(:,ie) = max(zero,ktmp)
      fl11  = c(i2,1) * pdlib_ien(1,ie) + c(i2,2) * pdlib_ien(2,ie) ! weights for simpson integration
      fl12  = c(i3,1) * pdlib_ien(1,ie) + c(i3,2) * pdlib_ien(2,ie)
      fl21  = c(i3,1) * pdlib_ien(3,ie) + c(i3,2) * pdlib_ien(4,ie)
      fl22  = c(i1,1) * pdlib_ien(3,ie) + c(i1,2) * pdlib_ien(4,ie)
      fl31  = c(i1,1) * pdlib_ien(5,ie) + c(i1,2) * pdlib_ien(6,ie)
      fl32  = c(i2,1) * pdlib_ien(5,ie) + c(i2,2) * pdlib_ien(6,ie)
      fl111 = 2.d0*fl11+fl12
      fl112 = 2.d0*fl12+fl11
      fl211 = 2.d0*fl21+fl22
      fl212 = 2.d0*fl22+fl21
      fl311 = 2.d0*fl31+fl32
      fl312 = 2.d0*fl32+fl31
      flall(1,ie) = (fl311 + fl212) * onesixth + kelem(1,ie)
      flall(2,ie) = (fl111 + fl312) * onesixth + kelem(2,ie)
      flall(3,ie) = (fl211 + fl112) * onesixth + kelem(3,ie)
    end do
    if (lcalc) then
      kksum = zero
      do ie = 1, ne
        ni = ine(:,ie)
        kksum(ni) = kksum(ni) + kelem(:,ie)
      end do
      dtmaxexp = 1.e10
      do ip = 1, np
        ip_glob      = iplg(ip)
        if (iobp_loc(ip) .eq. 1 .or. fsbccfl) then
          dtmaxexp     = pdlib_si(ip)/max(dble(10.e-10),kksum(ip)*iobdp_loc(ip))
          dtmax        = min( dtmax, dtmaxexp)
        endif
        cflxymax(ip) = max(cflxymax(ip),dble(dt)/dtmaxexp)
      end do
      fin(1)=dtmax
      call mpi_allreduce(fin,fout,1,rtype,mpi_min,mpi_comm_wcmp,ierr)
      dtmax_gl=fout(1)
      cflxy = dble(dt)/dtmax_gl
      rest  = abs(mod(cflxy,1.0d0))
      if (rest .lt. thr) then
        iter(ik,ith) = abs(nint(cflxy))
      else if (rest .gt. thr .and. rest .lt. 0.5d0) then
        iter(ik,ith) = abs(nint(cflxy)) + 1
      else
        iter(ik,ith) = abs(nint(cflxy))
      end if
    end if ! lcalc
    do ip = 1, npa
      dtsi(ip) = dble(dt)/dble(iter(ik,ith))/pdlib_si(ip) ! some precalculations for the time integration.
    end do
    do it = 1, iter(ik,ith)
      u = dble(ac)
      st = zero
      do ie = 1, ne
        ni     = ine(:,ie)
        utilde = nm(ie) * (dot_product(flall(:,ie),u(ni)))
        st(ni) = st(ni) + kelem(:,ie) * (u(ni) - utilde) ! the 2nd term are the theta values of each node ...
      end do ! ie
      !
      ! iobpd=0  : waves coming from land
      ! iobpd=1 : waves coming from the coast
      !
      do ip = 1, npa
        u(ip) = max(zero,u(ip)-dtsi(ip)*st(ip)*(1-iobpa_loc(ip)))*dble(iobpd_loc(ith,ip))*iobdp_loc(ip)
      end do
      ac = real(u)
      !
      ! 5 update boundaries ... would be better to omit any if clause in this loop ...
      !   a possibility would be to use nbi = 0 when flbpi is false and loop on ibi whatever the value of nbi
      !
      if ( flbpi ) then
        rd1=rd10 - dt * real(iter(ik,ith)-it)/real(iter(ik,ith))
        rd2=rd20
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        do ibi = 1, nbi
          ip_glob = mapsf(isbpi(ibi),1)
          jx=ipgl_npa(ip_glob)
          if (jx .gt. 0) then
            ac(jx) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
                 / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
          end if
        end do
      end if
      call pdlib_exchange1dreal(ac)
    end do
  end subroutine pdlib_w3xypfsn2
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_w3xypfspsi2 ( isp, c, lcalc, rd10, rd20, dt, ac)
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
    !  1. purpose : explicit psi-scheme
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
    use w3gdatmd, only: nk, nth, nx,  ien, clats, mapsf
    use w3gdatmd, only: iobpd_loc, iobp_loc, iobdp_loc, iobpa_loc, fsbccfl
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter, dw , cflxymax, nsealm
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    use w3adatmd, only: mpi_comm_wcmp
    use w3gdatmd, only: nseal, dmin, nsea
    use yownodepool,    only: pdlib_si, pdlib_ien, pdlib_tria, ipgl, iplg, npa, np
    use yowelementpool, only: ne, ine
    use yowdatapool, only: rtype
    use yowexchangemodule, only : pdlib_exchange1dreal
    use w3odatmd, only : iaproc
    use mpi, only : mpi_min
    use w3parall, only : init_get_jsea_isproc
    use w3parall, only : onesixth, zero, thr
    use yowrankmodule, only : ipgl_npa
    implicit none
    integer, intent(in)    :: isp        ! actual frequency/wavenumber,
                                         ! actual wave direction
    real,    intent(in)    :: dt         ! time intervall for which the
                                         ! advection should be computed
                                         ! for the given velocity field
    real,    intent(in)    :: c(npa,2)   ! velocity field in it's
                                         ! x- and y- components,
    real,    intent(inout) :: ac(npa)    ! wave action before and
                                         ! after advection
    real,    intent(in)    :: rd10, rd20 ! time interpolation
                                         ! coefficients for boundary
                                         ! conditions
    logical, intent(in)    :: lcalc      ! switch for the calculation of
                                         ! the max. global time step
    integer :: ip, ie, pos, it, i1, i2, i3, i, j, ith, ik
    integer :: ibi, ni(3), jx
    integer :: isproc, ip_glob, jsea, ierr
    real    :: rd1, rd2
    real  :: utilde
    real  :: sumtheta
    real  :: fl1, fl2, fl3
    real  :: ft, cflxy
    real  :: fl11, fl12, fl21, fl22, fl31, fl32
    real  :: fl111, fl112, fl211, fl212, fl311, fl312
    real  :: dtsi(npa), u(npa)
    real  :: dtmax, dtmax_gl, dtmaxexp, rest
    real  :: lambda(2), ktmp(3), tmp(3)
    real  :: theta_l(3), bet1(3), betahat(3)
    real  :: kelem(3,ne), flall(3,ne)
    real  :: kksum(npa), st(npa)
    real  :: nm(ne), fin(1), fout(1)
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmax  = dble(10.e10)
    !
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ne
      i1 = ine(1,ie)
      i2 = ine(2,ie)
      i3 = ine(3,ie)
      lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! linearized advection speed in x and y direction
      lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2))
      kelem(1,ie) = lambda(1) * pdlib_ien(1,ie) + lambda(2) * pdlib_ien(2,ie) ! k-values - so called flux jacobians
      kelem(2,ie) = lambda(1) * pdlib_ien(3,ie) + lambda(2) * pdlib_ien(4,ie)
      kelem(3,ie) = lambda(1) * pdlib_ien(5,ie) + lambda(2) * pdlib_ien(6,ie)
      ktmp        = kelem(:,ie) ! copy
      nm(ie)      = - 1.d0/min(-thr,sum(min(zero,ktmp))) ! n-values
      kelem(:,ie) = max(zero,ktmp)
      fl11  = c(i2,1) * pdlib_ien(1,ie) + c(i2,2) * pdlib_ien(2,ie) ! weights for simpson integration
      fl12  = c(i3,1) * pdlib_ien(1,ie) + c(i3,2) * pdlib_ien(2,ie)
      fl21  = c(i3,1) * pdlib_ien(3,ie) + c(i3,2) * pdlib_ien(4,ie)
      fl22  = c(i1,1) * pdlib_ien(3,ie) + c(i1,2) * pdlib_ien(4,ie)
      fl31  = c(i1,1) * pdlib_ien(5,ie) + c(i1,2) * pdlib_ien(6,ie)
      fl32  = c(i2,1) * pdlib_ien(5,ie) + c(i2,2) * pdlib_ien(6,ie)
      fl111 = 2.d0*fl11+fl12
      fl112 = 2.d0*fl12+fl11
      fl211 = 2.d0*fl21+fl22
      fl212 = 2.d0*fl22+fl21
      fl311 = 2.d0*fl31+fl32
      fl312 = 2.d0*fl32+fl31
      flall(1,ie) = (fl311 + fl212)! * onesixth + kelem(1,ie)
      flall(2,ie) = (fl111 + fl312)! * onesixth + kelem(2,ie)
      flall(3,ie) = (fl211 + fl112)! * onesixth + kelem(3,ie)
    end do
    if (lcalc) then
      kksum = zero
      do ie = 1, ne
        ni = ine(:,ie)
        kksum(ni) = kksum(ni) + kelem(:,ie)
      end do
      dtmaxexp = 1.e10
      do ip = 1, npa
        ip_glob      = iplg(ip)
        if (iobp_loc(ip) .eq. 1 .or. fsbccfl) then
          dtmaxexp     = pdlib_si(ip)/max(dble(10.e-10),kksum(ip)*iobdp_loc(ip))
          dtmax        = min( dtmax, dtmaxexp)
        endif
        cflxymax(ip) = max(cflxymax(ip),dble(dt)/dtmaxexp)
      end do
      fin(1)=dtmax
      call mpi_allreduce(fin,fout,1,rtype,mpi_min,mpi_comm_wcmp,ierr)
      dtmax_gl=fout(1)
      cflxy = dble(dt)/dtmax_gl
      rest  = abs(mod(cflxy,1.0d0))
      if (rest .lt. thr) then
        iter(ik,ith) = abs(nint(cflxy))
      else if (rest .gt. thr .and. rest .lt. 0.5d0) then
        iter(ik,ith) = abs(nint(cflxy)) + 1
      else
        iter(ik,ith) = abs(nint(cflxy))
      end if
    end if ! lcalc
    do ip = 1, npa
      dtsi(ip) = dble(dt)/dble(iter(ik,ith))/pdlib_si(ip) ! some precalculations for the time integration.
    end do
    do it = 1, iter(ik,ith)
      u  = dble(ac)
      st = zero
      do ie = 1, ne
        ni   =  ine(:,ie)
        ft   = - onesixth*dot_product(u(ni),flall(:,ie))
        utilde = nm(ie) * ( dot_product(kelem(:,ie),u(ni)) - ft )
        theta_l(:) = kelem(:,ie) * (u(ni) - utilde)
        if (abs(ft) .gt. 0.0d0) then
          bet1(:) = theta_l(:)/ft
          if (any( bet1 .lt. 0.0d0) ) then
            betahat(1)    = bet1(1) + 0.5d0 * bet1(2)
            betahat(2)    = bet1(2) + 0.5d0 * bet1(3)
            betahat(3)    = bet1(3) + 0.5d0 * bet1(1)
            bet1(1)       = max(zero,min(betahat(1),1.d0-betahat(2),1.d0))
            bet1(2)       = max(zero,min(betahat(2),1.d0-betahat(3),1.d0))
            bet1(3)       = max(zero,min(betahat(3),1.d0-betahat(1),1.d0))
            theta_l(:) = ft * bet1
          end if
        else
          theta_l(:) = zero
        end if
        st(ni) = st(ni) + theta_l ! the 2nd term are the theta values of each node ...
      end do
      !
      do ip = 1, npa
        u(ip) = max(zero,u(ip)-dtsi(ip)*st(ip)*(1-iobpa_loc(ip)))*iobpd_loc(ith,ip)*iobdp_loc(ip)
      end do
      ac = real(u)
      !
      ! 5 update boundaries ... this should be implemented differently ... it is better to omit any if clause in this loop ...
      !
      if ( flbpi ) then
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
        ! nb: this treatment of the open boundary (time interpolation) is different from
        ! the constant boundary in the structured grids ... which restores the boundary
        ! to the initial value: if ( mapsta(ixy).eq.2 ) vq(ixy) = aq(ixy)
        ! why this difference ?
        !
        do ibi=1, nbi
          ip_glob    = mapsf(isbpi(ibi),1)
          jx=ipgl_npa(ip_glob)
          if (jx .gt. 0) then
            ac(jx) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
                 / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
          end if
        enddo
      end if
      call pdlib_exchange1dreal(ac)
    end do ! it
  end subroutine pdlib_w3xypfspsi2
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_w3xypfsfct2 ( isp, c, lcalc, rd10, rd20, dt, ac)
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
    !  1. purpose : explicit psi-scheme
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
    use w3gdatmd, only: nk, nth, nx,  ien, clats, mapsf
    use w3gdatmd, only: iobpd_loc, iobp_loc, iobdp_loc, iobpa_loc, fsbccfl
    use w3wdatmd, only: time
    use w3adatmd, only: cg, iter, dw , cflxymax, nsealm
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpin, isbpi, bbpi0, bbpin
    use w3timemd, only: dsec21
    use w3adatmd, only: mpi_comm_wcmp
    use w3gdatmd, only: nseal, dmin, nsea
    use yownodepool,    only: pdlib_si, pdlib_ien, pdlib_tria, pdlib_ccon, pdlib_ie_cell2, ipgl, iplg, npa, np
    use yowelementpool, only: ne, ine
    use yowdatapool, only: rtype
    use yowexchangemodule, only : pdlib_exchange1dreal
    use w3odatmd, only : iaproc
    use mpi, only : mpi_min
    use w3parall, only : init_get_jsea_isproc
    use w3parall, only : onesixth, zero, thr
    use yowrankmodule, only : ipgl_npa
    implicit none
    integer, intent(in)    :: isp        ! actual frequency/wavenumber,
                                         ! actual wave direction
    real,    intent(in)    :: dt         ! time intervall for which the
                                         ! advection should be computed
                                         ! for the given velocity field
    real,    intent(in)    :: c(npa,2)   ! velocity field in it's
                                         ! x- and y- components,
    real,    intent(inout) :: ac(npa)    ! wave action before and
                                         ! after advection
    real,    intent(in)    :: rd10, rd20 ! time interpolation
                                         ! coefficients for boundary
                                         ! conditions
    logical, intent(in)    :: lcalc      ! switch for the calculation of
                                         ! the max. global time step
    integer :: ip, ie, pos, it, i1, i2, i3, i, j, ith, ik
    integer :: ibi, ni(3)
    integer :: jx
    !
    ! local real
    !
    real    :: rd1, rd2
    !:
    ! local double
    !
    real  :: sumtheta, cflxy
    real*8  :: ft, utilde
    real*8  :: fl11, fl12, fl21, fl22, fl31, fl32
    real*8  :: fl111, fl112, fl211, fl212, fl311, fl312
    real  :: dtsi(npa), u(npa), ul(npa)
    real  :: dtmax_gl, dtmax, dtmaxexp, rest
    real*8  :: lambda(2), ktmp(3)
    real*8  :: kelem(3,ne), flall(3,ne)
    real*8  :: kksum(npa), st(npa)
    real*8  :: nm(ne), bet1(3), betahat(3), tmp(3), tmp1
    integer :: isproc, jsea, ip_glob, ierr, ix
    real  :: esumac, sumac, sumbpi0, sumbpin, sumcg, sumclats
    logical :: testwrite
    real  :: fin(1), fout(1)
    real  :: uip(ne), uipip(npa), uimip(npa), u3(3)
    real*8 :: theta_h(3), theta_ace(3,ne), theta_l(3,ne)
    real*8 :: pm(npa), pp(npa), uim(ne), wii(2,npa)
    real   :: ustari(2,npa)
    ith    = 1 + mod(isp-1,nth)
    ik     = 1 + (isp-1)/nth
    dtmax  = dble(10.e10)
    !
    !
    !2       propagation
    !2.a     calculate k-values and contour based quantities ...
    !
    do ie = 1, ne
      i1 = ine(1,ie)
      i2 = ine(2,ie)
      i3 = ine(3,ie)
      lambda(1) = onesixth *(c(i1,1)+c(i2,1)+c(i3,1)) ! linearized advection speed in x and y direction
      lambda(2) = onesixth *(c(i1,2)+c(i2,2)+c(i3,2))
      kelem(1,ie) = lambda(1) * pdlib_ien(1,ie) + lambda(2) * pdlib_ien(2,ie) ! k-values - so called flux jacobians
      kelem(2,ie) = lambda(1) * pdlib_ien(3,ie) + lambda(2) * pdlib_ien(4,ie)
      kelem(3,ie) = lambda(1) * pdlib_ien(5,ie) + lambda(2) * pdlib_ien(6,ie)
      ktmp        = kelem(:,ie) ! copy
      nm(ie)      = - 1.d0/min(-thr,sum(min(zero,ktmp))) ! n-values
      kelem(:,ie) = max(zero,ktmp)
      fl11  = c(i2,1) * pdlib_ien(1,ie) + c(i2,2) * pdlib_ien(2,ie) ! weights for simpson integration
      fl12  = c(i3,1) * pdlib_ien(1,ie) + c(i3,2) * pdlib_ien(2,ie)
      fl21  = c(i3,1) * pdlib_ien(3,ie) + c(i3,2) * pdlib_ien(4,ie)
      fl22  = c(i1,1) * pdlib_ien(3,ie) + c(i1,2) * pdlib_ien(4,ie)
      fl31  = c(i1,1) * pdlib_ien(5,ie) + c(i1,2) * pdlib_ien(6,ie)
      fl32  = c(i2,1) * pdlib_ien(5,ie) + c(i2,2) * pdlib_ien(6,ie)
      fl111 = 2.d0*fl11+fl12
      fl112 = 2.d0*fl12+fl11
      fl211 = 2.d0*fl21+fl22
      fl212 = 2.d0*fl22+fl21
      fl311 = 2.d0*fl31+fl32
      fl312 = 2.d0*fl32+fl31
      flall(1,ie) = (fl311 + fl212)! * onesixth + kelem(1,ie)
      flall(2,ie) = (fl111 + fl312)! * onesixth + kelem(2,ie)
      flall(3,ie) = (fl211 + fl112)! * onesixth + kelem(3,ie)
    end do
    if (lcalc) then
      kksum = zero
      do ie = 1, ne
        ni = ine(:,ie)
        kksum(ni) = kksum(ni) + kelem(:,ie)
      end do
      dtmaxexp = 1.e10
      do ip = 1, np
        ip_glob      = iplg(ip)
        if (iobp_loc(ip) .eq. 1 .or. fsbccfl) then
          dtmaxexp     = pdlib_si(ip)/max(dble(10.e-10),kksum(ip)*iobdp_loc(ip))
          dtmax        = min( dtmax, dtmaxexp)
        endif
        cflxymax(ip) = max(cflxymax(ip),dble(dt)/dtmaxexp)
      end do
      fin(1)=dtmax
      call mpi_allreduce(fin,fout,1,rtype,mpi_min,mpi_comm_wcmp,ierr)
      dtmax_gl=fout(1)
      cflxy = dble(dt)/dtmax_gl
      rest  = abs(mod(cflxy,1.0d0))
      if (rest .lt. thr) then
        iter(ik,ith) = abs(nint(cflxy))
      else if (rest .gt. thr .and. rest .lt. 0.5d0) then
        iter(ik,ith) = abs(nint(cflxy)) + 1
      else
        iter(ik,ith) = abs(nint(cflxy))
      end if
    end if ! lcalc
    do ip = 1, npa
      dtsi(ip) = dble(dt)/dble(iter(ik,ith))/pdlib_si(ip) ! some precalculations for the time integration.
    end do
    do it = 1, iter(ik,ith)
      u  = dble(ac)
      st = zero
      pm = zero
      pp = zero
      do ie = 1, ne
        ni   =  ine(:,ie)
        ft   = - onesixth*dot_product(u(ni),flall(:,ie))
        utilde = nm(ie) * ( dot_product(kelem(:,ie),u(ni)) - ft )
        theta_l(:,ie) = kelem(:,ie) * (u(ni) - utilde)
        if (abs(ft) .gt. 0.0d0) then
          bet1(:) = theta_l(:,ie)/ft
          if (any( bet1 .lt. 0.0d0) ) then
            betahat(1)    = bet1(1) + 0.5d0 * bet1(2)
            betahat(2)    = bet1(2) + 0.5d0 * bet1(3)
            betahat(3)    = bet1(3) + 0.5d0 * bet1(1)
            bet1(1)       = max(zero,min(betahat(1),1.d0-betahat(2),1.d0))
            bet1(2)       = max(zero,min(betahat(2),1.d0-betahat(3),1.d0))
            bet1(3)       = max(zero,min(betahat(3),1.d0-betahat(1),1.d0))
            theta_l(:,ie) = ft * bet1
          end if
        end if
        st(ni) = st(ni) + theta_l(:,ie) ! the 2nd term are the theta values of each node ...
        theta_h         = (1./3.+dt/(2.*pdlib_tria(ie)) * kelem(:,ie) ) * ft ! lax
        !        theta_h = (1./3.+2./3.*kelem(:,ie)/sum(max(zero,kelem(:,ie))))*ft  ! central ... can be tested as well a bit more dispersive then lax
        theta_ace(:,ie) = theta_h-theta_l(:,ie)
        pp(ni) =  pp(ni) + max(zero, -theta_ace(:,ie)) * dtsi(ni)
        pm(ni) =  pm(ni) + min(zero, -theta_ace(:,ie)) * dtsi(ni)
      end do
      do ip = 1, npa
        ul(ip) = max(zero,u(ip)-dtsi(ip)*st(ip)*(1-iobpa_loc(ip)))*dble(iobpd_loc(ith,ip))*iobdp_loc(ip)
      end do
      ustari(1,:) = max(ul,u)
      ustari(2,:) = min(ul,u)
      uip = 0.
      uim = 0.
      do ie = 1, ne
        ni = ine(:,ie)
        uip(ni) = max (uip(ni), maxval( ustari(1,ni) ))
        uim(ni) = min (uim(ni), minval( ustari(2,ni) ))
      end do
      wii(1,:) = min(1.0d0,(uip-ul)/max( thr,pp))
      wii(2,:) = min(1.0d0,(uim-ul)/min(-thr,pm))
      st = zero
      do ie = 1, ne
        i1 = ine(1,ie)
        i2 = ine(2,ie)
        i3 = ine(3,ie)
        if (theta_ace(1,ie) .lt. zero) then
          tmp(1) = wii(1,i1)
        else
          tmp(1) = wii(2,i1)
        end if
        if (theta_ace(2,ie) .lt. zero) then
          tmp(2) = wii(1,i2)
        else
          tmp(2) = wii(2,i2)
        end if
        if (theta_ace(3,ie) .lt. zero) then
          tmp(3) = wii(1,i3)
        else
          tmp(3) = wii(2,i3)
        end if
        tmp1 = minval(tmp)
        st(i1) = st(i1) + theta_ace(1,ie) * tmp1! * (one - bl) + bl * theta_l(1,ie)
        st(i2) = st(i2) + theta_ace(2,ie) * tmp1! * (one - bl) + bl * theta_l(2,ie)
        st(i3) = st(i3) + theta_ace(3,ie) * tmp1! * (one - bl) + bl * theta_l(3,ie)
      end do
      do ip = 1, npa
        u(ip) = max(zero,ul(ip)-dtsi(ip)*st(ip)*(1-iobpa_loc(ip)))*dble(iobpd_loc(ith,ip))*iobdp_loc(ip)
      end do
      ac = real(u)
      !
      ! 5 update boundaries ... would be better to omit any if clause in this loop ...
      !   a possibility would be to use nbi = 0 when flbpi is false and loop on ibi whatever the value of nbi
      !
      if ( flbpi ) then
        rd1=rd10 - dt * real(iter(ik,ith)-it)/real(iter(ik,ith))
        rd2=rd20
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        do ibi = 1, nbi
          ip_glob = mapsf(isbpi(ibi),1)
          jx=ipgl_npa(ip_glob)
          if (jx .gt. 0) then
            ac(jx) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
                 / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
          end if
        end do
      end if
      call pdlib_exchange1dreal(ac)
    end do ! it
  end subroutine pdlib_w3xypfsfct2
  !/ ------------------------------------------------------------------- /
  subroutine test_mpi_status(string)
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
    !  1. purpose : check mpi status
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
    use w3adatmd, only : mpi_comm_wcmp
    use w3gdatmd, only : gtype, ungtype
    use w3odatmd, only : iaproc, naproc, ntproc
    use yowdatapool, only: rtype, istatus
    include "mpif.h"
    character(*), intent(in) :: string
    real vcollexp(1)
    real rvect(1)
    integer iproc, ierr
    write(740+iaproc,*) 'test_mpi_status, at string=', string
    flush(740+iaproc)
    if (iaproc .gt. naproc) then
      return
    end if
    write(740+iaproc,*) 'after status settings'
    flush(740+iaproc)
    !
    ! now find global arrays
    !
    if (iaproc .eq. 1) then
      do iproc=2,naproc
        call mpi_recv(rvect,1,mpi_real, iproc-1, 37, mpi_comm_wcmp, istatus, ierr)
      end do
    else
      call mpi_send(vcollexp,1,mpi_real, 0, 37, mpi_comm_wcmp, ierr)
    end if
    write(740+iaproc,*) 'leaving the test_mpi_status'
    flush(740+iaproc)
  end subroutine test_mpi_status
  !/ ------------------------------------------------------------------- /
  subroutine scal_integral_print_general(v, string, maxidx, checkuncovered, printfullvalue)
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
    !  1. purpose : source code for parallel debugging
    !  2. method : maxidx = npa or np for arrays that have been synchronized or not
    !              checkuncovered is because some the triangulation may not cover all nodes
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
    use w3gdatmd, only : nk, nth, fte
    use w3gdatmd, only : nspec, nx, ny, nseal, mapfs
    use w3adatmd, only : mpi_comm_wcmp
    use w3gdatmd, only : gtype, ungtype
    use w3odatmd, only : iaproc, naproc, ntproc
    use yowdatapool, only: rtype, istatus
    use yownodepool, only: npa, iplg
    use w3parall, only: init_get_isea
    include "mpif.h"
    !
    real*8, intent(in) :: v(nseal)
    character(*), intent(in) :: string
    integer, intent(in) :: maxidx
    logical, intent(in) :: checkuncovered
    logical, intent(in) :: printfullvalue
    !
    real*8, allocatable :: vcoll(:)
    integer, allocatable :: status(:)
    real*8, allocatable :: listval(:)
    integer, allocatable :: listidx(:)
    integer singv(2)
    real coherencyerror, eval1, eval2, eerr
    integer nseal_dist, maxidx_dist
    integer jsea, isea, iproc, i, ix, ierr, isp, ip, ip_glob
    integer nbincorr, idx
    integer ith, ik
    if (iaproc .gt. naproc) then
      return
    end if
    if (gtype .ne. ungtype) then
      return
    end if
    !
    ! now find global arrays
    !
    if (iaproc .eq. 1) then
      coherencyerror=0
      allocate(vcoll(nx), status(nx))
      vcoll=0
      status=0
      do jsea=1,maxidx
        ip      = jsea
        ip_glob = iplg(ip)
        isea=mapfs(1,ip_glob)
        vcoll(ip_glob)=v(jsea)
        status(ip_glob)=1
      end do
      do iproc=2,naproc
        call mpi_recv(singv,2,mpi_integer, iproc-1, 360, mpi_comm_wcmp, istatus, ierr)
        nseal_dist = singv(1)
        maxidx_dist = singv(2)
        allocate(listval(nseal_dist), listidx(nseal_dist))
        call mpi_recv(listval, nseal_dist, mpi_real8,   iproc-1, 370, mpi_comm_wcmp, istatus, ierr)
        call mpi_recv(listidx, nseal_dist, mpi_integer, iproc-1, 430, mpi_comm_wcmp, istatus, ierr)
        do idx=1,maxidx_dist
          ip_glob = listidx(idx)
          eval1 = vcoll(ip_glob)
          eval2 = listval(idx)
          vcoll(ip_glob) = eval2
          if (status(ip_glob) .eq. 1) then
            eerr=abs(eval1 - eval2)
            coherencyerror = coherencyerror + eerr
          end if
          status(ip_glob) = 1
        end do
        deallocate(listval, listidx)
      end do
      write(740+iaproc,'(a,f14.7,f14.7,a,a)') 'sum,coh=', sum(vcoll), coherencyerror, ' ', trim(string)
      nbincorr=0
      do ix=1,nx
        isea=mapfs(1,ix)
        if (isea .gt. 0) then
          if (status(ix) .eq. 0) then
            nbincorr=nbincorr+1
          end if
        end if
      end do
      if (checkuncovered) then
        if (nbincorr .gt. 0) then
          write(*,*) '    nbincorr=', nbincorr
          write(*,*) '          nx=', nx
          write(*,*) '       nseal=', nseal
          write(*,*) '         npa=', npa
          stop
        end if
      end if
      if (printfullvalue) then
        write(740+iaproc,*) 'value of v at nodes'
        do ix=1,nx
          write(740+iaproc,*) 'ix=', ix, ' v=', vcoll(ix)
        end do
      end if
      flush(740+iaproc)
      deallocate(vcoll, status)
    else
      singv(1) = nseal
      singv(2) = maxidx
      call mpi_send(singv,2,mpi_integer, 0, 360, mpi_comm_wcmp, ierr)
      allocate(listval(nseal), listidx(nseal))
      do jsea=1,nseal
        ip      = jsea
        ip_glob = iplg(ip)
        isea=mapfs(1,ip_glob)
        listval(jsea) = v(jsea)
        listidx(jsea) = ip_glob
      end do
      call mpi_send(listval, nseal, mpi_real8,   0, 370, mpi_comm_wcmp, ierr)
      call mpi_send(listidx, nseal, mpi_integer, 0, 430, mpi_comm_wcmp, ierr)
      deallocate(listval, listidx)
    end if
  end subroutine scal_integral_print_general
  !/ ------------------------------------------------------------------- /
  subroutine scal_integral_print_r8(v, string)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nseal
    real*8, intent(in) :: v(nseal)
    character(*), intent(in) :: string
    real*8 :: v8(nseal)
    logical :: checkuncovered = .false.
    logical :: printfullvalue = .false.
    v8 = v
    call scal_integral_print_general(v8, string, nseal, checkuncovered, printfullvalue)
  end subroutine scal_integral_print_r8
  !/ ------------------------------------------------------------------- /
  subroutine scal_integral_print_r4(v, string)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nseal
    real, intent(in) :: v(nseal)
    character(*), intent(in) :: string
    logical :: checkuncovered = .false.
    logical :: printfullvalue = .false.
    real*8 v8(nseal)
    v8 = dble(v)
    call scal_integral_print_general(v8, string, nseal, checkuncovered, printfullvalue)
  end subroutine scal_integral_print_r4
  !/ ------------------------------------------------------------------- /
  subroutine all_vaold_integral_print(string, choice)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nseal
    use w3wdatmd, only : vaold
    use w3odatmd, only : iaproc
    use w3gdatmd, only : nspec
    use yownodepool, only: np, npa
    character(*), intent(in) :: string
    integer, intent(in) :: choice
    real :: field(nspec,nseal)
    integer ispec, jsea, maxidx
    logical :: printminisp = .false.
    logical :: localizemaximum = .false.
    do jsea=1,nseal
      do ispec=1,nspec
        field(ispec,jsea) = vaold(ispec,jsea)
      end do
    end do
    if (choice .eq. 1) then
      maxidx = npa
    else
      maxidx = np
    end if
    !      call all_field_integral_print_general(field, string)
    call check_array_integral_nx_r8_maxfunct(field, string, maxidx, printminisp, localizemaximum)
  end subroutine all_vaold_integral_print
  !/ ------------------------------------------------------------------- /
  subroutine all_va_integral_print(imod, string, choice)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nseal, nsea, nx, ny
    use w3wdatmd, only : va
    use w3odatmd, only : iaproc, naproc
    use w3gdatmd, only : nspec, grids, gtype, ungtype
    use yownodepool, only: npa, np, iplg
    integer, intent(in) :: imod
    character(*), intent(in) :: string
    integer, intent(in) :: choice
    real :: field(nspec,nseal)
    integer ispec, jsea, ip_glob, maxidx
    logical :: printminisp = .false.
    logical :: localizemaximum = .false.
    integer :: test_ip = 46
    integer :: test_isp = 370
    if (grids(imod)%gtype .ne. ungtype) then
      return
    end if
    if (iaproc .gt. naproc) then
      return
    end if
    write(740+iaproc,*) 'entering all_integral_print, nseal=', nseal
    flush(740+iaproc)
    if (nseal .ne. npa) then
      print *, 'nseal=', nseal, " npa=", npa
      stop
    end if
    do jsea=1,nseal
      ip_glob=iplg(jsea)
      do ispec=1,nspec
        field(ispec,jsea) = va(ispec,jsea)
        if ((ip_glob .eq. test_ip).and.(ispec .eq. test_isp)) then
          write(740+iaproc,*) 'ass test_ip=', test_ip, ' test_isp=', test_isp, ' val=', va(ispec,jsea)
        end if
      end do
    end do
    write(740+iaproc,*) 'before call to all_field_integral'
    write(740+iaproc,*) 'nspec=', nspec, ' nx=', nx
    flush(740+iaproc)
    if (choice .eq. 1) then
      maxidx = npa
    else
      maxidx = np
    end if
    call check_array_integral_nx_r8_maxfunct(field, string, maxidx, printminisp, localizemaximum)
    write(740+iaproc,*) 'after call to all_field_integral'
    flush(740+iaproc)
  end subroutine all_va_integral_print
  !/ ------------------------------------------------------------------- /
  subroutine all_field_integral_print(field, string)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nseal
    use w3wdatmd, only : va
    use w3odatmd, only : iaproc
    use w3gdatmd, only : nspec
    integer maxidx
    real, intent(in) :: field(nspec,nseal)
    character(*), intent(in) :: string
    logical :: printminisp = .false.
    logical :: localizemaximum = .false.
    maxidx = nseal
    call check_array_integral_nx_r8_maxfunct(field, string, maxidx, printminisp, localizemaximum)
  end subroutine all_field_integral_print
  !/ ------------------------------------------------------------------- /
  !/ ------- coherency info for thearr(nspec,npa) ---------------------- /
  !/ ----------- maxidx is np or npa ----------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine check_array_integral_nx_r8_maxfunct(thearr, string, maxidx, printminisp, localizemaximum)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nk, nth
    use w3gdatmd, only : nspec, nx, ny, nseal, mapfs
    use w3adatmd, only : mpi_comm_wcmp
    use w3gdatmd, only : gtype, ungtype
    use w3odatmd, only : iaproc, naproc, ntproc
    use yowdatapool, only: rtype, istatus
    use yownodepool, only: npa, iplg
    use w3parall, only: init_get_isea
    include "mpif.h"
    character(*), intent(in) :: string
    integer, intent(in) :: maxidx
    real, intent(in) :: thearr(nspec, npa)
    logical, intent(in) :: printminisp, localizemaximum
    !
    real vcoll(nspec,nx), vcollexp(nspec*nx), rvect(nspec*nx)
    real coherencyerror_max, coherencyerror_sum
    real eval1, eval2, eerr
    integer locatemax_i, locatemax_isp
    integer rstatus(nx), status(nx)
    integer jsea, isea, iproc, i, ix, ierr, isp, ip, ip_glob
    real :: mval, eval, esum
    real :: themax, thesum, thenb, theavg
    real :: efact, threshold
    logical :: isfirst
    integer nbincorr, n_control
    integer ith, ik
    integer :: test_ip = 46
    integer :: test_isp = 370
    if (iaproc .gt. naproc) then
      return
    end if
    if (gtype .ne. ungtype) then
      return
    end if
    write(740+iaproc,*) 'check_array_integral nseal=', nseal, ' npa=', npa, ' maxidx=', maxidx
    vcollexp=0
    status=0
    do ip=1,maxidx
      ip_glob=iplg(ip)
      do isp=1,nspec
        vcollexp(isp+nspec*(ip_glob-1)) = thearr(isp,ip)
        if ((ip_glob .eq. test_ip).and.(isp .eq. test_isp)) then
          write(740+iaproc,*) 'test_ip=', test_ip, ' test_isp=', test_isp, ' val=', thearr(isp,ip)
        end if
      end do
      status(ip_glob)=1
    end do
    !
    ! now find global arrays
    !
    coherencyerror_max = 0
    coherencyerror_sum = 0
    locatemax_i = -1
    locatemax_isp = -1
    n_control = 0
    if (iaproc .eq. 1) then
      do iproc=2,naproc
        call mpi_recv(rvect  ,nspec*nx,mpi_real   , iproc-1, 37, mpi_comm_wcmp, istatus, ierr)
        call mpi_recv(rstatus,nx      ,mpi_integer, iproc-1, 43, mpi_comm_wcmp, istatus, ierr)
        do i=1,nx
          if (rstatus(i) .eq. 1) then
            do isp=1,nspec
              eval1 = vcollexp(isp+nspec*(i-1))
              eval2 = rvect(isp+nspec*(i-1))
              if (status(i) .eq. 1) then
                eerr=abs(eval1 - eval2)
                coherencyerror_sum = coherencyerror_sum + eerr
                if (eerr .gt. coherencyerror_max) then
                  coherencyerror_max = eerr
                  locatemax_i = i
                  locatemax_isp = isp
                end if
                if (isp .eq. 1) then
                  n_control = n_control + 1
                end if
              else
                vcollexp(isp+nspec*(i-1))=eval2
              end if
            end do
            status(i)=1
          end if
        end do
      end do
    else
      call mpi_send(vcollexp,nspec*nx,mpi_real   , 0, 37, mpi_comm_wcmp, ierr)
      call mpi_send(status  ,nx      ,mpi_integer, 0, 43, mpi_comm_wcmp, ierr)
    end if
    if (iaproc .eq. 1) then
      do i=1,nx
        do isp=1,nspec
          vcoll(isp,i)=vcollexp(isp + nspec*(i-1))
        end do
      end do
      nbincorr=0
      do ix=1,nx
        isea=mapfs(1,ix)
        if (isea .gt. 0) then
          if (status(ix) .eq. 0) then
            nbincorr=nbincorr+1
          end if
        end if
      end do
      if (nbincorr .gt. 0) then
        write(*,*) '    nbincorr=', nbincorr
        write(*,*) '          nx=', nx
        write(*,*) '         npa=', npa
        stop
      end if
      write(740+iaproc,*) 'check_array_integral n_control=', n_control
      write(740+iaproc,*) 'array_nx sum,coh=', sum(vcoll), coherencyerror_sum, trim(string)
      write(740+iaproc,*) 'array_nx max,loc=', coherencyerror_max,locatemax_i,locatemax_isp, trim(string)
      if (printminisp) then
        do isp=1,nspec
          isfirst=.true.
          esum=0
          do ip=1,maxidx
            eval=abs(vcoll(isp, ip))
            esum=esum + eval
            if (isfirst.eqv. .true.) then
              mval=eval
            else
              if (eval .lt. mval) then
                mval=eval
              endif
            endif
            isfirst=.false.
          end do
          write(740+iaproc,*) 'isp=', isp, ' mval/sum=', mval, esum
        end do
        flush(740+iaproc)
      end if
      if (localizemaximum) then
        themax=0
        thenb=0
        thesum=0
        do ip=1,maxidx
          do isp=1,nspec
            eval = abs(vcoll(isp, ip))
            thesum = thesum + eval
            thenb = thenb + 1
            if (eval .gt. themax) then
              themax=eval
            end if
          end do
        end do
        theavg = thesum / thenb
        write(740+iaproc,*) 'theavg/themax=', theavg, themax
        efact=0.5
        threshold=efact * themax
        do ip=1,maxidx
          do isp=1,nspec
            eval = abs(vcoll(isp, ip))
            if (eval .gt. threshold) then
              write(740+iaproc,*) 'isp/ip/val=', isp, ip, eval
            end if
          end do
        end do
        flush(740+iaproc)
      end if
    end if
  end subroutine check_array_integral_nx_r8_maxfunct
  !/ ------------------------------------------------------------------- /
  !*    maxidx should be "np" or "npa"                                  *
  subroutine check_array_integral_nx_r8(thearr, string, maxidx)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nspec
    use yownodepool, only: npa
    character(*), intent(in) :: string
    integer, intent(in) :: maxidx
    real, intent(in) :: thearr(nspec, npa)
    real*8 :: thearr_red(npa)
    !      logical :: full_nspec = .false.
    !      logical :: printminisp = .false.
    !      logical :: localizemaximum = .false.
    !      logical :: checkuncovered = .false.
    !      logical :: printfullvalue = .false.
    logical :: full_nspec = .true.
    logical :: printminisp = .true.
    logical :: localizemaximum = .true.
    logical :: checkuncovered = .true.
    logical :: printfullvalue = .true.
    integer :: ip
    if (full_nspec) then
      call check_array_integral_nx_r8_maxfunct(thearr, string, maxidx, printminisp, localizemaximum)
    else
      do ip=1,npa
        thearr_red(ip) = sum(abs(thearr(:,ip)))
      end do
      call scal_integral_print_general(thearr_red, string, maxidx, checkuncovered, printfullvalue)
    end if
  end subroutine check_array_integral_nx_r8
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_w3xypug_block_implicit(imod, facx, facy, dtg, vgx, vgy, lcalc )
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
    !/    01-june-2018 : origination.                        ( version 6.04 )
    !/
    !  1. purpose : block explicit n-scheme
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
    use w3odatmd, only: iaproc
    use w3gdatmd, only: b_jgs_use_jacobi
    logical, intent(in) :: lcalc
    integer, intent(in) :: imod
    real, intent(in)        :: facx, facy, dtg, vgx, vgy
    if (b_jgs_use_jacobi) then
      call pdlib_jacobi_gauss_seidel_block(imod, facx, facy, dtg, vgx, vgy, lcalc)
      return
    end if
    write(*,*) 'error: you need to use with jgs_use_jacobi'
    stop 'correct your implicit solver options'
    !/
    !/ end of w3xypfsn --------------------------------------------------- /
    !/
  end subroutine pdlib_w3xypug_block_implicit
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_w3xypug_block_explicit(imod, facx, facy, dtg, vgx, vgy, lcalc)
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
    !  1. purpose : driver for block explicit routine
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
    use w3odatmd, only: iaproc
    use w3gdatmd, only: b_jgs_use_jacobi
    logical, intent(in) :: lcalc
    integer, intent(in) :: imod
    real, intent(in) :: facx, facy, dtg, vgx, vgy
    call pdlib_explicit_block(imod, facx, facy, dtg, vgx, vgy, lcalc)
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine pdlib_w3xypug_block_explicit
  !/ --------------------------------------------------------------------- /
  subroutine print_wn_statistic(string)
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
    !  1. purpose : source code for parallel debugging
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
    use w3odatmd, only : iaproc
    use w3gdatmd, only: nk
    use w3adatmd, only: wn
    use w3gdatmd, only: nseal
    use yownodepool, only: np
    character(*), intent(in) :: string
    real totalsumdmm, edmm, sumdmm
    integer ip, ik, isea
    write(740+iaproc,*) 'print_wn_statistic'
    totalsumdmm=0
    do isea=1,nseal
      sumdmm=0
      do ik=0, nk
        edmm = wn(ik+1,isea) - wn(ik,isea)
        sumdmm=sumdmm + abs(edmm)
      end do
      if (isea .eq. 1) then
        write(740+iaproc,*) 'isea=', isea
        write(740+iaproc,*) 'sumdmm=', sumdmm
      end if
      totalsumdmm = totalsumdmm + sumdmm
    end do
    write(740+iaproc,*) 'string=', string
    write(740+iaproc,*) 'totalsumdmm=', totalsumdmm
    flush(740+iaproc)
    !/
    !/ end of w3xypfsn --------------------------------------------------- /
    !/
  end subroutine print_wn_statistic
  !/ ------------------------------------------------------------------- /
  subroutine write_var_to_text_file(thearr, efile)
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
    !  1. purpose : source code for parallel debugging
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
    use w3gdatmd, only : nk, nth
    use w3wdatmd, only : va
    use w3gdatmd, only : nspec, nx, ny, nseal, mapfs
    use w3adatmd, only : mpi_comm_wcmp
    use w3gdatmd, only : gtype, ungtype
    use w3odatmd, only : iaproc, naproc, ntproc
    use yowdatapool, only: rtype, istatus
    use yownodepool, only: npa, iplg, np
    use w3parall, only: init_get_isea
    include "mpif.h"
    character(*), intent(in) :: efile
    real, intent(in) :: thearr(nspec, npa)
    !
    real vcoll(nspec,nx), vcollexp(nspec*nx), rvect(nspec*nx)
    real coherencyerror, eval1, eval2, eerr
    integer rstatus(nx), status(nx)
    integer jsea, isea, iproc, i, ix, ierr, isp, ip, ip_glob
    integer nbincorr
    integer ith, ik
    integer fhndl
    real esum
    if (iaproc .gt. naproc) then
      return
    end if
    if (gtype .ne. ungtype) then
      return
    end if
    vcollexp=0
    status=0
    do ip=1,np
      ip_glob=iplg(ip)
      do isp=1,nspec
        vcollexp(isp+nspec*(ip_glob-1))=thearr(isp,ip)
      end do
      status(ip_glob)=1
    end do
    !
    ! now find global arrays
    !
    coherencyerror=0
    if (iaproc .eq. 1) then
      do iproc=2,naproc
        call mpi_recv(rvect  ,nspec*nx,mpi_double , iproc-1, 37, mpi_comm_wcmp, istatus, ierr)
        call mpi_recv(rstatus,nx      ,mpi_integer, iproc-1, 43, mpi_comm_wcmp, istatus, ierr)
        do i=1,nx
          if (rstatus(i) .eq. 1) then
            do isp=1,nspec
              eval1=vcollexp(isp+nspec*(i-1))
              eval2=rvect(isp+nspec*(i-1))
              vcollexp(isp+nspec*(i-1))=rvect(isp+nspec*(i-1))
              if (status(i) .eq. 1) then
                eerr=abs(eval1 - eval2)
                coherencyerror = coherencyerror + eerr
              else
                vcollexp(isp+nspec*(i-1))=eval2
              end if
            end do
            status(i)=1
          end if
        end do
      end do
    else
      call mpi_send(vcollexp,nspec*nx,mpi_double , 0, 37, mpi_comm_wcmp, ierr)
      call mpi_send(status  ,nx      ,mpi_integer, 0, 43, mpi_comm_wcmp, ierr)
    end if
    if (iaproc .eq. 1) then
      do i=1,nx
        do isp=1,nspec
          vcoll(isp,i)=vcollexp(isp + nspec*(i-1))
        end do
      end do
      open(fhndl, file=efile)
      do ix=1,nx
        esum=sum(vcoll(:,ix))
        write(fhndl,*) 'ix=', ix, 'esum=', esum
      end do
      close(fhndl)
    end if
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine write_var_to_text_file
  !/ ------------------------------------------------------------------- /
  subroutine printtotaloffcontrib(string)
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
    !  1. purpose : source code for parallel debugging
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
    use yownodepool,    only: pdlib_ccon, npa, pdlib_i_diag, pdlib_ja, pdlib_ia_p
    use w3gdatmd, only: nspec
    use w3odatmd, only : iaproc
    character(*), intent(in) :: string
    integer j, ip, jp, i, isp
    real thesum1, thesum2
    j = 0
    thesum1=0
    do ip = 1, npa
      do i = 1, pdlib_ccon(ip)
        j = j + 1
        if (j .ne. pdlib_i_diag(ip)) then
          do isp=1,nspec
            thesum1=thesum1 + abs(aspar_jac(isp,j))
          end do
        end if
      end do
    end do
    !
    thesum2=0
    do ip = 1, npa
      do i = pdlib_ia_p(ip)+1, pdlib_ia_p(ip+1)
        jp=pdlib_ja(i)
        if (jp .ne. ip) then
          do isp=1,nspec
            thesum2=thesum2 + abs(aspar_jac(isp,i))
          end do
        end if
      end do
    end do
    write(740+iaproc,'(a,f14.7,f14.7,a,a)') 'thesum12=', thesum1, thesum2, ' ', string
    flush(740+iaproc)
    !/
    !/ end of w3xypfsn --------------------------------------------------- /
    !/
  end subroutine printtotaloffcontrib
  !/ ------------------------------------------------------------------- /
  subroutine compute_mean_param (a, cg, wn, emean, fmean, wnmean, amax)
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
    !  1. purpose : compute mean prarameter
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
    use constants
    use w3gdatmd, only: nk, nth, sig, dden, fte, ftf, ftwn
    !
    real, intent(in)        :: a(nth,nk), cg(nk), wn(nk)
    real, intent(out)       :: emean, fmean, wnmean, amax
    integer                 :: ik, ith
    real                    :: eb(nk), eband
    !
    emean  = 0.
    fmean  = 0.
    wnmean = 0.
    amax   = 0.
    !
    ! 1.  integral over directions
    !
    do ik=1, nk
      eb(ik) = 0.
      do ith=1, nth
        eb(ik) = eb(ik) + a(ith,ik)
        amax   = max ( amax , a(ith,ik) )
      end do
    end do
    !
    ! 2.  integrate over directions
    !
    do ik=1, nk
      eb(ik) = eb(ik) * dden(ik) / cg(ik)
      emean  = emean  + eb(ik)
      fmean  = fmean  + eb(ik) / sig(ik)
      wnmean = wnmean + eb(ik) / sqrt(wn(ik))
    end do
    !
    ! 3.  add tail beyond discrete spectrum
    !     ( dth * sig absorbed in ftxx )
    !
    eband  = eb(nk) / dden(nk)
    emean  = emean  + eband * fte
    fmean  = fmean  + eband * ftf
    wnmean = wnmean + eband * ftwn
    !
    ! 4.  final processing
    !
    fmean  = tpiinv * emean / max ( 1.e-7 , fmean )
    wnmean = ( emean / max ( 1.e-7 , wnmean ) )**2
    !
    !
    return
    !
    ! formats
    !
    !/
    !/ end of w3spr0 ----------------------------------------------------- /
    !/
  end subroutine compute_mean_param
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi(dtg,facx,facy,vgx,vgy)
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
    !  1. purpose : compute matrix coefficients for advection part
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
    use w3gdatmd, only: nk, nk2, nth, nspec, fachfa, dmin
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3gdatmd, only: nseal, clats
    use w3gdatmd, only: mapsta
    use w3wdatmd, only: va
    use w3adatmd, only: cg, dw, wn, cx, cy
    use w3idatmd, only: flcur, fllev
    use w3gdatmd, only: ecos, esin, mapfs
    use w3parall, only : onesixth, zero, thr
    use yowelementpool, only: ne, ine
    use yownodepool,    only: pdlib_ien, pdlib_tria,                  &
         pdlib_ccon, pdlib_pos_cell2, pdlib_ie_cell2, np, npa,          &
         pdlib_ia_p, pdlib_posi, pdlib_ia, pdlib_nnz, iplg,           &
         pdlib_i_diag, pdlib_ja
    use w3odatmd, only : iaproc
    use w3parall, only : zero
    real, intent(in) :: dtg, facx, facy, vgx, vgy
    integer :: ip, isp, isea, ip_glob
    integer :: idx, is
    integer :: i, j, ith, ik, j2
    integer :: ie, pos, jsea
    integer :: i1, i2, i3, ni(3)
    integer :: counter
    real :: dtk, tmp3
    real :: lambda(2)
    real :: fl11, fl12
    real :: fl21, fl22
    real :: fl31, fl32
    real :: crfs(3), cxy(3,2)
    real :: kp(3,nspec,ne)
    real :: km(3), k(3)
    real :: k1, esi, evs, evd
    real :: eval1, eval2, eval3
    real :: deltal(3,nspec,ne)
    real :: nm(nspec,ne)
    real :: tria03, sidt, ccos, csin
    real :: spec(nspec), depth
    memunit = 50000+iaproc
    i      = 0
    ie     = 0
    pos    = 0
    i1     = 0
    i2     = 0
    i3     = 0
    dtk    = 0
    tmp3   = 0
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_jacobi section 0')
    do ie = 1, ne
      i1 = ine(1,ie)
      i2 = ine(2,ie)
      i3 = ine(3,ie)
      ni = ine(:,ie)
      do is = 1, nspec
        ith    = 1 + mod(is-1,nth)
        ik     = 1 + (is-1)/nth
        ccos   = facx * ecos(ith)
        csin   = facy * esin(ith)
        cxy(:,1) = ccos * cg(ik,ni) / clats(ni)
        cxy(:,2) = csin * cg(ik,ni)
        if (flcur) then
          cxy(:,1) = cxy(:,1) + facx * cx(ni)/clats(ni)
          cxy(:,2) = cxy(:,2) + facy * cy(ni)
        endif
        fl11 = cxy(2,1)*pdlib_ien(1,ie)+cxy(2,2)*pdlib_ien(2,ie)
        fl12 = cxy(3,1)*pdlib_ien(1,ie)+cxy(3,2)*pdlib_ien(2,ie)
        fl21 = cxy(3,1)*pdlib_ien(3,ie)+cxy(3,2)*pdlib_ien(4,ie)
        fl22 = cxy(1,1)*pdlib_ien(3,ie)+cxy(1,2)*pdlib_ien(4,ie)
        fl31 = cxy(1,1)*pdlib_ien(5,ie)+cxy(1,2)*pdlib_ien(6,ie)
        fl32 = cxy(2,1)*pdlib_ien(5,ie)+cxy(2,2)*pdlib_ien(6,ie)
        crfs(1) = - onesixth *  (2.0d0 *fl31 + fl32 + fl21 + 2.0d0 * fl22 )
        crfs(2) = - onesixth *  (2.0d0 *fl32 + 2.0d0 * fl11 + fl12 + fl31 )
        crfs(3) = - onesixth *  (2.0d0 *fl12 + 2.0d0 * fl21 + fl22 + fl11 )
        lambda(1) = onesixth * sum(cxy(:,1))
        lambda(2) = onesixth * sum(cxy(:,2))
        k(1)  = lambda(1) * pdlib_ien(1,ie) + lambda(2) * pdlib_ien(2,ie)
        k(2)  = lambda(1) * pdlib_ien(3,ie) + lambda(2) * pdlib_ien(4,ie)
        k(3)  = lambda(1) * pdlib_ien(5,ie) + lambda(2) * pdlib_ien(6,ie)
        kp(:,is,ie) = max(zero,k(:))
        deltal(:,is,ie) = crfs(:) - kp(:,is,ie)
        km(:) = min(zero,k(:))
        nm(is,ie) = 1.d0/min(-thr,sum(km))
      enddo
    end do
    j = 0
    do ip = 1, npa
      ip_glob=iplg(ip)
      do i = 1, pdlib_ccon(ip)
        j = j + 1
        ie    =  pdlib_ie_cell2(i,ip)
        pos   =  pdlib_pos_cell2(i,ip)
        i1    =  pdlib_posi(1,j)
        i2    =  pdlib_posi(2,j)
        i3    =  pdlib_posi(3,j)
        do isp=1,nspec
          ith    = 1 + mod(isp-1,nth)
          ik     = 1 + (isp-1)/nth
          k1    =  kp(pos,isp,ie)
          tria03        =  1./3. * pdlib_tria(ie)
          dtk           =  k1 * dtg * iobdp_loc(ip) * (1-iobpa_loc(ip)) * iobpd_loc(ith,ip)
          b_jac(isp,ip) =  b_jac(isp,ip) + tria03 * va(isp,ip) * iobdp_loc(ip) * (1-iobpa_loc(ip)) * iobpd_loc(ith,ip)
          tmp3          =  dtk * nm(isp,ie)
          if (fsgeoadvect) then
            aspar_jac(isp,i1) = aspar_jac(isp,i1) + tria03 + dtk - tmp3*deltal(pos,isp,ie)
            aspar_jac(isp,i2) = aspar_jac(isp,i2)                - tmp3*deltal(pos_trick(pos,1),isp,ie)
            aspar_jac(isp,i3) = aspar_jac(isp,i3)                - tmp3*deltal(pos_trick(pos,2),isp,ie)
          else
            aspar_jac(isp,i1) = aspar_jac(isp,i1) + tria03
          end if
        end do
      end do
    end do
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_jacobi section 1')
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine calcarray_jacobi
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi_vec(dtg,facx,facy,vgx,vgy)
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
    !  1. purpose : compute matrix coefficients for advection part
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
    use w3gdatmd, only: nk, nk2, nth, nspec, fachfa, dmin
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3gdatmd, only: nseal, clats
    use w3gdatmd, only: mapsta, sig
    use w3wdatmd, only: va
    use w3adatmd, only: cg, dw, wn, cx, cy
    use w3idatmd, only: flcur, fllev
    use w3gdatmd, only: ecos, esin, mapfs
    use w3parall, only : onesixth, zero, thr
    use yowelementpool, only: ne, ine
    use yownodepool,    only: pdlib_ien, pdlib_tria,                  &
         pdlib_ie_cell2, pdlib_pos_cell2, pdlib_ccon, np, npa,          &
         pdlib_ia_p, pdlib_posi, pdlib_ia, pdlib_nnz, iplg,           &
         pdlib_i_diag, pdlib_ja, pdlib_tria03, pdlib_si
    use w3odatmd, only : iaproc
    use w3parall, only : zero
    use w3dispmd, only : wavnu_local
    real, intent(in) :: dtg, facx, facy, vgx, vgy
    integer :: ip, isp, isea, ip_glob
    integer :: idx, is
    integer :: i, j, ith, ik, j2
    integer :: ie, pos, jsea
    integer :: i1, i2, i3, ni(3)
    integer :: counter, ib1, ib2, ibr
    real    :: dtk, tmp3
    real    :: lambda(2), cxyy(2,3), cxy(2,npa)
    real    :: fl11, fl12
    real    :: fl21, fl22
    real    :: fl31, fl32
    real    :: crfs(3), k(3)
    real    :: kp(3,ne)
    real    :: km(3), deltal(3,ne)
    real    :: k1, esi, evs, evd
    real    :: eval1, eval2, eval3
    real    :: cg1, wn1
    real    :: tria03, sidt, ccos, csin
    real    :: spec(nspec), depth, ccosa(nth), csina(nth)
    integer :: iobpth1(nth), iobpth2(nth)
    memunit = 50000+iaproc
    i      = 0
    ie     = 0
    pos    = 0
    i1     = 0
    i2     = 0
    i3     = 0
    dtk    = 0
    tmp3   = 0
    ccosa = facx * ecos(1:nth)
    csina = facx * esin(1:nth)
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_jacobi section 0')
    do isp = 1, nspec
      ith    = 1 + mod(isp-1,nth)
      ik     = 1 + (isp-1)/nth
      ccos   = ccosa(ith)
      csin   = csina(ith)
      do ip = 1, npa
        ip_glob = iplg(ip)
        cg1    = cg(ik,ip_glob)
        cxy(1,ip) = ccos * cg1/clats(ip_glob)
        cxy(2,ip) = csin * cg1
        if (flcur) then
          cxy(1,ip) = cxy(1,ip) + facx * cx(ip_glob)/clats(ip_glob)*iobdp_loc(ip)
          cxy(2,ip) = cxy(2,ip) + facy * cy(ip_glob)*iobdp_loc(ip)
        endif
      enddo
      do ie = 1, ne
        ni = ine(:,ie)
        cxyy(1,:) = cxy(1,ni)
        cxyy(2,:) = cxy(2,ni)
        fl11 = cxyy(1,2)*pdlib_ien(1,ie)+cxyy(2,2)*pdlib_ien(2,ie)
        fl12 = cxyy(1,3)*pdlib_ien(1,ie)+cxyy(2,3)*pdlib_ien(2,ie)
        fl21 = cxyy(1,3)*pdlib_ien(3,ie)+cxyy(2,3)*pdlib_ien(4,ie)
        fl22 = cxyy(1,1)*pdlib_ien(3,ie)+cxyy(2,1)*pdlib_ien(4,ie)
        fl31 = cxyy(1,1)*pdlib_ien(5,ie)+cxyy(2,1)*pdlib_ien(6,ie)
        fl32 = cxyy(1,2)*pdlib_ien(5,ie)+cxyy(2,2)*pdlib_ien(6,ie)
        crfs(1) = - onesixth *  (2.0d0 *fl31 + fl32 + fl21 + 2.0d0 * fl22 )
        crfs(2) = - onesixth *  (2.0d0 *fl32 + 2.0d0 * fl11 + fl12 + fl31 )
        crfs(3) = - onesixth *  (2.0d0 *fl12 + 2.0d0 * fl21 + fl22 + fl11 )
        lambda(1) = onesixth * sum(cxyy(1,:))
        lambda(2) = onesixth * sum(cxyy(2,:))
        k(1)  = lambda(1) * pdlib_ien(1,ie) + lambda(2) * pdlib_ien(2,ie)
        k(2)  = lambda(1) * pdlib_ien(3,ie) + lambda(2) * pdlib_ien(4,ie)
        k(3)  = lambda(1) * pdlib_ien(5,ie) + lambda(2) * pdlib_ien(6,ie)
        kp(1:3,ie) = max(zero,k(1:3))
        deltal(1:3,ie) = (crfs(1:3) - kp(1:3,ie)) * 1.d0/min(-thr,sum(min(zero,k(1:3))))
      enddo
      j = 0
      do ip = 1, np
        ib1 = (1-iobpa_loc(ip)) * iobpd_loc(ith,ip)
        ib2 = iobpd_loc(ith,ip)
        if (iobdp_loc(ip) .eq. 1) then
          do i = 1, pdlib_ccon(ip)
            j     =  j + 1
            ie    =  pdlib_ie_cell2(i,ip)
            pos   =  pdlib_pos_cell2(i,ip)
            dtk               = kp(pos,ie) * dtg * ib1
            b_jac(isp,ip)     = b_jac(isp,ip) + pdlib_tria03(ie) * va(isp,ip) * ib2
            i1  =  pdlib_posi(1,j)
            i2  =  pdlib_posi(2,j)
            i3  =  pdlib_posi(3,j)
            if (fsgeoadvect) then
              aspar_jac(isp,i1) = aspar_jac(isp,i1) + pdlib_tria03(ie) + dtk - dtk * deltal(pos,ie)
              aspar_jac(isp,i2) = aspar_jac(isp,i2)                          - dtk * deltal(pos_trick(pos,1),ie)
              aspar_jac(isp,i3) = aspar_jac(isp,i3)                          - dtk * deltal(pos_trick(pos,2),ie)
            else
              aspar_jac(isp,i1) = aspar_jac(isp,i1) + pdlib_tria03(ie)
            endif
          end do
        else
          do i = 1, pdlib_ccon(ip)
            j = j + 1
            i1                =  pdlib_posi(1,j)
            ie                =  pdlib_ie_cell2(i,ip)
            aspar_jac(isp,i1) = aspar_jac(isp,i1) + pdlib_tria03(ie)
          end do
          b_jac(isp,ip) = 0.
        endif
      end do
    end do ! isp
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_jacobi section 1')
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine calcarray_jacobi_vec
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi2(dtg,facx,facy,vgx,vgy)
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
    !  1. purpose : compute matrix coefficients for advection part
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
    use w3gdatmd, only: nk, nk2, nth, nspec, fachfa, dmin
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3gdatmd, only: nseal, clats
    use w3gdatmd, only: mapsta
    use w3wdatmd, only: va, vaold
    use w3adatmd, only: cg, dw, wn, cx, cy
    use w3idatmd, only: flcur, fllev
    use w3gdatmd, only: ecos, esin, mapfs
    use w3parall, only : onesixth, zero, thr, imem
    use yowelementpool, only: ne, ine
    use yownodepool,    only: pdlib_ien, pdlib_tria,                  &
         pdlib_ccon, pdlib_pos_cell2, pdlib_ie_cell2, np, npa,        &
         pdlib_ia_p, pdlib_posi, pdlib_ia, pdlib_nnz, iplg,           &
         pdlib_i_diag, pdlib_ja
    use w3odatmd, only : iaproc
    real, intent(in) :: dtg, facx, facy, vgx, vgy
    integer :: ip, isp, isea, ip_glob
    integer :: idx, is
    integer :: i, j, ith, ik, j2
    integer :: ie, pos, jsea
    integer :: i1, i2, i3, ni(3), ni_glob(3), ni_isea(3)
    integer :: counter
    integer :: ip1, ip2, ipp1, ipp2
    real  :: dtk, tmp3
    real  :: lambda(2)
    real  :: fl11, fl12
    real  :: fl21, fl22
    real  :: fl31, fl32
    real  :: crfs(3), k(3)
    real  :: kp(3)
    real  :: km(3), cxy(3,2)
    real  :: k1, esi, evs, evd
    real  :: eval1, eval2, eval3
    real  :: deltal(3)
    real  :: nm, tria03, sidt
    real  :: ien_local(6), cg2(nk,nth)
    real  :: ccos, csin
    real  :: spec(nspec), depth
    memunit = 50000+iaproc
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_jacobi section 0')
    j = 0
    do ip = 1, npa
      ip_glob=iplg(ip)
      isea=mapfs(1,ip_glob)
      do i = 1, pdlib_ccon(ip)
        j = j + 1
        ie    =  pdlib_ie_cell2(i,ip)
        ien_local = pdlib_ien(:,ie)
        pos   =  pdlib_pos_cell2(i,ip)
        i1    =  pdlib_posi(1,j)
        i2    =  pdlib_posi(2,j)
        i3    =  pdlib_posi(3,j)
        ip1   =  ine(pos_trick(pos,1),ie)
        ip2   =  ine(pos_trick(pos,2),ie)
        ipp1  =  pos_trick(pos,1)
        ipp2  =  pos_trick(pos,2)
        ni    = ine(:,ie)
        ni_glob = iplg(ni)
        ni_isea = mapfs(1,ni_glob)
        do isp=1,nspec
          ith    = 1 + mod(isp-1,nth)
          ik     = 1 + (isp-1)/nth
          ccos   = facx * ecos(ith)
          csin   = facy * esin(ith)
          cxy(:,1) = ccos * cg(ik,ni_isea) / clats(ni_isea)
          cxy(:,2) = csin * cg(ik,ni_isea)
          if (flcur) then
            cxy(:,1) = cxy(:,1) + facx * cx(ni_isea)/clats(ni_isea)
            cxy(:,2) = cxy(:,2) + facy * cy(ni_isea)
          endif
          fl11 = cxy(2,1)*ien_local(1)+cxy(2,2)*ien_local(2)
          fl12 = cxy(3,1)*ien_local(1)+cxy(3,2)*ien_local(2)
          fl21 = cxy(3,1)*ien_local(3)+cxy(3,2)*ien_local(4)
          fl22 = cxy(1,1)*ien_local(3)+cxy(1,2)*ien_local(4)
          fl31 = cxy(1,1)*ien_local(5)+cxy(1,2)*ien_local(6)
          fl32 = cxy(2,1)*ien_local(5)+cxy(2,2)*ien_local(6)
          crfs(1) = - onesixth *  (2.0d0 *fl31 + fl32 + fl21 + 2.0d0 * fl22 )
          crfs(2) = - onesixth *  (2.0d0 *fl32 + 2.0d0 * fl11 + fl12 + fl31 )
          crfs(3) = - onesixth *  (2.0d0 *fl12 + 2.0d0 * fl21 + fl22 + fl11 )
          lambda(1) = onesixth * sum(cxy(:,1))
          lambda(2) = onesixth * sum(cxy(:,2))
          k(1)  = lambda(1) * ien_local(1) + lambda(2) * ien_local(2)
          k(2)  = lambda(1) * ien_local(3) + lambda(2) * ien_local(4)
          k(3)  = lambda(1) * ien_local(5) + lambda(2) * ien_local(6)
          kp(:) = max(zero,k(:))
          deltal(:) = crfs(:) - kp(:)
          km(:) = min(zero,k(:))
          nm = 1.d0/min(-thr,sum(km))
          k1 =  kp(pos)
          tria03 = 1./3. * pdlib_tria(ie)
          dtk    =  k1 * dtg * iobdp_loc(ip) * iobpd_loc(ith,ip) * (1-iobpa_loc(ip))
          tmp3   =  dtk * nm
          if (fsgeoadvect) then
            aspar_jac(isp,i1) = aspar_jac(isp,i1) + tria03 + dtk - tmp3*deltal(pos)
            aspar_jac(isp,i2) = aspar_jac(isp,i2)                - tmp3*deltal(ipp1)
            aspar_jac(isp,i3) = aspar_jac(isp,i3)                - tmp3*deltal(ipp2)
          else
            aspar_jac(isp,i1) = aspar_jac(isp,i1) + tria03
          end if
          b_jac(isp,ip) = b_jac(isp,ip) + tria03 * va(isp,ip) * iobdp_loc(ip) * iobpd_loc(ith,ip)
        end do
      end do
    end do
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_jacobi section 1')
    !/
    !/ end of w3xypfsn ----------------------------------------------------- /
    !/
  end subroutine calcarray_jacobi2
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi3(ip,j,dtg,facx,facy,vgx,vgy,aspar_diag_local,aspar_off_diag_local,b_jac_local)
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
    !  1. purpose : compute matrix coefficients for advection part
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
    use w3gdatmd, only: nk, nk2, nth, nspec, fachfa, dmin
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3gdatmd, only: nseal, clats
    use w3gdatmd, only: mapsta
    use w3wdatmd, only: va, vaold
    use w3adatmd, only: cg, dw, wn, cx, cy
    use w3idatmd, only: flcur, fllev
    use w3gdatmd, only: ecos, esin, mapfs
    use w3parall, only : onesixth, zero, thr, onethird
    use yowelementpool, only: ne, ine
    use yownodepool,    only: pdlib_ien, pdlib_tria,                  &
         pdlib_ccon, np, npa, pdlib_pos_cell2, pdlib_ie_cell2,        &
         pdlib_ia_p, pdlib_posi, pdlib_ia, pdlib_nnz, iplg,           &
         pdlib_i_diag, pdlib_ja
    use w3gdatmd, only: iobp
    use w3odatmd, only : iaproc
    integer, intent(in) :: ip
    integer, intent(inout) :: j
    real, intent(in) :: dtg, facx, facy, vgx, vgy
    real, intent(out) :: aspar_diag_local(nspec), b_jac_local(nspec), aspar_off_diag_local(nspec)
    integer :: isp, isea, ip_glob, ipp1, ipp2
    integer :: idx, is, ip1, ip2
    integer :: i, ith, ik, j2
    integer :: ie, pos, jsea
    integer :: i1, i2, i3, ni(3), ni_glob(3), ni_isea(3)
    integer :: counter
    real :: dtk, tmp3
    real :: lambda(2)
    real :: fl11, fl12
    real :: fl21, fl22
    real :: fl31, fl32
    real :: crfs(3), k(3)
    real :: kp(3)
    real :: km(3), cxy(3,2)
    real :: k1, esi, evs, evd
    real :: eval1, eval2, eval3
    real :: ien_local(6)
    real :: deltal(3)
    real :: nm
    real :: tria03, sidt, ccos, csin
    real :: depth
    aspar_diag_local     = 0.d0
    b_jac_local          = 0.d0
    aspar_off_diag_local = 0.d0
    ip_glob=iplg(ip)
    do i = 1, pdlib_ccon(ip)
      j         = j + 1
      ie    =  pdlib_ie_cell2(i,ip)
      ien_local = pdlib_ien(:,ie)
      pos   =  pdlib_pos_cell2(i,ip)
      i1    =  pdlib_posi(1,j)
      i2    =  pdlib_posi(2,j)
      i3    =  pdlib_posi(3,j)
      ip1   =  ine(pos_trick(pos,1),ie)
      ip2   =  ine(pos_trick(pos,2),ie)
      ipp1  =  pos_trick(pos,1)
      ipp2  =  pos_trick(pos,2)
      ni    =  ine(:,ie)
      ni_glob = iplg(ni)
      ni_isea = mapfs(1,ni_glob)
      do isp=1,nspec
        ith    = 1 + mod(isp-1,nth)
        ik     = 1 + (isp-1)/nth
        ccos   = facx * ecos(ith)
        csin   = facy * esin(ith)
        cxy(:,1) = ccos * cg(ik,ni_isea) / clats(ni_isea)
        cxy(:,2) = csin * cg(ik,ni_isea)
        if (flcur) then
          cxy(:,1) = cxy(:,1) + facx * cx(ni_isea)/clats(ni_isea)
          cxy(:,2) = cxy(:,2) + facy * cy(ni_isea)
        endif
        fl11 = cxy(2,1)*ien_local(1)+cxy(2,2)*ien_local(2)
        fl12 = cxy(3,1)*ien_local(1)+cxy(3,2)*ien_local(2)
        fl21 = cxy(3,1)*ien_local(3)+cxy(3,2)*ien_local(4)
        fl22 = cxy(1,1)*ien_local(3)+cxy(1,2)*ien_local(4)
        fl31 = cxy(1,1)*ien_local(5)+cxy(1,2)*ien_local(6)
        fl32 = cxy(2,1)*ien_local(5)+cxy(2,2)*ien_local(6)
        crfs(1) = - onesixth * (2.0d0 *fl31 + fl32 + fl21 + 2.0d0 * fl22 )
        crfs(2) = - onesixth * (2.0d0 *fl32 + 2.0d0 * fl11 + fl12 + fl31 )
        crfs(3) = - onesixth * (2.0d0 *fl12 + 2.0d0 * fl21 + fl22 + fl11 )
        lambda(1) = onesixth * sum(cxy(:,1))
        lambda(2) = onesixth * sum(cxy(:,2))
        k(1)  = lambda(1) * ien_local(1) + lambda(2) * ien_local(2)
        k(2)  = lambda(1) * ien_local(3) + lambda(2) * ien_local(4)
        k(3)  = lambda(1) * ien_local(5) + lambda(2) * ien_local(6)
        kp(:) = max(zero,k(:))
        deltal(:) = crfs(:) - kp(:)
        km(:) = min(zero,k(:))
        nm = 1.d0/min(-thr,sum(km))
        tria03 = onethird * pdlib_tria(ie)
        dtk    =  kp(pos) * dble(dtg) * iobdp_loc(ip) * iobpd_loc(ith,ip) * (1-iobpa_loc(ip))
        tmp3   =  dtk * nm
        if (fsgeoadvect) then
          aspar_diag_local(isp)     = aspar_diag_local(isp)    + tria03 + dtk   - tmp3*deltal(pos)
          aspar_off_diag_local(isp) = aspar_off_diag_local(isp)                 - tmp3*deltal(ipp1)*va(isp,ip1)
          aspar_off_diag_local(isp) = aspar_off_diag_local(isp)                 - tmp3*deltal(ipp2)*va(isp,ip2)
        else
          aspar_diag_local(isp) = aspar_diag_local(isp) + tria03
        end if
        b_jac_local(isp) = b_jac_local(isp) + tria03 * vaold(isp,ip) * iobdp_loc(ip) * iobpd_loc(ith,ip)
      end do
    end do
    !/
    !/ end of w3xypfsn --------------------------------------------------- /
    !/
  end subroutine calcarray_jacobi3
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi4(ip,dtg,facx,facy,vgx,vgy,aspar_diag_local,aspar_off_diag_local,b_jac_local)
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
    !  1. purpose : compute matrix coefficients for advection part
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
    use w3gdatmd, only: nk, nk2, nth, nspec, fachfa, dmin
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3gdatmd, only: nseal,clats
    use w3gdatmd, only: mapsta, nk
    use w3wdatmd, only: va, vaold
    use w3adatmd, only: cg, dw, wn, cx, cy
    use w3idatmd, only: flcur, fllev
    use w3gdatmd, only: ecos, esin, mapfs
    use w3parall, only : onesixth, zero, thr, onethird
    use yowelementpool, only: ne, ine
    use yownodepool,    only: pdlib_ien, pdlib_tria,                  &
         pdlib_ie_cell2, pdlib_pos_cell2, pdlib_ccon, np, npa,          &
         pdlib_ia_p, pdlib_posi, pdlib_ia, pdlib_nnz, iplg,           &
         pdlib_i_diag, pdlib_ja
    use w3odatmd, only : iaproc
    integer, intent(in) :: ip
    real, intent(in) :: dtg, facx, facy, vgx, vgy
    real, intent(out) :: aspar_diag_local(nspec), b_jac_local(nspec), aspar_off_diag_local(nspec)
    !
    integer :: ip1, ip2
    integer :: ith, ik
    integer :: ie, pos, jsea
    integer :: i, i1, i2, i3, ni(3), ni_glob(3), ni_isea(3)
    integer :: isp, ip_glob, ipp1, ipp2, iobpth1(nth), iobpth2(nth)
    integer :: counter
    real  :: dtk, tmp3, d1, d2
    real  :: lambda(2)
    real  :: crfs(3), k(3)
    real  :: kp(3), uv_cur(3,2)
    real  :: km(3), csx(3), csy(3)
    real  :: k1, esi, evs, evd
    real  :: eval1, eval2, eval3
    real  :: ien_local(6)
    real  :: deltal(3), k_x(3,nk), k_y(3,nk), k_u(3)
    real  :: crfs_x(3,nk), crfs_y(3,nk), crfs_u(3)
    real  :: nm, cgfak(3,nk), csina(nth), ccosa(nth)
    real  :: tria03, sidt, ccos, csin
    real  :: fl11_x, fl12_x, fl21_x, fl22_x, fl31_x, fl32_x
    real  :: fl11_y, fl12_y, fl21_y, fl22_y, fl31_y, fl32_y
    real  :: fl11_u, fl12_u, fl21_u, fl22_u, fl31_u, fl32_u
    ip_glob              = iplg(ip)
    aspar_diag_local     = zero
    b_jac_local          = zero
    aspar_off_diag_local = zero
    do ith = 1, nth
      ccosa(ith) = facx * ecos(ith)
      csina(ith) = facx * esin(ith)
      iobpth1(ith) = iobdp_loc(ip) * (1-iobpa_loc(ip)) * iobpd_loc(ith,ip)
      iobpth2(ith) = iobdp_loc(ip) * iobpd_loc(ith,ip)
    enddo
    do i = 1, pdlib_ccon(ip)
      ie        = pdlib_ie_cell2(i,ip)
      tria03    = onethird * pdlib_tria(ie)
      ien_local = pdlib_ien(1:6,ie)
      pos       = pdlib_pos_cell2(i,ip)
      ip1       = ine(pos_trick(pos,1),ie)
      ip2       = ine(pos_trick(pos,2),ie)
      ipp1      = pos_trick(pos,1)
      ipp2      = pos_trick(pos,2)
      ni        = ine(1:3,ie)
      ni_glob   = iplg(ni)
      ni_isea   = mapfs(1,ni_glob)
      crfs_u    = zero
      if (flcur) then
        uv_cur(1:3,1) = facx * cx(ni_isea) / clats(ni_isea)
        uv_cur(1:3,2) = facy * cy(ni_isea)
        lambda(1) = onesixth*(uv_cur(1,1)+uv_cur(2,1)+uv_cur(3,1))
        lambda(2) = onesixth*(uv_cur(1,2)+uv_cur(2,2)+uv_cur(3,2))
        k_u(1)  = lambda(1) * ien_local(1) + lambda(2) * ien_local(2)
        k_u(2)  = lambda(1) * ien_local(3) + lambda(2) * ien_local(4)
        k_u(3)  = lambda(1) * ien_local(5) + lambda(2) * ien_local(6)
        fl11_u  = uv_cur(2,1)*ien_local(1)+uv_cur(2,2)*ien_local(2)
        fl12_u  = uv_cur(3,1)*ien_local(1)+uv_cur(3,2)*ien_local(2)
        fl21_u  = uv_cur(3,1)*ien_local(3)+uv_cur(3,2)*ien_local(4)
        fl22_u  = uv_cur(1,1)*ien_local(3)+uv_cur(1,2)*ien_local(4)
        fl31_u  = uv_cur(1,1)*ien_local(5)+uv_cur(1,2)*ien_local(6)
        fl32_u  = uv_cur(2,1)*ien_local(5)+uv_cur(2,2)*ien_local(6)
        crfs_u(1) = - onesixth*(2.d0 *fl31_u + fl32_u + fl21_u + 2.d0 * fl22_u)
        crfs_u(2) = - onesixth*(2.d0 *fl32_u + 2.d0 * fl11_u + fl12_u + fl31_u)
        crfs_u(3) = - onesixth*(2.d0 *fl12_u + 2.d0 * fl21_u + fl22_u + fl11_u)
      endif
      do ik = 1, nk
        csx = cg(ik,ni_isea) / clats(ni_isea)
        csy = cg(ik,ni_isea)
        lambda(1) = onesixth * (csx(1) + csx(2) + csx(3))
        lambda(2) = onesixth * (csy(1) + csy(2) + csy(3))
        k_x(1,ik) = lambda(1) * ien_local(1)
        k_x(2,ik) = lambda(1) * ien_local(3)
        k_x(3,ik) = lambda(1) * ien_local(5)
        k_y(1,ik) = lambda(2) * ien_local(2)
        k_y(2,ik) = lambda(2) * ien_local(4)
        k_y(3,ik) = lambda(2) * ien_local(6)
        fl11_x = csx(2) * ien_local(1)
        fl12_x = csx(3) * ien_local(1)
        fl21_x = csx(3) * ien_local(3)
        fl22_x = csx(1) * ien_local(3)
        fl31_x = csx(1) * ien_local(5)
        fl32_x = csx(2) * ien_local(5)
        fl11_y = csy(2) * ien_local(2)
        fl12_y = csy(3) * ien_local(2)
        fl21_y = csy(3) * ien_local(4)
        fl22_y = csy(1) * ien_local(4)
        fl31_y = csy(1) * ien_local(6)
        fl32_y = csy(2) * ien_local(6)
        crfs_x(1,ik) = - onesixth*(2.d0*fl31_x + fl32_x + fl21_x + 2.d0 * fl22_x)
        crfs_x(2,ik) = - onesixth*(2.d0*fl32_x + 2.d0 * fl11_x + fl12_x + fl31_x)
        crfs_x(3,ik) = - onesixth*(2.d0*fl12_x + 2.d0 * fl21_x + fl22_x + fl11_x)
        crfs_y(1,ik) = - onesixth*(2.d0*fl31_y + fl32_y + fl21_y + 2.d0 * fl22_y)
        crfs_y(2,ik) = - onesixth*(2.d0*fl32_y + 2.d0 * fl11_y + fl12_y + fl31_y)
        crfs_y(3,ik) = - onesixth*(2.d0*fl12_y + 2.d0 * fl21_y + fl22_y + fl11_y)
      enddo
      do isp = 1, nspec
        ith     = 1 + mod(isp-1,nth)
        ik      = 1 + (isp-1)/nth
        k(1)     = k_x(1,ik) * ccosa(ith) + k_y(1,ik) * csina(ith) + k_u(1)
        k(2)     = k_x(2,ik) * ccosa(ith) + k_y(2,ik) * csina(ith) + k_u(2)
        k(3)     = k_x(3,ik) * ccosa(ith) + k_y(3,ik) * csina(ith) + k_u(3)
        crfs(1)  = crfs_x(1,ik) * ccosa(ith) + crfs_y(1,ik) * csina(ith) + crfs_u(1)
        crfs(2)  = crfs_x(2,ik) * ccosa(ith) + crfs_y(2,ik) * csina(ith) + crfs_u(2)
        crfs(3)  = crfs_x(3,ik) * ccosa(ith) + crfs_y(3,ik) * csina(ith) + crfs_u(3)
        !km      = min(zero,k)
        kp(1:3)  = max(zero,k(1:3))
        deltal(1:3) = crfs(1:3) - kp(1:3)
        !nm      = 1.d0/min(-thr,sum(min(zero,k)))
        dtk     = kp(pos) * dtg * iobpth1(ith)!iobdp(ip_glob) * (1-iobpa(ip_glob)) * iobpd(ith,ip_glob)
        tmp3    = dtk * 1.d0/min(-thr,sum(min(zero,k(1:3))))
        if (fsgeoadvect) then
          aspar_diag_local(isp)     = aspar_diag_local(isp) + tria03 + dtk - tmp3*deltal(pos)
          d1                        = deltal(ipp1)*va(isp,ip1)
          d2                        = deltal(ipp2)*va(isp,ip2)
          aspar_off_diag_local(isp) = aspar_off_diag_local(isp) - ( tmp3 * ( d1 + d2 )  )
          !aspar_off_diag_local(isp) = aspar_off_diag_local(isp)            - d2
        else
          aspar_diag_local(isp)     = aspar_diag_local(isp) + tria03
        end if
        b_jac_local(isp) = b_jac_local(isp) + tria03 * vaold(isp,ip) * iobpth2(ith)!iobdp(ip_glob) * iobpd(ith,ip_glob)
      end do
    end do
  end subroutine calcarray_jacobi4
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi_spectral_1(dtg)
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
    !  1. purpose : compute matrix coefficients for spectral part
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
    use w3gdatmd, only: fsrefraction, fsfreqshift, fachfa
    use w3odatmd, only : iaproc
    use yownodepool, only: np, iplg, pdlib_si, pdlib_i_diag
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3idatmd, only: fllev, flcur
    use w3gdatmd, only: nk, nk2, nth, nspec, mapfs, dmin, dsip, nseal
    use w3parall, only : prop_refraction_pr3, prop_refraction_pr1, prop_freq_shift, prop_freq_shift_m2, zero, imem
    use w3adatmd, only: cg, dw
    real, intent(in) :: dtg
    integer ip, ip_glob, ith, ik
    integer isea, isp
    real ::  esi
    real  :: b_sig(nspec), b_the(nspec)
    real  :: cp_sig(nspec), cm_sig(nspec)
    real  :: cp_the(nspec), cm_the(nspec)
    real  :: cad(nspec), cas(nspec)
    real  :: dmm(0:nk2), eval
    real  :: dwni_m2(nk), cwnb_m2(1-nth:nspec)
    logical :: dolimiterrefraction = .false.
    logical :: dolimiterfreqshit   = .false. !ar: this one is missing ...
    integer :: ith0
    logical :: lsig = .false.
    !ar: todo: check&report if needed ...
    lsig = flcur .or. fllev
    do ip = 1, np
      ip_glob=iplg(ip)
      isea=mapfs(1,ip_glob)
      esi=pdlib_si(ip)
      if (fsfreqshift .and. lsig) then
        if (freqshiftmethod .eq. 1) then
          if (iobp_loc(ip).eq.1.and.iobdp_loc(ip).eq.1.and.iobpa_loc(ip).eq.0) then
            call prop_freq_shift(ip, isea, cas, dmm, dtg)
            cp_sig = max(zero,cas)
            cm_sig = min(zero,cas)
            b_sig=0
            do ith=1,nth
              do ik=1,nk
                isp=ith + (ik-1)*nth
                b_sig(isp)= cp_sig(isp)/dmm(ik-1) - cm_sig(isp)/dmm(ik)
              end do
              isp  = ith + (nk-1)*nth
              b_sig(isp)= b_sig(isp) + cm_sig(isp)/dmm(nk) * fachfa
            end do
            aspar_jac(:,pdlib_i_diag(ip))=aspar_jac(:,pdlib_i_diag(ip)) + b_sig(:)*esi
          else
            cas=0
          end if
          cas_sig(:,ip) = cas
        else if (freqshiftmethod .eq. 2) then
          if (iobp_loc(ip).eq.1.and.iobdp_loc(ip).eq.1.and.iobpa_loc(ip).eq.0) then
            call prop_freq_shift_m2(ip, isea, cwnb_m2, dwni_m2, dtg)
            do ith=1,nth
              do ik=1,nk
                isp = ith + (ik-1)*nth
                eval = dwni_m2(ik) * ( min(cwnb_m2(isp - nth), zero) - max(cwnb_m2(isp),zero) )
                aspar_jac(isp,pdlib_i_diag(ip)) =  aspar_jac(isp,pdlib_i_diag(ip)) - esi * eval
              end do
              eval = dwni_m2(nk) * min(cwnb_m2(ith + (nk-1)*nth), zero) * fachfa
              ith0 = nspec - nth
              aspar_jac(ith0 + ith,pdlib_i_diag(ip)) = aspar_jac(ith0 + ith,pdlib_i_diag(ip)) + esi * eval
            end do
          else
            cwnb_m2 = 0
          end if
          cwnb_sig_m2(:,ip)=cwnb_m2
        end if
      end if
      !
      ! the refraction
      !
      if (fsrefraction) then
        if (iobp_loc(ip) .eq. 1 .and. iobdp_loc(ip).eq.1.and.iobpa_loc(ip).eq.0) then
          !    call prop_refraction_pr1(isea,dtg,cad) !ar: check statuts ...
          !    call prop_refraction_pr3(isea,dtg,cad, dolimiterrefraction)
          call prop_refraction_pr3(ip,isea,dtg,cad,dolimiterrefraction)
        else
          cad=zero
        end if
        cad_the(:,ip)=cad
        cp_the = dtg*max(zero,cad)
        cm_the = dtg*min(zero,cad)
        b_the(:) = cp_the(:) - cm_the(:)
        aspar_jac(:,pdlib_i_diag(ip))=aspar_jac(:,pdlib_i_diag(ip)) + b_the(:)*esi
      end if
    end do
  end subroutine calcarray_jacobi_spectral_1
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi_spectral_2(dtg,aspar_diag_local)
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
    !  1. purpose : compute matrix coefficients for spectral part
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
    !/ ------------------------------------------------------------------- /
    !
    use w3gdatmd, only: fsrefraction, fsfreqshift, fachfa
    use w3odatmd, only : iaproc
    use yownodepool, only: np, iplg, pdlib_si, pdlib_i_diag
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3idatmd, only: fllev, flcur
    use w3gdatmd, only: nk, nk2, nth, nspec, mapfs, dmin, dsip, nseal, mapsta
    use w3parall, only : prop_refraction_pr3, prop_refraction_pr1, prop_freq_shift, prop_freq_shift_m2, zero, imem
    use w3adatmd, only: cg, dw
    real, intent(in) :: dtg
    real, intent(inout) :: aspar_diag_local(nspec,nseal)
    integer ip, ip_glob, ith, ik
    integer isea, isp
    real ::  esi
    real  :: b_sig(nspec), b_the(nspec)
    real  :: cp_sig(nspec), cm_sig(nspec)
    real  :: cp_the(nspec), cm_the(nspec)
    real  :: cad(nspec), cas(nspec)
    real  :: dmm(0:nk2), eval
    real  :: dwni_m2(nk), cwnb_m2(1-nth:nspec)
    logical :: dolimiterrefraction = .false.
    logical :: dolimiterfreqshit   = .false. !ar: this one is missing ...
    integer :: ith0
    logical :: lsig = .false.
    lsig = flcur .or. fllev
    do ip = 1, np
      ip_glob=iplg(ip)
      isea=mapfs(1,ip_glob)
      esi=pdlib_si(ip)
      !
      ! the frequency shifting
      !
      if (fsfreqshift .and. lsig) then
        if (freqshiftmethod .eq. 1) then
          if (iobp_loc(ip).eq.1.and.iobdp_loc(ip).eq.1.and.iobpa_loc(ip).eq.0) then
            call prop_freq_shift(ip, isea, cas, dmm, dtg)
            cp_sig = max(zero,cas)
            cm_sig = min(zero,cas)
            b_sig=0
            do ith=1,nth
              do ik=1,nk
                isp=ith + (ik-1)*nth
                b_sig(isp)= cp_sig(isp)/dmm(ik-1) - cm_sig(isp)/dmm(ik)
              end do
              isp  = ith + (nk-1)*nth
              b_sig(isp)= b_sig(isp) + cm_sig(isp)/dmm(nk) * fachfa
            end do
            aspar_diag_local(:,ip) = aspar_diag_local(:,ip) + b_sig * esi
          else
            cas = 0
          end if
          cas_sig(:,ip) = cas
        end if
        if (freqshiftmethod .eq. 2) then
          if (iobp_loc(ip).eq.1) then
            call prop_freq_shift_m2(ip, isea, cwnb_m2, dwni_m2, dtg)
            do ith=1,nth
              do ik=1,nk
                isp = ith + (ik-1)*nth
                eval = dwni_m2(ik) * ( min(cwnb_m2(isp - nth), zero) - max(cwnb_m2(isp),zero) )
                if (imem == 1) then
                  aspar_jac(isp,pdlib_i_diag(ip)) =  aspar_jac(isp,pdlib_i_diag(ip)) - esi * eval
                else if (imem == 2) then
                  aspar_diag_local(isp,ip) =  aspar_diag_local(isp,ip) - esi * eval
                endif
              end do
              eval = dwni_m2(nk) * min(cwnb_m2(ith + (nk-1)*nth), zero) * fachfa
              ith0 = nspec - nth
              aspar_diag_local(ith0 + ith,ip) = aspar_diag_local(ith0 + ith,ip) + esi * eval
            end do
          else
            cwnb_m2=0
          end if
          cwnb_sig_m2(:,ip)=cwnb_m2
        end if
      end if
      !
      if (fsrefraction) then
        if (iobp_loc(ip) .eq. 1.and.iobdp_loc(ip).eq.1.and.iobpa_loc(ip).eq.0) then
          !    call prop_refraction_pr1(isea,dtg,cad) !ar: is this working?
          !    call prop_refraction_pr3(isea,dtg,cad, dolimiterrefraction)
          call prop_refraction_pr3(ip,isea,dtg,cad,dolimiterrefraction)
        else
          cad=zero
        end if
        cad_the(:,ip)=cad
        cp_the = dtg*max(zero,cad)
        cm_the = dtg*min(zero,cad)
        b_the(:) = cp_the(:) - cm_the(:)
        aspar_diag_local(:,ip) = aspar_diag_local(:,ip) + b_the(:)*esi
      end if
    end do
  end subroutine calcarray_jacobi_spectral_2
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi_source_1(dtg)
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
    !  1. purpose : compute matrix coefficients for source part
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
    use w3odatmd, only : iaproc
    use yownodepool, only: iplg, pdlib_si, pdlib_i_diag, npa, np
    use w3adatmd, only: cg, dw, wn
    use w3wdatmd, only: ust, ustdir
    use w3gdatmd, only: nk, nth, nspec, mapfs, optioncall, dmin
    use w3gdatmd, only: mapsta, facp, sig
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3parall, only: imem
    use w3gdatmd, only: nseal, clats
    use w3wdatmd, only: va, vstot, vdtot, shavetot
    use constants, only : tpi, tpiinv, grav
    real, intent(in) :: dtg
    real, parameter :: coef4 = 5.0e-07
    real, parameter :: facdam = 1
    integer jsea, ip, ip_glob, isea
    integer ik, ith, isp, is0
    logical :: lbreak
    real ::  esi, evs, evd, sidt
    real :: depth, dam(nspec), ratio, maxdac, vsdb(nspec), vddb(nspec)
    real :: prevs, edam, dvs, freq, emean, fmean, wnmean, amax, cg1(nk),wn1(nk),spec_va(nspec)
    real thefactor
    do jsea = 1, np
      ip      = jsea
      ip_glob = iplg(ip)
      isea    = mapfs(1,ip_glob)
      if ((iobp_loc(ip).eq.1..or.iobp_loc(jsea).eq. 3).and.iobdp_loc(ip).eq.1.and.iobpa_loc(ip).eq.0) then
        do ik=1, nk
          dam(1+(ik-1)*nth) = facp / ( sig(ik) * wn(ik,isea)**3 )
        end do
        do ik=1, nk
          is0    = (ik-1)*nth
          do ith=2, nth
            dam(ith+is0) = dam(1+is0)
          end do
        end do
        esi    = pdlib_si(ip)
        sidt   = esi * dtg
        depth  = dw(isea)
        do ik=1,nk
          do ith=1,nth
            isp=ith + (ik-1)*nth
            if (shavetot(jsea)) then ! limit only the source term part ...
              maxdac    = facdam * dam(isp)
              thefactor = dtg / max ( 1. , (1.-dtg*vdtot(isp,jsea)))
              dvs       = vstot(isp,jsea) * thefactor
              dvs       = sign(min(maxdac,abs(dvs)),dvs)
              prevs     = dvs / thefactor
            else
              prevs     = vstot(isp,jsea)
            end if
            evs = prevs * clats(isea) / cg(ik,isea)
            evd = dble(vdtot(isp,jsea))
            b_jac(isp,ip)                   = b_jac(isp,ip) + sidt * (evs - evd*va(isp,jsea))
            aspar_jac(isp,pdlib_i_diag(ip)) = aspar_jac(isp,pdlib_i_diag(ip)) - sidt * evd
          end do
        end do
      end if
    end do
  end subroutine calcarray_jacobi_source_1
  !/ ------------------------------------------------------------------- /
  subroutine calcarray_jacobi_source_2(dtg,aspar_diag_local)
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
    !  1. purpose : compute matrix coefficients for source part
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
    use w3odatmd, only : iaproc
    use yownodepool, only: iplg, pdlib_si, pdlib_i_diag, npa, np
    use w3adatmd, only: cg, dw, wn
    use w3wdatmd, only: ust, ustdir
    use w3gdatmd, only: nk, nth, nspec, mapfs, optioncall, dmin
    use w3gdatmd, only: iobp, mapsta, facp, sig, iobpd, iobpa, iobdp
    use w3parall, only: imem
    use w3gdatmd, only: nseal, clats
    use w3wdatmd, only: va, vstot, vdtot, shavetot
    use constants, only : tpi, tpiinv, grav
    real, intent(in) :: dtg
    real, intent(inout) :: aspar_diag_local(:,:)
    real, parameter :: coef4 = 5.0e-07
    real, parameter :: facdam = 1
    integer jsea, ip, ip_glob, isea
    integer ik, ith, isp, is0
    logical :: lbreak
    real ::  esi, evs, evd, sidt
    real :: depth, dam(nspec), ratio, maxdac, vsdb(nspec), vddb(nspec)
    real :: prevs, edam, dvs, freq, emean, fmean, wnmean, amax, cg1(nk),wn1(nk),spec_va(nspec)
    real thefactor
    do jsea = 1, np
      ip      = jsea
      ip_glob = iplg(ip)
      isea    = mapfs(1,ip_glob)
      if (iobp(ip_glob).eq.1..and.iobdp(ip_glob).eq.1.and.iobpa(ip_glob).eq.0) then
        do ik=1, nk
          dam(1+(ik-1)*nth) = facp / ( sig(ik) * wn(ik,isea)**3 )
        end do
        do ik=1, nk
          is0    = (ik-1)*nth
          do ith=2, nth
            dam(ith+is0) = dam(1+is0)
          end do
        end do
        esi    = pdlib_si(ip)
        sidt   = esi * dtg
        depth  = dw(isea)
        do ik=1,nk
          do ith=1,nth
            isp=ith + (ik-1)*nth
            if (shavetot(jsea)) then ! limit only the source term part ...
              maxdac    = facdam * dam(isp)
              thefactor = dtg / max ( 1. , (1.-dtg*vdtot(isp,jsea)))
              dvs       = vstot(isp,jsea) * thefactor
              dvs       = sign(min(maxdac,abs(dvs)),dvs)
              prevs     = dvs / thefactor
            else
              prevs     = vstot(isp,jsea)
            end if
            evs = prevs / cg(ik,isea) * clats(isea)
            evd = dble(vdtot(isp,jsea))
            b_jac(isp,ip)                   = b_jac(isp,ip) + sidt * (evs - evd*va(isp,jsea))
            aspar_diag_local(isp,ip) = aspar_diag_local(isp,ip) - sidt * evd
          end do
        end do
      end if
    end do
  end subroutine calcarray_jacobi_source_2
  !/ ------------------------------------------------------------------- /
  subroutine apply_boundary_condition_va
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
    !  1. purpose : boudary conditions on va
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
    use yowrankmodule, only : ipgl_npa
    use w3gdatmd, only: nseal, clats, gtype, ungtype
    use w3wdatmd, only: time
    use w3timemd, only: dsec21
    use w3adatmd, only: cg, cx, cy
    use w3wdatmd, only: va
    use w3gdatmd, only: nk, nk2, nth, ecos, esin, nspec
    use w3odatmd, only: tbpi0, tbpin, flbpi, iaproc, naproc, bbpi0, bbpin, isbpi, nbi
    use w3parall, only : isea_to_jsea
    !/
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
    real    :: rd1, rd2, rd10, rd20
    real    :: eva, eac
    integer :: ik, ith, isea, jsea
    integer :: ibi, isp
    if (gtype .eq. ungtype) then
      if ( flbpi ) then
        rd10    = dsec21 ( tbpi0, time )
        rd20    = dsec21 ( tbpi0, tbpin )
      else
        rd10=1.
        rd20=0.
      end if
      if (flbpi .and. (iaproc .le. naproc)) then
        rd1=rd10 ! i am not completely sure about that
        rd2=rd20
        if ( rd2 .gt. 0.001 ) then
          rd2    = min(1.,max(0.,rd1/rd2))
          rd1    = 1. - rd2
        else
          rd1    = 0.
          rd2    = 1.
        end if
        do ibi=1, nbi
          isea=isbpi(ibi)
          jsea=isea_to_jsea(isea)
          if (jsea .gt. 0) then
            do ith=1,nth
              do ik=1,nk
                isp=ith + (ik-1)*nth
                eac = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )   &
                     / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))
                eva = max(0., cg(ik,isea)/clats(isea)*eac)
                va(isp,jsea) = eva
              end do
            end do
          end if
        end do
      end if
    end if
  end subroutine apply_boundary_condition_va
  !/ ------------------------------------------------------------------- /
  subroutine apply_boundary_condition(imod)
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
    !  1. purpose : apply boundary conditions
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
    use yownodepool, only: npa, np
    use yowrankmodule, only : ipgl_npa
    use w3gdatmd, only: nseal, clats, mapsf
    use w3wdatmd, only: time
    use w3timemd, only: dsec21
    use w3wdatmd, only : va
    use w3adatmd, only: cg, cx, cy
    use w3gdatmd, only: nk, nk2, nth, nspec
    use w3odatmd, only: tbpi0, tbpin, flbpi, iaproc, bbpi0, bbpin, isbpi, nbi
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobdp_loc, iobpa_loc
    !/
    integer, intent(in) :: imod
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
    real    :: rd1, rd2, rd10, rd20
    integer :: ik, ith, isea
    integer :: ibi, ip_glob, isp, jx
    if ( flbpi ) then
      rd10    = dsec21 ( tbpi0, time )
      rd20    = dsec21 ( tbpi0, tbpin )
    else
      rd10=1.
      rd20=0.
    end if
    if ( flbpi ) then
      rd1=rd10 ! i am not completely sure about that
      rd2=rd20
      if ( rd2 .gt. 0.001 ) then
        rd2    = min(1.,max(0.,rd1/rd2))
        rd1    = 1. - rd2
      else
        rd1    = 0.
        rd2    = 1.
      end if
      do ibi=1, nbi
        isea    = isbpi(ibi)
        ip_glob = mapsf(isea,1)
        jx      = ipgl_npa(ip_glob)
        if (jx .gt. 0) then
          do ith=1,nth
            do ik=1,nk
              isp=ith + (ik-1)*nth
              va(isp,jx) = (( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) )  &
                   / cg(ik,isbpi(ibi)) * clats(isbpi(ibi))) * iobdp_loc(jx)
            end do
          end do
        end if
      enddo
    end if
  end subroutine apply_boundary_condition
  !/ ------------------------------------------------------------------- /
  subroutine action_limiter_local(ip,acloc,acold, dtg)
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
    !  1. purpose : computation of the limiter function
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
    use yownodepool, only: iplg
    use constants, only : grav, tpi
    use w3adatmd, only : wn, cg
    use w3gdatmd, only : nth, nk, nspec, mapfs, sig, facp
    !/
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
    integer, intent(in) :: ip
    real, intent(in) :: acold(nspec)
    real, intent(inout) :: acloc(nspec)
    real, intent(in) :: dtg
    integer :: melim = 1
    real :: limfak = 0.1
    real :: const, snd, ewn, ewk, ewkpow
    real :: efact, espsig
    real :: newval
    real :: oldac, newac, newdac
    real :: maxdac
    real :: dac, limac, edam
    integer ip_glob, isea
    integer :: ik, ith, isp
    logical :: llimiter_wwm
    ip_glob=iplg(ip)
    isea=mapfs(1,ip_glob)
    espsig=sig(nk)
    const = tpi**2*3.0*1.0e-7*dtg*espsig
    snd   = tpi*5.6*1.0e-3
    llimiter_wwm = .false.
    if (llimiter_wwm) then
      maxdac = 0
      do ik=1,nk
        if (melim .eq. 1) then
          efact=2.*sig(ik)
          ewn=wn(ik,isea)
          ewk=ewn
          ewkpow=ewk**3
          maxdac = dble(0.0081*limfak/(efact*ewkpow*cg(ik,isea)))
        end if
        do ith=1,nth
          isp=ith + (ik-1)*nth
          newac  = acloc(isp)
          oldac  = acold(isp)
          newdac = newac - oldac
          newdac = sign(min(maxdac,abs(newdac)), newdac)
          newval = max(0., oldac + newdac )
          acloc(isp) = newval
        end do
      end do
    else
      do ik = 1, nk
        edam=dble(facp / (sig(ik) * wn(ik,isea)**3))
        do ith=1,nth
          isp = ith + (ik-1)*nth
          dac = acloc(isp) - acold(isp)
          limac = sign (min(edam,abs(dac)),dac)
          acloc(isp) = max(0., acloc(isp) + limac)
        end do
      end do
    endif
  end subroutine action_limiter_local
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_jacobi_gauss_seidel_block(imod, facx, facy, dtg, vgx, vgy, lcalc)
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
    !  1. purpose : block gauss seidel and jacobi solver
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
    !/
    use constants, only : tpi, tpiinv, grav
    use w3gdatmd, only: mapsta
    use w3gdatmd, only: fsrefraction, fsfreqshift, fssource, nx, dsip
    use w3gdatmd, only: b_jgs_norm_thr, b_jgs_terminate_norm, b_jgs_pmin
    use w3gdatmd, only: b_jgs_terminate_difference, b_jgs_maxiter, b_jgs_limiter
    use w3gdatmd, only: b_jgs_terminate_maxiter, b_jgs_block_gauss_seidel, b_jgs_diff_thr
    use w3gdatmd, only: mapwn
    use yownodepool, only: pdlib_i_diag, pdlib_ia_p, pdlib_ja, np
    use yownodepool, only: pdlib_si, pdlib_nnz, pdlib_ccon
    use yowdatapool, only: rtype
    use yownodepool, only: npa, iplg
    use yowexchangemodule, only : pdlib_exchange2dreal_zero, pdlib_exchange2dreal
    use mpi, only : mpi_sum, mpi_int
    use w3adatmd, only: mpi_comm_wcmp
    use w3gdatmd, only: nsea, sig, facp, flsou
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobdp_loc, iobpa_loc
    use w3gdatmd, only: nk, nk2, nth, ecos, esin, nspec, mapfs, nsea, sig
    use w3wdatmd, only: time
    use w3odatmd, only: nbi
    use w3timemd, only: dsec21
    use w3gdatmd, only: nseal, clats, fachfa
    use w3idatmd, only: flcur, fllev
    use w3wdatmd, only: va, vaold, vstot, vdtot, ust
    use w3adatmd, only: cg, cx, cy, wn, dw
    use w3odatmd, only: tbpin, flbpi, iaproc
    use w3parall, only : imem
    use w3parall, only : init_get_jsea_isproc, zero, thr8, lsloc
    use w3parall, only : listispprevdir, listispnextdir
    use w3parall, only : jx_to_jsea
    use w3gdatmd, only: b_jgs_nlevel, b_jgs_source_nonlinear
    use yowfunction, only : pdlib_abort
    use yownodepool, only: np_global
    use w3dispmd, only : wavnu_local
    use w3adatmd, only: u10, u10d
    implicit none
    logical, intent(in) :: lcalc
    integer, intent(in) :: imod
    real, intent(in) :: facx, facy, dtg, vgx, vgy
    !
    integer :: ip, isp, ith, ik, jsea, isea, ip_glob, is0
    integer :: myrank
    integer :: nbiter, ispnextdir, ispprevdir
    integer :: ispp1, ispm1, jp, icount1, icount2
    ! for the exchange
    real*8  :: ccos, csin, ccurx, ccury
    real*8  :: esum(nspec), frlocal
    real*8  :: ea_the, ec_the, ea_sig, ec_sig, esi
    real*8  :: cad(nspec), cas(nspec), acloc(nspec)
    real*8  :: cp_sig(nspec), cm_sig(nspec)
    real*8  :: efactm1, efactp1
    real*8  :: sum_prev, sum_new, p_is_converged, diffnew, prop_conv
    real*8  :: sum_l2, sum_l2_gl
    real  :: dmm(0:nk2), dam(nspec), dam2(nspec), spec(nspec)
    real*8  :: ediff(nspec), eprod(nspec), ediffb(nspec)
    real*8  :: dwni_m2(nk), cwnb_m2(1-nth:nspec)
    real  :: vanew(nspec), vflwn(1-nth:nspec), jac, jac2
    real  :: vaanew(1-nth:nspec+nth), vaaacloc(1-nth:nspec+nth)
    real  :: vainput(nspec), vaacloc(nspec), aspar_diag(nspec)
    real  :: aspar_diag_local(nspec), aspar_off_diag_local(nspec), b_jac_local(nspec)
    real*8 :: ediffsing, esumpart
    real  :: emean, fmean, fmean1, wnmean, amax, u10abs, u10dir, taua, tauadir
    real  :: ustar, ustdir, tauwx, tauwy, cd, z0, charn, fmeanws, dlwmean
    real*8  :: eval1, eval2
    real*8  :: eva, evo, cg2, newdac, newac, oldac, maxdac
    real  :: cg1(0:nk+1), wn1(0:nk+1)
    logical :: lconverged(nseal), lexist, llws(nspec)
    character(len=128) efile
    integer ierr, i
    integer jp_glob
    integer is_converged, itmp
    integer :: testnode = 923
    logical :: lsig = .false.
    memunit = 50000+iaproc
    !ar: this is missing in init ... but there is a design error in ww3_grid with flcur and fllev
    lsig = flcur .or. fllev
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 0')
    ccurx  = facx
    ccury  = facy
    call mpi_comm_rank(mpi_comm_wcmp, myrank, ierr)
    !
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 1')
    !
    ! 2.  convert to wave action ---------------- *
    !
    do jsea=1,nseal
      ip      = jsea
      ip_glob = iplg(ip)
      isea    = mapfs(1,ip_glob)
      do isp=1,nspec
        ith    = 1 + mod(isp-1,nth)
        ik     = 1 + (isp-1)/nth
        cg1(ik)    = cg(ik,isea)
        va(isp,jsea) = va(isp,jsea) / cg1(ik) * clats(isea)
      end do
    end do
    vaold = va(1:nspec,1:nseal)
    !
    !    init matrix and right hand side
    !
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 2')
    !
    if (.not. lsloc) then
      if (imem == 1) then
        aspar_jac = zero
      else if (imem == 2) then
        aspar_diag_all = zero
      endif
      b_jac = zero
    endif
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 3')
    !
    !     source terms
    !
    if (fssource) then
      if (.not. lsloc) then
        if (imem == 1) then
          call calcarray_jacobi_source_1(dtg)
        else if (imem == 2) then
          call calcarray_jacobi_source_2(dtg,aspar_diag_all)
        endif
      endif
    end if
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 4')
    !
    !     geographical advection
    !
    if (imem == 1) then
      call calcarray_jacobi_vec(dtg,facx,facy,vgx,vgy)
    endif
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 5')
    !
    !
    !     spectral advection
    !
    if (fsfreqshift .or. fsrefraction) then
      if (imem == 1) then
        call calcarray_jacobi_spectral_1(dtg)
      else if (imem == 2) then
        call calcarray_jacobi_spectral_2(dtg,aspar_diag_all)
      endif
    end if
    call apply_boundary_condition(imod)
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section 6')
    !
    nbiter=0
    do ip = 1, np
      lconverged(ip) = .false.
    enddo
    !
    do
      is_converged = 0
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section solver loop 1')
      do ip = 1, np
        ip_glob = iplg(ip)
        isea    = mapfs(1,ip_glob)
        if (iobdp_loc(ip) .eq. 0) then
          is_converged   = is_converged + 1
          lconverged(ip) = .true.
          cycle
        endif
        do ik = 0, nk + 1
          cg1(ik)  = cg(ik,isea)
          wn1(ik)  = wn(ik,isea)
        enddo
        jsea  = jx_to_jsea(ip)
        isea  = mapfs(1,ip_glob)
        esi   = pdlib_si(ip)
        acloc = va(:,jsea)
        if (.not. lconverged(ip)) then
          sum_prev = sum(acloc)
          if (imem == 2) then
            call calcarray_jacobi4(ip,dtg,facx,facy,vgx,vgy,aspar_diag_local,aspar_off_diag_local,b_jac_local)
            aspar_diag(1:nspec) = aspar_diag_local(1:nspec) + aspar_diag_all(1:nspec,ip)
            esum       = b_jac_local - aspar_off_diag_local + b_jac(1:nspec,ip)
          elseif (imem == 1) then
            esum(1:nspec)       = b_jac(1:nspec,ip)
            aspar_diag(1:nspec) = aspar_jac(1:nspec,pdlib_i_diag(ip))
            do i = pdlib_ia_p(ip)+1, pdlib_ia_p(ip+1)
              jp = pdlib_ja(i)
              if (jp .ne. ip) then
                eprod(1:nspec) = aspar_jac(1:nspec,i) * va(1:nspec,jp)
                esum(1:nspec)  = esum(1:nspec) - eprod(1:nspec)
              end if
            end do
          endif ! imem
          if (fsrefraction) then
            cad = cad_the(:,ip)
            do isp=1,nspec
              ispprevdir=listispprevdir(isp)
              ispnextdir=listispnextdir(isp)
              ea_the = - dtg*esi*max(zero,cad(ispprevdir))
              ec_the =   dtg*esi*min(zero,cad(ispnextdir))
              esum(isp) = esum(isp) - ea_the * va(ispprevdir,ip)
              esum(isp) = esum(isp) - ec_the * va(ispnextdir,ip)
            end do
          end if
          if (fsfreqshift .and. lsig) then
            if (freqshiftmethod .eq. 1) then
              cas = cas_sig(:,ip)
              cp_sig = max(zero,cas)
              cm_sig = min(zero,cas)
              do ik=0, nk
                dmm(ik+1) = dble(wn1(ik+1) - wn1(ik))
              end do
              dmm(nk+2) = zero
              dmm(0)=dmm(1)
              do ith=1,nth
                do ik=2,nk
                  isp       = ith + (ik   -1)*nth
                  ispm1     = ith + (ik-1 -1)*nth
                  efactm1   = cg1(ik-1) / cg1(ik)
                  ea_sig    = - esi * cp_sig(ispm1)/dmm(ik-1) * efactm1
                  esum(isp) = esum(isp) - ea_sig*va(ispm1,ip)
                end do
                do ik=1,nk-1
                  isp       = ith + (ik   -1)*nth
                  ispp1     = ith + (ik+1 -1)*nth
                  efactp1   = cg1(ik+1) / cg1(ik)
                  ec_sig    = esi * cm_sig(ispp1)/dmm(ik) * efactp1
                  esum(isp) = esum(isp) - ec_sig*va(ispp1,ip)
                end do
              end do
            else if (freqshiftmethod .eq. 2) then
              cwnb_m2=cwnb_sig_m2(:,ip)
              do ik=1, nk
                dwni_m2(ik) = dble( cg1(ik) / dsip(ik) )
              end do
              do ith=1,nth
                do ik=2,nk
                  isp       = ith + (ik   -1)*nth
                  ispm1     = ith + (ik-1 -1)*nth
                  efactm1   = dble( cg1(ik-1) / cg1(ik) )
                  ea_sig    = - esi * dwni_m2(ik) * max(cwnb_m2(ispm1),zero) *efactm1
                  esum(isp) = esum(isp) - ea_sig*va(ispm1,ip)
                end do
                do ik=1,nk-1
                  isp       = ith + (ik   -1)*nth
                  ispp1     = ith + (ik+1 -1)*nth
                  efactp1   = dble( cg1(ik+1) / cg1(ik) )
                  ec_sig    = esi * dwni_m2(ik) * min(cwnb_m2(isp),zero) * efactp1
                  esum(isp) = esum(isp) - ec_sig*va(ispp1,ip)
                end do
              end do
            end if
          end if
          esum(1:nspec)  = esum(1:nspec) / aspar_diag(1:nspec)
          if (b_jgs_block_gauss_seidel) then
            va(1:nspec,ip) = real(esum) * iobdp_loc(ip)
          else
            u_jac(1:nspec,ip) = esum
          end if
        else
          esum = va(1:nspec,ip)
        endif ! .not. lconverged
        if (b_jgs_terminate_difference) then
          sum_new = sum(esum)
          if (sum_new .gt. 0.d0) then
            diffnew = abs(sum(acloc-esum))/sum_new
            p_is_converged = diffnew
          else
            p_is_converged = zero
          endif
          if (p_is_converged .lt. b_jgs_diff_thr .and. nbiter .gt. 1) then
            is_converged   = is_converged + 1
            lconverged(ip) = .true.
          else
            lconverged(ip) = .false.
          endif
        end if
      end do ! ip
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section solver loop 2')
      if (b_jgs_block_gauss_seidel) then
        call pdlib_exchange2dreal_zero(va)
      else
        call pdlib_exchange2dreal(u_jac)
        va(:,1:npa) = u_jac
      end if
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section solver loop 3')
      !
      ! terminate via number of iteration
      !
      if (b_jgs_terminate_maxiter) then
        if (nbiter .gt. b_jgs_maxiter) then
          exit
        end if
      end if
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section solver loop 4')
      !
      ! terminate via differences
      !
      if (b_jgs_terminate_difference .and. int(mod(nbiter,10)) == 0) then ! every 10th step check conv.
        call mpi_allreduce(is_converged, itmp, 1, mpi_int, mpi_sum, mpi_comm_wcmp, ierr)
        is_converged = itmp
        prop_conv = (dble(nx) - dble(is_converged))/dble(nx) * 100.
        if (myrank == 0) write(*,*) 'no. of solver iterations', nbiter, is_converged, prop_conv, b_jgs_pmin
        if (prop_conv .le. b_jgs_pmin + tiny(1.)) then
          exit
        end if
      end if
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section solver loop 5')
      !
      ! terminate via norm
      !
      if (b_jgs_terminate_norm) then
        sum_l2 =0
        do ip = 1, np
          ip_glob=iplg(ip)
          if (iobp_loc(ip).eq.1) then
            jsea=jx_to_jsea(ip)
            esi=pdlib_si(ip)
            esum=b_jac(:,ip)
            acloc=va(:,ip)
            isea= mapfs(1,ip_glob)
            esum(:) = esum(:) - aspar_diag(:)*acloc
            do i = pdlib_ia_p(ip)+1, pdlib_ia_p(ip+1)
              jp=pdlib_ja(i)
              esum(:) = esum(:) - aspar_jac(:,i)*va(:,jp)
            end do
            if (fsrefraction) then
              cad=cad_the(:,ip)
              do isp=1,nspec
                ispprevdir=listispprevdir(isp)
                ispnextdir=listispnextdir(isp)
                ea_the = - dtg*esi*max(zero,cad(ispprevdir))
                ec_the =   dtg*esi*min(zero,cad(ispnextdir))
                esum(isp) = esum(isp) - ea_the*va(ispprevdir,ip)
                esum(isp) = esum(isp) - ec_the*va(ispnextdir,ip)
              end do
            end if
            if (fsfreqshift) then
              cas=cas_sig(:,ip)
              cp_sig = max(zero,cas)
              cm_sig = min(zero,cas)
              do ik = 0, nk + 1
                cg1(ik)  = cg(ik,isea)
                wn1(ik)  = wn(ik,isea)
              enddo
              do ith=1,nth
                if (iobpd_loc(ith,ip) .ne. 0) then
                  do ik=2,nk
                    isp  =ith + (ik  -1)*nth
                    ispm1=ith + (ik-1-1)*nth
                    efactm1=cg(ik-1,isea) / cg1(ik)
                    ea_sig= - esi*cp_sig(ispm1)/dmm(ik-1) * efactm1
                    esum(isp) = esum(isp) - ea_sig*va(ispm1,ip)
                  end do
                  do ik=1,nk-1
                    isp  =ith + (ik  -1)*nth
                    ispp1=ith + (ik+1-1)*nth
                    efactp1=cg(ik+1,isea) / cg1(ik)
                    ec_sig= esi*cm_sig(ispp1)/dmm(ik) * efactp1
                    esum(isp) = esum(isp) - ec_sig*va(ispp1,ip)
                  end do
                end if
              end do
            end if
            sum_l2 = sum_l2 + sum(esum*esum)
          end if
        end do
        call mpi_allreduce(sum_l2, sum_l2_gl, 1, rtype, mpi_sum, mpi_comm_wcmp, ierr)
        if (sum_l2_gl .le. b_jgs_norm_thr) then
          exit
        end if
      end if
      call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section solver loop 6')
      nbiter = nbiter + 1
    end do ! open do loop ... end of time interval
    ! tihs is below also goes into the matrix ... like the wave boundary ...
    do ip = 1, npa
      do isp=1,nspec
        ith    = 1 + mod(isp-1,nth)
        va(isp,ip)=max(zero, va(isp,ip))*iobdp_loc(ip)*dble(iobpd_loc(ith,ip))
      end do
    end do
    do jsea=1, nseal
      ip      = jsea
      ip_glob = iplg(ip)
      isea    = mapfs(1,ip_glob)
      !
      !
      do isp=1,nspec
        ik     = 1 + (isp-1)/nth
        cg1(ik)    = cg(ik,isea)
        eva = max ( zero ,cg1(ik)/clats(isea)*real(va(isp,ip)) )
        evo = max ( zero ,cg1(ik)/clats(isea)*real(vaold(isp,jsea)) )
        vaold(isp,jsea) = evo
        va(isp,jsea) = eva
      end do
      if (flsou) then
        if (b_jgs_limiter) then
          do isp=1,nspec
            ik   = 1 + (isp-1)/nth
            spec(isp) = vaold(isp,jsea)
          enddo
          dam = 0.
          do ik=1, nk
            dam(1+(ik-1)*nth) = 0.0081*0.1 / ( 2 * sig(ik) * wn(ik,isea)**3 * cg(ik,isea)) * cg1(ik) / clats(isea)
          end do
          !
          do ik=1, nk
            is0    = (ik-1)*nth
            do ith=2, nth
              dam(ith+is0) = dam(1+is0)
            end do
          end do
          dam2 = 0.
          do ik=1, nk
            jac2     = 1./tpi/sig(ik)
            frlocal  = sig(ik)*tpiinv
            dam2(1+(ik-1)*nth) = 1e-06 * grav/frlocal**4 * ustar * max(fmeanws,fmean) * dtg * jac2 * cg1(ik) / clats(isea)
          end do
          do ik=1, nk
            is0  = (ik-1)*nth
            do ith=2, nth
              dam2(ith+is0) = dam2(1+is0)
            end do
          end do
          do ik = 1, nk
            do ith = 1, nth
              isp = ith + (ik-1)*nth
              newdac     = va(isp,ip) - vaold(isp,jsea)
              maxdac     = max(dam(isp),dam2(isp))
              newdac     = sign(min(maxdac,abs(newdac)), newdac)
              va(isp,ip) = max(0., vaold(isp,ip) + newdac)
            enddo
          enddo
        endif ! b_jgs_limiter
      endif  ! flsou
    end do ! jsea
    !
    call print_memcheck(memunit, 'memcheck_____:'//' ww3_prop section loop 7')
    !
  end subroutine pdlib_jacobi_gauss_seidel_block
  !/ ------------------------------------------------------------------- /
  subroutine pdlib_explicit_block(imod, facx, facy, dtg, vgx, vgy, lcalc)
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
    !  1. purpose : explicit block solver
    !  2. method : it uses the n-scheme and the idea is to reduce latency due
    !              to dd communication and increase vectorization level on the
    !              single core
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
    !
    !  5. called by :
    !
    !      name      type  module   description
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
    use w3gdatmd, only: nk, nth, nspec, sig, dth, esin, ecos, nseal, fsbccfl, clats, mapfs
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc, mapsf, nsea
    use w3odatmd, only: ndse, ndst, flbpi, nbi, tbpi0, tbpin, isbpi, bbpi0, bbpin
    use w3adatmd, only: dw, cx, cy, mpi_comm_wcmp
    use w3idatmd, only: flcur, fllev
    use w3wdatmd, only: va
    use w3dispmd, only: wavnu3
    use w3odatmd, only : iaproc
    implicit none
    logical, intent(in) :: lcalc
    integer, intent(in) :: imod
    real, intent(in)    :: facx, facy, dtg, vgx, vgy
    real              :: ktmp(3), utilde(nth), st(nth,npa)
    real              :: fl11(nth), fl12(nth), fl21(nth), fl22(nth), fl31(nth), fl32(nth), kksum(nth,npa)
    real              :: fl111(nth), fl112(nth), fl211(nth), fl212(nth), fl311(nth), fl312(nth)
    real              :: ksig(npa), cgsig(npa), cxx(nth,npa), cyy(nth,npa)
    real              :: lambdax(nth), lambday(nth)
    real              :: dtmax(nth), dtmaxexp(nth), dtmaxout, dtmaxgl
    real              :: fin(1), fout(1), rest, cflxy, rd1, rd2, rd10, rd20
    real              :: uold(nth,npa), u(nth,npa)
    real, parameter   :: onesixth = 1.0/6.0
    real, parameter   :: zero = 0.0
    real, parameter   :: thr = 1.0e-12
    integer           :: ik, isp, ith, ie, ip, it, ibi, ni(3), i1, i2, i3, jx, ierr, ip_glob, isea
    !
    ! 1.b initialize arrays
    !
    ! 2.  calculate velocities ---------------- *
    !
    !   2a. vectorized for all points looping over each wave number (maybe do a dirty save will be nice!)
    !
    do ik = 1, nk
      if (lcalc) then
        do ip = 1, npa
          call wavnu3 (sig(ik), dw(iplg(ip)), ksig(ip), cgsig(ip))
        enddo
        do ip = 1, npa
          do ith = 1, nth
            isea = iplg(ip)
            cxx(ith,ip) = cgsig(ip) * facx * ecos(ith) / clats(isea)
            cyy(ith,ip) = cgsig(ip) * facy * esin(ith)
          enddo ! ith
          if (flcur) then
            do ith = 1, nth
              isea = iplg(ip)
              if (iobp_loc(ip) .gt. 0) then
                cxx(ith,ip) = cxx(ith,ip) + facx * cx(isea)/clats(isea)
                cyy(ith,ip) = cyy(ith,ip) + facy * cy(isea)
              endif
            enddo !ith
          endif
        enddo
        do ie = 1, ne
          ni  = ine(:,ie)
          i1  = ni(1)
          i2  = ni(2)
          i3  = ni(3)
          do ith = 1, nth
            lambdax(ith) = onesixth *(cxx(ith,i1)+cxx(ith,i2)+cxx(ith,i3)) ! linearized advection speed in x and y direction
            lambday(ith) = onesixth *(cyy(ith,i1)+cyy(ith,i2)+cyy(ith,i3))
            kelem1(ith,ie,ik) = lambdax(ith) * pdlib_ien(1,ie) + lambday(ith) * pdlib_ien(2,ie) ! k-values - so called flux jacobians
            kelem2(ith,ie,ik) = lambdax(ith) * pdlib_ien(3,ie) + lambday(ith) * pdlib_ien(4,ie)
            kelem3(ith,ie,ik) = lambdax(ith) * pdlib_ien(5,ie) + lambday(ith) * pdlib_ien(6,ie)
            ktmp(1)           = kelem1(ith,ie,ik) ! extract
            ktmp(2)           = kelem2(ith,ie,ik)
            ktmp(3)           = kelem3(ith,ie,ik)
            nm(ith,ie,ik)     = - 1.d0/min(-thr,sum(min(zero,ktmp))) ! n-values
            kelem1(ith,ie,ik) = max(zero,ktmp(1))
            kelem2(ith,ie,ik) = max(zero,ktmp(2))
            kelem3(ith,ie,ik) = max(zero,ktmp(3))
          enddo
          fl11  = cxx(:,i2) * pdlib_ien(1,ie) + cyy(:,i2) * pdlib_ien(2,ie) ! weights for simpson integration
          fl12  = cxx(:,i3) * pdlib_ien(1,ie) + cyy(:,i3) * pdlib_ien(2,ie)
          fl21  = cxx(:,i3) * pdlib_ien(3,ie) + cyy(:,i3) * pdlib_ien(4,ie)
          fl22  = cxx(:,i1) * pdlib_ien(3,ie) + cyy(:,i1) * pdlib_ien(4,ie)
          fl31  = cxx(:,i1) * pdlib_ien(5,ie) + cyy(:,i1) * pdlib_ien(6,ie)
          fl32  = cxx(:,i2) * pdlib_ien(5,ie) + cyy(:,i2) * pdlib_ien(6,ie)
          fl111 = 2.d0 * fl11 + fl12
          fl112 = 2.d0 * fl12 + fl11
          fl211 = 2.d0 * fl21 + fl22
          fl212 = 2.d0 * fl22 + fl21
          fl311 = 2.d0 * fl31 + fl32
          fl312 = 2.d0 * fl32 + fl31
          flall1(:,ie,ik) = (fl311 + fl212) * onesixth + kelem1(:,ie,ik)
          flall2(:,ie,ik) = (fl111 + fl312) * onesixth + kelem2(:,ie,ik)
          flall3(:,ie,ik) = (fl211 + fl112) * onesixth + kelem3(:,ie,ik)
        enddo  ! ie
        kksum = zero
        do ie = 1, ne
          ni = ine(:,ie)
          do ith = 1, nth
            kksum(ith,ni(1)) = kksum(ith,ni(1)) + kelem1(ith,ie,ik)
            kksum(ith,ni(2)) = kksum(ith,ni(2)) + kelem2(ith,ie,ik)
            kksum(ith,ni(3)) = kksum(ith,ni(3)) + kelem3(ith,ie,ik)
          enddo
        end do
        dtmaxexp = 1.e10
        dtmax    = 1.e10
        do ip = 1, npa
          if (iobp_loc(ip) .eq. 1 .or. fsbccfl) then
            do ith = 1, nth
              dtmaxexp(ith) = pdlib_si(ip)/max(thr,kksum(ith,ip)*iobdp_loc(ip))
              dtmax(ith)    = min(dtmax(ith),dtmaxexp(ith))
            enddo
            dtmaxout = minval(dtmax)
          endif
        end do
        fin(1) = dtmaxout
        call mpi_allreduce(fin,fout,1,rtype,mpi_min,mpi_comm_wcmp,ierr)
        dtmaxgl = fout(1)
        cflxy = dble(dtg)/dtmaxgl
        rest  = abs(mod(cflxy,1.0d0))
        if (rest .lt. thr) then
          iter(ik) = abs(nint(cflxy))
        else if (rest .gt. thr .and. rest .lt. 0.5d0) then
          iter(ik) = abs(nint(cflxy)) + 1
        else
          iter(ik) = abs(nint(cflxy))
        end if
        do ip = 1, npa
          dtsi(ip) = dble(dtmaxgl)/dble(iter(ik))/pdlib_si(ip) ! some precalculations for the time integration.
        end do
      end if ! lcalc
      ! exact and convert wave action - should be some subroutine function or whatever
      do ip = 1,npa
        isp = 0
        do ith = 1,nth
          isp = ith + (ik-1)*nth
          u(ith,ip) = va(isp,ip) / cgsig(ip) * clats(iplg(ip))
        enddo
      enddo
      call pdlib_exchange2dreal(u)
      do it = 1, iter(ik)
        st = zero
        do ie = 1, ne
          ni  = ine(:,ie)
          do ith = 1, nth
            utilde(ith)   = nm(ith,ie,ik) * (flall1(ith,ie,ik)*u(ith,ni(1)) + flall2(ith,ie,ik)*u(ith,ni(2)) + flall3(ith,ie,ik)*u(ith,ni(3)))
            st(ith,ni(1)) = st(ith,ni(1)) + kelem1(ith,ie,ik) * (u(ith,ni(1)) - utilde(ith)) ! the 2nd term are the theta values of each node ...
            st(ith,ni(2)) = st(ith,ni(2)) + kelem2(ith,ie,ik) * (u(ith,ni(2)) - utilde(ith)) ! the 2nd term are the theta values of each node ...
            st(ith,ni(3)) = st(ith,ni(3)) + kelem3(ith,ie,ik) * (u(ith,ni(3)) - utilde(ith)) ! the 2nd term are the theta values of each node ...
          enddo
        end do ! ie
        do ip = 1, npa
          do ith = 1, nth
            u(ith,ip) = max(zero,u(ith,ip)-dtsi(ip)*st(ith,ip)*(1-iobpa_loc(ip)))*iobpd_loc(ith,ip)*iobdp_loc(ip)
          enddo
        enddo ! ip
        if ( flbpi ) then
          do ith = 1, nth
            isp = ith + (ik-1) * nth
            rd1 = rd10 - dtg * real(iter(ik)-it)/real(iter(ik))
            rd2 = rd20
            if ( rd2 .gt. 0.001 ) then
              rd2    = min(1.,max(0.,rd1/rd2))
              rd1    = 1. - rd2
            else
              rd1    = 0.
              rd2    = 1.
            end if
            do ibi = 1, nbi
              ip_glob = mapsf(isbpi(ibi),1)
              jx      = ipgl_npa(ip_glob)
              if (jx .gt. 0) then
                u(ith,jx) = ( rd1*bbpi0(isp,ibi) + rd2*bbpin(isp,ibi) ) / cgsig(isbpi(ibi)) * clats(isbpi(ibi))
              end if
            end do
          enddo
        endif ! flbpi
        call pdlib_exchange2dreal(u)
      enddo ! it
      ! exact and convert wave action
      do ip = 1,npa
        isp = 0
        do ith = 1,nth
          isp = ith + (ik-1)*nth
          va(isp,ip) = u(ith,ip) * cgsig(ip) / clats(iplg(ip))
        end do
      end do
    enddo ! ik
  end subroutine pdlib_explicit_block
  !/ ------------------------------------------------------------------- /
  subroutine block_solver_explicit_init()
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
    !  1. purpose : initialization of the block solver
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
    use w3gdatmd, only:  nth, nk
    implicit none
    !/
    allocate(flall1(nth,ne,nk), flall2(nth,ne,nk), flall3(nth,ne,nk))
    allocate(kelem1(nth,ne,nk), kelem2(nth,ne,nk), kelem3(nth,ne,nk))
    allocate(nm(nth,ne,nk), dtsi(npa))
    allocate(iter(nk))
    !/ ------------------------------------------------------------------- /
    !/
  end subroutine block_solver_explicit_init
  !/ ------------------------------------------------------------------- /
  subroutine block_solver_init(imod)
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
    !  1. purpose : initialization of the block solver
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
    use constants, only : lpdlib, tpi, tpiinv
    use w3gdatmd, only: mapsf, nseal, dmin, iobdp, mapsta, iobp, mapfs, nx
    use w3adatmd, only: dw
    use w3parall, only: init_get_isea
    use yownodepool, only: iplg, np
    use yowfunction, only: pdlib_abort
    use yownodepool, only: npa
    use w3gdatmd, only: b_jgs_use_jacobi
    use w3parall, only : listispprevdir, listispnextdir
    use w3parall, only : listispprevfreq, listispnextfreq
    use w3gdatmd, only: nspec, nth, nk
    use w3gdatmd, only: fstotalimp
    use w3odatmd, only: iaproc
    !/
    integer, intent(in) :: imod
    !
    !/ ------------------------------------------------------------------- /
    !/
    integer isp, ith, ik, ispprevfreq, ispnextfreq
    integer newisp, jth, istat
    pos_trick(1,1) = 2
    pos_trick(1,2) = 3
    pos_trick(2,1) = 3
    pos_trick(2,2) = 1
    pos_trick(3,1) = 1
    pos_trick(3,2) = 2
    allocate(listispnextdir(nspec), listispprevdir(nspec), listispnextfreq(nspec), listispprevfreq(nspec),stat=istat)
    if (istat /= 0) call pdlib_abort(8)
    do isp=1,nspec
      ith    = 1 + mod(isp-1,nth)
      ik     = 1 + (isp-1)/nth
      if (ik .eq. 1) then
        ispprevfreq=-1
      else
        ispprevfreq=ith + (ik-1 -1)*nth
      end if
      listispprevfreq(isp)=ispprevfreq
      if (ik .eq. nk) then
        ispnextfreq=-1
      else
        ispnextfreq=ith + (ik+1 -1)*nth
      end if
      listispnextfreq(isp)=ispnextfreq
      !
      if (ith .eq. 1) then
        jth=nth
      else
        jth=ith-1
      endif
      newisp=jth + (ik-1)*nth
      listispprevdir(isp)=newisp
      if (ith .eq. nth) then
        jth=1
      else
        jth=ith+1
      endif
      newisp=jth + (ik-1)*nth
      listispnextdir(isp)=newisp
    end do
    if (fstotalimp .and. b_jgs_use_jacobi) then
      call jacobi_init(imod)
    end if
  end subroutine block_solver_init
  !/ ------------------------------------------------------------------ /
  subroutine set_iobdp_pdlib
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
    !  1. purpose : set depth pointer
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
    use w3gdatmd, only: mapsf, nseal, dmin, mapsta, nx
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3adatmd, only: dw
    use w3parall, only: init_get_isea
    use yownodepool, only: iplg, np, npa
    !/
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
    integer :: jsea, isea, ix, ip, ip_glob
    real*8, parameter :: dthr = 10e-6
    do jsea=1,npa
      ip = jsea
      ip_glob = iplg(ip)
      if (dw(ip_glob) .lt. dmin + dthr) then
        iobdp_loc(ip)  = 0
      else
        iobdp_loc(ip)  = 1
      endif
    end do
    !/
    !/ end of setdepth_pdlib --------------------------------------------- /
    !/
  end subroutine set_iobdp_pdlib
  subroutine set_iobpa_pdlib
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
    !  1. purpose : set depth pointer
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
    use w3gdatmd, only: mapsf, nseal, dmin, mapsta, nx
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3adatmd, only: dw
    use w3parall, only: init_get_isea
    use yownodepool, only: iplg, np
    !/
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
    integer :: jsea, isea, ix, ip, ip_glob
    real*8, parameter :: dthr = 10e-6
    do jsea=1,nseal
      ip_glob = iplg(jsea)
      if (mapsta(1,ip_glob).eq.2) then
        iobpa_loc(jsea) = 1
      else
        iobpa_loc(jsea) = 0
      endif
    end do
    !/
    !/ end of setdepth_pdlib --------------------------------------------- /
    !/
  end subroutine set_iobpa_pdlib
  subroutine set_ug_iobp_pdlib_init()
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |        fabrice ardhuin            |
    !/                  |        aron roland                |
    !/                  |                        fortran 90 |
    !/                  | last update :         17-apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    23-aug-2011 : origination.                        ( version 4.04 )
    !/    17-apr-2016 : uses optimized boundary detection   ( version 5.10 )
    !/
    !  1. purpose :
    !
    !     redefines the values of the boundary points and angle pointers
    !     based on the mapsta array
    !
    !  2. method :
    !
    !     adapted boundary detection from a. roland and m. dutour (wwm code)
    !
    !  3. parameters :
    !
    !     parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     local variables.
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. subroutines used :
    !
    !  5. called by :
    !
    !      name      type  module   description
    !     ----------------------------------------------------------------
    !      ww3_grid  prog. ww3_grid grid preprocessor
    !      w3ulev    subr. w3updtmd water level update
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
    !
    use w3gdatmd, only: nx, ny, nsea, mapfs,                        &
         nk, nth, dth, xfr, mapsta, countri,         &
         ecos, esin, ien, ntri, trigp,               &
         iobp,iobpd, iobpa,                          &
         angle0, angle, nseal
    use w3odatmd, only: tbpi0, tbpin, flbpi
    use w3adatmd, only: cg, cx, cy, atrnx, atrny, itime, cflxymax
    use w3gdatmd, only: iobp_loc, iobpd_loc, iobpa_loc, iobdp_loc
    use w3idatmd, only: flcur
    use w3odatmd, only : iaproc
    use yownodepool,    only: pdlib_si, pdlib_ien, pdlib_tria, ipgl, iplg, npa, np
    use yowelementpool, only: ne, ine
    use yowexchangemodule, only : pdlib_exchange1dreal
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameters
    !/
    integer                 :: ith, ix, i, j, ip, ie, ndirsum
    real (kind = 8)         :: cossum, sinsum
    real (kind = 8)         :: dirmin, dirmax, shift, tempo, dircoast
    real (kind = 8)         :: x1, x2, y1, y2, dxp1, dxp2, dxp3
    real (kind = 8)         :: dyp1, dyp2, dyp3, edet1, edet2, evx, evy
    real(kind=8), parameter :: thr    = tiny(1.)
    integer                 :: i1, i2, i3
    integer                 :: itmp(nx), nextvert(nx), prevvert(nx)
    integer                 :: max_iobpd, min_iobpd
    real                    :: rtmp(npa)
    character(60) :: fname
    !/ ------------------------------------------------------------------- /
    !
    !
    do ie = 1, ne
      i1   =   ine(1,ie)
      i2   =   ine(2,ie)
      i3   =   ine(3,ie)
      dxp1 =   pdlib_ien(6,ie)
      dyp1 = - pdlib_ien(5,ie)
      dxp2 =   pdlib_ien(2,ie)
      dyp2 = - pdlib_ien(1,ie)
      dxp3 =   pdlib_ien(4,ie)
      dyp3 = - pdlib_ien(3,ie)
      do ith = 1, nth
        evx = ecos(ith)
        evy = esin(ith)
        do i = 1, 3
          if (i .eq. 1) then
            x1 =   dxp1
            y1 =   dyp1
            x2 = - dxp3
            y2 = - dyp3
            ip =   i1
          else if (i.eq.2) then
            x1 =   dxp2
            y1 =   dyp2
            x2 = - dxp1
            y2 = - dyp1
            ip =   i2
          else if (i.eq.3) then
            x1 =   dxp3
            y1 =   dyp3
            x2 = - dxp2
            y2 = - dyp2
            ip =   i3
          end if
          if (iobp_loc(ip) .eq. 0) then ! physical boundary
            edet1 = thr-x1*evy+y1*evx
            edet2 = thr+x2*evy-y2*evx
            if ((edet1.gt.0.).and.(edet2.gt.0.)) then
              ! this is the case of waves going towards the boundary ...
              iobpd_loc(ith,ip) = 1
            endif
          else ! water ...
            iobpd_loc(ith,ip) = 1
          end if
        end do
      end do
    end do
    do ith = 1, nth
      rtmp = real(iobpd_loc(ith,1:npa))
      call pdlib_exchange1dreal(rtmp)
      iobpd_loc(ith,1:npa) = int(rtmp)
    enddo
    max_iobpd = maxval(iobpd_loc)
    min_iobpd = minval(iobpd_loc)
    if (max_iobpd .gt. 1 .or. min_iobpd .lt. 0) then
      write(*,*) 'max_iobpd - min_iobpd', max_iobpd, min_iobpd
      stop 'max_iobpd errror'
    endif
    do ip = 1, npa
      if ( iobpa_loc(ip) .eq. 1 .or. iobp_loc(ip) .eq. 3 .or. iobp_loc(ip) .eq. 4) iobpd_loc(:,ip) = 1
    end do
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3. updates the reflection direction and sharp / flat shoreline angle
  end subroutine set_ug_iobp_pdlib_init
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  subroutine block_solver_finalize
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
    !  1. purpose : finalize solver
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
    use w3gdatmd, only: b_jgs_use_jacobi
    !/
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
    if (b_jgs_use_jacobi) then
      call jacobi_finalize
    end if
    !/
    !/ end of setdepth_pdlib --------------------------------------------- /
    !/
  end subroutine block_solver_finalize
  !/ ------------------------------------------------------------------- /
  subroutine deallocate_pdlib_global(imod)
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
    !  1. purpose : init jacobi solver
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
    use w3gdatmd, only: nspec, b_jgs_block_gauss_seidel, grids
    use yownodepool, only: pdlib_nnz, npa, np
    use yowfunction, only: pdlib_abort
    use w3gdatmd, only: nth, nk, nseal
    use w3parall, only: imem
    !/
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
    integer, intent(in) :: imod
    deallocate (                 &
                                ! grids(imod)%trigp,    &
         grids(imod)%si,         &
         grids(imod)%tria,       &
         grids(imod)%crossdiff,  &
         grids(imod)%ien,        &
         grids(imod)%len,        &
         grids(imod)%angle,      &
         grids(imod)%angle0,     &
         grids(imod)%ccon,       &
         grids(imod)%countcon,   &
         grids(imod)%index_cell, &
         grids(imod)%ie_cell,    &
         grids(imod)%pos_cell,   &
         grids(imod)%iaa,        &
         grids(imod)%jaa,        &
         grids(imod)%posi,       &
         grids(imod)%i_diag,     &
         grids(imod)%ja_ie,      &
                                !grids(imod)%iobp,      &
                                !grids(imod)%iobpd,     &
         grids(imod)%iobdp,      &
         grids(imod)%iobpa  )
    !/
    !/ end of deallocate_pdlib_global ------------------------------------------------ /
    !/
  end subroutine deallocate_pdlib_global
  subroutine ergout(fhndl, ergname)
    !/
    !/                  +-----------------------------------+
    !/                  | wavewatch iii           noaa/ncep |
    !/                  |                                   |
    !/                  | aron roland (bgs it&e gmbh)       |
    !/                  |                                   |
    !/                  |                        fortran 90 |
    !/                  | last update :      01-januar-2023 |
    !/                  +-----------------------------------+
    !/
    !/    01-june-2018 : origination.                        ( version 7.xx )
    !/
    !  1. purpose : write spatial out for xfn
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
    use w3gdatmd, only: nspec, nth, nk, nseal
    use w3wdatmd, only: va, vaold
    implicit none
    integer, intent(in)           :: fhndl
    character(len=*), intent(in) :: ergname
    real    :: sumva(nseal)
    integer :: jsea
    if (linit_output) then
      open(fhndl, file  = trim(ergname), form = 'unformatted')
      linit_output = .false.
    endif
    rtime = rtime + 1.
    do jsea = 1, nseal
      sumva(jsea) = sum(va(:,jsea))
    enddo
    write(fhndl)  rtime
    write(fhndl) (sumva(jsea), sumva(jsea), sumva(jsea), jsea = 1, nseal)
  end subroutine ergout
  !/ ------------------------------------------------------------------- /
  subroutine jacobi_init(imod)
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
    !  1. purpose : init jacobi solver
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
    use w3gdatmd, only: nspec, b_jgs_block_gauss_seidel, grids
    use yownodepool, only: pdlib_nnz, npa, np
    use yowfunction, only: pdlib_abort
    use w3gdatmd, only: nth, nk, nseal
    use w3parall, only: imem
    !/
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
    integer, intent(in) :: imod
    integer istat
    if (imem == 1) then
      allocate(aspar_jac(nspec, pdlib_nnz), stat=istat)
      if(istat /= 0) call pdlib_abort(9)
    else if (imem == 2) then
      allocate(aspar_diag_all(nspec, npa), stat=istat)
      if(istat /= 0) call pdlib_abort(9)
    endif
    allocate(b_jac(nspec,nseal), stat=istat)
    if(istat /= 0) call pdlib_abort(10)
    allocate(cad_the(nspec,nseal), stat=istat)
    if(istat /= 0) call pdlib_abort(11)
    if (freqshiftmethod .eq. 1) then
      allocate(cas_sig(nspec,nseal), stat=istat)
      if(istat /= 0) call pdlib_abort(11)
    else if (freqshiftmethod .eq. 2) then
      allocate(cwnb_sig_m2(1-nth:nspec,nseal), stat=istat)
      if(istat /= 0) call pdlib_abort(11)
    end if
    if (.not. b_jgs_block_gauss_seidel) then
      allocate(u_jac(nspec,npa), stat=istat)
      if(istat /= 0) call pdlib_abort(12)
    end if
    !/
    !/ end of jacobi_init ------------------------------------------------ /
    !/
  end subroutine jacobi_init
  !/ ------------------------------------------------------------------- /
  subroutine jacobi_finalize
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
    !  1. purpose : finalize jacobi solver
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
    use w3gdatmd, only: b_jgs_block_gauss_seidel
    use w3parall, only: imem
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ local parameter
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    if (imem == 1) then
      deallocate(aspar_jac)
    else if (imem == 2) then
      deallocate(aspar_diag_all)
    endif
    deallocate(b_jac)
    deallocate(cad_the)
    if (freqshiftmethod .eq. 1) then
      deallocate(cas_sig)
    else if (freqshiftmethod .eq. 2) then
      deallocate(cwnb_sig_m2)
    end if
    if (.not. b_jgs_block_gauss_seidel) then
      deallocate(u_jac)
    end if
    !/
    !/ end of jacobi_finalize -------------------------------------------- /
    !/
  end subroutine jacobi_finalize
  !/ ------------------------------------------------------------------- /
end module pdlib_w3profsmd
