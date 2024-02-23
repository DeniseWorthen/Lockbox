!> @file
!> @brief Contains MODULE W3WAVEMD.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>
#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Contains wave model subroutine, w3wave.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>
module wav_step_mod

  use w3parall   ,  only : init_get_isea
  use w3pro3md   ,  only : w3ktp3, w3xyp3
  use w3profsmd  ,  only : w3xypug
  use w3servmd   ,  only : print_memcheck
  use w3srcemd   ,  only : w3srce
  use w3timemd   ,  only : dsec21
  use w3triamd   ,  only : ug_gradients
  use w3updtmd   ,  only : w3dzxy, w3utrn
  use w3wavemd   ,  only : w3gath, w3scat
  use w3pro3md   ,  only : w3map3, w3mapt
  use w3iogoncdmd,  only : w3iogoncd
  use w3iogomd   ,  only : w3outg
  use constants  ,  only : undef, dera, radius, lpdlib, srce_direct, tpi, tpiinv

  use w3adatmd   ,  only : iappro, flcold
  use w3adatmd   ,  only : cx, cy, dtdyn, fcut, u10, u10d, ua, ud, as, tauox, tauoy
  use w3adatmd   ,  only : dw, cg, wn, dcdx, dcdy, dddx, dddy, dcxdx, dcxdy
  use w3adatmd   ,  only : dcydx, dcydy, cflthmax, cflkmax, tauwix, tauwiy
  use w3adatmd   ,  only : tauwnx, tauwny, phiaw, charn, tws, phioc, phibbl
  use w3adatmd   ,  only : whitecap, bedforms, taubbl, tauice, alpha, phice
  use w3adatmd   ,  only : tauocx, tauocy, wnmean

  use w3gdatmd   ,  only : mapsf, mapfs, gtype, ungtype, iobp, cthg0s
  use w3gdatmd   ,  only : flcth, flck, flcx, flcy, flagll, flsou, flagst
  use w3gdatmd   ,  only : fsrefraction, fsfreqshift
  use w3gdatmd   ,  only : sig, nk, nspec, nsea, nseal, nx, ny, mapsta
  use w3gdatmd   ,  only : clats, dmin, trnx, trny
  use w3gdatmd   ,  only : dtmax, dtcfli, dth, rwindc

  use w3idatmd   ,  only : flcur, flwind
  use w3idatmd   ,  only : wx0, wy0, dt0

  use w3odatmd   ,  only : iaproc, naplog, napout, naperr, napfld, naprst, ndso
  use w3odatmd   ,  only : nrqgo, irqgo, nrqgo2, irqgo2
  use w3odatmd   ,  only : histwr, rstwr, user_netcdf_grdout

  use w3wdatmd   ,  only : va, ust, ustdir, ice, iceh, icef, icedmax, berg
  use w3wdatmd   ,  only : fpis, rhoair, asf
#ifdef W3_MPI
  use w3adatmd   ,  only : mpibuf, nrqsg1, nrqsg2, irqsg1
#endif

  implicit none

  include "mpif.h"

  integer, parameter :: imod = 1

!   USE CONSTANTS
!     !/
!     USE W3GDATMD
!     USE W3WDATMD
!     USE W3ADATMD
!     USE W3IDATMD
!     USE W3ODATMD
!     !/
!     USE W3UPDTMD
!     USE W3SRCEMD
! #ifdef W3_PR1
!     USE W3PRO1MD
! #endif
! #ifdef W3_PR2
!     USE W3PRO2MD
! #endif
! #ifdef W3_PR3
!     USE W3PRO3MD
! #endif
!     !
! #ifdef W3_PR1
!     USE W3PROFSMD
! #endif
! #ifdef W3_PR2
!     USE W3PROFSMD
! #endif
! #ifdef W3_PR3
!     USE W3PROFSMD
! #endif
!     !/
!     USE W3TRIAMD
!     USE W3IOGRMD
!     USE W3IOGOMD
!     USE W3IOPOMD
!     USE W3IOTRMD
!     USE W3IORSMD
!     USE W3IOBCMD
!     USE W3IOSFMD
! #ifdef W3_PDLIB
!     USE PDLIB_FIELD_VEC, only : DO_OUTPUT_EXCHANGES
!     USE PDLIB_W3PROFSMD, ONLY: ASPAR_JAC, ASPAR_DIAG_ALL, B_JAC
!     USE W3PARALL, only : LSLOC

!     USE PDLIB_W3PROFSMD, only : APPLY_BOUNDARY_CONDITION_VA
!     USE PDLIB_W3PROFSMD, only : PDLIB_W3XYPUG, PDLIB_W3XYPUG_BLOCK_IMPLICIT, PDLIB_W3XYPUG_BLOCK_EXPLICIT
!     USE PDLIB_W3PROFSMD, only : ALL_VA_INTEGRAL_PRINT, ALL_VAOLD_INTEGRAL_PRINT, ALL_FIELD_INTEGRAL_PRINT
!     USE W3PARALL, only : PDLIB_NSEAL, PDLIB_NSEALM
!     USE yowNodepool, only: npa, iplg, np
! #endif
!     !/
!     USE W3SERVMD
!     USE W3TIMEMD
! #ifdef W3_IC3
!     USE W3SIC3MD
! #endif
! #ifdef W3_IS2
!     USE W3SIS2MD
! #endif
! #ifdef W3_UOST
!     USE W3UOSTMD, ONLY: UOST_SETGRID
! #endif
!     USE W3PARALL, ONLY : INIT_GET_ISEA
! #ifdef W3_SETUP
!     USE W3WAVSET, only : WAVE_SETUP_COMPUTATION
! #endif

!     use w3iogoncdmd   , only : w3iogoncd

contains
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Run WAVEWATCH III for a given time interval.
  !>
  !> @details Currents are updated before winds as currents are used in wind
  !> and USTAR processing.
  !>
  !> Ice and water levels can be updated only once per call.
  !>
  !> If ice or water level time are undefined, the update
  !> takes place asap, otherwise around the "half-way point"
  !> between the old and new times.
  !>
  !> To increase accuracy, the calculation of the intra-spectral
  !> propagation is performed in two parts around the spatial propagation.
  !>
  !> @param[in] IMOD      Model number.
  !> @param[in] TEND      Ending time of integration.
  !> @param[in] STAMP     Write time stamp (optional, defaults to T).
  !> @param[in] NO_OUT    Skip output (optional, defaults to F).
  !> @param[in] ODAT
  !> @param[in] ID_LCOMM  Present only when using W3_OASIS.
  !> @param[in] TIMEN     Present only when using W3_OASIS.
  !>
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  subroutine w3step ( time )

    integer, intent(in) :: time(2)
    !INTEGER, INTENT(IN)           :: IMOD, TEND(2),ODAT(35)
    !LOGICAL, INTENT(IN), OPTIONAL :: STAMP, NO_OUT

    ! local parameters
    integer :: isea, jsea, ispec, it, itloc, itloch, ix, iy, ixrel, j
    integer :: refled(6)
    integer :: ntloc
    logical :: flgmpi(0:8)

    !     INTEGER                 :: IP
    !     INTEGER                 :: TCALC(2), IT, IT0, NT, ITEST,        &
    !          ITLOC, ITLOCH, NTLOC, ISEA, JSEA,    &
    !          IX, IY, ISPEC, J, TOUT(2), TLST(2),  &
    !          REFLED(6), IK, ITH, IS, NKCFL
    !     INTEGER                 :: ISP, IP_glob
    !     INTEGER                 :: TTEST(2),DTTEST
    integer :: ttest(2)
    real :: dttest
    !     REAL                    :: ICEDAVE
    !     !
    !     LOGICAL                 :: SBSED
    !     LOGICAL                 :: CPLWRTFLG
    ! #ifdef W3_SEC1
    !     INTEGER                 :: ISEC1
    ! #endif
    !     INTEGER                 :: JJ, NDSOFLG
#ifdef W3_MPI
    INTEGER                 :: IERR_MPI, NRQMAX
    INTEGER, ALLOCATABLE    :: STATCO(:,:), STATIO(:,:)
#endif
    !     INTEGER                 :: IXrel
    !     REAL                    :: DTTST, DTTST1, DTTST2, DTTST3,       &
    !          DTL0, DTI0, DTR0, DTI10, DTI50,      &
    !          DTGA, DTG, DTGpre, DTRES,            &
    !          FAC, VGX, VGY, FACK, FACTH,          &
    !          FACX, XXX, REFLEC(4),                &
    !          DELX, DELY, DELA, DEPTH, D50, PSIC
    real :: vgx, vgy, uxr, uyr
    real :: dtg, dt, fac, fack, facth, facx
    real :: d50, dela, delx, dely, depth, psic

    real :: reflec(4)
    !     REAL                     :: VSioDummy(NSPEC), VDioDummy(NSPEC), VAoldDummy(NSPEC)
    !     LOGICAL                  :: SHAVETOTioDummy
    real :: vsiodummy(nspec), vdiodummy(nspec), vaolddummy(nspec)
    logical :: shavetotioDummy
    !     !
    !     REAL, ALLOCATABLE       :: FIELD(:)
    REAL                    :: TMP1(4), TMP2(3), TMP3(2), TMP4(2)
    ! #ifdef W3_IC3
    !     REAL, ALLOCATABLE       :: WN_I(:)
    ! #endif
    ! #ifdef W3_REFRX
    !     REAL, ALLOCATABLE       :: CIK(:)
    ! #endif
    !     !
    !     ! Orphaned arrays from old data structure
    !     !
    REAL, ALLOCATABLE       :: TAUWX(:), TAUWY(:)
    REAL, ALLOCATABLE       :: FIELD(:)
    !     !
    !     LOGICAL                 :: FLACT, FLZERO, FLFRST, FLMAP, TSTAMP,&
    !          SKIP_O, FLAG_O, FLDDIR, READBC,      &
    !          FLAG0 = .FALSE., FLOUTG, FLPFLD,     &
    !          FLPART, LOCAL, FLOUTG2
    logical :: flddir, flmap
    !     !
    ! #ifdef W3_IC3
    !     REAL                    :: FIXEDVISC,FIXEDDENS,FIXEDELAS
    !     REAL                    :: USE_CHENG, USE_CGICE, HICE
    ! #endif
    logical                 :: ugdtupdate    ! true if time step should be updated for ug schemes
    !     CHARACTER(LEN=8)        :: STTIME
    !     CHARACTER(LEN=21)       :: IDACT
    !     CHARACTER(LEN=16)       :: OUTID
    !     CHARACTER(LEN=23)       :: IDTIME
    !     INTEGER eIOBP
    !     INTEGER ITH_F
    ! #ifdef W3_PDLIB
    !     REAL ::             VS_SPEC(NSPEC)
    !     REAL ::             VD_SPEC(NSPEC)
    ! #endif
    !     !
    !     !CHARACTER(LEN=30)       :: FOUTNAME
    !     !
    !     !/
    !     ! locally defined flags
    ! #ifdef W3_SBS
    !     logical, parameter ::  w3_sbs_flag = .true.
    ! #else
    !     logical, parameter ::  w3_sbs_flag = .false.
    ! #endif
    ! #ifdef W3_CESMCOUPLED
    !     logical, parameter :: w3_cesmcoupled_flag = .true.
    ! #else
    !     logical, parameter :: w3_cesmcoupled_flag = .false.
    ! #endif
    integer :: memunit
    !     logical :: do_gridded_output
    !     logical :: do_point_output
    !     logical :: do_track_output
    !     logical :: do_restart_output
    !     logical :: do_sf_output
    !     logical :: do_coupler_output
    !     logical :: do_wavefield_separation_output
         logical :: do_startall
    !     logical :: do_w3outg

    !/ ------------------------------------------------------------------- /
    ! 0.  Initializations
    !
    !XXX = undef
    memunit = 40000+iaproc
    ! 0.a Set pointers to data structure
    !

    !
    !IF ( IOUTP  .NE. IMOD ) CALL W3SETO ( IMOD, NDSE, NDST )
    !IF ( IGRID  .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
    !IF ( IWDATA .NE. IMOD ) CALL W3SETW ( IMOD, NDSE, NDST )
    !IF ( IADATA .NE. IMOD ) CALL W3SETA ( IMOD, NDSE, NDST )
    !IF ( IIDATA .NE. IMOD ) CALL W3SETI ( IMOD, NDSE, NDST )
#ifdef w3_uost
    call uost_setgrid(imod)
#endif
    !
    allocate(tauwx(nseal), tauwy(nseal))
#ifdef w3_refrx
    allocate(cik(nseal))
#endif
    allocate ( field(1-ny:ny*(nx+2)) )

    print *,'XXX ',naplog,napout,naperr,napfld,naprst
    !--------------------------------------------------------------------
    ! local parameter initialization
    !--------------------------------------------------------------------

    flmap = .true.
    flgmpi = .false.
    tauwx = 0.0
    tauwy = 0.0
    field = 0.0
#ifdef W3_REFRX
    cik = 0.0
#endif
    if ( flcold ) then
      dtdyn = 0.
      fcut  = sig(nk) * tpiinv
    end if

    ugdtupdate = .false.
    if (flagll) then
      facx =  1./(dera * radius)
    else
      facx =  1.
    end if

    !
    ! IF ( PRESENT(STAMP) ) THEN
    !   TSTAMP = STAMP
    ! ELSE
    !   TSTAMP = .TRUE.
    ! END IF
    ! !
    ! IF ( PRESENT(NO_OUT) ) THEN
    !   SKIP_O = NO_OUT
    ! ELSE
    !   SKIP_O = .FALSE.
    ! END IF
    !FLDDIR = ITIME .EQ. 0 .AND. ( FLCTH .OR. FSREFRACTION .OR. FLCK .OR. FSFREQSHIFT )
    flddir = FLCTH .OR. FSREFRACTION .OR. FLCK .OR. FSFREQSHIFT

    ! 'loop over timesteps'
#ifdef w3_pdlib
    ! copy old values
    do ip=1,nseal
      do ispec=1,nspec
        vaold(ispec,ip)=va(ispec,ip)
      end do
    end do
#endif
    ! what is inflags1(10)?? False, whatever it is, flcx,flcy true

    !--------------------------------------------------------------------
    ! update fields
    !--------------------------------------------------------------------

    do isea=1, nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      ua(isea) = sqrt ( wx0(ix,iy)**2 + wy0(ix,iy)**2 )

      if ( ua(isea) .gt. 1.e-7) then
        ud(isea)    = mod ( tpi + atan2(wy0(ix,iy),wx0(ix,iy)) , tpi )
      else
        ud(isea) = 0.0
      end if
      as(isea) = dt0(ix,iy)
    end do
#ifdef  W3_RWND
    if ( flcur ) then
      do isea=1, nsea
        uxr = ua(isea)*cos(ud(isea)) - rwindc*cx(isea)
        uyr = ua(isea)*sin(ud(isea)) - rwindc*cy(isea)
        u10 (isea) = max ( 0.001 , sqrt(uxr**2 + uyr**2) )
        u10d(isea) = mod ( tpi + atan2(uyr,uxr) , tpi )
      end do
    else
#endif
      do isea=1, nsea
        u10 (isea) = max ( ua(isea) , 0.001 )
        u10d(isea) = ud(isea)
      end do
#ifdef W3_RWND
    end if
#endif

    ! set ice,icef,iceh values

    ! maps and derivatives (need flmap?)
    call w3map3
    call w3utrn ( trnx, trny )
    call w3mapt

    ! if fldir true, then do this
    if (gtype .eq. ungtype) then
      call ug_gradients(dw, dddx, dddy)
    else
      call w3dzxy(dw(1:ubound(dw,1)),'m',dddx,dddy)
    end if

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 12')
    !
    ! calculate phase speed gradient.
    dcdx = 0.
    dcdy = 0.
    dtg = dtmax
    !
    !
    !fliwnd = .false.
    !flfrst = .false.
    !
#ifdef w3_pdlib
    if (it .eq. 0) then
      dtgpre = 1.
    else
      dtgpre = dtg
    end if
    if (lpdlib .and. flsou .and. fssource) then
      !!$omp parallel do private (jsea,isea,ix,iy) schedule (dynamic,1)
      d50=0.0002
      reflec(:)=0.
      refled(:)=0
      psic=0.

      if (lsloc) then
        b_jac     = 0.
        aspar_jac = 0.
      else
        vstot = 0.
        vdtot = 0.
      endif

      do jsea = 1, np

        call init_get_isea(isea, jsea)

        ix     = mapsf(isea,1)
        iy     = mapsf(isea,2)
        dela=1.
        delx=1.
        dely=1.

#ifdef w3_ref1
        if (gtype.eq.rlgtype) then
          delx=sx*clats(isea)/facx
          dely=sy/facx
          dela=delx*dely
        end if
        if (gtype.eq.clgtype) then
          ! maybe what follows works also for rlgtype ... to be verified
          delx=hpfac(iy,ix)/ facx
          dely=hqfac(iy,ix)/ facx
          dela=delx*dely
        end if
        reflec=reflc(:,isea)
        reflec(4)=berg(isea)*reflec(4)
        refled=refld(:,isea)
#endif

#ifdef w3_bt4
        d50=sed_d50(isea)
        psic=sed_psic(isea)
#endif
        !
        call w3srce(srce_imp_pre, it, isea, jsea, ix, iy, imod, &
             vaold(:,jsea), va(:,jsea),                         &
             vsiodummy, vdiodummy, shavetot(jsea),              &
             alpha(1:nk,jsea), wn(1:nk,isea),                   &
             cg(1:nk,isea), clats(isea), dw(isea), u10(isea),   &
             u10d(isea),                                        &
#ifdef w3_flx5
             taua(isea), tauadir(isea),                         &
#endif
             as(isea), ust(isea),                               &
             ustdir(isea), cx(isea), cy(isea),                  &
             ice(isea), iceh(isea), icef(isea),                 &
             icedmax(isea),                                     &
             reflec, refled, delx, dely, dela,                  &
             trnx(iy,ix), trny(iy,ix), berg(isea),              &
             fpis(isea), dtdyn(jsea),                           &
             fcut(jsea), dtgpre, tauwx(jsea), tauwy(jsea),      &
             tauox(jsea), tauoy(jsea), tauwix(jsea),            &
             tauwiy(jsea), tauwnx(jsea),                        &
             tauwny(jsea),  phiaw(jsea), charn(jsea),           &
             tws(jsea), phioc(jsea), tmp1, d50, psic, tmp2,     &
             phibbl(jsea), tmp3, tmp4, phice(jsea),             &
             tauocx(jsea), tauocy(jsea), wnmean(jsea),          &
             rhoair(isea), asf(isea))
        if (.not. lsloc) then
          vstot(:,jsea) = vsiodummy
          vdtot(:,jsea) = vdiodummy
        endif
      end do ! jsea
    end if ! pdlib
#endif
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 14')
    !
    ! 3.6 Perform Propagation = = = = = = = = = = = = = = = = = = = = = = =
    ! 3.6.1 Preparations
    !
    print *,'xxx ',dtg,dtcfli
    ntloc  = 1 + int( dtg/dtcfli - 0.001 )
    !
    facth  = dtg / (dth*real(ntloc))
    fack   = dtg / real(ntloc)

    ttest(1) = time(1)
    ttest(2) = 0
    dttest = dsec21(ttest,time)
    itloch = ( ntloc + 1 - mod(nint(dttest/dtg),2) ) / 2
    print *,'yyy ',ntloc,facth,fack,itloch
    !
    ! 3.6.2 Intra-spectral part 1
    !
    if ( flcth .or. flck ) then
      do itloc=1, itloch
        !
        !!$omp parallel private (jsea,isea,ix,iy,depth,ixrel)
        !!$omp do schedule (dynamic,1)
        !
        do jsea=1, nseal
          call init_get_isea(isea, jsea)
          ix     = mapsf(isea,1)
          iy     = mapsf(isea,2)

          if ( gtype .eq. ungtype ) then
            if (lpdlib) then
#ifdef w3_pdlib
              if (iobp_loc(jsea) .ne. 1) cycle
#endif
            else
              if (iobp(isea) .ne. 1) cycle
            endif
          endif

          if ( mapsta(iy,ix) .eq. 1 ) then
            depth  = max ( dmin , dw(isea) )
            if (lpdlib) then
              ixrel = jsea
            else
              ixrel = ix
            end if
            !
            j = 1
            !
#ifdef w3_pr1
            call w3ktp1 ( isea, facth, fack, cthg0s(isea),       &
                 cg(:,isea), wn(:,isea), depth,                  &
                 dddx(iy,ixrel), dddy(iy,ixrel), cx(isea),       &
                 cy(isea), dcxdx(iy,ixrel), dcxdy(iy,ixrel),     &
                 dcydx(iy,ixrel), dcydy(iy,ixrel),               &
                 dcdx(:,iy,ixrel), dcdy(:,iy,ixrel), va(:,jsea))
#endif
#ifdef w3_pr2
            call w3ktp2 ( isea, facth, fack, cthg0s(isea),       &
                 cg(:,isea), wn(:,isea), depth,                  &
                 dddx(iy,ixrel), dddy(iy,ixrel), cx(isea),       &
                 cy(isea), dcxdx(iy,ixrel), dcxdy(iy,ixrel),     &
                 dcydx(iy,ixrel), dcydy(iy,ixrel),               &
                 dcdx(:,iy,ixrel), dcdy(:,iy,ixrel), va(:,jsea))
#endif
#ifdef w3_pr3
            call w3ktp3 ( isea, facth, fack, cthg0s(isea),       &
                 cg(:,isea), wn(:,isea), depth,                  &
                 dddx(iy,ixrel), dddy(iy,ixrel), cx(isea),       &
                 cy(isea), dcxdx(iy,ixrel), dcxdy(iy,ixrel),     &
                 dcydx(iy,ixrel), dcydy(iy,ixrel),               &
                 dcdx(:,iy,ixrel), dcdy(:,iy,ixrel), va(:,jsea), &
                 cflthmax(jsea), cflkmax(jsea) )
#endif
          end if
        end do
        !
        !!$omp end do
        !!$omp end parallel
        !
      end do
    end if

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 16')

    print *,'yyy here0'
    !
    ! 3.6.3 Longitude-latitude
    !       (time step correction in routine)
    !
    if (gtype .eq. ungtype) then
      if (flagll) then
        facx   =  1./(dera * radius)
      else
        facx   =  1.
      end if
    end if

    if (lpdlib) then
#ifdef w3_pdlib
      if (flcx .or. flcy) then
        if (.not. fstotalimp .and. .not. fstotalexp) then
          do ispec=1,nspec
            call pdlib_w3xypug ( ispec, facx, facx, dtg, vgx, vgy, ugdtupdate )
          end do
        end if
      end if
      if (fstotalimp .and. (it .ne. 0)) then
        call pdlib_w3xypug_block_implicit(imod, facx, facx, dtg, vgx, vgy, ugdtupdate )
      else if(fstotalexp .and. (it .ne. 0)) then
        call pdlib_w3xypug_block_explicit(imod, facx, facx, dtg, vgx, vgy, ugdtupdate )
      endif
#endif
    else
      if (flcx .or. flcy) then
#ifdef w3_mpi
        if ( nrqsg1 .gt. 0 ) then
          call mpi_startall (nrqsg1, irqsg1(1,1), ierr_mpi)
          call mpi_startall (nrqsg1, irqsg1(1,2), ierr_mpi)
        end if
#endif
        ! initialize field variable
        field = 0.
        do ispec=1, nspec
          if ( iappro(ispec) .eq. iaproc ) then
            if (.not.lpdlib ) then
              print *,'yyy start gath'
              call w3gath ( ispec, field )
              print *,'yyy done gath'
            end if
            !
            if (gtype .eq. ungtype) then
              ix = 1
#ifdef w3_mpi
              if (.not. lpdlib) then
#endif
#ifdef w3_pr1
                call w3xypug ( ispec, facx, facx, dtg, field, vgx, vgy, ugdtupdate )
#endif
#ifdef w3_pr2
                call w3xypug ( ispec, facx, facx, dtg, field, vgx, vgy, ugdtupdate )
#endif
#ifdef w3_pr3
                call w3xypug ( ispec, facx, facx, dtg, field, vgx, vgy, ugdtupdate )
#endif
#ifdef w3_mpi
              end if
#endif
            else
              ix = 1
#ifdef w3_pr1
              call w3xyp1 ( ispec, dtg, mapsta, field, vgx, vgy )
#endif
#ifdef w3_pr2
              call w3xyp2 ( ispec, dtg, mapsta, mapfs, field, vgx, vgy )
#endif
#ifdef w3_pr3
              call w3xyp3 ( ispec, dtg, mapsta, mapfs, field, vgx, vgy )
#endif
            end if
            if (.not.lpdlib ) then
              call w3scat ( ispec, mapsta, field )
            end if
          end if
        end do
        !
        print *,'yyy here 01'
#ifdef w3_mpi
        if ( nrqsg1 .gt. 0 ) then
          allocate ( statco(mpi_status_size,nrqsg1) )
          call mpi_waitall (nrqsg1, irqsg1(1,1), statco, ierr_mpi)
          call mpi_waitall (nrqsg1, irqsg1(1,2), statco, ierr_mpi)
          deallocate ( statco )
        end if
#endif
      end if !if (flcx .or. flcy) then
    end if ! if (lpdlib) then
    print *,'yyy here 1'
        call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 17')
        !
        !
        ! 3.6.4 Intra-spectral part 2
        !
        if ( flcth .or. flck ) then
          do itloc=itloch+1, ntloc
            !
            !!$omp parallel private (jsea,isea,ix,iy,depth,ixrel)
            !!$omp do schedule (dynamic,1)
            !
            do jsea = 1, nseal

              call init_get_isea(isea, jsea)
              ix     = mapsf(isea,1)
              iy     = mapsf(isea,2)
              depth  = max ( dmin , dw(isea) )

              if ( gtype .eq. ungtype ) then
                if (lpdlib) then
#ifdef w3_pdlib
                  if (iobp_loc(jsea) .ne. 1) cycle
#endif
                else
                  if (iobp(isea) .ne. 1) cycle
                endif
              endif

              if ( mapsta(iy,ix) .eq. 1 ) then
                if (lpdlib) then
                  ixrel = jsea
                else
                  ixrel = ix
                end if
                j = 1
#ifdef w3_pr1
                call w3ktp1 ( isea, facth, fack, cthg0s(isea),       &
                     cg(:,isea), wn(:,isea), depth,                  &
                     dddx(iy,ixrel), dddy(iy,ixrel), cx(isea),       &
                     cy(isea), dcxdx(iy,ixrel), dcxdy(iy,ixrel),     &
                     dcydx(iy,ixrel), dcydy(iy,ixrel),               &
                     dcdx(:,iy,ixrel), dcdy(:,iy,ixrel), va(:,jsea))
#endif
#ifdef w3_pr2
                call w3ktp2 ( isea, facth, fack, cthg0s(isea),       &
                     cg(:,isea), wn(:,isea), depth,                  &
                     dddx(iy,ixrel), dddy(iy,ixrel), cx(isea),       &
                     cy(isea), dcxdx(iy,ixrel), dcxdy(iy,ixrel),     &
                     dcydx(iy,ixrel), dcydy(iy,ixrel),               &
                     dcdx(:,iy,ixrel), dcdy(:,iy,ixrel), va(:,jsea))
#endif
#ifdef w3_pr3
                call w3ktp3 ( isea, facth, fack, cthg0s(isea),       &
                     cg(:,isea), wn(:,isea), depth,                  &
                     dddx(iy,ixrel), dddy(iy,ixrel), cx(isea),       &
                     cy(isea), dcxdx(iy,ixrel), dcxdy(iy,ixrel),     &
                     dcydx(iy,ixrel), dcydy(iy,ixrel),               &
                     dcdx(:,iy,ixrel), dcdy(:,iy,ixrel), va(:,jsea), &
                     cflthmax(jsea), cflkmax(jsea) )
#endif
              end if
            end do
            !
            !!$omp end do
            !!$omp end parallel
            !
          end do
        end if
        !
        ugdtupdate = .false.
        print *,'yyy here 2'
        !
        ! 3.6 end propapgation  = = = = = = = = = = = = = = = = = = = = = = = =

        ! 3.7 Calculate and integrate source terms.
        !
370     continue
        if ( flsou ) then
          d50=0.0002
          reflec(:)=0.
          refled(:)=0
          psic=0.
          !
          !!$omp parallel private (jsea,isea,ix,iy,dela,delx,dely,        &
          !!$omp&                  reflec,refled,d50,psic,tmp1,tmp2,tmp3,tmp4)
          !!$omp do schedule (dynamic,1)
          !
          do jsea=1, nseal
            call init_get_isea(isea, jsea)
            ix     = mapsf(isea,1)
            iy     = mapsf(isea,2)
            dela=1.
            delx=1.
            dely=1.
#ifdef w3_ref1
            if (gtype.eq.rlgtype) then
              delx=sx*clats(isea)/facx
              dely=sy/facx
              dela=delx*dely
            end if
            if (gtype.eq.clgtype) then
              ! maybe what follows works also for rlgtype ... to be verified
              delx=hpfac(iy,ix)/ facx
              dely=hqfac(iy,ix)/ facx
              dela=delx*dely
            end if
#endif
#ifdef w3_ref1
            reflec=reflc(:,isea)
            reflec(4)=berg(isea)*reflec(4)
            refled=refld(:,isea)
#endif
#ifdef w3_bt4
            d50=sed_d50(isea)
            psic=sed_psic(isea)
#endif
            if ( mapsta(iy,ix) .eq. 1 .and. flagst(isea)) then
              tmp1   = whitecap(jsea,1:4)
              tmp2   = bedforms(jsea,1:3)
              tmp3   = taubbl(jsea,1:2)
              tmp4   = tauice(jsea,1:2)
#ifdef w3_pdlib
              if (fssource) then
                call w3srce(srce_imp_post,it,isea,jsea,ix,iy,imod,     &
                     vaold(:,jsea), va(:,jsea),                        &
                     vsiodummy,vdiodummy,shavetot(jsea),               &
                     alpha(1:nk,jsea), wn(1:nk,isea),                  &
                     cg(1:nk,isea), clats(isea), dw(isea), u10(isea),  &
                     u10d(isea),                                       &
#ifdef w3_flx5
                     taua(isea), tauadir(isea),                        &
#endif
                     as(isea), ust(isea),                              &
                     ustdir(isea), cx(isea), cy(isea),                 &
                     ice(isea), iceh(isea), icef(isea),                &
                     icedmax(isea),                                    &
                     reflec, refled, delx, dely, dela,                 &
                     trnx(iy,ix), trny(iy,ix), berg(isea),             &
                     fpis(isea), dtdyn(jsea),                          &
                     fcut(jsea), dtg, tauwx(jsea), tauwy(jsea),        &
                     tauox(jsea), tauoy(jsea), tauwix(jsea),           &
                     tauwiy(jsea), tauwnx(jsea),                       &
                     tauwny(jsea),  phiaw(jsea), charn(jsea),          &
                     tws(jsea),phioc(jsea), tmp1, d50, psic, tmp2,     &
                     phibbl(jsea), tmp3, tmp4, phice(jsea),            &
                     tauocx(jsea), tauocy(jsea), wnmean(jsea),         &
                     rhoair(isea), asf(isea))
              else
#endif
                call w3srce(srce_direct, it, isea, jsea, ix, iy, imod, &
                     vaolddummy, va(:,jsea),                           &
                     vsiodummy, vdiodummy, shavetotiodummy,            &
                     alpha(1:nk,jsea), wn(1:nk,isea),                  &
                     cg(1:nk,isea), clats(isea), dw(isea), u10(isea),  &
                     u10d(isea),                                       &
#ifdef w3_flx5
                     taua(isea), tauadir(isea),                        &
#endif
                     as(isea), ust(isea),                              &
                     ustdir(isea), cx(isea), cy(isea),                 &
                     ice(isea), iceh(isea), icef(isea),                &
                     icedmax(isea),                                    &
                     reflec, refled, delx, dely, dela,                 &
                     trnx(iy,ix), trny(iy,ix), berg(isea),             &
                     fpis(isea), dtdyn(jsea),                          &
                     fcut(jsea), dtg, tauwx(jsea), tauwy(jsea),        &
                     tauox(jsea), tauoy(jsea), tauwix(jsea),           &
                     tauwiy(jsea), tauwnx(jsea),                       &
                     tauwny(jsea),  phiaw(jsea), charn(jsea),          &
                     tws(jsea), phioc(jsea), tmp1, d50, psic,tmp2,     &
                     phibbl(jsea), tmp3, tmp4 , phice(jsea),           &
                     tauocx(jsea), tauocy(jsea), wnmean(jsea),         &
                     rhoair(isea), asf(isea))
#ifdef w3_pdlib
              end if
#endif
              whitecap(jsea,1:4) = tmp1
              bedforms(jsea,1:3) = tmp2
              taubbl(jsea,1:2) = tmp3
              tauice(jsea,1:2) = tmp4
            else
              ust   (isea) = undef
              ustdir(isea) = undef
              dtdyn (jsea) = undef
              fcut  (jsea) = undef
              !                    va(:,jsea)  = 0.
            end if
          end do
          !
          !!$omp end do
          !!$omp end parallel
          !
       end if
        print *,'yyy here 3'
#ifdef test
        ! output
        call w3outg(va, .true., .true., .false.)
        do_startall = .true.
        print *,'ZZZ done w3outg ',nrqgo, irqgo,flgmpi(0),flgmpi(1)

        if (do_startall) then
          if (.not. lpdlib) then
            if (nrqgo .ne.0 ) then
              call mpi_startall ( nrqgo, irqgo , ierr_mpi )
              flgmpi(0) = .true.
              nrqmax = max ( nrqmax , nrqgo )
            end if
            print *,'ZZZ done 0'
            !
            if (nrqgo2.ne.0 ) then
              call mpi_startall ( nrqgo2, irqgo2, ierr_mpi )
              flgmpi(1) = .true.
              nrqmax = max ( nrqmax , nrqgo2 )
            end if
            print *,'ZZZ done 1'
          else
          end if ! if (.not. lpdlib) then
        end if ! if (do_startall)

        if ( flgmpi(0) )call mpi_waitall( nrqgo, irqgo, statio, ierr_mpi )
        flgmpi(0) = .false.
        print *,'ZZZ done 2'
        if ( iaproc .eq. napfld ) then
           if ( flgmpi(1) ) call mpi_waitall( nrqgo2, irqgo2, statio, ierr_mpi )
           flgmpi(1) = .false.
           print *,'ZZZ call nc'
           call w3iogoncd ()
           print *,'ZZZ done nc'
        end if

        if ( flgmpi(0) ) call mpi_waitall ( nrqgo, irqgo , statio, ierr_mpi )
        if (user_netcdf_grdout) then
          if ( flgmpi(1) .and. ( iaproc .eq. napfld ) ) then
            call mpi_waitall ( nrqgo2, irqgo2 , statio, ierr_mpi )
          end if
       end if
#endif
        !
        ! end of interations for dtmax < 1s
        !
        !
        !
        ! 3.8 update global time step.
        !     (branch point fldry, it=0)
        !
380     continue
        !
        !if (it.ne.nt) then
        !  dttst  = dsec21 ( time , tcalc )
        !  dtg    = dttst / real(nt-it)
        !end if
        !
        ! if ( flact .and. it.ne.nt .and. iaproc.eq.naplog ) then
        !   call stme21 ( time , idtime )
        !   if ( idlast .ne. time(1) ) then
        !     write (ndso,900) itime, ipass, idtime(01:19), idact, outid
        !     idlast = time(1)
        !   else
        !     write (ndso,901) itime, ipass, idtime(12:19), idact, outid
        !   end if
        !   flact  = .false.
        !   idact  = '         '
        ! end if
        !
        !
        !
        !end do ! do it = it0, nt

        !call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE END TIME LOOP')
        !
        !if ( iaproc .eq. naplog ) write (ndso,902)
        !
        deallocate(field)
        deallocate(tauwx, tauwy)
        !
        call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE END W3WAVE')
        !
        return
      end subroutine w3step
end module wav_step_mod
