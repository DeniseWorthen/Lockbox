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

  use w3parall,  only : init_get_isea
  use w3pro3md,  only : w3ktp3, w3xyp3
  use w3profsmd, only : w3xypug
  use w3servmd,  only : print_memcheck
  use w3srcemd,  only : w3srce
  use w3timemd,  only : dsec21
  use w3triamd,  only : ug_gradients
  use w3updtmd,  only : w3dzxy
  use w3wavemd,  only : w3gath, w3scat
  use constants, only : undef, dera, radius, lpdlib, srce_direct

  use w3adatmd,  only : iappro
  use w3adatmd,  only : cx, cy, dtdyn, fcut, u10, u10d, as, tauox, tauoy
  use w3adatmd,  only : dw, cg, wn, dcdx, dcdy, dddx, dddy, dcxdx, dcxdy
  use w3adatmd,  only : dcydx, dcydy, cflthmax, cflkmax, tauwix, tauwiy
  use w3adatmd,  only : tauwnx, tauwny, phiaw, charn, tws, phioc, phibbl
  use w3adatmd,  only : whitecap, bedforms, taubbl, tauice, alpha, phice
  use w3adatmd,  only : tauocx, tauocy, wnmean

  use w3gdatmd,  only : mapsf, mapfs, gtype, ungtype, iobp, cthg0s
  use w3gdatmd,  only : flcth, flck, flcx, flcy, flagll, flsou, flagst
  use w3gdatmd,  only : fsrefraction, fsfreqshift
  use w3gdatmd,  only : nk, nspec, nsea, nseal, nx, ny, mapsta
  use w3gdatmd,  only : clats, dmin, trnx, trny
  use w3gdatmd,  only : dtcfli, dth

  use w3odatmd,  only : iaproc, naplog, ndso

  use w3wdatmd,  only : va, ust, ustdir, ice, iceh, icef, icedmax, berg
  use w3wdatmd,  only : fpis, rhoair, asf
#ifdef W3_MPI
  use w3adatmd,  only : mpibuf, nrqsg1, nrqsg2, irqsg1
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
!     use w3odatmd      , only : histwr, rstwr, user_netcdf_grdout
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
    real :: vgx, vgy
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
    logical :: flddir
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
    !     logical :: do_startall
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
#ifdef W3_UOST
    CALL UOST_SETGRID(IMOD)
#endif
    !
    ALLOCATE(TAUWX(NSEAL), TAUWY(NSEAL))
#ifdef W3_REFRX
    ALLOCATE(CIK(NSEAL))
#endif
    ALLOCATE ( FIELD(1-NY:NY*(NX+2)) )
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
    !
    ! 0.b Subroutine tracing
    !
    !
    !
    !IF ( FLDDIR ) THEN
    IF (GTYPE .EQ. UNGTYPE) THEN
      CALL UG_GRADIENTS(DW, DDDX, DDDY)
    ELSE
      CALL W3DZXY(DW(1:UBOUND(DW,1)),'m',DDDX,DDDY)
    END IF
    FLDDIR = .FALSE.
    !END IF

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 12')
    !
    ! Calculate PHASE SPEED GRADIENT.
    DCDX = 0.
    DCDY = 0.
#ifdef W3_REFRX
    CIK  = 0.
    !
    IF (GTYPE .NE. UNGTYPE) THEN
      DO IK=0,NK+1
        CIK = SIG(IK) / WN(IK,1:NSEA)
        CALL W3DZXY(CIK,'m/s',DCDX(IK,:,:),DCDY(IK,:,:))
      END DO
    ELSE
      WRITE (NDSE,1040)
      CALL EXTCDE(2)
      ! CALL UG_GRADIENTS(CMN, DCDX, DCDY) !/ Stefan, to be confirmed!
    END IF
#endif
    !
    !
    !FLIWND = .FALSE.
    !FLFRST = .FALSE.
    !
#ifdef W3_PDLIB
    IF (IT .eq. 0) THEN
      DTGpre = 1.
    ELSE
      DTGpre = DTG
    END IF
    IF (LPDLIB .and. FLSOU .and. FSSOURCE) THEN
      !$OMP PARALLEL DO PRIVATE (JSEA,ISEA,IX,IY) SCHEDULE (DYNAMIC,1)
      D50=0.0002
      REFLEC(:)=0.
      REFLED(:)=0
      PSIC=0.

      IF (LSLOC) THEN
        B_JAC     = 0.
        ASPAR_JAC = 0.
      ELSE
        VSTOT = 0.
        VDTOT = 0.
      ENDIF

      DO JSEA = 1, NP

        CALL INIT_GET_ISEA(ISEA, JSEA)

        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        DELA=1.
        DELX=1.
        DELY=1.

#ifdef W3_REF1
        IF (GTYPE.EQ.RLGTYPE) THEN
          DELX=SX*CLATS(ISEA)/FACX
          DELY=SY/FACX
          DELA=DELX*DELY
        END IF
        IF (GTYPE.EQ.CLGTYPE) THEN
          ! Maybe what follows works also for RLGTYPE ... to be verified
          DELX=HPFAC(IY,IX)/ FACX
          DELY=HQFAC(IY,IX)/ FACX
          DELA=DELX*DELY
        END IF
        REFLEC=REFLC(:,ISEA)
        REFLEC(4)=BERG(ISEA)*REFLEC(4)
        REFLED=REFLD(:,ISEA)
#endif

#ifdef W3_BT4
        D50=SED_D50(ISEA)
        PSIC=SED_PSIC(ISEA)
#endif
        !
        CALL W3SRCE(srce_imp_pre, IT, ISEA, JSEA, IX, IY, IMOD, &
             VAold(:,JSEA), VA(:,JSEA),                         &
             VSioDummy, VDioDummy, SHAVETOT(JSEA),              &
             ALPHA(1:NK,JSEA), WN(1:NK,ISEA),                   &
             CG(1:NK,ISEA), CLATS(ISEA), DW(ISEA), U10(ISEA),   &
             U10D(ISEA),                                        &
#ifdef W3_FLX5
             TAUA(ISEA), TAUADIR(ISEA),                         &
#endif
             AS(ISEA), UST(ISEA),                               &
             USTDIR(ISEA), CX(ISEA), CY(ISEA),                  &
             ICE(ISEA), ICEH(ISEA), ICEF(ISEA),                 &
             ICEDMAX(ISEA),                                     &
             REFLEC, REFLED, DELX, DELY, DELA,                  &
             TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),              &
             FPIS(ISEA), DTDYN(JSEA),                           &
             FCUT(JSEA), DTGpre, TAUWX(JSEA), TAUWY(JSEA),      &
             TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),            &
             TAUWIY(JSEA), TAUWNX(JSEA),                        &
             TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),           &
             TWS(JSEA), PHIOC(JSEA), TMP1, D50, PSIC, TMP2,     &
             PHIBBL(JSEA), TMP3, TMP4, PHICE(JSEA),             &
             TAUOCX(JSEA), TAUOCY(JSEA), WNMEAN(JSEA),          &
             RHOAIR(ISEA), ASF(ISEA))
        IF (.not. LSLOC) THEN
          VSTOT(:,JSEA) = VSioDummy
          VDTOT(:,JSEA) = VDioDummy
        ENDIF
      END DO ! JSEA
    END IF ! PDLIB
#endif
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 14')
    !
    ! 3.6 Perform Propagation = = = = = = = = = = = = = = = = = = = = = = =
    ! 3.6.1 Preparations
    !
    NTLOC  = 1 + INT( DTG/DTCFLI - 0.001 )
    !
    FACTH  = DTG / (DTH*REAL(NTLOC))
    FACK   = DTG / REAL(NTLOC)

    TTEST(1) = TIME(1)
    TTEST(2) = 0
    DTTEST = DSEC21(TTEST,TIME)
    ITLOCH = ( NTLOC + 1 - MOD(NINT(DTTEST/DTG),2) ) / 2
    !
    ! 3.6.2 Intra-spectral part 1
    !
    IF ( FLCTH .OR. FLCK ) THEN
      DO ITLOC=1, ITLOCH
        !
        !$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DEPTH,IXrel)
        !$OMP DO SCHEDULE (DYNAMIC,1)
        !
        DO JSEA=1, NSEAL
          CALL INIT_GET_ISEA(ISEA, JSEA)
          IX     = MAPSF(ISEA,1)
          IY     = MAPSF(ISEA,2)

          IF ( GTYPE .EQ. UNGTYPE ) THEN
            IF (LPDLIB) THEN
#ifdef W3_PDLIB
              IF (IOBP_LOC(JSEA) .NE. 1) CYCLE
#endif
            ELSE
              IF (IOBP(ISEA) .NE. 1) CYCLE
            ENDIF
          ENDIF

          IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
            DEPTH  = MAX ( DMIN , DW(ISEA) )
            IF (LPDLIB) THEN
              IXrel = JSEA
            ELSE
              IXrel = IX
            END IF
            !
            J = 1
            !
#ifdef W3_PR1
            CALL W3KTP1 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                 CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                 DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                 CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                 DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                 DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR2
            CALL W3KTP2 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                 CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                 DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                 CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                 DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                 DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR3
            CALL W3KTP3 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                 CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                 DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                 CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                 DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                 DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA), &
                 CFLTHMAX(JSEA), CFLKMAX(JSEA) )
#endif
          end if
        END DO
        !
        !$OMP END DO
        !$OMP END PARALLEL
        !
      END DO
    END IF

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 16')

    !
    ! 3.6.3 Longitude-latitude
    !       (time step correction in routine)
    !
    IF (GTYPE .EQ. UNGTYPE) THEN
      IF (FLAGLL) THEN
        FACX   =  1./(DERA * RADIUS)
      ELSE
        FACX   =  1.
      END IF
    END IF

    IF (LPDLIB) THEN
#ifdef W3_PDLIB
      IF (FLCX .or. FLCY) THEN
        IF (.NOT. FSTOTALIMP .AND. .NOT. FSTOTALEXP) THEN
          DO ISPEC=1,NSPEC
            CALL PDLIB_W3XYPUG ( ISPEC, FACX, FACX, DTG, VGX, VGY, UGDTUPDATE )
          END DO
        END IF
      END IF
      IF (FSTOTALIMP .and. (IT .ne. 0)) THEN
        CALL PDLIB_W3XYPUG_BLOCK_IMPLICIT(IMOD, FACX, FACX, DTG, VGX, VGY, UGDTUPDATE )
      ELSE IF(FSTOTALEXP .and. (IT .ne. 0)) THEN
        CALL PDLIB_W3XYPUG_BLOCK_EXPLICIT(IMOD, FACX, FACX, DTG, VGX, VGY, UGDTUPDATE )
      ENDIF
#endif
    ELSE
      IF (FLCX .or. FLCY) THEN
#ifdef W3_MPI
        IF ( NRQSG1 .GT. 0 ) THEN
          CALL MPI_STARTALL (NRQSG1, IRQSG1(1,1), IERR_MPI)
          CALL MPI_STARTALL (NRQSG1, IRQSG1(1,2), IERR_MPI)
        END IF
#endif
        ! Initialize FIELD variable
        FIELD = 0.
        DO ISPEC=1, NSPEC
          IF ( IAPPRO(ISPEC) .EQ. IAPROC ) THEN
            IF (.NOT.LPDLIB ) THEN
              CALL W3GATH ( ISPEC, FIELD )
            END IF
            !
            IF (GTYPE .EQ. UNGTYPE) THEN
              IX = 1
#ifdef W3_MPI
              IF (.NOT. LPDLIB) THEN
#endif
#ifdef W3_PR1
                CALL W3XYPUG ( ISPEC, FACX, FACX, DTG, FIELD, VGX, VGY, UGDTUPDATE )
#endif
#ifdef W3_PR2
                CALL W3XYPUG ( ISPEC, FACX, FACX, DTG, FIELD, VGX, VGY, UGDTUPDATE )
#endif
#ifdef W3_PR3
                CALL W3XYPUG ( ISPEC, FACX, FACX, DTG, FIELD, VGX, VGY, UGDTUPDATE )
#endif
#ifdef W3_MPI
              END IF
#endif
            ELSE
              IX = 1
#ifdef W3_PR1
              CALL W3XYP1 ( ISPEC, DTG, MAPSTA, FIELD, VGX, VGY )
#endif
#ifdef W3_PR2
              CALL W3XYP2 ( ISPEC, DTG, MAPSTA, MAPFS, FIELD, VGX, VGY )
#endif
#ifdef W3_PR3
              CALL W3XYP3 ( ISPEC, DTG, MAPSTA, MAPFS, FIELD, VGX, VGY )
#endif
            END IF
            IF (.NOT.LPDLIB ) THEN
              CALL W3SCAT ( ISPEC, MAPSTA, FIELD )
            END IF
          END IF
        END DO
        !
#ifdef W3_MPI
        IF ( NRQSG1 .GT. 0 ) THEN
          ALLOCATE ( STATCO(MPI_STATUS_SIZE,NRQSG1) )
          CALL MPI_WAITALL (NRQSG1, IRQSG1(1,1), STATCO, IERR_MPI)
          CALL MPI_WAITALL (NRQSG1, IRQSG1(1,2), STATCO, IERR_MPI)
          DEALLOCATE ( STATCO )
        END IF
#endif
      end if !IF (FLCX .or. FLCY) THEN
    end if ! IF (LPDLIB) THEN
        call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE TIME LOOP 17')
        !
        !
        ! 3.6.4 Intra-spectral part 2
        !
        IF ( FLCTH .OR. FLCK ) THEN
          DO ITLOC=ITLOCH+1, NTLOC
            !
            !$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DEPTH,IXrel)
            !$OMP DO SCHEDULE (DYNAMIC,1)
            !
            DO JSEA = 1, NSEAL

              CALL INIT_GET_ISEA(ISEA, JSEA)
              IX     = MAPSF(ISEA,1)
              IY     = MAPSF(ISEA,2)
              DEPTH  = MAX ( DMIN , DW(ISEA) )

              IF ( GTYPE .EQ. UNGTYPE ) THEN
                IF (LPDLIB) THEN
#ifdef W3_PDLIB
                  IF (IOBP_LOC(JSEA) .NE. 1) CYCLE
#endif
                ELSE
                  IF (IOBP(ISEA) .NE. 1) CYCLE
                ENDIF
              ENDIF

              IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                IF (LPDLIB) THEN
                  IXrel = JSEA
                ELSE
                  IXrel = IX
                END IF
                J = 1
#ifdef W3_PR1
                CALL W3KTP1 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                     CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                     DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                     CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                     DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                     DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR2
                CALL W3KTP2 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                     CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                     DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                     CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                     DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                     DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA))
#endif
#ifdef W3_PR3
                CALL W3KTP3 ( ISEA, FACTH, FACK, CTHG0S(ISEA),       &
                     CG(:,ISEA), WN(:,ISEA), DEPTH,                  &
                     DDDX(IY,IXrel), DDDY(IY,IXrel), CX(ISEA),       &
                     CY(ISEA), DCXDX(IY,IXrel), DCXDY(IY,IXrel),     &
                     DCYDX(IY,IXrel), DCYDY(IY,IXrel),               &
                     DCDX(:,IY,IXrel), DCDY(:,IY,IXrel), VA(:,JSEA), &
                     CFLTHMAX(JSEA), CFLKMAX(JSEA) )
#endif
              END IF
            END DO
            !
            !$OMP END DO
            !$OMP END PARALLEL
            !
          END DO
        END IF
        !
        UGDTUPDATE = .FALSE.
        !
        ! 3.6 End propapgation  = = = = = = = = = = = = = = = = = = = = = = = =

        ! 3.7 Calculate and integrate source terms.
        !
370     CONTINUE
        IF ( FLSOU ) THEN
          D50=0.0002
          REFLEC(:)=0.
          REFLED(:)=0
          PSIC=0.
          !
          !$OMP PARALLEL PRIVATE (JSEA,ISEA,IX,IY,DELA,DELX,DELY,        &
          !$OMP&                  REFLEC,REFLED,D50,PSIC,TMP1,TMP2,TMP3,TMP4)
          !$OMP DO SCHEDULE (DYNAMIC,1)
          !
          DO JSEA=1, NSEAL
            CALL INIT_GET_ISEA(ISEA, JSEA)
            IX     = MAPSF(ISEA,1)
            IY     = MAPSF(ISEA,2)
            DELA=1.
            DELX=1.
            DELY=1.
#ifdef W3_REF1
            IF (GTYPE.EQ.RLGTYPE) THEN
              DELX=SX*CLATS(ISEA)/FACX
              DELY=SY/FACX
              DELA=DELX*DELY
            END IF
            IF (GTYPE.EQ.CLGTYPE) THEN
              ! Maybe what follows works also for RLGTYPE ... to be verified
              DELX=HPFAC(IY,IX)/ FACX
              DELY=HQFAC(IY,IX)/ FACX
              DELA=DELX*DELY
            END IF
#endif
#ifdef W3_REF1
            REFLEC=REFLC(:,ISEA)
            REFLEC(4)=BERG(ISEA)*REFLEC(4)
            REFLED=REFLD(:,ISEA)
#endif
#ifdef W3_BT4
            D50=SED_D50(ISEA)
            PSIC=SED_PSIC(ISEA)
#endif
            IF ( MAPSTA(IY,IX) .EQ. 1 .AND. FLAGST(ISEA)) THEN
              TMP1   = WHITECAP(JSEA,1:4)
              TMP2   = BEDFORMS(JSEA,1:3)
              TMP3   = TAUBBL(JSEA,1:2)
              TMP4   = TAUICE(JSEA,1:2)
#ifdef W3_PDLIB
              IF (FSSOURCE) THEN
                CALL W3SRCE(srce_imp_post,IT,ISEA,JSEA,IX,IY,IMOD,     &
                     VAOLD(:,JSEA), VA(:,JSEA),                        &
                     VSioDummy,VDioDummy,SHAVETOT(JSEA),               &
                     ALPHA(1:NK,JSEA), WN(1:NK,ISEA),                  &
                     CG(1:NK,ISEA), CLATS(ISEA), DW(ISEA), U10(ISEA),  &
                     U10D(ISEA),                                       &
#ifdef W3_FLX5
                     TAUA(ISEA), TAUADIR(ISEA),                        &
#endif
                     AS(ISEA), UST(ISEA),                              &
                     USTDIR(ISEA), CX(ISEA), CY(ISEA),                 &
                     ICE(ISEA), ICEH(ISEA), ICEF(ISEA),                &
                     ICEDMAX(ISEA),                                    &
                     REFLEC, REFLED, DELX, DELY, DELA,                 &
                     TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),             &
                     FPIS(ISEA), DTDYN(JSEA),                          &
                     FCUT(JSEA), DTG, TAUWX(JSEA), TAUWY(JSEA),        &
                     TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),           &
                     TAUWIY(JSEA), TAUWNX(JSEA),                       &
                     TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),          &
                     TWS(JSEA),PHIOC(JSEA), TMP1, D50, PSIC, TMP2,     &
                     PHIBBL(JSEA), TMP3, TMP4, PHICE(JSEA),            &
                     TAUOCX(JSEA), TAUOCY(JSEA), WNMEAN(JSEA),         &
                     RHOAIR(ISEA), ASF(ISEA))
              ELSE
#endif
                CALL W3SRCE(srce_direct, IT, ISEA, JSEA, IX, IY, IMOD, &
                     VAoldDummy, VA(:,JSEA),                           &
                     VSioDummy, VDioDummy, SHAVETOTioDummy,            &
                     ALPHA(1:NK,JSEA), WN(1:NK,ISEA),                  &
                     CG(1:NK,ISEA), CLATS(ISEA), DW(ISEA), U10(ISEA),  &
                     U10D(ISEA),                                       &
#ifdef W3_FLX5
                     TAUA(ISEA), TAUADIR(ISEA),                        &
#endif
                     AS(ISEA), UST(ISEA),                              &
                     USTDIR(ISEA), CX(ISEA), CY(ISEA),                 &
                     ICE(ISEA), ICEH(ISEA), ICEF(ISEA),                &
                     ICEDMAX(ISEA),                                    &
                     REFLEC, REFLED, DELX, DELY, DELA,                 &
                     TRNX(IY,IX), TRNY(IY,IX), BERG(ISEA),             &
                     FPIS(ISEA), DTDYN(JSEA),                          &
                     FCUT(JSEA), DTG, TAUWX(JSEA), TAUWY(JSEA),        &
                     TAUOX(JSEA), TAUOY(JSEA), TAUWIX(JSEA),           &
                     TAUWIY(JSEA), TAUWNX(JSEA),                       &
                     TAUWNY(JSEA),  PHIAW(JSEA), CHARN(JSEA),          &
                     TWS(JSEA), PHIOC(JSEA), TMP1, D50, PSIC,TMP2,     &
                     PHIBBL(JSEA), TMP3, TMP4 , PHICE(JSEA),           &
                     TAUOCX(JSEA), TAUOCY(JSEA), WNMEAN(JSEA),         &
                     RHOAIR(ISEA), ASF(ISEA))
#ifdef W3_PDLIB
              END IF
#endif
              WHITECAP(JSEA,1:4) = TMP1
              BEDFORMS(JSEA,1:3) = TMP2
              TAUBBL(JSEA,1:2) = TMP3
              TAUICE(JSEA,1:2) = TMP4
            ELSE
              UST   (ISEA) = UNDEF
              USTDIR(ISEA) = UNDEF
              DTDYN (JSEA) = UNDEF
              FCUT  (JSEA) = UNDEF
              !                    VA(:,JSEA)  = 0.
            END IF
          END DO
          !
          !$OMP END DO
          !$OMP END PARALLEL
          !
        END IF
        !
        ! End of interations for DTMAX < 1s
        !
        !
        !
        ! 3.8 Update global time step.
        !     (Branch point FLDRY, IT=0)
        !
380     CONTINUE
        !
        !IF (IT.NE.NT) THEN
        !  DTTST  = DSEC21 ( TIME , TCALC )
        !  DTG    = DTTST / REAL(NT-IT)
        !END IF
        !
        ! IF ( FLACT .AND. IT.NE.NT .AND. IAPROC.EQ.NAPLOG ) THEN
        !   CALL STME21 ( TIME , IDTIME )
        !   IF ( IDLAST .NE. TIME(1) ) THEN
        !     WRITE (NDSO,900) ITIME, IPASS, IDTIME(01:19), IDACT, OUTID
        !     IDLAST = TIME(1)
        !   ELSE
        !     WRITE (NDSO,901) ITIME, IPASS, IDTIME(12:19), IDACT, OUTID
        !   END IF
        !   FLACT  = .FALSE.
        !   IDACT  = '         '
        ! END IF
        !
        !
        !
        !END DO ! DO IT = IT0, NT

        call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE END TIME LOOP')
        !
        !     End of loop over time steps
        ! ==================================================================== /
        !
400     CONTINUE
        call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE AFTER TIME LOOP 5')
        !
        !IF ( IAPROC .EQ. NAPLOG ) WRITE (NDSO,902)
        !
        DEALLOCATE(FIELD)
        DEALLOCATE(TAUWX, TAUWY)
        !
        call print_memcheck(memunit, 'memcheck_____:'//' WW3_WAVE END W3WAVE')
        !
        RETURN
!         !
!         ! Formats
!         !
! 900     FORMAT (4X,I6,'|',I6,'| ', A19  ,' | ',A,' | ',A,' |')
! 901     FORMAT (4X,I6,'|',I6,'| ',11X,A8,' | ',A,' | ',A,' |')
! 902     FORMAT (2X,'--------+------+---------------------+'                  &
!              ,'-----------------------+------------------+')
!         !
! #ifdef W3_IC3
! 920     FORMAT ('     Updating k and Cg from ice param. 1,2,3,4.'/)
! #endif
! 950     FORMAT ('  WAVEWATCH III calculating for ',A,' at ',A)
! 951     FORMAT ('  WAVEWATCH III reached the end of a computation',          &
!              ' loop at ',A)
! 1000    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     ENDING TIME BEFORE STARTING TIME '/)
! 1001    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     NEW WATER LEVEL BEFORE OLD WATER LEVEL '/)
! 1002    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     ILLEGAL CURRENT INTERVAL '/)
! 1003    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     ILLEGAL WIND INTERVAL '/)
! 1004    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     NEW ICE FIELD BEFORE OLD ICE FIELD '/)
! 1005    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     NEW IC1 FIELD BEFORE OLD IC1 FIELD '/)
! 1007    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     NEW ATM MOMENTUM BEFORE OLD ATM MOMENTUM '/)
! 1008    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     NEW AIR DENSITY BEFORE OLD AIR DENSITY '/)
! #ifdef W3_IS2
! 1006    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     NEW IC5 FIELD BEFORE OLD IC5 FIELD '/)
! #endif
! 1030    FORMAT (/' *** WAVEWATCH III WARING IN W3WAVE :'/                   &
!              '     AT LEAST ONE PROCESSOR HAS 0 ACTIVE POINTS',              &
!              ' IN GRID',I3)
! #ifdef W3_REFRX
! 1040    FORMAT (/' *** WAVEWATCH III ERROR IN W3WAVE :'/                    &
!              '     EXPERIMENTAL FEATURE !/REFRX NOT FULLY IMPLEMENTED.'/)
! #endif
      end subroutine w3step
end module wav_step_mod
