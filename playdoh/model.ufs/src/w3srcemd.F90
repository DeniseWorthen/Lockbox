!> @file
!> @brief Source term integration routine.
!>
!> @author H. L. Tolman
!> @author F. Ardhuin
!> @date   22-Mar-2021
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /

!>
!> @brief Source term integration routine.
!>
!> @author H. L. Tolman
!> @author F. Ardhuin
!> @date   22-Mar-2021
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3SRCEMD
  REAL, PARAMETER, PRIVATE:: OFFSET = 1.
  !/
CONTAINS
  SUBROUTINE W3SRCE ( srce_call, IT, ISEA, JSEA, IX, IY, IMOD,          &
       SPECOLD, SPEC, VSIO, VDIO, SHAVEIO,         &
       ALPHA, WN1, CG1, CLATSL,                    &
       D_INP, U10ABS, U10DIR,                      &
#ifdef W3_FLX5
       TAUA, TAUADIR,                              &
#endif
       AS, USTAR, USTDIR,                          &
       CX, CY,  ICE, ICEH, ICEF, ICEDMAX,          &
       REFLEC, REFLED, DELX, DELY, DELA, TRNX,     &
       TRNY, BERG, FPI, DTDYN, FCUT, DTG, TAUWX,   &
       TAUWY, TAUOX, TAUOY, TAUWIX, TAUWIY, TAUWNX,&
       TAUWNY, PHIAW, CHARN, TWS, PHIOC, WHITECAP, &
       D50, PSIC, BEDFORM , PHIBBL, TAUBBL, TAUICE,&
       PHICE, TAUOCX, TAUOCY, WNMEAN, DAIR, COEF)
    USE CONSTANTS, ONLY: DWAT, srce_imp_post, srce_imp_pre,         &
         srce_direct, GRAV, TPI, TPIINV
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, TH, DMIN, DTMAX,       &
         DTMIN, FACTI1, FACTI2, FACSD, FACHFA, FACP, &
         XFC, XFLT, XREL, XFT, FXFM, FXPM, DDEN,     &
         FTE, FTF, FHMAX, ECOS, ESIN, IICEDISP,      &
         ICESCALES, IICESMOOTH
    USE W3WDATMD, ONLY: TIME
    USE W3ODATMD, ONLY: NDSE, NDST, IAPROC
    USE W3IDATMD, ONLY: INFLAGS2
    USE W3DISPMD
#ifdef W3_T
    USE CONSTANTS, ONLY: RADE
#endif
#ifdef W3_REF1
    USE W3GDATMD, ONLY: IOBP, IOBPD, GTYPE, UNGTYPE, REFPARS
#endif
#ifdef W3_NNT
    USE W3ODATMD, ONLY: IAPROC, SCREEN, FNMPRE
#endif
#ifdef W3_FLD1
    USE W3FLD1MD, ONLY: W3FLD1
    USE W3GDATMD, ONLY: AALPHA
#endif
#ifdef W3_FLD2
    USE W3FLD2MD, ONLY: W3FLD2
    USE W3GDATMD, ONLY: AALPHA
#endif
#ifdef W3_FLX1
    USE W3FLX1MD
#endif
#ifdef W3_FLX2
    USE W3FLX2MD
#endif
#ifdef W3_FLX3
    USE W3FLX3MD
#endif
#ifdef W3_FLX4
    USE W3FLX4MD
#endif
#ifdef W3_FLX5
    USE W3FLX5MD
#endif
#ifdef W3_LN1
    USE W3SLN1MD
#endif
#ifdef W3_ST0
    USE W3SRC0MD
#endif
#ifdef W3_ST1
    USE W3SRC1MD
#endif
#ifdef W3_ST2
    USE W3SRC2MD
    USE W3GDATMD, ONLY : ZWIND
#endif
#ifdef W3_ST3
    USE W3SRC3MD
    USE W3GDATMD, ONLY : ZZWND, FFXFM, FFXPM
#endif
#ifdef W3_ST4
    USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
    USE W3GDATMD, ONLY : ZZWND, FFXFM, FFXPM, FFXFA
#endif
#ifdef W3_ST6
    USE W3SRC6MD
    USE W3SWLDMD, ONLY : W3SWL6
    USE W3GDATMD, ONLY : SWL6S6
#endif
#ifdef W3_NL1
    USE W3SNL1MD
#endif
#ifdef W3_NL2
    USE W3SNL2MD
#endif
#ifdef W3_NL3
    USE W3SNL3MD
#endif
#ifdef W3_NL4
    USE W3SNL4MD
#endif
#ifdef W3_NL5
    USE W3SNL5MD
    USE W3TIMEMD, ONLY: TICK21
#endif
#ifdef W3_NLS
    USE W3SNLSMD
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_IC4
    USE W3SIC4MD
#endif
#ifdef W3_IC5
    USE W3SIC5MD
#endif
#ifdef W3_IS1
    USE W3SIS1MD
#endif
#ifdef W3_IS2
    USE W3SIS2MD
    USE W3GDATMD, ONLY : IS2PARS
#endif
#ifdef W3_DB1
    USE W3SDB1MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
#ifdef W3_BS1
    USE W3SBS1MD
#endif
#ifdef W3_REF1
    USE W3REF1MD
#endif
#ifdef W3_IG1
    USE W3GDATMD, ONLY : IGPARS
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
#ifdef W3_NNT
    USE W3SERVMD, ONLY: EXTCDE
#endif
#ifdef W3_UOST
    USE W3UOSTMD, ONLY: UOST_SRCTRMCOMPUTE
#endif
#ifdef W3_PDLIB
    USE PDLIB_W3PROFSMD, ONLY : B_JAC, ASPAR_JAC, ASPAR_DIAG_ALL
    USE yowNodepool, ONLY: PDLIB_I_DIAG, PDLIB_SI
    USE W3GDATMD, ONLY: B_JGS_LIMITER, FSSOURCE, optionCall
    USE W3GDATMD, ONLY: IOBP_LOC, IOBPD_LOC, B_JGS_LIMITER_FUNC
    USE W3WDATMD, ONLY: VA
    USE W3PARALL, ONLY: IMEM, LSLOC
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: srce_call, IT, ISEA, JSEA, IX, IY, IMOD
    REAL, intent(in)        :: SPECOLD(NSPEC), CLATSL
    REAL, INTENT(OUT)       :: VSIO(NSPEC), VDIO(NSPEC)
    LOGICAL, INTENT(OUT)    :: SHAVEIO
    REAL, INTENT(IN)        :: D_INP, U10ABS,     &
         U10DIR, AS, CX, CY, DTG, D50,PSIC,   &
         ICE, ICEH
#ifdef W3_FLX5
    REAL, INTENT(IN)        :: TAUA, TAUADIR
#endif
    INTEGER, INTENT(IN)     :: REFLED(6)
    REAL, INTENT(IN)        :: REFLEC(4), DELX, DELY, DELA,         &
         TRNX, TRNY, BERG, ICEDMAX, DAIR
    REAL, INTENT(INOUT)     :: WN1(NK), CG1(NK), &
         SPEC(NSPEC), ALPHA(NK), USTAR,       &
         USTDIR, FPI, TAUOX, TAUOY,           &
         TAUWX, TAUWY, PHIAW, PHIOC, PHICE,   &
         CHARN, TWS, BEDFORM(3), PHIBBL,      &
         TAUBBL(2), TAUICE(2), WHITECAP(4),   &
         TAUWIX, TAUWIY, TAUWNX, TAUWNY,      &
         ICEF, TAUOCX, TAUOCY, WNMEAN
    REAL, INTENT(OUT)       :: DTDYN, FCUT
    REAL, INTENT(IN)        :: COEF
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IK, ITH, IS, IS0, NSTEPS, NKH, NKH1, &
         IKS1, IS1, NSPECH, IDT, IERR, ISP
    REAL :: DTTOT, FHIGH, DT, AFILT, DAMAX, AFAC, &
         HDT, ZWND, FP, DEPTH, TAUSCX, TAUSCY, FHIGI
    ! Scaling factor for SIN, SDS, SNL
    REAL :: ICESCALELN, ICESCALEIN, ICESCALENL, ICESCALEDS
    REAL :: EMEAN, FMEAN, AMAX, CD, Z0, SCAT,    &
         SMOOTH_ICEDISP
    REAL :: WN_R(NK), CG_ICE(NK), ALPHA_LIU(NK), ICECOEF2, R(NK)
    DOUBLE PRECISION :: ATT, ISO
    REAL :: EBAND, DIFF, EFINISH, HSTOT, PHINL,       &
         FMEAN1, FMEANWS, &
         FACTOR, FACTOR2, DRAT, TAUWAX, TAUWAY,    &
         MWXFINISH, MWYFINISH, A1BAND, B1BAND,     &
         COSI(2)
    REAL :: SPECINIT(NSPEC), SPEC2(NSPEC), FRLOCAL, JAC2
    REAL :: DAM (NSPEC), DAM2(NSPEC), WN2(NSPEC),  &
         VSLN(NSPEC),                         &
         VSIN(NSPEC), VDIN(NSPEC),            &
         VSNL(NSPEC), VDNL(NSPEC),            &
         VSDS(NSPEC), VDDS(NSPEC),            &
         VSBT(NSPEC), VDBT(NSPEC)
    REAL :: VS(NSPEC), VD(NSPEC), EB(NK)

    LOGICAL :: SHAVE
    LOGICAL :: LBREAK
    LOGICAL, SAVE :: FIRST = .TRUE.
    LOGICAL :: PrintDeltaSmDA
    REAL :: eInc1, eInc2, eVS, eVD, JAC
    REAL :: DeltaSRC(NSPEC)

    REAL :: FOUT(NK,NTH), SOUT(NK,NTH), DOUT(NK,NTH)
    REAL, SAVE :: TAUNUX, TAUNUY
    LOGICAL, SAVE :: FLTEST = .FALSE., FLAGNN = .TRUE.

#ifdef W3_OMPG
    !$omp threadprivate( TAUNUX, TAUNUY)
    !$omp threadprivate( FLTEST, FLAGNN )
    !$omp threadprivate( FIRST )
#endif

    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters dependent on compile switch
    !/
#ifdef W3_S
    INTEGER, SAVE :: IENT = 0
#endif

#ifdef W3_NNT
    INTEGER, SAVE :: NDSD = 89, NDSD2 = 88, J
    REAL :: QCERR  = 0.     !/XNL2 and !/NNT
#endif

#ifdef W3_NL5
    INTEGER :: QI5TSTART(2)
    REAL :: QR5KURT
    INTEGER, PARAMETER :: NL5_SELECT = 1
    REAL, PARAMETER :: NL5_OFFSET = 0.  ! explicit dyn.
#endif

#ifdef W3_SEED
    REAL :: UC, SLEV
#endif

#ifdef W3_MLIM
    REAL :: HM, EM
#endif

#ifdef W3_NNT
    REAL :: FACNN
#endif

#ifdef W3_T
    REAL :: DTRAW
#endif

#if defined(W3_IC1) || defined(W3_IC2) || defined(W3_IC3) || defined(W3_IC4) || defined(W3_IC5)
    REAL :: VSIC(NSPEC), VDIC(NSPEC)
#endif

#ifdef W3_DB1
    REAL :: VSDB(NSPEC), VDDB(NSPEC)
#endif

#ifdef W3_TR1
    REAL :: VSTR(NSPEC), VDTR(NSPEC)
#endif

#ifdef W3_BS1
    REAL :: VSBS(NSPEC), VDBS(NSPEC)
#endif

#ifdef W3_REF1
    REAL :: VREF(NSPEC)
#endif

#if defined(W3_IS1) || defined(W3_IS2)
    REAL :: VSIR(NSPEC), VDIR(NSPEC)
#endif

#ifdef W3_IS2
    REAL :: VDIR2(NSPEC)
    DOUBLE PRECISION :: SCATSPEC(NTH)
#endif

#ifdef W3_UOST
    REAL :: VSUO(NSPEC), VDUO(NSPEC)
#endif

#ifdef W3_ST1
    REAL :: FH1, FH2
#endif

#ifdef W3_ST2
    REAL :: FHTRAN, DFH, FACDIA, FACPAR
#endif

#ifdef W3_ST3
    REAL :: FMEANS, FH1, FH2
#endif

#ifdef W3_ST4
    REAL :: FMEANS, FH1, FH2, FAGE, DLWMEAN
    REAL :: BRLAMBDA(NSPEC)
#endif

#if defined(W3_ST3) || defined(W3_ST4)
    LOGICAL :: LLWS(NSPEC)
#endif

#ifdef W3_ST6
    REAL :: VSWL(NSPEC), VDWL(NSPEC)
#endif

#ifdef W3_PDLIB
    REAL :: PreVS, DVS, SIDT, FAKS, MAXDAC
#endif

#ifdef W3_NNT
    CHARACTER(LEN=17), SAVE :: FNAME = 'test_data_nnn.ww3'
#endif
    !
    !/ -- End of variable delclarations
    !
#ifdef W3_S
    CALL STRACE (IENT, 'W3SRCE')
#endif

#ifdef W3_T
    FLTEST = .TRUE.
#endif
    !
    IKS1 = 1
#ifdef W3_IG1
    ! Does not integrate source terms for IG band if IGPARS(12) = 0.
    IF (NINT(IGPARS(12)).EQ.0) IKS1 = NINT(IGPARS(5))
#endif
    IS1=(IKS1-1)*NTH+1

    !! Initialise source term arrays:
    VD   = 0.
    VS   = 0.
    VDIO = 0.
    VSIO = 0.
    VSBT = 0.
    VDBT = 0.

#if defined(W3_LN0) || defined(W3_LN1) || defined(W3_SEED)
    VSLN = 0.
#endif

#if defined(W3_ST0) || defined(W3_ST3) || defined(W3_ST4)
    VSIN = 0.
    VDIN = 0.
#endif

#if defined(W3_NL0) || defined(W3_NL1)
    VSNL = 0.
    VDNL = 0.
#endif

#ifdef W3_TR1
    VSTR = 0.
    VDTR = 0.
#endif

#if defined(W3_ST0) || defined(W3_ST4)
    VSDS = 0.
    VDDS = 0.
#endif

#ifdef W3_DB1
    VSDB = 0.
    VDDB = 0.
#endif

#if defined(W3_IC1) || defined(W3_IC2) || defined(W3_IC3) || defined(W3_IC4) || defined(W3_IC5)
    VSIC = 0.
    VDIC = 0.
#endif

#ifdef W3_UOST
    VSUO = 0.
    VDUO = 0.
#endif

#if defined(W3_IS1) || defined(W3_IS2)
    VSIR = 0.
    VDIR = 0.
#endif

#ifdef W3_IS2
    VDIR2 = 0.
#endif

#ifdef W3_ST6
    VSWL = 0.
    VDWL = 0.
#endif

#if defined(W3_ST0) || defined(W3_ST1) || defined(W3_ST6)
    ZWND = 10.
#endif

#if defined(W3_ST2)
    ZWND = ZWIND
#endif

#if defined(W3_ST4)
    ZWND = ZZWND
#endif
    !
    ! 1.  Preparations --------------------------------------------------- *
    !
    DEPTH = MAX ( DMIN , D_INP )
    DRAT = DAIR / DWAT
    ICESCALELN = MAX(0.,MIN(1.,1.-ICE*ICESCALES(1)))
    ICESCALEIN = MAX(0.,MIN(1.,1.-ICE*ICESCALES(2)))
    ICESCALENL = MAX(0.,MIN(1.,1.-ICE*ICESCALES(3)))
    ICESCALEDS = MAX(0.,MIN(1.,1.-ICE*ICESCALES(4)))

#ifdef W3_T
    WRITE (NDST,9000)
    WRITE (NDST,9001) DEPTH, U10ABS, U10DIR*RADE
#endif

    ! 1.a Set maximum change and wavenumber arrays.
    !
    !XP     = 0.15
    !FACP   = XP / PI * 0.62E-3 * TPI**4 / GRAV**2
    !
    DO IK=1, NK
      DAM(1+(IK-1)*NTH) = FACP / ( SIG(IK) * WN1(IK)**3 )
      WN2(1+(IK-1)*NTH) = WN1(IK)
    END DO
    !
    DO IK=1, NK
      IS0    = (IK-1)*NTH
      DO ITH=2, NTH
        DAM(ITH+IS0) = DAM(1+IS0)
        WN2(ITH+IS0) = WN2(1+IS0)
      END DO
    END DO
    !
    ! 1.b Prepare dynamic time stepping
    !
    DTDYN  = 0.
    DTTOT  = 0.
    NSTEPS = 0
    PHIAW  = 0.
    CHARN  = 0.
    TWS    = 0.
    PHINL  = 0.
    PHIBBL = 0.
    TAUWIX = 0.
    TAUWIY = 0.
    TAUWNX = 0.
    TAUWNY = 0.
    TAUWAX = 0.
    TAUWAY = 0.
    TAUSCX = 0.
    TAUSCY = 0.
    TAUBBL = 0.
    TAUICE = 0.
    PHICE  = 0.
    TAUOCX = 0.
    TAUOCY = 0.
    WNMEAN = 0.

    !
    ! TIME is updated in W3WAVEMD prior to the call of W3SCRE, we should
    ! move 'TIME' one time step backward (QL)
#ifdef W3_NL5
    QI5TSTART = TIME
    CALL TICK21 (QI5TSTART, -1.0 * DTG)
#endif
    !
#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) 'W3SRCE start sum(SPEC)=', sum(SPEC)
      WRITE(740+IAPROC,*) 'W3SRCE start sum(SPECOLD)=', sum(SPECOLD)
      WRITE(740+IAPROC,*) 'W3SRCE start sum(SPECINIT)=', sum(SPECINIT)
      WRITE(740+IAPROC,*) 'W3SRCE start sum(VSIO)=', sum(VSIO)
      WRITE(740+IAPROC,*) 'W3SRCE start sum(VDIO)=', sum(VDIO)
      WRITE(740+IAPROC,*) 'W3SRCE start USTAR=', USTAR
    END IF
#endif

#ifdef W3_ST4
    DLWMEAN= 0.
    BRLAMBDA(:)=0.
    WHITECAP(:)=0.
#endif
    !
    ! 1.c Set mean parameters
    !
#ifdef W3_ST0
    CALL W3SPR0 (SPEC, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
    FP     = 0.85 * FMEAN
#endif
#ifdef W3_ST1
    CALL W3SPR1 (SPEC, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
    FP     = 0.85 * FMEAN
#endif
#ifdef W3_ST2
    CALL W3SPR2 (SPEC, CG1, WN1, DEPTH, FPI, U10ABS, USTAR,    &
         EMEAN, FMEAN, WNMEAN, AMAX, ALPHA, FP )
#endif
#ifdef W3_ST3
    TAUWX=0.
    TAUWY=0.
    IF ( IT .eq. 0 ) THEN
      LLWS(:) = .TRUE.
      USTAR=0.
      USTDIR=0.
      CALL W3SPR3 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEANS, WNMEAN, &
           AMAX, U10ABS, U10DIR, USTAR, USTDIR,          &
           TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS)
    ELSE
      CALL W3SPR3 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEANS, WNMEAN, &
           AMAX, U10ABS, U10DIR, USTAR, USTDIR,          &
           TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS)
      CALL W3SIN3 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,   &
           U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY,   &
           ICE, VSIN, VDIN, LLWS, IX, IY )
    END IF
    CALL W3SPR3 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEANS, WNMEAN, &
         AMAX, U10ABS, U10DIR, USTAR, USTDIR,          &
         TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS)
    TWS = 1./FMEANWS
#endif
#ifdef W3_ST4
    TAUWX=0.
    TAUWY=0.
    IF ( IT .eq. 0 ) THEN
      LLWS(:) = .TRUE.
      USTAR=0.
      USTDIR=0.
    ELSE
      CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN, &
           AMAX, U10ABS, U10DIR,                           &
#ifdef W3_FLX5
           TAUA, TAUADIR, DAIR,                             &
#endif
           USTAR, USTDIR,                                  &
           TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
#endif

#if defined(W3_DEBUGSRC) && defined(W3_ST4)
      IF (IX == DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) '1: out value USTAR=', USTAR, ' USTDIR=', USTDIR
        WRITE(740+IAPROC,*) '1: out value EMEAN=', EMEAN, ' FMEAN=', FMEAN
        WRITE(740+IAPROC,*) '1: out value FMEAN1=', FMEAN1, ' WNMEAN=', WNMEAN
        WRITE(740+IAPROC,*) '1: out value CD=', CD, ' Z0=', Z0
        WRITE(740+IAPROC,*) '1: out value ALPHA=', CHARN, ' FMEANWS=', FMEANWS
      END IF
#endif

#ifdef W3_ST4
      CALL W3SIN4 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,       &
           U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY,       &
           VSIN, VDIN, LLWS, IX, IY, BRLAMBDA )
    END IF
#endif
#if defined(W3_DEBUGSRC) && defined(W3_ST4)
    IF (IX == DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) '1: U10DIR=', U10DIR, ' Z0=', Z0, ' CHARN=', CHARN
      WRITE(740+IAPROC,*) '1: USTAR=', USTAR, ' U10ABS=', U10ABS, ' AS=', AS
      WRITE(740+IAPROC,*) '1: DRAT=', DRAT
      WRITE(740+IAPROC,*) '1: TAUWX=', TAUWX, ' TAUWY=', TAUWY
      WRITE(740+IAPROC,*) '1: TAUWAX=', TAUWAX, ' TAUWAY=', TAUWAY
      WRITE(740+IAPROC,*) '1: min(CG1)=', minval(CG1), ' max(CG1)=', maxval(CG1)
      WRITE(740+IAPROC,*) '1: W3SIN4(min/max/sum)VSIN=', minval(VSIN), maxval(VSIN), sum(VSIN)
      WRITE(740+IAPROC,*) '1: W3SIN4(min/max/sum)VDIN=', minval(VDIN), maxval(VDIN), sum(VDIN)
    END IF
#endif

#ifdef W3_ST4
    CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN, &
         AMAX, U10ABS, U10DIR,                         &
#ifdef W3_FLX5
         TAUA, TAUADIR, DAIR,                    &
#endif
         USTAR, USTDIR,                                &
         TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
    TWS = 1./FMEANWS
#endif
#ifdef W3_ST6
    CALL W3SPR6 (SPEC, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX, FP)
#endif
    !
    ! 1.c2 Stores the initial data
    !
    SPECINIT = SPEC
    !
    ! 1.d Stresses
    !
#ifdef W3_FLX1
    CALL W3FLX1 ( ZWND, U10ABS, U10DIR, USTAR, USTDIR, Z0, CD )
#endif
#ifdef W3_FLX2
    CALL W3FLX2 ( ZWND, DEPTH, FP, U10ABS, U10DIR,            &
         USTAR, USTDIR, Z0, CD )
#endif
#ifdef W3_FLX3
    CALL W3FLX3 ( ZWND, DEPTH, FP, U10ABS, U10DIR,            &
         USTAR, USTDIR, Z0, CD )
#endif
#ifdef W3_FLX4
    CALL W3FLX4 ( ZWND, U10ABS, U10DIR, USTAR, USTDIR, Z0, CD )
#endif
#ifdef W3_FLX5
    CALL W3FLX5 ( ZWND, U10ABS, U10DIR, TAUA, TAUADIR, DAIR,  &
         USTAR, USTDIR, Z0, CD, CHARN )
#endif
    !
    ! 1.e Prepare cut-off beyond which the tail is imposed with a power law
    !
#ifdef W3_ST0
    FHIGH  = SIG(NK)
#endif
#ifdef W3_ST1
    FH1    = FXFM * FMEAN
    FH2    = FXPM / USTAR
    FHIGH  = MAX ( FH1 , FH2 )
    IF (FLTEST) WRITE (NDST,9004) FH1*TPIINV, FH2*TPIINV, FHIGH*TPIINV
#endif
#ifdef W3_ST2
    FHIGH  = XFC * FPI
#endif
#ifdef W3_ST3
    FHIGH  = MAX(FFXFM * MAX(FMEAN,FMEANWS),FFXPM / USTAR)
#endif
#ifdef W3_ST4
    ! Introduces a Long & Resio (JGR2007) type dependance on wave age
    ! !/ST4      FAGE   = FFXFA*TANH(0.3*U10ABS*FMEANWS*TPI/GRAV)
    FAGE   = 0.
    FHIGH  = MAX( (FFXFM + FAGE ) * MAX(FMEAN1,FMEANWS), FFXPM / USTAR)
    FHIGI  = FFXFA * FMEAN1
#endif
#ifdef W3_ST6
    IF (FXFM .LE. 0) THEN
      FHIGH = SIG(NK)
    ELSE
      FHIGH  = MAX (FXFM * FMEAN, FXPM / USTAR)
    ENDIF
#endif
    !
    ! 1.f Prepare output file for !/NNT option
    !
#ifdef W3_NNT
    IF ( IT .EQ. 0 ) THEN
      J      = LEN_TRIM(FNMPRE)
      WRITE (FNAME(11:13),'(I3.3)') IAPROC
      OPEN (NDSD,FILE=FNMPRE(:J)//FNAME,form='UNFORMATTED', convert=file_endian,   &
           ERR=800,IOSTAT=IERR)
      WRITE (NDSD,ERR=801,IOSTAT=IERR) NK, NTH
      WRITE (NDSD,ERR=801,IOSTAT=IERR) SIG(1:NK) * TPIINV
      OPEN (NDSD2,FILE=FNMPRE(:J)//'time.ww3',                &
           FORM='FORMATTED',ERR=800,IOSTAT=IERR)
    END IF
#endif
    !
    ! ... Branch point dynamic integration - - - - - - - - - - - - - - - -
    !
    DO
      !
      NSTEPS = NSTEPS + 1
      !
#ifdef W3_T
      WRITE (NDST,9020) NSTEPS, DTTOT
#endif
      !
      ! 2.  Calculate source terms ----------------------------------------- *
      !
      ! 2.a Input.
      !
#ifdef W3_LN1
      CALL W3SLN1 (       WN1, FHIGH, USTAR, U10DIR , VSLN       )
#endif
      !
#ifdef W3_ST1
      CALL W3SIN1 ( SPEC, WN2, USTAR, U10DIR ,        VSIN, VDIN )
#endif
#ifdef W3_ST2
      CALL W3SIN2 ( SPEC, CG1, WN2, U10ABS, U10DIR, CD, Z0,        &
           FPI, VSIN, VDIN )
#endif
#ifdef W3_ST3
      CALL W3SIN3 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,       &
           U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY,       &
           ICE, VSIN, VDIN, LLWS, IX, IY )
#endif
#ifdef W3_ST4
      CALL W3SIN4 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,       &
           U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY,       &
           VSIN, VDIN, LLWS, IX, IY, BRLAMBDA )
#endif

#if defined(W3_DEBUGSRC) && defined(W3_ST4)
      IF (IX == DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) '2 : W3SIN4(min/max/sum)VSIN=', minval(VSIN), maxval(VSIN), sum(VSIN)
        WRITE(740+IAPROC,*) '2 : W3SIN4(min/max/sum)VDIN=', minval(VDIN), maxval(VDIN), sum(VDIN)
      END IF
#endif

#ifdef W3_ST6
      CALL W3SIN6 ( SPEC, CG1, WN2, U10ABS, USTAR, USTDIR, CD, DAIR, &
           TAUWX, TAUWY, TAUWAX, TAUWAY, VSIN, VDIN )
#endif
      !
      ! 2.b Nonlinear interactions.
      !
#ifdef W3_NL1
      CALL W3SNL1 ( SPEC, CG1, WNMEAN*DEPTH, VSNL, VDNL )
#endif
#ifdef W3_NL2
      CALL W3SNL2 ( SPEC, CG1, DEPTH, VSNL, VDNL )
#endif
#ifdef W3_NL3
      CALL W3SNL3 ( SPEC, CG1, WN1, DEPTH, VSNL, VDNL )
#endif
#ifdef W3_NL4
      CALL W3SNL4 ( SPEC, CG1, WN1, DEPTH, VSNL, VDNL )
#endif
#ifdef W3_NL5
      CALL W3SNL5 ( SPEC, CG1, WN1, FMEAN, QI5TSTART,          &
           U10ABS, U10DIR, JSEA, VSNL, VDNL, QR5KURT)
#endif
      !
#ifdef W3_PDLIB
      IF (.NOT. FSSOURCE .or. LSLOC) THEN
#endif
#ifdef W3_TR1
        CALL W3STR1 ( SPEC, SPECOLD, CG1, WN1, DEPTH, IX,        VSTR, VDTR )
#endif
#ifdef W3_PDLIB
      ENDIF
#endif
      !
      ! 2.c Dissipation... except for ST4
      ! 2.c1   as in source term package
      !
#ifdef W3_ST1
      CALL W3SDS1 ( SPEC, WN2, EMEAN, FMEAN, WNMEAN,  VSDS, VDDS )
#endif
#ifdef W3_ST2
      CALL W3SDS2 ( SPEC, CG1, WN1, FPI, USTAR, ALPHA,VSDS, VDDS )
#endif
#ifdef W3_ST3
      CALL W3SDS3 ( SPEC, WN1, CG1, EMEAN, FMEANS, WNMEAN,              &
           USTAR, USTDIR, DEPTH, VSDS, VDDS, IX, IY )
#endif
#ifdef W3_ST4
      CALL W3SDS4 ( SPEC, WN1, CG1, USTAR, USTDIR, DEPTH, DAIR, VSDS,   &
           VDDS, IX, IY, BRLAMBDA, WHITECAP, DLWMEAN )
#endif
#if defined(W3_DEBUGSRC) && defined(W3_ST4)
      IF (IX == DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) '2 : W3SDS4(min/max/sum)VSDS=', minval(VSDS), maxval(VSDS), sum(VSDS)
        WRITE(740+IAPROC,*) '2 : W3SDS4(min/max/sum)VDDS=', minval(VDDS), maxval(VDDS), sum(VDDS)
      END IF
#endif

#ifdef W3_ST6
      CALL W3SDS6 ( SPEC, CG1, WN1,                   VSDS, VDDS )
#endif
      !
#ifdef W3_PDLIB
      IF (.NOT. FSSOURCE .or. LSLOC) THEN
#endif
#ifdef W3_DB1
        CALL W3SDB1 ( IX, SPEC, DEPTH, EMEAN, FMEAN, WNMEAN, CG1,       &
             LBREAK, VSDB, VDDB )
#endif
#ifdef W3_PDLIB
      ENDIF
#endif
      !
      ! 2.c2   optional dissipation parameterisations
      !
#ifdef W3_ST6
      IF (SWL6S6) THEN
        CALL W3SWL6 ( SPEC, CG1, WN1, VSWL, VDWL )
      END IF
#endif
      !
      ! 2.d Bottom interactions.
      !
#ifdef W3_BT1
      CALL W3SBT1 ( SPEC, CG1, WN1, DEPTH,            VSBT, VDBT )
#endif
#ifdef W3_BT4
      CALL W3SBT4 ( SPEC, CG1, WN1, DEPTH, D50, PSIC, TAUBBL,    &
           BEDFORM, VSBT, VDBT, IX, IY )
#endif
#ifdef W3_BT8
      CALL W3SBT8 ( SPEC, DEPTH, VSBT, VDBT, IX, IY )
#endif
#ifdef W3_BT9
      CALL W3SBT9 ( SPEC, DEPTH, VSBT, VDBT, IX, IY )
#endif
      !
#ifdef W3_BS1
      CALL W3SBS1 ( SPEC, CG1, WN1, DEPTH, CX, CY,                &
           TAUSCX, TAUSCY, VSBS, VDBS )
#endif
      !
      ! 2.e Unresolved Obstacles Source Term
      !
#ifdef W3_UOST
      ! UNRESOLVED OBSTACLES
      CALL UOST_SRCTRMCOMPUTE(IX, IY, SPEC, CG1, DT,            &
           U10ABS, U10DIR, VSUO, VDUO)
#endif
      !
      ! 2.g Dump training data if necessary
      !
#ifdef W3_NNT
      WRITE (SCREEN,8888) TIME, DTTOT, FLAGNN, QCERR
      WRITE (NDSD2,8888) TIME, DTTOT, FLAGNN, QCERR
8888  FORMAT (1X,I8.8,1X,I6.6,F8.1,L2,F8.2)
      WRITE (NDSD,ERR=801,IOSTAT=IERR) IX, IY, TIME, NSTEPS,        &
           DTTOT, FLAGNN, DEPTH, U10ABS, U10DIR
      !
      IF ( FLAGNN ) THEN
        DO IK=1, NK
          FACNN  = TPI * SIG(IK) / CG1(IK)
          DO ITH=1, NTH
            IS     = ITH + (IK-1)*NTH
            FOUT(IK,ITH) = SPEC(IS) * FACNN
            SOUT(IK,ITH) = VSNL(IS) * FACNN
            DOUT(IK,ITH) = VDNL(IS)
          END DO
        END DO
        WRITE (NDSD,ERR=801,IOSTAT=IERR) FOUT
        WRITE (NDSD,ERR=801,IOSTAT=IERR) SOUT
        WRITE (NDSD,ERR=801,IOSTAT=IERR) DOUT
      END IF
#endif
      !
      ! 3.  Set frequency cut-off ------------------------------------------ *
      !
#ifdef W3_ST2
      FHIGH  = XFC * FPI
      IF ( FLTEST ) WRITE (NDST,9005) FHIGH*TPIINV
#endif
      NKH    = MIN ( NK , INT(FACTI2+FACTI1*LOG(MAX(1.E-7,FHIGH))) )
      NKH1   = MIN ( NK , NKH+1 )
      NSPECH = NKH1*NTH
#ifdef W3_T
      WRITE (NDST,9021) NKH, NKH1, NSPECH
#endif
      !
      ! 4.  Summation of source terms and diagonal term and time step ------ *
      !
      DT     = MIN ( DTG-DTTOT , DTMAX )
      AFILT  = MAX ( DAM(NSPEC) , XFLT*AMAX )
      !
      !     For input and dissipation calculate the fraction of the ice-free
      !     surface. In the presence of ice, the effective water surface
      !     is reduce to a fraction of the cell size free from ice, and so is
      !     input :
      !             SIN = (1-ICE)**ISCALEIN*SIN and SDS=(1-ICE)**ISCALEDS*SDS ------------------ *
      !     INFLAGS2(4) is true if ice concentration was ever read during
      !             this simulation
      IF ( INFLAGS2(4) ) THEN
        VSNL(1:NSPECH) = ICESCALENL * VSNL(1:NSPECH)
        VDNL(1:NSPECH) = ICESCALENL * VDNL(1:NSPECH)
        VSLN(1:NSPECH) = ICESCALELN * VSLN(1:NSPECH)
        VSIN(1:NSPECH) = ICESCALEIN * VSIN(1:NSPECH)
        VDIN(1:NSPECH) = ICESCALEIN * VDIN(1:NSPECH)
        VSDS(1:NSPECH) = ICESCALEDS * VSDS(1:NSPECH)
        VDDS(1:NSPECH) = ICESCALEDS * VDDS(1:NSPECH)
      END IF

#ifdef W3_PDLIB
      IF (B_JGS_LIMITER_FUNC == 2) THEN
        DO IK=1, NK
          JAC      = CG1(IK)/CLATSL
          JAC2     = 1./TPI/SIG(IK)
          FRLOCAL  = SIG(IK)*TPIINV
#ifdef W3_ST6
          DAM2(1+(IK-1)*NTH) = 5E-7 * GRAV/FRLOCAL**4 * USTAR * FMEAN * DTMIN * JAC * JAC2
#else
          DAM2(1+(IK-1)*NTH) = 5E-7 * GRAV/FRLOCAL**4 * USTAR * MAX(FMEANWS,FMEAN) * DTMIN * JAC * JAC2
#endif
          !FROM WWM:           5E-7  * GRAV/FR(IS)**4          * USTAR * MAX(FMEANWS(IP),FMEAN(IP)) * DT4S/PI2/SPSIG(IS)
        END DO
        DO IK=1, NK
          IS0  = (IK-1)*NTH
          DO ITH=2, NTH
            DAM2(ITH+IS0) = DAM2(1+IS0)
          END DO
        END DO
      ENDIF
#endif
      !
      DO IS=IS1, NSPECH
        VS(IS) = VSLN(IS) + VSIN(IS) + VSNL(IS)  &
             + VSDS(IS) + VSBT(IS)
#ifdef W3_ST6
        VS(IS) = VS(IS) + VSWL(IS)
#endif
#if defined(W3_TR1) && !defined(W3_PDLIB)
        VS(IS) = VS(IS) + VSTR(IS)
#endif
#ifdef W3_BS1
        VS(IS) = VS(IS) + VSBS(IS)
#endif
#ifdef W3_UOST
        VS(IS) = VS(IS) + VSUO(IS)
#endif
        VD(IS) =  VDIN(IS) + VDNL(IS)  &
             + VDDS(IS) + VDBT(IS)
#ifdef W3_ST6
        VD(IS) = VD(IS) + VDWL(IS)
#endif
#if defined(W3_TR1) && !defined(W3_PDLIB)
        VD(IS) = VD(IS) + VDTR(IS)
#endif
#ifdef W3_BS1
        VD(IS) = VD(IS) + VDBS(IS)
#endif
#ifdef W3_UOST
        VD(IS) = VD(IS) + VDUO(IS)
#endif
        DAMAX = MIN ( DAM(IS) , MAX ( XREL*SPECINIT(IS) , AFILT ) )
        AFAC = 1. / MAX( 1.E-10 , ABS(VS(IS)/DAMAX) )
#ifdef W3_NL5
        IF (NL5_SELECT .EQ. 1)  THEN
          DT = MIN ( DT , AFAC / ( MAX ( 1.E-10,                  &
               1. + NL5_OFFSET*AFAC*MIN(0.,VD(IS)) ) ) )
        ELSE
#endif
          DT = MIN ( DT , AFAC / ( MAX ( 1.E-10,                  &
               1. + OFFSET*AFAC*MIN(0.,VD(IS)) ) ) )
#ifdef W3_NL5
        ENDIF
#endif
      END DO  ! end of loop on IS

      !
      DT     = MAX ( 0.5, DT ) ! The hardcoded min. dt is a problem for certain cases e.g. laborotary scale problems.
      !
      DTDYN  = DTDYN + DT
#ifdef W3_T
      DTRAW  = DT
#endif
      IDT = 1 + INT ( 0.99*(DTG-DTTOT)/DT ) ! number of iterations
      DT = (DTG-DTTOT)/REAL(IDT)           ! actualy time step
      SHAVE = DT.LT.DTMIN .AND. DT.LT.DTG-DTTOT   ! limiter check ...
      SHAVEIO = SHAVE
      DT = MAX ( DT , MIN (DTMIN,DTG-DTTOT) ) ! override dt with input time step or last time step if it is bigger ... anyway the limiter is on!
      !
#ifdef W3_NL5
      DT     = INT(DT) * 1.0
#endif
      IF (srce_call .eq. srce_imp_post) DT = DTG  ! for implicit part
#ifdef W3_NL5
      IF (NL5_SELECT .EQ. 1) THEN
        HDT    = NL5_OFFSET * DT
      ELSE
#endif
        HDT    = OFFSET * DT
#ifdef W3_NL5
      ENDIF
#endif
      DTTOT  = DTTOT + DT

#ifdef W3_DEBUGSRC
      IF (IX == DEBUG_NODE) WRITE(*,'(A20,2I10,5F20.10,L20)') 'TIMINGS 2', IDT, NSTEPS, DT, DTMIN, DTDYN, HDT, DTTOT, SHAVE
      IF (IX == DEBUG_NODE) THEN
        WRITE(740+IAPROC,*) '1: min/max/sum(VS)=', minval(VS), maxval(VS), sum(VS)
        WRITE(740+IAPROC,*) '1: min/max/sum(VD)=', minval(VD), maxval(VD), sum(VD)
        WRITE(740+IAPROC,*) 'min/max/sum(VSIN)=', minval(VSIN), maxval(VSIN), sum(VSIN)
        WRITE(740+IAPROC,*) 'min/max/sum(VDIN)=', minval(VDIN), maxval(VDIN), sum(VDIN)
        WRITE(740+IAPROC,*) 'min/max/sum(VSLN)=', minval(VSLN), maxval(VSLN), sum(VSLN)
        WRITE(740+IAPROC,*) 'min/max/sum(VSNL)=', minval(VSNL), maxval(VSNL), sum(VSNL)
        WRITE(740+IAPROC,*) 'min/max/sum(VDNL)=', minval(VDNL), maxval(VDNL), sum(VDNL)
        WRITE(740+IAPROC,*) 'min/max/sum(VSDS)=', minval(VSDS), maxval(VSDS), sum(VSDS)
        WRITE(740+IAPROC,*) 'min/max/sum(VDDS)=', minval(VDDS), maxval(VDDS), sum(VDDS)
#ifdef W3_ST6
        WRITE(740+IAPROC,*) 'min/max/sum(VSWL)=', minval(VSWL), maxval(VSWL), sum(VSWL)
        WRITE(740+IAPROC,*) 'min/max/sum(VDWL)=', minval(VDWL), maxval(VDWL), sum(VDWL)
#endif
#ifdef W3_DB1
        WRITE(740+IAPROC,*) 'min/max/sum(VSDB)=', minval(VSDB), maxval(VSDB), sum(VSDB)
        WRITE(740+IAPROC,*) 'min/max/sum(VDDB)=', minval(VDDB), maxval(VDDB), sum(VDDB)
#endif
#ifdef W3_TR1
        WRITE(740+IAPROC,*) 'min/max/sum(VSTR)=', minval(VSTR), maxval(VSTR), sum(VSTR)
        WRITE(740+IAPROC,*) 'min/max/sum(VDTR)=', minval(VDTR), maxval(VDTR), sum(VDTR)
#endif
#ifdef W3_BS1
        WRITE(740+IAPROC,*) 'min/max/sum(VSBS)=', minval(VSBS), maxval(VSBS), sum(VSBS)
        WRITE(740+IAPROC,*) 'min/max/sum(VDBS)=', minval(VDBS), maxval(VDBS), sum(VDBS)
#endif
        WRITE(740+IAPROC,*) 'min/max/sum(VSBT)=', minval(VSBT), maxval(VSBT), sum(VSBT)
        WRITE(740+IAPROC,*) 'min/max/sum(VDBT)=', minval(VDBT), maxval(VDBT), sum(VDBT)
      END IF
#endif

#ifdef W3_PDLIB
      IF (srce_call .eq. srce_imp_pre) THEN
        IF (LSLOC) THEN
          IF (IMEM == 1) THEN
            SIDT  = PDLIB_SI(JSEA) * DTG
            DO IK = 1, NK
              JAC = CLATSL/CG1(IK)
              DO ITH = 1, NTH
                ISP = ITH + (IK-1)*NTH
                VD(ISP) = MIN(0., VD(ISP))
                IF (B_JGS_LIMITER_FUNC == 2) THEN
                  MAXDAC = MAX(DAM(ISP),DAM2(ISP))
                ELSE
                  MAXDAC = DAM(ISP)
                ENDIF
                FAKS   = DTG / MAX ( 1. , (1.-DTG*VD(ISP)))
                DVS    = VS(ISP) * FAKS
                IF (.NOT. B_JGS_LIMITER) THEN
                  DVS  = SIGN(MIN(MAXDAC,ABS(DVS)),DVS)
                ENDIF
                PreVS  = DVS / FAKS
                eVS    = PreVS / CG1(IK) * CLATSL
                eVD    = MIN(0.,VD(ISP))
                B_JAC(ISP,JSEA)                   = B_JAC(ISP,JSEA) + SIDT * (eVS - eVD*SPEC(ISP)*JAC)
                ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) - SIDT * eVD
#ifdef W3_DB1
                eVS = VSDB(ISP) * JAC
                eVD = MIN(0.,VDDB(ISP))
                IF (eVS .gt. 0.) THEN
                  evS = 2*evS
                  evD = -evD
                ELSE
                  evS = -evS
                  evD = 2*evD
                ENDIF
#endif
                B_JAC(ISP,JSEA)                   = B_JAC(ISP,JSEA) + SIDT * eVS
                ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) - SIDT * eVD

#ifdef W3_TR1
                eVS = VSTR(ISP) * JAC
                eVD = VDTR(ISP)
                IF (eVS .gt. 0.) THEN
                  evS = 2*evS
                  evD = -evD
                ELSE
                  evS = -evS
                  evD = 2*evD
                ENDIF
#endif
                B_JAC(ISP,JSEA)                   = B_JAC(ISP,JSEA) + SIDT * eVS
                ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(JSEA)) - SIDT * eVD
              END DO
            END DO

          ELSEIF (IMEM == 2) THEN

            SIDT   = PDLIB_SI(JSEA) * DTG
            DO IK=1,NK
              JAC = CLATSL/CG1(IK)
              DO ITH=1,NTH
                ISP=ITH + (IK-1)*NTH
                VD(ISP) = MIN(0., VD(ISP))
                IF (B_JGS_LIMITER_FUNC == 2) THEN
                  MAXDAC    = MAX(DAM(ISP),DAM2(ISP))
                ELSE
                  MAXDAC    = DAM(ISP)
                ENDIF
                FAKS      = DTG / MAX ( 1. , (1.-DTG*VD(ISP)))
                DVS       = VS(ISP) * FAKS
                IF (.NOT. B_JGS_LIMITER) THEN
                  DVS       = SIGN(MIN(MAXDAC,ABS(DVS)),DVS)
                ENDIF
                PreVS     = DVS / FAKS
                eVS = PreVS / CG1(IK) * CLATSL
                eVD = VD(ISP)
#ifdef W3_DB1
                eVS = eVS + DBLE(VSDB(ISP)) * JAC
                eVD = evD + MIN(0.,DBLE(VDDB(ISP)))
                B_JAC(ISP,JSEA)          = B_JAC(ISP,JSEA) + SIDT * (eVS - eVD*VA(ISP,JSEA))
                ASPAR_DIAG_ALL(ISP,JSEA) = ASPAR_DIAG_ALL(ISP,JSEA) - SIDT * eVD
#endif
              END DO
            END DO
          ENDIF
        ENDIF

        PrintDeltaSmDA=.FALSE.
        IF (PrintDeltaSmDA .eqv. .TRUE.) THEN
          DO IS=1,NSPEC
            DeltaSRC(IS) = VSIN(IS) - SPEC(IS)*VDIN(IS)
          END DO
          WRITE(740+IAPROC,*) 'min/max/sum(VSIN)=', minval(VSIN), maxval(VSIN), sum(VSIN)
          WRITE(740+IAPROC,*) 'min/max/sum(DeltaIN)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
          !
          DO IS=1,NSPEC
            DeltaSRC(IS) = VSNL(IS) - SPEC(IS)*VDNL(IS)
          END DO
          WRITE(740+IAPROC,*) 'min/max/sum(VSNL)=', minval(VSNL), maxval(VSNL), sum(VSNL)
          WRITE(740+IAPROC,*) 'min/max/sum(DeltaNL)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
          !
          DO IS=1,NSPEC
            DeltaSRC(IS) = VSDS(IS) - SPEC(IS)*VDDS(IS)
          END DO
          WRITE(740+IAPROC,*) 'min/max/sum(VSDS)=', minval(VSDS), maxval(VSDS), sum(VSDS)
          WRITE(740+IAPROC,*) 'min/max/sum(DeltaDS)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
          !
          !            DO IS=1,NSPEC
          !              DeltaSRC(IS) = VSIC(IS) - SPEC(IS)*VDIC(IS)
          !            END DO
          WRITE(740+IAPROC,*) 'min/max/sum(DeltaDS)=', minval(DeltaSRC), maxval(DeltaSRC), sum(DeltaSRC)
        END IF

        IF (.not. LSLOC) THEN
          IF (optionCall .eq. 1) THEN
            CALL SIGN_VSD_PATANKAR_WW3(SPEC,VS,VD)
          ELSE IF (optionCall .eq. 2) THEN
            CALL SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC,VS,VD)
          ELSE IF (optionCall .eq. 3) THEN
            CALL SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC,VS,VD)
          ENDIF
          VSIO = VS
          VDIO = VD
        ENDIF

#ifdef W3_DEBUGSRC
        IF (IX == DEBUG_NODE) THEN
          WRITE(740+IAPROC,*) '     srce_imp_pre : SHAVE = ', SHAVE
          WRITE(740+IAPROC,*) '     srce_imp_pre : DT=', DT, ' HDT=', HDT, 'DTG=', DTG
          WRITE(740+IAPROC,*) '     srce_imp_pre : sum(SPEC)=', sum(SPEC)
          WRITE(740+IAPROC,*) '     srce_imp_pre : sum(VSTOT)=', sum(VS)
          WRITE(740+IAPROC,*) '     srce_imp_pre : sum(VDTOT)=', sum(MIN(0. , VD))
        END IF

        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSIN)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDIN)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSDS)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDDS)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSNL)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDNL)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSLN)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSBT)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VS)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VD)
#endif
        RETURN ! return everything is done for the implicit ...

      END IF ! srce_imp_pre
! --end W3_PDLIB
#endif
      !
#ifdef W3_T
      WRITE (NDST,9040) DTRAW, DT, SHAVE
#endif
      !
      ! 5.  Increment spectrum --------------------------------------------- *
      !
      IF (srce_call .eq. srce_direct) THEN
        IF ( SHAVE ) THEN
          DO IS=IS1, NSPECH
            eInc1 = VS(IS) * DT / MAX ( 1. , (1.-HDT*VD(IS)))
            eInc2 = SIGN ( MIN (DAM(IS),ABS(eInc1)) , eInc1 )
            SPEC(IS) = MAX ( 0. , SPEC(IS)+eInc2 )
          END DO
        ELSE
          !
          DO IS=IS1, NSPECH
            eInc1 = VS(IS) * DT / MAX ( 1. , (1.-HDT*VD(IS)))
            SPEC(IS) = MAX ( 0. , SPEC(IS)+eInc1 )
          END DO
        END IF
        !
#ifdef W3_DB1
        DO IS=IS1, NSPECH
          eInc1 = VSDB(IS) * DT / MAX ( 1. , (1.-HDT*VDDB(IS)))
          SPEC(IS) = MAX ( 0. , SPEC(IS)+eInc1 )
        END DO
#endif
#ifdef W3_TR1
        DO IS=IS1, NSPECH
          eInc1 = VDTR(IS) * DT / MAX ( 1. , (1.-HDT*VDTR(IS)))
          SPEC(IS) = MAX ( 0. , SPEC(IS)+eInc1 )
        END DO
#endif

#ifdef W3_DEBUGSRC
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSIN)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDIN)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSDS)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDDS)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSNL)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VDNL)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSLN)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VSBT)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VS)
        IF (IX == DEBUG_NODE) WRITE(44,'(1EN15.4)') SUM(VD)
        IF (IX == DEBUG_NODE) THEN
          WRITE(740+IAPROC,*) '     srce_direct : SHAVE = ', SHAVE
          WRITE(740+IAPROC,*) '     srce_direct : DT=', DT, ' HDT=', HDT, 'DTG=', DTG
          WRITE(740+IAPROC,*) '     srce_direct : sum(SPEC)=', sum(SPEC)
          WRITE(740+IAPROC,*) '     srce_direct : sum(VSTOT)=', sum(VS)
          WRITE(740+IAPROC,*) '     srce_direct : sum(VDTOT)=', sum(MIN(0. , VD))
        END IF
#endif
      END IF ! srce_call .eq. srce_direct
      !
      ! 5.b  Computes
      !              atmos->wave flux PHIAW-------------------------------- *
      !              wave ->BBL  flux PHIBBL------------------------------- *
      !              wave ->ice  flux PHICE ------------------------------- *
      !
      WHITECAP(3)=0.
      HSTOT=0.
      DO IK=IKS1, NK
        FACTOR = DDEN(IK)/CG1(IK)                    !Jacobian to get energy in band
        FACTOR2= FACTOR*GRAV*WN1(IK)/SIG(IK)         ! coefficient to get momentum

        ! Wave direction is "direction to"
        ! therefore there is a PLUS sign for the stress
        DO ITH=1, NTH
          IS   = (IK-1)*NTH + ITH
          COSI(1)=ECOS(IS)
          COSI(2)=ESIN(IS)
          PHIAW = PHIAW + (VSIN(IS))* DT * FACTOR                    &
               / MAX ( 1. , (1.-HDT*VDIN(IS))) ! semi-implict integration scheme

          PHIBBL= PHIBBL- (VSBT(IS))* DT * FACTOR                    &
               / MAX ( 1. , (1.-HDT*VDBT(IS))) ! semi-implict integration scheme
          PHINL = PHINL + VSNL(IS)* DT * FACTOR                      &
               / MAX ( 1. , (1.-HDT*VDNL(IS))) ! semi-implict integration scheme
          IF (VSIN(IS).GT.0.) WHITECAP(3) = WHITECAP(3) + SPEC(IS)  * FACTOR
          HSTOT = HSTOT + SPEC(IS) * FACTOR
        END DO
      END DO
      WHITECAP(3) = 4. * SQRT(WHITECAP(3))
      HSTOT =4.*SQRT(HSTOT)
      TAUWIX = TAUWIX + TAUWX * DRAT * DT
      TAUWIY = TAUWIY + TAUWY * DRAT * DT
      TAUWNX = TAUWNX + TAUWAX * DRAT * DT
      TAUWNY  = TAUWNY + TAUWAY * DRAT * DT
      ! MISSING: TAIL TO BE ADDED ?
      !
#ifdef W3_NLS
      CALL W3SNLS ( SPEC, CG1, WN1, DEPTH, U10ABS, DT, AA=SPEC )
#endif
      !
      ! 6.  Add tail ------------------------------------------------------- *
      !   a Mean parameters
      !
      !
#ifdef W3_ST0
      CALL W3SPR0 (SPEC, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
#endif
#ifdef W3_ST1
      CALL W3SPR1 (SPEC, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
#endif
#ifdef W3_ST2
      CALL W3SPR2 (SPEC, CG1, WN1, DEPTH, FPI, U10ABS, USTAR,   &
           EMEAN, FMEAN, WNMEAN, AMAX, ALPHA, FP )
#endif
#ifdef W3_ST3
      CALL W3SPR3 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEANS,        &
           WNMEAN, AMAX, U10ABS, U10DIR, USTAR, USTDIR, &
           TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS)
#endif
#ifdef W3_ST4
      CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN,&
           AMAX, U10ABS, U10DIR,                          &
#ifdef W3_FLX5
           TAUA, TAUADIR, DAIR,                     &
#endif
           USTAR, USTDIR,                                 &
           TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
#endif
#ifdef W3_ST6
      CALL W3SPR6 (SPEC, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX, FP)
#endif
      !
#ifdef W3_FLX2
      CALL W3FLX2 ( ZWND, DEPTH, FP, U10ABS, U10DIR,           &
           USTAR, USTDIR, Z0, CD )
#endif
#ifdef W3_FLX3
      CALL W3FLX3 ( ZWND, DEPTH, FP, U10ABS, U10DIR,           &
           USTAR, USTDIR, Z0, CD )
#endif
      !
#ifdef W3_ST1
      FH1    = FXFM * FMEAN
      FHIGH  = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
      NKH    = MAX ( 2 , MIN ( NKH1 ,                           &
           INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH)) ) ) )
      !
      IF ( FLTEST ) WRITE (NDST,9060)                           &
           FH1*TPIINV, FH2*TPIINV, FHIGH*TPIINV, NKH
#endif
      !
#ifdef W3_ST2
      FHTRAN = XFT*FPI
      FHIGH  = XFC*FPI
      DFH    = FHIGH - FHTRAN
      NKH    = MAX ( 1 ,                                        &
           INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHTRAN)) ) )
      !
      IF ( FLTEST ) WRITE (NDST,9061) FHTRAN, FHIGH, NKH
#endif
      !
#ifdef W3_ST3
      FH1    = FFXFM * FMEAN
      FH2    = FFXPM / USTAR
      FHIGH  = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
      NKH    = MAX ( 2 , MIN ( NKH1 ,                           &
           INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH)) ) ) )
      !
      IF ( FLTEST ) WRITE (NDST,9062)                           &
           FH1*TPIINV, FH2*TPIINV, FHIGH*TPIINV, NKH
#endif
      !
#ifdef W3_ST4
      ! Introduces a Long & Resio (JGR2007) type dependance on wave age
      FAGE   = FFXFA*TANH(0.3*U10ABS*FMEANWS*TPI/GRAV)
      FH1    = (FFXFM+FAGE) * FMEAN1
      FH2    = FFXPM / USTAR
      FHIGH  = MIN ( SIG(NK) , MAX ( FH1 , FH2 ) )
      NKH    = MAX ( 2 , MIN ( NKH1 ,                           &
           INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH)) ) ) )
#endif
      !
#ifdef W3_ST6
      IF (FXFM .LE. 0) THEN
        FHIGH = SIG(NK)
      ELSE
        FHIGH = MIN ( SIG(NK), MAX(FXFM * FMEAN, FXPM / USTAR) )
      ENDIF
      NKH    = MAX ( 2 , MIN ( NKH1 ,                           &
           INT ( FACTI2 + FACTI1*LOG(MAX(1.E-7,FHIGH)) ) ) )
      !
      IF ( FLTEST ) WRITE (NDST,9063) FHIGH*TPIINV, NKH
#endif
      !
      ! 6.b Limiter for shallow water or Miche style criterion
      !     Last time step ONLY !
      !     uses true depth (D_INP) instead of limited depth
      !
#ifdef W3_MLIM
      IF ( DTTOT .GE. 0.9999*DTG ) THEN
        HM     = FHMAX *TANH(WNMEAN*MAX(0.,D_INP)) / MAX(1.E-4,WNMEAN )
        EM     = HM * HM / 16.
        IF ( EMEAN.GT.EM .AND. EMEAN.GT.1.E-30 ) THEN
          SPEC   = SPEC / EMEAN * EM
          EMEAN  = EM
        END IF
      END IF
#endif
      !
      ! 6.c Seeding of spectrum
      !     alpha = 0.005 , 0.5 in eq., 0.25 for directional distribution
      !
#ifdef W3_SEED
      DO IK=MIN(NK,NKH), NK
        UC     = FACSD * GRAV / SIG(IK)
        SLEV   = MIN ( 1. , MAX ( 0. , U10ABS/UC-1. ) ) *      &
             6.25E-4 / WN1(IK)**3 / SIG(IK)
        IF (INFLAGS2(4)) SLEV=SLEV*(1-ICE)
        DO ITH=1, NTH
          SPEC(ITH+(IK-1)*NTH) = MAX ( SPEC(ITH+(IK-1)*NTH) ,  &
               SLEV * MAX ( 0. , COS(U10DIR-TH(ITH)) )**2 )
        END DO
      END DO
#endif
      !
      ! 6.d Add tail
      !
      DO IK=NKH+1, NK
#ifdef W3_ST2
        FACDIA = MAX ( 0. , MIN ( 1., (SIG(IK)-FHTRAN)/DFH) )
        FACPAR = MAX ( 0. , 1.-FACDIA )
#endif
        DO ITH=1, NTH
          SPEC(ITH+(IK-1)*NTH) = SPEC(ITH+(IK-2)*NTH) * FACHFA         &
#ifdef W3_ST2
               * FACDIA + FACPAR * SPEC(ITH+(IK-1)*NTH)            &
#endif
               + 0.
        END DO
      END DO
      !
      ! 6.e  Update wave-supported stress----------------------------------- *
      !
#ifdef W3_ST3
      CALL W3SIN3 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,      &
           U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY, &
           ICE, VSIN, VDIN, LLWS, IX, IY )
#endif
#ifdef W3_ST4
      CALL W3SIN4 ( SPEC, CG1, WN2, U10ABS, USTAR, DRAT, AS,      &
           U10DIR, Z0, CD, TAUWX, TAUWY, TAUWAX, TAUWAY, &
           VSIN, VDIN, LLWS, IX, IY, BRLAMBDA )
#endif

      !
      ! 7.  Check if integration complete ---------------------------------- *
      !
      ! Update QI5TSTART (Q. Liu)
#ifdef W3_NL5
      CALL TICK21(QI5TSTART, DT)
#endif

      IF (srce_call .eq. srce_imp_post) THEN
        EXIT
      ENDIF

      IF ( DTTOT .GE. 0.9999*DTG ) THEN
        ! IF (IX == DEBUG_NODE) WRITE(*,*) 'DTTOT, DTG', DTTOT, DTG
        EXIT
      ENDIF

    END DO ! INTEGRATION LOOP

#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) 'NSTEPS=', NSTEPS
      WRITE(740+IAPROC,*) '1 : sum(SPEC)=', sum(SPEC)
    END IF
    WRITE(740+IAPROC,*) 'DT=', DT, 'DTG=', DTG
#endif
    !
    ! ... End point dynamic integration - - - - - - - - - - - - - - - - - -
    !
    ! 8.  Save integration data ------------------------------------------ *
    !
    DTDYN  = DTDYN / REAL(MAX(1,NSTEPS))
    FCUT   = FHIGH * TPIINV
    !
    GOTO 888
    !
    ! Error escape locations
    !
#ifdef W3_NNT
800 CONTINUE
    WRITE (NDSE,8000) FNAME, IERR
    CALL EXTCDE (1)
    !
801 CONTINUE
    WRITE (NDSE,8001) IERR
    CALL EXTCDE (2)
#endif
    !
888 CONTINUE
    !
    ! 9.a  Computes PHIOC------------------------------------------ *
    !     The wave to ocean flux is the difference between initial energy
    !     and final energy, plus wind input plus the SNL flux to high freq.,
    !     minus the energy lost to the bottom boundary layer (BBL)
    !
#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) '2 : sum(SPEC)=', sum(SPEC)
    END IF
#endif
    EFINISH  = 0.
    MWXFINISH  = 0.
    MWYFINISH  = 0.
    DO IK=1, NK
      EBAND = 0.
      A1BAND = 0.
      B1BAND = 0.
      DO ITH=1, NTH
        DIFF = SPECINIT(ITH+(IK-1)*NTH)-SPEC(ITH+(IK-1)*NTH)
        EBAND = EBAND + DIFF
        A1BAND = A1BAND + DIFF*ECOS(ITH)
        B1BAND = B1BAND + DIFF*ESIN(ITH)
      END DO
      EFINISH  = EFINISH  + EBAND * DDEN(IK) / CG1(IK)
      MWXFINISH  = MWXFINISH  + A1BAND * DDEN(IK) / CG1(IK)        &
           * WN1(IK)/SIG(IK)
      MWYFINISH  = MWYFINISH  + B1BAND * DDEN(IK) / CG1(IK)        &
           * WN1(IK)/SIG(IK)
    END DO
    !
    ! Transformation in momentum flux in m^2 / s^2
    !
    TAUOX=(GRAV*MWXFINISH+TAUWIX-TAUBBL(1))/DTG
    TAUOY=(GRAV*MWYFINISH+TAUWIY-TAUBBL(2))/DTG
    TAUWIX=TAUWIX/DTG
    TAUWIY=TAUWIY/DTG
    TAUWNX=TAUWNX/DTG
    TAUWNY=TAUWNY/DTG
    TAUBBL(:)=TAUBBL(:)/DTG
    TAUOCX=DAIR*COEF*COEF*USTAR*USTAR*COS(USTDIR) + DWAT*(TAUOX-TAUWIX)
    TAUOCY=DAIR*COEF*COEF*USTAR*USTAR*SIN(USTDIR) + DWAT*(TAUOY-TAUWIY)
    !
    ! Transformation in wave energy flux in W/m^2=kg / s^3
    !
    PHIOC =DWAT*GRAV*(EFINISH+PHIAW-PHIBBL)/DTG
    PHIAW =DWAT*GRAV*PHIAW /DTG
    PHINL =DWAT*GRAV*PHINL /DTG
    PHIBBL=DWAT*GRAV*PHIBBL/DTG
    !
    ! 10.1  Adds ice scattering and dissipation: implicit integration---------------- *
    !     INFLAGS2(4) is true if ice concentration was ever read during
    !             this simulation
    !
#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) '3 : sum(SPEC)=', sum(SPEC)
    END IF
#endif

    IF ( INFLAGS2(4).AND.ICE.GT.0 ) THEN

      IF (IICEDISP) THEN
        ICECOEF2 = 1E-6
        CALL LIU_FORWARD_DISPERSION (ICEH,ICECOEF2,DEPTH, &
             SIG,WN_R,CG_ICE,ALPHA_LIU)
        !
        IF (IICESMOOTH) THEN
#ifdef W3_IS2
          DO IK=1,NK
            SMOOTH_ICEDISP=0.
            IF (IS2PARS(14)*(TPI/WN_R(IK)).LT.ICEF) THEN ! IF ICE IS NOT TOO MUCH BROKEN
              SMOOTH_ICEDISP=TANH((ICEF-IS2PARS(14)*(TPI/WN_R(IK)))/(ICEF*IS2PARS(13)))
            END IF
            WN_R(IK)=WN1(IK)*(1-SMOOTH_ICEDISP)+WN_R(IK)*(SMOOTH_ICEDISP)
          END DO
#endif
        END IF
      ELSE
        WN_R=WN1
        CG_ICE=CG1
      END IF
      !
      R(:)=1 ! In case IC2 is defined but not IS2
      !
#ifdef W3_IC1
      CALL W3SIC1 ( SPEC,DEPTH, CG1, IX, IY, VSIC, VDIC )
#endif
#ifdef W3_IS2
      CALL W3SIS2 ( SPEC, DEPTH, ICE, ICEH, ICEF, ICEDMAX, IX, IY, &
           VSIR, VDIR, VDIR2, WN1, CG1, WN_R, CG_ICE, R )
#endif
#ifdef W3_IC2
      CALL W3SIC2 ( SPEC, DEPTH, ICEH, ICEF, CG1, WN1,&
           IX, IY, VSIC, VDIC, WN_R, CG_ICE, ALPHA_LIU, R)
#endif
#ifdef W3_IC3
      CALL W3SIC3 ( SPEC,DEPTH, CG1,  WN1, IX, IY, VSIC, VDIC )
#endif
#ifdef W3_IC4
      CALL W3SIC4 ( SPEC,DEPTH, CG1,       IX, IY, VSIC, VDIC )
#endif
#ifdef W3_IC5
      CALL W3SIC5 ( SPEC,DEPTH, CG1,  WN1, IX, IY, VSIC, VDIC )
#endif
      !
#ifdef W3_IS1
      CALL W3SIS1 ( SPEC, ICE, VSIR )
#endif
      SPEC2 = SPEC
      !
      TAUICE(:) = 0.
      PHICE = 0.
      DO IK=1,NK
        IS = 1+(IK-1)*NTH
        !
        ! First part of ice term integration: dissipation part
        !
        ATT=1.
#ifdef W3_IC1
        ATT=EXP(ICE*VDIC(IS)*DTG)
#endif
#ifdef W3_IC2
        ATT=EXP(ICE*VDIC(IS)*DTG)
#endif
#ifdef W3_IC3
        ATT=EXP(ICE*VDIC(IS)*DTG)
#endif
#ifdef W3_IC4
        ATT=EXP(ICE*VDIC(IS)*DTG)
#endif
#ifdef W3_IC5
        ATT=EXP(ICE*VDIC(IS)*DTG)
#endif
#ifdef W3_IS1
        ATT=ATT*EXP(ICE*VDIR(IS)*DTG)
#endif
#ifdef W3_IS2
        ATT=ATT*EXP(ICE*VDIR2(IS)*DTG)
        IF (IS2PARS(2).EQ.0) THEN ! Reminder : IS2PARS(2) = IS2BACKSCAT
          !
          ! If there is not re-distribution in directions the scattering is just an attenuation
          !
          ATT=ATT*EXP((ICE*VDIR(IS))*DTG)
        END IF
#endif
        SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH) = ATT*SPEC2(1+(IK-1)*NTH:NTH+(IK-1)*NTH)
        !
        ! Second part of ice term integration: scattering including re-distribution in directions
        !
#ifdef W3_IS2
        IF (IS2PARS(2).GE.0) THEN
          IF (IS2PARS(20).GT.0.5) THEN
            !
            ! Case of isotropic back-scatter: the directional spectrum is decomposed into
            !               - an isotropic part (ISO): eigenvalue of scattering is 0
            !               - the rest     (SPEC-ISO): eigenvalue of scattering is VDIR(IS)
            !
            SCAT = EXP(VDIR(IS)*IS2PARS(2)*DTG)
            ISO = SUM(SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH))/NTH
            SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH) = ISO &
                 +(SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH)-ISO)*SCAT
          ELSE
            !
            ! General solution with matrix exponentials: same as bottom scattering, see Ardhuin & Herbers (JFM 2002)
            !
            SCATSPEC(1:NTH)=DBLE(SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH))
            SPEC(1+(IK-1)*NTH:NTH+(IK-1)*NTH) =  &
                 REAL(MATMUL(IS2EIGVEC(:,:), EXP(IS2EIGVAL(:)*VDIR(IS)*DTG*IS2PARS(2)) &
                 *MATMUL(TRANSPOSE(IS2EIGVEC(:,:)),SCATSPEC)))
          END IF
        END IF
#endif
        !
        ! 10.2  Fluxes of energy and momentum due to ice effects
        !
        FACTOR = DDEN(IK)/CG1(IK)                    !Jacobian to get energy in band
        FACTOR2= FACTOR*GRAV*WN1(IK)/SIG(IK)         ! coefficient to get momentum
        DO ITH = 1,NTH
          IS = ITH+(IK-1)*NTH
          PHICE = PHICE + (SPEC(IS)-SPEC2(IS)) * FACTOR
          COSI(1)=ECOS(IS)
          COSI(2)=ESIN(IS)
          TAUICE(:) = TAUICE(:) - (SPEC(IS)-SPEC2(IS))*FACTOR2*COSI(:)
        END DO
      END DO
      PHICE =-1.*DWAT*GRAV*PHICE /DTG
      TAUICE(:)=TAUICE(:)/DTG
    ELSE
#ifdef W3_IS2
      IF (IS2PARS(10).LT.0.5) THEN
        ICEF = 0.
      ENDIF
#endif
    END IF
    !
    !
    ! - - - - - - - - - - - - - - - - - - - - - -
    ! 11. Sea state dependent stress routine calls
    ! - - - - - - - - - - - - - - - - - - - - - -
    !Note the Sea-state dependent stress calculations are primarily for high-wind
    !conditions (>10 m/s).  It is not recommended to use these at lower wind
    !in their current state.
    !
#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) '4 : sum(SPEC)=', sum(SPEC)
    END IF
#endif

    ! FLD1/2 requires the calculation of FPI:
#ifdef W3_FLD1
    CALL CALC_FPI(SPEC, CG1, FPI, VSIN )
#endif
#ifdef W3_FLD2
    CALL CALC_FPI(SPEC, CG1, FPI, VSIN )
#endif
    !
#ifdef W3_FLD1
    IF (U10ABS.GT.10. .and. HSTOT.gt.0.5) then
      CALL W3FLD1 ( SPEC,min(FPI/TPI,2.0),COEF*U10ABS*COS(U10DIR),        &
           COEF*U10ABS*Sin(U10DIR), ZWND, DEPTH, 0.0, &
           DAIR, USTAR, USTDIR, Z0,TAUNUX,TAUNUY,CHARN)
    ELSE
      CHARN = AALPHA
    ENDIF
#endif
#ifdef W3_FLD2
    IF (U10ABS.GT.10. .and. HSTOT.gt.0.5) then
      CALL W3FLD2 ( SPEC,min(FPI/TPI,2.0),COEF*U10ABS*COS(U10DIR),        &
           COEF*U10ABS*Sin(U10DIR), ZWND, DEPTH, 0.0, &
           DAIR, USTAR, USTDIR, Z0,TAUNUX,TAUNUY,CHARN)
    ELSE
      CHARN = AALPHA
    ENDIF
#endif
    !
    ! 12. includes shoreline reflection --------------------------------------------- *
    !
#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) '5 : sum(SPEC)=', sum(SPEC)
    END IF
#endif

#ifdef W3_REF1
    IF (REFLEC(1).GT.0.OR.REFLEC(2).GT.0.OR.(REFLEC(4).GT.0.AND.BERG.GT.0)) THEN
      CALL W3SREF ( SPEC, CG1, WN1, EMEAN, FMEAN, DEPTH, CX, CY,   &
           REFLEC, REFLED, TRNX, TRNY,  &
           BERG, DTG, IX, IY, JSEA, VREF )
      IF (GTYPE.EQ.UNGTYPE.AND.REFPARS(3).LT.0.5) THEN
#ifdef W3_PDLIB
        IF (IOBP_LOC(JSEA).EQ.0) THEN
#else
        IF (IOBP(IX).EQ.0) THEN
#endif
          DO IK=1, NK
            DO ITH=1, NTH
              ISP = ITH+(IK-1)*NTH
#ifdef W3_PDLIB
              IF (IOBPD_LOC(ITH,JSEA).EQ.0) SPEC(ISP) = DTG*VREF(ISP)
#else
              IF (IOBPD(ITH,IX).EQ.0) SPEC(ISP) = DTG*VREF(ISP)
#endif
            END DO
          END DO
        ELSE
          SPEC(:) = SPEC(:) + DTG * VREF(:)
        ENDIF
      ELSE
        SPEC(:) = SPEC(:) + DTG * VREF(:)
      END IF
    END IF
#endif

    !
#ifdef W3_DEBUGSRC
    IF (IX .eq. DEBUG_NODE) THEN
      WRITE(740+IAPROC,*) '6 : sum(SPEC)=', sum(SPEC)
    END IF
#endif

    FIRST  = .FALSE.

    IF (IT.EQ.0) SPEC = SPECINIT

    SPEC = MAX(0., SPEC)
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_NNT
8000 FORMAT (/' *** ERROR W3SRCE : ERROR IN OPENING FILE ',A,' ***'/ &
         '                    IOSTAT = ',I10/)
8001 FORMAT (/' *** ERROR W3SRCE : ERROR IN WRITING TO FILE ***'/    &
         '                    IOSTAT = ',I10/)
#endif
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SRCE : COUNTERS   : NO LONGER AVAILABLE')
9001 FORMAT (' TEST W3SRCE : DEPTH      :',F8.1/                     &
         '               WIND SPEED :',F8.1/                     &
         '               WIND DIR   :',F8.1)
#endif
#ifdef W3_ST1
9004 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
#ifdef W3_ST2
9005 FORMAT (' TEST W3SRCE : FHIGH      : ',F8.4/                    &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
#ifdef W3_ST3
9006 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
#ifdef W3_ST4
9006 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         ' ------------- NEW DYNAMIC INTEGRATION LOOP',          &
         ' ------------- ')
#endif
    !
#ifdef W3_T
9020 FORMAT (' TEST W3SRCE : NSTEP : ',I4,'    DTTOT :',F6.1)
9021 FORMAT (' TEST W3SRCE : NKH (3X)   : ',2I3,I6)
9040 FORMAT (' TEST W3SRCE : DTRAW, DT, SHAVE :',2F6.1,2X,L1)
#endif
    !
#ifdef W3_ST1
9060 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST2
9061 FORMAT (' TEST W3SRCE : FHIGH (2X) : ',2F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST3
9062 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST4
9062 FORMAT (' TEST W3SRCE : FHIGH (3X) : ',3F8.4/                   &
         '               NKH        : ',I3)
#endif
#ifdef W3_ST6
9063 FORMAT (' TEST W3SRCE : FHIGH      : ',F8.4/                    &
         '               NKH        : ',I3)
#endif
    !/
    !/ End of W3SRCE ----------------------------------------------------- /
    !/
  END SUBROUTINE W3SRCE
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Calculate equivalent peak frequency.
  !>
  !> @details Tolman and Chalikov (1996), equivalent peak frequency from source.
  !>
  !> @param[in]  A    Action density spectrum (1-D).
  !> @param[in]  CG   Group velocities for k-axis of spectrum.
  !> @param[out]  FPI  Input 'peak' frequency.
  !> @param[in] S    Source term (1-D version).
  !>
  !> @author Jessica Meixner
  !> @date   06-Jun-2018
  !>
  SUBROUTINE CALC_FPI( A, CG, FPI, S )
    USE CONSTANTS
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, XFR, DDEN, SIG,FTE, FTTR
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NSPEC), CG(NK), S(NSPEC)
    REAL, INTENT(OUT)       :: FPI
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IS, IK
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    ::  M0, M1, SIN1A(NK)
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'CALC_FPI')
#endif
    !
    !     Calculate FPI: equivalent peak frequncy from wind source term
    !     input
    !
    DO IK=1, NK
      SIN1A(IK) = 0.
      DO IS=(IK-1)*NTH+1, IK*NTH
        SIN1A(IK) = SIN1A(IK) + MAX ( 0. , S(IS) )
      END DO
    END DO
    !
    M0     = 0.
    M1     = 0.
    DO IK=1, NK
      SIN1A(IK) = SIN1A(IK) * DDEN(IK) / ( CG(IK) * SIG(IK)**3 )
      M0        = M0 + SIN1A(IK)
      M1        = M1 + SIN1A(IK)/SIG(IK)
    END DO
    !
    SIN1A(NK) = SIN1A(NK) / DDEN(NK)
    M0        = M0 + SIN1A(NK) * FTE
    M1        = M1 + SIN1A(NK) * FTTR
    IF ( M1 .LT. 1E-20 ) THEN
      FPI    = XFR * SIG(NK)
    ELSE
      FPI    = M0 / M1
    END IF

  END SUBROUTINE CALC_FPI
  !/ ------------------------------------------------------------------- /!

  !>
  !> @brief Put source term in matrix same as done always.
  !>
  !> @param[in]    SPEC
  !> @param[inout] VS
  !> @param[inout] VD
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3(SPEC, VS, VD)
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3GDATMD, only : NTH, NK, NSPEC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/

    INTEGER             :: ISP, ITH, IK, IS
    REAL, INTENT(IN)    :: SPEC(NSPEC)
    REAL, INTENT(INOUT) :: VS(NSPEC), VD(NSPEC)
#ifdef W3_S
    CALL STRACE (IENT, 'SIGN_VSD_SEMI_IMPLICIT_WW3')
#endif
    DO IS=1,NSPEC
      VD(IS) = MIN(0., VD(IS))
    END DO
  END SUBROUTINE SIGN_VSD_SEMI_IMPLICIT_WW3
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Put source term in matrix Patankar style (experimental).
  !>
  !> @param[in]    SPEC
  !> @param[inout] VS
  !> @param[inout] VD
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SIGN_VSD_PATANKAR_WW3(SPEC, VS, VD)
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !

    USE W3GDATMD, only : NTH, NK, NSPEC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
    INTEGER             :: ISP, ITH, IK, IS
    REAL, INTENT(IN)    :: SPEC(NSPEC)
    REAL, INTENT(INOUT) :: VS(NSPEC), VD(NSPEC)
#ifdef W3_S
    CALL STRACE (IENT, 'SIGN_VSD_PATANKAR_WW3')
#endif
    DO IS=1,NSPEC
      VD(IS) = MIN(0., VD(IS))
      VS(IS) = MAX(0., VS(IS))
    END DO
  END SUBROUTINE SIGN_VSD_PATANKAR_WW3
  !/
  !/ End of module W3SRCEMD -------------------------------------------- /
  !/
END MODULE W3SRCEMD
