  SUBROUTINE W3OUTG ( A, FLPART, FLOUTG, FLOUTG2 )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         10-Apr-2015 |
    !/                  +-----------------------------------+
    !/
    !/    10-Dec-1998 : Distributed FORTRAN 77 version.     ( version 1.18 )
    !/    04-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
    !/    19-Oct-2004 : Multiple grid version.              ( version 3.06 )
    !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
    !/    23-Apr-2006 : Filter for directional spread.      ( version 3.09 )
    !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
    !/                  Adding user slots for outputs.
    !/    08-Oct-2007 : Adding ST3 source term option.      ( version 3.13 )
    !/                  ( F. Ardhuin )
    !/    05-Mar-2008 : Added NEC sxf90 compiler directives
    !/                  (Chris Bunney, UK Met Office)       ( version 3.13 )
    !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
    !/                  memory footprint.
    !/    10-Feb-2014 : Bug correction for US3D: div. by df ( version 4.18 )
    !/    30-Apr-2014 : Add th2m and sth2m calculation      ( version 5.01 )
    !/    27-May-2014 : Switch to OMPG switch.              ( version 5.02 )
    !/    10-Apr-2015 : Remove unused variables             ( version 5.08 )
    !/    10-Jan-2017 : Separate Stokes drift calculation   ( version 6.01 )
    !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
    !/                  processing code)
    !/    22-Aug-2018 : Add WBT parameter                   ( version 6.06 )
    !/    25-Sep-2019 : Corrected th2m and sth2m            ( version 6.07 )
    !/                  calculations. (J Dykes, NRL)
    !/
    !  1. Purpose :
    !
    !     Fill necessary arrays with gridded data for output.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       A       R.A.   I   Input spectra. Left in par list to change
    !                          shape.
    !       FLPART  Log.   I   Flag for filling fields with part. data.
    !       FLOUTG  Log.   I   Flag for file field output
    !       FLOUTG2 Log.   I   Flag for coupling field output
    !     ----------------------------------------------------------------
    !
    !     Locally saved parameters
    !     ----------------------------------------------------------------
    !       HSMIN   Real  Filter level in Hs for calculation of mean
    !                     wave parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     See module documentation.
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/SHRD  Switch for shared / distributed memory architecture.
    !     !/DIST  Id.
    !
    !     !/OMPG  OpenMP compiler directive for loop splitting.
    !
    !     !/O8    Filter for low wave heights ( HSMIN )
    !     !/O9    Negative wave height alowed, other mean parameters will
    !             not be correct.
    !
    !     !/ST0   No source terms.
    !     !/ST1   Source term set 1 (WAM equiv.)
    !     !/ST2   Source term set 2 (Tolman and Chalikov)
    !     !/ST3   Source term set 3 (WAM 4+)
    !     !/ST6   Source term set 6 (BYDRZ)
    !
    !     !/S     Enable subroutine tracing.
    !     !/T     Test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD
    USE W3WDATMD, ONLY: UST, FPIS
    USE W3ADATMD, ONLY: CG, WN, DW
    USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0,               &
         THM, THS, THP0
    USE W3ADATMD, ONLY: ABA, ABD, UBA, UBD, FCUT, SXX,              &
         SYY, SXY, PHS, PTP, PLP, PDIR, PSI, PWS,    &
         PWST, PNR, USERO, TUSX, TUSY, PRMS, TPMS,   &
         USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,   &
         MSCD, CHARN,                                &
         BHD, CGE, P2SMS, US3D, EF, TH1M, STH1M,     &
         TH2M, STH2M, HSIG, STMAXE, STMAXD,          &
         HCMAXE, HMAXE, HCMAXD, HMAXD, USSP, QP, PQP,&
         PTHP0, PPE, PGW, PSW, PTM1, PT1, PT2, PEP,  &
         WBT
    USE W3ODATMD, ONLY: NDST, UNDEF, IAPROC, NAPROC, NAPFLD,        &
         ICPRT, DTPRT, WSCUT, NOSWLL, FLOGRD, FLOGR2,&
         NOGRP, NGRPP
    USE W3ADATMD, ONLY: NSEALM
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3PARALL, ONLY : INIT_GET_ISEA
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: A(NTH,NK,0:NSEAL)
    LOGICAL, INTENT(IN)     :: FLPART, FLOUTG, FLOUTG2
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IK, ITH, JSEA, ISEA, IX, IY,         &
         IKP0(NSEAL), NKH(NSEAL),             &
         I, J, LKMS, HKMS, ITL
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: FXPMC, FACTOR, FACTOR2, EBAND, FKD,  &
         AABS, UABS,                          &
         XL, XH, XL2, XH2, EL, EH, DENOM, KD, &
         M1, M2, MA, MB, MC, STEX, STEY, STED
    REAL                    :: ET(NSEAL), EWN(NSEAL), ETR(NSEAL),   &
         ETX(NSEAL), ETY(NSEAL), AB(NSEAL),   &
         ETXX(NSEAL), ETYY(NSEAL), ETXY(NSEAL),&
         ABX(NSEAL), ABY(NSEAL),ET02(NSEAL),  &
         EBD(NK,NSEAL), EC(NSEAL),            &
         ABR(NSEAL), UBR(NSEAL), UBS(NSEAL),  &
         ABX2(NSEAL), ABY2(NSEAL),            &
         AB2X(NSEAL), AB2Y(NSEAL),            &
         ABST(NSEAL), ABXX(NSEAL),            &
         ABYY(NSEAL), ABXY(NSEAL),            &
         ABYX(NSEAL), EET1(NSEAL),            &
         ETUSCX(NSEAL), ETUSCY(NSEAL),        &
         ETMSSL(NSEAL), ETMSSCL(NSEAL),       &
         ETTPMM(NSEAL), ETF(NSEAL),           &
         ET1(NSEAL), ABX2M(NSEAL),            &
         ABY2M(NSEAL), ABXM(NSEAL),           &
         ABYM(NSEAL), ABXYM(NSEAL),           &
         MSSXM(NSEAL), MSSYM(NSEAL),          &
         MSSXTM(NSEAL), MSSYTM(NSEAL),        &
         MSSXYM(NSEAL), THMP(NSEAL),          &
         T02P(NSEAL), NV(NSEAL), NS(NSEAL),   &
         NB(NSEAL), MODE(NSEAL),              &
         MU(NSEAL), NI(NSEAL), STMAXEL(NSEAL),&
         PHI(21,NSEAL),PHIST(NSEAL),         &
         EBC(NK,NSEAL), ABP(NSEAL),           &
         STMAXDL(NSEAL), TLPHI(NSEAL),        &
         WL02X(NSEAL), WL02Y(NSEAL),          &
         ALPXT(NSEAL), ALPYT(NSEAL),          &
         ALPXY(NSEAL), SCREST(NSEAL)
    REAL                       USSCO, FT1
    REAL, SAVE              :: HSMIN = 0.01
    LOGICAL                 :: FLOLOC(NOGRP,NGRPP)
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3OUTG')
#endif
    DO I=1,NOGRP
      DO J=1,NGRPP
        FLOLOC(I,J) =   &
             ((FLOUTG.AND.FLOGRD(I,J)).OR.(FLOUTG2.AND.FLOGR2(I,J)))
      END DO
    END DO
    !
    FXPMC  = 0.66 * GRAV / 28.
    HSMIN  = HSMIN
    FT1    =  0.3333 * SIG(NK)**2 * DTH * SIG(NK)
    !
    ! 1.  Initialize storage arrays -------------------------------------- *
    !
    ET     = 0.
    ET02   = 0.
    EWN    = 0.
    ETR    = 0.
    ET1    = 0.
    EET1   = 0.
    ETX    = 0.
    ETY    = 0.
    ETXX   = 0.
    ETYY   = 0.
    ETXY   = 0.
    ABR    = 0.
    ABA    = 0.
    ABD    = 0.
    UBR    = 0.
    UBA    = 0.
    UBD    = 0.
    UBS    = 0.
    SXX    = 0.
    SYY    = 0.
    SXY    = 0.
    USSX   = 0.
    USSY   = 0.
    TUSX   = 0.
    TUSY   = 0.
    MSSX   = 0.
    MSSY   = 0.
    MSSD   = 0.
    MSCX   = 0.
    MSCY   = 0.
    MSCD   = 0.
    PRMS   = 0.
    TPMS   = 0.
    ETUSCY = 0.
    ETUSCY = 0.
    ETMSSL = 0.
    ETMSSCL= 0.
    ETTPMM = 0.
    EBD    = 0.
    EC     = 0.
    ETF    = 0.
    EBC    = 0.
    BHD = 0.
    MSSXM = 0.
    MSSYM = 0.
    MSSXTM = 0.
    MSSYTM = 0.
    MSSXYM = 0.
    PHI    = 0.
    PHIST  = 0.
    TLPHI  = 0.
    STMAXEL = 0.
    STMAXDL = 0.
    !
    HS     = UNDEF
    WLM    = UNDEF
    T0M1   = UNDEF
    T01    = UNDEF
    T02    = UNDEF
    FP0    = UNDEF
    THM    = UNDEF
    THS    = UNDEF
    THP0   = UNDEF
    HSIG   = UNDEF
    WL02X  = UNDEF
    WL02Y  = UNDEF
    ALPXY  = UNDEF
    ALPXT  = UNDEF
    ALPYT  = UNDEF
    THMP = UNDEF
    T02P = UNDEF
    SCREST = UNDEF
    NV = UNDEF
    NS = UNDEF
    NB = UNDEF
    MU = UNDEF
    NI = UNDEF
    MODE = UNDEF
    STMAXE = UNDEF
    STMAXD = UNDEF
    HCMAXE = UNDEF
    HMAXE = UNDEF
    HCMAXD = UNDEF
    HMAXD = UNDEF
    QP    = UNDEF
    WBT    = UNDEF
    !
    ! 2.  Integral over discrete part of spectrum ------------------------ *
    !
    DO IK=1, NK
      !
      ! 2.a Initialize energy in band
      !
      AB     = 0.
      ABX    = 0.
      ABY    = 0.
      ABX2   = 0.
      ABY2   = 0.
      AB2X   = 0.
      AB2Y   = 0.
      ABXX   = 0.
      ABYY   = 0.
      ABXY   = 0.
      ABYX   = 0.
      ABST   = 0.
      !
      ! 2.b Integrate energy in band
      !
      DO ITH=1, NTH
        !
#ifdef W3_OMPG
        !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR)
#endif
        !
        DO JSEA=1, NSEAL
          NKH(JSEA)  = MIN ( NK ,   &
               INT(FACTI2+FACTI1*LOG(MAX(1.E-7,FCUT(JSEA)))) )
          AB (JSEA)  = AB (JSEA) + A(ITH,IK,JSEA)
          ABX(JSEA)  = ABX(JSEA) + A(ITH,IK,JSEA)*ECOS(ITH)
          ABY(JSEA)  = ABY(JSEA) + A(ITH,IK,JSEA)*ESIN(ITH)
          ! These are the integrals with cos^2 and sin^2
          ABX2(JSEA) = ABX2(JSEA) + A(ITH,IK,JSEA)*EC2(ITH)
          ABY2(JSEA) = ABY2(JSEA) + A(ITH,IK,JSEA)*ES2(ITH)
          ! Using trig identities to represent cos2theta and sin2theta.
          AB2X(JSEA) = AB2X(JSEA) + A(ITH,IK,JSEA)*(2*EC2(ITH) - 1)
          AB2Y(JSEA) = AB2Y(JSEA) + A(ITH,IK,JSEA)*(2*ESC(ITH))
          ABYX(JSEA) = ABYX(JSEA) + A(ITH,IK,JSEA)*ESC(ITH)
          IF (ITH.LE.NTH/2) THEN
            ABST(JSEA) = ABST(JSEA) +                               &
                 A(ITH,IK,JSEA)*A(ITH+NTH/2,IK,JSEA)
          END IF
          CALL INIT_GET_ISEA(ISEA, JSEA)
          FACTOR     = MAX ( 0.5 , CG(IK,ISEA)/SIG(IK)*WN(IK,ISEA) )
          ABXX(JSEA) = ABXX(JSEA) + ((1.+EC2(ITH))*FACTOR-0.5) *    &
               A(ITH,IK,JSEA)
          ABYY(JSEA) = ABYY(JSEA) + ((1.+ES2(ITH))*FACTOR-0.5) *    &
               A(ITH,IK,JSEA)
          ABXY(JSEA) = ABXY(JSEA) + ESC(ITH)*FACTOR * A(ITH,IK,JSEA)
        END DO
        !
#ifdef W3_OMPG
        !$OMP END PARALLEL DO
#endif
        !
      END DO
      !
      ! 2.c Finalize integration over band and update mean arrays
      !
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR,FACTOR2,MA,MC,MB,KD,FKD,USSCO,M1,M2)
#endif
      !
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        FACTOR       = DDEN(IK) / CG(IK,ISEA)
        EBD(IK,JSEA) = AB(JSEA) * FACTOR
        ET (JSEA)    = ET (JSEA) + EBD(IK,JSEA)
#ifdef W3_IG1
        IF (IK.EQ.NINT(IGPARS(5))) HSIG(JSEA) = 4*SQRT(ET(JSEA))
#endif
        ETF(JSEA)  = ETF(JSEA) + EBD(IK,JSEA) * CG(IK,ISEA)
        EWN(JSEA)  = EWN(JSEA) + EBD(IK,JSEA) / WN(IK,ISEA)
        ETR(JSEA)  = ETR(JSEA) + EBD(IK,JSEA) / SIG(IK)
        ET1(JSEA)  = ET1(JSEA) + EBD(IK,JSEA) * SIG(IK)
        EET1(JSEA) = EET1(JSEA)+ EBD(IK,JSEA)**2 * SIG(IK)
        ET02(JSEA) = ET02(JSEA)+ EBD(IK,JSEA) * SIG(IK)**2
        ETX(JSEA)  = ETX(JSEA) + ABX(JSEA) * FACTOR
        ETY(JSEA)  = ETY(JSEA) + ABY(JSEA) * FACTOR
        TUSX(JSEA) = TUSX(JSEA) + ABX(JSEA)*FACTOR               &
             *GRAV*WN(IK,ISEA)/SIG(IK)
        TUSY(JSEA)  = TUSY(JSEA) + ABY(JSEA)*FACTOR               &
             *GRAV*WN(IK,ISEA)/SIG(IK)
        ETXX(JSEA) = ETXX(JSEA) + ABX2(JSEA) * FACTOR* WN(IK,ISEA)**2
        ETYY(JSEA) = ETYY(JSEA) + ABY2(JSEA) * FACTOR* WN(IK,ISEA)**2
        ETXY(JSEA) = ETXY(JSEA) + ABYX(JSEA) * FACTOR* WN(IK,ISEA)**2
        IF (SIG(IK)*0.5*(1+XFR).LT.0.4*TPI) THEN
          ETMSSL(JSEA)  = ETMSSL(JSEA) + AB(JSEA)*FACTOR           &
               *WN(IK,ISEA)**2
        ELSE
          IF (SIG(MAX(IK-1,1))*0.5*(1+XFR).LT.0.4*TPI) THEN
            ETMSSL(JSEA)  = ETMSSL(JSEA) + AB(JSEA)*FACTOR         &
                 *(SIG(IK)*0.5*(1+1/XFR)-(0.4*TPI))/DSII(IK)     &
                 *WN(IK,ISEA)**2
            FACTOR2       = SIG(IK)**5/(GRAV**2)/DSII(IK)
            ETMSSCL(JSEA) = AB(JSEA)*FACTOR*FACTOR2
          END IF
        END IF
        !
        UBS(JSEA) = UBS(JSEA) + AB(JSEA) * SIG(IK)**2
        !
        !   2nd order equivalent surface pressure spectral density at K=0
        !   this is used for microseismic or microbarom sources
        !   Finite water depth corrections (Ardhuin & Herbers 2013) are not
        !   included here.
        !
        FACTOR2 = DTH*2/(TPI**2)                        &
             * SIG(IK)                             &
             * (TPI*SIG(IK)/CG(IK,ISEA))**2        &  ! Jacobian^2 to get E(f,th) from A(k,th)
             * ABST(JSEA)
        !
        !   Integration over seismic radian frequency : *2*dsigma
        !
        PRMS(JSEA)  = PRMS(JSEA) + FACTOR2 * 2 * DSII(IK)
        IF ( FLOLOC (6, 9).AND.(IK.GE.P2MSF(2).AND.IK.LE.P2MSF(3)))   &
             P2SMS(JSEA,IK) = FACTOR2 * 2 * TPI
        IF (FACTOR2 .GT. ETTPMM(JSEA)) THEN
          ETTPMM(JSEA) = FACTOR2
          TPMS(JSEA) = TPI/SIG(IK)
        END IF

        !
        ! Directional moments in the last freq. band
        !
        IF (IK.EQ.NK) THEN
          FACTOR2       = SIG(IK)**5/(GRAV**2)/DSII(IK)
          ETUSCX(JSEA)  = ABX(JSEA)*FACTOR*FACTOR2
          ETUSCY(JSEA)  = ABY(JSEA)*FACTOR*FACTOR2
          !
          !     NB: the slope PDF is proportional to ell1=ETYY*EC2-2*ETXY*ECS+ETYY*ES2 = A*EC2-2*B*ECS+C*ES2
          !     This is an ellipse equation with axis direction given by dir=0.5*ATAN2(-2.*ETXY,ETYY-ETXX)
          !
          MA  = ABX2(JSEA) * FACTOR * FACTOR2
          MC  = ABY2(JSEA) * FACTOR * FACTOR2
          MB  = ABYX(JSEA) * FACTOR * FACTOR2
          !
          ! Old definitions:  MSCX(JSEA)  = ABX2(JSEA) * FACTOR * FACTOR2
          !                   MSCY(JSEA)  = ABY2(JSEA) * FACTOR * FACTOR2
          MSCD(JSEA)=0.5*ATAN2(2*MB,MA-MC)

          MSCX(JSEA)= MA*COS(MSCD(JSEA))**2   &
               +2*MB*SIN(MSCD(JSEA))*COS(MSCD(JSEA))+MA*SIN(MSCD(JSEA))**2
          MSCY(JSEA)= MC*COS(MSCD(JSEA))**2   &
               -2*MB*SIN(MSCD(JSEA))*COS(MSCD(JSEA))+MA*SIN(MSCD(JSEA))**2
        END IF
        !
        ! Deep water limits
        !
        KD    = MAX ( 0.001 , WN(IK,ISEA) * DW(ISEA) )
        IF ( KD .LT. 6. ) THEN
          FKD       = FACTOR / SINH(KD)**2
          ABR(JSEA) = ABR(JSEA) + AB(JSEA) * FKD
          ABA(JSEA) = ABA(JSEA) + ABX(JSEA) * FKD
          ABD(JSEA) = ABD(JSEA) + ABY(JSEA) * FKD
          UBR(JSEA) = UBR(JSEA) + AB(JSEA) * SIG(IK)**2 * FKD
          UBA(JSEA) = UBA(JSEA) + ABX(JSEA) * SIG(IK)**2 * FKD
          UBD(JSEA) = UBD(JSEA) + ABY(JSEA) * SIG(IK)**2 * FKD
          USSCO=FKD*SIG(IK)*WN(IK,ISEA)*COSH(2.*KD)
          BHD(JSEA) = BHD(JSEA) +                             &
               GRAV*WN(IK,ISEA) * EBD(IK,JSEA) / (SINH(2.*KD))
        ELSE
          USSCO=FACTOR*SIG(IK)*2.*WN(IK,ISEA)
        END IF
        !
        ABXX(JSEA)   = MAX ( 0. , ABXX(JSEA) ) * FACTOR
        ABYY(JSEA)   = MAX ( 0. , ABYY(JSEA) ) * FACTOR
        ABXY(JSEA)   = ABXY(JSEA) * FACTOR
        SXX(JSEA)    = SXX(JSEA)  + ABXX(JSEA)
        SYY(JSEA)    = SYY(JSEA)  + ABYY(JSEA)
        SXY(JSEA)    = SXY(JSEA)  + ABXY(JSEA)
        EBD(IK,JSEA) = EBD(IK,JSEA) / DSII(IK)
        !
        IF ( FLOLOC( 3, 1).AND.(IK.GE.E3DF(2,1).AND.IK.LE.E3DF(3,1)))   &
             EF(JSEA,IK)  = EBD(IK,JSEA) * TPI
        !
        USSX(JSEA)  = USSX(JSEA) + ABX(JSEA)*USSCO
        USSY(JSEA)  = USSY(JSEA) + ABY(JSEA)*USSCO
        !
        ! Fills the 3D Stokes drift spectrum array
        !  ! The US3D Stokes drift specrum array is now calculated in a
        !  subroutine and called at the end of this subroutine
        !          IF ( FLOLOC( 6, 8).AND.(IK.GE.US3DF(2).AND.IK.LE.US3DF(3) ))   THEN
        !            US3D(JSEA,IK)    =  ABX(JSEA)*USSCO/(DSII(IK)*TPIINV)
        !            US3D(JSEA,NK+IK) =  ABY(JSEA)*USSCO/(DSII(IK)*TPIINV)
        !          END IF
        IF ( FLOLOC( 3, 2).AND.(IK.GE.E3DF(2,2).AND.IK.LE.E3DF(3,2)))  &
             TH1M(JSEA,IK)= MOD ( 630. - RADE*ATAN2(ABY(JSEA),ABX(JSEA)) , 360. )
        M1 = SQRT(ABX(JSEA)**2+ABY(JSEA)**2)/MAX(1E-20,AB(JSEA))
        IF ( FLOLOC( 3, 3).AND.(IK.GE.E3DF(2,3).AND.IK.LE.E3DF(3,3)))  &
             STH1M(JSEA,IK)= SQRT(ABS(2.*(1-M1)))*RADE
        IF ( FLOLOC( 3, 4).AND.(IK.GE.E3DF(2,4).AND.IK.LE.E3DF(3,4)))  &
             TH2M(JSEA,IK)= MOD ( 270. - RADE*0.5*ATAN2(ABY2(JSEA),AB2X(JSEA)) , 180. )
        M2 = SQRT(AB2X(JSEA)**2+AB2Y(JSEA)**2)/MAX(1E-20,AB(JSEA))
        IF ( FLOLOC( 3, 5).AND.(IK.GE.E3DF(2,5).AND.IK.LE.E3DF(3,5)))  &
             STH2M(JSEA,IK)= SQRT(ABS(0.5*(1-M2)))*RADE
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !
    END DO
    !
    ! Start of Space-Time Extremes Section
    IF ( ( STEXU .GT. 0. .AND. STEYU .GT. 0. ) &
         .OR. ( STEDU .GT. 0. ) ) THEN
      !  Space-Time extremes
      !    (for references:
      !     - Krogstad et al, OMAE 2004
      !     - Baxevani and Rychlik, OE 2006
      !     - Adler and Taylor, 2007
      !     - Fedele, JPO 2012
      !     - Fedele et al, OM 2013
      !     - Benetazzo et al, JPO 2015)
      !
      !  Compute spectral parameters wrt the mean wave direction
      !  (no tail contribution - Prognostic)
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        IF ( MAPSTA(IY,IX) .GT. 0 ) THEN
          IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 ) THEN
            THMP(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
          END IF
        END IF
      END DO
      !
      DO IK=1, NK
        !
        ABX2M = 0.
        ABY2M = 0.
        ABXM = 0.
        ABYM = 0.
        ABXYM = 0.
        !
        DO ITH=1, NTH
          !
#ifdef W3_OMPG
          !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
          !
          DO JSEA=1, NSEAL
            ABX2M(JSEA) = ABX2M(JSEA) + A(ITH,IK,JSEA)*                &
                 (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))**2
            ABY2M(JSEA) = ABY2M(JSEA) + A(ITH,IK,JSEA)*                &
                 (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))**2
            ABXM(JSEA)  = ABXM(JSEA) + A(ITH,IK,JSEA)*                 &
                 (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))
            ABYM(JSEA)  = ABYM(JSEA) + A(ITH,IK,JSEA)*                 &
                 (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))
            ABXYM(JSEA) = ABXYM(JSEA) + A(ITH,IK,JSEA)*                &
                 (ECOS(ITH)*COS(THMP(JSEA))+ESIN(ITH)*SIN(THMP(JSEA)))*   &
                 (ESIN(ITH)*COS(THMP(JSEA))-ECOS(ITH)*SIN(THMP(JSEA)))
          END DO
          !
#ifdef W3_OMPG
          !$OMP END PARALLEL DO
#endif
          !
        END DO
        !
#ifdef W3_OMPG
        !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,FACTOR)
#endif
        !
        DO JSEA=1, NSEAL
          CALL INIT_GET_ISEA(ISEA, JSEA)
          FACTOR       = DDEN(IK) / CG(IK,ISEA)
          MSSXM(JSEA)  = MSSXM(JSEA) + ABX2M(JSEA)*FACTOR*             &
               WN(IK,ISEA)**2
          MSSYM(JSEA)  = MSSYM(JSEA) + ABY2M(JSEA)*FACTOR*             &
               WN(IK,ISEA)**2
          MSSXTM(JSEA)  = MSSXTM(JSEA) + ABXM(JSEA)*FACTOR*WN(IK,ISEA)* &
               SIG(IK)
          MSSYTM(JSEA)  = MSSYTM(JSEA) + ABYM(JSEA)*FACTOR*WN(IK,ISEA)* &
               SIG(IK)
          MSSXYM(JSEA)  = MSSXYM(JSEA) + ABXYM(JSEA)*FACTOR*           &
               WN(IK,ISEA)**2
        END DO
        !
#ifdef W3_OMPG
        !$OMP END PARALLEL DO
#endif
        !
      END DO

#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA,STEX,STEY,STED,ITL,IK)
#endif
      !
      DO JSEA=1, NSEAL
        !
        !  Mean wave period (no tail contribution - Prognostic)
        IF ( ET02(JSEA) .GT. 1.E-7 ) THEN
          T02P(JSEA) = TPI * SQRT(ET(JSEA) / ET02(JSEA) )
        END IF
        !
        !  Mean wavelength and mean crest length (02) for space-time extremes
        IF ( MSSXM(JSEA) .GT. 1.E-7 ) THEN
          WL02X(JSEA) = TPI * SQRT(ET(JSEA) / MSSXM(JSEA))
        END IF
        IF ( MSSYM(JSEA) .GT. 1.E-7 ) THEN
          WL02Y(JSEA) = TPI * SQRT(ET(JSEA) / MSSYM(JSEA))
        END IF
        !
        !  Irregularity parameters for space-time extremes
        IF ((MSSXM(JSEA) .GT. 1.E-7) .AND. (ET02(JSEA) .GT. 1.E-7)) THEN
          ALPXT(JSEA) = MSSXTM(JSEA) / (SQRT(MSSXM(JSEA) * ET02(JSEA)))
        ENDIF
        IF ((MSSYM(JSEA) .GT. 1.E-7) .AND. (ET02(JSEA) .GT. 1.E-7)) THEN
          ALPYT(JSEA) = MSSYTM(JSEA) / (SQRT(MSSYM(JSEA) * ET02(JSEA)))
        ENDIF
        IF ((MSSXM(JSEA) .GT. 1.E-7) .AND. (MSSYM(JSEA) .GT. 1.E-7)) THEN
          ALPXY(JSEA) = MSSXYM(JSEA) / (SQRT(MSSXM(JSEA) * MSSYM(JSEA)))
        ENDIF
        !
        !  Short-crestedness parameter
        IF (MSSXM(JSEA) .GT. 1.E-7)  THEN
          SCREST(JSEA) = SQRT(MSSYM(JSEA)/MSSXM(JSEA))
        END IF
        !
        !  Space domain size (user-defined or default)
        IF ( STEXU .GT. 0 .AND. STEYU .GT. 0 ) THEN
          STEX = STEXU
          STEY = STEYU
        ELSE
          STEX = 0.
          STEY = 0.
        END IF
        !
        !  Time domain size (user-defined or default)
        IF ( STEDU .GT. 0 ) THEN
          STED = STEDU
        ELSE
          STED = 0.
        END IF
        !
        !  Average numbers of waves in the space-time domain (Volume+Sides+Borders)
        IF ((WL02X(JSEA) .GT. 1.E-7) .AND. (WL02Y(JSEA) .GT. 1.E-7)    &
             .AND. (T02P(JSEA) .GT. 1.E-7)) THEN
          NV(JSEA) = TPI*(STEX*STEY*STED)/                             &
               (WL02X(JSEA)*WL02Y(JSEA)*T02P(JSEA))  *                    &
               SQRT(1-ALPXT(JSEA)**2-ALPYT(JSEA)**2  -                    &
               ALPXY(JSEA)**2+2*ALPXT(JSEA)*ALPYT(JSEA)*ALPXY(JSEA))
          NS(JSEA) = SQRT(TPI)*((STEX*STED)/(WL02X(JSEA)*T02P(JSEA)) * &
               SQRT(1-ALPXT(JSEA)**2) +                                   &
               (STEY*STED)/(WL02Y(JSEA)*T02P(JSEA)) *                     &
               SQRT(1-ALPYT(JSEA)**2) +                                   &
               (STEX*STEY)/(WL02X(JSEA)*WL02Y(JSEA)) *                    &
               SQRT(1-ALPXY(JSEA)**2))
          NB(JSEA) = STEX/WL02X(JSEA) + STEY/WL02Y(JSEA) +             &
               STED/T02P(JSEA)
        END IF
        !
        ! Integral measure of wave steepness (Fedele & Tayfun, 2009) MU, as a
        ! function of the spectral width parameter NI (Longuet-Higgins, 1985)
        IF (ET1(JSEA) .GT. 1.E-7) THEN
          NI(JSEA) = SQRT(ET(JSEA)*ET02(JSEA)/ET1(JSEA)**2 - 1)
        ENDIF
        IF (ET(JSEA) .GT. 1.E-7) THEN
          MU(JSEA) = ET1(JSEA)**2/GRAV * (ET(JSEA))**(-1.5) *          &
               (1-NI(JSEA)+NI(JSEA)**2)
        ENDIF
        !
        ! Mode of the Adler&Taylor distribution
        ! (normalized on the standard deviation = Hs/4)
        ! Time extremes
        IF ((STEX .EQ. 0) .AND. (STEY .EQ. 0)) THEN
          MODE(JSEA) = SQRT(2.*LOG(NB(JSEA)))
          ! Space extremes (strictly for STEX*STEY >> WL02X*WL02Y)
        ELSEIF (STED .EQ. 0) THEN
          MODE(JSEA) = SQRT(2.*LOG(NS(JSEA))+LOG(2.*LOG(NS(JSEA))+     &
               LOG(2.*LOG(NS(JSEA)))))
          ! Space-time extremes (strictly for STEX*STEY >> WL02X*WL02Y)
        ELSEIF ((WL02X(JSEA) .GT. 1.E-7) .AND. (WL02Y(JSEA) .GT. 1.E-7) &
             .AND. (T02P(JSEA) .GT. 1.E-7)) THEN
          MODE(JSEA) = SQRT(2.*LOG(NV(JSEA))+2.*LOG(2.*LOG(NV(JSEA))+  &
               2.*LOG(2.*LOG(NV(JSEA)))))
        ENDIF
        !
        ! Expected maximum sea surface elevation in the ST domain - nonlinear
        ! (in meters, Hs/4=SQRT(ET(JSEA)))
        STMAXE(JSEA) = SQRT(ET(JSEA)) *                                &
             ( MODE(JSEA)+0.5*MU(JSEA)*MODE(JSEA)**2 +                  &
             0.5772*(1+MU(JSEA)*MODE(JSEA)) /                           &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        !
        ! Standard deviation of the maximum sea surface elevation in ST domain
        !  - nonlinear (in meters, Hs/4=SQRT(ET(JSEA)))
        STMAXD(JSEA) =  SQRT(ET(JSEA)) *                               &
             ( PI*(1+MU(JSEA)*MODE(JSEA))/SQRT(6.) /                    &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        !
        ! Autocovariance (time) function (normalized on the maximum, i.e. total
        ! variance)
        IF (T02P(JSEA) .GT. 1.E-7) THEN
          TLPHI(JSEA) = 0.3*T02P(JSEA)
          DO ITL = 1, 21
            DO IK = 1, NK-3, 4
              PHI(ITL,JSEA) = PHI(ITL,JSEA) +                          &
                   (XFR**3*EBD(IK+3,JSEA)*COS(XFR**3*SIG(IK)*TLPHI(JSEA))+    &
                   XFR**2*EBD(IK+2,JSEA)*COS(XFR**2*SIG(IK)*TLPHI(JSEA))+     &
                   XFR*EBD(IK+1,JSEA)*COS(XFR*SIG(IK)*TLPHI(JSEA)) +          &
                   EBD(IK,JSEA)*COS(SIG(IK)*TLPHI(JSEA)))*DSII(IK)
            ENDDO
            TLPHI(JSEA) = TLPHI(JSEA) + T02P(JSEA)/20.
          ENDDO
          PHI(:,JSEA) = PHI(:,JSEA)/ET(JSEA)
          !
          ! First minimum of the autocovariance function (absolute value)
          PHIST(JSEA) = ABS(MINVAL(PHI(:,JSEA),1))
        ENDIF
        !
        ! Wave height of the wave with the maximum expected crest height
        ! and corresponding standard deviation
        ! (according to Boccotti Quasi-Determinism theory - linear)
        STMAXEL(JSEA) = SQRT(ET(JSEA)) * ( MODE(JSEA)+0.5772 /         &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        STMAXDL(JSEA) = SQRT(ET(JSEA)) *                               &
             ( PI/SQRT(6.) /                                            &
             (MODE(JSEA)-(2*NV(JSEA)*MODE(JSEA)+NS(JSEA)) /             &
             (NV(JSEA)*MODE(JSEA)**2+NS(JSEA)*MODE(JSEA)+NB(JSEA))) )
        HCMAXE(JSEA) = STMAXEL(JSEA)*(1+PHIST(JSEA))
        HCMAXD(JSEA) = STMAXDL(JSEA)*(1+PHIST(JSEA))
        ! Maximum expected wave height and corresponding standard deviation
        ! (according to Boccotti Quasi-Determinism theory - linear)
        HMAXE(JSEA) = STMAXEL(JSEA)*SQRT(2*(1+PHIST(JSEA)))
        HMAXD(JSEA) = STMAXDL(JSEA)*SQRT(2*(1+PHIST(JSEA)))
      ENDDO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !

      ! End of Space-Time Extremes Section
    ENDIF
    !
    ! 3.  Finalize computation of mean parameters ------------------------ *
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,EBAND)
#endif
    !
    DO JSEA=1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      !
      ! 3.a Directional mss parameters
      !     NB: the slope PDF is proportional to ell1=ETYY*EC2-2*ETXY*ECS+ETXX*ES2 = C*EC2-2*B*ECS+A*ES2
      !     This is an ellipse equation with axis direction given by dir=0.5*ATAN2(2.*ETXY,ETXX-ETYY)
      !     From matlab script: t0=0.5*(atan2(2.*B,A-C));
      !     From matlab script: A2=A.*cos(t0).^2+2.*B.*sin(t0).*cos(t0)+A.*cos(t0).^2+C.*sin(t0)^2;
      !     From matlab script: C2=C.*cos(t0)^2-2.*B.*sin(t0).*cos(t0)+A.*sin(t0).^2;
      MSSD(JSEA)=0.5*(ATAN2(2*ETXY(JSEA),ETXX(JSEA)-ETYY(JSEA)))
      MSSX(JSEA)  = ETXX(JSEA)*COS(MSSD(JSEA))**2   &
           +2*ETXY(JSEA)*SIN(MSSD(JSEA))*COS(MSSD(JSEA))+ETYY(JSEA)*SIN(MSSD(JSEA))**2
      MSSY(JSEA)  = ETYY(JSEA)*COS(MSSD(JSEA))**2   &
           -2*ETXY(JSEA)*SIN(MSSD(JSEA))*COS(MSSD(JSEA))+ETXX(JSEA)*SIN(MSSD(JSEA))**2
      !
      ! 3.b Add tail
      !     ( DTH * SIG absorbed in FTxx )

      EBAND     = AB(JSEA) / CG(NK,ISEA)
      ET (JSEA) = ET (JSEA) + FTE  * EBAND
      EWN(JSEA) = EWN(JSEA) + FTWL * EBAND
      ETF(JSEA) = ETF(JSEA) + GRAV * FTTR * EBAND  ! this is the integral of CgE in deep water
      ETR(JSEA) = ETR(JSEA) + FTTR * EBAND
      ET1(JSEA) = ET1(JSEA) + FT1  * EBAND
      EET1(JSEA)= ET1(JSEA) + FT1  * EBAND**2
      ET02(JSEA)= ET02(JSEA)+ EBAND* 0.5 * SIG(NK)**4 * DTH
      ETX(JSEA) = ETX(JSEA) + FTE * ABX(JSEA) / CG(NK,ISEA)
      ETY(JSEA) = ETY(JSEA) + FTE * ABY(JSEA) / CG(NK,ISEA)
      SXX(JSEA) = SXX(JSEA) + FTE * ABXX(JSEA) / CG(NK,ISEA)
      SYY(JSEA) = SYY(JSEA) + FTE * ABYY(JSEA) / CG(NK,ISEA)
      SXY(JSEA) = SXY(JSEA) + FTE * ABXY(JSEA) / CG(NK,ISEA)
      !
      ! Tail for surface stokes drift is commented out: very sensitive to tail power
      !
      !       USSX(JSEA)  = USSX(JSEA) + 2*GRAV*ETUSCX(JSEA)/SIG(NK)
      !       USSY(JSEA)  = USSY(JSEA) + 2*GRAV*ETUSCY(JSEA)/SIG(NK)
      UBS(JSEA) = UBS(JSEA) + FTWL * EBAND/GRAV
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    SXX    = SXX * DWAT * GRAV
    SYY    = SYY * DWAT * GRAV
    SXY    = SXY * DWAT * GRAV
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,IX,IY)
#endif
    !
    DO JSEA=1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      IX     = MAPSF(ISEA,1)
      IY     = MAPSF(ISEA,2)
      IF ( MAPSTA(IY,IX) .GT. 0 ) THEN
#ifdef W3_O9
        IF ( ET(JSEA) .GE. 0. ) THEN
#endif
          HS (JSEA) = 4. * SQRT ( ET(JSEA) )
#ifdef W3_O9
        ELSE
          HS (JSEA) = - 4. * SQRT ( -ET(JSEA) )
        END IF
#endif
        IF ( ET(JSEA) .GT. 1.E-7 ) THEN
          QP(JSEA) = ( 2. / ET(JSEA)**2 ) * EET1(JSEA) * TPIINV**2
          WLM(JSEA) = EWN(JSEA) / ET(JSEA) * TPI
          T0M1(JSEA) = ETR(JSEA) / ET(JSEA) * TPI
          THS(JSEA) = RADE * SQRT ( MAX ( 0. , 2. * ( 1. - SQRT ( &
               MAX(0.,(ETX(JSEA)**2+ETY(JSEA)**2)/ET(JSEA)**2) ) ) ) )
          IF ( THS(JSEA) .LT. 0.01*RADE*DTH ) THS(JSEA) = 0.
        ELSE
          WLM(JSEA) = 0.
          T0M1(JSEA) = TPI / SIG(NK)
          THS(JSEA) = 0.
        END IF
        IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 ) THEN
          THM(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
        ELSE
          THM(JSEA) = 0.
        END IF
        ABR(JSEA) = SQRT ( 2. * MAX ( 0. , ABR(JSEA) ) )
        IF ( ABR(JSEA) .GE. 1.E-7 ) THEN
          ABD(JSEA) = ATAN2(ABD(JSEA),ABA(JSEA))
        ELSE
          ABD(JSEA) = 0.
        ENDIF
        ABA(JSEA) = ABR(JSEA)
        UBR(JSEA) = SQRT ( 2. * MAX ( 0. , UBR(JSEA) ) )
        IF ( UBR(JSEA) .GE. 1.E-7 ) THEN
          UBD(JSEA) = ATAN2(UBD(JSEA),UBA(JSEA))
        ELSE
          UBD(JSEA) = 0.
        ENDIF
        UBA(JSEA) = UBR(JSEA)
        CGE(JSEA) = DWAT*GRAV*ETF(JSEA)
        IF ( ET02(JSEA) .GT. 1.E-7  .AND.  ET(JSEA) .GT. 0 ) THEN
          T02(JSEA) = TPI * SQRT(ET(JSEA) / ET02(JSEA) )
          T01(JSEA) = TPI * ET(JSEA) / ET1(JSEA)
        ELSE
          T02(JSEA) = TPI / SIG(NK)
          T01(JSEA)= T02(JSEA)
        ENDIF
        !
        !  Add here USERO(JSEA,1) ...
        !
      END IF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 3.b Clean-up small values if !/O8 switch selected
    !
#ifdef W3_O8
    DO JSEA=1, NSEAL
      IF ( HS(JSEA).LE.HSMIN .AND. HS(JSEA).NE.UNDEF) THEN
        WLM(JSEA) = UNDEF
        T02(JSEA) = UNDEF
        T0M1(JSEA) = UNDEF
        THM(JSEA) = UNDEF
        THS(JSEA) = UNDEF
      END IF
    END DO
#endif
    !
    ! 4.  Peak frequencies and directions -------------------------------- *
    ! 4.a Initialize
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      EC  (JSEA) = EBD(NK,JSEA)
      FP0 (JSEA) = UNDEF
      IKP0(JSEA) = NK
      THP0(JSEA) = UNDEF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 4.b Discrete peak frequencies
    !
    DO IK=NK-1, 1, -1
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
      !
      DO JSEA=1, NSEAL
        IF ( EC(JSEA) .LT. EBD(IK,JSEA) ) THEN
          EC  (JSEA) = EBD(IK,JSEA)
          IKP0(JSEA) = IK
        END IF
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !
    END DO
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      IF ( IKP0(JSEA) .NE. NK ) FP0(JSEA) = SIG(IKP0(JSEA)) * TPIINV
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 4.c Continuous peak frequencies
    !
    XL     = 1./XFR - 1.
    XH     =  XFR - 1.
    XL2    = XL**2
    XH2    = XH**2
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA,EL,EH,DENOM)
#endif
    !
    DO JSEA=1, NSEAL
      IF ( IKP0(JSEA) .NE. NK ) THEN
        IF ( IKP0(JSEA) .EQ. 1 ) THEN
          EL = - EBD(IKP0(JSEA), JSEA)
        ELSE
          EL = EBD(IKP0(JSEA)-1, JSEA) - EBD(IKP0(JSEA), JSEA)
        END IF

        EH = EBD(IKP0(JSEA)+1, JSEA) - EBD(IKP0(JSEA), JSEA)

        DENOM  = XL*EH - XH*EL
        FP0(JSEA) = FP0 (JSEA) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )   &
             / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
      END IF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 4.d Peak directions
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      ETX(JSEA) = 0.
      ETY(JSEA) = 0.
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    DO ITH=1, NTH
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
      !
      DO JSEA=1, NSEAL
        IF ( IKP0(JSEA) .NE. NK ) THEN
          ETX(JSEA) = ETX(JSEA) + A(ITH,IKP0(JSEA),JSEA)*ECOS(ITH)
          ETY(JSEA) = ETY(JSEA) + A(ITH,IKP0(JSEA),JSEA)*ESIN(ITH)
        END IF
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !
    END DO
    !
#ifdef W3_OMPG
    !$OMP PARALLEL DO PRIVATE(JSEA)
#endif
    !
    DO JSEA=1, NSEAL
      IF ( ABS(ETX(JSEA))+ABS(ETY(JSEA)) .GT. 1.E-7 .AND.           &
           FP0(JSEA).NE.UNDEF )                                     &
           THP0(JSEA) = ATAN2(ETY(JSEA),ETX(JSEA))
      ETX(JSEA) = 0.
      ETY(JSEA) = 0.
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
    !$OMP PARALLEL DO PRIVATE(JSEA,ISEA,IX,IY)
#endif
    !
    DO JSEA =1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      IX          = MAPSF(ISEA,1)
      IY          = MAPSF(ISEA,2)
      IF ( MAPSTA(IY,IX) .LE. 0 ) THEN
        FP0 (JSEA) = UNDEF
        THP0(JSEA) = UNDEF
      END IF
    END DO
    !
#ifdef W3_OMPG
    !$OMP END PARALLEL DO
#endif
    !
    ! 5.  Test output (local to MPP only)
    !
#ifdef W3_T
    WRITE (NDST,9050)
    DO JSEA =1, NSEAL
      CALL INIT_GET_ISEA(ISEA, JSEA)
      IX     = MAPSF(ISEA,1)
      IY     = MAPSF(ISEA,2)
      IF ( HS(JSEA) .EQ. UNDEF ) THEN
        WRITE (NDST,9051) ISEA, IX, IY
      ELSE IF ( WLM(JSEA) .EQ. UNDEF ) THEN
        WRITE (NDST,9052) ISEA, IX, IY, HS(JSEA)
      ELSE IF ( FP0(JSEA) .EQ. UNDEF ) THEN
        WRITE (NDST,9053) ISEA, IX, IY, HS(JSEA), WLM(JSEA),   &
             T0M1(JSEA), RADE*THM(JSEA), THS(JSEA)
      ELSE
        WRITE (NDST,9054) ISEA, IX, IY, HS(JSEA), WLM(JSEA),   &
             T0M1(JSEA), RADE*THM(JSEA), THS(JSEA), FP0(JSEA),&
             THP0(JSEA)
      END IF
    END DO
#endif
    !
    ! 6.  Fill arrays wth partitioned data
    !
    IF ( FLPART ) THEN
      !
      ! 6.a Initializations
      !
      PHS    = UNDEF
      PTP    = UNDEF
      PLP    = UNDEF
      PDIR   = UNDEF
      PSI    = UNDEF
      PWS    = UNDEF
      PWST   = UNDEF
      PNR    = UNDEF
      PTHP0  = UNDEF
      PQP    = UNDEF
      PPE    = UNDEF
      PGW    = UNDEF
      PSW    = UNDEF
      PTM1   = UNDEF
      PT1    = UNDEF
      PT2    = UNDEF
      PEP    = UNDEF
      !
      ! 6.b Loop over local sea points
      !
#ifdef W3_OMPG
      !$OMP PARALLEL DO PRIVATE(ISEA,JSEA,IX,IY,I,J)
#endif
      !
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IX          = MAPSF(ISEA,1)
        IY          = MAPSF(ISEA,2)
        !
        IF ( MAPSTA(IY,IX).GT.0 ) THEN
          I         = ICPRT(JSEA,2)
          PNR(JSEA) = MAX ( 0. , REAL(ICPRT(JSEA,1)-1) )
          IF ( ICPRT(JSEA,1).GE.1 ) PWST(JSEA) = DTPRT(6,I)
        END IF
        !
        IF ( MAPSTA(IY,IX).GT.0 .AND. ICPRT(JSEA,1).GT.1 ) THEN
          I      = ICPRT(JSEA,2) + 1
          IF ( DTPRT(6,I) .GE. WSCUT ) THEN
            PHS(JSEA,0) = DTPRT(1,I)
            PTP(JSEA,0) = DTPRT(2,I)
            PLP(JSEA,0) = DTPRT(3,I)
            ! (PDIR is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(4,I) .NE. UNDEF) THEN
              PDIR(JSEA,0) = (270. - DTPRT(4,I)) * DERA
            ENDIF
            PSI(JSEA,0) = DTPRT(5,I)
            PWS(JSEA,0) = DTPRT(6,I)
            ! (PTHP0 is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(7,I) .NE. UNDEF) THEN
              PTHP0(JSEA,0) = (270. - DTPRT(7,I)) * DERA
            ENDIF
            PSW(JSEA,0) = DTPRT(8,I)
            PPE(JSEA,0) = DTPRT(9,I)
            PQP(JSEA,0) = DTPRT(10,I)
            PGW(JSEA,0) = DTPRT(11,I)
            PTM1(JSEA,0) = DTPRT(12,I)
            PT1(JSEA,0) = DTPRT(13,I)
            PT2(JSEA,0) = DTPRT(14,I)
            PEP(JSEA,0) = DTPRT(15,I)
            I      = I + 1
          END IF
          DO J=1, NOSWLL
            IF ( I .GT.  ICPRT(JSEA,2)+ICPRT(JSEA,1)-1 ) EXIT
            PHS(JSEA,J) = DTPRT(1,I)
            PTP(JSEA,J) = DTPRT(2,I)
            PLP(JSEA,J) = DTPRT(3,I)
            ! (PDIR is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(4,I) .NE. UNDEF) THEN
              PDIR(JSEA,J) = (270. - DTPRT(4,I)) * DERA
            ENDIF
            PSI(JSEA,J) = DTPRT(5,I)
            PWS(JSEA,J) = DTPRT(6,I)
            ! (PTHP0 is already in degrees nautical - convert back to
            !  Cartesian in radians to maintain internal convention)
            IF(DTPRT(7,I) .NE. UNDEF) THEN
              PTHP0(JSEA,J) = (270. - DTPRT(7,I)) * DERA
            ENDIF
            PSW(JSEA,J) = DTPRT(8,I)
            PPE(JSEA,J) = DTPRT(9,I)
            PQP(JSEA,J) = DTPRT(10,I)
            PGW(JSEA,J) = DTPRT(11,I)
            PTM1(JSEA,J) = DTPRT(12,I)
            PT1(JSEA,J) = DTPRT(13,I)
            PT2(JSEA,J) = DTPRT(14,I)
            PEP(JSEA,J) = DTPRT(15,I)
            I      = I + 1
          END DO
        END IF
        !
      END DO
      !
#ifdef W3_OMPG
      !$OMP END PARALLEL DO
#endif
      !

    END IF

    IF (FLOLOC( 6, 8)) THEN
      CALL CALC_U3STOKES(A,1)
    END IF
    !
    IF (FLOLOC( 6, 12)) THEN
      CALL CALC_U3STOKES(A,2)
    ENDIF
    !
    ! Dominant wave breaking probability
    !
    IF (FLOLOC(2, 17)) CALL CALC_WBT(A)
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9050 FORMAT (' TEST W3OUTG : ISEA, IX, IY, HS, L, Tm, THm, THs',     &
         ', FP0, THP0')
9051 FORMAT (2X,I8,2I8)
9052 FORMAT (2X,I8,2I8,F6.2)
9053 FORMAT (2X,I8,2I8,F6.2,F7.1,F6.2,2F6.1)
9054 FORMAT (2X,I8,2I8,F6.2,F7.1,F6.2,2F6.1,F6.3,F6.0)
#endif

    !/
    !/ End of W3OUTG ----------------------------------------------------- /
    !/
  END SUBROUTINE W3OUTG
