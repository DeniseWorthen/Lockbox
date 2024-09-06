  SUBROUTINE W3IOGO ( INXOUT, NDSOG, IOTST, IMOD )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    17-Mar-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
    !/    04-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    24-Jan-2001 : Flat grid version (formats only)    ( version 2.06 )
    !/    23-Apr-2002 : Clean up                            ( version 2.19 )
    !/    29-Apr-2002 : Add output types 17-18.             ( version 2.20 )
    !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
    !/    25-Oct-2004 : Multiple grid version.              ( version 3.06 )
    !/    27-Jun-2005 : Adding MAPST2.                      ( version 3.07 )
    !/    21-Jul-2005 : Adding output fields 19-21.         ( version 3.07 )
    !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
    !/    05-Jul-2006 : Consolidate stress arrays.          ( version 3.09 )
    !/    02-Apr-2007 : Adding partitioned output.          ( version 3.11 )
    !/                  Adding user slots for outputs.
    !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
    !/                  (W. E. Rogers & T. J. Campbell, NRL)
    !/    31-Oct-2010 : Implement unstructured grids        ( version 3.14 )
    !/                  (A. Roland and F. Ardhuin)
    !/    05-Feb-2011 : Renumbering of output fields        ( version 3.14 )
    !/                  (F. Ardhuin)
    !/    25-Dec-2012 : New output structure and smaller    ( version 4.11 )
    !/                  memory footprint.
    !/    21-Aug-2013 : Added missing cos,sin for UBA, ABA  ( version 4.11 )
    !/    27-Nov-2013 : Management of coupling output       ( version 4.18 )
    !/    01-Mar-2018 : Removed RTD code (now used in post  ( version 6.02 )
    !/                  processing code)
    !/    25-Aug-2018 : Add WBT parameter                   ( version 6.06 )
    !/    22-Mar-2021 : Add extra coupling fields as output ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Read/write gridded output.
    !
    !  2. Method :
    !
    !     Fields in file are determined by flags in FLOGRD in W3ODATMD.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       INXOUT  C*(*)  I   Test string for read/write, valid are:
    !                          'READ' and 'WRITE'.
    !       NDSOG   Int.   I   File unit number.
    !       IOTST   Int.   O   Test indictor for reading.
    !                           0 : Fields read.
    !                          -1 : Past end of file.
    !       IMOD    Int.   I   Model number for W3GDAT etc.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !       See module documentation above.
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3WAVE    Subr. W3WAVEMD Actual wave model routine.
    !      WW3_OUTF  Prog.   N/A    Ouput postprocessor.
    !      WW3_GRIB  Prog.   N/A    Ouput postprocessor.
    !      GX_OUTF   Prog.   N/A    Ouput postprocessor.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       Tests on INXOUT, file status and on array dimensions.
    !
    !  7. Remarks :
    !
    !     - MAPSTA is dumped as it contains information on the ice edge.
    !       Dynamic ice edges require MAPSTA to be dumped every time step.
    !     - The output file has the pre-defined name 'out_grd.FILEXT'.
    !     - The current components CX and CY are written to out_grd as
    !       components, but converted to magnitude and direction in most
    !       gridded and point output post-processors (except gx_outf).
    !     - All written direction are in degrees, nautical convention,
    !       but in reading, all is convered back to radians and cartesian
    !       conventions.
    !     - Before writing, wind and current directions are converted,
    !       wave directions are already in correct convention (see W3OUTG).
    !     - In MPP version of model data is supposed to be gatherd at the
    !       correct processor before the routine is called.
    !     - In MPP version routine is called by only one process, therefore
    !       no test on process for error messages is needed.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/ST1   First source term package (WAM3).
    !     !/ST2   Second source term package (TC96).
    !     !/S     Enable subroutine tracing.
    !     !/T     Test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE CONSTANTS
    USE W3GDATMD
    !/
    USE W3WDATMD, ONLY: W3SETW, W3DIMW
    USE W3ADATMD, ONLY: W3SETA, W3DIMA, W3XETA
    USE W3ODATMD, ONLY: W3SETO
    !/
    USE W3WDATMD, ONLY: TIME, DINIT, WLV, ICE, ICEF, ICEH, BERG,    &
         UST,  USTDIR, ASF, RHOAIR
    USE W3ADATMD, ONLY: AINIT, DW, UA, UD, AS, CX, CY, WN,          &
         TAUA, TAUADIR
    USE W3ADATMD, ONLY: HS, WLM, T02, T0M1, T01, FP0, THM, THS, THP0,&
         WBT, WNMEAN
    USE W3ADATMD, ONLY: DTDYN, FCUT, ABA, ABD, UBA, UBD, SXX, SYY, SXY,&
         PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR,    &
         PTHP0, PQP, PPE, PGW, PSW, PTM1, PT1, PT2,  &
         PEP, USERO, TAUOX, TAUOY, TAUWIX, TAUWIY,    &
         PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,        &
         USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,    &
         MSCD, QP, TAUWNX, TAUWNY, CHARN, TWS, BHD,   &
         PHIBBL, TAUBBL, WHITECAP, BEDFORMS, CGE, EF, &
         CFLXYMAX, CFLTHMAX, CFLKMAX, P2SMS, US3D,    &
         TH1M, STH1M, TH2M, STH2M, HSIG, PHICE, TAUICE,&
         STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD,&
         USSP, TAUOCX, TAUOCY
    USE W3ADATMD, ONLY: USSHX, USSHY
    !/
    USE W3ODATMD, ONLY: NOGRP, NGRPP, IDOUT, UNDEF, NDST, NDSE,     &
         FLOGRD, IPASS => IPASS1, WRITE => WRITE1,   &
         FNMPRE, NOSWLL, NOEXTR
    !/
    USE W3SERVMD, ONLY: EXTCDE
    USE W3ODATMD, only : IAPROC
    USE W3ODATMD, ONLY :  OFILES
#ifdef W3_SETUP
    USE W3WDATMD, ONLY: ZETA_SETUP
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(INOUT)        :: IOTST
    INTEGER, INTENT(IN)           :: NDSOG
    INTEGER, INTENT(IN), OPTIONAL :: IMOD
    CHARACTER, INTENT(IN)         :: INXOUT*(*)
    CHARACTER(LEN=15) :: TIMETAG
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IGRD, IERR, I, J, IX, IY, MOGRP,     &
         MGRPP, ISEA, MOSWLL, IK, IFI, IFJ    &
         ,IFILOUT
    INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: AUX1(NSEA), AUX2(NSEA),              &
         AUX3(NSEA), AUX4(NSEA)
#ifdef W3_SMC
    REAL                    :: UDARC
#endif
    CHARACTER(LEN=30)       :: IDTST, TNAME
    CHARACTER(LEN=10)       :: VERTST
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3IOGO')
#endif
    !
    ! test input parameters ---------------------------------------------- *
    !
    IF ( PRESENT(IMOD) ) THEN
      IGRD   = IMOD
    ELSE
      IGRD   = 1
    END IF
    !
    CALL W3SETO ( IGRD, NDSE, NDST )
    CALL W3SETG ( IGRD, NDSE, NDST )
    CALL W3SETA ( IGRD, NDSE, NDST )
#ifdef W3_MPI
    CALL W3XETA ( IGRD, NDSE, NDST )
#endif
    CALL W3SETW ( IGRD, NDSE, NDST )
    !
    IPASS  = IPASS + 1
    IOTST  = 0
    !
    IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE' ) THEN
      WRITE (NDSE,900) INXOUT
      CALL EXTCDE ( 1 )
    END IF
    !
    IF ( IPASS.EQ.1 .AND. OFILES(1) .EQ. 0) THEN
      WRITE  = INXOUT.EQ.'WRITE'
    ELSE
      IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
        WRITE (NDSE,901) INXOUT
        CALL EXTCDE ( 2 )
      END IF
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9000) IPASS, INXOUT, WRITE, NDSOG, IGRD, FILEXT
#endif
    !
    !
    ! open file ---------------------------------------------------------- *
    ! ( IPASS = 1 )
    !
    IF ( IPASS.EQ.1 .AND. OFILES(1) .EQ. 0) THEN
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
      !
#ifdef W3_T
      WRITE (NDST,9001) FNMPRE(:J)//'out_grd.'//FILEXT(:I)
#endif
      IF ( WRITE ) THEN
        OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
             form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR)
      ELSE
        OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
             form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR,STATUS='OLD')
      END IF
      !
      REWIND ( NDSOG )
      !
      ! test info --------------------------------------------------------- *
      ! ( IPASS = 1 )
      !
      IF ( WRITE ) THEN
        WRITE (NDSOG)                                           &
             IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL
      ELSE
        READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                &
             IDTST, VERTST, TNAME, MOGRP, MGRPP, NSEA, NX, NY,     &
             UNDEF, MOSWLL
        !
        IF ( IDTST .NE. IDSTR ) THEN
          WRITE (NDSE,902) IDTST, IDSTR
          CALL EXTCDE ( 20 )
        END IF
        IF ( VERTST .NE. VEROGR ) THEN
          WRITE (NDSE,903) VERTST, VEROGR
          CALL EXTCDE ( 21 )
        END IF
        IF ( NOGRP .NE. MOGRP .OR. NGRPP .NE. MGRPP ) THEN
          WRITE (NDSE,904) MOGRP, MGRPP, NOGRP, NGRPP
          CALL EXTCDE ( 22 )
        END IF
        IF ( TNAME .NE. GNAME ) THEN
          WRITE (NDSE,905) TNAME, GNAME
        END IF
        IF ( NOSWLL .NE. MOSWLL ) THEN
          WRITE (NDSE,906) MOSWLL, NOSWLL
          CALL EXTCDE ( 24 )
        END IF
        !
      END IF
      !
#ifdef W3_T
      WRITE (NDST,9002) IDSTR, VEROGR, GNAME, NSEA, NX, NY,    &
           UNDEF
#endif
      !
    END IF
    !
    !  IN CASE OF GENERATION OF A NEW FILE OUTPUT EVERY DELTA OUTPUT
    ! open file ---------------------------------------------------------- *
    ! ( IPASS = 1 )
    !
    IF ( IPASS.GE.1 .AND. OFILES(1) .EQ. 1) THEN
      WRITE  = INXOUT.EQ.'WRITE'
    ELSE
      IF ( WRITE .AND. INXOUT.EQ.'READ' ) THEN
        WRITE (NDSE,901) INXOUT
        CALL EXTCDE ( 2 )
      END IF
    END IF
    !
    IF ( IPASS.GE.1 .AND. OFILES(1) .EQ. 1) THEN
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
      !
      ! Create TIMETAG for file name using YYYYMMDD.HHMMS prefix
      WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
#ifdef W3_T
      WRITE (NDST,9001) FNMPRE(:J)//TIMETAG//'.out_grd.'//FILEXT(:I)
#endif
      IF ( WRITE ) THEN
        OPEN (NDSOG,FILE=FNMPRE(:J)//TIMETAG//'.out_grd.'  &
             //FILEXT(:I),form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR)
      ELSE
        OPEN (NDSOG,FILE=FNMPRE(:J)//'out_grd.'//FILEXT(:I),    &
             form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR,STATUS='OLD')
      END IF
      !
      REWIND ( NDSOG )
      !
      ! test info --------------------------------------------------------- *
      ! ( IPASS >= 1 & OFILES(1) = 1)
      !
      IF ( WRITE ) THEN
        WRITE (NDSOG)                                           &
             IDSTR, VEROGR, GNAME, NOGRP, NGRPP, NSEA, NX, NY,     &
             UNDEF, NOSWLL
      ELSE
        READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                &
             IDTST, VERTST, TNAME, MOGRP, MGRPP, NSEA, NX, NY,     &
             UNDEF, MOSWLL
        !
        IF ( IDTST .NE. IDSTR ) THEN
          WRITE (NDSE,902) IDTST, IDSTR
          CALL EXTCDE ( 20 )
        END IF
        IF ( VERTST .NE. VEROGR ) THEN
          WRITE (NDSE,903) VERTST, VEROGR
          CALL EXTCDE ( 21 )
        END IF
        IF ( NOGRP .NE. MOGRP .OR. NGRPP .NE. MGRPP ) THEN
          WRITE (NDSE,904) MOGRP, MGRPP, NOGRP, NGRPP
          CALL EXTCDE ( 22 )
        END IF
        IF ( TNAME .NE. GNAME ) THEN
          WRITE (NDSE,905) TNAME, GNAME
        END IF
        IF ( NOSWLL .NE. MOSWLL ) THEN
          WRITE (NDSE,906) MOSWLL, NOSWLL
          CALL EXTCDE ( 24 )
        END IF
        !
      END IF
      !
#ifdef W3_T
      WRITE (NDST,9002) IDSTR, VEROGR, GNAME, NSEA, NX, NY,    &
           UNDEF
#endif
      !
    END IF
    !
    ! TIME and flags ----------------------------------------------------- *
    !
    IF ( WRITE ) THEN
      WRITE (NDSOG)                            TIME, FLOGRD
    ELSE
      READ (NDSOG,END=803,ERR=802,IOSTAT=IERR) TIME, FLOGRD
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9003) TIME, FLOGRD
#endif
    !
    ! MAPSTA ------------------------------------------------------------- *
    !
    ALLOCATE ( MAPTMP(NY,NX) )
    IF ( WRITE ) THEN
      MAPTMP = MAPSTA + 8*MAPST2
      WRITE (NDSOG)                                               &
           ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
    ELSE
      READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)                    &
           ((MAPTMP(IY,IX),IX=1,NX),IY=1,NY)
      MAPSTA = MOD(MAPTMP+2,8) - 2
      MAPST2 = (MAPTMP-MAPSTA) / 8
    END IF
    DEALLOCATE ( MAPTMP )
    !
    ! Fields ---------------------------------------------- *
    !
    ! Initialization ---------------------------------------------- *
    !
    IF ( WRITE ) THEN
      DO ISEA=1, NSEA
        IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .LT. 0 ) THEN
          !
          IF ( FLOGRD( 2, 2) ) WLM   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 3) ) T02   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 4) ) T0M1  (ISEA) = UNDEF
          IF ( FLOGRD( 2, 5) ) T01   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 6) .OR. FLOGRD( 2,18) )                 &
               FP0   (ISEA) = UNDEF  ! FP or TP
          IF ( FLOGRD( 2, 7) ) THM   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 8) ) THS   (ISEA) = UNDEF
          IF ( FLOGRD( 2, 9) ) THP0  (ISEA) = UNDEF
          UST   (ISEA) = UNDEF
          USTDIR(ISEA) = UNDEF
          IF ( FLOGRD( 2,10) ) HSIG  (ISEA) = UNDEF
          IF ( FLOGRD( 2,11) ) STMAXE(ISEA) = UNDEF
          IF ( FLOGRD( 2,12) ) STMAXD(ISEA) = UNDEF
          IF ( FLOGRD( 2,13) ) HMAXE (ISEA) = UNDEF
          IF ( FLOGRD( 2,14) ) HCMAXE(ISEA) = UNDEF
          IF ( FLOGRD( 2,15) ) HMAXD (ISEA) = UNDEF
          IF ( FLOGRD( 2,16) ) HCMAXD(ISEA) = UNDEF
          IF ( FLOGRD( 2,17) ) WBT   (ISEA) = UNDEF
          IF ( FLOGRD( 2,19) ) WNMEAN(ISEA) = UNDEF
          !
          IF ( FLOGRD( 3, 1) ) EF   (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 2) ) TH1M (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 3) ) STH1M(ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 4) ) TH2M (ISEA,:) = UNDEF
          IF ( FLOGRD( 3, 5) ) STH2M(ISEA,:) = UNDEF
          !
          IF ( FLOGRD( 4, 1) ) PHS (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 2) ) PTP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 3) ) PLP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 4) ) PDIR (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 5) ) PSI (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 6) ) PWS (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 7) ) PTHP0(ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 8) ) PQP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4, 9) ) PPE(ISEA,:)  = UNDEF
          IF ( FLOGRD( 4,10) ) PGW(ISEA,:)  = UNDEF
          IF ( FLOGRD( 4,11) ) PSW (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,12) ) PTM1(ISEA,:) = UNDEF
          IF ( FLOGRD( 4,13) ) PT1 (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,14) ) PT2 (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,15) ) PEP (ISEA,:) = UNDEF
          IF ( FLOGRD( 4,16) ) PWST(ISEA  ) = UNDEF
          IF ( FLOGRD( 4,17) ) PNR (ISEA  ) = UNDEF
          !
          IF ( FLOGRD( 5, 2) ) CHARN (ISEA) = UNDEF
          IF ( FLOGRD( 5, 3) ) CGE   (ISEA) = UNDEF
          IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
          IF ( FLOGRD( 5, 5) ) THEN
            TAUWIX(ISEA) = UNDEF
            TAUWIY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 6) ) THEN
            TAUWNX(ISEA) = UNDEF
            TAUWNY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
          IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
          IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
          IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
          !
          IF ( FLOGRD( 6, 1) ) THEN
            SXX   (ISEA) = UNDEF
            SYY   (ISEA) = UNDEF
            SXY   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 2) ) THEN
            TAUOX (ISEA) = UNDEF
            TAUOY (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 3) ) BHD(ISEA) = UNDEF
          IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
          IF ( FLOGRD( 6, 5) ) THEN
            TUSX  (ISEA) = UNDEF
            TUSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 6) ) THEN
            USSX  (ISEA) = UNDEF
            USSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 7) ) THEN
            PRMS  (ISEA) = UNDEF
            TPMS  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 8) ) US3D(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 9) ) P2SMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 10) ) TAUICE(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 11) ) PHICE(ISEA) = UNDEF
          IF ( FLOGRD( 6, 12) ) USSP(ISEA,:) = UNDEF
          IF ( FLOGRD( 6, 13) ) THEN
            TAUOCX(ISEA) = UNDEF
            TAUOCY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 14) ) THEN
            USSHX (ISEA) = UNDEF
            USSHY (ISEA) = UNDEF
          END IF
          !
          IF ( FLOGRD( 7, 1) ) THEN
            ABA   (ISEA) = UNDEF
            ABD   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 7, 2) ) THEN
            UBA   (ISEA) = UNDEF
            UBD   (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
          IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
          !
          IF ( FLOGRD( 8, 1) ) THEN
            MSSX  (ISEA) = UNDEF
            MSSY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 8, 2) ) THEN
            MSCX  (ISEA) = UNDEF
            MSCY  (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 8, 3) ) MSSD (ISEA) = UNDEF
          IF ( FLOGRD( 8, 4) ) MSCD (ISEA) = UNDEF
          IF ( FLOGRD( 8, 5) ) QP   (ISEA) = UNDEF
          !
          IF ( FLOGRD( 9, 1) ) DTDYN (ISEA) = UNDEF
          IF ( FLOGRD( 9, 2) ) FCUT  (ISEA) = UNDEF
          IF ( FLOGRD( 9, 3) ) CFLXYMAX(ISEA) = UNDEF
          IF ( FLOGRD( 9, 4) ) CFLTHMAX(ISEA) = UNDEF
          IF ( FLOGRD( 9, 5) ) CFLKMAX(ISEA) = UNDEF
          !
        END IF
        !
        IF ( MAPSTA(MAPSF(ISEA,2),MAPSF(ISEA,1)) .EQ. 2 ) THEN
          !
          IF ( FLOGRD( 5, 4) ) PHIAW (ISEA) = UNDEF
          IF ( FLOGRD( 5, 5) ) THEN
            TAUWIX(ISEA) = UNDEF
            TAUWIY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 6) ) THEN
            TAUWNX(ISEA) = UNDEF
            TAUWNY(ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 5, 7) ) WHITECAP(ISEA,1) = UNDEF
          IF ( FLOGRD( 5, 8) ) WHITECAP(ISEA,2) = UNDEF
          IF ( FLOGRD( 5, 9) ) WHITECAP(ISEA,3) = UNDEF
          IF ( FLOGRD( 5,10) ) WHITECAP(ISEA,4) = UNDEF
          !
          IF ( FLOGRD( 6, 2) )THEN
            TAUOX (ISEA) = UNDEF
            TAUOY (ISEA) = UNDEF
          END IF
          IF ( FLOGRD( 6, 4) ) PHIOC (ISEA) = UNDEF
          !
          IF ( FLOGRD( 7, 3) ) BEDFORMS(ISEA,:) = UNDEF
          IF ( FLOGRD( 7, 4) ) PHIBBL(ISEA) = UNDEF
          IF ( FLOGRD( 7, 5) ) TAUBBL(ISEA,:) = UNDEF
          !
        END IF
        !
      END DO
      !
    ELSE
      IF (.NOT.DINIT) CALL W3DIMW ( IGRD, NDSE, NDST, .TRUE. )
      IF (.NOT.AINIT) CALL W3DIMA ( IGRD, NDSE, NDST, .TRUE. )
    END IF
    !
    ! Actual output  ---------------------------------------------- *
    DO IFI=1, NOGRP
      DO IFJ=1, NGRPP

        IF ( FLOGRD(IFI,IFJ) ) THEN
          !
#ifdef W3_T
          WRITE (NDST,9010) FLOGRD(IFI,IFJ), IDOUT(IFI,IFJ)
#endif
          !
          IF ( WRITE ) THEN
            !
            !     Section 1)
            !
            IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) DW(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) CX(1:NSEA)
              WRITE ( NDSOG ) CY(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
              DO ISEA=1, NSEA
#ifdef W3_SMC
                !!Li  Rotate map-east wind in Arctic part back to local east.  JGLi02Feb2016
                IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
                  UDARC = UD(ISEA) - ANGARC(ISEA - NGLO)*DERA
                  UD(ISEA) = MOD(TPI + UDARC, TPI)
                ENDIF
#endif
                IF (UA(ISEA) .NE.UNDEF) THEN
                  AUX1(ISEA) = UA(ISEA)*COS(UD(ISEA))
                  AUX2(ISEA) = UA(ISEA)*SIN(UD(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
              WRITE ( NDSOG ) AUX2
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) AS(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) WLV(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) ICE(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) BERG(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 8 ) THEN
              DO ISEA=1, NSEA
#ifdef W3_SMC
                !!Li  Rotate map-east momentum in Arctic part back to local east.  JGLi02Feb2016
                IF( ARCTC .AND. (ISEA .GT. NGLO) ) THEN
                  UDARC = TAUADIR(ISEA) - ANGARC(ISEA - NGLO)*DERA
                  TAUADIR(ISEA) = MOD(TPI + UDARC, TPI)
                ENDIF
#endif
                IF (TAUA(ISEA) .NE.UNDEF) THEN
                  AUX1(ISEA) = TAUA(ISEA)*COS(TAUADIR(ISEA))
                  AUX2(ISEA) = TAUA(ISEA)*SIN(TAUADIR(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
              WRITE ( NDSOG ) AUX2
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) RHOAIR(1:NSEA)
#ifdef W3_BT4
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) SED_D50(1:NSEA)
#endif
#ifdef W3_IS2
            ELSE IF (IFI .EQ. 1 .AND. IFJ .EQ. 11 ) THEN
              WRITE (NDSOG ) ICEH(1:NSEA)
            ELSE IF (IFI .EQ. 1 .AND. IFJ .EQ. 12 ) THEN
              WRITE (NDSOG ) ICEF(1:NSEA)
#endif
#ifdef W3_SETUP
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) ZETA_SETUP(1:NSEA)
#endif

              !
              !     Section 2)
              !
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) HS(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) WLM(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) T02(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) T0M1(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) T01(1:NSEA)
            ELSE IF ( (IFI .EQ. 2 .AND. IFJ .EQ. 6) .OR.         &
                 (IFI .EQ. 2 .AND. IFJ .EQ. 18) ) THEN
              ! Note: TP output is derived from FP field.
              WRITE ( NDSOG ) FP0(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) THM(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) THS(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) THP0(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) HSIG(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) STMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
              WRITE ( NDSOG ) STMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) HMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
              WRITE ( NDSOG ) HCMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
              WRITE ( NDSOG ) HMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
              WRITE ( NDSOG ) HCMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
              WRITE ( NDSOG ) WBT(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 19 ) THEN
              WRITE ( NDSOG ) WNMEAN(1:NSEA)
              !
              !     Section 3)
              !
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) EF(1:NSEA,E3DF(2,1):E3DF(3,1))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6) THEN
              WRITE ( NDSOG ) WN(1:NK,1:NSEA)
              !
              !     Section 4)
              !
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) PHS(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) PTP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) PLP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PDIR(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) PSI(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) PWS(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) PTHP0(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8  ) THEN
              WRITE ( NDSOG ) PQP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9  ) THEN
              WRITE ( NDSOG ) PPE(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) PGW(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) PSW(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
              WRITE ( NDSOG ) PTM1(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) PT1(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
              WRITE ( NDSOG ) PT2(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
              WRITE ( NDSOG ) PEP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
              WRITE ( NDSOG ) PWST(1:NSEA)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
              WRITE ( NDSOG ) PNR(1:NSEA)
              !
              !     Section 5)
              !
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
              DO ISEA=1, NSEA
                IX     = MAPSF(ISEA,1)
                IY     = MAPSF(ISEA,2)
                IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
                  AUX1(ISEA) = UST(ISEA) * ASF(ISEA) *        &
                       COS(USTDIR(ISEA))
                  AUX2(ISEA) = UST(ISEA) * ASF(ISEA) *        &
                       SIN(USTDIR(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
              WRITE ( NDSOG ) AUX2
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) CHARN(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) CGE(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PHIAW(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) TAUWIX(1:NSEA)
              WRITE ( NDSOG ) TAUWIY(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) TAUWNX(1:NSEA)
              WRITE ( NDSOG ) TAUWNY(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,1)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,2)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,3)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) WHITECAP(1:NSEA,4)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) TWS(1:NSEA)
              !
              !     Section 6)
              !
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) SXX(1:NSEA)
              WRITE ( NDSOG ) SYY(1:NSEA)
              WRITE ( NDSOG ) SXY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) TAUOX(1:NSEA)
              WRITE ( NDSOG ) TAUOY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3  ) THEN
              WRITE ( NDSOG ) BHD(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PHIOC(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) TUSX(1:NSEA)
              WRITE ( NDSOG ) TUSY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
              WRITE ( NDSOG ) USSX(1:NSEA)
              WRITE ( NDSOG ) USSY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
              WRITE ( NDSOG ) PRMS(1:NSEA)
              WRITE ( NDSOG ) TPMS(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
              WRITE ( NDSOG ) US3D(1:NSEA,   US3DF(2):US3DF(3))
              WRITE ( NDSOG ) US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
              WRITE ( NDSOG ) P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
              WRITE ( NDSOG ) TAUICE(1:NSEA,1)
              WRITE ( NDSOG ) TAUICE(1:NSEA,2)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
              WRITE ( NDSOG ) PHICE(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
              WRITE ( NDSOG ) USSP(1:NSEA,   1:USSPF(2))
              WRITE ( NDSOG ) USSP(1:NSEA,NK+1:NK+USSPF(2))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 13 ) THEN
              WRITE ( NDSOG ) TAUOCX(1:NSEA)
              WRITE ( NDSOG ) TAUOCY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 14 ) THEN
              WRITE ( NDSOG ) USSHX(1:NSEA)
              WRITE ( NDSOG ) USSHY(1:NSEA)
              !
              !     Section 7)
              !
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
              DO ISEA=1, NSEA
                IF ( ABA(ISEA) .NE. UNDEF ) THEN
                  AUX1(ISEA) = ABA(ISEA)*COS(ABD(ISEA))
                  AUX2(ISEA) = ABA(ISEA)*SIN(ABD(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
              WRITE ( NDSOG ) AUX2
              !WRITE ( NDSOG ) ABA(1:NSEA)
              !WRITE ( NDSOG ) ABD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
              DO ISEA=1, NSEA
                IF ( UBA(ISEA) .NE. UNDEF ) THEN
                  AUX1(ISEA) = UBA(ISEA)*COS(UBD(ISEA))
                  AUX2(ISEA) = UBA(ISEA)*SIN(UBD(ISEA))
                ELSE
                  AUX1(ISEA) = UNDEF
                  AUX2(ISEA) = UNDEF
                END IF
              END DO
              WRITE ( NDSOG ) AUX1
              WRITE ( NDSOG ) AUX2
              !                    WRITE ( NDSOG ) UBA(1:NSEA)
              !                    WRITE ( NDSOG ) UBD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) BEDFORMS(1:NSEA,1)
              WRITE ( NDSOG ) BEDFORMS(1:NSEA,2)
              WRITE ( NDSOG ) BEDFORMS(1:NSEA,3)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) PHIBBL(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) TAUBBL(1:NSEA,1)
              WRITE ( NDSOG ) TAUBBL(1:NSEA,2)
              !
              !     Section 8)
              !
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) MSSX(1:NSEA)
              WRITE ( NDSOG ) MSSY(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) MSCX(1:NSEA)
              WRITE ( NDSOG ) MSCY(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) MSSD(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) MSCD(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) QP(1:NSEA)
              !
              !     Section 9)
              !
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
              WRITE ( NDSOG ) DTDYN(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
              WRITE ( NDSOG ) FCUT(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
              WRITE ( NDSOG ) CFLXYMAX(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
              WRITE ( NDSOG ) CFLTHMAX(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
              WRITE ( NDSOG ) CFLKMAX(1:NSEA)
              !
              !     Section 10)
              !
            ELSE IF ( IFI .EQ. 10 ) THEN
              WRITE ( NDSOG ) USERO(1:NSEA,IFJ)
              !
            END IF
            !
          ELSE
            !
            !     Start of reading ......
            !
            !     Section 1)
            !
            IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) DW(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CY(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UD(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) AS(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WLV(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICE(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) BERG(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) TAUA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) TAUADIR(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) RHOAIR(1:NSEA)
#ifdef W3_BT4
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SED_D50(1:NSEA)
#endif
#ifdef W3_IS2
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICEH(1:NSEA)
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ICEF(1:NSEA)
#endif
#ifdef W3_SETUP
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ZETA_SETUP(1:NSEA)
#endif
              !
              !     Section 2)
              !
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) HS(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WLM(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T02(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T0M1(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) T01(1:NSEA)
            ELSE IF ( (IFI .EQ. 2 .AND. IFJ .EQ. 6) .OR.       &
                 (IFI .EQ. 2 .AND. IFJ .EQ. 18) ) THEN
              ! Note: TP output is derived from FP field.
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) FP0(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) THM(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) THS(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   THP0(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HSIG(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HCMAXE(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   HCMAXD(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) WBT(1:NSEA)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 19 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WNMEAN(1:NSEA)
              !
              !     Section 3)
              !
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   EF(1:NSEA,E3DF(2,1):E3DF(3,1))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TH1M(1:NSEA,E3DF(2,2):E3DF(3,2))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STH1M(1:NSEA,E3DF(2,3):E3DF(3,3))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TH2M(1:NSEA,E3DF(2,4):E3DF(3,4))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   STH2M(1:NSEA,E3DF(2,5):E3DF(3,5))
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   WN(1:NK,1:NSEA)
              !
              !     Section 4)
              !
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHS(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PTP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PLP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PDIR(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PSI(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PWS(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PTHP0(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8  ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PQP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9  ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PPE(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PGW(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PSW(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PTM1(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PT1(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PT2(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PEP(1:NSEA,0:NOSWLL)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PWST(1:NSEA)
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) PNR(1:NSEA)
              !
              !     Section 5)
              !
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)          &
                   UST(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)          &
                   USTDIR(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CHARN(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) CGE(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHIAW(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWIX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWIY(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWNX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUWNY(1:NSEA)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,1)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,2)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,3)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   WHITECAP(1:NSEA,4)
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TWS(1:NSEA)
              !
              !     Section 6)
              !
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SXX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SYY(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) SXY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BHD(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHIOC(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TUSX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TUSY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USSX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USSY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PRMS(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TPMS(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   US3D(1:NSEA,US3DF(2):US3DF(3))
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   US3D(1:NSEA,NK+US3DF(2):NK+US3DF(3))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)     &
                   P2SMS(1:NSEA,P2MSF(2):P2MSF(3))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUICE(1:NSEA,1)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUICE(1:NSEA,2)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHICE(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   USSP(1:NSEA,1:USSPF(2))
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)  &
                   USSP(1:NSEA,NK+1:NK+USSPF(2))
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 13 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOCX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUOCY(1:NSEA)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 14 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USSHX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USSHY(1:NSEA)

              !
              !     Section 7)
              !
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ABA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) ABD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UBA(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) UBD(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BEDFORMS(1:NSEA,1)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BEDFORMS(1:NSEA,2)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   BEDFORMS(1:NSEA,3)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   PHIBBL(1:NSEA)
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUBBL(1:NSEA,1)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   TAUBBL(1:NSEA,2)
              !
              !     Section 8)
              !
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSSX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSSY(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSCX(1:NSEA)
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSCY(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSSD(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   MSCD(1:NSEA)
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR) QP(1:NSEA)
              !
              !     Section 9)
              !
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   DTDYN(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   FCUT(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CFLXYMAX(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CFLTHMAX(1:NSEA)
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   CFLKMAX(1:NSEA)
              !
              !     Section 10)
              !
            ELSE IF ( IFI .EQ. 10 ) THEN
              READ (NDSOG,END=801,ERR=802,IOSTAT=IERR)         &
                   USERO(1:NSEA,IFJ)
            END IF
            !
            ! End of test on WRITE/READ:
            !
          END IF
          !
          ! End of test on  FLOGRD(IFI,IFJ):
          !
        END IF
        !
        ! End of IFI and IFJ loops
        !
      END DO
    END DO
    !
    ! Flush the buffers for write
    !
    IF ( WRITE ) CALL FLUSH ( NDSOG )
    !
    IF(OFILES(1) .EQ. 1) CLOSE(NDSOG)
    !
#ifdef W3_MPI
    CALL W3SETA ( IGRD, NDSE, NDST )
#endif
    !
    RETURN
    !
    ! Escape locations read errors
    !
800 CONTINUE
    WRITE (NDSE,1000) IERR
    CALL EXTCDE ( 41 )
    !
801 CONTINUE
    WRITE (NDSE,1001)
    CALL EXTCDE ( 42 )
    !
802 CONTINUE
    WRITE (NDSE,1002) IERR
    CALL EXTCDE ( 43 )
    !
803 CONTINUE
    IOTST  = -1
#ifdef W3_T
    WRITE (NDST,9020)
#endif
    RETURN
    !
    ! Formats
    !
900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL INXOUT VALUE: ',A/)
901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     MIXED READ/WRITE, LAST REQUEST: ',A/)
902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL IDSTR, READ : ',A/                        &
         '                  CHECK : ',A/)
903 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL VEROGR, READ : ',A/                       &
         '                   CHECK : ',A/)
904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     DIFFERENT NUMBER OF FIELDS, FILE :',I8,I8/       &
         '                              PROGRAM :',I8,I8/)
905 FORMAT (/' *** WAVEWATCH III WARNING IN W3IOGO :'/              &
         '     ILEGAL GNAME, READ : ',A/                        &
         '                  CHECK : ',A/)
906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
         '     ILEGAL NOSWLL, READ : ',I4/                      &
         '                   CHECK : ',I4/)
    !
    !  999 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO :'/                &
    !               '     PLEASE UPDATE FIELDS !!! '/)
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
         '     ERROR IN OPENING FILE'/                          &
         '     IOSTAT =',I5/)
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
         '     PREMATURE END OF FILE'/)
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGO : '/               &
         '     ERROR IN READING FROM FILE'/                     &
         '     IOSTAT =',I5/)
    !
#ifdef W3_T
9000 FORMAT (' TEST W3IOGO : IPASS =',I4,' INXOUT = ',A,          &
         ' WRITE = ',L1,' UNIT =',I3/                         &
         '               IGRD =',I3,' FEXT = ',A)
9001 FORMAT (' TEST W3IOGO : OPENING NEW FILE [',A,']')
9002 FORMAT (' TEST W3IOGO : TEST PARAMETERS:'/                   &
         '       IDSTR :  ',A/                                &
         '      VEROGR :  ',A/                                &
         '       GNAME :  ',A/                                &
         '        NSEA :',I6/                                 &
         '       NX,NY : ',I9,I12/                            &
         '       UNDEF : ',F8.2)
9003 FORMAT (' TEST W3IOGO : TIME  :',I9.8,I7.6/                  &
         '               FLAGS :',20L2,1X,20L2/               &
         '                      ',20L2,2X,20L2/               &
         '                      ',20L2,2X,20L2/               &
         '                      ',20L2,2X,20L2/               &
         '                      ',20L2,2X,20L2)
9010 FORMAT (' TEST W3IOGO : PROC = ',L1,' FOR ',A)
9020 FORMAT (' TEST W3IOGO : END OF FILE REACHED')
#endif
    !/
    !/ End of W3IOGO ----------------------------------------------------- /
    !/
  END SUBROUTINE W3IOGO