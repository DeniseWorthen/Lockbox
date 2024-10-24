!> @file
!> @brief Read/write restart files.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Read/write restart files.
!>
!> @author H. L. Tolman  @date 22-Mar-2021
!>
MODULE W3IORSMD
  !
  !/ ------------------------------------------------------------------- /
  !module default
  IMPLICIT NONE
  PUBLIC
  !/
  ! Add fields needed for OASIS coupling in restart
  LOGICAL :: OARST
  !/
  !/ Private parameter statements (ID strings)
  !/
  CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERINI = '2021-05-28'
  CHARACTER(LEN=26), PARAMETER, PRIVATE ::                        &
       IDSTR = 'WAVEWATCH III RESTART FILE'
  !/
CONTAINS
  !> @param[in]    INXOUT   Test string for read/write.
  !> @param[inout] NDSR     File unit number.
  !> @param[in]    DUMFPI   Dummy values for FPIS for cold start.
  !> @param[in]    IMOD     Optional grid number, defaults to 1.
  !> @param[in]    FLRSTRT  A second request for restart files (optional TRUE).
  !>
  !> @author H. L. Tolman  @date 22-Mar-2021
  !>
  SUBROUTINE W3IORS ( INXOUT, NDSR, DUMFPI, IMOD, FLRSTRT )
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD, ONLY: W3SETG, W3SETREF, RSTYPE
    USE W3ODATMD, ONLY: W3SETO
    USE W3WDATMD, only: W3SETW, W3DIMW
    USE W3ADATMD, ONLY: W3SETA, W3XETA, NSEALM
    USE W3ADATMD, ONLY: CX, CY, HS, WLM, T0M1, T01, FP0, THM, CHARN,&
         TAUWIX, TAUWIY, TWS, TAUOX, TAUOY, BHD,     &
         PHIOC, TUSX, TUSY, USSX, USSY, TAUICE,      &
         UBA, UBD, PHIBBL, TAUBBL, TAUOCX, TAUOCY,   &
         WNMEAN
    !/
    USE W3GDATMD, ONLY: NX, NY, NSEA, NSEAL, NSPEC, MAPSTA, MAPST2, &
         GNAME, FILEXT, GTYPE, UNGTYPE
    USE W3TRIAMD, ONLY: SET_UG_IOBP
    USE W3WDATMD, only : DINIT, VA, TIME, TLEV, TICE, TRHO, ICE, UST
    USE W3WDATMD, only : USTDIR, ASF, FPIS, ICEF, TIC1, TIC5, WLV
#ifdef W3_WRST
    USE W3IDATMD, ONLY: WXN, WYN, W3SETI
    USE W3IDATMD, ONLY: WXNwrst, WYNwrst
#endif
    USE W3ODATMD, ONLY: NDSE, NDST, IAPROC, NAPROC, NAPERR, NAPRST, &
         IFILE => IFILE4, FNMPRE, NTPROC, IOSTYP,    &
         FLOGRR, NOGRP, NGRPP, SCREEN
#ifdef W3_MPI
    USE W3ODATMD, ONLY: NRQRS, NBLKRS, RSBLKS, IRQRS, IRQRSS,  &
         VAAUX
    USE W3ADATMD, ONLY: MPI_COMM_WCMP
#endif
    !/
    USE W3SERVMD, ONLY: EXTCDE
    USE CONSTANTS, only: LPDLIB, file_endian
    USE W3PARALL, ONLY: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
    USE W3GDATMD, ONLY: NK, NTH
#ifdef W3_TIMINGS
    USE W3PARALL, ONLY: PRINT_MY_TIME
#endif
    USE w3odatmd, ONLY : RUNTYPE, INITFILE
    USE w3adatmd, ONLY : USSHX, USSHY
#ifdef W3_PDLIB
    USE PDLIB_FIELD_VEC
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    use w3timemd, only: set_user_timestring
    use w3odatmd, only: use_user_restname, user_restfname, ndso
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER                       :: NDSR
    !      INTEGER, INTENT(IN)           :: NDSR
    INTEGER, INTENT(IN), OPTIONAL :: IMOD
    REAL, INTENT(INOUT)           :: DUMFPI
    CHARACTER, INTENT(IN)         :: INXOUT*(*)
    LOGICAL, INTENT(IN),OPTIONAL  :: FLRSTRT
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, PARAMETER      :: LRB = 4
    !
    INTEGER                 :: IGRD, I, J, LRECL, NSIZE, IERR,      &
         NSEAT, MSPEC, TTIME(2), ISEA, JSEA,  &
         NREC, NPART, IPART, IX, IY, IXL, IP, &
         NPRTX2, NPRTY2, IYL, ITMP
    INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_MPI
    INTEGER                 :: IERR_MPI, IH, IB, ISEA0, ISEAN, &
         NRQ, NSEAL_MIN
#endif
    INTEGER(KIND=8)         :: RPOS
#ifdef W3_MPI
    INTEGER, ALLOCATABLE    :: STAT1(:,:), STAT2(:,:)
    REAL, ALLOCATABLE       :: VGBUFF(:), VLBUFF(:)
#endif
    REAL(KIND=LRB), ALLOCATABLE :: WRITEBUFF(:), TMP(:), TMP2(:)

    LOGICAL                 :: WRITE, IOSFLG
    LOGICAL                 :: FLOGOA(NOGRP,NGRPP)
    LOGICAL                 :: NDSROPN
    CHARACTER(LEN=4)        :: TYPE
    CHARACTER(LEN=10)       :: VERTST
    CHARACTER(LEN=512)      :: FNAME
    CHARACTER(LEN=26)       :: IDTST
    CHARACTER(LEN=30)       :: TNAME
    CHARACTER(LEN=15)       :: TIMETAG
    character(len=16)       :: user_timestring    !YYYY-MM-DD-SSSSS
    logical                 :: exists
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3IORS')
#endif
    !
    !
    ! Constant NDSR for using mpiifort in ZEUS ... paralell runs crashing
    !  because compiler doesn't accept reciclyng of UNIT for FORMATTED or
    !  UNFORMATTED files in OPEN
    !
    !     NDSR = 525

    IOSFLG = IOSTYP .GT. 0
    !
    ! test parameter list input ------------------------------------------ *
    !
    IF ( PRESENT(IMOD) ) THEN
      IGRD   = IMOD
    ELSE
      IGRD   = 1
    END IF
    !
    CALL W3SETO ( IGRD, NDSE, NDST )
    CALL W3SETG ( IGRD, NDSE, NDST )
    CALL W3SETW ( IGRD, NDSE, NDST )
#ifdef W3_WRST
    CALL W3SETI ( IGRD, NDSE, NDST )
#endif
    !
    IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'HOT'  .AND.               &
         INXOUT.NE.'COLD' .AND. INXOUT.NE.'WIND' .AND.               &
         INXOUT.NE.'CALM' ) THEN
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
      CALL EXTCDE ( 1 )
    END IF
    !
    WRITE = INXOUT .NE. 'READ'
    IF ( INXOUT .EQ. 'HOT' ) THEN
      TYPE   = 'FULL'
    ELSE
      TYPE   = INXOUT
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9000) INXOUT, WRITE, NTPROC, NAPROC, IAPROC, NAPRST
#endif
    !
    ! initializations ---------------------------------------------------- *
    !
    IF ( .NOT.DINIT ) THEN
      IF ( IAPROC .LE. NAPROC ) THEN
        CALL W3DIMW ( IMOD, NDSE, NDST )
      ELSE
        CALL W3DIMW ( IMOD, NDSE, NDST, .FALSE. )
      END IF
    END IF
    !
    IF ( IAPROC .LE. NAPROC ) VA(:,0) = 0.
    !
    LRECL  = MAX ( LRB*NSPEC ,                                      &
         LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
    NSIZE  = LRECL / LRB
    !     --- Allocate buffer array with zeros (used to
    !         fill bytes up to size LRECL). ---
    ALLOCATE(WRITEBUFF(NSIZE))
    WRITEBUFF(:) = 0.
    !
    !     Allocate memory to receive fields needed for coupling
    IF (OARST) THEN
      ALLOCATE(TMP(NSEA))
      ALLOCATE(TMP2(NSEA))
    ENDIF
    !
    ! open file ---------------------------------------------------------- *
    !
    if (use_user_restname) then
      ierr = -99
      if (.not. write) then
        if (runtype == 'initial') then
          if (len_trim(initfile) == 0) then
            ! no IC file, use startup option
            goto 800
          else
            ! IC file exists - use it
            fname = trim(initfile)
          end if
        else
          call set_user_timestring(time,user_timestring)
          fname = trim(user_restfname)//trim(user_timestring)
          inquire( file=trim(fname), exist=exists)
          if (.not. exists) then
            call extcde (60, msg="required initial/restart file " // trim(fname) // " does not exist")
          end if
        end if
      else
        call set_user_timestring(time,user_timestring)
        fname = trim(user_restfname)//trim(user_timestring)
      end if
      ! write out filename
      if (iaproc == naprst) then
        IF ( WRITE ) THEN
          write (ndso,'(a)') 'WW3: writing restart file '//trim(fname)
        else
          write (ndso,'(a)') 'WW3: reading initial/restart file '//trim(fname)
        end if
      end if
      if ( write ) then
        IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST )        &
             open (ndsr,file=trim(fname), form='unformatted', convert=file_endian,       &
             ACCESS='STREAM',ERR=800,IOSTAT=IERR)
      ELSE  ! READ
        open (ndsr, file=trim(fname), form='unformatted', convert=file_endian,       &
             ACCESS='STREAM',ERR=800,IOSTAT=IERR,           &
             STATUS='OLD',ACTION='READ')
      END IF
    else
      I      = LEN_TRIM(FILEXT)
      J      = LEN_TRIM(FNMPRE)
      !
      !CHECKPOINT RESTART FILE
      ITMP=0
      IF ( PRESENT(FLRSTRT) ) THEN
        IF (FLRSTRT) THEN
          WRITE(TIMETAG,"(i8.8,'.'i6.6)")TIME(1),TIME(2)
          FNAME=TIMETAG//'.restart.'//FILEXT(:I)
          ITMP=1
        END IF
      END IF
      IF(ITMP.NE.1)THEN ! FNAME is not set above, so do it here
        IF ( IFILE.EQ.0 ) THEN
          FNAME  = 'restart.'//FILEXT(:I)
        ELSE
          FNAME  = 'restartNNN.'//FILEXT(:I)
          IF ( WRITE .AND. IAPROC.EQ.NAPRST )                         &
               WRITE (FNAME(8:10),'(I3.3)') IFILE
        END IF
      END IF
      IFILE  = IFILE + 1
      !
#ifdef W3_T
      WRITE (NDST,9001) trim(FNAME), LRECL
#endif
      !

      IF(NDST.EQ.NDSR)THEN
        IF ( IAPROC .EQ. NAPERR )                                    &
             WRITE(NDSE,'(A,I8)')'UNIT NUMBERS OF RESTART FILE AND '&
             //'TEST OUTPUT ARE THE SAME : ',NDST
        CALL EXTCDE ( 15 )
      ENDIF

      IF ( WRITE ) THEN
        IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST )                    &
             OPEN (NDSR,FILE=FNMPRE(:J)//trim(FNAME),form='UNFORMATTED', convert=file_endian,       &
             ACCESS='STREAM',ERR=800,IOSTAT=IERR)
      ELSE
        OPEN (NDSR,FILE=FNMPRE(:J)//trim(FNAME),form='UNFORMATTED', convert=file_endian,       &
             ACCESS='STREAM',ERR=800,IOSTAT=IERR,                  &
             STATUS='OLD',ACTION='READ')
      END IF
    end if
    !
    ! test info ---------------------------------------------------------- *
    !
    IF ( WRITE ) THEN
      !
      IF ( IAPROC .EQ. NAPRST ) THEN
        !           Because data has mixed data types we do not know how many
        !           bytes remain to fill up to LRECL. ---
        !           --- Make the entire record zero ---
        WRITEBUFF(:) = 0.
        WRITE (NDSR,POS=1) WRITEBUFF
        !           --- Replace zeros with data ---
        WRITE (NDSR,POS=1) IDSTR, VERINI, GNAME, TYPE, NSEA,      &
             NSPEC, FLOGRR
      END IF
      RSTYPE = 3
      !
    ELSE
      READ (NDSR,POS=1,ERR=802,IOSTAT=IERR)                       &
           IDTST, VERTST, TNAME, TYPE, NSEAT, MSPEC, FLOGOA
      !
      IF ( IDTST .NE. IDSTR ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,901) IDTST, IDSTR
        CALL EXTCDE ( 10 )
      END IF
      IF ( VERTST .NE. VERINI ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,902) VERTST, VERINI
        CALL EXTCDE ( 11 )
      END IF
      IF ( TNAME .NE. GNAME ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,903) TNAME, GNAME
      END IF
      IF (TYPE.NE.'FULL' .AND. TYPE.NE.'COLD' .AND.               &
           TYPE.NE.'WIND' .AND. TYPE.NE.'CALM' ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,904) TYPE
        CALL EXTCDE ( 12 )
      END IF
      IF (NSEAT.NE.NSEA .OR. NSPEC.NE.MSPEC) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) MSPEC, NSEAT, NSPEC, NSEA
        CALL EXTCDE ( 13 )
      END IF
      IF (TYPE.EQ.'FULL') THEN
        RSTYPE = 2
      ELSE IF (TYPE.EQ.'WIND') THEN
        RSTYPE = 1
      ELSE IF (TYPE.EQ.'CALM') THEN
        RSTYPE = 4
      ELSE
        RSTYPE = 0
      END IF

      IF (.NOT. WRITE .AND. OARST .AND. IAPROC .EQ. NAPROC) THEN
        DO I=1, NOGRP
          DO J=1, NGRPP
            IF (FLOGRR(I,J) .AND. .NOT. FLOGOA(I,J)) THEN
              WRITE(SCREEN,1000) I, J
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      !
    END IF
    !
100 CONTINUE
    !
#ifdef W3_T
    WRITE (NDST,9002) IDSTR, VERINI, GNAME, TYPE,                &
         NSEA, NSEAL, NSPEC
#endif
    !
    ! TIME if required --------------------------------------------------- *
    !
    IF (TYPE.EQ.'FULL') THEN
      RPOS  = 1_8 + LRECL*(2-1_8)
      IF ( WRITE ) THEN
        IF ( IAPROC .EQ. NAPRST ) THEN
          WRITEBUFF(:) = 0.
          WRITE (NDSR,POS=RPOS) WRITEBUFF
          WRITE (NDSR,POS=RPOS) TIME
        END IF
      ELSE
        READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR) TTIME
#ifdef W3_CESMCOUPLED
        if (runtype == 'branch' .or. runtype == 'continue') then
          IF (TIME(1).NE.TTIME(1) .OR. TIME(2).NE.TTIME(2)) THEN
            IF ( IAPROC .EQ. NAPERR )                           &
                 WRITE (NDSE,906) TTIME, TIME
            CALL EXTCDE ( 20 )
          END IF
        end if
#else
        IF (TIME(1).NE.TTIME(1) .OR. TIME(2).NE.TTIME(2)) THEN
          IF ( IAPROC .EQ. NAPERR )                           &
               WRITE (NDSE,906) TTIME, TIME
          CALL EXTCDE ( 20 )
        END IF
#endif
      END IF
      !
#ifdef W3_T
      WRITE (NDST,9003) TIME
    ELSE
      WRITE (NDST,9004)
#endif
      !
    END IF
    !
    ! Spectra ------------------------------------------------------------ *
    !          ( Bail out if write for TYPE.EQ.'WIND' )
    !
    IF ( WRITE ) THEN
      IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
        IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
          CLOSE ( NDSR )
        END IF
#ifdef W3_T
        WRITE (NDST,9005) TYPE
#endif
        ! Clean up file handles and allocated arrays
        INQUIRE (UNIT=NDSR, OPENED=NDSROPN)
        IF (NDSROPN)              CLOSE(NDSR)
        IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
        IF (ALLOCATED(TMP))       DEALLOCATE(TMP)
        IF (ALLOCATED(TMP2))      DEALLOCATE(TMP2)

        RETURN
      ELSE IF ( IAPROC.LE.NAPROC .OR. IAPROC.EQ. NAPRST ) THEN
        !
        ! Original non-server version writing of spectra
        !
        IF ( .NOT.IOSFLG .OR. (NAPROC.EQ.1.AND.NAPRST.EQ.1) ) THEN
#ifdef W3_MPI
          DO JSEA=1, NSEAL
            CALL INIT_GET_ISEA(ISEA, JSEA)
            NREC   = ISEA + 2
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITEBUFF(:) = 0.
            WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
          END DO
#else
          DO JSEA=1, NSEA
            ISEA = JSEA
            NREC   = ISEA + 2
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITEBUFF(:) = 0.
            WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
          END DO
#endif
          !
          ! I/O server version writing of spectra ( !/MPI )
          !
#ifdef W3_MPI
        ELSE
          !
          IF (LPDLIB) THEN
#endif
#ifdef W3_TIMINGS
            CALL PRINT_MY_TIME("Before UNST_PDLIB_WRITE_TO_FILE")
#endif
#ifdef W3_PDLIB
            CALL UNST_PDLIB_WRITE_TO_FILE(NDSR)
#endif
#ifdef W3_TIMINGS
            CALL PRINT_MY_TIME("After UNST_PDLIB_WRITE_TO_FILE")
#endif
#ifdef W3_MPI
          ELSE

            IF ( IAPROC .NE. NAPRST ) THEN
              NRQ    = 1
            ELSE IF ( NAPRST .LE. NAPROC ) THEN
              NRQ    = NAPROC - 1
            ELSE
              NRQ    = NAPROC
            END IF
            !
            ALLOCATE ( STAT1(MPI_STATUS_SIZE,NRQ) )
            IF ( IAPROC .EQ. NAPRST ) CALL MPI_STARTALL    &
                 ( NRQ, IRQRSS, IERR_MPI )
            !
            DO IB=1, NBLKRS
              ISEA0  = 1 + (IB-1)*RSBLKS*NAPROC
              ISEAN  = MIN ( NSEA , IB*RSBLKS*NAPROC )
              !
              IF ( IAPROC .EQ. NAPRST ) THEN
                !
                IH     = 1 + NRQ * (IB-1)
                CALL MPI_WAITALL                         &
                     ( NRQ, IRQRSS(IH), STAT1, IERR_MPI )
                IF ( IB .LT. NBLKRS ) THEN
                  IH     = 1 + NRQ * IB
                  CALL MPI_STARTALL                    &
                       ( NRQ, IRQRSS(IH), IERR_MPI )
                END IF
                !
                DO ISEA=ISEA0, ISEAN
                  NREC   = ISEA + 2
                  CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, IP)
                  RPOS   = 1_8 + LRECL*(NREC-1_8)
                  WRITEBUFF(:) = 0.
                  IF ( IP .EQ. NAPRST ) THEN
                    WRITEBUFF(1:NSPEC) = VA(1:NSPEC,JSEA)
                  ELSE
                    JSEA   = JSEA - 2*((IB-1)/2)*RSBLKS
                    WRITEBUFF(1:NSPEC) = VAAUX(1:NSPEC,JSEA,IP)
                  END IF
                  WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) &
                       WRITEBUFF
                END DO
                !
              ELSE
                !
                CALL MPI_STARTALL                        &
                     ( 1, IRQRSS(IB), IERR_MPI )
                CALL MPI_WAITALL                         &
                     ( 1, IRQRSS(IB), STAT1, IERR_MPI )
                !
              END IF
            END DO
            !
            DEALLOCATE ( STAT1 )
          END IF
#endif
          !
        END IF
        !
      END IF
    ELSE
      !
      ! Reading spectra
      !
      IF ( TYPE.EQ.'WIND' .OR. TYPE.EQ.'CALM' ) THEN
#ifdef W3_T
        WRITE (NDST,9020) TYPE
#endif
      ELSE
        IF (LPDLIB) THEN
#ifdef W3_TIMINGS
          CALL PRINT_MY_TIME("Before UNST_PDLIB_READ_FROM_FILE")
#endif
#ifdef W3_PDLIB
          CALL UNST_PDLIB_READ_FROM_FILE(NDSR)
#endif
#ifdef W3_TIMINGS
          CALL PRINT_MY_TIME("After UNST_PDLIB_READ_FROM_FILE")
#endif
        ELSE
#ifdef W3_MPI
          NSEAL_MIN = 1 + (NSEA-NAPROC)/NAPROC
          IF ( NAPROC.GT.1 ) THEN
            !/ ----------- Large number of small-sized record reads will tend ---- *
            !/             to perform badly on most file systems. We read this part
            !/             using streams and scatter the results using MPI.
            !/                                                      ( M. WARD, NCI )
            !
            !              Begin computational proc. only section ---------------- *
            IF ( IAPROC.LE.NAPROC ) THEN
              !
              !              Main loop --------------------------------------------- *
              ALLOCATE( VGBUFF( NSIZE * NAPROC ) )
              ALLOCATE( VLBUFF( NSIZE ) )
              !
              DO JSEA = 1, NSEAL_MIN
                !                Read NAPROC records into buffer VGBUFF. ------------- *
                IF ( IAPROC .EQ. NAPROC ) THEN
                  RPOS = 1_8 + (2 + (JSEA - 1_8) * NAPROC) * LRECL
                  READ(NDSR, POS=RPOS,ERR=802,IOSTAT=IERR) VGBUFF(:)
                ELSE
                  VGBUFF(:) = 0.
                END IF
                !                Distribute one record to each rank.
                CALL MPI_SCATTER(VGBUFF, NSIZE, MPI_REAL,             &
                     VLBUFF, NSIZE, MPI_REAL,             &
                     NAPROC-1, MPI_COMM_WCMP, IERR        )
                !                Transfer the spectral content of VLBUFF to VA. ------ *
                VA(1:NSPEC,JSEA) = VLBUFF(1:NSPEC)
              END DO
              !
              !              Include remainder values (switch to record format) ---- *
              JSEA = NSEAL_MIN + 1
              IF ( JSEA.EQ.NSEAL ) THEN
                CALL INIT_GET_ISEA(ISEA, JSEA)
                NREC = ISEA + 2
                RPOS = 1_8 + LRECL*(NREC-1_8)
                READ (NDSR, POS=RPOS, ERR=802, IOSTAT=IERR)          &
                     (VA(I,JSEA), I=1,NSPEC)
              END IF
              !
              DEALLOCATE( VGBUFF )
              DEALLOCATE( VLBUFF )
              !
              !              End computational proc. only section ------------------ *
            END IF
            !
          ELSE
#endif
            VA = 0.
            DO JSEA=1, NSEA
              CALL INIT_GET_ISEA(ISEA, JSEA)
              NREC   = ISEA + 2
              RPOS   = 1_8 + LRECL*(NREC-1_8)
              READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                   (VA(I,JSEA),I=1,NSPEC)
            ENDDO
#ifdef W3_MPI
          END IF
#endif
        END IF
      END IF
    END IF

    VA = MAX(0.,VA)
    !
#ifdef W3_T
    WRITE (NDST,9006)
#endif
    !
    ! Water level etc. if required --------------------------------------- *
    !     ( For cold start write test output and cold start initialize
    !       water levels. Note that MAPSTA overwrites the one read from the
    !       model definition file, so that it need not be initialized. )
    !
    NREC   = NSEA + 3
    NPART  = 1 + (NSEA-1)/NSIZE
    NPRTX2 = 1 + (NX-1)/NSIZE
    NPRTY2 = 1 + (NY-1)/NSIZE
    !
    IF ( WRITE ) THEN
      !
      IF (TYPE.EQ.'FULL') THEN
        !
        IF ( IAPROC .EQ. NAPRST ) THEN
          !
#ifdef W3_MPI
          if (associated(irqrs)) then
            ALLOCATE ( STAT2(MPI_STATUS_SIZE,NRQRS) )
            CALL MPI_WAITALL                               &
                 ( NRQRS, IRQRS , STAT2, IERR_MPI )
            DEALLOCATE ( STAT2 )
          end if
#endif
          !
          RPOS  = 1_8 + LRECL*(NREC-1_8)
          WRITEBUFF(:) = 0.
          WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
          WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)           &
               TLEV, TICE, TRHO
          DO IPART=1,NPART
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                 (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                 MIN(NSEA,IPART*NSIZE))
          END DO
          DO IPART=1,NPART
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                 (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                 MIN(NSEA,IPART*NSIZE))
          END DO

#ifdef W3_WRST
          ! The WRST switch saves the values of wind in the
          ! restart file and then uses the wind for the first
          ! time step here.  This is needed when coupling with
          ! an atm model that does not have 10m wind speeds at
          ! initialization.  If there is no restart, wind is zero
#endif

#ifdef W3_WRST
          DO IX=1, NX
            DO IPART=1,NPRTY2
              NREC  = NREC + 1
              RPOS  = 1_8 + LRECL*(NREC-1_8)
              WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
              WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                   (WXN(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                   MIN(NY,IPART*NSIZE))
            END DO
          END DO
          DO IX=1, NX
            DO IPART=1,NPRTY2
              NREC  = NREC + 1
              RPOS  = 1_8 + LRECL*(NREC-1_8)
              WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
              WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                   (WYN(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                   MIN(NY,IPART*NSIZE))
            END DO
          END DO
#endif
          ALLOCATE ( MAPTMP(NY,NX) )
          MAPTMP = MAPSTA + 8*MAPST2
          DO IY=1, NY
            DO IPART=1,NPRTX2
              NREC  = NREC + 1
              RPOS  = 1_8 + LRECL*(NREC-1_8)
              WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                   WRITEBUFF
              WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)       &
                   (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,    &
                   MIN(NX,IPART*NSIZE))
            END DO
          END DO
          DEALLOCATE ( MAPTMP )
          DO IPART=1,NPART
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                 (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                 MIN(NSEA,IPART*NSIZE))
          END DO
          DO IPART=1,NPART
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                 (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,       &
                 MIN(NSEA,IPART*NSIZE))
          END DO
          DO IPART=1,NPART
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                 (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,          &
                 MIN(NSEA,IPART*NSIZE))
          END DO
          DO IPART=1,NPART
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR) WRITEBUFF
            WRITE (NDSR,POS=RPOS,ERR=803,IOSTAT=IERR)         &
                 (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,         &
                 MIN(NSEA,IPART*NSIZE))
          END DO

#ifdef W3_T
          WRITE (NDST,9007)
        ELSE
          DO ISEA=1, NSEA
            WLV(ISEA) = 0.
            ICE(ISEA) = 0.
          END DO
          WRITE (NDST,9008)
#endif
        END IF
      END IF
    ELSE
      IF (TYPE.EQ.'FULL') THEN
        RPOS = 1_8 + LRECL*(NREC-1_8)
        READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)                &
             TLEV, TICE, TRHO
        DO IPART=1,NPART
          NREC  = NREC + 1
          RPOS = 1_8 + LRECL*(NREC-1_8)
          READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
               (WLV(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
               MIN(NSEA,IPART*NSIZE))
        END DO
        DO IPART=1,NPART
          NREC  = NREC + 1
          RPOS = 1_8 + LRECL*(NREC-1_8)
          READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
               (ICE(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
               MIN(NSEA,IPART*NSIZE))
        END DO
#ifdef W3_WRST
        DO IX=1, NX
          DO IPART=1,NPRTY2
            NREC  = NREC + 1
            RPOS = 1_8 + LRECL*(NREC-1_8)
            READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                 (WXNwrst(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                 MIN(NY,IPART*NSIZE))
          END DO
        END DO
        DO IX=1, NX
          DO IPART=1,NPRTY2
            NREC  = NREC + 1
            RPOS = 1_8 + LRECL*(NREC-1_8)
            READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
                 (WYNwrst(IX,IYL),IYL=1+(IPART-1)*NSIZE,         &
                 MIN(NY,IPART*NSIZE))
          END DO
        END DO
#endif
        ALLOCATE ( MAPTMP(NY,NX) )
        DO IY=1, NY
          DO IPART=1,NPRTX2
            NREC  = NREC + 1
            RPOS  = 1_8 + LRECL*(NREC-1_8)
            READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)            &
                 (MAPTMP(IY,IXL),IXL=1+(IPART-1)*NSIZE,        &
                 MIN(NX,IPART*NSIZE))
          END DO
        END DO
        MAPSTA = MOD(MAPTMP+2,8) - 2
        MAPST2 = (MAPTMP-MAPSTA) / 8
        DEALLOCATE ( MAPTMP )
        !
        ! Updates reflections maps:
        !
        IF (GTYPE.EQ.UNGTYPE) THEN
#ifdef W3_REF1
        ELSE
          CALL W3SETREF
#endif
        ENDIF
        !
        DO IPART=1,NPART
          NREC  = NREC + 1
          RPOS  = 1_8 + LRECL*(NREC-1_8)
          READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
               (UST(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
               MIN(NSEA,IPART*NSIZE))
        END DO
        DO IPART=1,NPART
          NREC  = NREC + 1
          RPOS  = 1_8 + LRECL*(NREC-1_8)
          READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
               (USTDIR(ISEA),ISEA=1+(IPART-1)*NSIZE,           &
               MIN(NSEA,IPART*NSIZE))
        END DO
        DO IPART=1,NPART
          NREC  = NREC + 1
          RPOS  = 1_8 + LRECL*(NREC-1_8)
          READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
               (ASF(ISEA),ISEA=1+(IPART-1)*NSIZE,              &
               MIN(NSEA,IPART*NSIZE))
        END DO
        DO IPART=1,NPART
          NREC  = NREC + 1
          RPOS  = 1_8 + LRECL*(NREC-1_8)
          READ (NDSR,POS=RPOS,ERR=802,IOSTAT=IERR)              &
               (FPIS(ISEA),ISEA=1+(IPART-1)*NSIZE,             &
               MIN(NSEA,IPART*NSIZE))
        END DO
#ifdef W3_T
        WRITE (NDST,9007)
#endif
      ELSE
        TLEV(1) = -1
        TLEV(2) =  0
        TICE(1) = -1
        TICE(2) =  0
        TRHO(1) = -1
        TIC1(1) = -1
        TIC1(2) =  0
        TIC5(1) = -1
        TIC5(2) =  0
#ifdef W3_WRST
        WXNwrst =  0.
        WYNwrst =  0.
#endif
        WLV     =  0.
        ICE     =  0.
        ASF     =  1.
        FPIS    =  DUMFPI

#ifdef W3_T
        WRITE (NDST,9008)
#endif
      END IF
    END IF
    !
    ! Close file --------------------------------------------------------- *
    !
    IF (WRITE) THEN
      IF ( .NOT.IOSFLG .OR. IAPROC.EQ.NAPRST ) THEN
        CLOSE ( NDSR )
      END IF
    ELSE
      CLOSE ( NDSR )
    END IF
    !
    IF (ALLOCATED(WRITEBUFF)) DEALLOCATE(WRITEBUFF)
    IF (ALLOCATED(TMP))  DEALLOCATE(TMP)
    IF (ALLOCATED(TMP2)) DEALLOCATE(TMP2)
    !
    RETURN
    !
    ! Escape locations read errors :
    !
800 CONTINUE
#ifdef W3_LN0
    TYPE   = 'WIND'
    RSTYPE = 1
#endif
#ifdef W3_SEED
    TYPE   = 'CALM'
    RSTYPE = 4
#endif
#ifdef W3_LN1
    TYPE   = 'CALM'
    RSTYPE = 4
#endif
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,990) TYPE, IERR
    GOTO 100
    !
801 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,991)
    CALL EXTCDE ( 30 )
    !
802 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,992) IERR
    CALL EXTCDE ( 31 )
    !
803 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,993) IERR, RPOS
    CALL EXTCDE ( 31 )
    !
    !
    ! Formats
    !
900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
         '     ILLEGAL INXOUT VALUE: ',A/)
901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
         '     ILLEGAL IDSTR, READ : ',A/                       &
         '                   CHECK : ',A/)
902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
         '     ILLEGAL VERINI, READ : ',A/                      &
         '                    CHECK : ',A/)
903 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS :'/              &
         '     ILLEGAL GNAME, READ : ',A/                       &
         '                   CHECK : ',A/)
904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
         '     ILLEGAL TYPE : ',A/)
905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
         '     CONFLICTING NSPEC, NSEA GRID : ',2I8/            &
         '                         EXPECTED : ',2I8/)
906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS :'/                &
         '     CONFLICTING TIMES: FILE : ',I10.8,I8.6/          &
         '                       MODEL : ',I10.8,I8.6/)
    !
990 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS : '/             &
         '     NO READABLE RESTART FILE, ',                     &
         'INITIALIZE WITH ''',A,''' INSTEAD'/              &
         '     IOSTAT =',I5/)
991 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
         '     PREMATURE END OF FILE'/)
992 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
         '     ERROR IN READING FROM FILE'/                     &
         '     IOSTAT =',I5/)
993 FORMAT (/' *** WAVEWATCH III ERROR IN W3IORS : '/               &
         '     ERROR IN WRITING TO FILE'/                       &
         '     IOSTAT =',I5,', POS =',I11 /)
1000 FORMAT (/' *** WAVEWATCH III WARNING IN W3IORS : '/             &
         '     REQUESTED EXTRA RESTART GROUP',I2,' FIELD',I2, / &
         '     IS NOT PRESENT IN THE RESTART FILE.'/            &
         '     THIS MAY CAUSE INSTABILITIES IN COUPLED CONFIGURATIONS')
    !
    !
#ifdef W3_T
9000 FORMAT (' TEST W3IORS : TEST PARAMETERS :'/                  &
         '      INXOUT : ',A,/                                &
         '       WRITE : ',L10/                               &
         '      NTPROC : ',I10/                               &
         '      NAPROC : ',I10/                               &
         '      IAPROC : ',I10/                               &
         '      NAPRST : ',I10)
9001 FORMAT ('      FNAME  : ',A/                                 &
         '       LRECL : ',I10)
9002 FORMAT ('       IDSTR : ',A/                                 &
         '      VERINI : ',A/                                 &
         '       GNAME : ',A/                                 &
         '        TYPE : ',A/                                 &
         '        NSEA : ',I10/                               &
         '       NSEAL : ',I10/                               &
         '       NSPEC : ',I10)
9003 FORMAT (' TEST W3IORS :',I10.8,I8.6,' UTC')
9004 FORMAT (' TEST W3IORS : TIME NOT AVAILABLE ')
9005 FORMAT (' TEST W3IORS : NO SPECTRA, TYPE=''',A,''' ')
9006 FORMAT (' TEST W3IORS : SPECTRA PROCESSED ')
9007 FORMAT (' TEST W3IORS : WATER LEVELS ETC. PROCESSED ')
9008 FORMAT (' TEST W3IORS : WATER LEVELS ETC. PROCESSED (DUMMY)')
    !
9020 FORMAT (' TEST W3IORS : RSTYPE = ',A,', PERFORMED BY W3INIT')
#endif
    !/
    !/ End of W3IORS ----------------------------------------------------- /
    !/
  END SUBROUTINE W3IORS
  !/
  !/ End of module W3IORSMD -------------------------------------------- /
  !/
END MODULE W3IORSMD
