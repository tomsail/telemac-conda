!                   *************************
                    SUBROUTINE FLUXPR_GAIA
!                   *************************
!
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,
     & FLXS,VOLNEGS,VOLPOSS,SUSP,FLXC,VOLNEGC,VOLPOSC,CHARR)
!
!***********************************************************************
! GAIA   V7P0                                         21/07/2011
!***********************************************************************
!
!>@brief Computes fluxes through control sections
!!       and adds them up to obtain oscillating volumes.
!
!>@note  This subroutine prints out discharges through control
!!       sections. You can rewrite it to divert these printouts
!!       to a file or to change the format.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] CHARR   Logical, bedload or not
!>@param[in] CTRLSC  Numbers of points in the control sections
!>@param[in] FLX     Fluxes through control sections
!>@param[in] FLXC    Bedload discharge
!>@param[in] FLXS    Suspended load discharge
!>@param[in] INFO    If yes : information is printed
!>@param[in] NCSIZE  Number of processors (parallel)
!>@param[in] NSEC    Number of control sections
!>@param[in] NSEG    Number of segments per control section
!>@param[in] SUSP    Logical, suspension or not
!>@param[in] TPS     Time
!>@param[in] VOLNEG  Cumulated negative volume through sections
!>@param[in] VOLNEGC Cumulated negative volume for the bedload
!>@param[in] VOLNEGS Cumulated negative volume for the suspension
!>@param[in] VOLPOS  Cumulated positive volume through sections
!>@param[in] VOLPOSC Cumulated positive volume for the bedload
!>@param[in] VOLPOSS Cumulated positive volume for the suspended load
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: IPID
      USE DECLARATIONS_GAIA, ONLY: GAI_FILES,GAISEO,CHAIN,
     &                                INIT_FLUXPR,WORK,WORKB
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN,P_SUM
      IMPLICIT NONE
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,SUSP,CHARR
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC),TPS
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: FLXS(NSEC),FLXC(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGS(NSEC),VOLPOSS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEGC(NSEC),VOLPOSC(NSEC)
!
!!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISEC,II,ERR,NSEO
      CHARACTER(LEN=16) :: FMTZON='(4(1X,1PG21.14))'
      LOGICAL :: OLD_METHOD=.FALSE.
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
      DOUBLE PRECISION :: DTMP1,DTMP2,DTMP3,DTMP4
!##<JR @ ADJOINTWARE
!
!-----------------------------------------------------------------------
!
      NSEO=GAI_FILES(GAISEO)%LU
!
!-----------------------------------------------------------------------
!
      IF(.NOT.ALLOCATED(CHAIN)) OLD_METHOD=.TRUE.
!
!     DONE ONCE FOR ALL
!
      IF(INIT_FLUXPR.AND.(TRIM(GAI_FILES(GAISEO)%NAME).NE.'') ) THEN
!
        IF(NCSIZE.GT.1) THEN
          ALLOCATE (WORK(NSEC), STAT=ERR)
          IF(ERR.NE.0) THEN
            WRITE(LU,*) 'FLUXPR_GAIA: ERROR ALLOCATING WORK:',ERR
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(CHARR.AND.SUSP) THEN
            ALLOCATE (WORKB(NSEC), STAT=ERR)
            IF(ERR.NE.0) THEN
              WRITE(LU,*) 'FLUXPR_GAIA: ERROR ALLOCATING WORK:',ERR
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
!
        INIT_FLUXPR=.FALSE.
!
        IF(CHARR.AND..NOT.SUSP) THEN
          WRITE(NSEO,*) ' INTEGRATED BEDLOAD DISCHARGES '
          WRITE(NSEO,*) ' VARIABLES = TIME(S) QC(M3/S) FOR',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC)
        ENDIF
!
        IF(SUSP.AND..NOT.CHARR) THEN
          WRITE(NSEO,*) ' INTEGRATED SUSPENDED LOAD DISCHARGES '
          WRITE(NSEO,*) ' VARIABLES = TIME   QS (M3/S) FOR SECTIONS '
     &           ,(II,II=1,NSEC)
        ENDIF
!
        IF(CHARR.AND.SUSP) THEN
          WRITE(NSEO,*) ' INTEGRATED BEDLOAD AND SUSPENDED LOAD '
          WRITE(NSEO,*) 'VARIABLES = TIME , QC FOR ',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC), ' QS FOR',
     &           (' '//TRIM(CHAIN(ISEC)%DESCR), ISEC=1,NSEC)
        ENDIF
!
        WRITE(NSEO,100)(II , II= 1 ,NSEC)
100     FORMAT(' TIME',' SECTION:',I2, 'SECTION: ',I2)
!
      ENDIF
!
      IF(INFO) THEN
!
      IF(OLD_METHOD) THEN
!
        IF(NCSIZE.LE.1) THEN
!
!         SCALAR MODE
!
          DO ISEC = 1,NSEC
!
            WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                 CTRLSC(2+2*(ISEC-1)),
     &                                 FLX(ISEC),VOLNEG(ISEC),
     &                                 VOLPOS(ISEC)
!
131         FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE:                 ',G16.7,/,5X,
     &               'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &               'CUMULATED POSITIVE VOLUME: ',G16.7)
            IF(SUSP) THEN
              WRITE(LU,1302) FLXS(ISEC),
     &                                    VOLNEGS(ISEC),
     &                                    VOLPOSS(ISEC)
            ENDIF
!
1302        FORMAT(5X,'DISCHARGE IN SUSPENSION:   ',G16.7,/,5X,
     &                'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &                'CUMULATED POSITIVE VOLUME: ',G16.7)
            IF(CHARR) THEN
              WRITE(LU,1304) FLXC(ISEC),
     &                                    VOLNEGC(ISEC),
     &                                    VOLPOSC(ISEC)
1304          FORMAT(5X,'BEDLOAD DISCHARGE:         ',G16.7,/,5X,
     &                  'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &                  'CUMULATED POSITIVE VOLUME: ',G16.7)
            ENDIF
!
          ENDDO
!
        ELSE
!
!         PARALLEL MODE
!
          DO ISEC = 1,NSEC
!
!           SECTIONS ACROSS 2 SUB-DOMAINS WILL HAVE NSEG=0 OR -1
!           AND -1 WANTED HERE FOR RELEVANT MESSAGE
!
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
            DTMP1 = P_MIN(FLX(ISEC))
            DTMP2 = P_MAX(FLX(ISEC))
            DTMP3 = P_MIN(VOLNEG(ISEC))
            DTMP4 = P_MAX(VOLPOS(ISEC))
!##<JR @ ADJOINTWARE
            II=P_MIN(NSEG(ISEC))
            IF(II.GE.0) THEN
!
              WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                   CTRLSC(2+2*(ISEC-1)),
     &                                   DTMP1+DTMP2,DTMP3,DTMP4
!
              IF(SUSP) THEN
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
                DTMP1 = P_MIN(FLXS(ISEC))
                DTMP2 = P_MAX(FLXS(ISEC))
                DTMP3 = P_MIN(VOLNEGS(ISEC))
                DTMP4 = P_MAX(VOLPOSS(ISEC))
!##<JR @ ADJOINTWARE
                WRITE(LU,1302) DTMP1+DTMP2,DTMP3,DTMP4
              ENDIF
              IF(CHARR) THEN
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
                DTMP1 = P_MIN(FLXC(ISEC))
                DTMP2 = P_MAX(FLXC(ISEC))
                DTMP3 = P_MIN(VOLNEGC(ISEC))
                DTMP4 = P_MAX(VOLPOSC(ISEC))
!##<JR @ ADJOINTWARE
                WRITE(LU,1304) DTMP1+DTMP2,DTMP3,DTMP4
              ENDIF
!
!           OLD METHOD AND SECTION ON SEVERAL SUB-DOMAIN
!           IN THIS CASE NOTHING IS COMPUTED
!
            ELSE
!
              WRITE(LU,135) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                        CTRLSC(2+2*(ISEC-1))
135           FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'ACROSS TWO SUB-DOMAINS, NO COMPUTATION')
!
            ENDIF
!
          ENDDO
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
!       NEW METHOD
!       CHAIN ALLOCATED, I.E. SERIAL OR PARALLEL CASE FROM SECTIONS INPUT FILE
!       WE CAN APPLY CO-ORDINATES INSTEAD AND/OR NAMES OF SECTIONS
!
        DO ISEC = 1,NSEC
!
          IF(NCSIZE.GT.1) THEN
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
            DTMP1 = P_SUM(FLX(ISEC))
            DTMP2 = P_SUM(VOLNEG(ISEC))
            DTMP3 = P_SUM(VOLPOS(ISEC))
!##<JR @ ADJOINTWARE
            WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                                 DTMP1,DTMP2,DTMP3
          ELSE
            WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                   FLX(ISEC),VOLNEG(ISEC),VOLPOS(ISEC)
          ENDIF
231       FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE:                 ',G16.7,/,5X,
     &               'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &               'CUMULATED POSITIVE VOLUME: ',G16.7)
          IF(SUSP) THEN
            IF(NCSIZE.GT.1) THEN
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              DTMP1 = P_SUM(FLXS(ISEC))
              DTMP2 = P_SUM(VOLNEGS(ISEC))
              DTMP3 = P_SUM(VOLPOSS(ISEC))
!##<JR @ ADJOINTWARE
              WRITE(LU,2302) DTMP1,DTMP2,DTMP3
            ELSE
              WRITE(LU,2302)
     &                FLXS(ISEC),VOLNEGS(ISEC),VOLPOSS(ISEC)
            ENDIF
2302        FORMAT(5X,'DISCHARGE IN SUSPENSION:   ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,

     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
          ENDIF
!
          IF(CHARR) THEN
            IF(NCSIZE.GT.1) THEN
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              DTMP1 = P_SUM(FLXC(ISEC))
              DTMP2 = P_SUM(VOLNEGC(ISEC))
              DTMP3 = P_SUM(VOLPOSC(ISEC))
!##<JR @ ADJOINTWARE
              WRITE(LU,2304) DTMP1,DTMP2,DTMP3
            ELSE
              WRITE(LU,2304)
     &                FLXC(ISEC),VOLNEGC(ISEC),VOLPOSC(ISEC)
            ENDIF
2304        FORMAT(5X,'BEDLOAD DISCHARGE:         ',G16.7,/,5X,
     &            'CUMULATED NEGATIVE VOLUME: ',G16.7,/,5X,
     &            'CUMULATED POSITIVE VOLUME: ',G16.7)
          ENDIF
!
        ENDDO
!
!
      ENDIF ! IF OLD_METHOD
!
!     A SECTIONS OUTPUT FILE HAS BEEN GIVEN, IT IS FILLED
!
      IF(TRIM(GAI_FILES(GAISEO)%NAME).NE.'') THEN
!
!       ONLY BEDLOAD
!
        IF(CHARR.AND..NOT.SUSP) THEN
          IF(NCSIZE.GT.1) THEN
            DO ISEC=1,NSEC
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              DTMP1 = P_SUM(FLXC(ISEC))
              WORK(ISEC) = DTMP1
!##<JR @ ADJOINTWARE
            ENDDO
!           IN // ONLY PROCESSOR 0 WRITES THE FILE
            IF(IPID.EQ.0) THEN
              WRITE(NSEO,FMT=FMTZON) TPS,(WORK(ISEC),ISEC=1,NSEC)
            ENDIF
          ELSE
            WRITE(NSEO,FMT=FMTZON) TPS,(FLXC(ISEC),ISEC=1,NSEC)
          ENDIF
        ENDIF
!
!       ONLY SUSPENSION
!
        IF(SUSP.AND..NOT.CHARR) THEN
          IF(NCSIZE.GT.1) THEN
            DO ISEC=1,NSEC
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              DTMP1 = P_SUM(FLXS(ISEC))
              WORK(ISEC) = DTMP1
!##<JR @ ADJOINTWARE
            ENDDO
!           IN // ONLY PROCESSOR 0 WRITES THE FILE
            IF(IPID.EQ.0) THEN
              WRITE (NSEO,FMT=FMTZON) TPS,(WORK(ISEC),ISEC=1,NSEC)
            ENDIF
          ELSE
            WRITE(NSEO,FMT=FMTZON) TPS,(FLXS(ISEC),ISEC=1,NSEC)
          ENDIF
        ENDIF
!
!       BOTH BEDLOAD AND SUSPENSION
!
        IF(SUSP.AND.CHARR) THEN
          IF(NCSIZE.GT.1) THEN
            DO ISEC=1,NSEC
!##>JR @ ADJOINTWARE: ALGORITHMIC DIFFERENTIATION
              DTMP1 = P_SUM(FLXC(ISEC))
              WORK(ISEC) = DTMP1
              DTMP2 = P_SUM(FLXS(ISEC))
              WORKB(ISEC)= DTMP2
!##<JR @ ADJOINTWARE
            ENDDO
            IF(IPID.EQ.0) THEN
              WRITE (NSEO,FMT=FMTZON) TPS,(WORK(ISEC),ISEC=1,NSEC),
     &                                    (WORKB(ISEC), ISEC=1,NSEC)
            ENDIF
          ELSE
            WRITE (NSEO,FMT=FMTZON) TPS,(FLXC(ISEC),ISEC=1,NSEC),
     &                                  (FLXS(ISEC),ISEC=1,NSEC)
          ENDIF
        ENDIF
!
      ENDIF
!
!     IF(INFO)...
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE FLUXPR_GAIA
