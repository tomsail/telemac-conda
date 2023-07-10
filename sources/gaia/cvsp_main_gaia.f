!                   *************************
                    SUBROUTINE CVSP_MAIN_GAIA
!                   *************************
!
     &(ZFCL_W,ZF,NSICLA,NPOIN)
!
!***********************************************************************
! GAIA   V8P1                                            16/05/2017
!***********************************************************************
!
!>@brief    CONTINOUS VERTICAL SORTING MODEL
!!        COMPUTES FRACTIONS FOR EACH CLASS AND EACH SECTION OF A C-VSM;
!
!>@history  U.MERKEL (BAW), R.KOPMANN (BAW)
!!        01/06/2012
!!        V6P2
!
!>@history  P. A. TASSI (EDF R&D, LNHE)
!!        12/03/2013
!!        V6P3
!!   Cleaning, cosmetic
!
!>@history  R. Kopmann (BAW)
!!        27/01/2016
!!        V7P1
!!   Dimension of VOLTOT changed from 10 to NSICLA
!
!>@history  Uwe Merkel (UHM) R. Kopmann (BAW)
!!        23/11/2016, 2017
!!        V6P3, V7P2
!!   MANY CHANGES
!
!>@history  R. KOPMANN (BAW)
!!        25/02/2019
!!        V7P2
!!   calling cvsp_make_actlayer moved, removing empty sections
!!   synchronizing ZR and PRO_D(1), update synchro ZF and PRO_D(PRO_MAX)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] ZFCL_W Evolution for each sediment class
!>@param[in] ZF Bottom elevation
!>@param[in] NSICLA Number of grain classes (fractions)
!>@param[in] NPOIN Number of mesh points
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_GAIA_BEDLOAD, EX => CVSP_MAIN_GAIA
      USE DECLARATIONS_GAIA, ONLY: CVSMOUTPUT,CVSM_OUT,CVSM_OUT_FULL,
     &                                PRO_D,PRO_MAX,PRO_MAX_MAX,PERCOU,
     &                                HN,LT,DT,MESH,Z,PRO_F,ZR
!
      USE DECLARATIONS_SPECIAL
      USE CVSP_OUTPUTFILES_GAIA, ONLY: CP
      USE INTERFACE_PARALLEL, ONLY: P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZF
      INTEGER,          INTENT(IN)    :: NSICLA,NPOIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IAMCASE, ISICLA, JG
      LOGICAL RET
      INTEGER I,J,K,ARRET,ARRET2
      DOUBLE PRECISION DZFCL,EVL,AT,DELTA,SUMF
      INTEGER KK,PROMAX
!
!-----------------------------------------------------------------------
!
      ARRET=0
      AT = DT*LT/PERCOU
      CP = .FALSE. ! If true: A lot of debug prints!
!
!-----------------------------------------------------------------------
!     INITIAL CHECK FOR DEBUGGING
!-----------------------------------------------------------------------
!

      DO J=1,NPOIN
        DO K = 1, PRO_MAX(J)
          !REMOVES NUMERIC INSTABILITIES
          RET = CVSP_CHECK_F_GAIA(J,K,'INIT_CVSP_MAIN:  ')
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!     CHECK FOR RIGID BED ERRORS
!RK not needed anymore for gaia, only ES... calcultes the layer thickness
!-----------------------------------------------------------------------
      DO J=1,NPOIN
        IF(Z%R(J)-ZF%R(J).LT.0.D0) THEN
          IF(CP) WRITE(LU,*) 'UHM_Z.LT.ZF_BEF ',AT,Z%R(J),ZF%R(J),
     &                 HN%R(J),(Z%R(J)-ZF%R(J))-HN%R(J)
          CALL CVSP_P_GAIA('./','Z_', J)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!     FOR ALL POINTS AND FOR ALL CLASSES
!-----------------------------------------------------------------------
      DO J=1,NPOIN
        JG = J
        IF (NCSIZE.GT.1) JG = MESH%KNOLG%I(J)
        EVL = 0.D0
        DO ISICLA = 1,NSICLA
          EVL = ZFCL_W%ADR(ISICLA)%P%R(J) + EVL
        END DO
!
! DEBUG INFO
        IAMCASE = 0
        IF (CVSP_DB_GAIA(JG,0)) CALL CVSP_P_GAIA('./','V_A',JG)
! DEBUG INFO
!
!-----------------------------------------------------------------------
! ADD SECTION IF DEPOSITION IN SUM OVER ALL CLASSES
!-----------------------------------------------------------------------
        IF(EVL.GT.0) THEN
          CALL CVSP_ADD_SECTION_GAIA(J)
        ENDIF
!
        DO I=1,NSICLA
          DZFCL = ZFCL_W%ADR(I)%P%R(J)
          !START DEPOSITION IN SUM OVER ALL CASES
          IF (EVL.GT.0D0) THEN
            IF (DZFCL.GT.0.D0) THEN
              CALL CVSP_ADD_FRACTION_GAIA(J,I,DZFCL)
              IAMCASE = 1 + IAMCASE !DEBUG INFO
              !CHECK
              DO K = 1, PRO_MAX(J)
                RET =  CVSP_CHECK_F_GAIA(J,K,' EVL>0: A     ')
              ENDDO
            ELSEIF( DZFCL.LT.0.D0) THEN
              CALL CVSP_RM_FRACTION_GAIA(J,I,DZFCL)
              IAMCASE = 10 + IAMCASE !DEBUG INFO
              !CHECK
              DO K = 1, PRO_MAX(J)
                RET =  CVSP_CHECK_F_GAIA(J,K,' EVL>0: B     ')
              ENDDO
            ENDIF
          ENDIF
!-----------------------------------------------------------------------
! EROSION IN SUM OVER ALL CLASSES
!-----------------------------------------------------------------------
          IF(EVL.LT.0.D0) THEN
            IF (DZFCL.GT.0.D0) THEN
              CALL CVSP_ADD_FRACTION_GAIA(J,I,DZFCL)
              IAMCASE = 100 + IAMCASE !DEBUG INFO
            ELSEIF(DZFCL.LT.0.D0) THEN
              CALL CVSP_RM_FRACTION_GAIA(J,I,DZFCL)
              IAMCASE = 1000 + IAMCASE !DEBUG INFO
            ENDIF                      ! DZFCL
              !CHECK
              DO K = 1, PRO_MAX(J)
                IF(CVSP_CHECK_F_GAIA(J,K,' EVL<0:       ')
     &              .EQV..FALSE.)THEN
                  IF(CP) WRITE(LU,*)'--> CVSP CF Case: ',IAMCASE
                ENDIF
              ENDDO
          ENDIF! EVL < 0
        ENDDO !NSICLA
!
! REMOVING EMPTY SECTIONS
        DO K=2,PRO_MAX(J)
          PROMAX = PRO_MAX(J)
          SUMF = 0.D0
          DO I=1,NSICLA
            SUMF = SUMF + PRO_F(J,K,I)
          END DO
          IF(SUMF.EQ.0.D0) THEN
              PROMAX = PROMAX - 1
              DO KK=K,PROMAX
                DO I=1,NSICLA
                  PRO_D(J,KK,I) = PRO_D(J,KK+1,I)
                  PRO_F(J,KK,I) = PRO_F(J,KK+1,I)
                END DO
              END DO
          ENDIF
          IF(PROMAX.EQ.K) GOTO 999
        END DO !K=2,PRO_MAX(J)
999     PRO_MAX(J) = PROMAX

!-----------------------------------------------------------------------
! WE ARE RUNNING OUT OF SECTION MEMORY! COMPRESS NOW!
!-----------------------------------------------------------------------
        IF ((PRO_MAX(J).GT.PRO_MAX_MAX/4*3).OR.
     &       (PRO_MAX_MAX-PRO_MAX(J).LT.8*NSICLA)) THEN
              DO K = 1, PRO_MAX(J)
                !REMOVES NUMERIC INSTABILITIES
                RET =  CVSP_CHECK_F_GAIA(J,K,' Before DP:   ')
              ENDDO
              CALL CVSP_COMPRESS_DP_GAIA(J, 1.0D-5)
              DO K = 1, PRO_MAX(J)
                !REMOVES NUMERIC INSTABILITIES
                RET =  CVSP_CHECK_F_GAIA(J,K,' After DP:   ')
              ENDDO
        ENDIF
!-----------------------------------------------------------------------
! SYNCHRONICE VSP WITH LAYER (FOR DEBUGGING ...)
!-----------------------------------------------------------------------
        DELTA = ZF%R(J) - PRO_D(J, PRO_MAX(J), 1)
!
        IF (DELTA.NE.0.D0) THEN
          DO I = 1 , NSICLA
            DO K = 2, PRO_MAX(J)
              PRO_D(J, K, I) = PRO_D(J, K, I) + DELTA
            ENDDO
          ENDDO
! problem due to adopting ZF and Pro_d top most layer
! bottom most layer could be higher than second top most layer
! -> deleting last layer
          IF(PRO_D(J,2,1).LT.PRO_D(J,1,1)) THEN
            WRITE(LU,*) 'Problem bottom',PRO_D(J,2,1),PRO_D(J,1,1),
     & PRO_MAX(J)
            DO I=1,NSICLA
              DO K=PRO_MAX(J),2,-1
                PRO_D(J,K-1,I) = PRO_D(J,K,I)
                PRO_F(J,K-1,I) = PRO_F(J,K,I)
              END DO
            END DO
            PRO_MAX(J) = PRO_MAX(J)-1
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
!FINAL CHECK ON NEW FRACTIONS AND STEADY STADE
!-----------------------------------------------------------------------
        DO K = 1, PRO_MAX(J)
!         REMOVES NUMERIC INSTABILITIES
          RET =  CVSP_CHECK_F_GAIA(J,K,' FINAL:   ')
        ENDDO
        CALL CVSP_CHECK_STEADY_GAIA(J)
!
! END FOR ALL POINTS
      ENDDO
!
!-----------------------------------------------------------------------
! GENERATE NEW LAYERS FROM SORTING PROFILE
!-----------------------------------------------------------------------
!
!in make_actlay werden noch mal checks durchgefuehrt, die ggf.
! pro_f und pro_d veraendern... daher vor die Ausgabe gezogen!
!
      CALL CVSP_MAKE_ACTLAY_GAIA()
!
!-----------------------------------------------------------------------
! PRINT OUT SORTING PROFILE FOR SELECTED GLOBAL POINT NUMBERS!INSERT
!-----------------------------------------------------------------------
!
      IF((CVSM_OUT).OR.(CVSP_DB_GAIA(-1,-1).EQV..TRUE.)) THEN
! WRITES THE FULL VSP AS SERAFIN
        IF (CVSM_OUT_FULL) CALL CVSP_WRITE_PROFILE_GAIA()
! WRITES THE VSP FOR SINGLE POINTS
        DO KK = 1, 100
          IF (CVSMOUTPUT(KK).GT.0) THEN
            CALL CVSP_P_GAIA('./','V_', CVSMOUTPUT(KK))
          ENDIF
        ENDDO
      END IF
!
!-----------------------------------------------------------------------
! CHECK FOR RIGID BED ERRORS
!-----------------------------------------------------------------------
! -- this is not needed in Gaia because the rigid bed is not supported anymore
! instead only es is used
!
!-----------------------------------------------------------------------
! PRINT OUT NEW LAYERS FOR SELECTED GLOBAL POINT NUMBERS
!-----------------------------------------------------------------------
!
      IF((CVSM_OUT).OR.(CVSP_DB_GAIA(-1,-1).EQV..TRUE.)) THEN
        DO KK = 1,100
          IF (CVSMOUTPUT(KK).GT.0) THEN
            CALL LAYERS_P_GAIA('./','Pnt_', CVSMOUTPUT(KK))
          ENDIF
        ENDDO
      END IF
!
!-----------------------------------------------------------------------
!     CLEAN STOP FOR ALL PROCESSORS IF PROBLEM
!-----------------------------------------------------------------------
!
      ARRET2=ARRET
      IF(NCSIZE.GT.1) ARRET2=P_SUM(ARRET)
      IF(ARRET2.GT.0) THEN
        WRITE(LU,*) 'STOP AFTER AN ERROR IN LAYER'
        IF(ARRET.EQ.0) THEN
          WRITE(LU,*) 'IN ',ARRET2,' PROCESSOR(S)'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

