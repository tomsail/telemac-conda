!                   ***************************
                    SUBROUTINE CVSP_MAKE_ACTLAY
!                   ***************************
!
!
!***********************************************************************
! SISYPHE   V7P2                                   16/05/2017
!***********************************************************************
!
!brief  BUILD A NEW ACTIVE LAYER WITH DATA FROM VERTICAL SORTING PROFILE
!+      AND A NEW ACTIVE STRATUM WITH DATA FROM VERTICAL SORTING PROFILE
!
!
!history U.MERKEL & REBEKKA KOPMANN
!+        2012
!+        V6P2
!+
!
!history PABLO TASSI PAT (EDF-LNHE)
!+        12/02/2013
!+        V6P3
!+ PREPARING FOR THE USE OF A HIGHER NSICLM VALUE
!+ (BY REBEKKA KOPMANN)
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!history  UWE MERKEL (UHM), R. KOPMANN (BAW)
!+        2016 / 2017
!+        V6P3 / V7P2
!+   Robustnes improved
!
!history  R. KOPMANN (BAW)
!+        19/02/2019
!+        V7P2
!+   total volume calculated directly with cvsp variables
!
!history  R. KOPMANN (BAW)
!+        25/02/2019
!+        V7P2
!+   inserting checking mass per class, removing 1/NSICLA
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELAY0              |--| ALTHICKNESS INIT
!| AVAIL(J,K,I)       |--| FRACTIONS
!| ELAY%R(J)          |--| ALTHICKNESS REAL FOR POINT(J)
!| ESTRAT%R(J)        |--| ASTHICKNESS REAL FOR POINT(J)
!| ES(J,K)            |--| LAYERTHICKNESS FOR POINT(J) / LAYER(K)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE BIEF_DEF
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES, ONLY: CP
      USE INTERFACE_PARALLEL, ONLY : P_DSUM

      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  I,J,K,JG, II
      DOUBLE PRECISION TEMP, Z_HIGH, Z_LOW, CVSP_INTEGRATE_VOLUME, AT
      DOUBLE PRECISION ASUM, NEW_ALT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, EXTERNAL :: CVSP_ALT
      DOUBLE PRECISION SUMES, SUMAV, ZSPACE
!
!-----------------------------------------------------------------------
! TIME
!-----------------------------------------------------------------------
      AT = DT*LT/PERCOU
      CALL CVSP_CHECK_ANYTHING()
!-----------------------------------------------------------------------
! CHECKING MASS AND CORRECTING
!-----------------------------------------------------------------------
      CALL  CVSP_CHECK_MASS_BILAN()
!-----------------------------------------------------------------------
! NODES LOOP
!-----------------------------------------------------------------------
      DO J=1,NPOIN
        JG = J
        IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
!-----------------------------------------------------------------------
! CALCULATING ACTIVE LAYER THICKNESS
!-----------------------------------------------------------------------
        ZSPACE = ZF%R(J)-ZR%R(J)
        NLAYER%I(J) = NOMBLAY
        NEW_ALT =  MIN( ZSPACE, CVSP_ALT(J,ALT_MODEL))
!
!-----------------------------------------------------------------------
! CALCULATING ES PER LAYER
!-----------------------------------------------------------------------
        ES(J,1) = NEW_ALT
        SUMES = ES(J,1)
        DO K=2,NOMBLAY-1
          IF ((ZSPACE-SUMES).GE.ELAY0) THEN
            ES(J,K) = ELAY0
          ELSE
            ES(J,K) = ZSPACE-SUMES
          ENDIF
          SUMES = SUMES + ES(J,K)
        END DO
        ES(J,NOMBLAY) = ZSPACE-SUMES
        SUMES = SUMES + ES(J,NOMBLAY)

!
        IF(ABS(ZSPACE-SUMES).GT.ZERO) THEN
          WRITE(LU,*)'ERR in MAKE_ACTLAY',J,(ES(J,K),K=1,NOMBLAY),
     &                SUMES,ZSPACE,ZF%R(J),ZR%R(J)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
! CALCULATING AVAIL PER LAYER
!-----------------------------------------------------------------------
        SUMES = 0.D0
        DO K=1,NOMBLAY
          SUMES = SUMES + ES(J,K)
          IF (ES(J,K).GT.0.D0) THEN
            !DEPTH COORDINATES OF NEW ES: Z_HIGH & Z_LOW
            IF (K == 1) THEN
              Z_HIGH = PRO_D(J,PRO_MAX(J),1)
            ELSE
              Z_HIGH = -SUMES + PRO_D(J,PRO_MAX(J),1) + ES(J,K)
            ENDIF
            Z_LOW =  Z_HIGH - ES(J,K)
            !FETCH IT
            TEMP = CVSP_INTEGRATE_VOLUME(J,1,Z_HIGH,Z_LOW,T1%R)
            !ASSIGN IT
            ASUM = 0.D0
            DO I=1,NSICLA
              AVAIL(J,K,I) = T1%R(I) / (Z_HIGH-Z_LOW)
              ASUM = AVAIL(J,K,I) + ASUM
            ENDDO
!
! DEBUG
            IF (ABS(ASUM-1.D0).GT.1.0D-7.AND.ASUM.GT.0.D0.AND.CP) THEN
              WRITE(LU,*)'ERR in MAKE_ACTLAY',ES(J,1),ES(J,2),
     &                   ES(J,3),SUMES,ZSPACE,ZF%R(J),ZR%R(J)
              WRITE(LU,*)'ASUM in MAKE_ACTLAY',J,K,ASUM, Z_HIGH,
     &                    Z_LOW
              DO I=1,NSICLA
                WRITE(LU,*)' AVAIL,T1',I, AVAIL(J,K,I), T1%R(I)
              ENDDO
            ENDIF
          ELSE !DUMMY VALUES FOR EMPTY LAYERS!!!
            DO I=1,NSICLA
              AVAIL(J,K,I) = 0.D0
            ENDDO
          ENDIF
        ENDDO
        ELAY%R(J) = ES(J,1)
        ESTRAT%R(J) = ES(J,2)
      ENDDO ! J
!-----------------------------------------------------------------------
! CHECKS, EXPENSIVE! One might try to remove this one day!
!-----------------------------------------------------------------------
      DO J=1,NPOIN
        ZSPACE = ZF%R(J)-ZR%R(J) ! Available Space
        SUMES = 0.D0             ! Sum of layer thicknesses
        JG = J
        IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
!       LOOP ALL LAYERS
        DO K=1,NLAYER%I(J) ! = NOMBLAY-NUMBER OF EMPTY LAYERS
          IF (ES(J,K).LT.0.D0) THEN
            IF (ES(J,K).GT.0.D0 - ZERO) THEN
              ES(J,K) = ZERO
            ELSE
              WRITE(LU,*) 'Lethal_ERROR ES',J,K,I,AVAIL(J,K,I)
              CALL CVSP_P('./','ERR_ES_VSP_',  JG)
              CALL LAYERS_P('./','ERR_ES_LAY_', JG)
              CALL PLANTE(1)
            ENDIF
          END IF
          IF (K.EQ.1) ELAY%R(J) = ES(J,K)
          IF (K.EQ.2) ESTRAT%R(J) = ES(J,K)
          SUMES = SUMES+ES(J,K)
!         LOOP ALL FRACTIONS
          SUMAV = 0.D0
          DO I=1,NSICLA
            !STOP DUE TO WRONG FRACTION CALCULATION
            IF((AVAIL(J,K,I) .GT. 1.D0 + ZERO)  .OR.
     &         (AVAIL(J,K,I) .LT. 0.D0 - ZERO)) THEN
              WRITE(LU,*) 'Lethal_ERROR AVAIL',J,K,I,AVAIL(J,K,I)
              CALL CVSP_P('./','ERR_AVAIL_VSP_',  JG)
              CALL LAYERS_P('./','ERR_AVAIL_LAY_', JG)
              CALL PLANTE(1)
            ENDIF
!           ONLY TRUNCATION ERROR
            IF (AVAIL(J,K,I) .LT. 0.D0) AVAIL(J,K,I) = 0.0D0
            IF (AVAIL(J,K,I) .GT. 1.D0) AVAIL(J,K,I) = 1.0D0
            SUMAV=AVAIL(J,K,I)+SUMAV
          ENDDO !I = Fractions
!
!         ELIMINTATION OF TRUNCATION ERRORS
          IF(ABS(SUMAV-1.D0).GT.ZERO.AND.SUMAV.GT.0.D0) THEN
            IF(CP) WRITE(LU,*) 'ERR SUMAV:J,K,SUMAV<>1:',J,K,SUMAV
            DO II=1,NSICLA
              AVAIL(J,K,II) = AVAIL(J,K,II) / SUMAV
            ENDDO
          ENDIF
        ENDDO !K = LAYERS

!       FINAL DEPTH CHECK
        IF( ABS(SUMES-ZSPACE).GT.ZERO) THEN
          WRITE(LU,*) 'ERROR SUM ES',J,SUMES,ZSPACE,NLAYER%I(J)
          CALL CVSP_P('./','ERR_SUM_ES_VSP_', JG)
          CALL LAYERS_P('./','ERR_SUM_ES_LAY_', JG)
          CALL PLANTE(1)
        ENDIF
      ENDDO ! J  = NODES
!
!     COMPUTES THE TOTAL VOLUME OF SEDIMENTS IN THE DOMAIN
      DO I = 1, NSICLA
        VOLTOT(I) = 0.D0
      ENDDO
!
      DO I=1,NSICLA
        DO J=1,NPOIN
          DO K=1,PRO_MAX(J)-1
            VOLTOT(I) = VOLTOT(I) + (PRO_F(J,K,I)+PRO_F(J,K+1,I))/2.D0
     &          *(PRO_D(J,K+1,I)-PRO_D(J,K,I))*VOLU2D%R(J)
          ENDDO
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_MAKE_ACTLAY
