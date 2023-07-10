!           *****************************
            RECURSIVE FUNCTION CVSP_CHECK_L_GAIA
!           *****************************
!
     &(J,K, SOMETEXT) RESULT(RET)
!
!***********************************************************************
! GAIA   V8P1                                   16/05/2017
!***********************************************************************
!
!>@brief   CHECKS IF SUM OF FRACTIONS = 1 FOR
!!        A LAYER
!
!>@history UWE MERKEL, R. KOPMANN (BAW)
!!        19/08/2016 / 2017
!!        V6P3 / V7P2
!
!>@history  R. KOPMANN (BAW)
!!        25/02/2019
!!        V7P2
!!   Removing 1/NSICLA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!>@param[in] J Index of a point in mesh
!>@param[in] K Index of a layer
!>@param[in] SOMETEXT Debuging text for log-output
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: NCSIZE
      USE BIEF
      USE DECLARATIONS_GAIA
      USE CVSP_OUTPUTFILES_GAIA, ONLY: CP
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: K
      CHARACTER(LEN=10),INTENT(IN) :: SOMETEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION ASUM
      INTEGER I, JG
      LOGICAL RET
!
!-----------------------------------------------------------------------
!
      JG = J
      IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
!
      RET = .TRUE.

      ASUM = 0.D0
!
!-----------------------------------------------------------------------
!SUM UP AND SLIGHT CORRECTION
!-----------------------------------------------------------------------
!
      DO I=1,NSICLA
        ASUM = AVAIL(J,K,I) + ASUM
        IF ((AVAIL(J,K,I)>1.D0+ZERO)) THEN
          IF(CP) WRITE(LU,*)
     &      'CVSP CL: AVAIL>1: WARN,LT,Pnt_J;Lay_K;F_I;%: '
     &      ,SOMETEXT,LT,JG,K,I,PRO_F(J,K,I)
        ENDIF
!
        IF ((AVAIL(J,K,I)<0.D0-ZERO)) THEN
          IF(CP) WRITE(LU,*)
     &      'CVSP CL: AVAIL<0: WARN,LT,Pnt_J;Lay_K;F_I;%: '
     &      ,SOMETEXT,LT,JG,K,I,PRO_F(J,K,I)
        ENDIF
      ENDDO
!
      IF (ABS(ASUM-1.0D0)>ZERO.AND.ASUM.GT.0.D0) THEN
        IF ((ABS(ASUM-1.0D0)>1.D-6).AND.(ASUM.GT.0.D0)) THEN
          DO I=1,NSICLA
            AVAIL(J,K,I) = AVAIL(J,K,I) / ASUM
          END DO
          IF (LT.GT.0) THEN
            IF(CP) WRITE(LU,*)
     &          'CVSP CL: |SUM_ERR|: WARN,LT,J;K;SUM: '
     &          ,SOMETEXT,LT,JG,K,ASUM
          RET = .FALSE.
          ENDIF
        ENDIF
      END IF


      IF (ABS(ASUM)<ZERO.AND.ASUM.GT.0.D0) THEN
!         WRITE(LU,*) 'CVSP CL: |SUM_ZERO|: WARN,LT,Pnt_J;Lay_K;F_I;SUM:'
!     &                  ,SOMETEXT,LT,JG,K,I,ASUM
        DO I=1,NSICLA
            AVAIL(J,K,I) = 0.D0
        END DO
        RET = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!
        IF (RET.EQV..FALSE.) THEN
          RET = CVSP_CHECK_L_GAIA(J,K,'ReCheck   ')
        ENDIF
!
!-----------------------------------------------------------------------
      RETURN
      END FUNCTION CVSP_CHECK_L_GAIA
