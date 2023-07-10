!                   *****************************
                    RECURSIVE FUNCTION CVSP_CHECK_F
!                   *****************************
!
     &(J,K, SOMETEXT) RESULT(RET)
!
!***********************************************************************
! SISYPHE   V7P2                                   16/05/2017
!***********************************************************************
!
!brief   CHECKS IF SUM OF FRACTIONS = 1 FOR
!+        A SECTION IN THE VERTICAL SORTING PROFILE
!
!history UWE MERKEL
!+        19/08/2011
!+        V6P2
!+
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!history UWE MERKEL, R. KOPMANN (BAW)
!+        19/08/2016 / 2017
!+        V6P3 / V7P2
!+        many changes!
!
!history R. KOPMANN (BAW)
!+        25/02/2019
!+        V7P2
!+        inserting check PRO_F>1, removing 1/NSICLA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX OF A POINT IN MESH
!| K              |<--| INDEX OF A SECTION IN VERTICAL SORTING PROFILE
!| SOMETEXT       |<--| DEBUGING TEXT FOR LOG-OUTPUT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY : NCSIZE
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES, ONLY: CP
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: J
      INTEGER,          INTENT(IN) :: K
      CHARACTER(LEN=10),INTENT(IN) :: SOMETEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION TEMP, ERRTOCORR
      INTEGER I, JG
      LOGICAL RET
!-----------------------------------------------------------------------
      JG = J
      IF (NCSIZE > 1) JG = MESH%KNOLG%I(J)
      RET = .TRUE.
      TEMP = 0.D0
!-----------------------------------------------------------------------
!SUM UP AND SLIGHT CORRECTION
!-----------------------------------------------------------------------
      DO I=1,NSICLA
        IF (PRO_F(J,K,I).LT.0.D0) THEN
          IF (PRO_F(J,K,I).LE.-1.D-7.AND.CP) WRITE(LU,*)
     &     'CVSP CF:PRO_F<0: WARN,J;K;F_I;%: ',
     &     SOMETEXT,JG,K,I,PRO_F(J,K,I)
          IF(PRO_F(J,K,I).GE.-1.D-3) THEN
            PRO_F(J,K,I) = 0.D0
          ELSE
            CALL CVSP_P('./','PRO_F.lt'//SOMETEXT, J)
            WRITE(LU,*) 'CVSP CF:PRO_F<0: ERR,LT,J;K;F_I;%: '
     &                 ,SOMETEXT,LT,JG,K,I,PRO_F(J,K,I)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!RK PRO_F > 1
        IF (PRO_F(J,K,I).GT.1.D0) THEN
          IF ((1.D0-PRO_F(J,K,I)).LE.-1.D-7.AND.CP) WRITE(LU,*)
     &     'CVSP CF:PRO_F>1: WARN,J;K;F_I;%: ',
     &     SOMETEXT,JG,J,K,I,PRO_F(J,K,I)
!          IF((1.D0-PRO_F(J,K,I)).GE.-1.D-3) THEN
! This barrier is quite low, as it is a numerical problem in rm_fraction
! if a fraction of value nearly 1 is removed than the "normalising procedure" is not very
! precise as it is diveded by (1-reomoved fraction) which can be nearly zero...
          IF((1.D0-PRO_F(J,K,I)).GE.-1.D0) THEN
            PRO_F(J,K,I) = 1.D0
          ELSE
            CALL CVSP_P('./','PRO_F.gt'//SOMETEXT, J)
            WRITE(LU,*) 'CVSP CF:PRO_F>1: ERR,LT,J;K;F_I;%: '
     &                 ,SOMETEXT,LT,JG,K,I,PRO_F(J,K,I),PRO_MAX(J)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        TEMP = TEMP + PRO_F(J,K,I)
      ENDDO
! ALL FRACTION ZERO IS OK, IN CVSP_MAIN this section will be deleted
      IF(TEMP.EQ.0.D0) THEN
        TEMP = 1.D0
      ENDIF
!-----------------------------------------------------------------------
! CHECK AND CORRECT DEVIATIONS
!-----------------------------------------------------------------------
      IF(ABS(TEMP-1.D0).GT.0.D0) THEN
        IF(ABS(TEMP).LT.1.D-6) THEN
!         SEVERE ERROR, FOR DEBUGGING ONLY RESET to 1 / NSICLA
          IF(CP) WRITE(LU,*) 'CVSP CF: |SUM_ERR|~0;LT;J;K;SUM:'
     &                  ,SOMETEXT,LT,JG,K,TEMP
          RET = .FALSE.
          IF(CP) WRITE(LU,*) 'CVSP  --> NSICLA: ', NSICLA
          DO I=1,NSICLA
            IF(CP) WRITE(LU,*) 'CVSP  --> ;LT;Pnt_J;Lay_K;F_I,%: '
     &                 ,LT,JG,K,I,PRO_F(J,K,I)
            PRO_F(J,K,I) = 0.D0
          ENDDO
        ELSEIF(ABS(TEMP-1.D0).GT.1.D-6) THEN
!STRONG DIFFERENCES ARE CORRECTED BY NORMALIZING ALL FRACTIONS
!!!!!!!!!!! README!
!The following warning occured in 0.00025 of all cases
!In almost every case |SUM_ERR| < 2*1.D-5
!To remove this remaining errors would cost 2-3 times higher
!computational expense with no significant global effects
!So the following warning is just meant to remember you on truncation errors
!that still exist
          IF(ABS(TEMP-1.D0).GT.5.D-5) THEN
            IF(CP) WRITE(LU,*) 'CVSP CF: |SUM_ERR|>1.-5 ;LT;J;K;SUM:'
     &                  ,SOMETEXT,LT,JG,K,TEMP
          ENDIF
          RET = .FALSE.
          DO I=1,NSICLA
            IF(PRO_F(J,K,I).GT.0.D0) THEN
              PRO_F(J,K,I) = PRO_F(J,K,I) / TEMP
            ENDIF
          ENDDO
        ELSE
! SLIGHT DIFFERENCES TO 0 ARE CORRECTED BY CHANGING ONLY
! THE FIRST FRACTION BIG ENOUGH
          ERRTOCORR = 1.D0-TEMP
          DO I=1,NSICLA
            IF(PRO_F(J,K,I)+ERRTOCORR.GT.0.D0.AND.
     &        PRO_F(J,K,I)+ERRTOCORR.LE.1.D0 ) THEN
              PRO_F(J,K,I) = PRO_F(J,K,I) + ERRTOCORR
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
! RECHECK
!-----------------------------------------------------------------------
      IF(RET .EQV. .FALSE.) THEN
        RET = CVSP_CHECK_F(J,K,'ReCheck   ')
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END FUNCTION CVSP_CHECK_F
