!                   ******************
                    SUBROUTINE RELAXMU
!                   ******************
     &(ECRHMU,MODHMU,ITERMU)
!
!
!***********************************************************************
! ARTEMIS   V7P0                                   06/2014
!***********************************************************************
!
!brief    COMPUTES THE NEW DISSIPATION COEFFICIENT
!+                USING RELAXATION METHOD
!
!history  C PEYRARD (LNHE)
!+        27/03/2014
!+        V7P0
!+        NEW SUBROUTINE CREATED / IMPLEMENTED
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ITERMU             |-->| INDICE OF THE CURRENT CALCULATION
!| ECRHMU             |-->| ERROR ON WAVE HEIGHT BETWEEN 2 ITERATIONS
!| MODHMU             |-->| MODULE OF WAVE HEIGHT FOR THE CURRENT ITER.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER, INTENT(INOUT) :: ITERMU
      DOUBLE PRECISION, INTENT(INOUT) :: ECRHMU,MODHMU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      INTEGER I
!-----------------------------------------------------------------------
!
      INTRINSIC ABS,MAX
!
!----------------------------------------------------------------------
!
!     -------------------------------------------------------
!     RELAXATION ON MU2 TO TRY AND AVOID OSCILLATIONS IN THE
!     CONVERGENCE OF THE SOLVEUR
!     -------------------------------------------------------
!
!
      ECRHMU=0D0
      MODHMU = 1.D-9
      DO I = 1,NPOIN
!       ----------
!       RELAXATION
!       ----------
!
        MU2%R(I) = MU%R(I) + RELDIS * (MU2%R(I) - MU%R(I))
        IF(ITERMU.EQ.0) THEN
          HMUANC%R(I) = HMU%R(I)
          ECRHMU = 1.D0
          MODHMU = 1.D0
          MU%R(I) = MU2%R(I)
        ELSE
          ECRHMU = MAX(ECRHMU,ABS(HMU%R(I)-HMUANC%R(I)))
          MODHMU = MAX(MODHMU,ABS(HMU%R(I)))
          MU%R(I) = MU2%R(I)
          HMUANC%R(I) = HMU%R(I)
        ENDIF
      ENDDO
!
!     RELAXES THE RELAXATION AT EACH SUB-ITERATION
!     TO FACILITATE CONVERGENCE OF THE ALGORITHM USED TO
!     COMPUTE DISSIPATION (REGULAR WAVES)
!
      IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
        RELDIS = RELDIS * 0.85D0
      ENDIF
!
      IF (NCSIZE .GT. 1) THEN
        ECRHMU = P_MAX(ECRHMU)
        MODHMU = P_MAX(MODHMU)
      END IF
      WRITE(LU,*) 'DIFF. BETWEEN TWO
     &             SUB-ITERATIONS (%) ',
     &             100*ECRHMU/MODHMU
      ITERMU = ITERMU + 1
!
!
!     -----------------------------------------------------------
!     IF NUMBER OF SUB-ITERATIONS FOR MU >= MAX NUMBER OF SUB-ITERATIONS
!     EXITS THE LOOP OVER MU AND SETS THE RELATIVE DIFFERENCE
!     ECRHMU/MODHMU TO 10 % OF EPSDIS
!     -----------------------------------------------------------
!
      IF(ITERMU.GE.NITDIS) THEN
        WRITE(LU,101) ITERMU
 101    FORMAT(/,1X,'BERKHO (ARTEMIS): YOU REACHED THE MAXIMUM',
     & 1X,'NUMBER OF SUB-ITERATIONS :)',1X,I3)
            ECRHMU = EPSDIS*MODHMU/10.D0
      ENDIF
!
      RETURN
      END SUBROUTINE
