!                   *****************
                    SUBROUTINE DIRALE
!                   *****************
!
     &(DALE,EXPOS,TETAH,TETMIN,TETMAX,NDALE)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DISCRETISES A DIRECTIONAL ENERGY SPECTRUM IN
!+                NDALE BANDS OF EQUAL ENERGY. THE RESULT IS A
!+                LIST OF DIRECTIONS CORRESPONDING TO EACH BAND.
!+
!+      USES THE FORMULATION GIVEN BY GODA IN ' RANDOM SEAS AND
!+      DESIGN OF MARITIME STRUCTURES' - UNIVERSITY OF TOKYO PRESS
!+
!+      G = ( COS( (TETA-TETAH))/2 ) )**EXPOS
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
!+        V5P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DALE           |<--| DIRECTIONS FOR SPECTRUM DISCRETISATION
!| EXPOS          |-->| COEFFICIENT FOR THE SPECTRUM FORMULA
!| NDALE          |-->| NUMBER OF DISCRETISATION BAND
!| TETAH          |-->| MAIN DIRECTION OF THE PROPAGATION
!| TETMAX         |-->| MAXIMUM VALUE FOR THE PROPAGATION ANGLE
!| TETMIN         |-->| MAXIMUM VALUE FOR THE PROPAGATION ANGLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_ARTEMIS, ONLY: SPD
      USE DECLARATIONS_ARTEMIS, ONLY: EXPO
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NDALE
      DOUBLE PRECISION, INTENT(INOUT) :: DALE(NDALE)
      DOUBLE PRECISION, INTENT(IN) :: EXPOS,TETMIN,TETMAX,TETAH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPAS,I,K
!
      DOUBLE PRECISION DTETA,SUMB,VAR,SUMICI
!
!-----------------------------------------------------------------------
!
! EXPOS IS IN THE COMMON STATEMENT OF FUNCTION SPD (CANNOT BE
! CALLED EXPOS BECAUSE IT IS AN ARGUMENT OF THIS SUBROUTINE)
      EXPO = EXPOS
!
!-----------------------------------------------------------------------
!
!     NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD
      NPAS = 2000*NDALE
!
!     WIDTH OF AN INTEGRATION INTERVAL
      DTETA = (TETMAX-TETMIN)/FLOAT(NPAS)
!
!     SURFACE OF THE SPECTRUM
      SUMB = (SPD(TETMIN-TETAH) + SPD(TETMAX-TETAH))/2.D0
      DO I = 2,NPAS-1
        SUMB = SUMB + SPD(TETMIN-TETAH+FLOAT(I)*DTETA)
      ENDDO
!
!     DIVIDES THE SPECTRUM INTO 2*NDALE BANDS OF EQUAL ENERGY
      SUMB = SUMB/FLOAT(2*NDALE)
!
!     IDENTIFIES THE ANGLES EVERY (2*I-1)*SUMB (I=1,NDALE)
      SUMICI = SPD(TETMIN-TETAH)/2.D0
      I   = 1
      DO K=1,NPAS
        VAR = SPD(TETMIN-TETAH+DTETA*FLOAT(K))
        SUMICI = SUMICI + VAR/2.D0
        IF (SUMICI.GE.SUMB*FLOAT(2*I-1)) THEN
          DALE(I) =  TETMIN + DTETA*(FLOAT(K)-0.5D0)
          I = I + 1
          IF (I.GT.NDALE) RETURN
        ENDIF
        SUMICI = SUMICI + VAR/2.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
