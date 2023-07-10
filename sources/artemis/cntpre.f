!                   *****************
                    SUBROUTINE CNTPRE
!                   *****************
!
     &(DAM,NPOIN,IPRECO,IPREC2)
!
!***********************************************************************
! ARTEMIS   V7P0
!***********************************************************************
!
!brief    INHIBITS THE DIAGONAL PRECONDITIONING IF ONE OF THE
!+                ELEMENTS OF DAM IS NEGATIVE OR ZERO.
!code
!+  --------------------------------------------------------------------
!+ |  VALUE OF IPREC2   I                  MEANING
!+ |    OR IPRECO       I
!+  --------------------------------------------------------------------
!+ |       1            I  NOTHING.
!+ |       2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!+ |                    I  DIAGONAL.
!+ |       3            I  DIAGONAL PRECONDITIONING USING THE CONDENSED
!+ |                    I  MATRIX.
!+ |       5            I  OTHER (NOT DEFINED)
!+ |                    I
!+ |       7            I  CROUT PRECONDITIONING BY ELEMENT
!+ |                    I  (NOT CODED IN)
!+  --------------------------------------------------------------------
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        23/02/2015
!+        V7P0
!+   A parallel communication is necessary when the preconditioning
!+   is changed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DAM            |-->| DIAGONALE OF THE MATRIX
!| IPREC2         |<--| PRECONDITIONNING USED
!| IPRECO         |-->| PRECONDITIONNING REQUIRED BY USER
!| NPOIN          |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(INOUT) :: IPRECO, IPREC2
      DOUBLE PRECISION, INTENT(IN) :: DAM(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      IF (IPRECO.EQ.0) IPRECO = 1
      IPREC2 = IPRECO
!
      IF (MOD(IPRECO,2).EQ.0.OR.MOD(IPRECO,3).EQ.0) THEN
!
        DO I=1,NPOIN
          IF (DAM(I).LE.0.D0) THEN
            DO WHILE(MOD(IPREC2,2).EQ.0)
              IPREC2 = IPREC2/2
            ENDDO
            DO WHILE(MOD(IPREC2,3).EQ.0)
              IPREC2 = IPREC2/3
            ENDDO
            WRITE(LU,101)
101     FORMAT(1X,'CNTPRE (ARTEMIS) : DIAGONAL SCALING NOT APPLIED (ONE
     &COEFFICIENT OF THE MATRIX DIAGONAL IS NEGATIVE OR ZERO)')
            EXIT
          ENDIF
        ENDDO
!
      ELSEIF (MOD(IPRECO,5).EQ.0) THEN
!
        DO I=1,NPOIN
          IF (ABS(DAM(I)).LE.1.D-6) THEN
            DO WHILE(MOD(IPREC2,5).EQ.0)
              IPREC2 = IPREC2/5
            ENDDO
            WRITE(LU,201)
201     FORMAT(1X,'CNTPRE (ARTEMIS) : DIAGONAL SCALING NOT APPLIED (ONE
     &COEFFICIENT OF THE MATRIX DIAGONAL IS ZERO)')
            EXIT
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IPREC2 MAY HAVE BEEN CHANGED IN ANOTHER PROCESSOR, BUT ALWAYS
!     IN THE SENSE OF A REDUCTION
!
      IF(NCSIZE.GT.1) IPREC2=P_MIN(IPREC2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

