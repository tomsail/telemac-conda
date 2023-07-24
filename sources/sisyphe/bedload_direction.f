!                   **************************************
                    SUBROUTINE BEDLOAD_DIRECTION ! (_IMP_)
!                   **************************************
!
     &  (QU, QV, NPOIN, PI, THETAC)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE THETAC ANGLE (FLOW DIRECTION).
!
!history  C. VILLARET
!+        01/10/2003
!+        V5P4
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS
!| PI             |-->| PI
!| QU             |-->| DISCHARGE X
!| QV             |-->| DISCHARGE Y
!| THETAC         |<->| CURRENT ANGLE TO THE X AXIS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_DIRECTION => BEDLOAD_DIRECTION
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),  INTENT(IN)  :: QU, QV
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: PI
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: THETAC
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION, PARAMETER :: LOCAL_ZERO = 1.D-6
!
!======================================================================!
!======================================================================!
!                    DECLARES TYPES AND DIMENSIONS                     !
!======================================================================!
!======================================================================!
      DO I = 1, NPOIN
        IF (ABS(QU%R(I)) <= LOCAL_ZERO) THEN
          IF (QV%R(I) < = LOCAL_ZERO) THEN
            THETAC%R(I) = -PI*0.5D0
          ELSE
            THETAC%R(I) =  PI*0.5D0
          ENDIF
        ELSE
          THETAC%R(I) = ATAN(QV%R(I) / QU%R(I))
          IF (QU%R(I) < 0.D0) THEN
            THETAC%R(I) = PI + THETAC%R(I)
          ENDIF
        ENDIF
      END DO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_DIRECTION
