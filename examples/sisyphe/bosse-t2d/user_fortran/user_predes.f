!                   **********************
                    SUBROUTINE USER_PREDES
!                   **********************
!
     &(LLT,AAT,YAGOUT,CODE,LEO,IMP)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    USER PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AAT            |-->| CURRENT TIME (FOR BUILDING SOLUTIONS)
!| CODE           |-->| NAME OF CALLING PROGRAMME (TELEMAC2D OR 3D)
!| LLT            |-->| LOCAL LT (MAY BE LT-1+PERCOU)
!| YAGOUT         |-->| LOGICAL: IF YES GRAPHIC OUTPUT ANYWAY
!|                |   | (STEERED BY T2D)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: LLT
      DOUBLE PRECISION , INTENT(IN) :: AAT
      CHARACTER(LEN=24), INTENT(IN) :: CODE
      LOGICAL          , INTENT(IN) :: YAGOUT
      LOGICAL          , INTENT(IN) :: LEO
      LOGICAL          , INTENT(IN) :: IMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
!  SOLUTION ANALYTIQUE POUR LE FOND (PREMIER TABLEAU PRIVE)
!
      IF(LEO.OR.IMP) THEN
        CALL CARACTERISTIQUE(MESH%X%R,MESH%Y%R,NPOIN,
     &                       PRIVE%ADR(1)%P%R,AAT)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
