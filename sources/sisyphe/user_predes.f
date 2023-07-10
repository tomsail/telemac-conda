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
!| LEO            |-->| IF TRUE WRITING IN OUTPUT FILE
!| IMP            |-->| IF TRUE WRITING IN LISTING
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
!-----------------------------------------------------------------------
!
      RETURN
      END
