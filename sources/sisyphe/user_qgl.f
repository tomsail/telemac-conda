!                   *******************
                    SUBROUTINE USER_QGL
!                   *******************
!
     &(QGL,I,AT)
!
!***********************************************************************
! SISYPHE
!***********************************************************************
!
!brief    USER PRESCRIBES THE SOLID DISCHARGE FOR  IMPOSED
!+                LIQUID BOUNDARIES.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER IN THE ORIGINAL MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_SPECIAL
!
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: QGL
      INTEGER, INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN):: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,101) I
101   FORMAT(1X,/,1X,'QG: MORE PRESCRIBED SOLID DISCHARGES '
     &         ,/,1X,'     ARE REQUIRED IN THE PARAMETER FILE'
     &         ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
      CALL PLANTE(1)
      STOP
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
