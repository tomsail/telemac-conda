!                   *******************
                    SUBROUTINE USER_CGL
!                   *******************
!
     &(CGL,I,AT)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V6P0
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size FCT due to modification of TRACER numbering
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
!      USE INTERFACE_SISYPHE, EX_CGL => CGL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: CGL
      INTEGER, INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN):: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      WRITE(LU,101) I
101   FORMAT(1X,/,1X,'USER_CG: MORE PRESCRIBED ELEVATIONS ARE REQUIRED'
     &         ,/,1X,'     IN THE PARAMETER FILE'
     &         ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
      CALL PLANTE(1)
      STOP
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
