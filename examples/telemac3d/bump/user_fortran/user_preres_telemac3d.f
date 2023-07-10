!                   ********************************
                    SUBROUTINE USER_PRERES_TELEMAC3D
!                   ********************************
     &(LEO)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  J-M HERVOUET (LNH)
!+        30/03/04
!+        V5P7
!+
!
!history  Y. AUDOUIN (LNHE)
!+        22/10/18
!+        V8P1
!+   Creation from PRERES_TELEMAC3D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LEO            |-->| IF TRUE WRITING IN OUTPUT FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_USER_PRERES_TELEMAC3D =>
     &                            USER_PRERES_TELEMAC3D
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: LEO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!=======================================================================
! COMPUTE THE EXACT WATER DEPTH AND VELOCITY
!=======================================================================
!
      IF((LEO.AND.SORG2D(27)).OR.(LEO.AND.SORG2D(28))) THEN
        CALL EXACTE(PRIVE2D%ADR(1)%P%R,PRIVE2D%ADR(2)%P%R,ZF%R,
     &              MESH2D%X%R,NPOIN2,1)
      ENDIF
!
!=======================================================================
! COMPUTE THE EXACT FREE SURFACE
!=======================================================================
!
      IF(LEO.AND.SORG2D(29)) THEN
        CALL OV( 'X=Y+Z   ' ,PRIVE2D%ADR(3)%P%R,
     &                       PRIVE2D%ADR(1)%P%R,ZF%R,0.D0,NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
