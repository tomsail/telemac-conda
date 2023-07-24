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
!
!-----------------------------------------------------------------------
!
      RETURN
      END
