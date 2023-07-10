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
      DOUBLE PRECISION X0,Y0
      INTEGER I
!
!-----------------------------------------------------------------------
!
      IF(LEO.AND.SORG3D(25)) THEN
        DO I = 1,NPOIN3
          X0 = 10.05D0 + 4.95D0*COS(AT)
          Y0 = 10.05D0 + 4.95D0*SIN(AT)
          PRIVE1%R(I) = EXP(-0.5D0*((X(I)-X0)**2 + (Y(I)-Y0)**2))
        ENDDO
      ENDIF
!
      IF(LEO.AND.SORG2D(27)) THEN
        DO I = 1,NPOIN2
          X0 = 10.05D0 + 4.95D0*COS(AT)
          Y0 = 10.05D0 + 4.95D0*SIN(AT)
          PRIVE2D1%R(I) = EXP(-0.5D0*((X(I)-X0)**2 + (Y(I)-Y0)**2))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
