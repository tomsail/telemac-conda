!                   ****************************
                    SUBROUTINE USER_CONDI3D_TRAC
!                   ****************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES TRACER(S)
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_TRAC => USER_CONDI3D_TRAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I,J,IPLAN
!
!-----------------------------------------------------------------------
!
!     MODIFICATION FOR TIDAL TEST CASE, APPLY SUSP SEDIMENT
!     IN THE RIGHT HAND SIDE OF THE MODEL (WHERE THE INTERTIDALS ARE)
      DO I = 1,NPOIN2
        DO IPLAN = 1,NPLAN
          J = (IPLAN-1)*NPOIN2 + I
          IF (X(I).GT.15000.D0) THEN
            TA%ADR(1)%P%R(J) = 0.3D0
          ELSE
            TA%ADR(1)%P%R(J) = 0.D0
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
