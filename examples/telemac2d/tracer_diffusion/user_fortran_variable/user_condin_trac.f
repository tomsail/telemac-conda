!                   ***************************
                    SUBROUTINE USER_CONDIN_TRAC
!                   ***************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS TRAC
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
!
      DOUBLE PRECISION EIKON, X1, X2
!
      INTRINSIC EXP
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO IPOIN=1,NPOIN
!
          EIKON=((X(IPOIN)-10.0D0)**2)
!
!         Tracer 1:
          T%ADR(1)%P%R(IPOIN) = EXP(-0.5D0*EIKON)
!
!         Tracer 2:
          X1 = 7.50D0
          X2 = 12.50D0
          IF (X(IPOIN).GE.X1 .AND. X(IPOIN).LE.X2) THEN
            T%ADR(2)%P%R(IPOIN) = 1.D0
          ELSE
            T%ADR(2)%P%R(IPOIN) = 0.D0
          ENDIF
!
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
