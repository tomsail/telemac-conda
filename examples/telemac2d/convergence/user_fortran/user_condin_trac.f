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
      INTEGER ITRAC, I
      DOUBLE PRECISION PI, X_INI, Y_INI, L_BOX
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        PI = 4.D0*ATAN(1.D0)
        L_BOX = 200.D0
        DO I=1,NPOIN
          DO ITRAC=1,NTRAC
            X_INI = X(I)-10.D0
            Y_INI = Y(I)-10.D0
            T%ADR(ITRAC)%P%R(I) = (1.D0 + 8.D0*PI**2/L_BOX**2)
     &                          * SIN(2.D0*PI/L_BOX*X_INI)
     &                          * SIN(2.D0*PI/L_BOX*Y_INI)
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
