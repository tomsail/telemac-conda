!                   ************************
                    SUBROUTINE USER_CONDIN_H
!                   ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    USER INITIALISES THE PHYSICAL PARAMETERS U, V
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
      DOUBLE PRECISION H00, A, L
      DOUBLE PRECISION ETA, OMEGA
      INTEGER I
!     DOUBLE PRECISION R
!
!-----------------------------------------------------------------------
!
!=====================================
! DEFINITION DES PARAMETRES A UTILISER
!=====================================
      H00 = 0.1D0
      A  = 1.0D0
      L  = 4.0D0
      OMEGA = SQRT(2.D0*GRAV*H00)/A
      ETA = 0.5D0
!
      DO I = 1,NPOIN
!       R = SQRT((X(I)-L/2.D0)**2 + (Y(I)-L/2.D0)**2)
!
        H%R(I) = MAX((ETA*H00/A**2)
     &         *( 2.D0*(X(I)-L/2.D0)*COS(OMEGA*AT) +
     &            2.D0*(Y(I)-L/2.D0)*SIN(OMEGA*AT) -
     &            ETA ) - ZF%R(I), 0.D0)
!
        U%R(I) = -ETA*OMEGA*SIN(OMEGA*AT)
!
        V%R(I) = ETA*OMEGA*COS(OMEGA*AT)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
