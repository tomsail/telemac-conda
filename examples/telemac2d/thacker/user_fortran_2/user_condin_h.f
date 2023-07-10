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
      DOUBLE PRECISION H00, R, A, L, R0, B_A
      DOUBLE PRECISION OMEGA
      INTEGER I
!
!-----------------------------------------------------------------------
!
!=====================================
! DEFINITION DES PARAMETRES A UTILISER
! ===================================
      H00 = 0.1D0
      A  = 1.0D0
      R0 = 0.8D0
      L  = 4.0D0
      OMEGA = SQRT(8.D0*GRAV*H00)/A

      B_A = (A**2 - R0**2)/(A**2 + R0**2)

!
      DO I = 1,NPOIN
        R = SQRT((X(I)-L/2.D0)**2 + (Y(I)-L/2.D0)**2)
!
        ! Water depth
        H%R(I) = MAX(
     &         H00*(SQRT(1.D0-B_A**2)/(1.D0-B_A*COS(OMEGA*AT)) -1.D0 -
     &     (R**2/A**2)*((1.D0-B_A**2)/(1.D0-B_A*COS(OMEGA*AT))**2 -1.D0)
     &             ) - ZF%R(I), 0.D0)
!
        ! Velocity u
        U%R(I) = (0.5D0*OMEGA*(X(I)-L/2.D0)*B_A*SIN(OMEGA*AT))
     &          /(1.D0-B_A*COS(OMEGA*AT))
!
        ! Velocity v
        V%R(I) = (0.5D0*OMEGA*(Y(I)-L/2.D0)*B_A*SIN(OMEGA*AT))
     &          /(1.D0-B_A*COS(OMEGA*AT))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
