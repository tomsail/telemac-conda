!                   ********************************
                    SUBROUTINE USER_PRERES_TELEMAC2D
!                   ********************************
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    PREPARES THE USER VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_USER_PRERES_TELEMAC2D
     &                         => USER_PRERES_TELEMAC2D

!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION H00, R, A, L, R0, B_A
      DOUBLE PRECISION ETA, OMEGA
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF((SORLEO(23).OR.SORIMP(23))) THEN
        H00 = 0.1D0
        A  = 1.0D0
        R0 = 0.8D0
        L  = 4.0D0
        OMEGA = SQRT(8.D0*GRAV*H00)/A
        ETA = 0.5D0

        B_A = (A**2 - R0**2)/(A**2 + R0**2)

!
        DO I = 1,NPOIN
          R = SQRT((X(I)-L/2.D0)**2 + (Y(I)-L/2.D0)**2)
!
          ! Water depth
          PRIVE1(I) = MAX(
     &         H00*(SQRT(1.D0-B_A**2)/(1.D0-B_A*COS(OMEGA*AT)) -1.D0 -
     &     (R**2/A**2)*((1.D0-B_A**2)/(1.D0-B_A*COS(OMEGA*AT))**2 -1.D0)
     &             ) - ZF%R(I), 0.D0)
!
          ! Velocity u
          PRIVE2(I) = (0.5D0*OMEGA*(X(I)-L/2.D0)*B_A*SIN(OMEGA*AT))
     &               /(1.D0-B_A*COS(OMEGA*AT))
!
          ! Velocity v
          PRIVE3(I) = (0.5D0*OMEGA*(Y(I)-L/2.D0)*B_A*SIN(OMEGA*AT))
     &               /(1.D0-B_A*COS(OMEGA*AT))

          ! Elevation
          PRIVE4(I) = PRIVE1(I) + ZF%R(I)

        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
