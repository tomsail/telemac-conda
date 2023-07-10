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
      DOUBLE PRECISION X0, Y0
      DOUBLE PRECISION OMEGA
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF((SORLEO(23).OR.SORIMP(23))) THEN
        OMEGA = 1.0D0
!
        DO I = 1,NPOIN
          X0 = 10.0D0 + 5.0D0*COS(OMEGA*AT)
          Y0 = 10.0D0 + 5.0D0*SIN(OMEGA*AT)
!
          ! Tracer
          PRIVE1(I) = EXP(-0.2D0*((X(I)-X0)**2 + (Y(I)-Y0)**2))
!
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
