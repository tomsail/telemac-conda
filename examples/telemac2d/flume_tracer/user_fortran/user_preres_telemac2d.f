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
      USE INTERFACE_PARALLEL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION X0, X1, X2, STATIONNARY_VELOCITY
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF((SORLEO(23).OR.SORIMP(23))) THEN
!
        DO I = 1,NPOIN
          STATIONNARY_VELOCITY = 1.0D0
          X0 = 5.0D0 + STATIONNARY_VELOCITY*AT
!
!         Tracer 1:
          PRIVE1(I) = EXP(-0.5D0*((X(I)-X0)**2))
!
!         Tracer 2:
          X1 = 2.50D0 + STATIONNARY_VELOCITY*AT
          X2 = 7.50D0 + STATIONNARY_VELOCITY*AT
          IF (X(I).GE.X1 .AND. X(I).LE.X2) THEN
            PRIVE2(I) = 1.D0
          ELSE
            PRIVE2(I) = 0.D0
          ENDIF
!
        ENDDO
      ENDIF
!
!=======================================================================
!
      RETURN
      END
