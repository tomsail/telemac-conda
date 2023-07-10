!                   **********************
                    SUBROUTINE USER_CORFON
!                   **********************
!
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  Y AUDOUIN (LNHE)
!+        20/09/2018
!+        V8P0
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION PI
!
!-----------------------------------------------------------------------
!
!  Bosse à t=0
!
      PI=3.141592653D0
      DO I=1,NPOIN
      ZF%R(I) = 0.D0
        IF (X(I) .GE. 400.D0 .AND. X(I) .LE. 600.D0
     &     .AND. Y(I) .GE. -100.D0 .AND. Y(I) .LE. 100.D0) THEN
          ZF%R(I)=(SIN(PI*(X(I)-400.D0)/200.D0))**2
     &    *(SIN(PI*(Y(I)+100.D0)/200.D0))**2
        ENDIF
      END DO
      RETURN
      END
