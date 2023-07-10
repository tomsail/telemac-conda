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
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        IF(X(I).LE.3600.D0.OR.X(I).GE.6000.D0)THEN
          ZF%R(I) = -X(I)/2760.D0
        ENDIF
        IF(X(I).GE.3600.D0.AND.X(I).LE.4800.D0)THEN
          ZF%R(I) = X(I)/2760.D0-60.D0/23.D0
        ENDIF
        IF(X(I).GE.4800.D0.AND.X(I).LE.6000.D0)THEN
          ZF%R(I) = -X(I)/920.D0+100.D0/23.D0
        ENDIF
      ENDDO
      RETURN
      END
