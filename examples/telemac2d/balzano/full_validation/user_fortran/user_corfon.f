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
        IF(X(I).LE.3600.0D0.OR.X(I).GE.6000.0D0)THEN
          ZF%R(I)=-X(I)/2760.0D0
        ENDIF
        IF(X(I).GE.3600.0D0.AND.X(I).LE.4800.0D0)THEN
          ZF%R(I)=X(I)/2760.0D0-60.0D0/23.0D0
        ENDIF
        IF(X(I).GE.4800.0D0.AND.X(I).LE.6000.0D0)THEN
          ZF%R(I)=-X(I)/920.0D0+100.0D0/23.0D0
        ENDIF
      ENDDO
      RETURN
      END
