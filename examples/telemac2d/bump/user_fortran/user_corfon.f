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
!history  F SOUILLE (LNHE)
!+        22/10/2019
!+        V8P1
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
!     TYPE OF BOTTOM FUNCTION: 0 PARABOLIC / 1 EXP
      INTEGER, PARAMETER :: BOTTOM_FUNCTION = 1
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
!       -------------------
!       PARABOLIC FUNCTION:
!       -------------------
        IF (BOTTOM_FUNCTION.EQ.0) THEN
          IF ((8D0.LE.X(I)).AND.(X(I).LE.12D0)) THEN
            ZF%R(I) = 0.2D0-0.05D0*(X(I)-10.D0)**2
          ELSE
            ZF%R(I) = 0.0D0
          ENDIF
        ENDIF
!
!       ---------------------
!       EXPONENTIAL FUNCTION:
!       ---------------------
        IF (BOTTOM_FUNCTION.EQ.1) THEN
          ZF%R(I) = 0.25D0*EXP(-0.5D0*(X(I)-10.D0)**2)
        ENDIF
!
      ENDDO
      RETURN
      END
