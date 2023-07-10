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
      DOUBLE PRECISION H1, R0, R1, HDELTA, R
      INTEGER I,GAMMA2
!
!-----------------------------------------------------------------------
!
      H1 = 4000.D0
      R0 = 10000.D0
      R1 = 90000.D0
      GAMMA2 = 2
      HDELTA = H1 / ( (R1/R0)**GAMMA2 - 1.D0 )

      DO I = 1,NPOIN
        R = SQRT(X(I)**2 + Y(I)**2)
        IF (R.LT.R1) THEN
          ZF%R(I) = -HDELTA * ( (R/R0)**GAMMA2 - 1.D0 )
          IF (ZF%R(I).GT.-0.01D0) ZF%R(I) = -0.01D0
        ENDIF
      END DO
      RETURN
      END
