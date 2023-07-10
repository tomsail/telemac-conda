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
      DO I = 1,NPOIN
        IF (X(I).LT.-5.D0) THEN
          ZF%R(I) = -0.616D0
        ENDIF
        IF (X(I).GE.-5.D0.AND.X(I).LT.5.D0) THEN
          ZF%R(I) = -0.616D0 + 0.05*(X(I) + 5.D0)
        ENDIF
        IF (X(I).GE.5.D0.AND.X(I).LT.9.4) THEN
          ZF%R(I) = -0.116D0 -0.025*(X(I) - 5.D0)
        ENDIF
        IF (X(I).GE.9.4) THEN
          ZF%R(I) = -0.226D0 + 0.05*(X(I) - 9.4)
        ENDIF
        IF (ZF%R(I).GT.-0.04D0) THEN
          ZF%R(I) = -0.04D0
        ENDIF
      ENDDO
      RETURN
      END
