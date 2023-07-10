!                   **************************
                    SUBROUTINE USER_ART_CORFON
!                   **************************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!history  J-M HERVOUET
!+        01/03/1990
!+        V5P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION ECHEL
!
!-----------------------------------------------------------------------
!
      ECHEL=1.D0
      DO I=1,NPOIN
        IF((Y(I)/ECHEL).GE. 7.12D0)THEN
          ZF%R(I)=-0.33D0*ECHEL
        ELSEIF ((Y(I)/ECHEL) .LE. -9.13D0)THEN
          ZF%R(I)=-0.0044*ECHEL
!       ELSEIF ((Y(I)/ECHEL) .LE. -8D0)THEN
!        ZF%R(I)=-0.027*ECHEL
        ELSE
          ZF%R(I)=-0.02D0*((Y(I)/ECHEL)+9.35D0)*ECHEL
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
