!                   *************************
                    SUBROUTINE USER_CONDI3D_H
!                   *************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES DEPTH
!
!history  C.-T. PHAM (LNHE)
!+        24/03/2017
!+        V7P3
!+   Creation from not splitted CONDIM
!+   Called by CONDIM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_H => USER_CONDI3D_H
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER I
      DOUBLE PRECISION EIKON
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN2
        EIKON  = ( (X(I)-10.05D0)**2 + (Y(I)-10.05D0)**2 ) / 4.D0
        H%R(I) = 2.4D0 * ( 1.D0 + EXP(-EIKON) )
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
