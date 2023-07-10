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
      INTEGER IPOIN2
      DOUBLE PRECISION WX,WH,HV1,HV2,X0
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DE H , LA HAUTEUR D'EAU, POUR UNE COTE NULLE.
!
      CALL OS('X=C     ', X=H, C=0.D0)
      CALL OV('X=X-Y   ', X=H%R, Y=Z, DIM1=NPOIN2)
!
!     *** SOLITARY WAVE ***
!
!     WH WAVE HEIGHT
!     WX WQUILIBRIUM WATER DEPTH
!     X0 CREST INITIAL POSITION
!
!     WH=2.D0
      WH=1.D0
      WX=10.D0
!     X0=80.D0
      X0=150.D0
!
      DO IPOIN2=1,NPOIN2
        HV1=SQRT(3.D0/4.D0*WH/WX**3)*(X(IPOIN2)-X0)
        HV2=2.D0/(EXP(HV1)+EXP(-HV1))
        H%R(IPOIN2)=H%R(IPOIN2) + WH*HV2**2
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
