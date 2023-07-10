!                   ***************************
                    SUBROUTINE USER_CONDI3D_UVW
!                   ***************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    USER INITIALISES VELOCITY
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
      USE INTERFACE_TELEMAC3D, EX_USER_CONDI3D_UVW => USER_CONDI3D_UVW
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN,IPOIN2,IPOIN3
      DOUBLE PRECISION WX,WH,HV1,HV2,HV3,HV4,X0
!
!-----------------------------------------------------------------------
!
!     SOLITARY WAVE INITIAL VELOCITY - ANALYTICAL SOLUTION
!
!     *** SOLITARY WAVE ***
!
!     WH WAVE HEIGHT
!     WX WQUILIBRIUM WATER DEPTH
!     X0 CREST INITIAL POSITION
!
      WH=2.D0
      WX=10.D0
      X0=80.D0
      DO IPLAN=1,NPLAN
        DO IPOIN2=1,NPOIN2
          IPOIN3 = (IPLAN-1)*NPOIN2 + IPOIN2
!
          HV1=SQRT(3.D0/4.D0*WH/WX**3)*(X(IPOIN3)-X0)
          HV2=2.D0/(EXP(HV1)+EXP(-HV1))
          U%R(IPOIN3)=SQRT(GRAV*WX)*WH/WX*HV2**2

          HV3=(EXP(HV1)-EXP(-HV1))/(EXP(HV1)+EXP(-HV1))
          HV4=SQRT(3.D0*GRAV*WX)
     &       *((SQRT(WH/WX))**3)*(Z(IPOIN3)+WX)/WX
          W%R(IPOIN3)=(HV4*HV2**2)*HV3
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
