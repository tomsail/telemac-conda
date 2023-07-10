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
      INTEGER I,IPLAN
      DOUBLE PRECISION DELTAZ,AUX
!
!-----------------------------------------------------------------------
!
      DO IPLAN=1,NPLAN
        DO I=1,NPOIN2
          IF(IPLAN.EQ.1) THEN
            DELTAZ=(MESH3D%Z%R(I+NPOIN2)-MESH3D%Z%R(I))
     &              /2.71828182845D0**2
          ELSE
            DELTAZ=MESH3D%Z%R(I+(IPLAN-1)*NPOIN2)-MESH3D%Z%R(I)
          ENDIF
          AUX=MAX(30.D0*DELTAZ/0.0162D0,1.D0)
          U%R(I+(IPLAN-1)*NPOIN2)=(0.0703D0/0.41D0)*LOG(AUX)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
