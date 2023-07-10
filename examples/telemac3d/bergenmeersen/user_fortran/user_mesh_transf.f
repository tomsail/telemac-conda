!                   ***************************
                    SUBROUTINE USER_MESH_TRANSF
!                   ***************************
!
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!brief    DEFINES THE MESH TRANSFORMATION
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
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC3D, EX_USER_MESH_TRANSF => USER_MESH_TRANSF
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN
!
!-----------------------------------------------------------------------
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=2
      ENDDO
      ZSTAR%R(1)=0.D0
      ZSTAR%R(2)=0.12D0
      ZSTAR%R(3)=0.30D0
      ZSTAR%R(4)=0.60D0
      ZSTAR%R(5)=1.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
