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
      DOUBLE PRECISION AL
!
!-----------------------------------------------------------------------
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=2
      ENDDO
!
      AL = 1.3D0
!
      DO IPLAN = 1,NPLAN
        ZSTAR%R(IPLAN) = (1.D0-AL**(IPLAN-1))/(1.D0-AL**(NPLAN-1))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
