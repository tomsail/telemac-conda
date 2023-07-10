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
      INTEGER IPCENTER
      DOUBLE PRECISION A,B,C
!
!-----------------------------------------------------------------------
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=2
      ENDDO
      IPCENTER = INT(REAL(NPLAN)/3.D0+0.5D0)
      ZSTAR%R(1)        = 0.D0
      ZSTAR%R(IPCENTER) = 0.5D0
      ZSTAR%R(NPLAN)    = 1.D0
!     BOTTOM HALF
!     LINEAR
      A = (0.5D0-0.01D0)/REAL(IPCENTER-1)
      B = 0.01D0-A
      DO IPLAN = 2,IPCENTER-1
        ZSTAR%R(IPLAN) = A*REAL(IPLAN)+B
      ENDDO
!     TOP HALF
!     QUADRATIC PROFILE
      A =-0.5D0/REAL(NPLAN-IPCENTER)/REAL(IPCENTER-NPLAN)
      B = 2.D0*REAL(NPLAN)*A
      C = 1.D0-REAL(NPLAN)**2*A
      DO IPLAN = IPCENTER+1,NPLAN-1
        ZSTAR%R(IPLAN) = -A*REAL(IPLAN)**2+B*REAL(IPLAN)+C
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
