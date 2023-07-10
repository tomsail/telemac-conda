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
!
      ZSTAR%R( 1) = 0.D0
      ZSTAR%R( 2) = 0.075D0
      ZSTAR%R( 3) = 0.15D0
      ZSTAR%R( 4) = 0.225D0
      ZSTAR%R( 5) = 0.325D0
      ZSTAR%R( 6) = 0.425D0
      ZSTAR%R( 7) = 0.525D0
      ZSTAR%R( 8) = 0.625D0
      ZSTAR%R( 9) = 0.7D0
      ZSTAR%R(10) = 0.775D0
      ZSTAR%R(11) = 0.85D0
      ZSTAR%R(12) = 0.925D0
      ZSTAR%R(13) = 0.97D0
      ZSTAR%R(14) = 0.99D0
      ZSTAR%R(15) = 1.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
