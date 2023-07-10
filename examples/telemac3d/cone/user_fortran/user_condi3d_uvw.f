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
      INTEGER I
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN3
        U%R(I) = -(Y(I)-10.05D0)
        V%R(I) =  (X(I)-10.05D0)
      ENDDO

      DO I=1,NPTFR3
        UBORL%R(I)=U%R(MESH3D%NBOR%I(I))
        VBORL%R(I)=V%R(MESH3D%NBOR%I(I))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
