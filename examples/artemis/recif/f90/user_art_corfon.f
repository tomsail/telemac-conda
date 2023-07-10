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
      DOUBLE PRECISION D1,D3,B,HCP,XRCP,XDEBUT
!
!-----------------------------------------------------------------------
!
!     TOPOGRAPHY DATA (m)

      D1 = 6.
      D3 = 2.
!     value of variable b
      B = 4.
!     the D in the formula in the documentation is D=D1-D3
!     B used to be called L0 but confusion with the documentation.
      
      XDEBUT=35.-B

!     TANH VARIATION
      DO I = 1,NPOIN
        XRCP=X(I)-XDEBUT
        HCP =(D1+D3)/2.-( (D1-D3)*TANH(3.*PI*((XRCP/B)-0.5)) )/2.
        ZF%R(I) = D1-HCP
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
