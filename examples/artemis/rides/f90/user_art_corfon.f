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
      DOUBLE PRECISION NRID
      DOUBLE PRECISION D1,LCP,AA,XCP,XDEBUT,XRCP
!
!-----------------------------------------------------------------------
!
!     Variables
      D1  = 0.313
      AA  = 0.05
      LCP = 1.
!     nombre de longueur d'onde sur la bathymetrie
      NRID= 10.
      XDEBUT=25.
      XCP = XDEBUT+LCP*NRID/2.
!
!     bathy sinusoidale
      DO I = 1,NPOIN
        IF ( ABS(X(I)-XCP).LT.(NRID*LCP/2.) ) THEN
          XRCP    = X(I)-XDEBUT
          ZF%R(I) = AA*SIN(DEUPI*XRCP/LCP)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
