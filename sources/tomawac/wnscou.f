!                   *****************
                    SUBROUTINE WNSCOU
!                   *****************
!
     &( CK2   , FREQ  , DEPTH )
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
!***********************************************************************
!
!brief    COMPUTES THE WAVE NUMBER.
!+                SOLVES THE DISPERSION EQUATION WITHOUT CURRENT.
!code
!+     3 METHODS ARE USED DEPENDING ON THE VALUE OF K0*D
!+     (K0: WAVE NUMBER IN INFINITE DEPTH)
!+
!+                      3.2                  5.6
!+      -----------------!--------------------!-----------------> K0*D
!+                       !                    !
!+   EXPLICIT METHODE    ! ITERATIVE METHODE  !     K=K0
!+   (HUNT 9TH ORDER)    !                    ! (INFINITE DEPTH)
!
!history  M. BENOIT (EDF/DER/LNH)
!+        07/02/95
!+        V1P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF - LNHE)
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CK2            |<--| WAVE NUMBER
!| DEPTH          |-->| WATER DEPTH
!| FREQ           |-->| DISCRETIZED FREQUENCY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT
!
      USE INTERFACE_TOMAWAC, EX_WNSCOU => WNSCOU
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      DOUBLE PRECISION, INTENT(IN)    :: FREQ  , DEPTH
      DOUBLE PRECISION, INTENT(INOUT) :: CK2
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  I
      DOUBLE PRECISION :: P(9)  , XK0   , XK0D
      DOUBLE PRECISION :: AUX   , A     , Y     , YI    , OM
!
      DOUBLE PRECISION, PARAMETER :: EPS = 0.0001D0
      PARAMETER ( P   = (/
     &       0.66667D0,0.35550D0,0.16084D0,0.06320D0,0.02174D0,
     &       0.00654D0,0.00171D0,0.00039D0,0.00011D0 /) )
!
!.....COMPUTES THE ANGULAR FREQUENCY (OM), K0 AND K0D
      OM=FREQ*DEUPI
      XK0=OM**2/GRAVIT
      XK0D=XK0*DEPTH
!
!.....DETERMINES THE METHOD OF RESOLUTION DEPENDING ON THE VALUE OF XK0D
!     ================================================================
!
      IF(XK0D.LE.3.2D0) THEN
!
!.......EXPLICIT METHOD (HUNT 9TH ORDER)
!
        Y=XK0*DEPTH
        AUX=1.D0
        YI=1.D0
        DO I=1,9
          YI=YI*Y
          AUX=AUX+P(I)*YI
        ENDDO ! I
        AUX=Y+1.D0/AUX
        CK2=OM/SQRT(GRAVIT*DEPTH/AUX)
!
      ELSEIF(XK0D.LE.5.6D0) THEN
!
!.......ITERATIVE METHOD (FROM HUNT 9TH ORDER)
!
        Y=XK0*DEPTH
        AUX=1.D0
        YI=1.D0
        DO I=1,9
          YI=YI*Y
          AUX=AUX+P(I)*YI
        ENDDO ! I
        AUX=Y+1.D0/AUX
        CK2=OM/SQRT(GRAVIT*DEPTH/AUX)
        DO
        A=CK2
        CK2=XK0/TANH(A*DEPTH)
        IF (ABS(CK2-A)/CK2.LT.EPS) EXIT
        ENDDO
!
      ELSE
!
!.......APPROXIMATION OF INFINITE DEPTH
!
        CK2=XK0
!
      ENDIF
!
      RETURN
      END
